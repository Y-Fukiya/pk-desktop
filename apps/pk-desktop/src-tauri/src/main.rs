use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::{
    fs::{self, File, OpenOptions},
    io::{Read, Write},
    net::{TcpListener, TcpStream},
    os::unix::fs::OpenOptionsExt,
    path::{Path, PathBuf},
    process::{Child, Command, Stdio},
    sync::{Arc, Mutex},
    thread,
    time::{Duration, Instant},
};
use tauri::{Manager, WebviewUrl, WebviewWindowBuilder};

#[derive(Clone, Serialize, Deserialize)]
struct Runtime {
    api_port: u16,
    ui_port: u16,
    token: String,
    instance_id: String,
    pid: u32,
    parent_pid: u32,
    project: String,
    bundle: String,
}
struct EngineProcess {
    child: Child,
    runtime: Runtime,
    file: PathBuf,
}
#[derive(Default, Clone)]
struct DesktopState(Arc<Mutex<Option<EngineProcess>>>);
fn random_hex() -> Result<String, String> {
    let mut b = [0u8; 32];
    File::open("/dev/urandom")
        .and_then(|mut f| f.read_exact(&mut b))
        .map_err(|e| e.to_string())?;
    Ok(b.iter().map(|x| format!("{x:02x}")).collect())
}
fn http(runtime: &Runtime, method: &str, path: &str) -> Result<Value, String> {
    let address = format!("127.0.0.1:{}", runtime.api_port)
        .parse()
        .map_err(|_| "Invalid address")?;
    let mut stream = TcpStream::connect_timeout(&address, Duration::from_millis(600))
        .map_err(|e| e.to_string())?;
    stream
        .set_read_timeout(Some(Duration::from_secs(1)))
        .map_err(|e| e.to_string())?;
    stream
        .set_write_timeout(Some(Duration::from_secs(1)))
        .map_err(|e| e.to_string())?;
    let request = format!("{method} {path} HTTP/1.1\r\nHost: 127.0.0.1:{}\r\nAuthorization: Bearer {}\r\nContent-Length: 0\r\nConnection: close\r\n\r\n", runtime.api_port, runtime.token);
    stream
        .write_all(request.as_bytes())
        .map_err(|e| e.to_string())?;
    let mut reply = String::new();
    stream
        .take(65536)
        .read_to_string(&mut reply)
        .map_err(|e| e.to_string())?;
    let (head, body) = reply
        .split_once("\r\n\r\n")
        .ok_or("Malformed HTTP response")?;
    if !head.lines().next().unwrap_or("").contains(" 200 ") {
        return Err("R engine is not ready".into());
    }
    serde_json::from_str(body).map_err(|e| e.to_string())
}
fn save_runtime(path: &Path, runtime: &Runtime) -> Result<(), String> {
    let tmp = path.with_file_name(format!(".runtime-{}.tmp", random_hex()?));
    let mut f = OpenOptions::new()
        .write(true)
        .create_new(true)
        .mode(0o600)
        .open(&tmp)
        .map_err(|e| e.to_string())?;
    f.write_all(
        serde_json::to_string(runtime)
            .map_err(|e| e.to_string())?
            .as_bytes(),
    )
    .map_err(|e| e.to_string())?;
    f.sync_all().map_err(|e| e.to_string())?;
    fs::rename(tmp, path).map_err(|e| e.to_string())
}
fn reserve_runtime(path: &Path, runtime: &Runtime) -> Result<(), String> {
    let mut file = OpenOptions::new()
        .write(true)
        .create_new(true)
        .mode(0o600)
        .open(path)
        .map_err(|e| e.to_string())?;
    let result = file
        .write_all(
            serde_json::to_string(runtime)
                .map_err(|e| e.to_string())?
                .as_bytes(),
        )
        .and_then(|_| file.sync_all());
    if let Err(e) = result {
        let _ = fs::remove_file(path);
        return Err(e.to_string());
    }
    Ok(())
}
fn pid_alive(pid: u32) -> bool {
    pid > 0 && unsafe { libc::kill(pid as i32, 0) == 0 }
}
impl EngineProcess {
    fn stop(&mut self) {
        let _ = http(&self.runtime, "POST", "/shutdown");
        let deadline = Instant::now() + Duration::from_secs(5);
        while Instant::now() < deadline {
            if matches!(self.child.try_wait(), Ok(Some(_))) {
                break;
            }
            thread::sleep(Duration::from_millis(100));
        }
        if !matches!(self.child.try_wait(), Ok(Some(_))) {
            // Signal only the owned child, whose Child handle has not been reaped.
            unsafe {
                libc::kill(self.child.id() as i32, libc::SIGTERM);
            }
            let deadline = Instant::now() + Duration::from_secs(5);
            while Instant::now() < deadline {
                if matches!(self.child.try_wait(), Ok(Some(_))) {
                    break;
                }
                thread::sleep(Duration::from_millis(100));
            }
            if !matches!(self.child.try_wait(), Ok(Some(_))) {
                let _ = self.child.kill();
            }
        }
        let _ = self.child.wait();
        if let Ok(bytes) = fs::read(&self.file) {
            if serde_json::from_slice::<Runtime>(&bytes)
                .is_ok_and(|r| r.instance_id == self.runtime.instance_id)
            {
                let _ = fs::remove_file(&self.file);
            }
        }
        // A killed R cannot run on.exit; remove only this exact child owner's lock.
        let lock = Path::new(&self.runtime.project).join(".pkapp-owner");
        if fs::read_to_string(lock.join("pid"))
            .is_ok_and(|p| p.trim() == self.child.id().to_string())
        {
            let _ = fs::remove_file(lock.join("pid"));
            let _ = fs::remove_file(lock.join("identity.json"));
            let _ = fs::remove_dir(lock);
        }
    }
}
fn source_root() -> PathBuf {
    if let Ok(executable) = std::env::current_exe() {
        if let Some(contents) = executable.parent().and_then(|p| p.parent()) {
            let bundled = contents.join("Resources/source");
            if bundled.join("apps/pk-desktop/R/serve.R").is_file() {
                return bundled;
            }
        }
    }
    if let Ok(root) = std::env::var("PK_REVIEW_ROOT") {
        return root.into();
    }
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(3)
        .unwrap()
        .to_path_buf()
}
#[tauri::command]
fn defaults(app: tauri::AppHandle) -> Result<Value, String> {
    let root = source_root();
    let runtime = app
        .path()
        .app_data_dir()
        .map_err(|e| e.to_string())?
        .join("runtime.json");
    let bundled_r = root.parent().unwrap_or(&root).join("runtime/bin/Rscript");
    let rscript = if bundled_r.is_file() { bundled_r } else { PathBuf::from("/usr/local/bin/Rscript") };
    let project = app.path().app_data_dir().map_err(|e| e.to_string())?.join("projects/my-project");
    Ok(json!({"root":root, "rscript":rscript,
        "bundle":root.join("outputs/pk-review/bundles/reviewed-repeated"),
        "project":project, "runtime":runtime,
        "python":root.join(".venv/bin/python"), "mcp_script":root.join("apps/pk-desktop/mcp_server.py")}))
}
#[tauri::command]
async fn start_engine(
    app: tauri::AppHandle,
    state: tauri::State<'_, DesktopState>,
    project: String,
    bundle: String,
    rscript: String,
) -> Result<Value, String> {
    let shared = state.0.clone();
    let app_for_worker = app.clone();
    let started = tauri::async_runtime::spawn_blocking(move || -> Result<Runtime, String> {
        let mut guard = shared.lock().map_err(|_| "Process state unavailable")?;
        if guard.is_some() {
            return Err("Close the current analysis window before opening another project".into());
        }
        let data_dir = app_for_worker
            .path()
            .app_data_dir()
            .map_err(|e| e.to_string())?;
        fs::create_dir_all(&data_dir).map_err(|e| e.to_string())?;
        let runtime_file = data_dir.join("runtime.json");
        if runtime_file.exists() {
            let old: Runtime =
                serde_json::from_slice(&fs::read(&runtime_file).map_err(|e| e.to_string())?)
                    .map_err(|e| e.to_string())?;
            if pid_alive(old.pid) || pid_alive(old.parent_pid) {
                return Err("Another PK Desktop instance is active; close it first".into());
            }
            fs::remove_file(&runtime_file).map_err(|e| e.to_string())?;
        }
        fs::create_dir_all(&project).map_err(|e| e.to_string())?;
        let project = fs::canonicalize(&project).map_err(|e| e.to_string())?;
        let bundle = fs::canonicalize(&bundle).map_err(|e| e.to_string())?;
        if !Path::new(&rscript).is_absolute() || !Path::new(&rscript).is_file() {
            return Err("Select an installed Rscript executable".into());
        }
        let p1 = TcpListener::bind("127.0.0.1:0").map_err(|e| e.to_string())?;
        let p2 = TcpListener::bind("127.0.0.1:0").map_err(|e| e.to_string())?;
        let mut runtime = Runtime {
            api_port: p1.local_addr().unwrap().port(),
            ui_port: p2.local_addr().unwrap().port(),
            token: random_hex()?,
            instance_id: random_hex()?,
            pid: 0,
            parent_pid: std::process::id(),
            project: project.to_string_lossy().into(),
            bundle: bundle.to_string_lossy().into(),
        };
        let log_path = project.join("engine.log");
        if fs::metadata(&log_path).is_ok_and(|m| m.len() > 8 * 1024 * 1024) {
            fs::rename(&log_path, project.join("engine.previous.log"))
                .map_err(|e| e.to_string())?;
        }
        let log = OpenOptions::new()
            .create(true)
            .append(true)
            .mode(0o600)
            .open(&log_path)
            .map_err(|e| e.to_string())?;
        let stderr = log.try_clone().map_err(|e| e.to_string())?;
        reserve_runtime(&runtime_file, &runtime)?;
        let root = source_root();
        drop(p1);
        drop(p2); // R binds both ports; a collision is a visible startup failure.
        let child = Command::new(&rscript)
            .arg("--vanilla")
            .arg(root.join("apps/pk-desktop/R/serve.R"))
            .current_dir(&root)
            .env("PK_REVIEW_ROOT", &root)
            .env("PKAPP_RUNTIME", &runtime_file)
            .stdin(Stdio::null())
            .stdout(Stdio::from(log))
            .stderr(Stdio::from(stderr))
            .spawn();
        let child = match child {
            Ok(c) => c,
            Err(e) => {
                let _ = fs::remove_file(&runtime_file);
                return Err(e.to_string());
            }
        };
        runtime.pid = child.id();
        let mut process = EngineProcess {
            child,
            runtime: runtime.clone(),
            file: runtime_file,
        };
        if let Err(e) = save_runtime(&process.file, &runtime) {
            process.stop();
            return Err(e);
        }
        let deadline = Instant::now() + Duration::from_secs(30);
        while Instant::now() < deadline {
            if matches!(process.child.try_wait(), Ok(Some(_))) {
                break;
            }
            if let Ok(health) = http(&runtime, "GET", "/health") {
                if health["instance_id"] == runtime.instance_id && health["pid"] == runtime.pid {
                    *guard = Some(process);
                    return Ok(runtime);
                }
            }
            thread::sleep(Duration::from_millis(250));
        }
        process.stop();
        Err(format!(
            "R startup failed or timed out. See {}",
            log_path.display()
        ))
    })
    .await
    .map_err(|e| e.to_string())??;
    let port = started.ui_port;
    let url = format!("http://127.0.0.1:{port}/?token={}", started.token)
        .parse()
        .map_err(|_| "Invalid local URL")?;
    let window = WebviewWindowBuilder::new(&app, "teal", WebviewUrl::External(url))
        .title("PK Desktop · teal")
        .inner_size(1300.0, 900.0)
        .on_navigation(move |url| {
            url.scheme() == "http"
                && url.host_str() == Some("127.0.0.1")
                && url.port() == Some(port)
        })
        .build();
    if let Err(e) = window {
        stop_owned(&state);
        return Err(e.to_string());
    }
    Ok(json!({"running":true,"pid":started.pid,"project":started.project}))
}
fn stop_owned(state: &DesktopState) {
    if let Ok(mut guard) = state.0.lock() {
        if let Some(mut p) = guard.take() {
            p.stop();
        }
    }
}
#[tauri::command]
fn engine_status(
    app: tauri::AppHandle,
    state: tauri::State<'_, DesktopState>,
) -> Result<Value, String> {
    let mut guard = state.0.lock().map_err(|_| "Process state unavailable")?;
    if let Some(p) = guard.as_mut() {
        if p.child.try_wait().map_err(|e| e.to_string())?.is_some() {
            let log = Path::new(&p.runtime.project).join("engine.log");
            p.stop();
            *guard = None;
            drop(guard);
            if let Some(w) = app.get_webview_window("teal") {
                let _ = w.close();
            }
            return Ok(
                json!({"running":false,"message":format!("R stopped. See {}",log.display())}),
            );
        }
        return Ok(json!({"running":true}));
    }
    Ok(json!({"running":false}))
}
#[tauri::command]
fn stop_engine(app: tauri::AppHandle, state: tauri::State<'_, DesktopState>) {
    if let Some(w) = app.get_webview_window("teal") {
        let _ = w.close();
    }
    stop_owned(&state);
}
fn main() {
    tauri::Builder::default()
        .manage(DesktopState::default())
        .invoke_handler(tauri::generate_handler![
            defaults,
            start_engine,
            stop_engine,
            engine_status
        ])
        .on_window_event(|window, event| {
            if matches!(event, tauri::WindowEvent::CloseRequested { .. }) {
                stop_owned(&window.state::<DesktopState>());
                if window.label() == "main" {
                    window.app_handle().exit(0);
                }
            }
        })
        .build(tauri::generate_context!())
        .expect("Could not build desktop application")
        .run(|app, event| {
            if matches!(event, tauri::RunEvent::Exit) {
                stop_owned(&app.state::<DesktopState>());
            }
        });
}
