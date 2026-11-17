# Shared ownership protocol for Tauri and direct R entry points.
desktop_owner_message <- "プロジェクトの実行情報を安全に確認できません。機器IDを取得できない場合も自動復旧できません。他のPK Desktopを終了し、プロジェクトのコピーを保存したうえで、RECOVERY_GUIDE.md に従い残った実行情報を確認してください。"
desktop_machine_identity <- function() {
  system <- unname(Sys.info()[["sysname"]])
  raw <- NULL; scheme <- NULL
  if (identical(system, "Linux")) {
    raw <- tryCatch(trimws(readLines("/etc/machine-id", n = 1L, warn = FALSE)), error = function(e) character())
    if (length(raw) != 1L || !grepl("^[0-9a-fA-F]{32}$", raw) || grepl("^0+$", raw)) raw <- NULL
    scheme <- "linux-machine-id-sha256-v1"
  } else if (identical(system, "Darwin")) {
    lines <- tryCatch(suppressWarnings(system2("/usr/sbin/ioreg", c("-rd1", "-c", "IOPlatformExpertDevice"), stdout = TRUE, stderr = FALSE)), error = function(e) character())
    line <- grep('"IOPlatformUUID"', lines, value = TRUE)
    if (length(line) == 1L) raw <- sub('.*"IOPlatformUUID"[[:space:]]*=[[:space:]]*"([^"]+)".*', '\\1', line)
    if (is.null(raw) || length(raw) != 1L || !grepl("^[0-9A-Fa-f-]{36}$", raw)) raw <- NULL
    scheme <- "macos-ioplatformuuid-sha256-v1"
  }
  if (is.null(raw)) return(NULL)
  list(scheme = scheme, fingerprint = digest::digest(paste0("pkdesktop-machine-v1:", tolower(raw)), algo = "sha256", serialize = FALSE))
}
desktop_host_identity <- function() list(host = unname(Sys.info()[["nodename"]]), system = unname(Sys.info()[["sysname"]]), machine_identity_version = 1L, machine_identity = desktop_machine_identity())
desktop_same_host <- function(identity) {
  host <- desktop_host_identity()
  valid <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  if (!is.list(identity) || !valid(identity$system) || !identical(identity$system, host$system)) return(FALSE)
  if (!is.null(identity$machine_identity_version)) {
    x <- identity$machine_identity; y <- host$machine_identity
    return(is.list(x) && is.list(y) && valid(x$scheme) && valid(x$fingerprint) && identical(x, y))
  }
  valid(host$host) && valid(identity$host) && identical(identity$host, host$host)
}
# Linux kill(0) distinguishes ESRCH from EPERM. Unknown output must never unlock a project.
desktop_linux_pid_absent <- function(pid) {
  if (!identical(unname(Sys.info()[["sysname"]]), "Linux") || !file.exists("/bin/kill")) return(FALSE)
  err <- tempfile(); on.exit(unlink(err), add = TRUE)
  status <- tryCatch(suppressWarnings(system2("/bin/kill", c("-0", as.character(pid)), stdout = FALSE, stderr = err, env = "LC_ALL=C")), error = function(e) NA_integer_)
  lines <- tryCatch(readLines(err, warn = FALSE), error = function(e) character())
  identical(as.integer(status), 1L) && length(lines) == 1L && grepl("No such process[[:space:]]*$", lines)
}
desktop_process_start <- function(pid) {
  if (!requireNamespace("ps", quietly = TRUE)) return(NULL)
  tryCatch(as.numeric(ps::ps_create_time(ps::ps_handle(as.integer(pid)))), error = function(e) NULL)
}
# Linux boot ID and process start ticks avoid wall-clock changes and ps version rounding.
desktop_linux_process_identity <- function(pid) {
  if (!identical(unname(Sys.info()[["sysname"]]), "Linux")) return(NULL)
  tryCatch({
    boot <- trimws(readLines("/proc/sys/kernel/random/boot_id", n = 1L, warn = FALSE))
    stat <- readLines(paste0("/proc/", as.integer(pid), "/stat"), n = 1L, warn = FALSE)
    fields <- strsplit(sub("^.*\\) ", "", stat), " +")[[1L]]
    ticks <- fields[[20L]]
    if (length(boot) != 1L || !grepl("^[0-9a-fA-F-]{36}$", boot) || !grepl("^[0-9]+$", ticks)) return(NULL)
    namespace <- Sys.readlink("/proc/self/ns/pid")
    if (length(namespace) != 1L || is.na(namespace) || !nzchar(namespace)) return(NULL)
    list(boot = boot, start_ticks = ticks, pid_namespace = namespace)
  }, error = function(e) NULL)
}
desktop_process_state <- function(pid, identity = NULL) {
  if (!(is.numeric(pid) && length(pid) == 1L && is.finite(pid) && pid > 0 && pid == floor(pid))) return("unknown")
  if (!requireNamespace("ps", quietly = TRUE)) return("unknown")
  if (is.list(identity) && is.list(identity$linux_process_identity) && !is.null(identity$linux_process_identity$pid_namespace)) {
    current_namespace <- tryCatch(Sys.readlink("/proc/self/ns/pid"), error = function(e) NA_character_)
    if (!identical(identity$linux_process_identity$pid_namespace, current_namespace)) return("unknown")
  }
  handle <- tryCatch(ps::ps_handle(as.integer(pid)), error = function(e) e)
  if (inherits(handle, "no_such_process")) return("dead")
  if (inherits(handle, "error")) return(if (desktop_linux_pid_absent(pid)) "dead" else "unknown")
  running <- tryCatch(ps::ps_is_running(handle), error = function(e) NA)
  if (identical(running, FALSE)) return("dead")
  if (!identical(running, TRUE)) return("unknown")
  if (is.null(identity) || !is.list(identity)) return("unknown")
  if (!desktop_same_host(identity)) return("unknown")
  if (identical(unname(Sys.info()[["sysname"]]), "Linux")) {
    saved_linux <- identity$linux_process_identity
    actual_linux <- desktop_linux_process_identity(pid)
    if (is.list(saved_linux) && is.list(actual_linux) && !is.null(saved_linux$boot) && !is.null(saved_linux$start_ticks)) {
      if (!identical(saved_linux$boot, actual_linux$boot) || !identical(saved_linux$start_ticks, actual_linux$start_ticks)) return("reused")
      return("live")
    }
  }
  saved <- identity$process_start_time
  actual <- desktop_process_start(pid)
  if (!is.numeric(saved) || length(saved) != 1L || !is.finite(saved) || is.null(actual) || !is.finite(actual)) return("unknown")
  if (abs(saved - actual) > 2) {
    if (identical(unname(Sys.info()[["sysname"]]), "Linux")) "unknown" else "reused"
  } else "live"
}
desktop_owner_identity <- function(project) {
  host <- desktop_host_identity(); start <- desktop_process_start(Sys.getpid())
  list(format = "pkapp-owner-identity-1", pid = Sys.getpid(), project = project,
    host = host$host, system = host$system, machine_identity_version = host$machine_identity_version, machine_identity = host$machine_identity, linux_process_identity = desktop_linux_process_identity(Sys.getpid()), process_start_time = start, started_at = desktop_time())
}
desktop_acquire_project <- function(project) {
  desktop_check(is.character(project) && length(project) == 1 && !is.na(project) && nzchar(project), "E_VALIDATION", "プロジェクトフォルダを指定してください。")
  dir.create(project, recursive = TRUE, showWarnings = FALSE)
  project <- normalizePath(project, mustWork = TRUE)
  lock <- file.path(project, ".pkapp-owner"); recovery <- file.path(project, ".pkapp-owner-recovery")
  desktop_check(!dir.exists(recovery), "E_PROJECT_BUSY", desktop_owner_message)
  if (dir.exists(lock)) {
    lock_link <- Sys.readlink(lock)
    desktop_check(is.na(lock_link) || !nzchar(lock_link), "E_PROJECT_BUSY", desktop_owner_message)
    desktop_check(dir.create(recovery, showWarnings = FALSE), "E_PROJECT_BUSY", desktop_owner_message)
    on.exit(unlink(recovery, recursive = TRUE), add = TRUE)
    files <- list.files(lock, all.files = TRUE, no.. = TRUE)
    safe_files <- setequal(files, "pid") || setequal(files, c("pid", "identity.json"))
    old_pid <- tryCatch(suppressWarnings(as.integer(readLines(file.path(lock, "pid"), n = 1, warn = FALSE))), error = function(e) integer())
    identity <- if ("identity.json" %in% files) tryCatch(desktop_read(file.path(lock, "identity.json")), error = function(e) NULL) else NULL
    if ("identity.json" %in% files) desktop_check(!is.null(identity) && identical(identity$format, "pkapp-owner-identity-1") && identical(as.numeric(identity$pid), as.numeric(old_pid)), "E_PROJECT_BUSY", desktop_owner_message)
    state <- desktop_process_state(old_pid, identity)
    desktop_check(safe_files && state %in% c("dead", "reused"), "E_PROJECT_BUSY", desktop_owner_message)
    # Never recover a known foreign host automatically, even if its PID is absent locally.
    if (!is.null(identity)) {
      desktop_check(desktop_same_host(identity), "E_PROJECT_BUSY", desktop_owner_message)
    }
    stale <- file.path(project, paste0(".pkapp-owner-stale-", desktop_random_token()))
    desktop_check(file.rename(lock, stale), "E_PROJECT_BUSY", desktop_owner_message)
    unlink(stale, recursive = TRUE)
  }
  desktop_check(dir.create(lock, showWarnings = FALSE), "E_PROJECT_BUSY", desktop_owner_message)
  committed <- FALSE
  on.exit(if (!committed) unlink(lock, recursive = TRUE), add = TRUE)
  # A concurrent recovery can appear between the first check and mkdir.
  desktop_check(!dir.exists(recovery) || exists("state", inherits = FALSE), "E_PROJECT_BUSY", desktop_owner_message)
  writeLines(as.character(Sys.getpid()), file.path(lock, "pid"))
  desktop_atomic(desktop_owner_identity(project), file.path(lock, "identity.json"))
  committed <- TRUE
  list(project = project, lock = lock, pid = Sys.getpid())
}
desktop_release_project <- function(owner) {
  pid_file <- file.path(owner$lock, "pid")
  files <- if (dir.exists(owner$lock)) list.files(owner$lock, all.files = TRUE, no.. = TRUE) else character()
  if (file.exists(pid_file) && identical(readLines(pid_file, n = 1, warn = FALSE), as.character(owner$pid)) &&
      (setequal(files, "pid") || setequal(files, c("pid", "identity.json")))) unlink(owner$lock, recursive = TRUE)
  invisible(NULL)
}
desktop_random_token <- function() paste(format(openssl::rand_bytes(32)), collapse = "")
desktop_r_runtime_path <- function(project) file.path(project, ".pkapp-runtime.json")
desktop_r_runtime <- function(engine, api_port, ui_port, mode = "summary") {
  owner <- desktop_owner_identity(engine$project)
  list(format = "pkapp-r-runtime-1", api_port = as.integer(api_port), ui_port = as.integer(ui_port),
    token = desktop_random_token(), instance_id = desktop_random_token(), pid = Sys.getpid(),
    host = owner$host, system = owner$system, machine_identity_version = owner$machine_identity_version, machine_identity = owner$machine_identity, linux_process_identity = owner$linux_process_identity, process_start_time = owner$process_start_time, started_at = owner$started_at,
    project = engine$project, mode = mode, bundle = if (identical(mode, "summary")) engine$spec$data$bundle else NULL)
}
desktop_write_r_runtime <- function(engine, runtime) {
  path <- desktop_r_runtime_path(engine$project)
  link <- Sys.readlink(path)
  is_link <- !is.na(link) && nzchar(link)
  if (file.exists(path) || is_link) {
    desktop_check(!is_link, "E_RUNTIME_BUSY", desktop_owner_message)
    previous <- tryCatch(desktop_read(path), error = function(e) NULL)
    desktop_check(is.list(previous) && identical(previous$format, "pkapp-r-runtime-1") && is.character(previous$project) && length(previous$project) == 1L, "E_RUNTIME_BUSY", desktop_owner_message)
    state <- desktop_process_state(previous$pid, previous)
    if (!is.null(previous$machine_identity_version) || !is.null(previous$host) || !is.null(previous$system)) {
      desktop_check(desktop_same_host(previous), "E_RUNTIME_BUSY", desktop_owner_message)
    }
    desktop_check(state %in% c("dead", "reused"), "E_RUNTIME_BUSY", desktop_owner_message)
    # Secure old credentials before retaining a copy; refuse if permissions cannot be enforced.
    permission_ok <- isTRUE(Sys.chmod(path, "0600"))
    mode <- file.info(path)$mode
    desktop_check(permission_ok && length(mode) == 1L && !is.na(mode) && bitwAnd(as.integer(mode), 63L) == 0L, "E_RUNTIME_BUSY", desktop_owner_message)
    archive <- file.path(engine$project, paste0(".pkapp-runtime-stale-", desktop_random_token(), ".json"))
    # Retain only an allowlist; old runtime credentials never enter the archive.
    archived <- list(format = "pkapp-runtime-stale-redacted-1", pid = previous$pid,
      archived_at = desktop_time())
    desktop_check(unlink(path) == 0L && !file.exists(path), "E_RUNTIME_BUSY", desktop_owner_message)
    desktop_atomic(archived, archive)
  }
  desktop_atomic(runtime, path)
  invisible(path)
}
desktop_remove_r_runtime <- function(engine, runtime) {
  path <- desktop_r_runtime_path(engine$project)
  if (file.exists(path) && identical(desktop_read(path)$instance_id, runtime$instance_id)) unlink(path)
  invisible(NULL)
}
