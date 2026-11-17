const $ = id => document.getElementById(id);
const invoke = window.__TAURI__.core.invoke;
let running = false;
async function initialise() {
  try {
    const d = await invoke("defaults");
    for (const name of ["project", "bundle", "rscript"]) $(name).value = d[name];
    $("config").textContent = `[mcp_servers.pkapp]\ncommand = ${JSON.stringify(d.python)}\nargs = ${JSON.stringify([d.mcp_script, "--runtime", d.runtime])}`;
    $("status").textContent = "保存先と入力フォルダを確認して、プロジェクトを開いてください。";
  } catch (e) { $("status").textContent = String(e); }
}
$("launch").addEventListener("submit", async event => {
  event.preventDefault(); $("start").disabled = true;
  $("status").textContent = "Rを起動し、CSVと共通設定を確認しています…";
  try {
    const r = await invoke("start_engine", Object.fromEntries(["project", "bundle", "rscript"].map(k => [k, $(k).value])));
    running = true;
    $("status").textContent = `tealの解析画面を開きました。保存先：${r.project}`;
  } catch (e) { $("status").textContent = String(e); }
  finally { $("start").disabled = false; }
});
$("stop").addEventListener("click", async () => {
  $("stop").disabled = true;
  try { await invoke("stop_engine"); running = false; $("status").textContent = "解析を終了しました。集計結果と設定はプロジェクトに保存されています。"; }
  catch (e) { $("status").textContent = String(e); }
  finally { $("stop").disabled = false; }
});
initialise();
setInterval(async () => {
  if (!running) return;
  try {
    const s = await invoke("engine_status");
    if (!s.running) { running = false; $("status").textContent = s.message || "解析を終了しました。プロジェクトを再度開けます。"; }
  } catch (e) { $("status").textContent = String(e); }
}, 1500);
