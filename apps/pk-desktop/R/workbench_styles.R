# CSS is scoped to desktop-owned surfaces. teal's public UI owns its internals.
desktop_workbench_styles <- function() shiny::singleton(shiny::tags$head(
  shiny::tags$style(shiny::HTML('
.pk-adpp-mapping .pk-adpp-step { margin-bottom: 14px; }
.pk-adpp-mapping a.shiny-download-link,
.pk-adpp-mapping a.shiny-download-link:hover,
.pk-adpp-mapping a.shiny-download-link:focus { color:#23343c; background:#fff; border-color:#b9c9cc; }
.pk-adpp-mapping a.shiny-download-link:focus-visible { outline:3px solid #176b79; outline-offset:2px; }
.pk-explore-summary-table .pk-number { text-align:right; font-variant-numeric:tabular-nums; }
.pk-explore-table-block { margin-bottom:24px; }
.pk-explore-table-block details { margin-top:8px; }
.pk-explore-section { margin:20px 0 24px; }
.pk-explore-pooling { max-width:300px; margin:8px 0 18px; }
.pk-explore-single-controls { margin-left:14px; max-width:260px; }
.pk-explore-form { display:grid; grid-template-columns:repeat(auto-fit,minmax(180px,1fr)); gap:8px 16px; align-items:start; }
.pk-explore-form > * { min-width:0; }
.pk-explore-form .selectize-input { white-space:nowrap; overflow:hidden; text-overflow:ellipsis; padding-right:32px; }
.pk-explore-form .selectize-input > .item { max-width:calc(100% - 8px); overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.pk-explore-form .pk-explore-note { grid-column:1 / -1; margin:0; }
.pk-workspace [id$="-results_anchor"] { scroll-margin-top:80px; }
:root { --pk-ink:#23343c; --pk-muted:#64747b; --pk-line:#dce4e6; --pk-accent:#176b79; --pk-paper:#fff; --pk-surface:#f4f7f7; }
body:has(.pk-brand) { background:var(--pk-surface); color:var(--pk-ink); font-family:-apple-system,BlinkMacSystemFont,"Segoe UI","Hiragino Kaku Gothic ProN","Yu Gothic",sans-serif; }
.navbar:has(.pk-brand) { background:#fff; border-bottom:1px solid var(--pk-line); padding:0 24px; min-height:62px; box-shadow:0 1px 3px #263b4210; }
.pk-brand { display:flex; align-items:center; gap:9px; font-size:18px; font-weight:750; letter-spacing:-.025em; color:var(--pk-ink); }
.pk-brand::before { content:""; width:10px; height:24px; border-radius:3px; background:var(--pk-accent); display:inline-block; }
.pk-version { color:var(--pk-muted); border:1px solid var(--pk-line); border-radius:4px; padding:2px 5px; font-size:10px; font-weight:500; letter-spacing:0; }
.navbar:has(.pk-brand) .nav-link { padding:20px 22px; color:var(--pk-muted); font-size:14px; border-bottom:3px solid transparent; font-weight:650; }
.navbar:has(.pk-brand) .nav-link.active { color:var(--pk-accent); border-bottom-color:var(--pk-accent); background:#f4faf9; }
.pk-analysis-context { display:flex; align-items:center; justify-content:space-between; gap:16px; padding:10px 24px; background:#edf3f2; border-bottom:1px solid var(--pk-line); }
.pk-analysis-context-content { display:flex; align-items:center; flex-wrap:wrap; column-gap:18px; row-gap:5px; font-size:12px; }
.pk-context-label { color:#52676d; font-weight:700; }
.pk-data-count { display:inline-flex; gap:8px; color:#52676d; font-size:11px; letter-spacing:.02em; }
.pk-data-count strong { color:var(--pk-ink); font-size:12px; font-variant-numeric:tabular-nums; }
.pk-context-note { font-size:11px; color:#5b6e74; }
.pk-analysis-context > .btn { flex:none; color:var(--pk-accent); background:#fff; border:1px solid #b6cccd; font-size:12px; }
.pk-data-page,.pk-analysis,.pk-reports { max-width:1920px; margin:0 auto; min-width:0; }
.pk-data-page { padding:20px 8px 28px; }
.pk-workbench,.pk-analysis,.pk-reports,.pk-import-settings { font-size:14px; color:var(--pk-ink); line-height:1.6; }
.pk-workbench *, .pk-analysis *, .pk-reports * { box-sizing:border-box; }
.pk-workbench { display:grid; grid-template-columns:292px minmax(0,1fr); gap:20px; align-items:start; }
.pk-wb-main,.pk-wb-sidebar { min-width:0; }
.pk-wb-sidebar { display:flex; flex-direction:column; position:sticky; top:16px; max-height:max(300px,calc(100vh - 200px)); max-height:max(300px,calc(100dvh - 200px)); overflow:hidden; background:#fff; border:1px solid var(--pk-line); border-radius:10px; }
.pk-wb-sidebar-heading { padding:16px 16px 10px; flex:none; }
.pk-wb-sidebar .pk-wb-sidebar-heading h2 { font-size:16px; margin:0 0 3px; }
.pk-wb-sidebar-heading p { font-size:11px; color:var(--pk-muted); margin:0; }
.pk-wb-sidebar > .shiny-html-output { flex:none; }
.pk-wb-sidebar-state { display:flex; align-items:center; flex-wrap:wrap; gap:6px 12px; margin:0 16px 12px; font-size:11px; color:var(--pk-muted); }
.pk-wb-sidebar-state strong { color:var(--pk-ink); overflow-wrap:anywhere; }
.pk-wb-sidebar-state.has-draft span { color:#805c13; background:#fff3da; border-radius:4px; padding:2px 6px; }
.pk-wb-sidebar > .tabbable { display:flex; flex-direction:column; min-height:0; flex:1 1 auto; }
.pk-wb-sidebar > .tabbable > .nav-pills { display:flex; flex-wrap:nowrap; gap:3px; flex:none; margin:0 12px; padding:4px; border-radius:9px; background:#eef3f3; }
.pk-wb-sidebar > .tabbable > .nav-pills > li { flex:1 1 auto; margin:0; }
.pk-wb-sidebar > .tabbable > .nav-pills .nav-link { display:block; border:1px solid transparent; border-radius:6px; padding:8px 7px; font-size:12px; font-weight:650; color:#52676d; text-align:center; white-space:nowrap; }
.pk-wb-sidebar > .tabbable > .nav-pills .nav-link.active { background:#fff; border-color:#b9d1d0; color:#155d69; box-shadow:0 1px 3px #23343c10; }
.pk-wb-sidebar > .tabbable > .nav-pills .nav-link:hover { color:#155d69; }
.pk-wb-sidebar > .tabbable > .tab-content { min-height:0; overflow-y:auto; padding:14px 16px 16px; overscroll-behavior:contain; scrollbar-gutter:stable; }
.pk-wb-sidebar > .tabbable > .tab-content > .tab-pane > .pk-caption:first-child,.pk-wb-catalog > .pk-caption:first-child { margin-top:0; }
.pk-wb-catalog { position:static; min-width:0; }
.pk-wb-sidebar-footer { display:block; flex:none; padding:12px 16px; border-top:1px solid var(--pk-line); background:#fafcfc; }
.pk-wb-sidebar-footer-host { flex:none; }
.pk-wb-sidebar-footer .pk-caption { margin:7px 0 0 !important; font-size:11px !important; }
.pk-wb-dropdown { z-index:1070; font-family:-apple-system,BlinkMacSystemFont,"Segoe UI","Hiragino Kaku Gothic ProN","Yu Gothic",sans-serif; font-size:13px; border:1px solid #b9c9cc; box-shadow:0 4px 16px #23343c26; }
.pk-wb-dropdown .selectize-dropdown-content { max-height:220px; overscroll-behavior:contain; }
.pk-workbench h1,.pk-page-heading h1 { font-size:25px; font-weight:700; line-height:1.35; letter-spacing:-.025em; margin:0 0 7px; }
.pk-workbench h2,.pk-analysis h2,.pk-reports h2 { font-size:16px; font-weight:700; line-height:1.5; margin:0 0 14px; }
.pk-workbench h3,.pk-analysis h3,.pk-reports h3 { font-size:14px; font-weight:700; line-height:1.5; margin:0 0 10px; }
.pk-wb-main { display:flex; flex-direction:column; gap:12px; }
.pk-wb-toolbar { display:flex; flex-wrap:wrap; align-items:center; justify-content:space-between; gap:10px; }
.pk-wb-toolbar .shiny-input-container { margin-bottom:0; }
.pk-wb-toolbar .pk-caption { margin:4px 0 0; }
.pk-wb-toolbar > .shiny-html-output { flex:1 1 240px; }
.pk-workbench .pk-wb-toolbar > .shiny-input-container { width:auto; flex:0 0 auto; }
.pk-wb-main > .tabbable { background:#fff; border:1px solid var(--pk-line); border-radius:8px; padding:0 16px 12px; min-width:0; }
.pk-wb-main > .tabbable > .nav { margin:0 -16px 14px; padding:0 8px; font-size:13px; }
.pk-wb-main > .tabbable > .nav .nav-link { padding:12px 14px; }
.pk-wb-main > .tabbable > .tab-content { min-width:0; }
.pk-wb-state { display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:8px; font-size:12px; color:#4b6167; padding:0 2px; }
.pk-wb-status { padding:3px 8px; border-radius:4px; font-size:10px; font-weight:700; }
.pk-wb-status.is-draft { background:#fff0d7; color:#8a5e0e; }
.pk-wb-status.is-applied { background:#eaf0f6; color:#42627c; }
.pk-wb-status.is-saved { background:#e4f3eb; color:#286b4f; }
.pk-wb-steps { padding-left:22px; font-size:12px; }
.pk-wb-steps li { padding:8px 0 8px 3px; border-bottom:1px solid #e7eded; }
.pk-wb-save-actions { display:flex; flex-wrap:wrap; gap:8px; align-items:center; }
.pk-grid-toolbar { align-items:flex-end !important; gap:10px; }
.pk-grid-toolbar > .shiny-input-container { margin-bottom:8px; max-width:100%; }
.pk-grid-toolbar > .pk-grid-column-picker { border:0; margin:0 0 8px; padding:0; }
.pk-grid-column-picker > summary { padding:8px 10px; border:1px solid var(--pk-line); border-radius:5px; background:#fff; }
.pk-grid-columns { padding:10px; border:1px solid var(--pk-line); background:#fff; }
.pk-grid > .shiny-text-output { color:var(--pk-muted); font-size:11px; margin:4px 0 8px; }
.pk-grid-pagination { margin-top:10px; font-size:12px; }
.pk-grid-pagination > .shiny-input-container { margin-bottom:0; width:120px; }
.pk-wb-catalog .shiny-options-group { display:flex; flex-wrap:wrap; gap:7px; }
.pk-wb-catalog .radio { padding:0; margin:0; max-width:100%; }
.pk-wb-catalog .radio label { display:flex; align-items:center; gap:8px; margin:0; padding:9px 10px; border:1px solid #dce4e6; border-radius:7px; font-size:12px; }
.pk-wb-catalog .radio label:hover { background:#f3f7f7; }
.pk-wb-catalog .radio label:has(input:checked) { background:#e9f4f2; border-color:#b6d5cf; color:#185b60; }
.pk-wb-catalog input[type=radio] { accent-color:var(--pk-accent); position:static; float:none; margin:0; flex:0 0 auto; }
.pk-data-label { display:block; font-weight:700; overflow-wrap:anywhere; }
.pk-data-meta { display:block; color:var(--pk-muted); font-size:11px; margin-top:3px; }
.pk-workbench .shiny-input-container,.pk-analysis .shiny-input-container,.pk-reports .shiny-input-container { width:100%; max-width:100%; }
.pk-workbench .form-group,.pk-analysis .form-group,.pk-reports .form-group { margin-bottom:14px; }
.pk-workbench label,.pk-analysis label,.pk-reports label { font-size:12px; font-weight:600; margin-bottom:5px; line-height:1.5; }
.pk-workbench .form-control,.pk-workbench .selectize-input,.pk-analysis .form-control,.pk-analysis .selectize-input,.pk-reports .form-control,.pk-reports .selectize-input { font-size:13px; border:1px solid #b9c9cc; border-radius:5px; min-height:36px; box-shadow:none; }
.pk-workbench .selectize-dropdown,.pk-analysis .selectize-dropdown,.pk-reports .selectize-dropdown { z-index:1060; font-size:13px; }
.pk-workbench .selectize-control,.pk-analysis .selectize-control { margin-bottom:0; }
.pk-workbench .btn,.pk-analysis .btn,.pk-reports .btn { border:1px solid #b9c9cc; border-radius:5px; font-size:12px; padding:7px 11px; min-height:36px; white-space:normal; line-height:1.6; }
.pk-workbench .btn-primary,.pk-analysis .btn-primary,.pk-reports .btn-primary { color:#fff; background:var(--pk-accent); border-color:var(--pk-accent); font-weight:650; }
.pk-workbench .btn-primary:hover,.pk-analysis .btn-primary:hover,.pk-reports .btn-primary:hover { background:#115560; border-color:#115560; }
.pk-workbench .btn[disabled],.pk-analysis .btn[disabled],.pk-reports .btn[disabled] { opacity:.55; cursor:not-allowed; }
.pk-workbench .pk-card,.pk-analysis .pk-card,.pk-reports .pk-card { padding:18px; background:#fff; border:1px solid var(--pk-line); border-radius:8px; min-width:0; }
.pk-workbench .pk-actions,.pk-analysis .pk-actions,.pk-reports .pk-actions { display:flex; align-items:center; gap:8px; flex-wrap:wrap; }
.pk-workbench .pk-caption,.pk-analysis .pk-caption,.pk-reports .pk-caption { font-size:12px; line-height:1.7; color:var(--pk-muted); margin:6px 0 12px; }
.pk-workbench .pk-notice,.pk-analysis .pk-notice,.pk-reports .pk-notice { padding:10px 12px; border:1px solid #bcd3d9; border-left:3px solid #568e9a; border-radius:6px; background:#f1f7f8; font-size:12px; overflow-wrap:anywhere; }
.pk-workbench .pk-notice-error,.pk-reports .pk-notice-error { background:#fff2f0; color:#9b3029; border-color:#e6b4af; border-left-color:#bc443b; }
.pk-workbench .pk-notice-success,.pk-reports .pk-notice-success { background:#edf7f2; color:#256148; border-color:#b1d4c0; border-left-color:#318260; }
.pk-workbench .pk-notice-warning,.pk-reports .pk-notice-warning { background:#fff8eb; color:#805c13; border-color:#e4cf9a; border-left-color:#a87c24; }
.pk-workbench .pk-empty,.pk-reports .pk-empty { padding:30px 22px; min-height:160px; text-align:center; border:1px dashed #c4d2d5; border-radius:7px; background:#fafcfc; }
.pk-workbench .pk-empty p,.pk-reports .pk-empty p { color:var(--pk-muted); font-size:13px; margin:0 auto; max-width:50ch; }
.pk-workbench .pk-metrics { display:grid; grid-template-columns:repeat(auto-fit,minmax(110px,1fr)); gap:8px; margin:0; }
.pk-workbench .pk-metric { padding:11px 12px; border:1px solid var(--pk-line); border-radius:6px; background:#fbfcfc; }
.pk-workbench .pk-metric dt { color:var(--pk-muted); font-size:11px; font-weight:600; }
.pk-workbench .pk-metric dd { margin:4px 0 0; font-size:21px; font-weight:700; font-variant-numeric:tabular-nums; }
.pk-workbench .pk-table-scroll,.pk-analysis .pk-table-scroll { width:100%; max-width:100%; max-height:max(240px,calc(100vh - 560px)); min-height:160px; overflow:auto; border:1px solid var(--pk-line); border-radius:6px; scrollbar-gutter:stable; }
.pk-workbench .pk-table-scroll table,.pk-analysis .pk-table-scroll table { width:max-content; min-width:100%; margin:0; font-size:12px; font-variant-numeric:tabular-nums; }
.pk-workbench .pk-table-scroll table,.pk-workbench .pk-table-scroll th,.pk-workbench .pk-table-scroll td,.pk-analysis .pk-table-scroll table,.pk-analysis .pk-table-scroll th,.pk-analysis .pk-table-scroll td { font-family:inherit !important; }
.pk-workbench .pk-table-scroll th,.pk-workbench .pk-table-scroll td,.pk-analysis .pk-table-scroll th,.pk-analysis .pk-table-scroll td { padding:9px 12px; white-space:nowrap; border-bottom:1px solid #e8eeee; }
.pk-workbench .pk-table-scroll thead th,.pk-analysis .pk-table-scroll thead th { position:sticky; top:0; z-index:1; color:#4c6269; font-size:11px; background:#eff4f4; }
.pk-workbench .pk-table-scroll tbody tr:nth-child(even),.pk-analysis .pk-table-scroll tbody tr:nth-child(even) { background:#fafcfc; }
.pk-workbench .pk-table-scroll tbody tr:hover,.pk-analysis .pk-table-scroll tbody tr:hover { background:#edf6f4; }
.pk-workbench details,.pk-analysis details,.pk-reports details { border-top:1px solid var(--pk-line); margin-top:16px; padding-top:12px; }
.pk-workbench summary,.pk-analysis summary,.pk-reports summary { color:#41646b; font-size:12px; font-weight:600; cursor:pointer; padding:4px 0; }
.pk-workbench details pre,.pk-analysis details pre,.pk-reports details pre { max-height:240px; overflow:auto; margin-top:10px; font-size:11px; }
.pk-workbench .pk-condition-list { padding-left:22px; margin:0; font-size:12px; }
.pk-workbench .pk-condition-list li { padding:7px 0 7px 3px; overflow-wrap:anywhere; }
.pk-wb-savebar,.pk-analysis-savebar { position:sticky; bottom:0; z-index:10; display:flex; flex-wrap:wrap; align-items:center; justify-content:space-between; gap:10px; padding:12px 14px; border:1px solid var(--pk-line); border-radius:7px; background:#fff; box-shadow:0 -3px 12px #29494708; }
.pk-wb-savebar .shiny-input-container { width:min(240px,100%); margin:0; }
.pk-analysis-savebar > span { color:var(--pk-muted); font-size:12px; }
.pk-workbench :focus-visible,.pk-analysis :focus-visible,.pk-reports :focus-visible,.pk-analysis-context :focus-visible,.pk-import-settings summary:focus-visible { outline:3px solid #bd7c14; outline-offset:3px; }
.pk-import-settings { margin-top:20px; padding:0; background:#fff; border:1px solid var(--pk-line); border-radius:8px; }
.pk-import-settings > summary { padding:16px 18px; font-size:13px; font-weight:650; color:#3e626a; cursor:pointer; }
.pk-import-intro { padding:0 18px; color:var(--pk-muted); font-size:12px; }
.pk-import-settings .pk-workspace { padding-top:0; }
.pk-teal-host { margin:0 -12px; }
.pk-extensions .pk-teal-host { margin:18px 0 0; padding:12px; background:white; border:1px solid var(--pk-line); border-radius:8px; }
.pk-extensions .pk-teal-host:empty { display:none; }
.pk-extensions .shiny-plot-output img { max-width:100%; height:auto; }
.pk-workspace label,.pk-workspace th { font-family:inherit; }
.pk-teal-host > .container-fluid { padding:0; }
.pk-analysis,.pk-reports { padding:22px 20px 30px; }
.pk-page-heading { display:flex; flex-wrap:wrap; justify-content:space-between; gap:12px; align-items:flex-start; margin-bottom:20px; }
.pk-page-heading p { color:var(--pk-muted); font-size:13px; margin:0; }
.pk-page-heading > .shiny-text-output { color:var(--pk-muted); font-size:11px; }
.pk-eyebrow { font-size:10px; letter-spacing:.12em; color:var(--pk-muted); font-weight:700; margin-bottom:6px; }
.pk-analysis-layout { display:grid; grid-template-columns:280px minmax(0,1fr); gap:18px; align-items:start; }
.pk-analysis-results { display:flex; flex-direction:column; gap:16px; min-width:0; }
.pk-status-message { font-size:12px; padding:10px 12px; background:#edf5f4; border:1px solid #d2e4e0; border-radius:6px; color:#395b5c; }
.pk-analysis-warning { font-size:12px; color:#805c13; white-space:pre-line; }
.pk-analysis .pk-table-scroll { max-height:390px; }
.pk-reports > .pk-status-message { margin-bottom:18px; }
.pk-report-layout { display:grid; grid-template-columns:minmax(0,1.1fr) minmax(0,1fr); gap:20px; align-items:start; }
.pk-status-pill { display:inline-block; padding:3px 8px; color:#2c7158; background:#e5f4ec; border-radius:4px; font-size:10px; font-weight:700; }
.pk-run-summary h3 { margin-top:12px; font-size:18px; }
.pk-run-summary p { margin:8px 0; font-size:12px; }
.pk-run-id { overflow-wrap:anywhere; color:var(--pk-muted); font-family:ui-monospace,monospace; font-size:11px !important; }
.pk-reports pre { white-space:pre-wrap; overflow-wrap:anywhere; border:1px solid var(--pk-line); padding:10px; background:#f8fafb; font-size:11px; border-radius:5px; }
.pk-form-grid { display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); gap:0 16px; min-width:0; }
.pk-form-grid > * { min-width:0; }
.pk-blq-matrix { border:1px solid var(--pk-line); border-radius:7px; padding:10px 12px; margin:10px 0 16px; }
.pk-blq-rule-row { display:grid; grid-template-columns:104px minmax(0,1fr) 64px; align-items:center; gap:10px; min-width:0; }
.pk-blq-rule-row > strong,.pk-blq-rule-label { font-size:12px; }
.pk-blq-rule-label { display:contents; }
.pk-blq-rule-label > strong { grid-column:1; grid-row:1; }
.pk-blq-rule-count { grid-column:3; grid-row:1; }
.pk-blq-rule-row > .shiny-input-radiogroup { grid-column:2; grid-row:1; }
.pk-blq-count-caption { position:absolute; width:1px; height:1px; overflow:hidden; clip:rect(0,0,0,0); white-space:nowrap; }
.pk-blq-rule-row .form-group { margin:4px 0; }
.pk-blq-rule-row .shiny-options-group { display:flex; flex-wrap:wrap; gap:4px; }
.pk-blq-rule-row .radio-inline { position:relative; display:inline-flex; align-items:center; gap:6px; margin:0; padding:7px 9px; border:1px solid #b9c9cc; border-radius:5px; font-size:12px; line-height:1.5; white-space:nowrap; cursor:pointer; background:#fff; }
.pk-blq-rule-row .radio-inline:hover { background:#f3f7f7; }
.pk-blq-rule-row .radio-inline:has(input:checked) { background:#e9f4f2; border-color:#176b79; color:#155d69; box-shadow:inset 0 0 0 1px #176b79; }
.pk-blq-rule-row .radio-inline:has(input:focus-visible) { outline:3px solid #bd7c14; outline-offset:2px; }
.pk-blq-rule-row .radio-inline + .radio-inline { margin-left:0; }
.pk-blq-rule-row .shiny-options-group > label.radio-inline > input[type=radio] { position:static !important; float:none; margin:0 !important; width:14px; height:14px; accent-color:#176b79; flex:0 0 14px; }
.pk-preparation-apply { display:flex; justify-content:flex-end; flex-wrap:wrap; gap:8px; padding-top:8px; border-top:1px solid var(--pk-line); }
.pk-preparation-apply .btn { min-width:160px; }
.pk-preparation-card .shiny-text-output { font-size:12px; color:var(--pk-muted); }
.pk-blq-rule-heading { padding-bottom:6px; color:var(--pk-muted); }
.pk-wb-sidebar-footer .btn { margin-bottom:6px; }
.pk-rule-label { display:inline-block; white-space:normal; min-width:3em; border-radius:4px; padding:2px 7px; background:#edf3f2; color:#365b60; }
.pk-rule-label.is-replaced { background:#fff1d5; color:#805c13; }
.pk-rule-label.is-warning { background:#faeae5; color:#8b4333; }
.pk-workspace .pk-rule-label.is-success,.pk-workbench .pk-rule-label.is-success { background:#e5f4ec; color:#21633e; }
.pk-workspace .pk-rule-label.is-qc-exceeds,.pk-workbench .pk-rule-label.is-qc-exceeds { background:#fff1d5; color:#805c13; }
.pk-workspace .pk-rule-label.is-qc-unavailable,.pk-workbench .pk-rule-label.is-qc-unavailable { background:#faeae5; color:#8b3328; }
.pk-workspace .pk-rule-label.is-qc-disabled,.pk-workbench .pk-rule-label.is-qc-disabled { background:#edf0f2; color:#52616a; }
.pk-workbench .pk-table-scroll td:has(.pk-rule-label),.pk-workspace .pk-table-scroll td:has(.pk-rule-label) { max-width:280px; white-space:normal; }
.pk-workbench .pk-visually-hidden { position:absolute; width:1px; height:1px; margin:-1px; padding:0; overflow:hidden; clip:rect(0,0,0,0); white-space:nowrap; border:0; }
.pk-draft-preview { border-top:1px solid var(--pk-line); margin-top:14px; padding-top:14px; }
.pk-draft-preview .pk-table-scroll { min-height:0; max-height:260px; }
:root { scroll-padding-bottom:var(--pk-nca-bottom-clearance,0px); }
body.pk-nca-runbar-visible { padding-bottom:var(--pk-nca-bottom-clearance,0px); }
.pk-workspace.pk-nca { padding-bottom:160px; }
body.pk-nca-runbar-visible .pk-workspace.pk-nca { padding-bottom:24px; }
.pk-profile-qc-metadata { display:grid; grid-template-columns:repeat(auto-fit,minmax(190px,1fr)); gap:10px 16px; margin:12px 0; }
.pk-profile-qc-metadata > div { min-width:0; }
.pk-profile-qc-metadata dt { color:var(--pk-muted); font-size:12px; }
.pk-profile-qc-metadata dd { margin:4px 0 0; overflow-wrap:anywhere; font-size:13px; }
.pk-original-diagnostic pre { white-space:pre-wrap; overflow-wrap:anywhere; }
.pk-workspace.pk-nca .pk-layout { display:block; }
.pk-workspace.pk-nca .pk-controls { display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); gap:20px; align-items:start; }
.pk-workspace.pk-nca .pk-nca-qc-card { grid-column:1 / -1; }
.pk-workspace.pk-nca .pk-nca-full { grid-column:1 / -1; }
.pk-workspace.pk-nca .pk-nca-judgment-heading { margin:8px 0 -10px; }
.pk-nca-judgment .pk-section-title { color:#176b79; }
.pk-workspace.pk-nca .pk-nca-advanced { margin:0; }
.pk-nca-advanced > summary { color:var(--pk-ink); font-size:16px; font-weight:700; }
.pk-nca-advanced-changes { margin-inline-start:8px; color:var(--pk-muted); font-size:12px; font-weight:500; overflow-wrap:anywhere; }
.pk-nca-advanced-section { padding:14px 0; border-top:1px solid var(--pk-line); min-width:0; }
.pk-nca-advanced-section:last-child { padding-bottom:0; }
.pk-nca-advanced-content { margin-top:12px; }
.pk-nca-review > div { display:flex; flex-wrap:wrap; align-items:center; gap:8px; }
.pk-nca-review-status { display:inline-block; padding:4px 8px; background:#fff4db; color:#6b4508; border-radius:5px; font-size:12px; font-weight:700; }
.pk-nca-review [data-review-state="confirmed"] .pk-nca-review-status,.pk-nca-review [data-review-state="applied"] .pk-nca-review-status { background:#e5f4ec; color:#184b32; }
.pk-nca-review [data-review-state="unselected"] .pk-nca-review-status,.pk-nca-review [data-review-state="loading"] .pk-nca-review-status { background:#f1f5f6; color:#465961; }
.pk-workspace.pk-nca .pk-nca-runbar > .pk-nca-issues { flex:1 0 100%; max-height:24vh; overflow:auto; font-size:12px; }
.pk-nca-issues:empty { display:none; }
.pk-nca-issues ul { margin:0; padding-inline-start:18px; }
.pk-nca-issues a { text-decoration:underline; }
.pk-workspace.pk-nca .pk-nca-qc-grid { display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); gap:20px; min-width:0; }
.pk-workspace.pk-nca .pk-nca-qc-grid > * { min-width:0; }
.pk-workspace.pk-nca .pk-results { margin-top:20px; }
.pk-workspace.pk-nca .pk-results > .shiny-html-output:empty { margin:0; }
.pk-workspace.pk-nca .pk-period-warning { color:#8a5a00; }
.pk-workspace.pk-nca .pk-nca-runbar > .pk-period-run-hint { display:block; flex:1 0 100%; font-size:12px; line-height:18px; }
.pk-workspace.pk-nca .pk-nca-runbar > .pk-period-run-hint:empty { display:none; }
.pk-workspace.pk-nca .pk-nca-runbar { position:fixed; z-index:1030; left:20px; right:20px; bottom:12px; display:flex; align-items:center; flex-wrap:wrap; gap:12px; padding:12px 16px; background:#fff; border:1px solid #bfd2d3; border-radius:8px; box-shadow:0 -2px 14px #23343c16; }
.pk-nca-runbar > .shiny-text-output { margin-left:auto; color:var(--pk-muted); font-size:12px; }
.pk-protected { padding:10px 12px; border-radius:6px; background:#edf5f3; }
.pk-workspace.pk-nca .pk-card:has(.shiny-plot-output) { overflow:hidden; }
@media (max-width:1280px) {
  .pk-workbench { grid-template-columns:280px minmax(0,1fr); gap:16px; }
  .pk-context-note { flex-basis:100%; }
}
@media (max-width:900px) {
  .pk-workbench,.pk-analysis-layout,.pk-report-layout { grid-template-columns:minmax(0,1fr); }
  .pk-wb-sidebar { position:static; top:auto; max-height:min(560px,75vh); max-height:min(560px,75dvh); }
  .pk-workbench .pk-table-scroll { max-height:460px; }
  .pk-analysis-context { align-items:flex-start; padding:10px 14px; }
  .pk-analysis-context-content { column-gap:12px; }
  .pk-analysis,.pk-reports { padding:18px 10px 24px; }
  .navbar:has(.pk-brand) { padding:8px 14px; }
  .navbar:has(.pk-brand) .nav-link { padding:12px 14px; }
  .pk-workspace.pk-nca .pk-controls,.pk-workspace.pk-nca .pk-nca-qc-grid { grid-template-columns:minmax(0,1fr); }
  .pk-workbench { padding-bottom:150px; }
  .pk-wb-sidebar-footer { position:fixed; bottom:8px; left:12px; right:12px; z-index:1030; display:grid; grid-template-columns:1fr 1fr; gap:8px; border:1px solid #bfd2d3; border-radius:8px; box-shadow:0 -2px 14px #23343c16; }
  .pk-wb-sidebar-footer .pk-caption { grid-column:1 / -1; }
  .pk-wb-sidebar-footer .btn { height:100%; margin:0; }
}
@media (max-width:700px) {
  .pk-workbench .pk-blq-rule-row { grid-template-columns:64px minmax(0,1fr); gap:8px; }
  .pk-workbench .pk-blq-rule-label { display:flex; flex-direction:column; gap:3px; min-width:0; }
  .pk-workbench .pk-blq-rule-count { color:var(--pk-muted); font-size:11px; }
  .pk-workbench .pk-blq-count-caption { position:static; width:auto; height:auto; overflow:visible; clip:auto; white-space:normal; }
  .pk-workbench .pk-blq-rule-heading > strong:last-child { display:none; }
  .pk-workbench .pk-blq-rule-row .radio-inline { padding:6px; gap:4px; }
}
@media (max-width:560px) {
  .pk-data-page { padding:12px 0 20px; }
  .pk-analysis-context { flex-wrap:wrap; gap:8px; }
  .pk-context-label { flex-basis:100%; }
  .pk-workbench .pk-card,.pk-analysis .pk-card,.pk-reports .pk-card { padding:12px; }
  .pk-workbench .pk-metrics { grid-template-columns:repeat(2,minmax(0,1fr)); }
  .pk-wb-savebar,.pk-analysis-savebar { position:static; align-items:stretch; flex-direction:column; }
  .pk-wb-savebar .shiny-input-container { width:100%; }
  .pk-wb-savebar .btn,.pk-analysis-savebar .btn { width:100%; }
  .pk-page-heading h1 { font-size:23px; }
  .pk-form-grid { grid-template-columns:minmax(0,1fr); }
  .pk-blq-matrix { padding:8px; }
  .pk-blq-rule-row > strong,.pk-blq-rule-label { font-size:11px; }
  .pk-workspace.pk-nca .pk-nca-runbar { left:8px; right:8px; bottom:8px; gap:8px; padding:10px; }
  .pk-nca-runbar > .shiny-text-output { flex-basis:100%; margin:0; }
}
@media (prefers-reduced-motion:reduce) { .pk-workbench *,.pk-analysis *,.pk-reports * { scroll-behavior:auto !important; transition:none !important; } }
')),
  shiny::tags$script(shiny::HTML('
$(function() {
  var bars = new Set();
  var frame = null;
  var resizeObserver = typeof ResizeObserver === "function" ? new ResizeObserver(schedule) : null;
  var mutationObserver = typeof MutationObserver === "function" ? new MutationObserver(schedule) : null;
  function visibleBar(bar) {
    return bar.isConnected && bar.getClientRects().length && bar.getBoundingClientRect().height > 0;
  }
  function measure() {
    var visible = document.hidden ? [] : Array.from(document.querySelectorAll(".pk-workspace.pk-nca .pk-nca-runbar")).filter(visibleBar);
    bars.forEach(function(bar) {
      if (visible.indexOf(bar) < 0) {
        if (resizeObserver) resizeObserver.unobserve(bar);
        bars.delete(bar);
      }
    });
    visible.forEach(function(bar) {
      if (!bars.has(bar)) {
        bars.add(bar);
        if (resizeObserver) resizeObserver.observe(bar);
      }
    });
    var clearance = visible.reduce(function(bottom, bar) {
      return Math.max(bottom, window.innerHeight - bar.getBoundingClientRect().top + 16);
    }, 0);
    var value = Math.ceil(clearance) + "px";
    if (document.documentElement.style.getPropertyValue("--pk-nca-bottom-clearance") !== value)
      document.documentElement.style.setProperty("--pk-nca-bottom-clearance", value);
    document.body.classList.toggle("pk-nca-runbar-visible", clearance > 0);
  }
  function focusDelta(target, bar) {
    if (target.right <= bar.left || target.left >= bar.right || target.bottom <= bar.top - 12 || target.top >= bar.bottom) return 0;
    return Math.max(0, target.bottom - (bar.top - 12));
  }
  function revealFocus() {
    var target = document.activeElement;
    if (!target || !target.getBoundingClientRect || target === document.body) return;
    bars.forEach(function(bar) {
      if (!visibleBar(bar) || bar.contains(target)) return;
      var delta = focusDelta(target.getBoundingClientRect(), bar.getBoundingClientRect());
      if (delta > 0) window.scrollBy({top:delta, left:0, behavior:"instant"});
    });
  }
  function schedule() {
    if (frame !== null) return;
    frame = window.requestAnimationFrame(function() {
      frame = null;
      measure();
      revealFocus();
    });
  }
  function track() {
    if (document.hidden) { measure(); return; }
    if (mutationObserver) mutationObserver.observe(document.body, {subtree:true, childList:true, attributes:true, attributeFilter:["class", "style", "hidden"]});
    schedule();
  }
  document.addEventListener("focusin", schedule);
  document.addEventListener("visibilitychange", function() {
    if (document.hidden) {
      if (resizeObserver) resizeObserver.disconnect();
      if (mutationObserver) mutationObserver.disconnect();
      bars.clear();
      measure();
    } else track();
  });
  window.addEventListener("resize", schedule);
  if (window.visualViewport) window.visualViewport.addEventListener("resize", schedule);
  $(document).on("shown.bs.tab hidden.bs.tab shiny:connected shiny:value", schedule);
  track();
  function reviewChoice(status, value) {
    var data = status.dataset;
    var choice = {key:data.reviewKey, generation:Number(data.generation), value:value};
    // Let the ordinary Shiny binding deliver the selected value first.
    queueMicrotask(function() {
      Shiny.setInputValue(data.reviewEvent, choice, {priority:"event"});
    });
  }
  document.addEventListener("change", function(event) {
    if (!event.isTrusted || !event.target.matches("select")) return;
    var card = event.target.closest(".pk-nca-judgment");
    var status = card && card.querySelector("[data-review-key]");
    if (status && status.dataset.reviewKey !== "units" && status.dataset.reviewInput === event.target.id)
      reviewChoice(status, event.target.value);
  });
  document.addEventListener("click", function(event) {
    var confirm = event.target.closest("button[data-nca-confirm]");
    if (confirm && event.isTrusted) {
      var status = confirm.closest("[data-review-key]");
      var input = document.getElementById(status.dataset.reviewInput);
      if (status.dataset.reviewKey === "units") {
        var manual = document.getElementById(status.dataset.reviewManualInput);
        if (manual.value === status.dataset.reviewManualUnit) reviewChoice(status, true);
      } else if (input) reviewChoice(status, input.value);
    }
    var link = event.target.closest("a[data-nca-focus]");
    if (!link) return;
    var field = document.getElementById(link.dataset.ncaFocus);
    if (!field) return;
    event.preventDefault();
    for (var ancestor = field.parentElement; ancestor; ancestor = ancestor.parentElement)
      if (ancestor.tagName === "DETAILS") ancestor.open = true;
    var control = field.selectize ? field.selectize.$control_input[0] : field;
    control.scrollIntoView({block:"center", behavior:"auto"});
    control.focus({preventScroll:true});
  });
  Shiny.addCustomMessageHandler("pk-workbench-focus-tab", function(message) {
    window.requestAnimationFrame(function() {
      var tabs = document.getElementById(message.id);
      if (!tabs) return;
      var link = Array.from(tabs.querySelectorAll("a[data-value]")).find(function(node) {
        return node.getAttribute("data-value") === message.value;
      });
      if (link) link.focus({preventScroll:true});
    });
  });
  // Body-mounted menus must close when their input moves during page/pane scroll.
  document.addEventListener("scroll", function(event) {
    if (event.target !== document && !(event.target.matches && event.target.matches(".pk-wb-sidebar > .tabbable > .tab-content"))) return;
    document.querySelectorAll(".pk-wb-sidebar select").forEach(function(input) {
      if (input.selectize && input.selectize.isOpen) input.selectize.close();
    });
  }, true);
  Shiny.addCustomMessageHandler("pk-nca-results-scroll", function(message) {
    window.requestAnimationFrame(function() { window.requestAnimationFrame(function() {
      var target = document.getElementById(message.id);
      if (target) target.scrollIntoView({block: "start", behavior: "auto"});
    }); });
  });
  Shiny.addCustomMessageHandler("pk-open-import", function(message) {
    var panel = document.getElementById(message.id);
    if (!panel) return;
    panel.open = true;
    window.requestAnimationFrame(function() {
      panel.scrollIntoView({block: "start", behavior: "auto"});
      var summary = panel.querySelector("summary");
      if (summary) summary.focus();
    });
  });
});
'))))
