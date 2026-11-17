'use strict';
// Browser regression on a dedicated synthetic fixture, not a user's project.
const { chromium } = require('playwright');
const { browserLaunchOptions } = require('./nca055_browser_args.cjs');
const { spawn } = require('child_process');
const { mkdtemp, rm } = require('fs/promises');
const path = require('path');
const os = require('os');
const net = require('net');
const assert = require('assert');
const { chooseNcaPolicies } = require('./helper_nca_browser.cjs');
async function freePort() {return await new Promise((resolve,reject)=>{const s=net.createServer();s.on('error',reject);s.listen(0,'127.0.0.1',()=>{const p=s.address().port;s.close(()=>resolve(p));});});}
async function countJapaneseLines(page) {
  return await page.evaluate(() => document.body.innerText.split('\n').map(s=>s.trim()).filter(s=>s && /[\u3040-\u30ff\u3400-\u9fff]/.test(s)).length);
}
async function main() {
 const root=path.resolve(process.env.PK_BROWSER_SOURCE || '.');
 const project=await mkdtemp(path.join(os.tmpdir(),'pk-i18n-fixture-'));
 const port=await freePort();
 const server=spawn(process.env.PK_RSCRIPT || 'Rscript',['--vanilla',path.join(root,'apps/pk-desktop/tests/nca057_browser_app.R')],
   {cwd:root,env:{...process.env,PK_BROWSER_SOURCE:root,PK_BROWSER_PROJECT:project,PK_BROWSER_PORT:String(port)},stdio:['ignore','pipe','pipe']});
 let log='';server.stdout.on('data',d=>{log+=d;});server.stderr.on('data',d=>{log+=d;});let browser;
 try {
  browser=await chromium.launch(browserLaunchOptions(process.env));
  const page=await browser.newPage({viewport:{width:1440,height:900}});let ready=false;
  for(let n=0;n<60;n++){if(server.exitCode!==null)throw new Error(`Fixture exited: ${log}`);try{await page.goto(`http://127.0.0.1:${port}/`,{waitUntil:'domcontentloaded',timeout:2000});ready=true;break;}catch(e){await new Promise(r=>setTimeout(r,250));}}
  if(!ready)throw new Error(`Fixture not ready: ${log}`);
  await page.waitForFunction(()=>window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected() && window.PK_TEST_SOURCE);
  await page.waitForFunction(()=>document.querySelector('#nca-source')?.selectize?.options[window.PK_TEST_SOURCE]);
  await page.evaluate(()=>document.querySelector('#nca-source').selectize.setValue(window.PK_TEST_SOURCE));
  await page.waitForFunction(()=>document.querySelector('#nca-time')?.value==='TIME');
  await page.locator('[data-pk-language=en]').click();
  await page.waitForFunction(()=>document.querySelector('[data-pk-language=en]').getAttribute('aria-pressed')==='true');
  const before=await countJapaneseLines(page);
  assert.equal(await page.locator('#nca-dosing_context option[value=""]').textContent(),'Select a dosing context');
  assert.equal(await page.locator('#nca-c0_method option[value=""]').textContent(),'Select a policy');
  assert.equal(await page.locator('#nca-blq option[value=""]').textContent(),'Select a policy');
  // Budgets apply only to visible lines in this fixed synthetic fixture; they
  // are not comparable to the independent reviewer's text-node counts.
  assert(before<=12,`Japanese-line budget exceeded before NCA: ${before}/12`);
  await page.evaluate(()=>document.querySelector('#nca-time_unit').selectize.setValue('min'));
  await page.waitForFunction(()=>document.querySelector('#nca-end').value==='1440');
  await page.evaluate(()=>document.querySelector('#nca-time_unit').selectize.setValue('h'));
  await page.waitForFunction(()=>document.querySelector('#nca-end').value==='24');
  await page.evaluate(()=>document.querySelector('#nca-time_unit').selectize.setValue('day'));
  await page.waitForFunction(()=>document.querySelector('#nca-end').value==='1');
  await page.evaluate(()=>document.querySelector('#nca-time_unit').selectize.setValue('min'));
  await page.waitForFunction(()=>document.querySelector('#nca-end').value==='1440');
  await page.locator('#nca-last_observation').uncheck();
  await page.evaluate(()=>document.querySelector('#nca-terminal_method').selectize.setValue('auto'));
  await chooseNcaPolicies(page, { dosing: 'single' });
  await page.locator('#nca-dose_history_confirmed').check();
  await page.locator('#nca-run').click();
  await page.waitForFunction(()=>document.querySelector('#nca-run_state')?.innerText.includes('Calculated'),null,{timeout:30000});
  await page.waitForFunction(()=>document.body.innerText.includes('Fewer than three positive observed quantifiable concentrations after Tmax'),null,{timeout:30000});
  await page.locator('#nca-plot img').waitFor({state:'visible',timeout:30000});
  await page.waitForTimeout(500);
  assert.match(await page.locator('body').innerText(),/Fewer than three positive observed quantifiable concentrations after Tmax/);
  const after=await countJapaneseLines(page);
  assert(after<=28,`Japanese-line budget exceeded after NCA: ${after}/28`);
  const saved=await page.evaluate(()=>({
    values:JSON.stringify(Object.fromEntries(Object.entries(Shiny.shinyapp.$inputValues).filter(([k])=>!k.includes('pk_ui_language')))),
    row:[...document.querySelectorAll('table')].map(t=>t.innerText).find(t=>t.includes('未指定')&&t.includes('CMAX')),
    revision:document.querySelector('#nca-shared_nca')?.innerText.match(/(?:共有リビジョン|Shared revision)\s+(\d+)/)?.[1]
  }));
  assert(saved.revision, 'Shared revision must be visible before comparing it');
  assert(saved.row && saved.row.includes('全体'));
  await page.locator('[data-pk-language=ja]').click();
  await page.waitForFunction(()=>document.querySelector('[data-pk-language=ja]').getAttribute('aria-pressed')==='true');
  const switched=await page.evaluate(()=>({
    values:JSON.stringify(Object.fromEntries(Object.entries(Shiny.shinyapp.$inputValues).filter(([k])=>!k.includes('pk_ui_language')))),
    row:[...document.querySelectorAll('table')].map(t=>t.innerText).find(t=>t.includes('未指定')&&t.includes('CMAX')),
    revision:document.querySelector('#nca-shared_nca')?.innerText.match(/(?:共有リビジョン|Shared revision)\s+(\d+)/)?.[1]
  }));
  // Only system status/diagnostic labels may change; synthetic ID/group and PK
  // values must remain untouched. Compare Shiny inputs and the core cells.
  assert.equal(saved.values,switched.values);
  assert.equal(saved.revision,switched.revision);
  assert(switched.row.includes('未指定') && switched.row.includes('全体') && switched.row.includes('10') && switched.row.includes('24'));
  await page.locator('[data-pk-language=en]').click();
  await page.reload({waitUntil:'domcontentloaded'});
  await page.waitForFunction(()=>document.querySelector('[data-pk-language=en]')?.getAttribute('aria-pressed')==='true');
  console.log(JSON.stringify({status:'PASS',japanese_visible_lines_before:before,japanese_visible_lines_after:after,budgets:[12,28],unit_roundtrip:'h/min/day/min',language_reloaded:true}));
 } finally {if(browser)await browser.close();if(server.exitCode===null && server.signalCode===null)await new Promise(r=>{server.once('close',r);server.kill('SIGTERM');});await rm(project,{recursive:true,force:true});}
}
main().catch(e=>{console.error(e.stack||e);process.exitCode=1;});
