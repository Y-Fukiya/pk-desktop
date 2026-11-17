'use strict';
// Runs a new loopback-only Shiny fixture with artificial data. Never opens a PK
// Desktop runtime URL or reads an existing project's credentials or data.
const { chromium } = require('playwright');
const { browserLaunchOptions } = require('./nca055_browser_args.cjs');
const { spawn } = require('child_process');
const { mkdtemp, rm } = require('fs/promises');
const path = require('path');
const os = require('os');
const net = require('net');
const assert = require('assert');
const { chooseNcaPolicies } = require('./helper_nca_browser.cjs');
async function freePort() { return await new Promise((resolve,reject)=>{const s=net.createServer();s.on('error',reject);s.listen(0,'127.0.0.1',()=>{const p=s.address().port;s.close(()=>resolve(p));});}); }
async function main() {
 const root=path.resolve(process.env.PK_BROWSER_SOURCE || '.');
 const project=await mkdtemp(path.join(os.tmpdir(),'pk-browser-fixture-'));
 const port=await freePort();
 const server=spawn(process.env.PK_RSCRIPT || 'Rscript',['--vanilla',path.join(root,'apps/pk-desktop/tests/nca054_browser_app.R')],
   {cwd:root,env:{...process.env,PK_BROWSER_SOURCE:root,PK_BROWSER_PROJECT:project,PK_BROWSER_PORT:String(port)},stdio:['ignore','pipe','pipe']});
 let log='';server.stdout.on('data',d=>{log+=d;});server.stderr.on('data',d=>{log+=d;});
 let browser;
 try {
  browser=await chromium.launch(browserLaunchOptions(process.env));
  const page=await browser.newPage({viewport:{width:1440,height:900}});
  const url=`http://127.0.0.1:${port}/`;
  let ready=false;
  for(let n=0;n<60;n++) {if(server.exitCode!==null) throw new Error(`Synthetic Shiny server exited: ${log}`);try{await page.goto(url,{waitUntil:'domcontentloaded',timeout:2000});ready=true;break;}catch(e){await new Promise(r=>setTimeout(r,250));}}
  if(!ready) throw new Error(`Synthetic Shiny server not ready: ${log}`);
  await page.waitForFunction(()=>window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected() && window.PK_TEST_SOURCE);
  await page.waitForFunction(()=>{let x=document.querySelector('#nca-source');return x && x.selectize && x.selectize.options[window.PK_TEST_SOURCE];});
  await page.evaluate(()=>document.querySelector('#nca-source').selectize.setValue(window.PK_TEST_SOURCE));
  await page.waitForFunction(()=>{let x=document.querySelector('#nca-groups');return x && x.selectize && x.selectize.options.PERIOD;});
  await page.evaluate(()=>document.querySelector('#nca-groups').selectize.setValue(['PERIOD']));
  // Shiny renders and rebinds group-role controls asynchronously; wait for
  // them to settle and retry the setup if a rebind temporarily resets it.
  const role=page.locator('select[id^="nca-group_role_column_"]');await role.waitFor();
  await page.waitForTimeout(750);
  const manual=page.locator('#nca-manual_period_order');
  let configured=false;
  for(let i=0;i<5;i++) {
   await role.selectOption('period');
   await page.waitForTimeout(500);
   if(await manual.count() && await role.inputValue()==='period') {configured=true;break;}
  }
  if(!configured) throw new Error('Could not configure synthetic PERIOD role');
  await manual.waitFor();
  assert.equal(await page.locator('#nca-dosing_context').inputValue(), '');
  await page.locator('#nca-dosing_context').selectOption('ongoing');
  await page.waitForFunction(()=>document.querySelector('#nca-review_dosing [data-review-state]')?.dataset.reviewState==='unconfirmed');
  await page.locator('#nca-confirm_dosing').press('Enter');
  await page.waitForFunction(()=>document.querySelector('#nca-review_dosing [data-review-state]')?.dataset.reviewState==='confirmed');
  assert.equal(await page.locator('#nca-blq').inputValue(), '');
  await page.locator('#nca-blq').press('0');
  await page.waitForFunction(()=>document.querySelector('#nca-review_blq [data-review-state]')?.dataset.reviewState==='confirmed');
  assert.equal(await page.locator('#nca-blq').inputValue(), 'zero');
  await chooseNcaPolicies(page);
  const button=page.locator('#nca-run');await button.waitFor();
  const hint=page.locator('#nca-period_order_run_hint');
  await page.waitForFunction(()=>getComputedStyle(document.querySelector('#nca-period_order_run_hint')).display==='none');
  const before=await page.evaluate(()=>({bar:document.querySelector('.pk-nca-runbar').getBoundingClientRect().height,y:document.querySelector('#nca-run').getBoundingClientRect().top}));
  await manual.check();
  await page.waitForFunction(()=>{let x=document.querySelector('#nca-period_order_values');return x && x.selectize && x.selectize.options['1'];});
  await page.evaluate(()=>document.querySelector('#nca-period_order_values').selectize.setValue(['1']));
  await hint.waitFor({state:'visible',timeout:10000});
  assert.match(await hint.innerText(),/期の値をすべて選択/);
  assert.equal(await button.isDisabled(),true);
  assert.equal(await button.getAttribute('aria-describedby'),'nca-period_order_run_hint');
  const during=await page.evaluate(()=>({bar:document.querySelector('.pk-nca-runbar').getBoundingClientRect().height,y:document.querySelector('#nca-run').getBoundingClientRect().top}));
  assert(during.bar>before.bar,`warning did not expand bar: ${JSON.stringify({before,during})}`);
  assert(Math.abs(during.y-before.y)<2,`button moved: ${JSON.stringify({before,during})}`);
  await page.evaluate(()=>document.querySelector('#nca-period_order_values').selectize.setValue(['1','2','3','4']));
  await hint.waitFor({state:'hidden',timeout:10000});
  await page.waitForFunction(()=>!document.querySelector('#nca-run').disabled);
  const after=await page.evaluate(()=>({bar:document.querySelector('.pk-nca-runbar').getBoundingClientRect().height,y:document.querySelector('#nca-run').getBoundingClientRect().top,state:document.querySelector('#nca-run_state').innerText}));
  assert(Math.abs(after.bar-before.bar)<2 && Math.abs(after.y-before.y)<2,JSON.stringify({before,after}));
  assert.match(after.state,/未計算/);
  console.log('PASS synthetic browser: warning becomes visible, run disabled, aria link, stable button, recovery, no calculation');
  console.log(JSON.stringify({before,during,after}));
 } finally {if(browser) await browser.close();if(server.exitCode===null && server.signalCode===null) await new Promise(r=>{server.once('close',r);server.kill('SIGTERM');});await rm(project,{recursive:true,force:true});}
}
main().catch(e=>{console.error(e.stack || e);process.exitCode=1;});
