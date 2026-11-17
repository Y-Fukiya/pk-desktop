'use strict';
// Only synthetic data and a disposable project are used by this test.
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
  const project=await mkdtemp(path.join(os.tmpdir(),'pk-warning-fixture-'));
  const port=await freePort();
  const server=spawn(process.env.PK_RSCRIPT || 'Rscript',['--vanilla',path.join(root,'apps/pk-desktop/tests/nca059_browser_app.R')],
    {cwd:root,env:{...process.env,PK_BROWSER_SOURCE:root,PK_BROWSER_PROJECT:project,PK_BROWSER_PORT:String(port)},stdio:['ignore','pipe','pipe']});
  let log=''; server.stdout.on('data',d=>{log+=d;}); server.stderr.on('data',d=>{log+=d;}); let browser;
  try {
    browser=await chromium.launch(browserLaunchOptions(process.env));
    const page=await browser.newPage({viewport:{width:1440,height:900}}); let ready=false;
    for(let n=0;n<60;n++){if(server.exitCode!==null)throw new Error(`Fixture exited: ${log}`);try{await page.goto(`http://127.0.0.1:${port}/`,{waitUntil:'domcontentloaded',timeout:2000});ready=true;break;}catch(e){await new Promise(r=>setTimeout(r,250));}}
    if(!ready)throw new Error(`Fixture not ready: ${log}`);
    await page.waitForFunction(()=>window.Shiny?.shinyapp?.isConnected() && window.PK_TEST_SOURCE && document.querySelector('#nca-source')?.selectize?.options[window.PK_TEST_SOURCE]);
    await page.evaluate(()=>document.querySelector('#nca-source').selectize.setValue(window.PK_TEST_SOURCE));
    await page.waitForFunction(()=>document.querySelector('#nca-time')?.value==='TIME');
    await page.locator('[data-pk-language=en]').click();
    await page.evaluate(()=>document.querySelector('#nca-terminal_method').selectize.setValue('auto'));
    await chooseNcaPolicies(page, { dosing: 'single' });
    await page.locator('#nca-dose_history_confirmed').check();
    await page.locator('#nca-advanced-summary').click();
    await page.locator('#nca-terminal_min_span_ratio').fill('100');
    await page.locator('#nca-run').click();
    await page.waitForFunction(()=>document.querySelector('#nca-run_state')?.innerText.includes('Calculated'),null,{timeout:30000});
    await page.waitForFunction(()=>document.querySelector('#nca-wide_table')?.innerText.includes('Terminal-phase span'),null,{timeout:30000});
    const wide=await page.locator('#nca-wide_table').innerText();
    assert(!wide.includes('Untranslated diagnostic; review the Japanese'));
    const summary=page.locator('#nca-results details.pk-warning-group').filter({hasText:'Terminal-phase span'}).first();
    assert.equal(await summary.count(),1);
    assert.match(await summary.locator('summary').innerText(),/6\s+analysis units/);
    assert.equal(await summary.evaluate(el=>el.open),false);
    await summary.locator('summary').click();
    for(let i=1;i<=6;i++)assert.match(await summary.innerText(),new RegExp(`ID=S0${i}`));
    const before=await page.locator('#nca-shared_nca').innerText();
    await page.locator('[data-pk-language=ja]').click();
    await page.waitForFunction(()=>document.querySelector('[data-pk-language=ja]').getAttribute('aria-pressed')==='true');
    await page.locator('[data-pk-language=en]').click();
    await page.waitForFunction(()=>document.querySelector('[data-pk-language=en]').getAttribute('aria-pressed')==='true');
    assert.equal(await page.locator('#nca-shared_nca').innerText(),before);
    console.log('PASS: coded diagnoses, grouped warning counts and identities, language switching');
  } finally { if(browser)await browser.close(); if(server.exitCode===null && server.signalCode===null)await new Promise(r=>{server.once('close',r);server.kill('SIGTERM');}); await rm(project,{recursive:true,force:true}); }
}
main().catch(e=>{console.error(e.stack||e);process.exitCode=1;});
