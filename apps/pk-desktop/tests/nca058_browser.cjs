'use strict';
// Synthetic browser regression: never opens an existing project.
const { chromium } = require('playwright');
const { browserLaunchOptions } = require('./nca055_browser_args.cjs');
const { spawn } = require('child_process');
const { mkdtemp, readFile, rm } = require('fs/promises');
const path = require('path');
const os = require('os');
const net = require('net');
const assert = require('assert');
const { chooseNcaPolicies, confirmDisplayedChoices } = require('./helper_nca_browser.cjs');
async function freePort() { return await new Promise((resolve,reject)=>{const s=net.createServer();s.on('error',reject);s.listen(0,'127.0.0.1',()=>{const p=s.address().port;s.close(()=>resolve(p));});}); }
async function main() {
  const root=path.resolve(process.env.PK_BROWSER_SOURCE || '.');
  const project=await mkdtemp(path.join(os.tmpdir(),'pk-boundary-fixture-'));
  const port=await freePort();
  const server=spawn(process.env.PK_RSCRIPT || 'Rscript',['--vanilla',path.join(root,'apps/pk-desktop/tests/nca058_browser_app.R')],
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
    await page.locator('#nca-last_observation').uncheck();
    await page.locator('#nca-start').fill('0.5');
    await page.locator('#nca-end').fill('7');
    await chooseNcaPolicies(page);
    await page.waitForFunction(()=>!document.querySelector('#nca-run').disabled);
    await page.locator('#nca-end').fill('');
    await page.waitForFunction(()=>document.querySelector('#nca-run').disabled && document.querySelector('#nca-period_order_run_hint').innerText.includes('Enter an analysis end'));
    await page.locator('#nca-end').fill('-5');
    await page.waitForFunction(()=>document.querySelector('#nca-run').disabled && document.querySelector('#nca-period_order_run_hint').innerText.includes('later than'));
    await page.locator('#nca-end').fill('7');
    await page.waitForFunction(()=>!document.querySelector('#nca-run').disabled);
    await page.locator('#nca-run').click();
    await page.waitForFunction(()=>document.querySelector('#nca-run_state')?.innerText.includes('Calculated'),null,{timeout:30000});
    await page.waitForFunction(()=>document.querySelector('#nca-wide_table')?.innerText.includes('CMAX'),null,{timeout:30000});
    assert.match(await page.locator('#nca-notice').innerText(), /NCA completed/);
    const before=await page.locator('#nca-wide_table').innerText();
    const revision=await page.locator('#nca-shared_nca').innerText();
    const advanced = page.locator('#nca-advanced');
    const issues = page.locator('#nca-first_use_issues');
    const stateFile = path.join(project, 'nca-state.json');
    const sharedBefore = await readFile(stateFile, 'utf8');
    const derived = page.locator('#nca-derived_enabled');
    const advancedSummary = page.locator('#nca-advanced-summary');
    await advancedSummary.click();
    await derived.check();
    await page.waitForFunction(() => !window.Shiny.shinyapp.isConnected() ||
      (document.querySelector('#nca-run')?.disabled &&
        document.querySelector('#nca-first_use_issues')?.innerText.includes('Confirm that the dose column contains actual administered doses.') &&
        document.querySelector('#nca-run_state')?.innerText.includes('Settings changed')));
    assert.equal(await page.evaluate(() => window.Shiny.shinyapp.isConnected()), true, log || 'An incomplete dose draft disconnected the Shiny session');
    await issues.getByText('Confirm that the dose column contains actual administered doses.').waitFor();
    assert.equal(await page.locator('#nca-run').isDisabled(), true);
    assert.equal(await readFile(stateFile, 'utf8'), sharedBefore, 'An incomplete dose draft changed shared project state');
    await derived.uncheck();
    await page.waitForFunction(() => document.querySelector('#nca-wide_table')?.innerText.includes('CMAX'));
    assert.equal(await page.locator('#nca-wide_table').innerText(), before);
    assert.equal(await readFile(stateFile, 'utf8'), sharedBefore, 'Discarding the incomplete dose draft changed shared project state');
    await advancedSummary.click();
    const lookBackIssue = issues.locator('li').filter({hasText:'Enter a positive look-back time for predose QC.'});
    const lookBack = page.locator('#nca-predose_qc_window_h');
    const predoseQc = page.locator('#nca-predose_qc');
    await issues.waitFor({state:'hidden'});
    assert.equal(await advanced.evaluate(node => node.open), false);
    await predoseQc.check();
    await lookBackIssue.waitFor({timeout:10000});
    assert.equal(await page.locator('#nca-run').isDisabled(), true);
    await lookBackIssue.getByRole('link').click();
    assert.equal(await advanced.evaluate(node => node.open), true);
    assert.equal(await lookBack.evaluate(node => node === document.activeElement), true);
    await lookBack.fill('2');
    await lookBackIssue.waitFor({state:'hidden'});
    await lookBack.fill('');
    await lookBackIssue.waitFor({timeout:10000});
    await predoseQc.uncheck();
    await confirmDisplayedChoices(page);
    await issues.waitFor({state:'hidden'});
    await page.locator('#nca-advanced-summary').click();
    for(const [unit,start,end] of [['min','30','420'],['day','0.0208333','0.291667'],['h','0.5','7'],['day','0.0208333','0.291667'],['min','30','420'],['h','0.5','7']]){
      await page.evaluate(unit=>document.querySelector('#nca-time_unit').selectize.setValue(unit),unit);
      await page.waitForFunction(([start,end])=>document.querySelector('#nca-start').value===start && document.querySelector('#nca-end').value===end,[start,end]);
      await page.waitForFunction(()=>document.querySelector('#nca-review_units [data-review-state]')?.dataset.reviewState==='unconfirmed');
      assert.equal(await page.locator('#nca-run').isDisabled(),true);
      await confirmDisplayedChoices(page);
      await page.waitForFunction(()=>!document.querySelector('#nca-run').disabled);
      assert.match(await page.locator('label[for=nca-end]').innerText(),new RegExp(`\\(${unit}\\)`));
    }
    await page.waitForFunction(()=>document.querySelector('#nca-wide_table')?.innerText.includes('CMAX') && document.querySelector('#nca-run_state')?.innerText.includes('Calculated'));
    assert.equal(await page.locator('#nca-wide_table').innerText(),before);
    assert.equal(await page.locator('#nca-shared_nca').innerText(),revision);
    await page.evaluate(()=>document.querySelector('#nca-time_unit').selectize.setValue('day'));
    await page.waitForFunction(()=>document.querySelector('#nca-end').value==='0.291667');
    await page.locator('#nca-start').fill('0.03125');
    await page.waitForTimeout(300);
    await page.evaluate(()=>document.querySelector('#nca-time_unit').selectize.setValue('h'));
    await page.waitForFunction(()=>document.querySelector('#nca-start').value==='0.75' && document.querySelector('#nca-end').value==='7');
    await page.locator('#nca-confirm_units').waitFor();
    await page.locator('#nca-start').fill('0');
    await page.evaluate(()=>document.querySelector('#nca-terminal_method').selectize.setValue('auto'));
    await chooseNcaPolicies(page, { dosing: 'single' });
    await page.locator('#nca-dose_history_confirmed').check();
    await page.waitForFunction(()=>!document.querySelector('#nca-run').disabled);
    await page.locator('#nca-run').click();
    await page.waitForFunction(()=>document.body.innerText.includes('Fewer than three positive observed quantifiable concentrations after Tmax'),null,{timeout:30000});
    await page.waitForFunction(()=>document.querySelector('#nca-wide_table')?.innerText.includes('Not calculable: LAMZ'),null,{timeout:30000});
    assert(!((await page.locator('body').innerText()).includes('Untranslated diagnostic; review the Japanese: Tmax後')));
    await page.locator('#nca-advanced-summary').click();
    assert.match(await page.locator('#nca-adpp_draft_exports').innerText(), /ADPP candidate \(CSV\)/);
    await page.evaluate(()=>document.querySelector('#nca-time_unit').selectize.setValue('min'));
    await page.waitForFunction(()=>document.querySelector('#nca-time_unit').value==='min');
    await page.evaluate(()=>document.querySelector('#nca-source').selectize.setValue(window.PK_PREPARED_SOURCE));
    await page.waitForFunction(()=>document.querySelector('#nca-time_unit').value==='h' && document.querySelector('#nca-time_origin').value==='5' && document.querySelector('#nca-start').value==='5',null,{timeout:30000});
    await page.waitForFunction(()=>document.querySelector('#nca-review_blq [data-review-state]')?.dataset.reviewState==='applied');
    await confirmDisplayedChoices(page);
    await page.waitForFunction(()=>!document.querySelector('#nca-run').disabled);
    console.log('PASS: incomplete dose draft keeps session and shared state intact, exact round-trip, validation, recurring predose QC notice and field focus, labels, unchanged results and revision, manual edit, terminal diagnosis, ADPP labels, BLQ-prepared source');
  } finally { if(browser)await browser.close(); if(server.exitCode===null && server.signalCode===null)await new Promise(r=>{server.once('close',r);server.kill('SIGTERM');}); await rm(project,{recursive:true,force:true}); }
}
main().catch(e=>{console.error(e.stack||e);process.exitCode=1;});
