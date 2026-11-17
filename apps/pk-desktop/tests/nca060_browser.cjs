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
    const selectedWarning=page.locator('#nca-selected_warning');
    await selectedWarning.getByText('Warnings for the selected analysis unit').waitFor();
    const style=await summary.locator('summary').evaluate(el=>{
      const count=el.querySelector('.pk-warning-count'), s=getComputedStyle(el), cs=getComputedStyle(count);
      const box=el.getBoundingClientRect(), countBox=count.getBoundingClientRect();
      const rgb=x=>x.match(/[\d.]+/g).slice(0,3).map(Number);
      const lum=x=>rgb(x).map(v=>v/255).map(v=>v<=.04045?v/12.92:Math.pow((v+.055)/1.055,2.4)).reduce((a,v,i)=>a+v*[.2126,.7152,.0722][i],0);
      const overview=el.closest('.pk-warning-overview');
      const groups=[...overview.querySelectorAll('details.pk-warning-group')];
      const overviewBox=overview.getBoundingClientRect();
      const overall=getComputedStyle(overview);
      const selected=getComputedStyle(document.querySelector('#nca-selected_warning .pk-notice'));
      const fore=lum(s.color), back=lum(overall.backgroundColor);
      const selectedBack=lum(selected.backgroundColor), selectedFore=lum(selected.color);
      return {
        size:parseFloat(s.fontSize),weight:parseInt(cs.fontWeight,10),rightGap:box.right-countBox.right,
        contrast:(Math.max(fore,back)+.05)/(Math.min(fore,back)+.05),
        collapsed:groups.every(group=>!group.open),
        topGap:groups[0].querySelector('summary').getBoundingClientRect().top-overviewBox.top,
        bottomGap:overviewBox.bottom-groups.at(-1).querySelector('summary').getBoundingClientRect().bottom,
        overallBorder:parseFloat(overall.borderLeftWidth),selectedBorder:parseFloat(selected.borderLeftWidth),
        overallBackground:overall.backgroundColor,selectedBackground:selected.backgroundColor,
        palerBackground:selectedBack>back,
        selectedContrast:(Math.max(selectedFore,selectedBack)+.05)/(Math.min(selectedFore,selectedBack)+.05)
      };
    });
    assert(style.size>=14 && style.weight>=600 && style.rightGap<25 && style.contrast>=4.5,JSON.stringify(style));
    const englishLabel=await selectedWarning.locator('.pk-selected-warning-item').first().innerText();
    console.log('Warning display:',JSON.stringify({style,englishLabel}));
    assert.deepStrictEqual({
      balancedGaps:style.collapsed && style.topGap>0 && Math.abs(style.topGap-style.bottomGap)<=.5,
      subordinateNotice:style.selectedBorder>0 && style.selectedBorder<style.overallBorder && style.palerBackground && style.selectedContrast>=4.5,
      englishColon:/^LAMZ(?:, [A-Z][A-Z0-9_]*)*:\s+Terminal-phase span/.test(englishLabel)
    },{balancedGaps:true,subordinateNotice:true,englishColon:true});
    await summary.locator('summary').click();
    for(let i=1;i<=6;i++)assert.match(await summary.innerText(),new RegExp(`ID=S0${i}`));
    assert.equal((await summary.innerText()).split('Terminal-phase span is below the specified number of half-lives').length-1,1);
    const before=await page.locator('#nca-shared_nca').innerText();
    assert.match(await page.locator('#nca-selected_warning').innerText(),/ID=S01/);
    assert.match(await page.locator('#nca-selected_warning').innerText(),/Terminal-phase span/);
    const positions=await page.evaluate(()=>Object.fromEntries(Object.entries({profile_selector:'#nca-profile_selector .shiny-input-container',selected_warning:'#nca-selected_warning .pk-notice',plot:'#nca-plot'}).map(([key,selector])=>[key,document.querySelector(selector).getBoundingClientRect().top])));
    assert(positions.profile_selector < positions.selected_warning && positions.selected_warning < positions.plot,JSON.stringify(positions));
    await page.evaluate(()=>document.querySelector('#nca-profile').selectize.setValue('7'));
    await page.waitForFunction(()=>document.querySelector('#nca-selected_warning')?.innerText.includes('ID=S07'));
    const selected=await page.locator('#nca-selected_warning').innerText();
    assert.match(selected,/Fewer than three positive/);
    assert(!selected.includes('Terminal-phase span'));
    assert.equal(await page.locator('#nca-shared_nca').innerText(),before);
    await page.locator('[data-pk-language=ja]').click();
    await page.waitForFunction(()=>document.querySelector('[data-pk-language=ja]').getAttribute('aria-pressed')==='true');
    await selectedWarning.getByText('選択中の解析単位の警告').waitFor();
    assert.match(await selectedWarning.locator('.pk-selected-warning-item').first().innerText(),/^LAMZ(?:・[A-Z][A-Z0-9_]*)*\s*：/);
    await page.locator('[data-pk-language=en]').click();
    await page.waitForFunction(()=>document.querySelector('[data-pk-language=en]').getAttribute('aria-pressed')==='true');
    assert.equal(await page.locator('#nca-shared_nca').innerText(),before);
    console.log('PASS: grouped warning style and compact details, selected profile warnings, revision and language switching');
  } finally { if(browser)await browser.close(); if(server.exitCode===null && server.signalCode===null)await new Promise(r=>{server.once('close',r);server.kill('SIGTERM');}); await rm(project,{recursive:true,force:true}); }
}
main().catch(e=>{console.error(e.stack||e);process.exitCode=1;});
