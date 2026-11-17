'use strict';
const assert = require('assert');
const fs = require('fs');
const vm = require('vm');
const source = fs.readFileSync('apps/pk-desktop/R/i18n.R', 'utf8');
const prefix = "shiny::tags$script(shiny::HTML('";
const start = source.indexOf(prefix);
assert(start >= 0);
const end = source.indexOf("\n')))\n}", start);
assert(end > start);
const script = source.slice(start + prefix.length, end);
const legacy = fs.readFileSync('apps/pk-desktop/R/workbench_styles.R', 'utf8');
assert(!legacy.includes('PK_ADPP_UPLOAD_I18N_BEGIN'), 'Competing observer must remain removed');
let clickHandler, onMutation, observerCount = 0;
const bar = {nodeType:1, textContent:'Upload complete', matches:s => s.includes('progress-bar'), querySelectorAll:() => []};
function button(language) {
  return {language, attributes:{}, getAttribute(k) {return k === 'data-pk-language' ? this.language : this.attributes[k];},
    setAttribute(k,v) {this.attributes[k]=v;}, closest() {return this;}};
}
const ja=button('ja'), en=button('en'), preview={hidden:true};
const doc={
  readyState:'complete', documentElement:{lang:'ja'},
  getElementById() {return null;},
  addEventListener(event, cb) {if (event === 'click') clickHandler=cb;},
  querySelectorAll(selector) {
    if (selector === '.shiny-file-input-progress .progress-bar') return [bar];
    if (selector === '[data-pk-language]') return [ja,en];
    if (selector === '.pk-language-preview') return [preview];
    return [];
  }
};
function MutationObserver(cb) {observerCount++; onMutation=cb; this.observe=function() {};}
vm.runInNewContext(script, {document:doc, window:{}, MutationObserver});
assert.strictEqual(bar.textContent, 'アップロード完了');
clickHandler({target:en});
assert.strictEqual(bar.textContent, 'Upload complete');
assert.strictEqual(doc.documentElement.lang, 'ja');
assert.strictEqual(preview.hidden, false);
for (let i=0; i<4; i++) onMutation([{addedNodes:[{nodeType:3,parentElement:bar}]}]);
assert.strictEqual(bar.textContent, 'Upload complete');
assert.strictEqual(observerCount, 1);
clickHandler({target:ja});
assert.strictEqual(bar.textContent, 'アップロード完了');
bar.textContent='Upload failed';
onMutation([{addedNodes:[{nodeType:3,parentElement:bar}]}]);
assert.strictEqual(bar.textContent, 'Upload failed');
console.log('Language-aware upload completion without competing observer: PASS');
