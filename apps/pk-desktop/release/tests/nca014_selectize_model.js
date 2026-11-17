'use strict';
const assert = require('node:assert/strict');
const fs = require('node:fs');
const expressions = JSON.parse(fs.readFileSync(0, 'utf8'));
const query = eval(`(${expressions.query})`);
const observe = eval(`(${expressions.events})`);
const prepareTab = eval(`(${expressions.prepareTab})`);
const nodes = new Map();
const handlers = new Map();
global.document = {getElementById: id => nodes.get(id)};
global.getComputedStyle = node => node.style;
const jq = {
    on(name, handler) { handlers.set(name, handler); return this; },
    off(name, handler) { assert.equal(handlers.get(name), handler); handlers.delete(name); return this; }
};
global.window = {jQuery: target => { assert.equal(target, document); return jq; }};
const option = (value, visible = true) => ({
    style: {display: 'block', visibility: 'visible'},
    getAttribute: name => name === 'data-value' ? value : null,
    getBoundingClientRect: () => ({width: visible ? 20 : 0, height: 20})
});
function control(value = 'selected') {
    const input = {isConnected: true, disabled: false, id: '',
        style: {display: 'block', visibility: 'visible', opacity: '0', left: '-10000px'},
        getBoundingClientRect: () => ({width: 20, height: 20})};
    const surface = {isConnected: true, click() {
        input.style.opacity = '1'; input.style.left = '0px';
        instance.isInputHidden = false;
    }};
    const dropdown = {
        isConnected: true,
        entries: [option('next-extra'), option('next'), option('next', false)],
        querySelectorAll(selector) { assert.equal(selector, '[data-selectable][data-value]'); return this.entries; }
    };
    const instance = {
        settings: {labelField: 'caption'},
        options: {selected: {caption: 'Selected only'}, next: {caption: 'Identical label'},
                      'next-extra': {caption: 'Identical label'}, table: {caption: 'Saved table'}},
        $control: [surface], $control_input: [input], $dropdown_content: [dropdown],
        isDisabled: false, isLocked: false, isInputHidden: true
    };
    return {
        isConnected: true, disabled: false, value,
        options: [{value, textContent: 'Selected only'}], selectize: instance
    };
}
let select = control();
nodes.set('field', select);
const request = {identifier: 'field', value: 'next'};
assert.equal(select.options.some(entry => entry.value === 'next'), false);
assert.equal(query(request).ready, true);
assert.equal(query(request).label, 'Identical label');
assert.equal(query({...request, operation: 'input'}), select.selectize.$control_input[0]);
assert.equal(query({...request, operation: 'exposedInput'}), null);
const surface = query({...request, operation: 'control'});
assert.equal(surface, select.selectize.$control[0]);
surface.click();
assert.equal(query({...request, operation: 'exposedInput'}), select.selectize.$control_input[0]);
select.selectize.isInputHidden = true;
assert.equal(query({...request, operation: 'exposedInput'}), null);
select.selectize.isInputHidden = false;
surface.isConnected = false;
assert.equal(query(request).ready, false);
surface.isConnected = true;
assert.equal(query({...request, operation: 'option'}), select.selectize.$dropdown_content[0].entries[1]);
assert.deepEqual(query({identifier: 'field', operation: 'labelValues', label: 'Saved table'}), ['table']);
assert.deepEqual(query({identifier: 'field', operation: 'labelValues', label: 'Identical label'}), ['next', 'next-extra']);
assert.deepEqual(query({identifier: 'field', operation: 'labelValues', label: 'Saved'}), []);
assert.equal(query({...request, value: 'unknown'}).reason, 'exact option key missing');
assert.equal(query({...request, value: 'toString'}).ready, false);
select.disabled = true;
assert.equal(query(request).ready, false);
select.disabled = false;
select.selectize.isDisabled = true;
assert.equal(query(request).ready, false);
select.selectize.isDisabled = false;
select.selectize.isLocked = true;
assert.equal(query(request).ready, false);
select.selectize.isLocked = false;
const oldInput = select.selectize.$control_input[0];
select.isConnected = false;
assert.equal(query(request).ready, false);
select = control();
nodes.set('field', select);
assert.notEqual(query({...request, operation: 'input'}), oldInput);
select.selectize.$dropdown_content[0].entries.push(option('next'));
assert.equal(query({...request, operation: 'option'}), null);
select.selectize.$dropdown_content[0].entries = [option('next', false)];
assert.equal(query({...request, operation: 'option'}), null);
const receipt = observe(request);
handlers.get('change')({target: {value: 'next'}});
handlers.get('shiny:inputchanged')({name: 'other', value: 'next'});
handlers.get('shiny:inputchanged')({name: 'field', value: 'next-extra'});
assert.equal(receipt.change, 0);
assert.equal(receipt.shiny, 0);
select.value = 'next';
handlers.get('change')({target: select});
handlers.get('shiny:inputchanged')({name: 'field', value: 'next'});
assert.equal(receipt.change, 1);
assert.equal(receipt.shiny, 1);
receipt.dispose();
assert.equal(handlers.size, 0);
const classes = new Set();
const tab = {
    attributes: {href: '#observed-prepare-panel'},
    getAttribute(name) { return this.attributes[name] ?? null; },
    classList: {contains: name => classes.has('tab-' + name)},
    parentElement: {tagName: 'LI', classList: {contains: name => classes.has('parent-' + name)}}
};
const panel = option('panel');
document.querySelector = selector => {
    assert.equal(selector, '#workbench-mode a[data-value="prepare"]');
    return tab;
};
nodes.set('observed-prepare-panel', panel);
assert.equal(prepareTab(), false);
classes.add('parent-active');
assert.equal(prepareTab(), true);
panel.style.display = 'none';
assert.equal(prepareTab(), false);
panel.style.display = 'block';
classes.clear(); tab.attributes['aria-selected'] = 'true';
assert.equal(prepareTab(), true);
tab.attributes['aria-selected'] = 'false'; classes.add('tab-active');
assert.equal(prepareTab(), true);
tab.attributes['aria-controls'] = 'missing-panel';
assert.equal(prepareTab(), false);
console.log('Selectize and tab query models passed. This is not a browser execution.');
