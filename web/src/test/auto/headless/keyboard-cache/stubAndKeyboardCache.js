import { assert } from 'chai';
import sinon from 'sinon';
import fs from 'fs';

import { KeyboardStub, StubAndKeyboardCache } from '../../../../../build/engine/keyboard-cache/obj/index.js';

import { NodeKeyboardLoader } from '@keymanapp/keyboard-processor/nodeKeyboardLoader';
import { KeyboardHarness, MinimalKeymanGlobal } from '@keymanapp/keyboard-processor';

// import path from 'path';
// import { fileURLToPath } from 'url';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

// const __filename = fileURLToPath(import.meta.url);
// const __dirname = path.dirname(__filename);

describe('StubAndKeyboardCache', function () {
  let commonResourcesPackage = '@keymanapp/common-test-resources';
  let commonStubsSubpath = 'json/keyboards';
  let rootCommonStubPath = `${commonResourcesPackage}/${commonStubsSubpath}`;

  it('caches stubs', () => {
    const cache = new StubAndKeyboardCache();

    // Could convert to run on all stubs, but... this should be fine as-is.
    const khmer_angkor_stub = new KeyboardStub(JSON.parse(fs.readFileSync(require.resolve(`${rootCommonStubPath}/khmer_angkor.json`))));
    cache.addStub(khmer_angkor_stub);

    const galaxie_stub = new KeyboardStub(JSON.parse(fs.readFileSync(require.resolve(`${rootCommonStubPath}/galaxie_hebrew_positional.json`))));
    cache.addStub(galaxie_stub);

    const lao_2008_basic_stub = new KeyboardStub(JSON.parse(fs.readFileSync(require.resolve(`${rootCommonStubPath}/lao_2008_basic.json`))));
    cache.addStub(lao_2008_basic_stub);

    assert.strictEqual(cache.getStub('khmer_angkor', 'km'), khmer_angkor_stub);
    assert.strictEqual(cache.getStub('lao_2008_basic', 'lo'), lao_2008_basic_stub);

    assert.strictEqual(cache.findMatchingStub(new KeyboardStub('khmer_angkor', 'km')), khmer_angkor_stub);
    assert.strictEqual(cache.findMatchingStub(new KeyboardStub('galaxie_hebrew_positional', 'he')), galaxie_stub);
  });

  it('caches `Keyboard`s, `Keyboard` promise pending resolution', async () => {
    let keyboardLoader = new NodeKeyboardLoader(new KeyboardHarness({}, MinimalKeymanGlobal));

    const lao_2008_basic_kbd_path = require.resolve(`${commonResourcesPackage}/keyboards/lao_2008_basic.js`);
    let lao_2008_basic_kbd_promise = keyboardLoader.loadKeyboardFromPath(lao_2008_basic_kbd_path);

    const cache = new StubAndKeyboardCache();
    cache.expectKeyboard(lao_2008_basic_kbd_promise, 'lao_2008_basic');
    assert.isTrue(cache.getKeyboard('lao_2008_basic') instanceof Promise);

    const then = sinon.fake();
    const thenPromise = cache.getKeyboard('lao_2008_basic').then(then);

    const lao_2008_basic_kbd = await lao_2008_basic_kbd_promise;

    // Fulfillment of the Promise should replace the Promise with the keyboard itself.
    assert.strictEqual(cache.getKeyboard('lao_2008_basic'), lao_2008_basic_kbd);

    assert.equal(lao_2008_basic_kbd.id,   'Keyboard_lao_2008_basic');
    assert.equal(lao_2008_basic_kbd.name, 'Lao 2008 Basic');

    // Ensure that any 'then' placed on that deferment Promise gets properly fulfilled.
    await thenPromise;
    assert.isTrue(then.called);
  });
});