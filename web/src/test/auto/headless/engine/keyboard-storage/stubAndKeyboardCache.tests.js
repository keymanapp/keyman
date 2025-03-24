import { assert } from 'chai';
import sinon from 'sinon';
import fs from 'fs';

import { KeyboardStub, StubAndKeyboardCache } from 'keyman/engine/keyboard-storage';

import { NodeKeyboardLoader } from 'keyman/engine/keyboard/node-keyboard-loader';
import { KeyboardHarness, MinimalKeymanGlobal } from 'keyman/engine/keyboard';

import path from 'path';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

describe('StubAndKeyboardCache', function () {
  let commonResourcesPackage = '@keymanapp/common-test-resources';
  let commonStubsSubpath = 'json/keyboards';
  let rootCommonStubPath = `${commonResourcesPackage}/${commonStubsSubpath}`;

  const resolvedResourcePackageAnchor = require.resolve(`${commonResourcesPackage}/index.mjs`);
  const resolvedResourcePackage = path.dirname(resolvedResourcePackageAnchor);

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

  it('can resolve original order of added stubs', () => {
    const cache = new StubAndKeyboardCache();

    assert.isNotOk(cache.defaultStub);

    // Could convert to run on all stubs, but... this should be fine as-is.
    const khmer_angkor_stub = new KeyboardStub(JSON.parse(fs.readFileSync(require.resolve(`${rootCommonStubPath}/khmer_angkor.json`))));
    cache.addStub(khmer_angkor_stub);

    const galaxie_stub = new KeyboardStub(JSON.parse(fs.readFileSync(require.resolve(`${rootCommonStubPath}/galaxie_hebrew_positional.json`))));
    cache.addStub(galaxie_stub);

    const lao_2008_basic_stub = new KeyboardStub(JSON.parse(fs.readFileSync(require.resolve(`${rootCommonStubPath}/lao_2008_basic.json`))));
    cache.addStub(lao_2008_basic_stub);

    assert.strictEqual(cache.defaultStub, khmer_angkor_stub);

    cache.forgetKeyboard(khmer_angkor_stub.id);

    assert.strictEqual(cache.defaultStub, galaxie_stub);
  });

  it('loads & caches `Keyboard`s, `Keyboard` promise pending resolution', async () => {
    let keyboardLoader = new NodeKeyboardLoader(new KeyboardHarness({}, MinimalKeymanGlobal));
    const cache = new StubAndKeyboardCache(keyboardLoader);

    const lao_2008_basic_stub = new KeyboardStub(JSON.parse(fs.readFileSync(require.resolve(`${rootCommonStubPath}/lao_2008_basic.json`))), resolvedResourcePackage + '/../');
    cache.addStub(lao_2008_basic_stub);
    const lao_2008_basic_kbd_promise = cache.fetchKeyboard('lao_2008_basic');

    assert.isNotOk(cache.getKeyboard('lao_2008_basic')); // The keyboard isn't loaded, so we can't retrieve it yet.
    const then = sinon.fake();
    const thenPromise = lao_2008_basic_kbd_promise.then(then);

    // A second 'fetch' for a keyboard with a pending Promise should reuse that Promise.
    assert.strictEqual(cache.fetchKeyboard('lao_2008_basic'), lao_2008_basic_kbd_promise);

    const lao_2008_basic_kbd = await lao_2008_basic_kbd_promise;

    // Fulfillment of the Promise should replace the Promise with the keyboard itself.
    assert.strictEqual(cache.getKeyboard('lao_2008_basic'), lao_2008_basic_kbd);

    assert.equal(lao_2008_basic_kbd.id,   'Keyboard_lao_2008_basic');
    assert.equal(lao_2008_basic_kbd.name, 'Lao 2008 Basic');

    // Ensure that any 'then' placed on that deferment Promise gets properly fulfilled.
    await thenPromise;
    assert.isTrue(then.called);

    // At this point, the keyboard should be fully loaded and now available from the cache.
    assert.strictEqual(cache.getKeyboardForStub(lao_2008_basic_stub), lao_2008_basic_kbd);
  });

  it('returns an available stub for a keyboard without specified language code', () => {
    const cache = new StubAndKeyboardCache();

    const lao_2008_basic_stub = new KeyboardStub(JSON.parse(fs.readFileSync(require.resolve(`${rootCommonStubPath}/lao_2008_basic.json`))), resolvedResourcePackage + '/../');
    cache.addStub(lao_2008_basic_stub);

    const codelessStub = cache.getStub('lao_2008_basic');
    assert.strictEqual(codelessStub, lao_2008_basic_stub);

    const badCodeStub = cache.getStub('lao_2008_basic', 'km'); // km = Khmer.
    assert.isNotOk(badCodeStub);
  });

  it('properly handles failed keyboard load attempts', async () => {
    let keyboardLoader = new NodeKeyboardLoader(new KeyboardHarness({}, MinimalKeymanGlobal));
    const cache = new StubAndKeyboardCache(keyboardLoader);

    const bad_stub = new KeyboardStub({
      KI: "bad_stub",
      KLC: "und",
      // This is a actual valid file... just... not a keyboard file.
      // (It's an empty module, and thus the keyboard load will fail.)
      KF: resolvedResourcePackageAnchor
    });

    cache.addStub(bad_stub);
    // Should not throw an error here; it occurs asynchronously during the load attempt.
    const firstFetch = cache.fetchKeyboard('bad_stub');

    assert.isNotOk(cache.getKeyboard('bad_stub')); // The keyboard isn't loaded, so we can't retrieve it yet.
    const catchHandler = sinon.fake();
    const catchPromise = firstFetch.catch(catchHandler);

    // A second 'fetch' for a keyboard with a pending Promise should reuse that Promise.
    assert.strictEqual(cache.fetchKeyboard('bad_stub'), firstFetch);

    try {
      await firstFetch; // return type irrelevant - we expect an error to be thrown.
      assert.fail();
    } catch (err) { }

    // Ensure that any 'catch' placed on that deferment Promise gets properly fulfilled.
    await catchPromise;
    assert.isTrue(catchHandler.called);
    assert.isTrue(catchHandler.firstCall.args[0] instanceof Error);

    // At this point, there should be no keyboard - it did fail to load, after all.
    assert.isNotOk(cache.getKeyboard('bad_stub'));

    const secondFetch = cache.fetchKeyboard('bad_stub');
    assert.notStrictEqual(secondFetch, firstFetch);
  });

  it('fails a keyboard load attempt synchronously if keyboard file or id is unspecified', () => {
    let keyboardLoader = new NodeKeyboardLoader(new KeyboardHarness({}, MinimalKeymanGlobal));
    const cache = new StubAndKeyboardCache(keyboardLoader);

    try {
      cache.fetchKeyboard('');
      assert.fail();
    } catch(err) {
      assert.isTrue(err instanceof Error);
    };

    const bad_stub = new KeyboardStub({
      KI: "bad_stub",
      KLC: "und"
    });

    cache.addStub(bad_stub);

    try {
      cache.fetchKeyboardForStub(bad_stub);
      assert.fail();
    } catch(err) {
      assert.isTrue(err instanceof Error);
      assert.isTrue(err.message.includes('bad_stub'));
    };
  });
});