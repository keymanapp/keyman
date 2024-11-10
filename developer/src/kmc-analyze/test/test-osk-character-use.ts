/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import * as fs from 'fs';
import { assert } from 'chai';
import 'mocha';
import { makePathToFixture } from './helpers/index.js';
import { AnalyzeOskCharacterUse } from '../src/osk-character-use/index.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

const KHMER_ANGKOR_TOUCH_LAYOUT = makePathToFixture('khmer', "khmer_angkor.keyman-touch-layout");
const KHMER_ANGKOR_KVKS = makePathToFixture('khmer', "khmer_angkor.kvks");
const KBDKHMR_JSON = makePathToFixture('khmer', "KbdKhmr.json");

const SIL_KHMER_TOUCH_LAYOUT = makePathToFixture('khmer', "sil_khmer.keyman-touch-layout");
const SIL_KHMER_KVKS = makePathToFixture('khmer', "sil_khmer.kvks");
const KBDKHMR_WITH_SIL_KHMER_JSON = makePathToFixture('khmer', "KbdKhmr-with-sil_khmer.json");

describe('AnalyzeOskCharacterUse', function () {
  const callbacks = new TestCompilerCallbacks();

  this.beforeEach(function() {
    callbacks.clear();
  });

  this.afterEach(function() {
    if(this.currentTest?.isFailed()) {
      callbacks.printMessages();
    }
  });

  it('generates a mapping file from 2 input files', async function() {
    // functional test
    const a = new AnalyzeOskCharacterUse(callbacks, {
      includeCounts: false,
      stripDottedCircle: true,
    });

    assert.isTrue(await a.analyze(KHMER_ANGKOR_TOUCH_LAYOUT));
    assert.isTrue(await a.analyze(KHMER_ANGKOR_KVKS));

    const expected = JSON.parse(fs.readFileSync(KBDKHMR_JSON, 'utf-8'));
    const actual = JSON.parse(a.getStrings(".json").join(''));
    assert.deepEqual(actual, expected);
  });

  it('extends an existing mapping file with 2 more input files', async function() {
    // functional test
    const a = new AnalyzeOskCharacterUse(callbacks, {
      includeCounts: false,
      stripDottedCircle: true,
      mergeMapFile: KBDKHMR_JSON,
    });

    assert.isTrue(await a.analyze(SIL_KHMER_TOUCH_LAYOUT));
    assert.isTrue(await a.analyze(SIL_KHMER_KVKS));

    const expected = JSON.parse(fs.readFileSync(KBDKHMR_WITH_SIL_KHMER_JSON, 'utf-8'));
    const actual = JSON.parse(a.getStrings(".json").join(''));
    assert.deepEqual(actual, expected);
  });

  it('loads an existing mapping file round trip', async function() {
    // functional test
    const a = new AnalyzeOskCharacterUse(callbacks, {
      includeCounts: false,
      stripDottedCircle: true,
      mergeMapFile: KBDKHMR_JSON,
    });

    const expected = JSON.parse(fs.readFileSync(KBDKHMR_JSON, 'utf-8'));
    const actual = JSON.parse(a.getStrings(".json").join(''));
    assert.deepEqual(actual, expected);
  });

  it('has no effect if the mapping has not changed', async function() {
    // functional test
    const a = new AnalyzeOskCharacterUse(callbacks, {
      includeCounts: false,
      stripDottedCircle: true,
      mergeMapFile: KBDKHMR_JSON,
    });

    assert.isTrue(await a.analyze(KHMER_ANGKOR_TOUCH_LAYOUT));
    assert.isTrue(await a.analyze(KHMER_ANGKOR_KVKS));

    const expected = JSON.parse(fs.readFileSync(KBDKHMR_JSON, 'utf-8'));
    const actual = JSON.parse(a.getStrings(".json").join(''));
    assert.deepEqual(actual, expected);
  });

  it('has no effect if the mapping has not changed, regardless of processing order', async function() {
    // functional test
    const a = new AnalyzeOskCharacterUse(callbacks, {
      includeCounts: false,
      stripDottedCircle: true,
      mergeMapFile: KBDKHMR_JSON,
    });

    assert.isTrue(await a.analyze(KHMER_ANGKOR_KVKS));
    assert.isTrue(await a.analyze(KHMER_ANGKOR_TOUCH_LAYOUT));

    const expected = JSON.parse(fs.readFileSync(KBDKHMR_JSON, 'utf-8'));
    const actual = JSON.parse(a.getStrings(".json").join(''));
    assert.deepEqual(actual, expected);
  });

});
