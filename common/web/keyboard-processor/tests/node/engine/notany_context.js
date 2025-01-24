import { assert } from 'chai';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { KeyboardInterface, MinimalKeymanGlobal, Mock } from '@keymanapp/keyboard-processor';
import { NodeKeyboardLoader } from '@keymanapp/keyboard-processor/node-keyboard-loader';
import { NodeProctor, RecordedKeystrokeSequence } from '@keymanapp/recorder-core';
import { extendString } from '@keymanapp/web-utils';

extendString();  // Ensure KMW's string-extension functionality is available.

// Initialize supplementary plane string extensions
String.kmwEnableSupplementaryPlane(false);

const device = {
  formFactor: 'desktop',
  OS: 'windows',
  browser: 'native'
}

/** @type {KeyboardInterface} */
let keyboardWithHarness;

function runEngineRuleSet(ruleSet) {
  for(let ruleDef of ruleSet) {
    keyboardWithHarness.resetContextCache();
    // Prepare the context!
    const ruleSeq = new RecordedKeystrokeSequence(ruleDef);
    const proctor = new NodeProctor(keyboardWithHarness, device, assert.equal);
    const target = new Mock();
    ruleSeq.test(proctor, target);
  }
}

/**
 * Wrapper to simplify running tests -- supports either virtual key codes as string (e.g. 'A' is VK_A)
 * or an array of integers. Does not currently support modifiers (not needed here).
 * @param {String|Array} input   Virtual key codes of each character (as string or array)
 * @param {String}       output  Expected output
 */

function runStringRuleSet(input, output) {
  const rule = {
    "inputs":
      typeof input == 'string'
      ? input.split("").map(ch =>
        { return { "type": "key", "keyCode": ch.charCodeAt(0), "states": 10752, "modifiers": 0, "modifierChanged": false, "isVirtualKey": true } })
      : input.map(ch =>
        { return { "type": "key", "keyCode": ch, "states": 10752, "modifiers": 0, "modifierChanged": false, "isVirtualKey": true } }),
    "output": output
  };
  return runEngineRuleSet([rule]);
}

// -----------

describe('Engine - notany() and context()', function() {
  before(async function() {
    let keyboardLoader = new NodeKeyboardLoader(new KeyboardInterface({}, MinimalKeymanGlobal));
    const keyboard = await keyboardLoader.loadKeyboardFromPath(require.resolve('@keymanapp/common-test-resources/keyboards/test_917.js'));
    keyboardWithHarness = keyboardLoader.harness;
    keyboardWithHarness.activeKeyboard = keyboard;
  });

  /*
  *  Isolated rule tests for validity of `context()` and `notany()` statements.
  */

  describe('First set -- notany() in first context position', function() {
    it("baseline: rule with nomatch is not executed", function() {
      runStringRuleSet('ABC', 'abc');
    });

    it("rule with nomatch is executed with a plain char matched", function() {
      runStringRuleSet('XBC', 'xBC');
    });

    it("rule with nomatch is executed with a deadkey matched", function() {
      runStringRuleSet('DBC1', 'pass!');
    });
  });

  describe('Second set -- notany() in second context position', function() {
    it("rule with nomatch is executed with a plain char matched", function() {
      runStringRuleSet('AXCD1', 'axXX1');
    });

    it("rule with nomatch is executed with a deadkey matched", function() {
      runStringRuleSet('ADCD1', 'pass!');
    });
  });

  describe('Third set -- notany() in third context position with initial if() testing AdjustIndex', function() {
    it("rule with nomatch is executed with a plain char matched", function() {
      runStringRuleSet('FGHI1', 'fgXX1');
    });

    it("rule with nomatch is executed with a deadkey matched", function() {
      runStringRuleSet('FDHI1', 'pass!');
    });
  });

  // This is actually a separate issue that was corrected in the same PR
  // so we placed the test here for conciseness.

  it('verify notany comparison flip correction from #3817', function() {
    runStringRuleSet('YZ', 'pass!');
  });
});