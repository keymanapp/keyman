import { assert } from 'chai';
import fs from 'fs';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { MinimalKeymanGlobal } from 'keyman/engine/keyboard';
import { KeyboardInterface } from 'keyman/engine/js-processor';
import { NodeKeyboardLoader } from 'keyman/engine/keyboard/node-keyboard-loader';
import { KeyboardTest, NodeProctor } from '@keymanapp/recorder-core';

import { getKeymanRoot } from '../../../getKeymanRoot.mjs';
const KEYMAN_ROOT = getKeymanRoot();

describe('Engine - Unmatched Final Groups', function() {
  let testJSONtext = fs.readFileSync(require.resolve('@keymanapp/common-test-resources/json/engine_tests/ghp_enter.json'));
  // Common test suite setup.
  let testSuite = new KeyboardTest(JSON.parse(testJSONtext));

  var keyboardWithHarness;
  let device = {
    formFactor: 'desktop',
    OS: 'windows',
    browser: 'native'
  }

  before(async function() {
    // -- START: Standard Recorder-based unit test loading boilerplate --
    let keyboardLoader = new NodeKeyboardLoader(new KeyboardInterface({}, MinimalKeymanGlobal));
    const keyboard = await keyboardLoader.loadKeyboardFromPath(KEYMAN_ROOT + '/common/test/' + testSuite.keyboard.filename);
    keyboardWithHarness = keyboardLoader.harness;
    keyboardWithHarness.activeKeyboard = keyboard;

    assert.equal(keyboard.id, "Keyboard_" + testSuite.keyboard.id);
    // --  END:  Standard Recorder-based unit test loading boilerplate --

    // This part provides extra assurance that the keyboard properly loaded.
    assert.equal(keyboard.id, "Keyboard_galaxie_hebrew_positional");
  });

  it('Emits default enter AND matches rule from early group', function() {
    let proctor = new NodeProctor(keyboardWithHarness, device, assert.equal);
    testSuite.test(proctor);
  });
});