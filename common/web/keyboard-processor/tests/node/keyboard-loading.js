import { assert } from 'chai';
import fs from 'fs';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { KeyboardHarness, KeyboardInterface, MinimalKeymanGlobal, Mock } from '@keymanapp/keyboard-processor';
import { NodeKeyboardLoader } from '@keymanapp/keyboard-processor/node-keyboard-loader';

describe('Headless keyboard loading', function() {
  const laoPath = require.resolve('@keymanapp/common-test-resources/keyboards/lao_2008_basic.js');
  // Common test suite setup.

  let device = {
    formFactor: 'desktop',
    OS: 'windows',
    browser: 'native'
  }

  describe('Minimal harness loading', () => {
    it('successfully loads', async function() {
      // -- START: Standard Recorder-based unit test loading boilerplate --
      let harness = new KeyboardHarness({}, MinimalKeymanGlobal);
      let keyboardLoader = new NodeKeyboardLoader(harness);
      let keyboard = await keyboardLoader.loadKeyboardFromPath(laoPath);
      // --  END:  Standard Recorder-based unit test loading boilerplate --

      // This part provides assurance that the keyboard properly loaded.
      assert.equal(keyboard.id, "Keyboard_lao_2008_basic");
    });

    it('cannot evaluate rules', async function() {
      // -- START: Standard Recorder-based unit test loading boilerplate --
      let harness = new KeyboardHarness({}, MinimalKeymanGlobal);
      let keyboardLoader = new NodeKeyboardLoader(harness);
      let keyboard = await keyboardLoader.loadKeyboardFromPath(laoPath);
      // --  END:  Standard Recorder-based unit test loading boilerplate --

      // Runs a blank KeyEvent through the keyboard's rule processing...
      // but via separate harness configured with a different captured global.
      let ruleHarness = new KeyboardInterface({}, MinimalKeymanGlobal);
      ruleHarness.activeKeyboard = keyboard;
      try {
        ruleHarness.processKeystroke(new Mock(), keyboard.constructNullKeyEvent(device));
        assert.fail();
      } catch (err) {
        // Drives home an important detail: the 'global' object is effectively
        // closure-captured.  (Similar constraints may occur when experimenting with
        // 'sandboxed' keyboard loading in the DOM!)
        assert.equal(err.message, 'k.KKM is not a function');
      }
    });
  });

  describe('Full harness loading', () => {
    it('successfully loads', async function() {
      // -- START: Standard Recorder-based unit test loading boilerplate --
      let harness = new KeyboardInterface({}, MinimalKeymanGlobal);
      let keyboardLoader = new NodeKeyboardLoader(harness);
      let keyboard = await keyboardLoader.loadKeyboardFromPath(laoPath);
      // --  END:  Standard Recorder-based unit test loading boilerplate --

      // This part provides assurance that the keyboard properly loaded.
      assert.equal(keyboard.id, "Keyboard_lao_2008_basic");
    });

    it('can evaluate rules', async function() {
      // -- START: Standard Recorder-based unit test loading boilerplate --
      let harness = new KeyboardInterface({}, MinimalKeymanGlobal);
      let keyboardLoader = new NodeKeyboardLoader(harness);
      let keyboard = await keyboardLoader.loadKeyboardFromPath(laoPath);
      // --  END:  Standard Recorder-based unit test loading boilerplate --

      // Runs a blank KeyEvent through the keyboard's rule processing.
      harness.processKeystroke(new Mock(), keyboard.constructNullKeyEvent(device));
    });
  })
});