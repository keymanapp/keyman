import { assert } from 'chai';
import fs from 'fs';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { InputProcessor } from 'keyman/engine/main';
import { KeyboardInterface, MinimalKeymanGlobal, Mock } from 'keyman/engine/keyboard';
import { NodeKeyboardLoader } from 'keyman/engine/keyboard/node-keyboard-loader';
import { KeyboardTest } from '@keymanapp/recorder-core';

import { Worker } from '@keymanapp/lexical-model-layer/node';
import * as utils from '@keymanapp/web-utils';

// Required initialization setup.
global.keyman = {}; // So that keyboard-based checks against the global `keyman` succeed.
                    // 10.0+ dependent keyboards, like khmer_angkor, will otherwise fail to load.

let device = {
  formFactor: 'phone',
  OS: 'ios',
  browser: 'safari'
};

// Initialize supplementary plane string extensions
String.kmwEnableSupplementaryPlane(false);

// Test the KeyboardProcessor interface.
describe('InputProcessor', function() {
  describe('[[constructor]]', function () {
    it('should initialize without errors', function () {
      let core = new InputProcessor(device);
      assert.isNotNull(core);
    });

    it('has expected default values after initialization', function () {
      // Can construct without the second parameter; if so, the final assertion - .mayPredict
      // will be invalidated.  (No worker, no ability to predict.)
      let worker = Worker.constructInstance();

      try {
        let core = new InputProcessor(device, worker);

        assert.isOk(core.keyboardProcessor);
        assert.isDefined(core.keyboardProcessor.contextDevice);
        assert.isOk(core.languageProcessor);
        assert.isOk(core.keyboardInterface);
        assert.isUndefined(core.activeKeyboard); // No keyboard should be loaded yet.
        assert.isUndefined(core.activeModel);    // Same for the model.

        // These checks are lifted from the keyboard init checks found in
        // common/web/keyboard/tests/cases/basic-init.js.
        assert.equal('us', core.keyboardProcessor.baseLayout, 'KeyboardProcessor has unexpected base layout')
        assert.isNotNull(global.KeymanWeb, 'KeymanWeb global was not automatically installed');
        assert.equal('default', core.keyboardProcessor.layerId, 'Default layer is not set to "default"');
        assert.isUndefined(core.keyboardProcessor.activeKeyboard, 'Initialized with already-active keyboard');

        // Lifted from languageProcessor.js - the core should not be changing these with its init.
        assert.isUndefined(core.languageProcessor.activeModel);
        assert.isFalse(core.languageProcessor.isActive);
        assert.isTrue(core.languageProcessor.mayPredict);
      } finally {
        worker.terminate();
      }
    });
  });

  describe('efficiency tests', function() {
    let testDistribution = [];
    let keyboardWithHarness;

    // Easy peasy long context:  use the input processor's full source!
    let coreSourceCode = fs.readFileSync('build/lib/index.mjs', 'utf-8');

    // At the time this test block was written...  810485 chars.
    // Let's force it to the same order of magnitude, even if the codebase grows.
    if(coreSourceCode.length > 1000000) {
      coreSourceCode = coreSourceCode.substring(0, 1000000);
    }

    this.beforeAll(async function() {
      testDistribution = [];

      for(let c = 'A'.charCodeAt(0); c <= 'Z'.charCodeAt(0); c++) {
        let char = String.fromCharCode(c);

        testDistribution.push({
          keyId: "K_" + char,
          p: 1 / 26
        });
      }

      // Load the keyboard.
      let keyboardLoader = new NodeKeyboardLoader(new KeyboardInterface({}, MinimalKeymanGlobal));
      const keyboard = await keyboardLoader.loadKeyboardFromPath(require.resolve('@keymanapp/common-test-resources/keyboards/test_chirality.js'));
      keyboardWithHarness = keyboardLoader.harness;
      keyboardWithHarness.activeKeyboard = keyboard;
    });

    describe('without fat-fingering', function() {
      it('with minimal context (no fat-fingers)', function() {
        this.timeout(32); // ms
        let core = new InputProcessor(device);
        let context = new Mock("", 0);

        core.keyboardProcessor.keyboardInterface = keyboardWithHarness;
        let keyboard = keyboardWithHarness.activeKeyboard;
        let layout = keyboard.layout(utils.DeviceSpec.FormFactor.Phone);
        let key = layout.getLayer('default').getKey('K_A');
        let event = keyboard.constructKeyEvent(key, device, core.keyboardProcessor.stateKeys);

        let behavior = core.processKeyEvent(event, context);
        assert.isNotNull(behavior);
      });

      it('with extremely long context (' + coreSourceCode._kmwLength() + ' chars, no fat-fingers)', function() {
        // Assumes no SMP chars in the source, which is fine.
        let context = new Mock(coreSourceCode, coreSourceCode._kmwLength());

        this.timeout(500);                // 500 ms, excluding text import.
                                          // These often run on VMs, so we'll be a bit generous.

        let core = new InputProcessor(device);  // I mean, it IS long context, and time
                                          // thresholding is disabled within Node.

        core.keyboardProcessor.keyboardInterface = keyboardWithHarness;
        let keyboard = keyboardWithHarness.activeKeyboard;
        let layout = keyboard.layout(utils.DeviceSpec.FormFactor.Phone);
        let key = layout.getLayer('default').getKey('K_A');
        let event = keyboard.constructKeyEvent(key, device, core.keyboardProcessor.stateKeys);

        let behavior = core.processKeyEvent(event, context);
        assert.isNotNull(behavior);
      });
    });

    describe('with fat-fingering', function() {
      it('with minimal context (with fat-fingers)', function() {
        this.timeout(32); // ms
        let core = new InputProcessor(device);
        let context = new Mock("", 0);

        core.keyboardProcessor.keyboardInterface = keyboardWithHarness;
        let keyboard = keyboardWithHarness.activeKeyboard;
        let layout = keyboard.layout(utils.DeviceSpec.FormFactor.Phone);
        let key = layout.getLayer('default').getKey('K_A');
        key.keyDistribution = testDistribution;
        let event = keyboard.constructKeyEvent(key, device, core.keyboardProcessor.stateKeys);

        let behavior = core.processKeyEvent(event, context);
        assert.isNotNull(behavior);
      });

      it('with extremely long context (' + coreSourceCode._kmwLength() + ' chars, with fat-fingers)', function() {
        // Assumes no SMP chars in the source, which is fine.
        let context = new Mock(coreSourceCode, coreSourceCode._kmwLength());

        this.timeout(500);                // 500 ms, excluding text import.
                                          // These often run on VMs, so we'll be a bit generous.
                                          //
                                          // Keep at the same 'order of magnitude' as the
                                          // 'without fat-fingers' test.

        let core = new InputProcessor(device);  // It IS long context, and time
                                          // thresholding is disabled within Node.

        core.keyboardProcessor.keyboardInterface = keyboardWithHarness;
        let keyboard = keyboardWithHarness.activeKeyboard;
        let layout = keyboard.layout(utils.DeviceSpec.FormFactor.Phone);
        let key = layout.getLayer('default').getKey('K_A');
        key.keyDistribution = testDistribution;
        let event = keyboard.constructKeyEvent(key, device, core.keyboardProcessor.stateKeys);

        let behavior = core.processKeyEvent(event, context);
        assert.isNotNull(behavior);
      });
    });
  });

  describe('Deadkeys bug #8568 - backspace should not reset all deadkeys', function () {
    let keyboardWithHarness;
    let testJSONtext = fs.readFileSync(require.resolve('@keymanapp/common-test-resources/json/engine_tests/8568_deadkeys.json'));
    // For convenience we define the key sequence in a test file although we don't use the
    // rest of the recorder stuff since it uses only KeyboardProcessor, not InputProcessor.
    let testDefinitions = new KeyboardTest(JSON.parse(testJSONtext));

    before(async function () {
      // Load the keyboard.
      let keyboardLoader = new NodeKeyboardLoader(new KeyboardInterface({}, MinimalKeymanGlobal));
      const keyboard = await keyboardLoader.loadKeyboardFromPath(require.resolve('@keymanapp/common-test-resources/keyboards/test_8568_deadkeys.js'));
      keyboardWithHarness = keyboardLoader.harness;
      keyboardWithHarness.activeKeyboard = keyboard;

      // This part provides extra assurance that the keyboard properly loaded.
      assert.equal(keyboard.id, "Keyboard_test_8568_deadkeys");
    });

    for (let testSet of testDefinitions.inputTestSets[0]['testSet']) {
      it(testSet.msg ?? 'test', function() {
        this.timeout(32); // ms
        let core = new InputProcessor(device);
        let context = new Mock("", 0);

        core.keyboardProcessor.keyboardInterface = keyboardWithHarness;
        let keyboard = keyboardWithHarness.activeKeyboard;

        for (let keystroke of testSet.inputs) {
          let keyEvent = {
            Lcode: keystroke.keyCode,
            Lmodifiers: keystroke.modifiers,
            LmodifierChange: keystroke.modifierChanged,
            vkCode: keystroke.vkCode,
            Lstates: keystroke.states,
            kName: '',
            device: device,
            isSynthetic: false,
            LisVirtualKey: keyboard.definesPositionalOrMnemonic // Only false for 1.0 keyboards.
          };

          let behavior = core.processKeyEvent(keyEvent, context);
          assert.isNotNull(behavior);
        }
        assert.equal(context.getText(), testSet.output);
      });
    }
  });
});
