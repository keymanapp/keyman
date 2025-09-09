import { env } from 'node:process';
import fs from 'node:fs';
import { createRequire } from 'node:module';

import { assert } from 'chai';
import * as sinon from 'sinon';

import { LexicalModelTypes } from '@keymanapp/common-types';
import { KeyboardTest, RecordedPhysicalKeystroke, RecordedSequenceTestSet } from '@keymanapp/recorder-core';
import { Worker } from '@keymanapp/lexical-model-layer/node';
import { DeviceSpec, KMWString } from '@keymanapp/web-utils';

import { InputProcessor } from 'keyman/engine/main';
import { KeyboardInterface, Mock } from 'keyman/engine/js-processor';
import { KeyEvent, KeyEventSpec, MinimalKeymanGlobal } from 'keyman/engine/keyboard';
import { NodeKeyboardLoader } from 'keyman/engine/keyboard/node-keyboard-loader';
import { PredictionContext } from 'keyman/engine/interfaces';

import Context = LexicalModelTypes.Context;
import Suggestion = LexicalModelTypes.Suggestion;

const require = createRequire(import.meta.url);
const KEYMAN_ROOT = env.KEYMAN_ROOT;

const dummyModel = (suggestionSets: Suggestion[][]) => ({
  id: 'dummy',
  languages: ['en'],
  code: `
LMLayerWorker.loadModel(new models.DummyModel({
  futureSuggestions: ${JSON.stringify(suggestionSets, null, 2)},
}));
`
});

declare global {
  var keyman: typeof MinimalKeymanGlobal;
  var KeymanWeb: any;
}

// Required initialization setup.
//
// So that keyboard-based checks against the global `keyman` succeed.
// 10.0+ dependent keyboards, like khmer_angkor, will otherwise fail to load.
global['keyman'] = MinimalKeymanGlobal;

let device: DeviceSpec = {
  formFactor: DeviceSpec.FormFactor.Phone,
  OS: DeviceSpec.OperatingSystem.iOS,
  browser: DeviceSpec.Browser.Safari,
  touchable: true
};

// Initialize supplementary plane string extensions
KMWString.enableSupplementaryPlane(false);

// Test the KeyboardProcessor interface.
describe('InputProcessor', function() {
  describe('[[constructor]]', function () {
    it('should initialize without errors', function () {
      let core = new InputProcessor(device, null);
      assert.isNotNull(core);
    });

    it('has expected default values after initialization', function () {
      let core;
      try {
        // Can construct without the second parameter; if so, the final assertion - .mayPredict
        // will be invalidated.  (No worker, no ability to predict.)
        // @ts-ignore
        core = new InputProcessor(device, Worker);

        assert.isOk(core.keyboardProcessor);
        assert.isDefined(core.keyboardProcessor.contextDevice);
        assert.isOk(core.languageProcessor);
        assert.isOk(core.keyboardInterface);
        assert.isUndefined(core.activeKeyboard); // No keyboard should be loaded yet.
        assert.isUndefined(core.activeModel);    // Same for the model.

        // These checks are lifted from the keyboard init checks found in
        // web/src/test/auto/headless/engine/js-processor/basic-init.js.
        assert.equal('us', core.keyboardProcessor.baseLayout, 'KeyboardProcessor has unexpected base layout')
        assert.isNotNull(global.KeymanWeb, 'KeymanWeb global was not automatically installed');
        assert.equal('default', core.keyboardProcessor.layerId, 'Default layer is not set to "default"');
        assert.isUndefined(core.keyboardProcessor.activeKeyboard, 'Initialized with already-active keyboard');

        // Lifted from languageProcessor.js - the core should not be changing these with its init.
        assert.isUndefined(core.languageProcessor.activeModel);
        assert.isFalse(core.languageProcessor.isActive);
        assert.isTrue(core.languageProcessor.mayPredict);
        assert.isTrue(core.languageProcessor.canEnable);
      } finally {
        core?.languageProcessor?.shutdown();
      }
    });
  });

  describe('efficiency tests', function() {
    let keyboardWithHarness: KeyboardInterface;

    let mainWebScriptURL = require.resolve('@keymanapp/lm-worker/worker-main.wrapped.js');

    // Easy peasy long context:  use the unminified main script for the predictive-text worker!
    let coreSourceCode = fs.readFileSync(mainWebScriptURL, 'utf-8');

    // At the time this test block was written...  810485 chars.
    // Let's force it to the same order of magnitude, even if the codebase grows.
    if(coreSourceCode.length > 1000000) {
      coreSourceCode = coreSourceCode.substring(0, 1000000);
    }

    this.beforeAll(async function() {
      // Load the keyboard.
      let keyboardLoader = new NodeKeyboardLoader(new KeyboardInterface({}, MinimalKeymanGlobal));
      const keyboard = await keyboardLoader.loadKeyboardFromPath(require.resolve('@keymanapp/common-test-resources/keyboards/test_chirality.js'));
      keyboardWithHarness = keyboardLoader.harness as KeyboardInterface;
      keyboardWithHarness.activeKeyboard = keyboard;
    });

    describe('without fat-fingering', function() {
      it('with minimal context (no fat-fingers)', function() {
        this.timeout(32); // ms
        let core = new InputProcessor(device, null);
        let context = new Mock("", 0);

        core.keyboardProcessor.keyboardInterface = keyboardWithHarness;
        let keyboard = keyboardWithHarness.activeKeyboard;
        let layout = keyboard.layout(DeviceSpec.FormFactor.Phone);
        let key = layout.getLayer('default').getKey('K_A');
        let event = keyboard.constructKeyEvent(key, device, core.keyboardProcessor.stateKeys);

        let behavior = core.processKeyEvent(event, context, null);
        assert.isNotNull(behavior);
      });

      it('with extremely long context (' + KMWString.length(coreSourceCode) + ' chars, no fat-fingers)', function() {
        // Assumes no SMP chars in the source, which is fine.
        let context = new Mock(coreSourceCode, KMWString.length(coreSourceCode));

        this.timeout(500);                // 500 ms, excluding text import.
                                          // These often run on VMs, so we'll be a bit generous.

        let core = new InputProcessor(device, null);  // I mean, it IS long context, and time
                                          // thresholding is disabled within Node.

        core.keyboardProcessor.keyboardInterface = keyboardWithHarness;
        let keyboard = keyboardWithHarness.activeKeyboard;
        let layout = keyboard.layout(DeviceSpec.FormFactor.Phone);
        let key = layout.getLayer('default').getKey('K_A');
        let event = keyboard.constructKeyEvent(key, device, core.keyboardProcessor.stateKeys);

        let behavior = core.processKeyEvent(event, context, null);
        assert.isNotNull(behavior);
      });
    });

    describe('with fat-fingering', function() {
      it('with minimal context (with fat-fingers)', function() {
        this.timeout(32); // ms
        let core = new InputProcessor(device, null);
        let context = new Mock("", 0);

        core.keyboardProcessor.keyboardInterface = keyboardWithHarness;
        let keyboard = keyboardWithHarness.activeKeyboard;
        let layout = keyboard.layout(DeviceSpec.FormFactor.Phone);
        let key = layout.getLayer('default').getKey('K_A');
        let event = keyboard.constructKeyEvent(key, device, core.keyboardProcessor.stateKeys);

        let behavior = core.processKeyEvent(event, context, null);
        assert.isNotNull(behavior);
      });

      it('with extremely long context (' + KMWString.length(coreSourceCode) + ' chars, with fat-fingers)', function() {
        // Assumes no SMP chars in the source, which is fine.
        let context = new Mock(coreSourceCode, KMWString.length(coreSourceCode));

        this.timeout(500);                // 500 ms, excluding text import.
                                          // These often run on VMs, so we'll be a bit generous.
                                          //
                                          // Keep at the same 'order of magnitude' as the
                                          // 'without fat-fingers' test.

        let core = new InputProcessor(device, null);  // It IS long context, and time
                                          // thresholding is disabled within Node.

        core.keyboardProcessor.keyboardInterface = keyboardWithHarness as KeyboardInterface;
        let keyboard = keyboardWithHarness.activeKeyboard;
        let layout = keyboard.layout(DeviceSpec.FormFactor.Phone);
        let key = layout.getLayer('default').getKey('K_A');
        let event = keyboard.constructKeyEvent(key, device, core.keyboardProcessor.stateKeys);

        let behavior = core.processKeyEvent(event, context, null);
        assert.isNotNull(behavior);
      });
    });
  });

  describe('Deadkeys bug #8568 - backspace should not reset all deadkeys', function () {
    let keyboardWithHarness: KeyboardInterface;
    let testJSONtext = fs.readFileSync(require.resolve('@keymanapp/common-test-resources/json/engine_tests/8568_deadkeys.json'));
    // For convenience we define the key sequence in a test file although we don't use the
    // rest of the recorder stuff since it uses only KeyboardProcessor, not InputProcessor.
    let testDefinitions = new KeyboardTest(JSON.parse(testJSONtext.toString()));

    before(async function () {
      // Load the keyboard.
      let keyboardLoader = new NodeKeyboardLoader(new KeyboardInterface({}, MinimalKeymanGlobal));
      const keyboard = await keyboardLoader.loadKeyboardFromPath(require.resolve('@keymanapp/common-test-resources/keyboards/test_8568_deadkeys.js'));
      keyboardWithHarness = keyboardLoader.harness as KeyboardInterface;
      keyboardWithHarness.activeKeyboard = keyboard;

      // This part provides extra assurance that the keyboard properly loaded.
      assert.equal(keyboard.id, "Keyboard_test_8568_deadkeys");
    });

    const testsToRun: RecordedSequenceTestSet = testDefinitions.inputTestSets[0] as RecordedSequenceTestSet;
    for (let testSet of testsToRun.testSet) {
      it(testSet.msg ?? 'test', function() {
        this.timeout(32); // ms
        let core = new InputProcessor(device, null);
        let context = new Mock("", 0);

        core.keyboardProcessor.keyboardInterface = keyboardWithHarness;
        let keyboard = keyboardWithHarness.activeKeyboard;

        for (let key of testSet.inputs) {
          const keystroke = key as RecordedPhysicalKeystroke;
          let keySpec: KeyEventSpec = {
            Lcode: keystroke.keyCode,
            Lmodifiers: keystroke.modifiers,
            LmodifierChange: keystroke.modifierChanged,
            vkCode: keystroke.vkCode,
            Lstates: keystroke.states,
            kName: '',
            device: device,
            isSynthetic: false,
            LisVirtualKey: keyboard.definesPositionalOrMnemonic // Only false for 1.0 keyboards.,
          };

          let behavior = core.processKeyEvent(new KeyEvent(keySpec), context, null);
          assert.isNotNull(behavior);
        }
        assert.equal(context.getText(), testSet.output);
      });
    }
  });

  describe('Predictive-text integration tests', () => {
    let keyboardWithHarness: KeyboardInterface;
    const device: DeviceSpec = {
      formFactor: DeviceSpec.FormFactor.Phone,
      OS: DeviceSpec.OperatingSystem.iOS,
      browser: DeviceSpec.Browser.Safari,
      touchable: true
    };
    const baseStates = {
      K_CAPS: false,
      K_NUMLOCK: false,
      K_SCROLL: false
    };
    const simpleTestingDummyModel = dummyModel([
      // empty:  should not show any suggestions on first load + context-set.
      [],
      [
        {
          transform: { insert: 'testing', deleteLeft: 4 },
          appendedTransform: { insert: ' ', deleteLeft: 0 },
          transformId: 0, // will be overwritten by the DummyModel to match the transition ID.
          id: 1,
          displayAs: 'testing'
        }
      ]
    ]);
    const simpleTrieModel = {
      id: 'simple-trie',
      languages: ['en'],
      path: `${KEYMAN_ROOT}/common/test/resources/models/simple-trie.js`
    };

    before(async () => {
      // Load the keyboard.
      let keyboardLoader = new NodeKeyboardLoader(new KeyboardInterface({}, MinimalKeymanGlobal));
      const keyboard = await keyboardLoader.loadKeyboardFromPath(require.resolve('@keymanapp/common-test-resources/keyboards/test_simple_deadkeys.js'));
      keyboardWithHarness = keyboardLoader.harness as KeyboardInterface;
      keyboardWithHarness.activeKeyboard = keyboard;

      // This part provides extra assurance that the keyboard properly loaded.
      assert.equal(keyboard.id, "Keyboard_test_simple_deadkeys");
    });

    it('replaces appended whitespace when a manually-applied suggestion is followed by a K_SPACE (dummy models)', async () => {
      // @ts-ignore
      const core = new InputProcessor(device, Worker);
      const langProcessor = core.languageProcessor;

      try {
        await langProcessor.loadModel(simpleTestingDummyModel);
        const predContext = new PredictionContext(core.languageProcessor, () => 'default');
        core.keyboardProcessor.keyboardInterface = keyboardWithHarness;
        const keyboard = keyboardWithHarness.activeKeyboard;

        const getEventFor = (keyId: string) => {
          const key = keyboard.layout(DeviceSpec.FormFactor.Phone).getLayer('default').getKey(keyId);
          return keyboard.constructKeyEvent(
            key, device, baseStates
          );
        }

        const contexts: Context[] = [
          { left: 'this context is for test', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for testi', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for testing', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for testing ', startOfBuffer: true, endOfBuffer: true },
        ];

        const mock = new Mock(contexts[0].left);
        predContext.setCurrentTarget(mock);

        const ruleBehavior = core.processKeyEvent(new KeyEvent(getEventFor('K_I')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[1].left);
        assert.isOk(ruleBehavior);
        const testiPredictions = await ruleBehavior.predictionPromise;
        const pred_testing = testiPredictions.find((s) => s.displayAs == 'testing')
        assert.isOk(pred_testing);

        const applySpy = sinon.spy(langProcessor, 'applySuggestion');
        predContext.accept(pred_testing);
        assert.equal(mock.getTextBeforeCaret(), contexts[3].left);
        // Intercept the return value that holds a reference to the reversion promise.
        await applySpy.firstCall.returnValue.reversion;

        // The reversion should be available on the prediction context and should have
        // an appended transform component.
        assert.isOk(predContext.immediateReversion, "no reversion was returned by the model")
        assert.isOk(
          predContext.immediateReversion?.appendedTransform,
          "the reversion returned by the model did not include an appended transform"
        );

        const behavior2 = core.processKeyEvent(new KeyEvent(getEventFor('K_SPACE')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[3].left); // no new whitespace!
        assert.isOk(behavior2);
        // Is after the suggestion still, but with the appended space removed!
        assert.equal(behavior2.transcription.preInput.getTextBeforeCaret(), contexts[2].left);
      } finally {
        langProcessor.shutdown();
      }
    });

    it('replaces appended whitespace when a manually-applied suggestion is followed by a K_SPACE (trie models)', async () => {
      // @ts-ignore
      const core = new InputProcessor(device, Worker);
      const langProcessor = core.languageProcessor;

      try {
        await langProcessor.loadModel(simpleTrieModel);
        const predContext = new PredictionContext(core.languageProcessor, () => 'default');
        core.keyboardProcessor.keyboardInterface = keyboardWithHarness;
        const keyboard = keyboardWithHarness.activeKeyboard;

        const getEventFor = (keyId: string) => {
          const key = keyboard.layout(DeviceSpec.FormFactor.Phone).getLayer('default').getKey(keyId);
          return keyboard.constructKeyEvent(
            key, device, baseStates
          );
        }

        const contexts: Context[] = [
          { left: 'this context is for tec', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for tech', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical ', startOfBuffer: true, endOfBuffer: true },
        ];

        const mock = new Mock(contexts[0].left);
        predContext.setCurrentTarget(mock);

        const ruleBehavior = core.processKeyEvent(new KeyEvent(getEventFor('K_H')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[1].left);
        assert.isOk(ruleBehavior);
        const techPredictions = await ruleBehavior.predictionPromise;
        const pred_testing = techPredictions.find((s) => s.displayAs == 'technical')
        assert.isOk(pred_testing);

        const applySpy = sinon.spy(langProcessor, 'applySuggestion');
        predContext.accept(pred_testing);
        assert.equal(mock.getTextBeforeCaret(), contexts[3].left);
        // Intercept the return value that holds a reference to the reversion promise.
        await applySpy.firstCall.returnValue.reversion;
        applySpy.restore();

        // The reversion should be available on the prediction context and should have
        // an appended transform component.
        assert.isOk(predContext.immediateReversion, "no reversion was returned by the model")
        assert.isOk(
          predContext.immediateReversion?.appendedTransform,
          "the reversion returned by the model did not include an appended transform"
        );

        const behavior2 = core.processKeyEvent(new KeyEvent(getEventFor('K_SPACE')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[3].left); // no new whitespace!
        assert.isOk(behavior2);
        // Is after the suggestion still, but with the appended space removed!
        assert.equal(behavior2.transcription.preInput.getTextBeforeCaret(), contexts[2].left);
      } finally {
        langProcessor.shutdown();
      }
    });

    it('auto-applies a suggestion properly when available and triggered appropriately', async () => {
      // @ts-ignore
      const core = new InputProcessor(device, Worker);
      const langProcessor = core.languageProcessor;

      try {
        await langProcessor.loadModel(simpleTestingDummyModel);
        const predContext = new PredictionContext(core.languageProcessor, () => 'default');
        core.keyboardProcessor.keyboardInterface = keyboardWithHarness;
        const keyboard = keyboardWithHarness.activeKeyboard;

        const getEventFor = (keyId: string) => {
          const key = keyboard.layout(DeviceSpec.FormFactor.Phone).getLayer('default').getKey(keyId);
          return keyboard.constructKeyEvent(
            key, device, baseStates
          );
        }

        const contexts: Context[] = [
          { left: 'this context is for test', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for testi', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for testing', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for testing.', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for testing..', startOfBuffer: true, endOfBuffer: true }
        ];

        const mock = new Mock(contexts[0].left);
        predContext.setCurrentTarget(mock);

        const ruleBehavior = core.processKeyEvent(new KeyEvent(getEventFor('K_I')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[1].left);
        assert.isOk(ruleBehavior);
        const testiPredictions = await ruleBehavior.predictionPromise;
        const pred_testing = testiPredictions.find((s) => s.displayAs == 'testing')
        assert.isOk(pred_testing);
        assert.isTrue(pred_testing.autoAccept);

        // Note:  `.`, `,`, and ` ` are among the default set of supported marks.
        const behavior2 = core.processKeyEvent(new KeyEvent(getEventFor('K_PERIOD')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[3].left);
        assert.isOk(behavior2);
        // Is after the suggestion still, but with the appended space removed!
        assert.equal(behavior2.transcription.preInput.getTextBeforeCaret(), contexts[2].left);

        await behavior2.predictionPromise;

        // Verify that the second period is emitted properly.
        core.processKeyEvent(new KeyEvent(getEventFor('K_PERIOD')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[4].left);
      } finally {
        langProcessor.shutdown();
      }
    });

    it('displays a reversion after manually applying a suggestion and immediately backspacing', async () => {
      // @ts-ignore
      const core = new InputProcessor(device, Worker);
      const langProcessor = core.languageProcessor;

      try {
        // This feature only activates with 14.0+ models.
        await langProcessor.loadModel(simpleTrieModel);
        const predContext = new PredictionContext(core.languageProcessor, () => 'default');
        core.keyboardProcessor.keyboardInterface = keyboardWithHarness;
        const keyboard = keyboardWithHarness.activeKeyboard;

        const getEventFor = (keyId: string) => {
          const key = keyboard.layout(DeviceSpec.FormFactor.Phone).getLayer('default').getKey(keyId);
          return keyboard.constructKeyEvent(
            key, device, baseStates
          );
        }

        const contexts: Context[] = [
          { left: 'this context is for tec', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for tech', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical ', startOfBuffer: true, endOfBuffer: true }
        ];

        const mock = new Mock(contexts[0].left);
        predContext.setCurrentTarget(mock);

        const ruleBehavior = core.processKeyEvent(new KeyEvent(getEventFor('K_H')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[1].left);
        assert.isOk(ruleBehavior);
        const techPredictions = await ruleBehavior.predictionPromise;
        const pred_technical = techPredictions.find((s) => s.displayAs == 'technical')
        assert.isOk(pred_technical);
        assert.isTrue(pred_technical.autoAccept);

        const applySpy = sinon.spy(langProcessor, 'applySuggestion');
        predContext.accept(pred_technical);
        assert.equal(mock.getTextBeforeCaret(), contexts[3].left);
        // Intercept the return value that holds a reference to the reversion promise.
        await applySpy.firstCall.returnValue.reversion;
        applySpy.restore();

        // Note:  `.`, `,`, and ` ` are among the default set of supported marks.
        assert.equal(mock.getTextBeforeCaret(), contexts[3].left);
        // Is after the suggestion still, but with the appended space removed!

        // Verify that the second period is emitted properly.
        const behavior3 = core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[2].left);
        const finalSuggestions = await behavior3.predictionPromise;

        assert.isOk(finalSuggestions.find(s => s.tag == 'revert'));
      } finally {
        langProcessor.shutdown();
      }
    });

    it('displays a reversion after returning to the whitespace after a manually-applied suggestion', async () => {
      // @ts-ignore
      const core = new InputProcessor(device, Worker);
      const langProcessor = core.languageProcessor;

      try {
        // This feature only activates with 14.0+ models.
        await langProcessor.loadModel(simpleTrieModel);
        const predContext = new PredictionContext(core.languageProcessor, () => 'default');
        core.keyboardProcessor.keyboardInterface = keyboardWithHarness;
        const keyboard = keyboardWithHarness.activeKeyboard;

        const getEventFor = (keyId: string) => {
          const key = keyboard.layout(DeviceSpec.FormFactor.Phone).getLayer('default').getKey(keyId);
          return keyboard.constructKeyEvent(
            key, device, baseStates
          );
        }

        const contexts: Context[] = [
          { left: 'this context is for tec', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for tech', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical ', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical det', startOfBuffer: true, endOfBuffer: true }
        ];

        const mock = new Mock(contexts[0].left);
        predContext.setCurrentTarget(mock);

        const ruleBehavior = core.processKeyEvent(new KeyEvent(getEventFor('K_H')), mock, predContext);
        const techPredictions = await ruleBehavior.predictionPromise;
        const pred_technical = techPredictions.find((s) => s.displayAs == 'technical')
        const applySpy = sinon.spy(langProcessor, 'applySuggestion');
        predContext.accept(pred_technical);

        // Intercept the return value that holds a reference to the reversion promise.
        await applySpy.firstCall.returnValue.reversion;
        applySpy.restore();

        // Note:  `.`, `,`, and ` ` are among the default set of supported marks.
        assert.equal(mock.getTextBeforeCaret(), contexts[3].left);
        // Is after the suggestion still, but with the appended space removed!

        // Verify that the second period is emitted properly.
        core.processKeyEvent(new KeyEvent(getEventFor('K_D')), mock, predContext);
        core.processKeyEvent(new KeyEvent(getEventFor('K_E')), mock, predContext);
        core.processKeyEvent(new KeyEvent(getEventFor('K_T')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[4].left);

        core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        const behavior3 = core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[3].left);

        const finalSuggestions = await behavior3.predictionPromise;

        assert.isOk(finalSuggestions.find(s => s.tag == 'revert'));
      } finally {
        langProcessor.shutdown();
      }
    });

    it("displays a reversion after returning to the end of a manually-applied suggestion's body", async () => {
      // @ts-ignore
      const core = new InputProcessor(device, Worker);
      const langProcessor = core.languageProcessor;

      try {
        // This feature only activates with 14.0+ models.
        await langProcessor.loadModel(simpleTrieModel);
        const predContext = new PredictionContext(core.languageProcessor, () => 'default');
        core.keyboardProcessor.keyboardInterface = keyboardWithHarness;
        const keyboard = keyboardWithHarness.activeKeyboard;

        const getEventFor = (keyId: string) => {
          const key = keyboard.layout(DeviceSpec.FormFactor.Phone).getLayer('default').getKey(keyId);
          return keyboard.constructKeyEvent(
            key, device, baseStates
          );
        }

        const contexts: Context[] = [
          { left: 'this context is for tec', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for tech', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical ', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical det', startOfBuffer: true, endOfBuffer: true }
        ];

        const mock = new Mock(contexts[0].left);
        predContext.setCurrentTarget(mock);

        const ruleBehavior = core.processKeyEvent(new KeyEvent(getEventFor('K_H')), mock, predContext);
        const techPredictions = await ruleBehavior.predictionPromise;
        const pred_technical = techPredictions.find((s) => s.displayAs == 'technical')
        const applySpy = sinon.spy(langProcessor, 'applySuggestion');
        predContext.accept(pred_technical);

        // Intercept the return value that holds a reference to the reversion promise.
        await applySpy.firstCall.returnValue.reversion;
        applySpy.restore();

        // Note:  `.`, `,`, and ` ` are among the default set of supported marks.
        assert.equal(mock.getTextBeforeCaret(), contexts[3].left);
        // Is after the suggestion still, but with the appended space removed!

        // Verify that the second period is emitted properly.
        core.processKeyEvent(new KeyEvent(getEventFor('K_D')), mock, predContext);
        core.processKeyEvent(new KeyEvent(getEventFor('K_E')), mock, predContext);
        core.processKeyEvent(new KeyEvent(getEventFor('K_T')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[4].left);

        core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        const behavior3 = core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[2].left);

        const finalSuggestions = await behavior3.predictionPromise;

        assert.isOk(finalSuggestions.find(s => s.tag == 'revert'));
      } finally {
        langProcessor.shutdown();
      }
    });

    it("does not display a reversion after backspacing part of an applied suggestion", async () => {
      // @ts-ignore
      const core = new InputProcessor(device, Worker);
      const langProcessor = core.languageProcessor;

      try {
        // This feature only activates with 14.0+ models.
        await langProcessor.loadModel(simpleTrieModel);
        const predContext = new PredictionContext(core.languageProcessor, () => 'default');
        core.keyboardProcessor.keyboardInterface = keyboardWithHarness;
        const keyboard = keyboardWithHarness.activeKeyboard;

        const getEventFor = (keyId: string) => {
          const key = keyboard.layout(DeviceSpec.FormFactor.Phone).getLayer('default').getKey(keyId);
          return keyboard.constructKeyEvent(
            key, device, baseStates
          );
        }

        const contexts: Context[] = [
          { left: 'this context is for tec', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for tech', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical ', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical det', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technica', startOfBuffer: true, endOfBuffer: true }
        ];

        const mock = new Mock(contexts[0].left);
        predContext.setCurrentTarget(mock);

        const ruleBehavior = core.processKeyEvent(new KeyEvent(getEventFor('K_H')), mock, predContext);
        const techPredictions = await ruleBehavior.predictionPromise;
        const pred_technical = techPredictions.find((s) => s.displayAs == 'technical')
        const applySpy = sinon.spy(langProcessor, 'applySuggestion');
        predContext.accept(pred_technical);

        // Intercept the return value that holds a reference to the reversion promise.
        await applySpy.firstCall.returnValue.reversion;
        applySpy.restore();

        // Note:  `.`, `,`, and ` ` are among the default set of supported marks.
        assert.equal(mock.getTextBeforeCaret(), contexts[3].left);
        // Is after the suggestion still, but with the appended space removed!

        // Verify that the second period is emitted properly.
        core.processKeyEvent(new KeyEvent(getEventFor('K_D')), mock, predContext);
        core.processKeyEvent(new KeyEvent(getEventFor('K_E')), mock, predContext);
        core.processKeyEvent(new KeyEvent(getEventFor('K_T')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[4].left);

        core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        const behavior3 = core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[5].left);

        const finalSuggestions = await behavior3.predictionPromise;

        assert.isNotOk(finalSuggestions.find(s => s.tag == 'revert'));
      } finally {
        langProcessor.shutdown();
      }
    });

    it('displays a reversion after auto-applying a suggestion and immediately backspacing', async () => {
      // @ts-ignore
      const core = new InputProcessor(device, Worker);
      const langProcessor = core.languageProcessor;

      try {
        // This feature only activates with 14.0+ models.
        await langProcessor.loadModel(simpleTrieModel);
        const predContext = new PredictionContext(core.languageProcessor, () => 'default');
        core.keyboardProcessor.keyboardInterface = keyboardWithHarness;
        const keyboard = keyboardWithHarness.activeKeyboard;

        const getEventFor = (keyId: string) => {
          const key = keyboard.layout(DeviceSpec.FormFactor.Phone).getLayer('default').getKey(keyId);
          return keyboard.constructKeyEvent(
            key, device, baseStates
          );
        }

        const contexts: Context[] = [
          { left: 'this context is for tech', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for techn', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical.', startOfBuffer: true, endOfBuffer: true }
        ];

        const mock = new Mock(contexts[0].left);
        predContext.setCurrentTarget(mock);

        const ruleBehavior = core.processKeyEvent(new KeyEvent(getEventFor('K_N')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[1].left);
        assert.isOk(ruleBehavior);
        const techPredictions = await ruleBehavior.predictionPromise;
        const pred_technical = techPredictions.find((s) => s.displayAs == 'technical')
        assert.isOk(pred_technical);
        assert.isTrue(pred_technical.autoAccept);

        // Note:  `.`, `,`, and ` ` are among the default set of supported marks.
        const behavior2 = core.processKeyEvent(new KeyEvent(getEventFor('K_PERIOD')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[3].left);
        assert.isOk(behavior2);
        // Is after the suggestion still, but with the appended space removed!
        assert.equal(behavior2.transcription.preInput.getTextBeforeCaret(), contexts[2].left);

        await behavior2.predictionPromise;

        // Verify that the second period is emitted properly.
        const behavior3 = core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[2].left);
        const finalSuggestions = await behavior3.predictionPromise;

        assert.isOk(finalSuggestions.find(s => s.tag == 'revert'));
      } finally {
        langProcessor.shutdown();
      }
    });

    it('displays a reversion after returning to the whitespace after a auto-applied suggestion', async () => {
      // @ts-ignore
      const core = new InputProcessor(device, Worker);
      const langProcessor = core.languageProcessor;

      try {
        // This feature only activates with 14.0+ models.
        await langProcessor.loadModel(simpleTrieModel);
        const predContext = new PredictionContext(core.languageProcessor, () => 'default');
        core.keyboardProcessor.keyboardInterface = keyboardWithHarness;
        const keyboard = keyboardWithHarness.activeKeyboard;

        const getEventFor = (keyId: string) => {
          const key = keyboard.layout(DeviceSpec.FormFactor.Phone).getLayer('default').getKey(keyId);
          return keyboard.constructKeyEvent(
            key, device, baseStates
          );
        }

        const contexts: Context[] = [
          { left: 'this context is for tech', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for techn', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical.', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical... ', startOfBuffer: true, endOfBuffer: true }
        ];

        const mock = new Mock(contexts[0].left);
        predContext.setCurrentTarget(mock);

        const ruleBehavior = core.processKeyEvent(new KeyEvent(getEventFor('K_N')), mock, predContext);
        await ruleBehavior.predictionPromise;

        // Note:  `.`, `,`, and ` ` are among the default set of supported marks.
        const behavior2 = core.processKeyEvent(new KeyEvent(getEventFor('K_PERIOD')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[3].left);

        await behavior2.predictionPromise;

        core.processKeyEvent(new KeyEvent(getEventFor('K_PERIOD')), mock, predContext);
        core.processKeyEvent(new KeyEvent(getEventFor('K_PERIOD')), mock, predContext);
        core.processKeyEvent(new KeyEvent(getEventFor('K_SPACE')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[4].left);

        core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        const behavior3 = core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[3].left);
        const finalSuggestions = await behavior3.predictionPromise;

        assert.isOk(finalSuggestions.find(s => s.tag == 'revert'));
      } finally {
        langProcessor.shutdown();
      }
    });

    it("displays a reversion after returning to the end of an auto-applied suggestion's body", async () => {
      // @ts-ignore
      const core = new InputProcessor(device, Worker);
      const langProcessor = core.languageProcessor;

      try {
        // This feature only activates with 14.0+ models.
        await langProcessor.loadModel(simpleTrieModel);
        const predContext = new PredictionContext(core.languageProcessor, () => 'default');
        core.keyboardProcessor.keyboardInterface = keyboardWithHarness;
        const keyboard = keyboardWithHarness.activeKeyboard;

        const getEventFor = (keyId: string) => {
          const key = keyboard.layout(DeviceSpec.FormFactor.Phone).getLayer('default').getKey(keyId);
          return keyboard.constructKeyEvent(
            key, device, baseStates
          );
        }

        const contexts: Context[] = [
          { left: 'this context is for tech', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for techn', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical.', startOfBuffer: true, endOfBuffer: true },
          { left: 'this context is for technical... ', startOfBuffer: true, endOfBuffer: true }
        ];

        const mock = new Mock(contexts[0].left);
        predContext.setCurrentTarget(mock);

        const ruleBehavior = core.processKeyEvent(new KeyEvent(getEventFor('K_N')), mock, predContext);
        await ruleBehavior.predictionPromise;

        // Note:  `.`, `,`, and ` ` are among the default set of supported marks.
        const behavior2 = core.processKeyEvent(new KeyEvent(getEventFor('K_PERIOD')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[3].left);

        await behavior2.predictionPromise;

        core.processKeyEvent(new KeyEvent(getEventFor('K_PERIOD')), mock, predContext);
        core.processKeyEvent(new KeyEvent(getEventFor('K_PERIOD')), mock, predContext);
        core.processKeyEvent(new KeyEvent(getEventFor('K_SPACE')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[4].left);

        core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        const behavior3 = core.processKeyEvent(new KeyEvent(getEventFor('K_BKSP')), mock, predContext);
        assert.equal(mock.getTextBeforeCaret(), contexts[2].left);
        const finalSuggestions = await behavior3.predictionPromise;

        assert.isOk(finalSuggestions.find(s => s.tag == 'revert'));
      } finally {
        langProcessor.shutdown();
      }
    });
  });
});
