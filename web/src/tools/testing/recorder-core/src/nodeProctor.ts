import { Proctor, AssertCallback } from "./proctor.js";
import {
  KeyboardTest,
  TestSet,
  TestSequence,
  RecordedKeystrokeSequence,
  RecordedPhysicalKeystroke,
  RecordedSyntheticKeystroke
} from "./index.js";

import { DefaultOutputRules, KeyEvent, KeyEventSpec, KeyboardHarness, SyntheticTextStore, TextStore } from "keyman/engine/keyboard";
import { DeviceSpec } from "keyman/common/web-utils";
import { JSKeyboardInterface, JSKeyboardProcessor } from 'keyman/engine/js-processor';

export class NodeProctor extends Proctor {
  private keyboardWithHarness: KeyboardHarness;
  public __debug = false;

  constructor(kbdHarness: KeyboardHarness, device: DeviceSpec, assert: AssertCallback) {
    super(device, assert);

    this.keyboardWithHarness = kbdHarness;
  }

  async beforeAll() {
    //
  }

  before() {
    //
  }

  compatibleWithSuite(testSuite: KeyboardTest): boolean {
    // Original-version tests did not supply core-compatible KeyEvent data.
    return !testSuite.specVersion.equals(KeyboardTest.FALLBACK_VERSION);
  }

  get debugMode(): boolean {
    return this.__debug;
  }

  set debugMode(value: boolean) {
    this.__debug = value;
  }

  matchesTestSet(testSet: TestSet<any>) {
    // KeyboardProcessor is abstract enough to run tests aimed at any platform.
    return true;
  }

  async simulateSequence(sequence: TestSequence<any>, textStore?: TextStore): Promise<string> {
    // Start with an empty TextStore and a fresh KeyboardProcessor.
    if(!textStore) {
      textStore = new SyntheticTextStore();
    }

    // Establish a fresh processor, setting its keyboard appropriately for the test.
    const processor = new JSKeyboardProcessor(this.device, {
      baseLayout: 'us',
      keyboardInterface: this.keyboardWithHarness as JSKeyboardInterface,
      defaultOutputRules: new DefaultOutputRules()
    });
    const keyboard = processor.activeKeyboard;

    if(sequence instanceof RecordedKeystrokeSequence) {
      for(const keystroke of sequence.inputs) {
        let keyEvent: KeyEventSpec;
        if(keystroke instanceof RecordedPhysicalKeystroke) {
          // Use the keystroke's stored data to reconstruct the KeyEvent.
          keyEvent = {
            Lcode: keystroke.keyCode,
            Lmodifiers: keystroke.modifiers,
            LmodifierChange: keystroke.modifierChanged,
            vkCode: keystroke.vkCode,
            Lstates: keystroke.states,
            kName: '',
            device: this.device,
            isSynthetic: false,
            LisVirtualKey: keyboard.definesPositionalOrMnemonic // Only false for 1.0 keyboards.
          }
        } else if(keystroke instanceof RecordedSyntheticKeystroke) {
          const key = keyboard.layout(this.device.formFactor).getLayer(keystroke.layer).getKey(keystroke.keyName);
          keyEvent = keyboard.constructKeyEvent(key, this.device, processor.stateKeys);
        }

        // Fill in the final details of the KeyEvent...
        keyEvent.device = this.device;

        // And now, execute the keystroke!
        // We don't care too much about particularities of per-keystroke behavior yet.
        // ... we _could_ if we wanted to, though.  The framework is mostly in place;
        // it's a matter of actually adding the feature.
        const ruleBehavior = processor.processKeystroke(new KeyEvent(keyEvent), textStore);

        if (this.debugMode) {
          console.log("Processing %d:", keyEvent.Lcode);
          console.log("textStore=%s", JSON.stringify(textStore, null, '  '));
          console.log("ruleBehavior=%s", JSON.stringify(ruleBehavior, null, '  '));
        }
      }
    } else {
      throw new Error("NodeProctor only supports RecordedKeystrokeSequences for testing at present.");
    }
    return textStore.getText();
  }
}
