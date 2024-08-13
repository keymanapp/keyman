import Proctor, { AssertCallback } from "./proctor.js";
import {
  KeyboardTest,
  TestSet,
  TestSequence,
  RecordedKeystrokeSequence,
  RecordedPhysicalKeystroke,
  RecordedSyntheticKeystroke
} from "./index.js";

import { KeyboardInterface, KeyEvent, KeyEventSpec, KeyboardProcessor, Mock, type OutputTarget, KeyboardHarness } from "@keymanapp/keyboard-processor";
import { DeviceSpec } from "@keymanapp/web-utils";

export default class NodeProctor extends Proctor {
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

  async simulateSequence(sequence: TestSequence<any>, target?: OutputTarget): Promise<string> {
    // Start with an empty OutputTarget and a fresh KeyboardProcessor.
    if(!target) {
      target = new Mock();
    }

    // Establish a fresh processor, setting its keyboard appropriately for the test.
    let processor = new KeyboardProcessor(this.device);
    processor.keyboardInterface = this.keyboardWithHarness as KeyboardInterface;
    const keyboard = processor.activeKeyboard;

    if(sequence instanceof RecordedKeystrokeSequence) {
      for(let keystroke of sequence.inputs) {
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
          let key = keyboard.layout(this.device.formFactor).getLayer(keystroke.layer).getKey(keystroke.keyName);
          keyEvent = keyboard.constructKeyEvent(key, this.device, processor.stateKeys);
        }

        // Fill in the final details of the KeyEvent...
        keyEvent.device = this.device;

        // And now, execute the keystroke!
        // We don't care too much about particularities of per-keystroke behavior yet.
        // ... we _could_ if we wanted to, though.  The framework is mostly in place;
        // it's a matter of actually adding the feature.
        let ruleBehavior = processor.processKeystroke(new KeyEvent(keyEvent), target);

        if (this.debugMode) {
          console.log("Processing %d:", keyEvent.Lcode);
          console.log("target=%s", JSON.stringify(target, null, '  '));
          console.log("ruleBehavior=%s", JSON.stringify(ruleBehavior, null, '  '));
        }
      }
    } else {
      throw new Error("NodeProctor only supports RecordedKeystrokeSequences for testing at present.");
    }
    return target.getText();
  }
}
