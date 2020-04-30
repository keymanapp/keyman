///<reference path="index.ts" />
///<reference path="../node_modules/@keymanapp/keyboard-processor/src/text/keyboardProcessor.ts" />

namespace KMWRecorder {
  export class NodeProctor extends Proctor {
    private processor: com.keyman.text.KeyboardProcessor;

    constructor(processor: com.keyman.text.KeyboardProcessor, device: com.keyman.text.EngineDeviceSpec, assert: AssertCallback) {
      super(device, assert);

      // TODO:  if we based this on a keyboard instead, we could throw out the KeyboardProcessor instance after
      //        every test for a guaranteed clean slate.  Heck, we need the Keyboard's data to reconstruct
      //        KeyEvents for testing.
      this.processor = processor;
    }

    beforeAll() {
      //
    }
    before() {
      //
    }

    compatibleWithSuite(testSuite: KeyboardTest): boolean {
      // Original-version tests did not supply core-compatible KeyEvent data.
      return testSuite.specVersion.equals(KeyboardTest.FALLBACK_VERSION);
    }

    matchesTestSet(testSet: TestSet<any>) {
      // KeyboardProcessor is abstract enough to run tests aimed at any platform.
      return true;
    }

    simulateSequence(sequence: TestSequence<any>): string {
      // Start with an empty OutputTarget
      let target = new com.keyman.text.Mock();

      if(sequence instanceof RecordedKeystrokeSequence) {
        for(let keystroke of sequence.inputs) {
          let keyEvent: com.keyman.text.KeyEvent;
          if(keystroke instanceof RecordedPhysicalKeystroke) {
            // TODO:  reconstruct the KeyEvent.
          } else if(keystroke instanceof RecordedSyntheticKeystroke) {
            // TODO:  reconstruct the KeyEvent.
          }

          // Fill in the final details of the KeyEvent...
          keyEvent.Ltarg = target;
          keyEvent.device = this.device;

          // And now, execute the keystroke!
          // We don't care too much about particularities of per-keystroke behavior yet.
          // ... we _could_ if we wanted to, though.  The framework is mostly in place; 
          // it's a matter of actually adding the feature.
          this.processor.processKeystroke(keyEvent, target);
        }
      } else {
        throw new Error("NodeProctor only supports RecordedKeystrokeSequences for testing at present.");
      }

      return target.getText();
    }

  }
}