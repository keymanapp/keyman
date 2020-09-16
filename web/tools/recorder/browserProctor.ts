/// <reference path="browserDriver.ts" />
// In theory, we could totally split Scribe off into its own, separate micro-module.
// Only BrowserDriver.ts and its prerequisites are needed during actual integrated unit testing.
/// <reference path="scribe.ts" />

namespace KMWRecorder {
  type AssertCallback = (s1: any, s2: any, msg?: string) => void;

  function resetElement(ele: HTMLElement): void {
    if(ele instanceof HTMLInputElement || ele instanceof HTMLTextAreaElement) {
      (window['keyman'] as any).resetContext();
      ele.value = "";
    } else {
      (window['keyman'] as any).resetContext();
      if(ele['base']) {
        // Gotta be extra-careful with the simulated touch fields!
        (<any> ele /*as com.keyman.dom.TouchAliasElement*/).setText("", 0);
      } else {
        ele.textContent = "";
      }
    }
  }

  /**
   * Runs Recorder-generated tests in a browser.
   * 
   * Future notes:  further abstraction needed; much of this code will be in common with Node.
   */
  export class BrowserProctor extends Proctor {
    target: HTMLElement;
    usingOSK: boolean;

    constructor(target: HTMLElement, device: com.keyman.utils.DeviceSpec, usingOSK: boolean, assert: AssertCallback) {
      super(device, assert);

      this.target = target;
      this.usingOSK = usingOSK;
    }

    // Performs browser-specific global test prep.
    beforeAll() {
      let ele = this.target;
      (window['keyman'] as any).setActiveElement(ele['base'] ? ele['base'] : ele);
    }

    before() {
      resetElement(this.target);
    }

    compatibleWithSuite(testSuite: KeyboardTest): boolean {
      // So far, the only thing using Recorder code always specifies the necessary data to construct
      // DOM events for use in integrated testing.
      return true;
    }

    // We only need to filter test cases when performing tests in an integrated
    // environment.
    matchesTestSet(testSet: TestSet<any>) {
      return testSet.isValidForDevice(this.device, this.usingOSK);
    }

    // Execution of a test sequence depends on the testing environment; this handles
    // the browser-specific aspects.
    simulateSequence(sequence: TestSequence<any>, outputTarget?: com.keyman.text.OutputTarget): string {
      let driver = new BrowserDriver(this.target);

      // For the version 10.0 spec
      if(sequence instanceof InputEventSpecSequence) {
        return driver.simulateSequence(sequence);

        // For the version 14.0+ spec
      } else if(sequence instanceof RecordedKeystrokeSequence) {
        let inputSpecs: InputEventSpec[] = [];

        for(let i=0; i < sequence.inputs.length; i++) {
          inputSpecs[i] = sequence.inputs[i].inputEventSpec;
        }

        // Converts the sequence back to version 10.0, since it's very well made for browser simulation.
        let eventSpecSequence = new InputEventSpecSequence(inputSpecs, sequence.output, sequence.msg);
        return driver.simulateSequence(eventSpecSequence);
      }
    }
  }
}