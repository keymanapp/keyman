/// <reference path="browserDriver.ts" />

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
  export class BrowserProctor {
    target: HTMLElement;
    device: com.keyman.text.EngineDeviceSpec;
    usingOSK: boolean;

    _assert: AssertCallback;

    constructor(target: HTMLElement, device: com.keyman.text.EngineDeviceSpec, usingOSK: boolean, assert: AssertCallback) {
      this.target = target;
      this.device = device;
      this.usingOSK = usingOSK;

      this._assert = assert;
    }

    assertEquals(s1: unknown, s2: unknown, msg?: string) {
      if(this._assert) {
        this._assert(s1, s2, msg);
      }
    }

    // Performs browser-specific global test prep.
    beforeAll() {
      let ele = this.target;
      (window['keyman'] as any).setActiveElement(ele['base'] ? ele['base'] : ele);
    }

    before() {
      resetElement(this.target);
    }

    // We only need to filter test cases when performing tests in an integrated
    // environment.
    matchesTestSet(testSet: TestSet<any>) {
      return testSet.isValidForDevice(this.device, this.usingOSK);
    }

    // Execution of a test sequence depends on the testing environment; this handles
    // the browser-specific aspects.
    simulateSequence(sequence: TestSequence): string {
      let driver = new BrowserDriver(this.target);

      // Yes, it's pretty simple for now... but this is only one of two code paths
      // that will exist here, hence the abstraction.
      if(sequence instanceof InputEventSpecSequence) {
        return driver.simulateSequence(sequence);
      } else {
        // Convert new spec sequence to an InputEventSpecSequence.
        // is not yet implemented.
        throw new Error("code path not yet implemented");
      }
    }
  }
}