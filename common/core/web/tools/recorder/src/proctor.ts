namespace KMWRecorder {
  export type AssertCallback = (s1: any, s2: any, msg?: string) => void;

  /**
   * Facilitates running Recorder-generated tests on various platforms.
   * 
   * Note that DOM-aware KeymanWeb will implement a Browser-based version, while
   * keyboard-processor and input-processor will use a Node-based version instead.
   */
  export abstract class Proctor {
    device: com.keyman.utils.DeviceSpec;

    _assert: AssertCallback;

    constructor(device: com.keyman.utils.DeviceSpec, assert: AssertCallback) {
      this.device = device;

      this._assert = assert;
    }

    assertEquals(s1: unknown, s2: unknown, msg?: string) {
      if(this._assert) {
        this._assert(s1, s2, msg);
      }
    }

    // Performs global test prep.
    abstract beforeAll();

    // Performs per-test setup
    abstract before();

    /**
     * Allows the proctor to indicate if is capable of executing a suite of tests or not.
     * @param testSuite 
     */
    abstract compatibleWithSuite(testSuite: KeyboardTest): boolean;

    /**
     * Indicates whether or not this Proctor is capable of running the specified set of tests. 
     */ 
    abstract matchesTestSet(testSet: TestSet<any>);

    /**
     * Simulates the specified test sequence for use in testing.
     * @param sequence The recorded sequence, generally provided by a test set.
     */
    abstract simulateSequence(sequence: TestSequence<any>, target?: com.keyman.text.OutputTarget);
  }
}