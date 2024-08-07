import { type DeviceSpec } from "@keymanapp/web-utils";
import { type OutputTarget } from "keyman/engine/keyboard";

import type { KeyboardTest, TestSet, TestSequence } from "./index.js";

export type AssertCallback = (s1: any, s2: any, msg?: string) => void;

/**
 * Facilitates running Recorder-generated tests on various platforms.
 *
 * Note that DOM-aware KeymanWeb will implement a Browser-based version, while
 * keyboard and input-processor will use a Node-based version instead.
 */
export default abstract class Proctor {
  device: DeviceSpec;

  _assert: AssertCallback;

  constructor(device: DeviceSpec, assert: AssertCallback) {
    this.device = device;

    this._assert = assert;
  }

  assertEquals(s1: unknown, s2: unknown, msg?: string) {
    if(this._assert) {
      this._assert(s1, s2, msg);
    }
  }

  // Performs global test prep.
  abstract beforeAll(): Promise<void>;

  // Performs per-test setup
  abstract before(): void;

  /**
   * Allows the proctor to indicate if is capable of executing a suite of tests or not.
   * @param testSuite
   */
  abstract compatibleWithSuite(testSuite: KeyboardTest): boolean;

  /**
   * Indicates whether or not this Proctor is capable of running the specified set of tests.
   */
  abstract matchesTestSet(testSet: TestSet<any>): boolean;

  /**
   * Simulates the specified test sequence for use in testing.
   * @param sequence The recorded sequence, generally provided by a test set.
   */
  abstract simulateSequence(sequence: TestSequence<any>, target?: OutputTarget): Promise<string>;
}