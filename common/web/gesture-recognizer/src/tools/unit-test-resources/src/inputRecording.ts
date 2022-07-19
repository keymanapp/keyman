/// <reference path="jsonObject.ts" />

namespace Testing {
  export interface RecordedCoordSequence {
    sequence: JSONObject<com.keyman.osk.InputSequence> & { samples: JSONObject<com.keyman.osk.InputSample>[] };
    terminationEvent?: "cancel" | "end";
  }

  /**
   * The top-level object produced by the "Test Sequence Recorder".
   */
  export interface RecordedCoordSequenceSet {
    set: RecordedCoordSequence[];
    config: JSONObject<FixtureLayoutConfiguration>;
  }
}