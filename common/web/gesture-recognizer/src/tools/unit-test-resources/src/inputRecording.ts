/// <reference path="jsonObject.ts" />

namespace Testing {
  export interface RecordedCoordSequence {
    sequence: com.keyman.osk.JSONTrackedPoint;
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