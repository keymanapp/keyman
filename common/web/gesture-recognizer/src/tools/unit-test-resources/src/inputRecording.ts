/// <reference path="jsonObject.ts" />

namespace Testing {
  /**
   * The top-level object produced by the "Test Sequence Recorder".
   */
  export interface RecordedCoordSequenceSet {
    inputs: com.keyman.osk.JSONTrackedInput[];
    config: JSONObject<FixtureLayoutConfiguration>;
  }
}