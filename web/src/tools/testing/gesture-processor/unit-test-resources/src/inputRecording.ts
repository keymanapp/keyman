import { type SerializedGestureSource } from "keyman/engine/gesture-processor";

import { type FixtureLayoutConfiguration } from "./fixtureLayoutConfiguration.js";
import { type JSONObject } from "./jsonObject.js";

/**
 * The top-level object produced by the "Test Sequence Recorder".
 */
export interface RecordedCoordSequenceSet {
  inputs: SerializedGestureSource[];
  config: JSONObject<FixtureLayoutConfiguration>;
}