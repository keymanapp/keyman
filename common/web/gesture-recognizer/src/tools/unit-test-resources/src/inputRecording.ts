import { type SerializedSimpleGestureSource } from "@keymanapp/gesture-recognizer";

import { type FixtureLayoutConfiguration } from "./fixtureLayoutConfiguration.js";
import { type JSONObject } from "./jsonObject.js";

/**
 * The top-level object produced by the "Test Sequence Recorder".
 */
export interface RecordedCoordSequenceSet {
  inputs: SerializedSimpleGestureSource[];
  config: JSONObject<FixtureLayoutConfiguration>;
}