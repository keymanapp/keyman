/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2023-10-16.
 *
 * The GestureHandler interface defines methods common to all OSK
 * gesture-handling classes, providing a common abstraction for common-path
 * gesture support.
 */

import { KeyDistribution } from "keyman/engine/keyboard";
import { type CorrectionDistanceMap } from "../../corrections.js";

export interface GestureHandler {
  /**
   * Triggers cancellation of any further processing for the gesture being
   * handled.
   *
   * The method should be able to safely handle the `cancel` method being called
   * multiple times, much like `clearTimeout` in JS.
   */
  cancel(): void;

  /**
   * Indicates when a gesture is actively displaying modal visual feedback - a
   * scenario in which key previews (on phones) should be disabled.
   */
  readonly hasModalVisualization: boolean;

  /**
   * Indicates whether or not the handler is responsible for emitting keystrokes itself
   * or if default handling should take effect.
   *
   * Default handling is predicated on there being corresponding DOM elements for a key,
   * so multitaps and flicks can't be handled via the default mechanism.  (Unless we
   * want to spin up dummy elements for them.)
   */
  readonly directlyEmitsKeys: boolean;

  /**
   * Implementations of this method should return an appropriate statistic distribution
   * for the likelihood of most-relevant keys that may have been intended for the
   * most recent keystroke generated.  Alternatively, returning `null` or `undefined`
   * will use the default simple-tap distribution.
   *
   * This method will be provided a map of the "corrective distance" used for
   * simple-tap corrections, allowing gestures to utilize the values as a basis
   * for their own calculations as appropriate.
   *
   * @param baseDistanceMap The distance map used for simple-tap corrections
   */
  currentStageKeyDistribution(baseDistanceMap: CorrectionDistanceMap): KeyDistribution | null;
}