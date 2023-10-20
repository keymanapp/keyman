export interface GestureHandler {
  /**
   * Triggers cancellation of any further processing for the gesture being handled.
   */
  cancel();

  /**
   * Indicates when a gesture is actively displaying modal visual feedback - a
   * scenario in which key previews (on phones) should be disabled.
   */
  readonly hasModalVisualization: boolean;
}