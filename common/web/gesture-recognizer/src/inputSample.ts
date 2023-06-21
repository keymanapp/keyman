/**
 * We want these to be readily and safely converted to and from
 * JSON (for unit test use and development)
 */
export interface InputSample {
  /**
   * Represents the x-coordinate of the input sample
   * in 'client' / viewport coordinates.
   */
  clientX?: number;

  /**
   * Represents the x-coordinate of the input sample in
   * coordinates relative to the recognizer's `targetRoot`.
   */
  targetX: number;

  /**
   * Represents the y-coordinate of the input sample
   * in 'client' / viewport coordinates.
   */
  clientY?: number;

  /**
   * Represents the y-coordinate of the input sample in
   * coordinates relative to the recognizer's `targetRoot`.
   */
  targetY: number;

  /**
   * Represents the timestamp at which the input was observed
   * (in ms)
   */
  t: number;
}

export type InputSampleSequence = InputSample[];

export function isAnInputSample(obj: any): obj is InputSample {
  return 'targetX' in obj && 'targetY' in obj && 't' in obj;
}