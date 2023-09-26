/**
 * We want these to be readily and safely converted to and from
 * JSON (for unit test use and development)
 */
export interface InputSample<Type> {
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

  /**
   * The UI/UX 'item' underneath the touchpoint for this sample.
   */
  item?: Type;
}

export type InputSampleSequence<Type> = InputSample<Type>[];

export function isAnInputSample<Type>(obj: any): obj is InputSample<Type> {
  return 'targetX' in obj && 'targetY' in obj && 't' in obj;
}