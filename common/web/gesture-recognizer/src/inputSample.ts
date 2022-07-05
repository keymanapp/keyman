namespace com.keyman.osk {
  /**
   * We want these to be readily and safely converted to and from
   * JSON (for unit test use and development)
   */
  export interface InputSample {
    /**
     * Represents the x-coordinate of one sample of the input
     * in 'client' / viewport coordinates.
     */
    x: number;

    /**
     * Represents the y-coordinate of one sample of the input
     * in 'client' / viewport coordinates.
     */
    y: number;

    /**
     * Represents the timestamp at which the input was observed
     * (in ms)
     */
    t: number;
  }

  export type InputSampleSequence = InputSample[];
}