import { ActiveKeyBase } from "./activeLayout.js";

// This interface lives within the keyboard-processor package because
// fat-finger correction layout info was spec'd there in and before
// Keyman 16.0.

/**
 * Defines correction-layout mappings for keys to be considered by
 * the fat-finger algorithm and its related calculations, which are
 * used to determine the "closest keys" for corrections.
 */
export interface CorrectionLayoutEntry {
  /**
   * The ID of the key corresponding to this entry.
   */
  readonly keySpec: ActiveKeyBase;

  /**
   * Represents the center x coordinate of the key based on the coordinate system
   * with the keyboard's layout bounding box mapped to a box from <0, 0> to <1, 1>.
   */
  readonly centerX: number;

  /**
   * Represents the center y coordinate of the key based on the coordinate system
   * with the keyboard's layout bounding box mapped to a box from <0, 0> to <1, 1>.
   */
  readonly centerY: number;

  /**
   * Represents the key's width based on the coordinate system with the
   * keyboard's layout bounding box mapped to a box from <0, 0> to <1, 1>.
   */
  readonly width: number;

  /**
   * Represents the key's height based on the coordinate system with the
   * keyboard's layout bounding box mapped to a box from <0, 0> to <1, 1>.
   */
  readonly height: number;
}

export interface CorrectionLayout {
  /**
   * Defines the mappings of each key to be considered by a key-correction
   * algorithm.  The key's bounding box should be defined relative to its
   * containers bounding box, with both mapped to a coordinate system from
   * <0, 0> to <1, 1> - a unit square.
   */
  keys: CorrectionLayoutEntry[];

  /**
   * The ratio of the keyboard's horizontal scale to its vertical scale.
   * For a 400 x 200 keyboard, should be 2.
   */
  kbdScaleRatio: number;
}