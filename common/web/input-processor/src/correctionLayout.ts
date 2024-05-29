import { ActiveKey, ActiveKeyBase, ActiveLayer, ActiveRow, Codes } from "@keymanapp/keyboard-processor";

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
   * container's bounding box, with both mapped to a coordinate system from
   * <0, 0> to <1, 1> - a unit square.
   */
  keys: CorrectionLayoutEntry[];

  /**
   * The ratio of the keyboard's horizontal scale to its vertical scale.
   * For a 400 x 200 keyboard, should be 2.
   */
  kbdScaleRatio: number;
}

// Not compatible with subkeys - their layout data is only determined (presently) at runtime.
export class CorrectiveBaseKeyLayout implements CorrectionLayoutEntry {
  readonly keySpec: ActiveKey;
  readonly centerX: number;
  readonly centerY: number;
  readonly width: number;
  readonly height: number;

  constructor(layer: ActiveLayer, row: ActiveRow, key: ActiveKey) {
    this.keySpec = key;
    this.centerX = key.proportionalX;
    this.centerY = row.proportionalY;
    this.width = key.proportionalWidth;
    this.height = layer.rowProportionalHeight;
  }
}

/**
 * Indicates whether or not the specified key should be considered as a valid
 * key-correction target during fat-finger operations.
 * @param key
 * @returns `true` if valid, `false` if invalid.
 */
export function correctionKeyFilter(key: ActiveKeyBase): boolean {
  // If the key lacks an ID, just skip it.  Sometimes used for padding.
  if(!key.baseKeyID) {
    return false;
    // Attempt to filter out known non-output keys.
    // Results in a more optimized distribution.
  } else if(Codes.isKeyNotCorrected(key.baseKeyID)) {
    return false;
  } else if(key.isPadding) { // to the user, blank / padding keys do not exist.
    return false;
  } else {
    return true;
  }
}


/**
 * Builds the corrective layout object corresponding to the specified keyboard layer,
 * as needed for use of our key-correction algorithms.
 *
 * @param layer         The layer spec to reference for key corrections.
 * @param kbdScaleRatio The ratio of the keyboard's horizontal scale to its vertical scale.
 *                           For a 400 x 200 keyboard, should be 2.
 */
export function buildCorrectiveLayout(layer: ActiveLayer, kbdScaleRatio: number) {
  return {
    keys: layer.row.map((row) => {
      return row.key.map((key) => new CorrectiveBaseKeyLayout(layer, row, key));
      // ... and flatten/merge the resulting arrays.
    }).reduce((flattened, rowEntries) => flattened.concat(rowEntries), [])
    .filter((entry) => correctionKeyFilter(entry.keySpec)),
    kbdScaleRatio: kbdScaleRatio
  };
}