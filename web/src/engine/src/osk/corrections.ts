import { ActiveKeyBase, KeyDistribution } from "keyman/engine/keyboard";
import { CorrectionLayout } from "./correctionLayout.js";

export type CorrectionDistanceMap = Map<
  string, {
    keySpec: ActiveKeyBase,
    distance: number
  }
>;

/**
 * Computes a squared 'pseudo-distance' for the touch from each key.  (Not a proper metric.)
 * Intended for use in generating a probability distribution over the keys based on the touch input.
 * @param touchCoords A proportional (x, y) coordinate of the touch within the keyboard's geometry.
 *                           Should be within <0, 0> to <1, 1>.
 * @param correctiveLayout  The corrective-layout mappings for keys under consideration
 *                          by a correction algorithm, also within <0, 0> to <1, 1>.
 * @returns A mapping of key IDs to the 'squared pseudo-distance' of the touchpoint to each key.
 */
export function keyTouchDistances(touchCoords: {x: number, y: number}, correctiveLayout: CorrectionLayout): CorrectionDistanceMap {
  const keyDists: CorrectionDistanceMap = new Map();

  // This loop computes a pseudo-distance for the touch from each key. Quite useful for
  // generating a probability distribution.
  correctiveLayout.keys.forEach((entry) => {
    // These represent the within-key distance of the touch from the key's center.
    // Both should be on the interval [0, 0.5].
    let dx = Math.abs(touchCoords.x - entry.centerX);
    let dy = Math.abs(touchCoords.y - entry.centerY);

    // If the touch isn't within the key, these store the out-of-key distance
    // from the closest point on the key being checked.
    let distX: number, distY: number;

    if(dx > 0.5 * entry.width) {
      distX = (dx - 0.5 * entry.width);
      dx = 0.5;
    } else {
      distX = 0;
      dx /= entry.width;
    }

    if(dy > 0.5 * entry.height) {
      distY = (dy - 0.5 * entry.height);
      dy = 0.5;
    } else {
      distY = 0;
      dy /= entry.height;
    }

    // Now that the differentials are computed, it's time to do distance scaling.
    //
    // For out-of-key distance, we scale the X component by the keyboard's aspect ratio
    // to get the actual out-of-key distance rather than proportional.
    distX *= correctiveLayout.kbdScaleRatio;

    // While the keys are rarely perfect squares, we map all within-key distance
    // to a square shape.  (ALT/CMD should seem as close to SPACE as a 'B'.)
    //
    // For that square, we take the rowHeight as its edge lengths.
    distX += dx * entry.height;
    distY += dy * entry.height;

    const distance = distX * distX + distY * distY;
    keyDists.set(entry.keySpec.elementID, {keySpec: entry.keySpec, distance});
  });

  return keyDists;
}

/**
 * @param squaredDistanceMaps A map of key-id to the squared distance of the original touch from each key under
 * consideration.
 * @returns
 */
export function distributionFromDistanceMaps(squaredDistanceMaps: CorrectionDistanceMap | CorrectionDistanceMap[]): KeyDistribution {
  const keyProbs: CorrectionDistanceMap = new Map();
  let totalMass = 0;

  if(!Array.isArray(squaredDistanceMaps)) {
    squaredDistanceMaps = [squaredDistanceMaps];
  }

  for(const squaredDistMap of squaredDistanceMaps) {
    // Should we wish to allow multiple different transforms for distance -> probability, use a function parameter in place
    // of the formula in the loop below.
    for(const [key, distanceTuple] of squaredDistMap.entries()) {
      // We've found that in practice, dist^-4 seems to work pretty well.  (Our input has dist^2.)
      // (Note:  our rule of thumb here has only been tested for layout-based distances.)
      //
      // The 3e-5 fudge-factor may seem a bit high, but it has two purposes:
      // 1. Prevent div-by-0 errors
      // 2. Ensures that the main key's probability doesn't get SO high that we don't
      //    consider correcting to immediate neighbors, even if perfectly accurate.
      const entry = 1 / (Math.pow(distanceTuple.distance, 2) + 3e-5);
      totalMass += entry;

      // In case of duplicate key IDs; this can occur if multiple sets are specified.
      const probTuple = keyProbs.get(key);
      keyProbs.set(key, {keySpec: distanceTuple.keySpec, distance: (probTuple?.distance ?? 0) + entry});
    }
  }

  const list: KeyDistribution = [];

  for(const [key, tuple] of keyProbs.entries()) {
    list.push({elementID: key, keySpec: tuple.keySpec, p: tuple.distance / totalMass});
  }

  return list.sort(function(a, b) {
    return b.p - a.p; // Largest probability keys should be listed first.
  });
}
