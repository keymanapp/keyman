import { ActiveKeyBase, KeyDistribution } from "@keymanapp/keyboard-processor";
import { CorrectionLayout } from "./correctionLayout.js";

/**
 * Computes a squared 'pseudo-distance' for the touch from each key.  (Not a proper metric.)
 * Intended for use in generating a probability distribution over the keys based on the touch input.
 * @param touchCoords A proportional (x, y) coordinate of the touch within the keyboard's geometry.
 *                           Should be within <0, 0> to <1, 1>.
 * @param correctiveLayout  The corrective-layout mappings for keys under consideration
 *                          by a correction algorithm, also within <0, 0> to <1, 1>.
 * @returns A mapping of key IDs to the 'squared pseudo-distance' of the touchpoint to each key.
 */
export function keyTouchDistances(touchCoords: {x: number, y: number}, correctiveLayout: CorrectionLayout): Map<ActiveKeyBase, number> {
  let keyDists: Map<ActiveKeyBase, number> = new Map<ActiveKeyBase, number>();

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
    keyDists.set(entry.keySpec, distance);
  });

  return keyDists;
}

/**
 * @param squaredDistMap A map of key-id to the squared distance of the original touch from each key under
 * consideration.
 * @returns
 */
export function distributionFromDistanceMaps(squaredDistMaps: Map<ActiveKeyBase, number> | Map<ActiveKeyBase, number>[]): KeyDistribution {
  const keyProbs = new Map<ActiveKeyBase, number>();
  let totalMass = 0;

  if(!Array.isArray(squaredDistMaps)) {
    squaredDistMaps = [squaredDistMaps];
  }

  for(let squaredDistMap of squaredDistMaps) {
    // Should we wish to allow multiple different transforms for distance -> probability, use a function parameter in place
    // of the formula in the loop below.
    for(let key of squaredDistMap.keys()) {
      // We've found that in practice, dist^-4 seems to work pretty well.  (Our input has dist^2.)
      // (Note:  our rule of thumb here has only been tested for layout-based distances.)
      const entry = 1 / (Math.pow(squaredDistMap.get(key), 2) + 1e-6); // Prevent div-by-0 errors.
      totalMass += entry;

      // In case of duplicate key IDs; this can occur if multiple sets are specified.
      keyProbs.set(key, keyProbs.get(key) ?? 0 + entry);
    }
  }

  const list: {keySpec: ActiveKeyBase, p: number}[] = [];

  for(let key of keyProbs.keys()) {
    list.push({keySpec: key, p: keyProbs.get(key) / totalMass});
  }

  return list.sort(function(a, b) {
    return b.p - a.p; // Largest probability keys should be listed first.
  });
}
