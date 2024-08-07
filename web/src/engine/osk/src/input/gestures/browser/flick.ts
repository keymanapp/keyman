import { type KeyElement } from '../../../keyElement.js';
import VisualKeyboard from '../../../visualKeyboard.js';

import { ActiveKey, ActiveKeyBase, ActiveSubKey, KeyDistribution, KeyEvent } from '@keymanapp/keyboard-processor';
import { ConfigChangeClosure, CumulativePathStats, GestureRecognizerConfiguration, GestureSequence, GestureSource, GestureSourceSubview, InputSample, RecognitionZoneSource } from '@keymanapp/gesture-recognizer';
import { GestureHandler } from '../gestureHandler.js';
import { distributionFromDistanceMaps } from '../../../corrections.js';
import { GestureParams } from '../specsForLayout.js';
import { GesturePreviewHost } from '../../../keyboard-layout/gesturePreviewHost.js';
import { TouchLayout } from '@keymanapp/common-types';

export const OrderedFlickDirections = ['n', 'ne', 'e', 'se', 's', 'sw', 'w', 'nw'] as const;

const PI = Math.PI;

export const FlickNameCoordMap = (() => {
  const map = new Map<typeof OrderedFlickDirections[number], [number, number]>();

  const angleIncrement = PI / 4;
  for(let i = 0; i < OrderedFlickDirections.length; i++) {
    map.set(OrderedFlickDirections[i], [angleIncrement * i, 1]);
  }

  return map;
})();

export function lockedAngleForDir(lockedDir: typeof OrderedFlickDirections[number]) {
  return Math.PI / 4 * OrderedFlickDirections.indexOf(lockedDir);
}

export function calcLockedDistance(pathStats: CumulativePathStats<any>, lockedDir: typeof OrderedFlickDirections[number]) {
  const lockedAngle = lockedAngleForDir(lockedDir);

  const rootCoord = pathStats.initialSample;
  const deltaX = pathStats.lastSample.targetX - rootCoord.targetX;
  const deltaY = pathStats.lastSample.targetY - rootCoord.targetY;

  const projY = Math.max(0, -deltaY * Math.cos(lockedAngle));
  const projX = Math.max(0,  deltaX * Math.sin(lockedAngle));

  // For intercardinals, note that Math.cos and Math.sin essentially result in component factors of sqrt(2);
  // essentially, we've already taken the sqrt of distance.
  return projX + projY;
}

export function buildFlickScroller(
  source: GestureSource<KeyElement>,
  lockedDir: typeof OrderedFlickDirections[number],
  previewHost: GesturePreviewHost,
  gestureParams: GestureParams
): (coord: InputSample<KeyElement>) => void {
  return (coord: InputSample<KeyElement>) => {
    const lockedAngle = lockedAngleForDir(lockedDir);

    const maxProgressDist = gestureParams.flick.triggerDist - gestureParams.flick.dirLockDist;
    let progressDist =  Math.max(0, calcLockedDistance(source.path.stats, lockedDir) - gestureParams.flick.dirLockDist);

    // Make progress appear slightly less than it really is; 'near complete' slides thus actually are, so
    // the user doesn't get aggrevated by 'near misses' in that regard.
    //
    // With 0.7, a flick-slide that looks 70% complete will actually be 100% complete.
    // This is about when the flick's key-cap preview starts becoming "mostly visible".
    const FUDGE_SCALE_FACTOR = 0.7;
    // Prevent overshoot
    let slidePc = Math.min(1, FUDGE_SCALE_FACTOR * progressDist / maxProgressDist);

    const previewX =  Math.sin(lockedAngle) * slidePc;
    const previewY = -Math.cos(lockedAngle) * slidePc;

    previewHost?.scrollFlickPreview(previewX, previewY);
  }
}

/**
 * The maximum angle-difference, in radians, allowed before a potential flick
 * is to be considered less likely than its base key.
 *
 * A 60 degree tolerance (Math.PI / 3) + a 'n' flick will consider most angles
 * north of the x-axis more likely than the base key - thus including
 * 'nw' and 'ne' and some 'w' and 'e' paths.
 */
export const MAX_TOLERANCE_ANGLE_SKEW = Math.PI / 3;

/**
 * Represents a flick gesture's implementation within KeymanWeb, including
 * its predictive-text correction aspects.
 */
export default class Flick implements GestureHandler {
  readonly directlyEmitsKeys = true;

  private readonly sequence: GestureSequence<KeyElement, string>;
  private readonly gestureParams: GestureParams;

  private readonly baseSpec: ActiveKey;
  readonly hasModalVisualization: false;

  private baseKeyDistances: Map<ActiveKeyBase, number>;
  private computedFlickDistribution: KeyDistribution;
  private lockedDir: typeof OrderedFlickDirections[number];
  private lockedSelectable: ActiveSubKey;
  private flickScroller: (coord: InputSample<KeyElement, any>) => void;

  constructor(
    sequence: GestureSequence<KeyElement, string>,
    configChanger: ConfigChangeClosure<KeyElement>,
    vkbd: VisualKeyboard,
    e: KeyElement,
    gestureParams: GestureParams,
    previewHost: GesturePreviewHost
  ) {
    this.sequence = sequence;
    this.gestureParams = gestureParams;
    this.baseSpec = e.key.spec as ActiveKey;

    // May be worth a temporary alt config:  global roaming, rather than auto-canceling.

    this.baseKeyDistances = vkbd.getSimpleTapCorrectionDistances(sequence.stageReports[0].sources[0].path.stats.initialSample, this.baseSpec)
    const baseSource = sequence.stageReports[0].sources[0].baseSource;
    let source: GestureSource<KeyElement> = baseSource;

    sequence.on('complete', () => {
      previewHost?.cancel()
    });

    this.sequence.on('stage', (result) => {
      const pathStats = source.path.stats;
      this.computedFlickDistribution = this.flickDistribution(pathStats, true);

      const baseSelection = this.computedFlickDistribution[0].keySpec;

      if(result.matchedId == 'flick-restart') {
        // The gesture-engine's already done this, but we need an analogue for it here.
        source.path.replaceInitialSample(result.sources[0].path.stats.initialSample);
        // Part of the flick-reset process.
        return;
      } if(result.matchedId == 'flick-reset-centering') {
        // Part of the flick-reset process.
        source = baseSource.constructSubview(true, true);
        return;
      } else if(result.matchedId == 'flick-reset-end') {
        this.emitKey(vkbd, this.baseSpec, source.path.stats);
        return;
      } else if(result.matchedId == 'flick-reset') {
        // Instant transitions to flick-mid state; entry indicates a lock "reset".
        // Cancel the flick-viz bit.
        if(this.flickScroller) {
          this.flickScroller(source.currentSample);
          // Clear any previously-set scroller.
          source.path.off('step', this.flickScroller);
        }
        this.lockedDir = null;
        this.lockedSelectable = null;

        // Chops off the prior part of the path
        if(source instanceof GestureSourceSubview) {
          // Clean up the handlers; we're replacing the subview.
          source.disconnect();
        }
        return;
      } else if(result.matchedId == 'flick-mid') {
        if(baseSelection == this.baseSpec) {
          // Do not store a locked direction; the direction we WOULD lock has
          // no valid flick available.
          return;
        }

        const dir = Object.keys(this.baseSpec.flick).find(
          (dir) => this.baseSpec.flick[dir as keyof TouchLayout.TouchLayoutFlick] == baseSelection
        ) as typeof OrderedFlickDirections[number];

        this.lockedDir = dir;
        this.lockedSelectable = baseSelection;

        if(this.flickScroller) {
          // Clear any previously-set scroller.
          source.path.off('step', this.flickScroller);
        }

        this.flickScroller = buildFlickScroller(source, dir, previewHost, this.gestureParams);
        this.flickScroller(source.currentSample);
        source.path.on('step', this.flickScroller);

        return;
      }

      const selection = this.lockedSelectable ?? baseSelection;
      this.emitKey(vkbd, selection, pathStats);
    });

    // Be sure to extend roaming bounds a bit more than usual for flicks, as they can be quick motions.
    const altConfig = this.buildPopupRecognitionConfig(vkbd);
    configChanger({
      type: 'push',
      config: altConfig
    });
  }

  private emitKey(vkbd: VisualKeyboard, selection: ActiveKeyBase, pathStats: CumulativePathStats<any>) {
    let keyEvent: KeyEvent;
    const projectedDistance = calcLockedDistance(pathStats, this.lockedDir);
    if(projectedDistance > this.gestureParams.flick.triggerDist) {
      keyEvent = vkbd.keyEventFromSpec(selection);
    } else {
      // Even if mid-way between base key and actual key.
      keyEvent = vkbd.keyEventFromSpec(this.baseSpec);
    }

    keyEvent.keyDistribution = this.currentStageKeyDistribution(this.baseKeyDistances);

    // emit the keystroke
    vkbd.raiseKeyEvent(keyEvent, null);
  }

  private buildPopupRecognitionConfig(vkbd: VisualKeyboard): GestureRecognizerConfiguration<KeyElement, string> {
    const roamBounding: RecognitionZoneSource = {
      getBoundingClientRect() {
        // We don't want to actually use Number.NEGATIVE_INFINITY or Number.POSITIVE_INFINITY
        // because that produces a DOMRect with a few NaN fields, and we don't want _that_.

        // Way larger than any screen resolution should ever be.
        const base = Number.MAX_SAFE_INTEGER;
        return new DOMRect(-base, -base, 2*base, 2*base);
      }
    }

    return {
      ...vkbd.gestureEngine.config,
      maxRoamingBounds: roamBounding,
      safeBounds: roamBounding // if embedded, ensure top boundary extends outside the WebView!
    }
  }

  cancel() {
    // Cancel any flick-specific visualization stuff.
  }

  /**
   * Builds a probability distribution for the likelihood of any key-supported flick
   * (or lack thereof) being intended given the path properties specified.
   * @param pathStats
   * @returns
   */
  flickDistribution(pathStats: CumulativePathStats, ignoreThreshold?: boolean) {
    // NOTE:  does not consider flick direction-locking.
    const flickSet = this.baseSpec.flick;

    /* Time to compute flick corrections!
     *
     * The best way to define a "flick distance"... the polar coordinate system, which
     * uses (angle, dist) instead of (x, y), with dist clamped at the net distance
     * threshold.  This way, a diagonal flick doesn't have odd effects due to
     * "corner of the square" positioning if hard-bounding on x & y instead.
     *
     * The greater the net distance, the less likely that the base key will be selected,
     * no matter which flick is actually picked.  In the case that only one flick is
     * supported, and in the opposite direction from the actual input, both the flick
     * and base key will be considered equally likely.  (One due to direction, the
     * other due to distance.)
     *
     * We do this even if pred-text is disabled:  it's the easiest way to pick a
     * 'nearest-neighbor' flick if the direction doesn't fall perfectly within a
     * defined bucket.  (It lets us 'fudge' the boundaries a bit.)
     */

    // Step 1:  build the list of supported flicks, including the base key as a fallback.
    let keys: {
      spec: ActiveKeyBase,
      coord: [number, number]
    }[] = [{
      spec: this.baseSpec,
      coord: [NaN, 0]
    }];

    keys = keys.concat(Object.keys(flickSet).map((dir) => {
      return {
        spec: flickSet[dir as typeof OrderedFlickDirections[number]] as ActiveSubKey,
        coord: FlickNameCoordMap.get(dir as typeof OrderedFlickDirections[number])
      };
    }));

    const angle = pathStats.angle;

    // Determine whether or not the flick distance-threshold has been passed...
    // and how close it is to being passed if not yet passed.
    const TRIGGER_DIST = this.gestureParams.flick.triggerDist;
    const baseDist = Math.min(TRIGGER_DIST, ignoreThreshold ? TRIGGER_DIST : pathStats.netDistance);
    const distThresholdRatio = baseDist / TRIGGER_DIST;

    let totalMass = 0;
    const distribution: KeyDistribution = keys.map((entry) => {
      let angleDist = 0;
      const coord = entry.coord;
      if(!isNaN(coord[0])) {
        const angleDelta1 = angle - coord[0];
        const angleDelta2 = 2 * PI + coord[0] - angle; // because of angle wrap-around.

        // NOTE:  max linear angle dist:  PI.  (Angles are between 0 and 2*PI.)
        angleDist = Math.min(angleDelta1 * angleDelta1, angleDelta2 * angleDelta2);
      }

      /*
       * Max linear geometric distance: 1.  We should weight it for better comparison
       * to angleDist.
       *
       * MAX_TOLERANCE_ANGLE_SKEW is a perfect conversion factor.  Being off by a
       * dist of 1 then converts into angle-equivalent distance of the skew, making
       * it an equal contributor to overall distance.
       */
      const geoDelta = MAX_TOLERANCE_ANGLE_SKEW * (coord[1] - distThresholdRatio);

      const geoDist = (geoDelta * geoDelta);
      const mass = 1 / (angleDist + geoDist + 1e-6); // prevent div-by-zero
      totalMass += mass;

      return {
        keySpec: entry.spec,
        p: mass
      }
    });

    const normalizer = 1.0 / totalMass;
    distribution.forEach((entry) => entry.p *= normalizer);

    // Sort in descending probability order.
    return distribution.sort((a, b) => b.p - a.p);
  }

  currentStageKeyDistribution(baseDistMap: Map<ActiveKeyBase, number>): KeyDistribution {
    const baseSpec = this.baseSpec;
    const baseDistances = this.baseKeyDistances;
    const flickDistrib = this.computedFlickDistribution;
    const entry = baseDistances.get(baseSpec);

    if(!entry) {
      const best = flickDistrib[0];
      return [
        {
          keySpec: best.keySpec,
          p: 1
        }
      ];
    }

    // Corrections are enabled:  return a full distribution
    const baseKeyFlickProbIndex = flickDistrib.findIndex((entry) => entry.keySpec == baseSpec);
    // Remove the base-key entry from the flick distribution but save its probability.
    // We'll scale the base distribution down so that its sum equals that value, enabling
    // us to merge the distributions while preserving normalization.
    const baseKeyFlickProb = flickDistrib.splice(baseKeyFlickProbIndex, 1)[0].p;

    const baseDistribution = distributionFromDistanceMaps(baseDistances);
    return flickDistrib.concat(baseDistribution.map((entry) => {
      return {
        keySpec: entry.keySpec,
        // Scale down all base key probabilities by how likely the base key's selection from
        // the flick itself is.
        p: entry.p * baseKeyFlickProb
      }
    }));
  }
}