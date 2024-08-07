import { type KeyElement } from '../../../keyElement.js';
import VisualKeyboard from '../../../visualKeyboard.js';

import { ActiveSubKey, ActiveKey, KeyDistribution, ActiveKeyBase } from '@keymanapp/keyboard-processor';
import { GestureSequence, GestureStageReport } from '@keymanapp/gesture-recognizer';
import { GestureHandler } from '../gestureHandler.js';
import { distributionFromDistanceMaps } from '../../../corrections.js';
import Modipress from './modipress.js';
import { keySupportsModipress } from '../specsForLayout.js';
import { GesturePreviewHost } from '../../../keyboard-layout/gesturePreviewHost.js';

/**
 * Represents a potential multitap gesture's implementation within KeymanWeb.
 * Once a simple-tap gesture occurs on a key with specified multitap subkeys,
 * this class is designed to take over further processing of said gesture.
 * This includes providing:
 * * UI feedback regarding the state of the ongoing multitap, as appropriate
 * * Proper selection of the appropriate multitap key for subsequent taps.
 */
export default class Multitap implements GestureHandler {
  readonly directlyEmitsKeys = true;

  public readonly baseKey: KeyElement;
  public readonly baseContextToken: number;
  public readonly hasModalVisualization = false;

  private readonly originalLayer: string;

  private readonly multitaps: ActiveSubKey[];
  private tapIndex = 0;
  private modipress: Modipress;

  private readonly sequence: GestureSequence<KeyElement, string>;

  constructor(
    source: GestureSequence<KeyElement, string>,
    vkbd: VisualKeyboard,
    e: KeyElement,
    contextToken: number,
    previewHost: GesturePreviewHost
  ) {
    this.baseKey = e;
    this.baseContextToken = contextToken;
    this.multitaps = [e.key.spec].concat(e.key.spec.multitap);
    this.sequence = source;

    const startModipress = (tap: GestureStageReport<KeyElement, string>) => {
      // In case of a previous modipress that somehow wasn't cleared.
      this.modipress?.clear();

      const modipressHandler = new Modipress(source, vkbd, () => {
        this.modipress = vkbd.activeModipress = null;
      });
      this.modipress = vkbd.activeModipress = modipressHandler;
    }

    this.originalLayer = vkbd.layerId;

    const tapLookahead = (offset: number) => (this.tapIndex + offset) % this.multitaps.length;

    const updatePreview = () => {
      previewHost?.setMultitapHint(this.multitaps[tapLookahead(0)], this.multitaps[tapLookahead(1)], vkbd);
    }

    source.on('complete', () => {
      this.modipress?.cancel();
      this.clear();
    });

    const stageHandler = (tap: GestureStageReport<KeyElement, string>) => {
      switch(tap.matchedId) {
        // In the case that a modifier key supports multitap, reaching this stage
        // indicates that the multitapping is over.  Not the modipressing, though.
        case 'modipress-hold':
          this.clear();
          // We'll let the co-existing modipress handler continue.
          source.off('stage', stageHandler);
          return;
        case 'modipress-end-multitap-transition':
        case 'modipress-multitap-end':
        case 'modipress-end':
        case 'multitap-end':
        case 'simple-tap':
          return;
        case 'modipress-multitap-lock-transition':
          this.modipress?.setLocked();
          return;
        // Once a multitap starts, it's better to emit keys on keydown; that way,
        // if a user holds long, they get what they see if they decide to stop,
        // but also have time to decide if they want to continue to what's next.
        case 'modipress-multitap-start':
        case 'multitap-start':
          break;
        default:
          throw new Error(`Unsupported gesture state encountered during multitap: ${tap.matchedId}`);
      }

      // For rota-style behavior
      this.tapIndex = tapLookahead(1);
      const selection = this.multitaps[this.tapIndex];
      updatePreview();

      const keyEvent = vkbd.keyEventFromSpec(selection);
      keyEvent.baseTranscriptionToken = this.baseContextToken;

      const coord = tap.sources[0].currentSample;
      const baseDistances = vkbd.getSimpleTapCorrectionDistances(coord, this.baseKey.key.spec as ActiveKey);
      if(coord.stateToken != vkbd.layerId && !tap.matchedId.includes('modipress')) {
        const matchKey = vkbd.layerGroup.findNearestKey({...coord, stateToken: vkbd.layerId});

        // Replace the key at the current location for the current layer key
        // with the multitap base key.
        const p = baseDistances.get(matchKey.key.spec);
        if(p == null) {
          console.warn("Could not find current layer's key")
        }
        baseDistances.delete(matchKey.key.spec);
        baseDistances.set(coord.item.key.spec, p);
      }
      keyEvent.keyDistribution = this.currentStageKeyDistribution(baseDistances);

      // TODO for future:  multitap previews.

      // When _some_ multitap keys support layer-swapping but others don't,
      // landing on a non-swap key should preserve the original layer... even
      // if no such 'nextLayer' is specified by default.
      keyEvent.kNextLayer ||= this.originalLayer;

      vkbd.raiseKeyEvent(keyEvent, null);

      // Now that the key has been processed, with a layer possibly changed as a result...
      if(tap.matchedId == 'modipress-multitap-start') {
        startModipress(tap);
      }
    };

    source.on('stage', stageHandler);

    const initialTap = source.stageReports[0];
    if(initialTap.matchedId == 'modipress-start') {
      startModipress(source.stageReports[0]);
    }

    // For this specific instance, we'll go ahead and directly maintain the preview;
    // a touch just ended, and all other updates occur on the start of a new touch.
    updatePreview();

    /* In theory, setting up a specialized recognizer config limited to the base key's surface area
     * would be pretty ideal - it'd provide automatic cancellation if anywhere else were touched.
     *
     * However, because multitap keys can swap layers, and because an invisible layer doesn't provide
     * the expected bounding-box that it would were it visible, it's anything but straightforward to
     * do for certain supported cases.  It's simpler to handle this problem by leveraging the
     * key-finding operation specified on the gesture model and ensuring the base key remains in place.
     */
  }

  currentStageKeyDistribution(baseDistances: Map<ActiveKeyBase, number>): KeyDistribution {
    /* Concept:  use the base distance map - what if the tap was meant for elsewhere?
     * That said, given the base key's probability... modify that by a 'tap distance' metric,
     * where the probability of all taps in the multitap rota sum up to the base key's original
     * probability.
     */

    const baseDistribution = distributionFromDistanceMaps(baseDistances);
    const keyIndex = baseDistribution.findIndex((entry) => entry.keySpec == this.baseKey.key.spec);

    if(keyIndex == -1) { // also covers undefined, but does not include 0.
      // Modipress keys generally get left out of the key-correction calculations.
      if(!keySupportsModipress(this.baseKey)) {
        console.warn("Could not find base key's probability for multitap correction");
      }

      // Decently recoverable; just use the simple-tap distances instead.
      return baseDistribution;
    }

    const baseProb = baseDistribution.splice(keyIndex, 1)[0].p;

    let totalWeight = 0;
    let multitapEntries: {keySpec: ActiveKeyBase, p: number}[] = [];
    for(let i = 0; i < this.multitaps.length; i++) {
      const key = this.multitaps[i];
      // 'standard distance', no real modular effects needed.
      const distStd = Math.abs(i - this.tapIndex) % this.multitaps.length;
      // 'wrapped distance', when the modular effects are definitely needed.
      const distWrap = (i + this.multitaps.length - this.tapIndex) % this.multitaps.length;
      const modularLinDist = distStd < distWrap ? distStd : distWrap;

      // Simple approach for now - we'll ignore timing considerations and
      // just use raw modular distance.
      // Actual tap:  1 (base weight)
      // "one off": 1/4 as likely
      // "two off": 1/9 as likely
      // etc.
      const keyWeight = 1.0 / ((1 + modularLinDist) * (1 + modularLinDist));
      totalWeight += keyWeight;
      multitapEntries.push({
        keySpec: key,
        p: keyWeight
      });
    }

    // Converts from the weights to the final probability values specified by the
    // top comment within this method.
    const scalar = baseProb  / totalWeight;
    multitapEntries.forEach((entry) => {
      entry.p = scalar * entry.p;
    });

    return baseDistribution.concat(multitapEntries).sort((a, b) => b.p - a.p);
  }

  cancel() {
    this.clear();
    this.sequence.cancel();
  }

  clear() {
    // TODO:  for hint stuff.
  }
}