import OSKSubKey from './oskSubKey.js';
import { type KeyElement } from '../../../keyElement.js';
import OSKBaseKey from '../../../keyboard-layout/oskBaseKey.js';
import VisualKeyboard from '../../../visualKeyboard.js';

import { DeviceSpec, KeyEvent, ActiveSubKey, ActiveKey, KeyDistribution } from '@keymanapp/keyboard-processor';
import { ConfigChangeClosure, GestureRecognizerConfiguration, GestureSequence, PaddedZoneSource } from '@keymanapp/gesture-recognizer';
import { GestureHandler } from '../gestureHandler.js';

/**
 * Represents a potential multitap gesture's implementation within KeymanWeb.
 * Once a simple-tap gesture occurs on a key with specified multitap subkeys,
 * this class is designed to take over further processing of said gesture.
 * This includes providing:
 * * UI feedback regarding the state of the ongoing multitap, as appropriate
 * * Proper selection of the appropriate multitap key for subsequent taps.
 */
export default class Multitap implements GestureHandler {
  public readonly baseKey: KeyElement;
  public readonly hasModalVisualization = false;

  private readonly multitaps: ActiveSubKey[];
  private tapIndex = 0;

  private sequence: GestureSequence<KeyElement, string>;

  constructor(
    source: GestureSequence<KeyElement, string>,
    vkbd: VisualKeyboard,
    e: KeyElement
  ) {
    this.baseKey = e;
    this.multitaps = e.key.spec.multitap;

    // // For multitaps, keeping the key highlighted makes sense.  I think.
    // this.baseKey.key.highlight(true);

    source.on('complete', () => {
      if(source.stageReports.length > 1) {
      }
      // this.currentSelection?.key.highlight(false);
      this.clear();
    });

    source.on('stage', (tap) => {
      switch(tap.matchedId) {
        case 'modipress-multitap-start':
          return;
        case 'multitap':
        case 'modipress-multitap-end':
          break;
        default:
          throw new Error("Unsupported gesture state encountered during multitap sequence");
      }

      // For rota-style behavior
      this.tapIndex = (this.tapIndex + 1) % (this.baseKey.key.spec.multitap.length+1);

      const selection = this.tapIndex == 0
        ? this.baseKey.key.spec
        : this.multitaps[this.tapIndex-1];

      const keyEvent = vkbd.keyEventFromSpec(selection);
      const keyDistances = this.currentStageKeyDistances();
      // TODO: special fat-finger alternates stuff?
      vkbd.raiseKeyEvent(keyEvent, null, keyDistances);
    });

    /* In theory, setting up a specialized recognizer config limited to the base key's surface area
     * would be pretty ideal - it'd provide automatic cancellation if anywhere else were touched.
     *
     * However, because multitap keys can swap layers, and because an invisible layer doesn't provide
     * the expected bounding-box that it would were it visible, it's anything but straightforward to
     * do for certain supported cases.  It's simpler to handle this problem by leveraging the
     * key-finding operation specified on the gesture model and ensuring the base key remains in place.
     */
  }

  currentStageKeyDistances(): KeyDistribution {
    // TODO:  multitap corrections
    return [];
  }

  cancel() {
    this.clear();
    this.sequence.cancel();
  }

  clear() {
    // TODO:  for hint stuff.
  }
}