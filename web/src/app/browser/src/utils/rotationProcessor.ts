import KeymanEngine from "../keymanEngine.js";

class RotationState {
  innerWidth: number;
  innerHeight: number;

  constructor() {
    this.innerWidth = window.innerWidth;
    this.innerHeight = window.innerHeight;
  }

  equals(other: RotationState) {
    return this.innerWidth == other.innerWidth && this.innerHeight == other.innerHeight;
  }
}

// Please reference /testing/rotation-events/index.html and update it as necessary when maintaining this class.
export class RotationProcessor {
  private keyman: KeymanEngine;

  // State variables used by rotations.
  private oskVisible: boolean;
  private isActive: boolean;

  // iOS-oriented members
  // --------------------
  // We'll assume permutations are complete after this many 'update' iterations.
  private static readonly IDLE_PERMUTATION_CAP = 15;
  // Tracks the number of idle 'update' iterations since the last permutation.
  private idlePermutationCounter: number = RotationProcessor.IDLE_PERMUTATION_CAP;
  // Tracks the most recent rotation state snapshot.
  private rotState: RotationState;
  // Tracks the window.setTimeout id for rotation update checks.
  private updateTimer: number;
  private static readonly UPDATE_INTERVAL = 20; // 20 ms, that is.
  // --------------------

  constructor(keyman: KeymanEngine) {
    this.keyman = keyman;
  }

  resolve() {
    var osk = this.keyman.osk;

    // `keyman: KeymanEngine` (modularized app/browser)
    this.keyman.touchLanguageMenu?.hide();
    this.keyman.touchLanguageMenu = null;

    osk.setNeedsLayout();
    if(this.oskVisible) {
      osk.present();
    }

    this.isActive = false;

    // If we've been using an update interval loop, we should clear the state information.
    if(this.updateTimer) {
      window.clearInterval(this.updateTimer);
      this.rotState = null;
    }

    const target = this.keyman.contextManager.activeTarget;
    if(target) {
      // This seems to help with scrolling accuracy in iOS Safari;
      // the scroll tends to consistently go too far without it.
      window.setTimeout(() => {
        this.keyman.ensureElementVisibility(target.getElement());
      }, 0);
    }
  }

  // Used by both Android and iOS.
  initNewRotation() {
    this.oskVisible = this.keyman.osk.isVisible();
    this.keyman.osk.hideNow();
    this.isActive = true;
  }

  /**
   * Establishes rotation-oriented event handling for native-mode KeymanWeb.  At this time, tablet PCs are not directly supported.
   */
  init() {
    // Note:  we use wrapper functions instead of `.bind(this)` in this method to facilitate stubbing for our rotation test page.
    var os = this.keyman.config.hostDevice.OS;
    var util = this.keyman.util;

    if(os == 'ios') {
    /* iOS is rather inconsistent about these events, with changes to important window state information -
      * especially to `window.innerWidth` - possible after the events trigger!  They don't always trigger
      * the same amount or in a consistently predictable manner.
      *
      * The overall idea is to wait out all those changes so that we don't produce a bad keyboard layout.
      */
      util.attachDOMEvent(window, 'orientationchange', () => {
        this.iOSEventHandler();
        return false;
      });
      util.attachDOMEvent(window, 'resize', () => {
        this.iOSEventHandler();
        return false;
      });
    } else if(os == 'android') {
      // Android's far more consistent with its event generation than iOS.
      if('onmozorientationchange' in screen) {
        // 'mozorientationchange' doesn't seem documented at this point, let alone by TypeScript.
        // Plain 'orientationchange' requires a (comparatively) late version of Firefox for Android,
        // though - v44, as opposed to Chrome for Android 18.
        //@ts-ignore
        util.attachDOMEvent(<any>screen, 'mozorientationchange', () => {
          this.initNewRotation();
          return false;
        });
      } else {
        util.attachDOMEvent(window, 'orientationchange', () => {
          this.initNewRotation();
          return false;
        });
      }

      util.attachDOMEvent(window, 'resize', () => {
        this.resolve();
        return false;
      });
    }
  }

  iOSEventHandler() {
    if(!this.isActive) {
      this.initNewRotation();
      this.rotState = new RotationState();

      this.updateTimer = window.setInterval(this.iOSEventUpdate.bind(this), RotationProcessor.UPDATE_INTERVAL);
    }

    // If one of the rotation-oriented events just triggered, we should ALWAYS reset the counter.
    this.idlePermutationCounter = 0;
  }

  iOSEventUpdate() {
    var newState = new RotationState();

    if(this.rotState.equals(newState)) {
      if(++this.idlePermutationCounter == RotationProcessor.IDLE_PERMUTATION_CAP) {
        this.resolve();
      }
    } else {
      this.rotState = newState;
      this.idlePermutationCounter = 0;
    }
  }
}