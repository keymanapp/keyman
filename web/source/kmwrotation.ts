// References the base KMW object.
/// <reference path="kmwbase.ts" />

namespace com.keyman {
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
  export class RotationManager {
    private keyman: KeymanBase;

    // State variables used by rotations.
    private oskVisible: boolean;
    private isActive: boolean;

    // iOS-oriented members 
    // --------------------
    // We'll assume permutations are complete after this many 'update' iterations.
    private static readonly IDLE_PERMUTATION_CAP = 15;
    // Tracks the number of idle 'update' iterations since the last permutation.
    private idlePermutationCounter: number = RotationManager.IDLE_PERMUTATION_CAP;
    // Tracks the most recent rotation state snapshot.
    private rotState: RotationState;
    // Tracks the window.setTimeout id for rotation update checks.
    private updateTimer: number;
    private static readonly UPDATE_INTERVAL = 20; // 20 ms, that is.
    // --------------------
    
    constructor(keyman: KeymanBase) {
      this.keyman = keyman;
    }

    resolve() {
      this.keyman.alignInputs();

      var osk = this.keyman.osk;
      // TODO:  Reattach later in the refactoring process!
      //osk.hideLanguageList();
      // Force a re-layout for the active keyboard's currently-utilized layout.
      if(this.keyman.core.activeKeyboard) {
        this.keyman.core.activeKeyboard.refreshLayouts();
      }
      
      osk._Load();
      if(this.oskVisible) {
        osk._Show();
      }

      this.isActive = false;

      // If we've been using an update interval loop, we should clear the state information.
      if(this.updateTimer) {
        window.clearInterval(this.updateTimer);
        this.rotState = null;
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
      // If we're in embedded mode, we really should NOT run this method.
      if(this.keyman.isEmbedded) {
        return;
      }

      // Note:  we use wrapper functions instead of `.bind(this)` in this method to facilitate stubbing for our rotation test page.
      var os = this.keyman.util.device.OS;
      var util = this.keyman.util;

      var rotationManager = this;

      if(os == 'iOS') {
      /* iOS is rather inconsistent about these events, with changes to important window state information -
       * especially to `window.innerWidth` - possible after the events trigger!  They don't always trigger
       * the same amount or in a consistently predictable manner.
       * 
       * The overall idea is to wait out all those changes so that we don't produce a bad keyboard layout.
       */
        util.attachDOMEvent(window, 'orientationchange', function() {
          rotationManager.iOSEventHandler();
          return false;
        });
        util.attachDOMEvent(window, 'resize', function() {
          rotationManager.iOSEventHandler();
          return false;
        });
      } else if(os == 'Android') {
        // Android's far more consistent with its event generation than iOS.
        if('onmozorientationchange' in screen) {
          util.attachDOMEvent(screen, 'mozorientationchange', function() {
            rotationManager.initNewRotation();
            return false;
          });
        } else {
          util.attachDOMEvent(window, 'orientationchange', function() {
            rotationManager.initNewRotation();
            return false;
          });
        }

        util.attachDOMEvent(window, 'resize', function() {
          rotationManager.resolve();
          return false;
        });
      }
    }
    
    iOSEventHandler() {
      if(!this.isActive) {
        this.initNewRotation();
        this.rotState = new RotationState();

        this.updateTimer = window.setInterval(this.iOSEventUpdate.bind(this), RotationManager.UPDATE_INTERVAL);
      }

      // If one of the rotation-oriented events just triggered, we should ALWAYS reset the counter.
      this.idlePermutationCounter = 0;
    }

    iOSEventUpdate() {
      var newState = new RotationState();

      if(this.rotState.equals(newState)) {
        if(++this.idlePermutationCounter == RotationManager.IDLE_PERMUTATION_CAP) {
          this.resolve();
        }
      } else {
        this.rotState = newState;
        this.idlePermutationCounter = 0;
      }
    }
  }
}