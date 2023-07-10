import { type KeyElement, OSKView, VisualKeyboard } from "keyman/engine/osk";
import { getAbsoluteX, getAbsoluteY } from "keyman/engine/dom-utils";
import { DeviceSpec } from "@keymanapp/keyboard-processor";
import { type EmbeddedGestureConfig } from "keyman/engine/osk";

import { GlobeHint } from './osk/globeHint.js';
import { KeyTip } from './osk/keytip.js';
import { PendingLongpress } from './osk/pendingLongpress.js';
import type KeymanEngine from "./keymanEngine.js";

export function setupEmbeddedListeners(engine: KeymanEngine, osk: OSKView) {
  osk.on('globekey', (key, on) => {
    if(on) {
      if(typeof engine.showKeyboardList == 'function') { // OSKView event: shouldShowLanguageMenu
        engine.showKeyboardList();                       // Is connected to VisualKeyboard event: globeKey
      }
    } else if(osk.vkbd) {
      if(osk.vkbd.menuEvent) {
        this.highlightKey(osk.vkbd.menuEvent, false);
      }
      if(typeof(engine.menuKeyUp) == 'function') { // VisualKeyboard event:  globeKey
        engine.menuKeyUp();
      }
      osk.vkbd.menuEvent = null;
    }

    if(osk.vkbd) {
      osk.vkbd.highlightKey(key, false); // never leave the globe key highlighted
    }
  });

  osk.on('hiderequested', (key) => {
    if(osk.vkbd) {
      osk.vkbd.highlightKey(key, false);
    }
    if(typeof engine.hideKeyboard == 'function') { // VisualKeyboard event:  hideRequested
      engine.hideKeyboard();
    }
  })
}

export function buildEmbeddedGestureConfig(device: DeviceSpec) {
  const embeddedGestureConfig: EmbeddedGestureConfig = {
    createGlobeHint: (vkbd) => {
      return new GlobeHint(vkbd);
    }
  }

  if(device.OS == DeviceSpec.OperatingSystem.Android) {
    embeddedGestureConfig.createKeyTip = (vkbd) => {
      if(vkbd.device.formFactor == 'phone') {
        return new KeyTip(window['oskCreateKeyPreview'], window['oskClearKeyPreview']);
      } else {
        return null;
      }
    }

    embeddedGestureConfig.startLongpress = (vkbd, key) => {
      if(typeof(window['oskCreatePopup']) == 'function') {
        var xBase = getAbsoluteX(key) - getAbsoluteX(vkbd.kbdDiv) + key.offsetWidth/2,
            yBase = getAbsoluteY(key);

        // #3718: No longer prepend base key to subkey array
        window['oskCreatePopup'](key['subKeys'], xBase, yBase, key.offsetWidth, key.offsetHeight);

        return new PendingLongpress(vkbd, key);
      } else {
        // When embedded within our Android app, we expect the `oskCreatePopup` function to
        // exist; all subkey control is delegated to the app.
        //
        // No function = big problem.
        console.error("Missing `oskCreatePopup` function for engine integration.");
        return null;
      }
    }

    return embeddedGestureConfig;
  }
};