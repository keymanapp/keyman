import { type KeyElement, OSKView, VisualKeyboard } from "keyman/engine/osk";
import { getAbsoluteX, getAbsoluteY } from "keyman/engine/dom-utils";
import { DeviceSpec } from "@keymanapp/keyboard-processor";
import { type EmbeddedGestureConfig } from "keyman/engine/osk";

import { GlobeHint } from './osk/globeHint.js';
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

  return embeddedGestureConfig;
};