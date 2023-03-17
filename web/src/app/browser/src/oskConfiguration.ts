import { type KeyElement, OSKView, VisualKeyboard } from "keyman/engine/osk";
import { getAbsoluteX, getAbsoluteY } from "keyman/engine/dom-utils";
import { DeviceSpec } from "@keymanapp/keyboard-processor";
import { type EmbeddedGestureConfig } from "keyman/engine/osk";

export function setupEmbeddedListeners(osk: OSKView) {
  osk.on('globeKey', (key, on) => {
    if(on) {
      if(osk.hostDevice.touchable) {
        this.lgMenu = new LanguageMenu(com.keyman.singleton);
        this.lgMenu.show();
      }
    }

    if(osk.vkbd) {
      osk.vkbd.highlightKey(key, false); // never leave the globe key highlighted
    }
  });

  osk.on('hideRequested', (key) => {
    if(osk) {
      keyman.uiManager.setActivatingUI(false);
      osk.startHide(true);
      keyman.domManager.lastActiveElement = null;
    }
  });

  osk.on('onhide', () => {
    // If hidden by the UI, be sure to restore the focus
    if(hiddenByUser && this.activeTarget) {
      this.activeTarget?.focus();
    }
  });
}