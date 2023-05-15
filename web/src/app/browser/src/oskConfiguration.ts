import { type KeyElement, OSKView, VisualKeyboard } from "keyman/engine/osk";
import { getAbsoluteX, getAbsoluteY } from "keyman/engine/dom-utils";
import { DeviceSpec } from "@keymanapp/keyboard-processor";
import ContextManager from "./contextManager.js";

export function setupOskListeners(osk: OSKView, contextManager: ContextManager) {
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
      contextManager.focusAssistant.setMaintainingFocus(false);
      osk.startHide(true);
      keyman.domManager.lastActiveElement = null;
    }
  });

  osk.on('onhide', (hiddenByUser) => {
    // If hidden by the UI, be sure to restore the focus
    if(hiddenByUser) {
      contextManager.activeTarget?.focus();
    }
  });
}