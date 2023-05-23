import { type KeyElement, OSKView, VisualKeyboard } from "keyman/engine/osk";
import { KEYMAN_VERSION } from "@keymanapp/keyman-version";
import ContextManager from "./contextManager.js";
import { KeymanEngine } from "./keymanEngine.js";
import { LanguageMenu } from "./languageMenu.js";

export function setupOskListeners(engine: KeymanEngine, osk: OSKView, contextManager: ContextManager) {
  const focusAssistant = contextManager.focusAssistant;

  osk.on('globeKey', (key, on) => { // K_LOPT
    if(on) {
      if(osk.hostDevice.touchable) {
        engine.touchLanguageMenu = new LanguageMenu(engine);
        engine.touchLanguageMenu.show();
      }
    }

    if(osk.vkbd) {
      osk.vkbd.highlightKey(key, false); // never leave the globe key highlighted
    }
  });

  osk.on('hideRequested', (key) => { // K_ROPT
    if(osk) {
      osk.startHide(true);
      contextManager.forgetActiveTarget();
    }
  });

  osk.on('onhide', (hiddenByUser) => {
    // If hidden by the UI, be sure to restore the focus
    if(hiddenByUser) {
      contextManager.activeTarget?.focus();
    }
  });

  osk.on('showBuild', () => {
    internalAlert('KeymanWeb Version ' + KEYMAN_VERSION.VERSION + '<br /><br />'
        +'<span style="font-size:0.8em">Copyright &copy; 2007-2023 SIL International</span>');
  });

  osk.on('dragMove', async (promise) => {
    focusAssistant.restoringFocus = true;

    await promise;

    contextManager.restoreLastActiveTarget();

    focusAssistant.restoringFocus = false;
    focusAssistant.setMaintainingFocus(false);
  });

  osk.on('resizeMove', async (promise) => {
    focusAssistant.restoringFocus = true;

    await promise;
    contextManager.restoreLastActiveTarget();

    focusAssistant.restoringFocus = false;
    focusAssistant.setMaintainingFocus(false);
  });

  osk.on('pointerInteraction', async (promise) => {
   // On event start
   focusAssistant.setMaintainingFocus(true);

   // The original did nothing when the pointer left the OSK's bounds.  Possible bug?
   // await promise;  // should we wish to change that.
  });
}