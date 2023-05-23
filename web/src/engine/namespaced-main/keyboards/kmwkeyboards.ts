/// <reference path="../kmwbase.ts" />

namespace com.keyman.keyboards {
  export class KeyboardTag {
    stores: {[text: string]: text.ComplexKeyboardStore} = {};
  }

  export class KeyboardManager {
    keymanweb: KeymanBase;

    activeStub: KeyboardStub = null;
    keyboardStubs: KeyboardStub[] = [];

    // For deferment of adding keyboards until keymanweb initializes
    deferment: Promise<void> = null;
    endDeferment:() => void;

    // The following was not actually utilized within KeymanWeb; I think it's handled via different logic.
    // See setDefaultKeyboard() below.
    dfltStub = null;           // First keyboard stub loaded - default for touch-screen devices, ignored on desktops

    keyboards: any[] = [];

    linkedScripts: HTMLScriptElement[] = [];

    constructor(kmw: KeymanBase) {
      this.keymanweb = kmw;

      let _this = this;
      this.deferment = new Promise(function(resolve) {
        _this.endDeferment = resolve;
      });
    }

    // Called on the embedded path at the end of its initialization.
    setDefaultKeyboard() {
      if(this.keyboardStubs.length > 0) {
        // Select the first stub as our active keyboard.
        this._SetActiveKeyboard(this.keyboardStubs[0]['KI'], this.keyboardStubs[0]['KLC']);
        return true;
      } else {
        return false;
      }
    }

    /* TODO: why not use util.loadCookie and saveCookie?? */

    /**
     * Restore the most recently used keyboard, if still available
     */
    restoreCurrentKeyboard() {
      var stubs = this.keyboardStubs, i, n=stubs.length;
      let core = com.keyman.singleton.core;

      // Do nothing if no stubs loaded
      if(stubs.length < 1) return;

      // If no saved keyboard, default to US English, else first loaded stub
      var d=this.getSavedKeyboard();
      var t=d.split(':');

      // Identify the stub with the saved keyboard
      t=d.split(':');
      if(t.length < 2) t[1]='';

      // This loop is needed to select the correct stub when several apply to a given keyboard
      // TODO: There should be a better way!
      for(i=0; i<n; i++)
      {
        if(stubs[i]['KI'] == t[0] && (stubs[i]['KLC'] == t[1] || t[1] == '')) break;
      }

      // Sets the default stub (as specified with the `getSavedKeyboard` call) as active.
      // if((i < n) || (device.touchable && (this.activeKeyboard == null)))
      if((i < n) || (core.activeKeyboard == null))
      {
        this._SetActiveKeyboard(t[0],t[1],false);
        this.keymanweb.globalKeyboard = t[0];
        this.keymanweb.globalLanguageCode = t[1];

        this.doKeyboardChange(t[0],t[1]);        // And update the UI if necessary
      }
    }

    shutdown() {
      for(let script of this.linkedScripts) {
        if(script.remove) {
          script.remove();
        } else if(script.parentNode) {
          script.parentNode.removeChild(script);
        }
      }
    }
  }
}
