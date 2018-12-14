// Includes KeymanWeb's Device class, as it's quite a useful resource for KMW-related projects.
/// <reference path="../source/kmwdevice.ts" />

type KeyboardMap = {[id: string]: any};

namespace com.keyman.renderer {


  export class BatchRenderer {
    // Filters the keyboard array to ensure only a single entry remains, rather than an entry per language.
    private filterKeyboards(): KeyboardMap {
      let keyman = window['keyman'];

      let kbds = keyman['getKeyboards']();

      let keyboardMap = [];

      for(var i = 0; i < kbds.length; i++) {
        let id: string = kbds[i]["InternalName"];
        if(keyboardMap[id]) {
          continue;
        } else {
          keyboardMap[id] = kbds[i];
        }
      }

      return keyboardMap;
    }

    run() {
      if(window['keyman']) {
        let keyman = window['keyman'];

        // Assumes that the keyboards have been preloaded for us.
        let kbds = this.filterKeyboards();

        console.log("Unique keyboard ids detected: " + Object.keys(kbds).length);
      } else {
        console.error("KeymanWeb not detected!");
      }
    }
  }

  (function(){
    window['kmw_renderer'] = new com.keyman.renderer.BatchRenderer();
  })();
}