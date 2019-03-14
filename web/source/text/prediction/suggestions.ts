// Defines the Suggestions and its related types.
///<reference path="../../kmwkeyboards.ts" />

// KeymanWeb
// Copyright 2019 SIL International

namespace com.keyman.text.prediction {
  export class Suggestions {
    _Box: HTMLDivElement;
    suggestionsBar: HTMLDivElement;

    ready: boolean = false;

    // Suggestions bar state fields
    _Visible: boolean = false;
    _Enabled: boolean = true;

    // First time initialization of OSK
    prepare() {
      let keymanweb = com.keyman.singleton;
      let util = keymanweb['util'];

      // Defer loading the suggestions until KMW code initialization complete
      if (!keymanweb['initialized']) {
        window.setTimeout(this.prepare.bind(this), 200);
        return;
      }

      // Suggestions initialization - create DIV and set default styles
      if (!this.ready) {
        this._Box = util._CreateElement('div');   // Container for OSK (Help DIV, displayed when user clicks Help icon)
        document.body.appendChild(this._Box);

        // Install the default suggestions stylesheet
        //var suggestionsUI = util.getOption('resources') + 'ui/suggestions/kmwsuggestions.css';
        // util.linkStyleSheet(keymanweb.getStyleSheetPath('kmwsuggestions.css'));
      }
    }

    _Load(){
      this._Visible = false;

    }
  }

}