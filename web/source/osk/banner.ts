///<reference path="visualKeyboard.ts" />
namespace com.keyman.osk {
  // Base class for a banner above the keyboard in the OSK

  export abstract class Banner {
    private _Visible: boolean;
    private _Enabled: boolean;
    private _Height: number; // pixels
    protected div: HTMLDivElement;

    public DEFAULT_HEIGHT: number = 40; // pixels

    public get height():number {
      return this._Height;
    }

    public set height(height:number) {
      this._Height = height;
      if (this.div) {
        let ds = this.div.style;
        ds.height = (height > 0) ? height + 'px' : '0px';
      }
    }

    public get visible():boolean {
      return this._Visible;
    }

    public set visible(visible: boolean) {
      this._Visible = visible;

      if (this.div) {
        let ds = this.div.style;
        ds.display=(this._Visible) ? 'block': 'none';
      }
    }

    public get enable():boolean {
      return this._Enabled;
    }

    public set enable(enable: boolean) {
      this._Enabled = enable;
    }

    public constructor(visible: boolean, enabled: boolean, height?: number) {
      let keymanweb = com.keyman.singleton;
      let util = keymanweb.util;

      let d = util._CreateElement('div');
      d.id = "keymanweb_banner_bar";
      d.className = "kmw-banner-bar";
      this.div = d;

      this.height = (height >= 0) ? height : this.DEFAULT_HEIGHT;
      this.visible = visible;
      this.enable = enabled;
    }

    public appendStyleSheet() {
      let keymanweb = com.keyman.singleton;
      let util = keymanweb.util;

      // TODO: add stylesheets
    }

    public getDiv():HTMLElement {
      return this.div;
    }
  }

  /**
   * Function       BlankBanner
   * Description    A banner of height 0 that should not be shown
   */
  export class BlankBanner extends Banner {

    constructor() {
      super(false, true, 0);
    }
  }

  /**
   * Function       ImageBanner
   * @param         {string}        imagePath   Path of image to display in the banner
   * Description    Display an image in the banner
   */
  export class ImageBanner extends Banner {
    private img: HTMLElement;

    constructor(imagePath, height?: number) {
      if (imagePath.length > 0) {
        super(true, true);
      } else {
        super(false, true, 0);
      }

      this.img = document.createElement('img');
      this.img.setAttribute('src', imagePath);
      let ds = this.img.style;
      ds.width = '100%';
      ds.height = this.height + 'px';
      if (this.div) {
        this.div.appendChild(this.img);
      }
    }

    /**
     * Function     setImagePath
     * Scope        Public
     * @param       {string}     imagePath   Path of image to display in the banner
     * Description  Update the image in the banner
     */
    public setImagePath(imagePath: string) {
      if (this.img) {
        this.img.setAttribute('src', imagePath);
      }
      if (imagePath.length > 0) {
        this.height = this.DEFAULT_HEIGHT;
        this.visible = true;
        this.enable = true;

        if (this.img) {
          let ds = this.img.style;
          ds.height = this.height + 'px';
        }
      }
    }
  }

  export class OSKSuggestionSpec {
    id: string;
    languageID: string;
    text?: string;
    width: string;
    pad?: string;
    widthpc?: number; // Added during OSK construction.
    padpc?: number; // Added during OSK construction.

    constructor(id: string, languageID: string, text?: string, width?: string, pad?: string) {
      this.id = id;
      this.languageID = languageID;
      this.text = text;
      this.width = width ? width : "50";
      this.pad = pad;
    }
  }

  export class OSKSuggestion {
    spec: OSKSuggestionSpec;

    constructor(spec: OSKSuggestionSpec) {
      this.spec = spec;
    }

    public update(suggestion: Suggestion) {
      this.spec.text = suggestion.displayAs;
    }

    // Produces a HTMLSpanElement with the key's actual text.
    public generateSuggestionText(): HTMLSpanElement {
      let util = (<KeymanBase>window['keyman']).util;
      let spec = this.spec;

      // Add OSK suggestion labels
      var suggestionText: string;
      var t=util._CreateElement('span'), ts=t.style;
      if(spec.text == null || spec.text == '') {
        suggestionText = '\xa0';  // default:  nbsp.
      } else {
        suggestionText = spec.text;
      }

      t.className = 'kmw-suggestion-text';

      if (this.spec.languageID) {
        t.lang = this.spec.languageID;
      }

      //Override font spec if set for this key in the layout
      if(typeof spec['font'] == 'string' && spec['font'] != '') {
        ts.fontFamily=spec['font'];
      }

      if(typeof spec['fontsize'] == 'string' && spec['fontsize'] != 0) {
        ts.fontSize=spec['fontsize'];
      }

      let keyboardManager = (<KeymanBase>window['keyman']).keyboardManager;
      if(keyboardManager.isRTL()) {
        // Add the RTL marker to ensure it displays correctly.
        suggestionText = '\u200f' + suggestionText;
      }

      // Finalize the suggestion text
      t.innerHTML = suggestionText;

      return t;
    }
  }

  /**
   * Function       SuggestionBanner
   * Description    Display lexical model suggestions in the banner
   */
  export class SuggestionBanner extends Banner {
    readonly SUGGESTION_LIMIT:number = 3;
    private suggestionList : OSKSuggestion[];

    constructor() {
      super(true, true);
      let suggestionList:OSKSuggestion[] = new Array();
      this.suggestionList = suggestionList;
      if (this.div) {
        for (var i=0; i<this.SUGGESTION_LIMIT; i++) {
          let s = new OSKSuggestionSpec('suggestion' + i, 'en', '', '33', ' ');
          let d = new OSKSuggestion(s);
          this.suggestionList[i] = d;
          this.div.appendChild(d.generateSuggestionText());
        }
      }
    }

    public static BLANK_SUGGESTION(): Suggestion {
      let s: Suggestion = {
        displayAs: '',
        transform: {
          insert: '', deleteLeft: 0, deleteRight: 0
        }
      };
      return s;
    };

    public invalidateSuggestions() {
      if (this.div) {
        for (var i=0; i<this.SUGGESTION_LIMIT; i++) {
          this.suggestionList[i].spec.text = '';
          this.div.replaceChild(this.suggestionList[i].generateSuggestionText(), this.div.childNodes.item(i));
        }
      }
    }

    public updateSuggestions(suggestions: Suggestion[]) {
      for(var i=0; i<suggestions.length; i++) {
        this.suggestionList[i].update(suggestions[i]);
        this.div.replaceChild(this.suggestionList[i].generateSuggestionText(), this.div.childNodes.item(i));
      }
    }
  }

}