namespace com.keyman.osk {
  // Base class for a banner above the keyboard in the OSK

  export abstract class Banner {
    private _Visible: boolean;
    private _Enabled: boolean;
    private _Height: number; // pixels
    div: HTMLDivElement;

    public get height():number {
      return this._Height;
    }

    public set height(height) {
      this._Height = height;
      let ds = this.div.style;
      if (height > 0) {
        ds.height= height + 'px';
      }
    }

    protected constructor(visible: boolean, enabled: boolean) {
      this._Visible = visible;
      this._Enabled = enabled;
      this._Height = 40;

      let keymanweb = com.keyman.singleton;
      let util = keymanweb.util;

      let d = util._CreateElement('div');
      d.id = "keymanweb_banner_bar";
      d.className = "kmw-banner-bar";
      this.div = d;
      this.setVisibility(visible);
    }

    appendStyleSheet() {
      let keymanweb = com.keyman.singleton;
      let util = keymanweb.util;

      // TODO: add stylesheets
    }

    disable() {
      this._Enabled = false;
    }

    enable() {
      this._Enabled = true;
    }



    setVisibility(visible: boolean) {
      this._Visible = visible;

      if (this.div) {
        let ds = this.div.style;
        ds.display=(this._Visible) ? 'block': 'none';
      }
    }
  }

  export class BlankBanner extends Banner {

    constructor() {
      super(false, true);
      this.height = 0;
    }
  }

  /**
   * Function       ImageBanner
   * @param         string        image
   * Description    Display an image in the banner
   */
  export class ImageBanner extends Banner {
    image: string;

    constructor(image) {
      super(true, true);
      this.image = image;
    }
  }

  export class SuggestionBanner extends Banner {
    constructor() {
      super(true, true);
    }
  }

}