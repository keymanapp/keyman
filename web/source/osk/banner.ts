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

  /**
   * Function       SuggestionBanner
   * Description    Display lexical model suggestions in the banner
   */
  export class SuggestionBanner extends Banner {
    constructor() {
      super(true, true);
    }
  }

}