namespace com.keyman.osk {
  // Base class for a banner above the keyboard in the OSK

  export abstract class Banner {
    _Visible: boolean = false;
    _Enabled: boolean = true;
    div: HTMLDivElement;

    protected constructor(visible: boolean, enabled: boolean) {
      this._Visible = visible;
      this._Enabled = enabled;

      let keymanweb = com.keyman.singleton;
      let util = keymanweb.util;

      this.div = util._CreateElement('div');
    }
  }

  export class BlankBanner extends Banner {

    constructor() {
      super(false, true);
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