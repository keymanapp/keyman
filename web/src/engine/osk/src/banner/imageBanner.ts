import { Banner } from "./banner.js";

/**
 * Function       ImageBanner
 * @param         {string}        imagePath   Path of image to display in the banner
 * @param         {number}        height      If provided, the height of the banner in pixels
 * Description    Display an image in the banner
 */
export class ImageBanner extends Banner {
  private img: HTMLElement;
  readonly type;

  constructor(imagePath: string, height?: number) {
    if (imagePath.length > 0) {
      super();
      if (height) {
        this.height = height;
      }
    } else {
      super(0);
    }

    this.type = 'image';

    if(imagePath.indexOf('base64') >=0) {
      console.log("Loading img from base64 data");
    } else {
      console.log("Loading img with src '" + imagePath + "'");
    }
    this.img = document.createElement('img');
    this.img.setAttribute('src', imagePath);
    let ds = this.img.style;
    ds.height = '100%';
    this.getDiv().appendChild(this.img);
    console.log("Image loaded.");
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
  }
}