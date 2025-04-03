import { JSKeyboard, KeyboardProperties } from 'keyman/engine/keyboard';
import { createUnselectableElement } from 'keyman/engine/dom-utils';

// Base class for a banner above the keyboard in the OSK

export abstract class Banner {
  private _height: number; // pixels
  private _width: number; // pixels
  private div: HTMLDivElement;

  public static DEFAULT_HEIGHT: number = 37; // pixels; embedded apps can modify

  public static readonly BANNER_CLASS: string = 'kmw-banner-bar';
  public static readonly BANNER_ID: string = 'kmw-banner-bar';

  /**
   * Function     height
   * Scope        Public
   * @returns     {number} height in pixels
   * Description  Returns the height of the banner in pixels
   */
  public get height(): number {
    return this._height;
  }

  /**
   * Function     height
   * Scope        Public
   * @param       {number} height   the height in pixels
   * Description  Sets the height of the banner in pixels. If a negative
   *              height is given, set height to 0 pixels.
   *              Also updates the banner styling.
   */
  public set height(height: number) {
    this._height = (height > 0) ?  height : 0;
    this.update();
  }

  public get width(): number {
    return this._width;
  }

  public set width(width: number) {
    this._width = width;
    this.update();
  }

  /**
   * Function      update
   * @return       {boolean}   true if the banner styling changed
   * Description   Update the height and display styling of the banner
   */
  protected update() : boolean {
    let ds = this.div.style;
    let currentHeightStyle = ds.height;
    let currentDisplayStyle = ds.display;

    if (this._height > 0) {
      ds.height = this._height + 'px';
      ds.display = 'block';
    } else {
      ds.height = '0px';
      ds.display = 'none';
    }

    return (!(currentHeightStyle === ds.height) ||
      !(currentDisplayStyle === ds.display));
  }

  public constructor(height?: number) {
    let d = createUnselectableElement('div');
    d.id = Banner.BANNER_ID;
    d.className = Banner.BANNER_CLASS;
    this.div = d;

    this.height = height;
    this.update();
  }

  public appendStyleSheet() {
    // TODO: add stylesheets
    // See VisualKeyboard's method + 'addFontStyle' for current handling.
  }

  /**
   * Function     getDiv
   * Scope        Public
   * @returns     {HTMLElement} Base element of the banner
   * Description  Returns the HTMLElement of the banner
   */
  public getDiv(): HTMLElement {
    return this.div;
  }

  /**
   * Allows banners to adapt based on the active keyboard and related properties, such as
   * associated fonts.
   * @param keyboard
   * @param keyboardProperties
   */
  public configureForKeyboard(keyboard: JSKeyboard, keyboardProperties: KeyboardProperties) { }

  public readonly refreshLayout?: () => void;

  abstract get type(): string;

  public shutdown() { };
}
