import { EventEmitter } from 'eventemitter3';

import { Keyboard } from '@keymanapp/keyboard-processor';

import OSKViewComponent from './oskViewComponent.interface.js';
import { ParsedLengthStyle } from '../lengthStyle.js';
import MouseDragOperation from '../input/mouseDragOperation.js';

import { createUnselectableElement } from 'keyman/engine/dom-utils';

interface EventMap {
  /**
   * The close button (to request that the OSK hide) has been clicked.
   */
  close: () => void,

  /**
   * The config button has been clicked.
   */
  config: () => void,

  /**
   * The help button has been clicked.
   */
  help: () => void,

  /**
   * The pin button was visible and has been clicked.
   */
  unpin: () => void
}

export default class TitleBar extends EventEmitter<EventMap, TitleBar> implements OSKViewComponent {
  private _element: HTMLDivElement;
  private _unpinButton: HTMLDivElement;
  private _closeButton: HTMLDivElement;
  private _helpButton: HTMLDivElement;
  private _configButton: HTMLDivElement;
  private _caption: HTMLSpanElement;

  private _helpEnabled:   boolean;
  private _configEnabled: boolean;

  public get helpEnabled(): boolean {
    return this._helpEnabled;
  }

  public set helpEnabled(val) {
    this._helpEnabled = val;

    this._helpButton.style.display = val ? 'inline' : 'none';
  }

  public get configEnabled(): boolean {
    return this._configEnabled;
  }

  public set configEnabled(val) {
    this._configEnabled = val;

    this._configButton.style.display = val ? 'inline' : 'none';
  }

  private static readonly DISPLAY_HEIGHT = ParsedLengthStyle.inPixels(20); // As set in kmwosk.css

  public constructor(dragHandler?: MouseDragOperation) {
    super();

    this._element = this.buildTitleBar();

    this.helpEnabled   = false;
    this.configEnabled = false;

    if(dragHandler) {
      this.element.onmousedown = dragHandler.mouseDownHandler;
    }
  }

  public get layoutHeight(): ParsedLengthStyle {
    return TitleBar.DISPLAY_HEIGHT;
  }

  private mouseCancellingHandler: (ev: MouseEvent) => boolean = function(ev: MouseEvent) {
    ev.preventDefault();
    ev.cancelBubble = true;
    return false;
  };

  public get element(): HTMLDivElement {
    return this._element;
  }

  public setPinCJKOffset() {
    this._unpinButton.style.left = '15px';
  }

  public showPin(visible: boolean) {
    this._unpinButton.style.display = visible ? 'block' : 'none';
  }

  public setTitle(str: string) {
    this._caption.innerHTML = str;
  }

  public setTitleFromKeyboard(keyboard: Keyboard) {
    let title = "<span style='font-weight:bold'>" + keyboard?.name + '</span>';  // I1972  // I2186
    this._caption.innerHTML = title;
  }

  /**
   * Create a control bar with title and buttons for the desktop OSK
   */
  buildTitleBar(): HTMLDivElement {
    let bar = createUnselectableElement('div');
    bar.id='keymanweb_title_bar';
    bar.className='kmw-title-bar';

    var Ltitle = this._caption = createUnselectableElement('span');
    Ltitle.className='kmw-title-bar-caption';
    Ltitle.style.color='#fff';
    bar.appendChild(Ltitle);

    var Limg = this._closeButton = this.buildCloseButton();
    this._closeButton.onclick = () => {
      this.emit('close');
      return false;
    };
    bar.appendChild(Limg);

    Limg = this._helpButton = this.buildHelpButton()
    this._helpButton.onclick = () => {
      this.emit('help');
      return false;
    }
    bar.appendChild(Limg);

    Limg = this._configButton = this.buildConfigButton();
    this._configButton.onclick = () => {
      this.emit('config');
      return false;
    }
    bar.appendChild(Limg);

    Limg = this._unpinButton = this.buildUnpinButton();
    this._unpinButton.onclick = () => {
      this.emit('unpin');
      return false;
    }
    bar.appendChild(Limg);

    return bar;
  }

  private buildCloseButton(): HTMLDivElement {
    var Limg = createUnselectableElement('div');

    Limg.id='kmw-close-button';
    Limg.className='kmw-title-bar-image';
    Limg.onmousedown = this.mouseCancellingHandler;

    return Limg;
  }

  private buildHelpButton(): HTMLDivElement {
    let Limg = createUnselectableElement('div');
    Limg.id='kmw-help-image';
    Limg.className='kmw-title-bar-image';
    Limg.title='KeymanWeb Help';
    Limg.onmousedown = this.mouseCancellingHandler;
    return Limg;
  }

  private buildConfigButton(): HTMLDivElement {
    let Limg = createUnselectableElement('div');

    Limg.id='kmw-config-image';
    Limg.className='kmw-title-bar-image';
    Limg.title='KeymanWeb Configuration Options';
    Limg.onmousedown = this.mouseCancellingHandler;

    return Limg;
  }

  /**
   * Builds an 'unpin' button for restoring OSK to default location, handle mousedown and click events
   */
  private buildUnpinButton(): HTMLDivElement {
    let Limg = createUnselectableElement('div');  //I2186

    Limg.id = 'kmw-pin-image';
    Limg.className = 'kmw-title-bar-image';
    Limg.title='Pin the On Screen Keyboard to its default location on the active text box';

    Limg.onmousedown = this.mouseCancellingHandler;

    return Limg;
  }

  public refreshLayout() {
    // The title bar is adaptable as it is and needs no adjustments.
  }
}