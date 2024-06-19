import { EventEmitter } from 'eventemitter3';

import OSKViewComponent from './oskViewComponent.interface.js';
import { ParsedLengthStyle } from '../lengthStyle.js';
import MouseDragOperation from '../input/mouseDragOperation.js';

import { createUnselectableElement } from 'keyman/engine/dom-utils';

interface EventMap {
  /**
   * Triggered when the user inputs a special command to show the engine's current version number.
   */
  showbuild: () => void;
}

export default class ResizeBar extends EventEmitter<EventMap, ResizeBar> implements OSKViewComponent {
  private _element: HTMLDivElement;
  private _resizeHandle: HTMLDivElement;

  private static readonly DISPLAY_HEIGHT = ParsedLengthStyle.inPixels(16); // As set in kmwosk.css

  private mouseCancellingHandler: (ev: MouseEvent) => boolean = function(ev: MouseEvent) {
    ev.preventDefault();
    ev.cancelBubble = true;
    return false;
  };

  public constructor(dragHandler?: MouseDragOperation) {
    super();
    this._element = this.buildResizeBar();

    if(dragHandler) {
      this._resizeHandle.onmousedown = dragHandler.mouseDownHandler;
    }
  }

  public get layoutHeight(): ParsedLengthStyle {
    return ResizeBar.DISPLAY_HEIGHT;
  }

  public get element(): HTMLDivElement {
    return this._element;
  }

  public get handle(): HTMLDivElement {
    return this._resizeHandle;
  }

  public allowResizing(flag: boolean) {
    this._resizeHandle.style.display = flag ? 'block' : 'none';
  }

  /**
   * Create a bottom bar with a resizing icon for the desktop OSK
   */
  buildResizeBar(): HTMLDivElement {
    var bar = createUnselectableElement('div');
    bar.className='kmw-footer';
    bar.onmousedown = this.mouseCancellingHandler;

    // Add caption
    var Ltitle=createUnselectableElement('div');
    Ltitle.className='kmw-footer-caption';
    Ltitle.innerHTML='<a href="https://keyman.com/developer/keymanweb/">KeymanWeb</a>';
    Ltitle.id='keymanweb-osk-footer-caption';

    // Display build number on shift+double click
    Ltitle.addEventListener('dblclick', (e) => {
      this.emit('showbuild');

      return false;
    }, false);

    bar.appendChild(Ltitle);

    var Limg = createUnselectableElement('div');
    Limg.className='kmw-footer-resize';
    bar.appendChild(Limg);
    this._resizeHandle=Limg;

    return bar;
  }

  public refreshLayout() {
    // The title bar is adaptable as it is and needs no adjustments.
  }
}