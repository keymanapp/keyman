import { DeviceSpec, ManagedPromise, Version } from '@keymanapp/keyboard-processor';
import { getAbsoluteX, getAbsoluteY, landscapeView } from 'keyman/engine/dom-utils';
import { EmitterListenerSpy } from 'keyman/engine/events';

import OSKView, { EventMap, type LegacyOSKEventMap, OSKPos, OSKRect } from './oskView.js';
import TitleBar from '../components/titleBar.js';
import ResizeBar from '../components/resizeBar.js';

import MouseDragOperation from '../input/mouseDragOperation.js';
import { getViewportScale } from '../screenUtils.js';
import Configuration from '../config/viewConfiguration.js';
import TwoStateActivator from './twoStateActivator.js';
import { FloatingOSKCookie, FloatingOSKCookieSerializer } from './floatingOskCookie.js';

/***
   KeymanWeb 10.0
   Copyright 2017 SIL International
***/

export interface FloatingOSKViewConfiguration extends Configuration {
  activator?: TwoStateActivator<HTMLElement>;
}

export default class FloatingOSKView extends OSKView {
  // OSK positioning fields
  userPositioned: boolean = false;
  specifiedPosition: boolean = false;
  x: number;
  y: number;
  noDrag: boolean = false;
  dfltX: string;
  dfltY: string;

  private layoutSerializer = new FloatingOSKCookieSerializer();

  private titleBar: TitleBar;
  private resizeBar: ResizeBar;

  // Encapsulations of the drag behaviors for OSK movement & resizing
  private _moveHandler: MouseDragOperation;
  private _resizeHandler: MouseDragOperation;

  public constructor(config: FloatingOSKViewConfiguration) {
    config.activator = config.activator || new TwoStateActivator<HTMLElement>();

    super(config);

    this.typedActivationModel.on('triggerchange', () => this.setDisplayPositioning());

    document.body.appendChild(this._Box);

    // Add header element to OSK only for desktop browsers
    this.titleBar = new TitleBar(this.titleDragHandler);
    this.titleBar.on('help', () => {
      this.legacyEvents.callEvent('helpclick', {});
    });
    this.titleBar.on('config', () => {
      this.legacyEvents.callEvent('configclick', {});
    });
    this.titleBar.on('close', () => this.startHide(true));
    this.titleBar.on('unpin', () => this.restorePosition(true));

    this.resizeBar = new ResizeBar(this.resizeDragHandler);
    this.resizeBar.on('showbuild', () => this.emit('showbuild'));

    this.headerView = this.titleBar;
    this._Box.insertBefore(this.headerView.element, this._Box.firstChild);

    const onListenedEvent = (eventName: keyof EventMap | keyof LegacyOSKEventMap) => {
      // As the following title bar buttons (for desktop / FloatingOSKView) do nothing unless a site
      // designer uses these events, we disable / hide them unless an event-handler is attached.
      let titleBar = this.headerView;
      if(titleBar && titleBar instanceof TitleBar) {
        switch(eventName) {
          case 'configclick':
            titleBar.configEnabled = this.legacyEvents.listenerCount('configclick') > 0;
            break;
          case 'helpclick':
            titleBar.helpEnabled = this.legacyEvents.listenerCount('helpclick') > 0;
            break;
          default:
            return;
        }
      }
    }

    const listenerSpyNew = new EmitterListenerSpy(this);
    const listenerSpyOld = new EmitterListenerSpy(this.legacyEvents);
    for(let listenerSpy of [listenerSpyNew, listenerSpyOld]) {
      listenerSpy.on('listeneradded', onListenedEvent);
      listenerSpy.on('listenerremoved', onListenedEvent);
    }

    this.loadPersistedLayout();
  }

  private get typedActivationModel(): TwoStateActivator<HTMLElement> {
    return this.activationModel as TwoStateActivator<HTMLElement>;
  }

  /**
   * Function     _Unload
   * Scope        Private
   * Description  Clears OSK variables prior to exit (JMD 1.9.1 - relocation of local variables 3/9/10)
   */
  _Unload() {
    this.keyboardView = null;
    this.bannerView = null;
    this._Box = null;
  }

  protected setBoxStyling() {
    const s = this._Box.style;

    s.zIndex   = '9999';
    s.display  = 'none';
    s.width    = 'auto';
    s.position = 'absolute';
  }

  protected postKeyboardAdjustments() {
    // Add header element to OSK only for desktop browsers
    this.enableMoveResizeHandlers();
    if(this.activeKeyboard) {
      this.titleBar.setTitleFromKeyboard(this.activeKeyboard.keyboard);
    }

    if(this.vkbd) {
      this.footerView = this.resizeBar;
      this._Box.appendChild(this.footerView.element);
    } else {
      if(this.footerView) {
        this._Box.removeChild(this.footerView.element);
      }
      this.footerView = null;
    }

    this.loadPersistedLayout();
    this.setNeedsLayout();
  }

  /**
   * Function     restorePosition
   * Scope        Public
   * @param       {boolean?}      keepDefaultPosition  If true, does not reset the default x,y set by `setRect`.
   *                                                   If false or omitted, resets the default x,y as well.
   * Description  Move OSK back to default position, floating under active input element
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/osk/restorePosition
   */
  public restorePosition: (keepDefaultPosition?: boolean) => void = function(this: FloatingOSKView, keepDefaultPosition?: boolean) {
    let isVisible = this._Visible;

    let dragPromise = new ManagedPromise<void>();
    this.emit('dragmove', dragPromise.corePromise);

    this.loadPersistedLayout();
    this.userPositioned=false;
    if(!keepDefaultPosition) {
      delete this.dfltX;
      delete this.dfltY;
    }
    this.savePersistedLayout();

    if(isVisible) {
      this.present();
    }

    this.titleBar.showPin(false);
    dragPromise.resolve();
    this.doResizeMove(); //allow the UI to respond to OSK movements
  }.bind(this);

  /**
   * Function     enabled
   * Scope        Public
   * @return      {boolean|number}    True if KMW OSK enabled
   * Description  Test if KMW OSK is enabled
   */
  ['isEnabled'](): boolean {
    return this.displayIfActive;
  }

  /**
   * Function     isVisible
   * Scope        Public
   * @return      {boolean|number}    True if KMW OSK visible
   * Description  Test if KMW OSK is actually visible
   * Note that this will usually return false after any UI event that results in (temporary) loss of input focus
   */
  ['isVisible'](): boolean {
    return this._Visible;
  }

  /**
   * Save size, position, font size and visibility of OSK
   */
  private savePersistedLayout() {
    var p = this.getPos();

    const c: FloatingOSKCookie = {
      visible: this.displayIfActive ? 1 : 0,
      userSet: this.userPositioned ?  1 : 0,
      left: p.left,
      top:  p.top,
      _version: Version.CURRENT.toString()
    }

    if(this.vkbd) {
      c.width =  this.width.val;
      c.height = this.height.val;
    }

    this.layoutSerializer.save(c as Required<FloatingOSKCookie>);
  }

  /**
   * Restore size, position, font size and visibility of desktop OSK
   *
   *  @return {boolean}
   */
  private loadPersistedLayout(): void {
    /*
      If a keyboard is available during OSK construction, it is possible
      for this field to be `undefined`.  `loadPersistedLayout` will be called
      later in construction, so it's safe to skip.
    */
    if(!this.layoutSerializer) {
      return;
    }

    let c = this.layoutSerializer.loadWithDefaults({
      visible: 1,
      userSet: 0,
      left: -1,
      top: -1,
      _version: undefined,
      width:  0.3*screen.width,
      height: 0.15*screen.height
    });

    this.activationModel.enabled = c.visible == 1;
    this.userPositioned = c.userSet == 1;
    this.x = c.left;
    this.y = c.top;
    const cookieVersionString = c._version;

    // Restore OSK size - font size now fixed in relation to OSK height, unless overridden (in em) by keyboard
    const isNewCookie = cookieVersionString === undefined;
    let newWidth  = c.width;
    let newHeight = c.height;

    // Limit the OSK dimensions to reasonable values
    if(newWidth < 0.2*screen.width) {
      newWidth = 0.2*screen.width;
    }
    if(newHeight < 0.1*screen.height) {
      newHeight = 0.1*screen.height;
    }
    if(newWidth > 0.9*screen.width) {
      newWidth=0.9*screen.width;
    }
    if(newHeight > 0.5*screen.height) {
      newHeight=0.5*screen.height;
    }

    // if(!cookieVersionString) - this component was not tracked until 15.0.
    // Before that point, the OSK's title bar and resize bar heights were not included
    // in the OSK's cookie-persisted height.
    if(isNewCookie || !cookieVersionString) {
      // Adds some space to account for the OSK's header and footer, should they exist.
      if(this.headerView && this.headerView.layoutHeight.absolute) {
        newHeight += this.headerView.layoutHeight.val;
      }

      if(this.footerView && this.footerView.layoutHeight.absolute) {
        newHeight += this.footerView.layoutHeight.val;
      }
    }

    this.setSize(newWidth, newHeight);

    // and OSK position if user located
    if(this.x == -1 || this.y == -1 || (!this._Box)) {
      this.userPositioned = false;
    }

    if(this.x < window.pageXOffset-0.8*newWidth) {
      this.x=window.pageXOffset-0.8*newWidth;
    }
    if(this.y < 0) {
      this.x=-1;
      this.y=-1;
      this.userPositioned=false;
    }

    if(this.userPositioned && this._Box) {
      this.setPos({'left': this.x, 'top': this.y});
    }
  }

  /**
   * Get the wanted height of the OSK for touch devices (does not include banner height)
   *  @return   {number}    height in pixels
   **/
  getDefaultKeyboardHeight(): number {
    // KeymanTouch - get OSK height from device
    if(this.configuration.heightOverride) {
      return this.configuration.heightOverride();
    }

    var oskHeightLandscapeView=Math.floor(Math.min(screen.availHeight,screen.availWidth)/2),
        height=oskHeightLandscapeView;

    if(this.targetDevice.formFactor == 'phone') {
      var sx=Math.min(screen.height,screen.width),
          sy=Math.max(screen.height,screen.width);

      if(!landscapeView())
        height=Math.floor(Math.max(screen.availHeight,screen.availWidth)/3);
      else
        height=height*(sy/sx)/1.6;  //adjust for aspect ratio, increase slightly for iPhone 5
    }

    // Correct for viewport scaling (iOS - Android 4.2 does not want this, at least on Galaxy Tab 3))
    if(this.targetDevice.OS == DeviceSpec.OperatingSystem.iOS) {
      height=height/getViewportScale(this.targetDevice.formFactor);
    }

    return height;
  }

  /**
   * Get the wanted width of the OSK for touch devices
   *
   *  @return   {number}    height in pixels
   **/
  getDefaultWidth(): number {
    // KeymanTouch - get OSK height from device
    if(this.configuration.widthOverride) {
      return this.configuration.widthOverride();
    }

    var width: number;
    if(this.targetDevice.OS == DeviceSpec.OperatingSystem.iOS) {
      // iOS does not interchange these values when the orientation changes!
      //width = util.portraitView() ? screen.width : screen.height;
      width = window.innerWidth;
    } else if(this.targetDevice.OS == DeviceSpec.OperatingSystem.Android) {
      try {
        width=document.documentElement.clientWidth;
      } catch(ex) {
        width=screen.availWidth;
      }
    } else {
      width=screen.width;
    }

    return width;
  }

  /**
   * Allow UI to update OSK position and properties
   *
   * @param       {Object=}     p       object with coordinates and userdefined flag
   *
   */
  doResizeMove(p?: any) {
    this.legacyEvents.callEvent('resizemove', p);
  }

  /**
   * Allow the UI or page to set the position and size of the OSK
   * and (optionally) override user repositioning or sizing
   *
   * @param       {Object.<string,number>}   p  Array object with position and size of OSK container
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/osk/setRect
  **/
  public setRect(p: OSKRect) {
    if(this._Box == null || this.targetDevice.formFactor != 'desktop') {
      return;
    }

    var b = this._Box, bs = b.style;
    if('left' in p) {
      this.x = p['left'] - getAbsoluteX(b) + b.offsetLeft;
      bs.left= this.x + 'px';
      this.dfltX=bs.left;
    }

    if('top' in p) {
      this.y = p['top'] - getAbsoluteY(b) + b.offsetTop;
      bs.top = this.y + 'px';
      this.dfltY=bs.top;
    }

    //Do not allow user resizing for non-standard keyboards (e.g. EuroLatin)
    if(this.vkbd != null) {
      var d=this.vkbd.kbdDiv, ds=d.style;

      // Set width, but limit to reasonable value
      if('width' in p) {
        var w=(p['width']-(b.offsetWidth-d.offsetWidth));
        if(w < 0.2*screen.width) {
          w=0.2*screen.width;
        }
        if(w > 0.9*screen.width) {
          w=0.9*screen.width;
        }
        ds.width=w+'px';
        // Use of the `computed` variant is here temporary.
        // Shouldn't use `setSize` for this in the long-term.
        this.setSize(w, this.computedHeight, true);
      }

      // Set height, but limit to reasonable value
      // This sets the default font size for the OSK in px, but that
      // can be modified at the key text level by setting
      // the font size in em in the kmw-key-text class
      if('height' in p) {
        var h=(p['height']-(b.offsetHeight-d.offsetHeight));
        if(h < 0.1*screen.height) {
          h=0.1*screen.height;
        }
        if(h > 0.5*screen.height) {
          h=0.5*screen.height;
        }
        ds.height=h+'px'; ds.fontSize=(h/8)+'px';
        // Use of the `computed` variant is here temporary.
        // Shouldn't use `setSize` for this in the long-term.
        this.setSize(this.computedWidth, h, true);
      }

      // Fix or release user resizing
      if('nosize' in p) {
        this.resizingEnabled = !p['nosize'];
      }

    }
    // Fix or release user dragging
    if('nomove' in p) {
      this.noDrag=p['nomove'];
      this.movementEnabled = !this.noDrag;
    }
    // Save the user-defined OSK size
    this.savePersistedLayout();
  }

  /**
   * Get position of OSK window
   *
   * @return      {Object.<string,number>}     Array object with OSK window position
  **/
  getPos(): OSKPos {
    var Lkbd=this._Box, p={
      left: this._Visible ? Lkbd.offsetLeft : this.x,
      top: this._Visible ? Lkbd.offsetTop : this.y
    };

    return p;
  }

  /**
   * Function     setPos
   * Scope        Private
   * @param       {Object.<string,number>}    p     Array object with OSK left, top
   * Description  Set position of OSK window, but limit to screen
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/osk/setPos
   */
  public setPos(p: OSKPos) {
    if(typeof(this._Box) == 'undefined') {
      return; // I3363 (Build 301)
    }

    if(this.userPositioned) {
      var Px=p['left'], Py=p['top'];

      if(typeof(Px) != 'undefined') {
        if(Px < -0.8*this._Box.offsetWidth) {
          Px = -0.8*this._Box.offsetWidth;
        }
        if(this.userPositioned) {
          this._Box.style.left=Px+'px';
          this.x = Px;
        }
      }
      // May not be needed - vertical positioning is handled differently and defaults to input field if off screen
      if(typeof(Py) != 'undefined') {
        if(Py < 0) {
          Py = 0;
        }

        if(this.userPositioned) {
          this._Box.style.top=Py+'px';
          this.y = Py;
        }
      }
    }

    this.titleBar.showPin(this.userPositioned);
  }

  public setDisplayPositioning() {
    var Ls = this._Box.style;

    Ls.position='absolute';
    // Keep it hidden if not currently displayed.
    if(this.activationModel.activate) {
      Ls.display='block'; //Ls.visibility='visible';
    }
    Ls.left='0px';
    if(this.specifiedPosition || this.userPositioned) {
      Ls.left = this.x+'px';
      Ls.top  = this.y+'px';
    } else {
      let el: HTMLElement = this.typedActivationModel.activationTrigger || null;

      if(this.dfltX) {
        Ls.left=this.dfltX;
      } else if(typeof el != 'undefined' && el != null) {
        Ls.left=getAbsoluteX(el) + 'px';
      }

      if(this.dfltY) {
        Ls.top=this.dfltY;
      } else if(typeof el != 'undefined' && el != null) {
        Ls.top=(getAbsoluteY(el) + el.offsetHeight)+'px';
      }
    }

    // Unset the flag, keeping 'specified position' specific to single
    // presentAtPosition calls.
    this.specifiedPosition = false;
  }

  /**
   * Display KMW OSK at specified position (returns nothing)
   *
   * @param       {number=}     Px      x-coordinate for OSK rectangle
   * @param       {number=}     Py      y-coordinate for OSK rectangle
   */
  presentAtPosition(Px?: number, Py?: number) {
    if(!this.mayShow()) {
      return;
    }

    this.specifiedPosition = Px >= 0 || Py >= 0; //probably never happens, legacy support only
    if(this.specifiedPosition) {
      this.x = Px;
      this.y = Py;
    }

    // Combines the two paths with set positioning.
    this.specifiedPosition = this.specifiedPosition || this.userPositioned;

    this.present();
  }

  present() {
    if(!this.mayShow()) {
      return;
    }

    this.titleBar.showPin(this.userPositioned);

    super.present();

    // Allow desktop UI to execute code when showing the OSK
    this.doShow({
      x: this._Box.offsetLeft,
      y: this._Box.offsetTop,
      userLocated: this.userPositioned
    });
  }

  public startHide(hiddenByUser: boolean) {
    super.startHide(hiddenByUser);

    if(hiddenByUser) {
      this.savePersistedLayout();  // Save current OSK state, size and position (desktop only)
    }
  }

  ['show'](bShow?: boolean) {
    if(bShow !== undefined) {
      super['show'](bShow);
    } else {
      super['show']();
    }
    this.savePersistedLayout();
  }

  /**
   * Function     userPositioned
   * Scope        Public
   * @return      {(boolean|number)}          true if user located
   * Description  Test if OSK window has been repositioned by user
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/osk/userLocated
   */
  public userLocated() {
    return this.userPositioned;
  }

  public get movementEnabled(): boolean {
    return this.titleDragHandler.enabled;
  }

  public set movementEnabled(flag: boolean) {
    this.titleDragHandler.enabled = flag;
    this.titleBar.showPin(flag && this.userPositioned);
  }

  public get resizingEnabled(): boolean {
    return this.resizeDragHandler.enabled;
  }

  public set resizingEnabled(flag: boolean) {
    this.resizeDragHandler.enabled = flag;
    this.resizeBar.allowResizing(flag);
  }

  public get isBeingMoved(): boolean {
    return this.titleDragHandler.isActive;
  }

  public get isBeingResized(): boolean {
    return this.resizeDragHandler.isActive;
  }

  private enableMoveResizeHandlers() {
    this.titleDragHandler.enabled = !this.noDrag;
    this.resizeDragHandler.enabled = true; // by default.
  }

  private get titleDragHandler(): MouseDragOperation {
    const _this = this;

    if(this._moveHandler) {
      return this._moveHandler;
    }

    this._moveHandler = new class extends MouseDragOperation {
      startX: number;
      startY: number;

      dragPromise: ManagedPromise<void>;

      constructor() {
        super('move'); // The type of cursor to use while 'active'.
      }

      onDragStart() {
        this.startX = _this._Box.offsetLeft;
        this.startY = _this._Box.offsetTop;

        if(_this.activeKeyboard.keyboard.isCJK) {
          _this.titleBar.setPinCJKOffset();
        }

        if(this.dragPromise) {
          // We got interrupted during the previous one; allow it to reset, at least!
          this.dragPromise.resolve();
        }

        this.dragPromise = new ManagedPromise<void>();
        _this.emit('dragmove', this.dragPromise.corePromise);
      }

      onDragMove(cumulativeX: number, cumulativeY: number) {
        _this.titleBar.showPin(true);
        _this.userPositioned = true;

        _this._Box.style.left = (this.startX + cumulativeX) + 'px';
        _this._Box.style.top  = (this.startY + cumulativeY) + 'px';

        var r=_this.getRect();
        _this.setSize(r.width, r.height, true);
        _this.x = r.left;
        _this.y = r.top;
      }

      onDragRelease() {
        if(_this.vkbd) {
          _this.vkbd.currentKey=null;
        }

        this.dragPromise.resolve();

        // Remainder should be done after anything else pending on the Promise.
        this.dragPromise.then(() => {
          _this.userPositioned = true;
          _this.doResizeMove();
          _this.savePersistedLayout();
        });
        this.dragPromise = null;
      }
    }

    return this._moveHandler;
  }

  private get resizeDragHandler(): MouseDragOperation {
    const _this = this;

    if(this._resizeHandler) {
      return this._resizeHandler;
    }

    this._resizeHandler = new class extends MouseDragOperation {
      startWidth: number;
      startHeight: number;

      dragPromise: ManagedPromise<void>;

      constructor() {
        super('se-resize'); // The type of cursor to use while 'active'.
      }

      onDragStart() {
        this.startWidth = _this.computedWidth;
        this.startHeight = _this.computedHeight;

        if(this.dragPromise) {
          // We got interrupted during the previous one; allow it to reset, at least!
          this.dragPromise.resolve();
        }

        this.dragPromise = new ManagedPromise<void>();
        _this.emit('resizemove', this.dragPromise.corePromise);
      }

      onDragMove(cumulativeX: number, cumulativeY: number) {
        let newWidth  = this.startWidth  + cumulativeX;
        let newHeight = this.startHeight + cumulativeY;

        // Set the smallest and largest OSK size
        if(newWidth < 0.2*screen.width) {
          newWidth = 0.2*screen.width;
        }
        if(newHeight < 0.1*screen.height) {
          newHeight = 0.1*screen.height;
        }
        if(newWidth > 0.9*screen.width) {
          newWidth = 0.9*screen.width;
        }
        if(newHeight > 0.5*screen.height) {
          newHeight = 0.5*screen.height;
        }

        // Explicitly set OSK width, height,  and font size - cannot safely rely on scaling from font
        _this.setSize(newWidth, newHeight, true);
      }

      onDragRelease() {
        if(_this.vkbd) {
          _this.vkbd.currentKey=null;
        }

        if(_this.vkbd) {
          this.startWidth  = _this.computedWidth;
          this.startHeight = _this.computedHeight;
        }

        _this.refreshLayout(); // Finalize the resize.

        this.dragPromise.resolve();

        // Remainder should be done after anything else pending on the Promise.
        this.dragPromise.then(() => {
          _this.doResizeMove();
          _this.savePersistedLayout();
        });
        this.dragPromise = null;
      }
    }

    return this._resizeHandler;
  }
}
