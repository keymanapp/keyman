type MouseHandler = (this: GlobalEventHandlers, ev: MouseEvent) => any;

/**
 * Represents the current location of the current cursor / touchpoint during
 * an ongoing contact-point event series.  This class standardizes to .pageX
 * (document) coordinates, rather than .clientX (viewport) coordinates.
 */
class InputEventCoordinate {
  public readonly x: number;
  public readonly y: number;

  public constructor(x: number, y: number, source?: MouseEvent | TouchEvent) {
    this.x = x;
    this.y = y;
  }

  // Converts a MouseEvent or TouchEvent into the base coordinates needed
  // by the mouse-dragging operations.
  public static fromEvent(e: MouseEvent | TouchEvent) {
    let coordSource: MouseEvent | Touch;

    // Desktop Safari versions as recent as 14.1 do not support TouchEvents.
    // So, just in case, a two-fold conditional check to avoid issues with a direct
    // 'instanceof' against the type.
    if(window['TouchEvent'] && e instanceof TouchEvent) {
      coordSource = e.changedTouches[0];
    } else if((e as TouchEvent).changedTouches) {
      coordSource = (e as TouchEvent).changedTouches[0] as Touch;
    } else {
      coordSource = e as MouseEvent;
    }

    // For MouseEvents, .pageX is slightly less supported in older browsers when
    // compared to .clientX.  They're about equally supported for TouchEvents.
    if (coordSource.pageX) {
      return new InputEventCoordinate(coordSource.pageX, coordSource.pageY, e);
    } else if (coordSource.clientX) {
      const x = coordSource.clientX + document.body.scrollLeft;
      const y = coordSource.clientY + document.body.scrollTop;

      return new InputEventCoordinate(x, y, e);
    } else {
      return new InputEventCoordinate(null, null, e);
    }
  }
}

/**
 * Used to store the page's original mouse handlers and properties
 * when temporarily overridden by OSK moving or resizing handlers due
 * to user interaction.
 */
class MouseStartSnapshot {
  private readonly _VPreviousMouseMove: MouseHandler;
  private readonly _VPreviousMouseUp: MouseHandler;
  private readonly _VPreviousCursor: string;
  private readonly _VPreviousMouseButton: number;

  constructor(e: MouseEvent) {
    this._VPreviousMouseMove = document.onmousemove;
    this._VPreviousMouseUp = document.onmouseup;

    this._VPreviousCursor = document.body.style.cursor;
    this._VPreviousMouseButton = (typeof(e.which)=='undefined' ? e.button : e.which);
  }

  restore() {
    document.onmousemove = this._VPreviousMouseMove;
    document.onmouseup = this._VPreviousMouseUp;

    if(document.body.style.cursor) {
      document.body.style.cursor = this._VPreviousCursor;
    }
  }

  matchesCausingClick(e: MouseEvent): boolean {
    return this._VPreviousMouseButton == (typeof(e.which)=='undefined' ? e.button : e.which);
  }
}

export default abstract class MouseDragOperation {
  private _enabled: boolean;
  private _startCoord: InputEventCoordinate;
  private _mouseStartSnapshot: MouseStartSnapshot;

  private startHandler: (e: MouseEvent) => void;
  private cursorType: string;

  public constructor(cursorType?: string) {
    this.startHandler = this._VMoveMouseDown.bind(this);
    this.cursorType = cursorType;
  }

  /**
   * Denotes whether or not this object should handle incoming events.
   */
  public get enabled(): boolean {
    return this._enabled;
  }

  public set enabled(flag: boolean) {
    this._enabled = flag;
  }

  /**
   * Denotes whether or not this object is currently handling an ongoing drag event.
   */
  public get isActive(): boolean {
    return !!this._mouseStartSnapshot;
  }

  public get mouseDownHandler(): (e: MouseEvent) => void {
    return this.startHandler;
  }

  /**
   * Function     _VMoveMouseDown
   * Scope        Private
   * @param       {Object}      e      event
   * Description  Process mouse down on OSK
   */
    private _VMoveMouseDown(e: MouseEvent) {
    if(!e) {
      return true;
    }

    if(!this._enabled) {
      return true;
    }

    if(!this._mouseStartSnapshot) { // I1472 - Dragging off edge of browser window causes muckup
      this._mouseStartSnapshot = new MouseStartSnapshot(e);
    }

    this._startCoord = InputEventCoordinate.fromEvent(e);

    document.onmousemove = this._VMoveMouseMove.bind(this);
    document.onmouseup = this._VMoveMouseUp.bind(this);
    if(document.body.style.cursor) {
      document.body.style.cursor = this.cursorType;
    }

    e.preventDefault();
    e.cancelBubble = true;

    this.onDragStart();
    return false;
  }

  protected abstract onDragStart(): void;

  /**
   * Process mouse drag on OSK
   *
   * @param       {Object}      e      event
   */
    private _VMoveMouseMove(e: MouseEvent) {
    if(!e) {
      return true;
    }

    if(!this.enabled) {
      return true;
    }

    e.preventDefault();
    e.cancelBubble = true;

    if(!this._mouseStartSnapshot.matchesCausingClick(e)) { // I1472 - Dragging off edge of browser window causes muckup
      return this._VMoveMouseUp(e);
    } else {
      const coord = InputEventCoordinate.fromEvent(e);
      const deltaX = coord.x - this._startCoord.x;
      const deltaY = coord.y - this._startCoord.y;

      this.onDragMove(deltaX, deltaY);
      return false;
    }
  }

  /**
   *
   * @param deltaX The total horizontal distance moved, in pixels, since the start of the drag
   * @param deltaY The total vertical distance moved, in pixels, since the start of the drag
   */
  protected abstract onDragMove(deltaX: number, deltaY: number): void;

  /**
   * Function     _VMoveMouseUp
   * Scope        Private
   * @param       {Object}      e      event
   * Description  Process mouse up during movement of KMW OSK UI
   */
  private _VMoveMouseUp(e: MouseEvent) {
    if(!e) {
      return true;
    }

    this._mouseStartSnapshot.restore();
    this._mouseStartSnapshot = null;

    e.preventDefault();
    e.cancelBubble = true;

    this.onDragRelease();
    return false;
  }

  protected abstract onDragRelease(): void;
}