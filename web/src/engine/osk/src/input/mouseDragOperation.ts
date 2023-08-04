import InputEventCoordinate from './inputEventCoordinate.js';

type MouseHandler = (this: GlobalEventHandlers, ev: MouseEvent) => any;

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

  protected abstract onDragStart();

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
  protected abstract onDragMove(deltaX: number, deltaY: number);

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

  protected abstract onDragRelease();
}