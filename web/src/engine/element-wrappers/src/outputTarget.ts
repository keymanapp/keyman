import { OutputTarget as OutputTargetBase } from "@keymanapp/keyboard-processor";
import EventEmitter from 'eventemitter3';

export default abstract class OutputTarget<EventMap extends EventEmitter.ValidEventTypes> extends OutputTargetBase {
  // JS/TS can't do true multiple inheritance, so we maintain class events on a readonly field.
  public readonly events: EventEmitter<EventMap, this> = new EventEmitter<EventMap, this>();

  /**
   * A field that may be used to track whether or not the represented context has changed over an
   * arbitrary period of time.
   */
  public changed = false;

  /**
   * Returns the underlying element / document modeled by the wrapper.
   */
  abstract getElement(): HTMLElement;

  public focus(): void {
    const ele = this.getElement();
    if(ele.focus) {
      ele.focus();
    }
  }

  /**
   * A helper method for doInputEvent; creates a simple common event and default dispatching.
   * @param elem
   */
  protected dispatchInputEventOn(elem: HTMLElement) {
    let event: InputEvent;

    // `undefined` in pre-Chrome Edge and Chrome for Android before version 60.
    if(window['InputEvent']) { // can't condition on the type directly; TS optimizes that out.
      event = new InputEvent('input', {"bubbles": true, "cancelable": false});
    }

    if(elem && event) {
      elem.dispatchEvent(event);
    }
  }
}