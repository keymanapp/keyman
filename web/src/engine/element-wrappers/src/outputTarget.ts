import { OutputTarget as OutputTargetBase } from "@keymanapp/keyboard-processor";
import EventEmitter from 'eventemitter3';

export interface BaseEventMap {
  /**
   * Meant to facilitate the following code that existed pre-modularization:
   ```
    // This class has non-integrated unit tests in which the `singleton` object doesn't exist.
    // Thus, we need to test for this case.
    let keyman = com.keyman['singleton'];

    // Signal the necessary text changes to the embedding app, if it exists.
    if(keyman && keyman['oninserttext'] && keyman.isEmbedded) {
      keyman['oninserttext'](transform.deleteLeft, transform.insert, transform.deleteRight);
    }
   ```
   */
  'oninserttext': (deleteLeft: number, insert: string, deleteRight: number) => void;
}

export default abstract class OutputTarget<EventMap extends BaseEventMap = BaseEventMap> extends OutputTargetBase {
  // JS/TS can't do multiple inheritance, so we maintain class events on a readonly field.
  public readonly events: EventEmitter<EventMap, this> = new EventEmitter<EventMap, this>();

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

  apply(transform: Transform) {
    super.apply(transform);

    // The TS compiler can't quite handle this typing scenario properly; the following cast
    // allows us to work around its type inference limitations.
    const baseEvents = (this.events as unknown as EventEmitter<BaseEventMap, this>);
    baseEvents.emit('oninserttext', transform.deleteLeft, transform.insert, transform.deleteRight);
  }
}