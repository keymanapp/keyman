import EventEmitter, { EventNames, EventListener } from "eventemitter3";
import { LegacyEventEmitter } from "./legacyEventEmitter.js";

interface EventMap<BaseEventMap extends Object> {
  // Provides IntelliSense suggestions in conditionals based on the parameters!

  /**
   * Indicates that a listener for the named event has been registered for the
   * EventEmitter being spied upon.
   * @param eventName
   */
  listenerAdded(eventName: EventNames<BaseEventMap>);

  /**
   * Indicates that a listener for the named event has been unregistered from the
   * EventEmitter being spied upon.
   * @param eventName
   */
  listenerRemoved(eventName: EventNames<BaseEventMap>);
}

type Emitter<BaseEventMap extends Object> = EventEmitter<BaseEventMap> | LegacyEventEmitter<BaseEventMap>;

/**
 * A spy-object that wraps event-emitters in order to listen in on listener addition methods and
 * raise events when new listeners are attached.
 */
export class EmitterListenerSpy<BaseEventMap extends Object> extends EventEmitter<EventMap<BaseEventMap>> {
  constructor(emitter: Emitter<BaseEventMap>) {
    super();

    if(emitter instanceof EventEmitter) {
      emitter.on = this.addListenerAttachmentExtension('listenerAdded', emitter, emitter.on);
      emitter.addListener = this.addListenerAttachmentExtension('listenerAdded', emitter, emitter.addListener);
      emitter.off = this.addListenerAttachmentExtension('listenerRemoved', emitter, emitter.off);
      emitter.removeListener = this.addListenerAttachmentExtension('listenerRemoved', emitter, emitter.off);
    } else {
      // TS gets really fussy about how the legacy event typing is a bit more
      // restrictive (due to less-restricted event name types in EventEmitter)
      // It's not worth the effort to make this 100% perfect at the moment.
      //
      // @ts-ignore
      emitter.addEventListener = this.addListenerAttachmentExtension('listenerAdded', emitter, emitter.addEventListener);
      // @ts-ignore
      emitter.removeEventListener = this.addListenerAttachmentExtension('listenerRemoved', emitter, emitter.removeEventListener);
    }
  }

  // Refer to https://stackoverflow.com/a/10057969.
  private addListenerAttachmentExtension(
    spyEventName: EventNames<EventMap<BaseEventMap>>,
    emitter: Emitter<BaseEventMap>,
    method: (
      eventName: EventNames<BaseEventMap>,
      listener: EventListener<BaseEventMap, EventNames<BaseEventMap>>,
    ) => any
  ): (
    eventName: EventNames<BaseEventMap>,
    listener: EventListener<BaseEventMap, EventNames<BaseEventMap>>,
  ) => any {
    return (eventName, listener) => {
      const retVal = method.apply(emitter, [eventName, listener]);
      this.emit(spyEventName, eventName);
      return retVal;
    }
  }
}

/**** A code block for verifying that typing, etc checks out: ****/

// interface TestMap {
//   'a': (str: string) => void;
//   'b': (num: number, str: string) => void;
// }

// const emitter = new LegacyEventEmitter<TestMap>;  // or `new EventEmitter<TestMap>`.
// const emitterSpy = new EmitterListenerSpy<TestMap>(emitter);
// emitterSpy.on('listenerAdded', (eventName) => {
//   // eventName = 'c'; // will error; there is no event 'c' in the event map.
//   if(eventName == 'a') {
//     // stuff
//   }
// })