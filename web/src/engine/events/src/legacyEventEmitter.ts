// Most of the typing below is derived from that of EventEmitter, but customized for
// modeling legacy KMW events.  Including the heavy typing gets us event Intellisense
// and compile-time errors if and when types don't match expectations.

/**
 * Can define the set of events as follows:
 * ```
 * interface Test extends EventMap {
 *   'event': (param: {'prop': any}) => boolean;
 * }
 * ```
 *
 * Each event may have either no parameters or a single parameter of type object.
 * The type definition of the parameter will be utilized by TS's type-inference engine
 * for type checking on handlers and for raising the event.
 *
 * Note: the `extends EventMap` part is actually important for TS type inference here.
 */
export type LegacyEventMap = object;

/**
 * Matches the name of any single event defined within the specified event-map definition.
 */
export type EventNames<T extends LegacyEventMap> = Exclude<keyof T, number | symbol>;

/**
 * Builds a type-array of the arguments for each named event, indexed by that name.
 */
type ArgumentMap<T extends LegacyEventMap> = {
  [K in Exclude<keyof T, number | symbol>]: T[K] extends (arg: any) => void
    ? Parameters<T[K]>[0]
    : (
      T[K] extends Function
      ? never
      : T[K]
    );
};

/**
 * Provides the type signature of event listeners able to handle defined events.
 */
export type EventListener<
    T extends LegacyEventMap,
    K extends EventNames<T>
  > = (       // argumentMap[eventName] - retrieves the specific parameter typing for the event.
        args: ArgumentMap<T>[Extract<K, keyof T>]
      ) => any;

/**
 * Provides fairly strong typing for all legacy KMW events.  Note that all events
 * assume a handler receiving up to one object, though that object's properties will
 * vary from event to event.
 *
 * Note that the behavior differs from EventEmitter events on a few points:
 * 1. Event functions are expected to return a boolean value - generally, `true`.
 *    If 'false' or `undefined` is returned, no further listeners will receive the event.
 * 2. These events receive up to one parameter, always of an object type.
 * 3. These events proactively prevent accidental event-handler recursion.  Should an event's
 *    handler retrigger the event, the newly-triggered event will be prevented entirely.
 */
export class LegacyEventEmitter<EventTypes extends LegacyEventMap> {
  // An object mapping event names to individual event lists.  Maps strings to arrays.
  private events: { [name: string]: ((arg0: Object) => boolean)[];} = {};
  private currentEvents: string[] = [];  // The event messaging call stack.

  /**
   * Function    addEventListener
   * Scope       Private
   * @param      {string}     event     name of event prefixed by module, e.g. osk.touchmove
   * @param      {function(Object)}   func      event handler
   * @return     {boolean}
   * Description Add (or replace) an event listener for this component
   */
  addEventListener<T extends EventNames<EventTypes>> (
    event: T,
    func: EventListener<EventTypes, T>
  ): boolean {
    this._removeEventListener(event, func);
    // TS gets hung up on the type info here because we can potentially store
    // different types of listeners for different events.
    this.events[event].push(func as unknown as any);
    return true;
  }

  /**
   * Function    removeEventListener
   * Scope       Private
   * @param      {string}     event     name of event prefixed by module, e.g. osk.touchmove
   * @param      {function(Object)}   func      event handler
   * @return     {boolean}
   * Description Remove the specified function from the listeners for this event
   */
  public removeEventListener<T extends EventNames<EventTypes>> (
    event: T,
    func: EventListener<EventTypes, T>
  ): boolean {
    return this._removeEventListener(event, func);
  }

  // Separate, in order to prevent `addEventListener` from sending 'listenerremoved' events with
  // EmitterListenerSpy.
  private _removeEventListener<T extends EventNames<EventTypes>> (
    event: T,
    func: EventListener<EventTypes, T>
  ): boolean {
    if(typeof this.events[event] == 'undefined') {
      this.events[event] = [];
    }

    for(var i=0; i<this.events[event].length; i++) {
      if(this.events[event][i] == func) {
        this.events[event].splice(i, 1);
        return true;
      }
    }
    return false;
  }

  /**
   * Function    callEvent
   * Scope       Private
   * @param      {string}     event     name of event prefixed by module, e.g. osk.touchmove
   * @param      {Array}      params    parameter array for function
   * @return     {boolean}    `true` only if all handlers return `true`, permitting further execution.
   * Description Invoke an event using any function
   */
  callEvent<T extends EventNames<EventTypes>> (
    event: T,
    params: ArgumentMap<EventTypes>[T]
  ): boolean {
    if(typeof this.events[event] == 'undefined') {
      return true;
    }

    if(this.currentEvents.indexOf(event) != -1) {
      return false;  // Avoid event messaging recursion!
    }

    this.currentEvents.push(event);

    for(var i=0; i<this.events[event].length; i++) {
      var func=this.events[event][i] as EventListener<EventTypes, T>, result=false;
      try {
        result=func(params as any);
      } catch(strExcept) {
        console.error(strExcept);
        result=false;
      } //don't know whether to use true or false here
      if(result === false) {
        this.currentEvents.pop();
        return false;
      }
    }
    this.currentEvents.pop();
    return true;
  }

  listenerCount<T extends EventNames<EventTypes>>(event: T) {
    const listeners = this.events[event];
    return listeners ? listeners.length : 0;
  }

  shutdown() {
    // Remove all event-handler references rooted in KMW events.
    this.events = {};
  }
}
