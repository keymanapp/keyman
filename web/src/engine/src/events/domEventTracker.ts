/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

class DomEventTracking {
  elem: EventTarget;
  eventName: string;
  handler: (arg0: Object) => boolean;
  useCapture?: boolean

  constructor(elem: EventTarget, eventName: string, handler: (arg0: Object) => boolean, useCapture?: boolean) {
    this.elem = elem;
    this.eventName = eventName.toLowerCase();
    this.handler = handler;
    this.useCapture = useCapture;
  }

  equals(other: DomEventTracking): boolean {
    return this.elem == other.elem && this.eventName == other.eventName &&
      this.handler == other.handler && this.useCapture == other.useCapture;
  }
}

/**
 * Facilitates adding and removing event listeners to and from DOM elements in a manner
 * that allows widespread removal/cleanup of the listeners at a future time if and when needed.
 *
 * Said "widespread removal" helps to prevent separate instances of KeymanWeb from stomping on
 * each other during unit tests.
 */
export class DomEventTracker {
  private domEvents: DomEventTracking[] = [];

  /**
   * Attaches event handler to element DOM event
   *
   * Note: for most browsers, adds an event to a chain, doesn't stop existing events
   *
   * @param  elem        Element (or IFrame-internal Document) to which
   *                     event is being attached
   * @param  eventName   Name of event without 'on' prefix
   * @param  handler     Event handler for event
   * @param  useCapture  True only if event to be handled on way to target element
   */
  public attachDOMEvent<K extends keyof WindowEventMap>(
    elem: Window,
    eventName: K,
    handler: (ev: WindowEventMap[K]) => any,
    useCapture?: boolean
  ): void;
  public attachDOMEvent<K extends keyof DocumentEventMap>(
    elem: Document,
    eventName: K,
    handler: (ev: DocumentEventMap[K]) => any,
    useCapture?: boolean
  ): void;
  public attachDOMEvent<K extends keyof HTMLElementEventMap>(
    elem: HTMLElement,
    eventName: K,
    handler: (ev: HTMLElementEventMap[K]) => any,
    useCapture?: boolean
  ): void;
  public attachDOMEvent(elem: EventTarget, eventName: string, handler: (arg0: Object) => boolean, useCapture?: boolean): void {
    // @ts-ignore // Since the trickery unfortunately don't also clear things up for anything we call within.
    // It's possible to fix, but that gets way more complex to spec out completely.
    this.detachDOMEvent(elem, eventName, handler, useCapture);
    elem.addEventListener(eventName, handler, !!useCapture);

    // Since we're attaching to the DOM, these events should be tracked for detachment during shutdown.
    const event = new DomEventTracking(elem, eventName, handler, useCapture);
    this.domEvents.push(event);
  }

  /**
   * Detaches event handler from element [to prevent memory leaks]
   *
   * @param  elem        Element from which event is being detached
   * @param  eventName   Name of event without 'on' prefix
   * @param  handler     Event handler for event
   * @param  useCapture  True if event was being handled on way to target element
   */
  public detachDOMEvent<K extends keyof WindowEventMap>(
    elem: Window,
    eventName: K,
    handler: (ev: WindowEventMap[K]) => any,
    useCapture?: boolean
  ): void;
  public detachDOMEvent<K extends keyof DocumentEventMap>(
    elem: Document,
    eventName: K,
    handler: (ev: DocumentEventMap[K]) => any,
    useCapture?: boolean
  ): void;
  public detachDOMEvent<K extends keyof HTMLElementEventMap>(
    elem: HTMLElement,
    eventName: K,
    handler: (ev: HTMLElementEventMap[K]) => any,
    useCapture?: boolean
  ): void;
  public detachDOMEvent(elem: EventTarget, eventName: string, handler: (arg0: Object) => boolean, useCapture?: boolean): void {
    elem.removeEventListener(eventName, handler, useCapture);

    // Since we're detaching, we should drop the tracking data from the old event.
    const event = new DomEventTracking(elem, eventName, handler, useCapture);
    for(let i = 0; i < this.domEvents.length; i++) {
      if(this.domEvents[i].equals(event)) {
        this.domEvents.splice(i, 1);
        break;
      }
    }
  }

  /**
   * Remove all events linking to elements of the original, unaltered page.
   * This should sever any still-existing page ties to this instance of KMW,
   * allowing browser GC to do its thing.
   */
  public shutdown() {
    for(const event of this.domEvents) {
      // @ts-ignore // since it's simpler this way and doesn't earn us much to re-check types.
      this.detachDOMEvent(event.elem, event.eventName, event.handler, event.useCapture);
    }
  }
}
