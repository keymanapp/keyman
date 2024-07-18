class DomEventTracking {
  Pelem: EventTarget;
  Peventname: string;
  Phandler: (arg0: Object) => boolean;
  PuseCapture?: boolean

  constructor(Pelem: EventTarget, Peventname: string, Phandler: (arg0: Object) => boolean, PuseCapture?: boolean) {
    this.Pelem = Pelem;
    this.Peventname = Peventname.toLowerCase();
    this.Phandler = Phandler;
    this.PuseCapture = PuseCapture;
  }

  equals(other: DomEventTracking): boolean {
    return this.Pelem == other.Pelem && this.Peventname == other.Peventname &&
      this.Phandler == other.Phandler && this.PuseCapture == other.PuseCapture;
  }
};

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
   * Function     attachDOMEvent: Note for most browsers, adds an event to a chain, doesn't stop existing events
   * Scope        Public
   * @param       {Object}    Pelem       Element (or IFrame-internal Document) to which event is being attached
   * @param       {string}    Peventname  Name of event without 'on' prefix
   * @param       {function(Object)}  Phandler    Event handler for event
   * @param       {boolean=}  PuseCapture True only if event to be handled on way to target element
   * Description  Attaches event handler to element DOM event
   */
  attachDOMEvent<K extends keyof WindowEventMap>(
    Pelem: Window,
    Peventname: K,
    Phandler: (ev: WindowEventMap[K]) => any,
    PuseCapture?: boolean
  ): void;
  attachDOMEvent<K extends keyof DocumentEventMap>(
    Pelem: Document,
    Peventname: K,
    Phandler: (ev: DocumentEventMap[K]) => any,
    PuseCapture?: boolean
  ): void;
  attachDOMEvent<K extends keyof HTMLElementEventMap>(
    Pelem: HTMLElement,
    Peventname: K,
    Phandler: (ev: HTMLElementEventMap[K]) => any,
    PuseCapture?: boolean
  ): void;
  attachDOMEvent(Pelem: EventTarget, Peventname: string, Phandler: (arg0: Object) => boolean, PuseCapture?: boolean): void {
    // @ts-ignore // Since the trickery unfortunately don't also clear things up for anything we call within.
    // It's possible to fix, but that gets way more complex to spec out completely.
    this.detachDOMEvent(Pelem, Peventname, Phandler, PuseCapture);
    Pelem.addEventListener(Peventname, Phandler, PuseCapture ? true : false);

    // Since we're attaching to the DOM, these events should be tracked for detachment during shutdown.
    var event = new DomEventTracking(Pelem, Peventname, Phandler, PuseCapture);
    this.domEvents.push(event);
  }

  /**
   * Function     detachDOMEvent
   * Scope        Public
   * @param       {Object}    Pelem       Element from which event is being detached
   * @param       {string}    Peventname  Name of event without 'on' prefix
   * @param       {function(Object)}  Phandler    Event handler for event
   * @param       {boolean=}  PuseCapture True if event was being handled on way to target element
   * Description Detaches event handler from element [to prevent memory leaks]
   */
  detachDOMEvent<K extends keyof WindowEventMap>(
    Pelem: Window,
    Peventname: K,
    Phandler: (ev: WindowEventMap[K]) => any,
    PuseCapture?: boolean
  ): void;
  detachDOMEvent<K extends keyof DocumentEventMap>(
    Pelem: Document,
    Peventname: K,
    Phandler: (ev: DocumentEventMap[K]) => any,
    PuseCapture?: boolean
  ): void;
  detachDOMEvent<K extends keyof HTMLElementEventMap>(
    Pelem: HTMLElement,
    Peventname: K,
    Phandler: (ev: HTMLElementEventMap[K]) => any,
    PuseCapture?: boolean
  ): void;
  detachDOMEvent(Pelem: EventTarget, Peventname: string, Phandler: (arg0: Object) => boolean, PuseCapture?: boolean): void {
    Pelem.removeEventListener(Peventname, Phandler, PuseCapture);

    // Since we're detaching, we should drop the tracking data from the old event.
    var event = new DomEventTracking(Pelem, Peventname, Phandler, PuseCapture);
    for(var i = 0; i < this.domEvents.length; i++) {
      if(this.domEvents[i].equals(event)) {
        this.domEvents.splice(i, 1);
        break;
      }
    }
  }

  shutdown() {
    // Remove all events linking to elements of the original, unaltered page.
    // This should sever any still-existing page ties to this instance of KMW,
    // allowing browser GC to do its thing.
    for(let event of this.domEvents) {
      // @ts-ignore // since it's simpler this way and doesn't earn us much to re-check types.
      this.detachDOMEvent(event.Pelem, event.Peventname, event.Phandler, event.PuseCapture);
    }
  }
}