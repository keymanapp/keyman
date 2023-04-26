import { DomEventTracker } from 'keyman/engine/events';

import { KeymanEngine } from "../keymanEngine.js";

// Note:  in the future, it'd probably be best to have an instance per iframe window as
// well as the top-level window.  This was not done in or before KMW 16.0 though, so
// we'll leave that out for now within the initial modular form of app/browser KMW in 17.0.
export class PageIntegrationHandlers {
  private readonly window: Window;
  private readonly engine: KeymanEngine;
  private readonly domEventTracker = new DomEventTracker();

  constructor(window: Window, engine: KeymanEngine) {
    this.window = window;
    this.engine = engine;

    this.attachHandlers();
  }

  suppressFocusCheck: (e: Event) => boolean = (e) => {
    if(this.engine.contextManager.focusAssistant._IgnoreBlurFocus) {
      // Prevent triggering other blur-handling events (as possible)
      e.stopPropagation();
      e.cancelBubble = true;
    }
    // But DO perform default event behavior (actually blurring & focusing the affected element)
    return true;
  }

  private attachHandlers() {
    const eventTracker = this.domEventTracker;
    /*
     * To prevent propagation of focus & blur events from the input-scroll workaround,
     * we attach top-level capturing listeners to the focus & blur events.  They prevent propagation
     * but NOT default behavior, allowing the scroll to complete while preventing nearly all
     * possible event 'noise' that could result from the workaround.
     */
    eventTracker.attachDOMEvent(document.body, 'focus', this.suppressFocusCheck, true);
    eventTracker.attachDOMEvent(document.body, 'blur', this.suppressFocusCheck, true);
  }

  public shutdown() {
    const eventTracker = this.domEventTracker;

    eventTracker.detachDOMEvent(document.body, 'focus', this.suppressFocusCheck, true);
    eventTracker.detachDOMEvent(document.body, 'blur', this.suppressFocusCheck, true);
  }
}