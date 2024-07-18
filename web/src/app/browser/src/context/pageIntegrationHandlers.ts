import { DomEventTracker } from 'keyman/engine/events';

import KeymanEngine from "../keymanEngine.js";
import { FocusAssistant } from './focusAssistant.js';
import { RotationProcessor } from '../utils/rotationProcessor.js';

// Note:  in the future, it'd probably be best to have an instance per iframe window as
// well as the top-level window.  This was not done in or before KMW 16.0 though, so
// we'll leave that out for now within the initial modular form of app/browser KMW in 17.0.
export class PageIntegrationHandlers {
  private readonly window: Window;
  private readonly engine: KeymanEngine;
  private readonly domEventTracker = new DomEventTracker();

  /**
   * Used together with `deactivateOnRelease` to determine the distance of vertical scrolls;
   * if sufficiently far at any point, we avoid deactivating the current context when it ends.
   */
  private touchY: number;

  /**
   * Used together with `touchY` to determine the distance of vertical scrolls;
   * if sufficiently far at any point, we avoid deactivating the current context when it ends.
   */
  private deactivateOnRelease: boolean;

  /**
   * Used on certain browser/OS combinations (e.g. Chrome on Android) to prevent odd behaviors
   * that arise when URL bars scroll into view during an ongoing scroll, as this can impede
   * proper / smooth positioning of the OSK.  (Deactivating the active target also hides the OSK.)
   */
  private deactivateOnScroll: boolean;

  /**
   * This component should only ever be applied to a base page:  we need the ability to add 'scroll
   * space' on touch devices so that the OSK doesn't block the bottom.
   */
  private mobilePageTrailer: HTMLDivElement;

  private rotationProcessor: RotationProcessor;

  constructor(window: Window, engine: KeymanEngine) {
    this.window = window;
    this.engine = engine;

    this.attachHandlers();

    if(engine.config.hostDevice.touchable) {
      this.buildPageTrailer();

      this.rotationProcessor = new RotationProcessor(this.engine);
      this.rotationProcessor.init();
    }
  }

  private buildPageTrailer() {
    // Add a blank DIV to the bottom of the page to allow the bottom of the page to be shown
    const dTrailer = this.mobilePageTrailer = document.createElement('div');
    const ds=dTrailer.style;
    ds.width='100%';
    ds.height=(screen.width/2)+'px';  // ... interesting choice, but okay.
    document.body.appendChild(dTrailer);
  }

  private get focusAssistant(): FocusAssistant {
    return this.engine.contextManager.focusAssistant;
  }

  private suppressFocusCheck: (e: FocusEvent) => boolean = (e) => {
    if(this.focusAssistant.isTargetForcingScroll()) {
      // Prevent triggering other blur-handling events (as possible) - this blur
      // is programmatic in order to force a browser scroll-position update.
      // All focus changes should be prevented at this time.
      e.stopPropagation();
      e.cancelBubble = true;
    }
    // But DO perform default event behavior (actually blurring & focusing the affected element)
    return true;
  }

  /**
   * Reset context when entering or exiting the active element.
   * Will also trigger OSK shift state / layer reset.
   **/
  private pageFocusHandler: (e: FocusEvent) => boolean = () => {
    if(!this.focusAssistant.maintainingFocus && this.engine.osk?.vkbd) {
      this.engine.contextManager.deactivateCurrentTarget();
      this.engine.contextManager.resetContext();
    }
    return false;
  }

  // Sets up page-default touch-based handling for activation-state management.
  // These always trigger for the page, wherever a touch may occur. Does not
  // prevent element-specific or OSK-key-specific handling from triggering.

  private touchStartActivationHandler: (e: TouchEvent) => boolean = (e) => {
    const osk = this.engine.osk;
    if(!osk) {
      return false;
    }
    const device = this.engine.config.hostDevice;

    this.deactivateOnRelease=true;
    this.touchY=e.touches[0].screenY;

    // On Chrome, scrolling up or down causes the URL bar to be shown or hidden
    // according to whether or not the document is at the top of the screen.
    // But when doing that, each OSK row top and height gets modified by Chrome
    // looking very ugly.  It would be best to hide the OSK then show it again
    // when the user scroll finishes, but Chrome has no way to reliably report
    // the touch end event after a move. c.f. http://code.google.com/p/chromium/issues/detail?id=152913
    // The best compromise behaviour is simply to hide the OSK whenever any
    // non-input and non-OSK element is touched.
    this.deactivateOnScroll=false;
    if(device.OS == 'android' && device.browser == 'chrome') {
      // this.deactivateOnScroll has the inverse of the 'true' default,
      // but that fact actually facilitates the following conditional logic.
      if(typeof(osk._Box) == 'undefined') return false;
      if(typeof(osk._Box.style) == 'undefined') return false;

      // The following tests are needed to prevent the OSK from being hidden during normal input!
      let p=(e.target as HTMLElement).parentElement;
      if(typeof(p) != 'undefined' && p != null) {
        if(p.getAttribute('class')?.indexOf('kmw-key-') >= 0) return false;
        if(typeof(p.parentElement) != 'undefined' && p.parentElement != null) {
          p=p.parentElement;
          if(p.getAttribute('class')?.indexOf('kmw-key-') >= 0) return false;
        }
      }

      this.deactivateOnScroll = true;
    }
    return false;
  };

  private touchMoveActivationHandler: (e: TouchEvent) => boolean = (e) => {
    if(this.deactivateOnScroll) {  // Android / Chrone case.
      this.focusAssistant.focusing = false;
      this.engine.contextManager.deactivateCurrentTarget();
    }

    const y = e.touches[0].screenY;
    const y0 = this.touchY;
    if(y-y0 > 5 || y0-y < 5) {
      this.deactivateOnRelease = false;
    }
    return false;
  };

  private touchEndActivationHandler: (e: TouchEvent) => boolean = (e) => {
    // Should not hide OSK if simply closing the language menu (30/4/15)
    // or if the focusing timer (focusAssistant.setFocusTimer) is still active.
    if(this.deactivateOnRelease && !this.engine.touchLanguageMenu && !this.focusAssistant.focusing) {
      this.engine.contextManager.deactivateCurrentTarget();
    }
    this.deactivateOnRelease=false;
    return false;
  };


  private _WindowLoad: (e: Event) => void = () => {
    // Always return to top of page after a page reload
    document.body.scrollTop=0;
    if(typeof document.documentElement != 'undefined') {
      document.documentElement.scrollTop=0;
    }
  }

  /**
   * Function     _WindowUnload
   * Scope        Private
   * Description  Remove handlers before detaching KMW window
   */
  private _WindowUnload: () => void = () => {
    // Future note:  should restrict this to anything for the corresponding document if on a
    // child iframe, not the whole engine.
    this.engine.shutdown();
  }

  private attachHandlers() {
    const eventTracker = this.domEventTracker;
    const device = this.engine.config.hostDevice;
    const docBody = this.window.document.body;

    eventTracker.attachDOMEvent(this.window, 'focus', this.pageFocusHandler, false);
    eventTracker.attachDOMEvent(this.window, 'blur', this.pageFocusHandler, false);

    /*
     * To prevent propagation of focus & blur events from the input-scroll workaround,
     * we attach top-level capturing listeners to the focus & blur events.  They prevent propagation
     * but NOT default behavior, allowing the scroll to complete while preventing nearly all
     * possible event 'noise' that could result from the workaround.
     */
    eventTracker.attachDOMEvent(docBody, 'focus', this.suppressFocusCheck, true);
    eventTracker.attachDOMEvent(docBody, 'blur', this.suppressFocusCheck, true);

    if(device.touchable) {
      eventTracker.attachDOMEvent(docBody, 'touchstart', this.touchStartActivationHandler,false);
      eventTracker.attachDOMEvent(docBody, 'touchmove',  this.touchMoveActivationHandler, false);
      eventTracker.attachDOMEvent(docBody, 'touchend',   this.touchEndActivationHandler,  false);
    }

    eventTracker.attachDOMEvent(window, 'load',   this._WindowLoad,  false);
    eventTracker.attachDOMEvent(window, 'unload', this._WindowUnload,false);

    eventTracker.attachDOMEvent(document, 'keyup', this.engine.hotkeyManager._Process, false);
  }

  public shutdown() {
    const eventTracker = this.domEventTracker;
    const device = this.engine.config.hostDevice;
    const docBody = this.window.document.body;

    // See `attachHandlers` for the purpose behind all handlers listed here.

    eventTracker.detachDOMEvent(this.window, 'focus', this.pageFocusHandler, false);
    eventTracker.detachDOMEvent(this.window, 'blur', this.pageFocusHandler, false);

    eventTracker.detachDOMEvent(docBody, 'focus', this.suppressFocusCheck, true);
    eventTracker.detachDOMEvent(docBody, 'blur', this.suppressFocusCheck, true);

    if(device.touchable) {
      eventTracker.detachDOMEvent(docBody, 'touchstart', this.touchStartActivationHandler,false);
      eventTracker.detachDOMEvent(docBody, 'touchmove',  this.touchMoveActivationHandler, false);
      eventTracker.detachDOMEvent(docBody, 'touchend',   this.touchEndActivationHandler,  false);

      this.mobilePageTrailer?.parentElement.removeChild(this.mobilePageTrailer);
    }

    eventTracker.detachDOMEvent(window, 'load',   this._WindowLoad,  false);
    eventTracker.detachDOMEvent(window, 'unload', this._WindowUnload,false);

    eventTracker.detachDOMEvent(document, 'keyup', this.engine.hotkeyManager._Process, false);
  }
}