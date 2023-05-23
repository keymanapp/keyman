import { type Keyboard, Mock } from '@keymanapp/keyboard-processor';
import { type KeyboardStub } from 'keyman/engine/package-cache';
import { CookieSerializer } from 'keyman/engine/dom-utils';
import { eventOutputTarget, PageContextAttachment } from 'keyman/engine/attachment';
import { DomEventTracker, LegacyEventEmitter } from 'keyman/engine/events';
import { DesignIFrame, OutputTarget, nestedInstanceOf } from 'keyman/engine/element-wrappers';
import {
  ContextManagerBase,
  type KeyboardInterface,
  LegacyAPIEvents
} from 'keyman/engine/main';
import { BrowserConfiguration } from './configuration.js';
import { FocusAssistant } from './context/focusAssistant.js';

interface KeyboardCookie {
  current: string;
}

/**
 * Set target element text direction (LTR or RTL), but only if the element is empty
 *
 * If the element base directionality is changed after it contains content, unless all the text
 * has the same directionality, text runs will be re-ordered which is confusing and causes
 * incorrect caret positioning
 *
 * @param       {Object}      Ptarg      Target element
 */
function _SetTargDir(Ptarg: HTMLElement, activeKeyboard: Keyboard) {
  var elDir=(activeKeyboard && activeKeyboard.isRTL) ? 'rtl' : 'ltr';

  if(Ptarg) {
    if(Ptarg instanceof Ptarg.ownerDocument.defaultView.HTMLInputElement
        || Ptarg instanceof Ptarg.ownerDocument.defaultView.HTMLTextAreaElement) {
      if((Ptarg as HTMLInputElement|HTMLTextAreaElement).value.length == 0) {
        Ptarg.dir=elDir;
      }
    } else if(typeof Ptarg.textContent == "string" && Ptarg.textContent.length == 0) { // As with contenteditable DIVs, for example.
      Ptarg.dir=elDir;
    }
  }
}

export default class ContextManager extends ContextManagerBase<BrowserConfiguration> {
  private _activeKeyboard: {keyboard: Keyboard, metadata: KeyboardStub};
  private config: BrowserConfiguration;
  private cookieManager = new CookieSerializer<KeyboardCookie>('KeymanWeb_Keyboard');
  readonly focusAssistant = new FocusAssistant();
  readonly page: PageContextAttachment;
  private mostRecentTarget: OutputTarget<any>;
  private currentTarget: OutputTarget<any>;

  private globalKeyboard: {keyboard: Keyboard, metadata: KeyboardStub};

  private _eventsObj: () => LegacyEventEmitter<LegacyAPIEvents>;
  private domEventTracker = new DomEventTracker();

  constructor(engineConfig: BrowserConfiguration, eventsClosure: () => LegacyEventEmitter<LegacyAPIEvents>) {
    super(engineConfig);

    this._eventsObj = eventsClosure;

    this.page = new PageContextAttachment(window.document, {
      hostDevice: this.config.hostDevice
    });
  }

  get apiEvents(): LegacyEventEmitter<LegacyAPIEvents> {
    return this._eventsObj();
  }

  initialize(): void {
    this.on('keyboardasyncload', (stub, completion) => {
      // TODO:  app/browser - display the loader UI if configured?
      // util.wait('Installing keyboard<br/>' + kbdName);

      completion.then(() => {
        // Cancel the loader UI.
      });
    });

    this.engineConfig.deferForInitialization.then(() => {
      const device = this.engineConfig.hostDevice;

      const noPropagation = (event: Event) => event.stopPropagation()

      // For any elements being attached, or being enabled after having been disabled...
      this.page.on('enabled', (elem) => {
        if(!(elem._kmwAttachment.interface instanceof DesignIFrame)) {
          // For anything attached but (design-mode) iframes...

          // This block:  has to do with maintaining focus.
          if(device.touchable) {
            // Remove any handlers for "NonKMWTouch" elements, since we're enabling it here.
            this.domEventTracker.detachDOMEvent(elem, 'touchstart', this.nonKMWTouchHandler);

            // Prevent base-page touch handlers from causing a defocus when interacting
            // with attached input elements.
            this.domEventTracker.attachDOMEvent(elem, 'touchmove', noPropagation, false);
            this.domEventTracker.attachDOMEvent(elem, 'touchend', noPropagation, false);
          }

          // This block:  has to do with maintaining focus.
          this.domEventTracker.attachDOMEvent(elem,'focus', this._ControlFocus);
          this.domEventTracker.attachDOMEvent(elem,'blur', this._ControlBlur);
          this.domEventTracker.attachDOMEvent(elem,'click', this._Click);
        } else {
          // For design-mode iframes:

          // This block:  has to do with maintaining focus.
          var Lelem=(elem as HTMLIFrameElement).contentWindow.document;
          // I2404 - Attach to IFRAMEs child objects, only editable IFRAMEs here
          if(device.browser == 'firefox') {
            this.domEventTracker.attachDOMEvent(Lelem,'focus', this._ControlFocus);
            this.domEventTracker.attachDOMEvent(Lelem,'blur', this._ControlBlur);
          } else { // Chrome, Safari
            this.domEventTracker.attachDOMEvent(Lelem.body,'focus', this._ControlFocus);
            this.domEventTracker.attachDOMEvent(Lelem.body,'blur', this._ControlBlur);
          }
        }
      });

      // For any elements being detached, disabled, or deliberately not being attached (b/c nonKMWTouchHandler)...
      this.page.on('disabled', (elem) => {
        // Note:  we may not actually be attached at this point.
        if(!(nestedInstanceOf(elem, "HTMLIFrameElement"))) {
          // For anything attached but (design-mode) iframes...

          // This block:  has to do with maintaining focus.
          if(device.touchable) {
            this.domEventTracker.attachDOMEvent(elem, 'touchstart', this.nonKMWTouchHandler, false);

            // does not detach the touch-handlers added in 'enabled'?
          }

          // This block:  has to do with maintaining focus.
          this.domEventTracker.detachDOMEvent(elem,'focus', this._ControlFocus);
          this.domEventTracker.detachDOMEvent(elem,'blur', this._ControlBlur);
          this.domEventTracker.detachDOMEvent(elem,'click', this._Click);
        } else {
          // For design-mode iframes:

          // This block:  has to do with maintaining focus.
          let Lelem = (elem as HTMLIFrameElement).contentWindow.document;
          // Mozilla      // I2404 - Attach to  IFRAMEs child objects, only editable IFRAMEs here
          if(device.browser == 'firefox') {
            // Firefox won't handle these events on Lelem.body - only directly on Lelem (the doc) instead.
            this.domEventTracker.detachDOMEvent(Lelem,'focus', this._ControlFocus);
            this.domEventTracker.detachDOMEvent(Lelem,'blur', this._ControlBlur);
          } else { // Chrome, Safari
            this.domEventTracker.detachDOMEvent(Lelem.body,'focus', this._ControlFocus);
            this.domEventTracker.detachDOMEvent(Lelem.body,'blur', this._ControlBlur);
          }
        }

        // This block:  has to do with maintaining focus (and consequences)
        var lastElem = this.mostRecentTarget?.getElement();
        if(lastElem && lastElem == elem) {
          this.forgetActiveTarget(); // should already auto-hide the OSK while at it via event.
        }
      });

      this.page.install(this.engineConfig.attachType == 'manual');
    });
  }

  get activeTarget(): OutputTarget<any> {
    const maintainingFocus = this.focusAssistant.maintainingFocus;
    return this.currentTarget || (maintainingFocus ? this.mostRecentTarget : null);
  }

  get lastActiveTarget(): OutputTarget<any> {
    return this.mostRecentTarget;
  }

  public deactivateCurrentTarget() {
    this.currentTarget = null;
    // TODO: Not in original, pre-modularized form... but should probably also _ControlBlur?
  }

  public forgetActiveTarget() {
    this.focusAssistant.maintainingFocus = false;
    this.focusAssistant.restoringFocus = false;

    this.setActiveTarget(null);
  }

  private setActiveTarget(target: OutputTarget<any>) {
    const previousTarget = this.mostRecentTarget;

    // We condition on 'priorElement' below as a check to allow KMW to set a default active keyboard.
    let hadRecentElement = !!previousTarget;

    // Must set before _Blur / _Focus to avoid infinite recursion due to complications
    // in setActiveKeyboard behavior with managed keyboard settings.
    this.currentTarget = this.mostRecentTarget = target; // I3363 (Build 301)

    if(this.focusAssistant.restoringFocus) {
      this._BlurKeyboardSettings(target.getElement());
    } else {
      this._FocusKeyboardSettings(target.getElement(), !hadRecentElement);
    }

    // Always do the common focus stuff, instantly returning if we're in an editable iframe.
    if(this._CommonFocusHelper(target)) {
      return true;
    };

    // Set element directionality (but only if element is empty)
    let Ltarg = target.getElement();
    if(target instanceof DesignIFrame) {
      Ltarg = target.docRoot;
    }
    if(Ltarg.ownerDocument && Ltarg instanceof Ltarg.ownerDocument.defaultView.HTMLElement) {
      _SetTargDir(Ltarg, this.activeKeyboard?.keyboard);
    }

    this.emit('targetchange', target);
  }

  // on set activeTarget, make sure to also change it for this.predictionContext!

  get activeKeyboard() {
    return this._activeKeyboard;
  }

  restoreLastActiveTarget() {
    if(!this.mostRecentTarget) {
      return;
    }

    this.focusAssistant.restoringFocus = true;
    this.mostRecentTarget.focus(); // should auto-restore .mostRecentTarget as .currentTarget
                                   // via related focus events.
    this.focusAssistant.restoringFocus = false;
  }

  insertText(kbdInterface: KeyboardInterface<ContextManager>, Ptext: string, PdeadKey: number) {
    // Find the correct output target to manipulate.  The user has likely be interacting with a
    // 'help page' keyboard, like desktop `sil_euro_latin`, and active browser focus on the
    // original context element may have been lost.
    this.restoreLastActiveTarget();

    let outputTarget = this.activeTarget;

    if(outputTarget == null && this.mostRecentTarget) {
      outputTarget = this.activeTarget;
    }

    if(outputTarget != null) {
      return super.insertText(kbdInterface, Ptext, PdeadKey);
    }
    return false;
  }

  /**
   * Reflects the active 'target' upon which any `set activeKeyboard` operation will take place.
   * When `null`, such operations will affect the global default; otherwise, such operations
   * affect only the specified `target`.
   */
  protected get keyboardTarget(): OutputTarget<any> {
    let target = this.currentTarget || this.mostRecentTarget;
    let attachmentInfo = target?.getElement()._kmwAttachment;

    if(attachmentInfo?.keyboard || attachmentInfo?.keyboard === '') {
      return target;
    } else {
      return null;
    }
  }

  // Note:  is part of the keyboard activation process.  Not to be called directly by published API.
  activateKeyboardForTarget(kbd: {keyboard: Keyboard, metadata: KeyboardStub}, target: OutputTarget<any>) {
    let attachment = target.getElement()._kmwAttachment;

    if(!attachment) {
      // if not set with an "independent keyboard", changes the global.
      this.globalKeyboard = kbd;
    } else {
      // if set with an "independent keyboard", changes only the active target's keyboard.
      //
      // This method is not called on the pathway to shift a control back to 'global keyboard' mode;
      // only after.
      attachment.keyboard = kbd?.metadata.id ?? '';
      attachment.languageCode = kbd?.metadata.langId ?? '';
    }

    if(this.keyboardTarget == target) {
      this._activeKeyboard = kbd;
    }
  }

  /**
   * Allows setting a control to a specific keyboard that does not change if the active keyboard changes when other
   * controls are active.  Only activates the keyboard if the specified control represents the currently-active
   * context.
   *
   * This is the core method that backs
   * https://help.keyman.com/DEVELOPER/ENGINE/WEB/15.0/reference/core/setKeyboardForControl.
   * @param target
   * @param metadata
   */
  public setKeyboardForTarget(target: OutputTarget<any>, kbdId: string, langId: string) {
    if(target instanceof DesignIFrame) {
      console.warn("'keymanweb.setKeyboardForControl' cannot set keyboard on iframes.");
      return;
    }

    let attachment = target.getElement()._kmwAttachment;

    if(!attachment) {
      return;
    } else {
      // If directly set
      attachment.keyboard = kbdId || null;
      attachment.languageCode = langId || null;

      if(this.keyboardTarget == target) {
        this.activateKeyboard(attachment.keyboard, attachment.languageCode, true);
      }
    }
  }

  public async activateKeyboard(keyboardId: string, languageCode?: string, saveCookie?: boolean): Promise<boolean> {
    saveCookie ||= false;
    const originalKeyboardTarget = this.keyboardTarget;

    try {
      let result = await super.activateKeyboard(keyboardId, languageCode, saveCookie);

      if(saveCookie && !originalKeyboardTarget) { // if the active target uses global keyboard settings
        this.cookieManager.save({current: `${keyboardId}:${languageCode}`});
      }

      // Only do these if the active keyboard-target still matches the original keyboard-target;
      // otherwise, maintain what's correct for the currently active one.
      if(originalKeyboardTarget == this.keyboardTarget) {
        _SetTargDir(this.currentTarget?.getElement(), this.keyboardCache.getKeyboard(keyboardId));
        // util.addStyleSheet(domManager.setAttachmentFontStyle(kbdStub.KF));
        this.focusAssistant.restoringFocus = true;
      }

      return result;
    } catch(err) {
      // non-embedded:  if keyboard activation failed, deactivate the keyboard.

      // Make sure we don't infinite-recursion should the deactivate somehow fail.
      if(this.config.hostDevice.touchable) {
        // Fallback behavior - if on a touch device, we need to keep a keyboard visible.
        const defaultStub = this.keyboardCache.defaultStub;
        if(defaultStub.id != keyboardId || defaultStub.langId != languageCode) {
          await this.activateKeyboard(defaultStub.id, defaultStub.langId, true).catch(() => {});
        } // else "We already failed, so give up."
      } else {
        // Fallback behavior - if on a desktop device, the user still has a physical keyboard.
        // Just clear out the active keyboard & OSK.
        await this.activateKeyboard('', '', false).catch(() => {});
      }

      if((this.config as BrowserConfiguration).shouldAlert) {
        // TODO:  util.alert error report
      }

      throw err; // since the site-dev consumer may want to do their own error-handling.
    }
  }

  //

  /**
   * Function             _BlurKeyboardSettings
   * Description          Stores the last active element's keyboard settings.  Should be called
   *                      whenever a KMW-enabled page element loses control.
   */
  _BlurKeyboardSettings(lastElem: HTMLElement, PInternalName?: string, PLgCode?: string) {
    var keyboardID = this.activeKeyboard ? this.activeKeyboard.keyboard.id : '';
    var langCode = this.activeKeyboard?.metadata.langId;

    if(PInternalName !== undefined && PLgCode !== undefined) {
      keyboardID = PInternalName;
      langCode = PLgCode;
    }

    if(lastElem && lastElem._kmwAttachment.keyboard != null) {
      lastElem._kmwAttachment.keyboard = keyboardID;
      lastElem._kmwAttachment.languageCode = langCode;
    } else {
      this.globalKeyboard = this.activeKeyboard;
    }
  }

  /**
   * Function             _FocusKeyboardSettings
   * @param   {boolean}   blockGlobalChange   A flag indicating if the global keyboard setting should be ignored for this call.
   * Description          Restores the newly active element's keyboard settings.  Should be called
   *                      whenever a KMW-enabled page element gains control, but only once the prior
   *                      element's loss of control is guaranteed.
   */
  _FocusKeyboardSettings(lastElem: HTMLElement, blockGlobalChange: boolean) {
    // Important pre-condition:  the newly-focused element must be set as active.
    let attachment = lastElem._kmwAttachment;
    const global = this.globalKeyboard;

    if(attachment.keyboard != null) {
      this.activateKeyboard(attachment.keyboard, attachment.languageCode, true);
    } else if(!blockGlobalChange) {
      this.activateKeyboard(global?.metadata.id, global?.metadata.langId, true);
    }
  }

  /**
   * Function             _CommonFocusHelper
   * @param   {Element}   target
   * @returns {boolean}
   * Description          Performs common state management for the various focus events of KeymanWeb.
   *                      The return value indicates whether (true) or not (false) the calling event handler
   *                      should be terminated immediately after the call.
   */
  _CommonFocusHelper(outputTarget: OutputTarget<any>): boolean {
    const focusAssistant = this.focusAssistant;

    // if(target.ownerDocument && target instanceof target.ownerDocument.defaultView.HTMLIFrameElement) {
    //   if(!this.keyman.domManager._IsEditableIframe(target, 1)) {
    //     DOMEventHandlers.states._DisableInput = true;
    //     return true;
    //   }
    // }
    // DOMEventHandlers.states._DisableInput = false;

    // const outputTarget = dom.Utils.getOutputTarget(target);

    let activeKeyboard = this.activeKeyboard?.keyboard;
    if(!focusAssistant.restoringFocus) {
      outputTarget?.deadkeys().clear();
      activeKeyboard?.notify(0, outputTarget, 1);  // I2187
    }

    //if(!focusAssistant.restoringFocus && DOMEventHandlers.states._SelectionControl != target) {
    if(!focusAssistant.restoringFocus && this.mostRecentTarget != outputTarget) {
      focusAssistant.maintainingFocus = false;
    }
    focusAssistant.restoringFocus = false;

    //DOMEventHandlers.states._SelectionControl = target; // effectively was .mostRecentTarget, as best as I can tell.

    // Now that we've fully entered the new context, invalidate the context so we can generate initial predictions from it.
    // (Note that the active keyboard will have been updated by a method called before this one; the newly-focused
    // context should now be 100% ready.)
    this.resetContext();

    return false;
  }



  /**
   * Respond to KeymanWeb-aware input element receiving focus
   */
  _ControlFocus = (e: FocusEvent): boolean => {
    // Step 1: determine the corresponding OutputTarget instance.
    const target = eventOutputTarget(e);
    if(!target) {
      // Probably should also make a warning or error?
      return true;
    }

    // ???? ?: ensure it's properly active?
    // if(target instanceof DesignIFrame) { //**TODO: check case reference
    //   // But... the following should already have been done during attachment...
    //   // attachmentEngine._AttachToIframe(Ltarg as HTMLIFrameElement);
    //   target.docRoot
    //   Ltarg=Ltarg.contentWindow.document.body; // And we only care about Ltarg b/c of finding the OutputTarget.
    // }

    // Save it for the event in step 3... but now, before we mutate the field's value!
    const previousTarget = this.lastActiveTarget;

    // Step 2:  Make the newly-focused control the active control, and thus the active context.
    this.setActiveTarget(target);

    // Step 3:  related events

    // //Execute external (UI) code needed on focus if required
    this.apiEvents.callEvent('controlfocused', {
      target: target.getElement(),
      activeControl: previousTarget?.getElement()
    });

    return true;
  }

  /**
   * Respond to KMW losing focus on event
   */
  _ControlBlur = (e: FocusEvent): boolean => {
    // Step 0:  if we're in a state where loss-of-focus should be outright-ignored, bypass the handler entirely.
    if(this.focusAssistant._IgnoreNextSelChange) {

      // If a keyboard calls saveFocus() (KSF), then ignore the
      // next selection change
      this.focusAssistant._IgnoreNextSelChange--;
      e.cancelBubble = true;
      e.stopPropagation();
      return true;
    }

    if(this.focusAssistant._IgnoreBlurFocus) {
      // Prevent triggering other blur-handling events (as possible)
      e.cancelBubble = true;
      e.stopPropagation();
      return true;
    }

    // Step 1: determine the corresponding OutputTarget instance.
    let target = eventOutputTarget(e);
    if (target == null) {
      return true;
    }

    // Step 2:  persist the keyboard setting for the deactivating context.

    ////keymanweb._SelectionControl = null;
    if(this.lastActiveTarget) {
      // There's no harm in saving them at this stage, even if we're still in the `maintainingFocus` state.
      this._BlurKeyboardSettings(this.lastActiveTarget.getElement());
    }

    // Step 3: Now that we've handled all prior-element maintenance, update the active and 'last-active element'.
    // (The "context target" state fields)
    const previousTarget = this.activeTarget;
    this.currentTarget = null; // I3363 (Build 301)
    this.mostRecentTarget = target;

    // Step 4: any and all related events
    /* If the KeymanWeb UI is active as a user changes controls, all UI-based effects
     * should be restrained to this control in case the user is manually specifying
     * languages on a per-control basis.
     */
    this.focusAssistant.restoringFocus = false;

    let activeKeyboard = this.activeKeyboard;
    const maintainingFocus = this.focusAssistant.maintainingFocus;
    if(!maintainingFocus && activeKeyboard) {
      activeKeyboard.keyboard.notify(0, target, 0);  // I2187
    }
    if(previousTarget && !this.activeTarget) {
      this.emit('targetchange', null);
    }

    this.apiEvents.callEvent('controlblurred', {
      target: target.getElement(),
      event: e,
      isActivating: maintainingFocus
    });

    // Is not an "API event"; it models a native browser event instead.
    this.doChangeEvent(target);
    this.resetContext();
    return true;
  }

  doChangeEvent(target: OutputTarget<any>) {
    if(target.changed) {
      let event = new Event('change', {"bubbles": true, "cancelable": false});
      target.getElement().dispatchEvent(event);
    }

    target.changed = false;
  }

  _Click: (e: MouseEvent) => boolean = (e: MouseEvent) => {
    this.resetContext();
    return true;
  };

  /**
   * Function     nonKMWTouchHandler
   * Scope        Private
   * Description  A handler for KMW-touch-disabled elements when operating on touch devices.
   */
  nonKMWTouchHandler = (x: Event) => {
    this.focusAssistant.focusing=false;
    clearTimeout(this.focusAssistant.focusTimer);
    this.forgetActiveTarget();
    // this.keyman.osk.hideNow(); // TODO:  is more aggressive than the default - how to migrate this tidbit?
  };

  shutdown() {
    this.page.shutdown();
    this.domEventTracker.shutdown();
  }
}