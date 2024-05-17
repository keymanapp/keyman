import { type Keyboard, KeyboardScriptError } from '@keymanapp/keyboard-processor';
import { type KeyboardStub } from 'keyman/engine/package-cache';
import { CookieSerializer } from 'keyman/engine/dom-utils';
import { eventOutputTarget, outputTargetForElement, PageContextAttachment } from 'keyman/engine/attachment';
import { DomEventTracker, LegacyEventEmitter } from 'keyman/engine/events';
import { DesignIFrame, OutputTarget, nestedInstanceOf } from 'keyman/engine/element-wrappers';
import {
  ContextManagerBase,
  type KeyboardInterface,
  LegacyAPIEvents
} from 'keyman/engine/main';
import { BrowserConfiguration } from './configuration.js';
import { FocusAssistant } from './context/focusAssistant.js';

export interface KeyboardCookie {
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
  const elDir = activeKeyboard?.isRTL ? 'rtl' : 'ltr';

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
  private cookieManager = new CookieSerializer<KeyboardCookie>('KeymanWeb_Keyboard');
  readonly focusAssistant = new FocusAssistant(() => this.activeTarget?.isForcingScroll());
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
      hostDevice: this.engineConfig.hostDevice
    });

    this.focusAssistant.on('maintainingfocusend', () => {
      // Basically, if the maintaining state were the reason we still had an `activeTarget`...
      if(!this.activeTarget && this.mostRecentTarget) {
        this.emit('targetchange', this.activeTarget);
      }
    });
  }

  get apiEvents(): LegacyEventEmitter<LegacyAPIEvents> {
    return this._eventsObj();
  }

  initialize(): void {
    this.on('keyboardasyncload', (stub, completion) => {
      this.engineConfig.alertHost?.wait('Installing keyboard<br/>' + stub.name);

      completion.then(() => {
        this.engineConfig.alertHost?.wait(); // cancels the wait.
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

        if(elem.ownerDocument.activeElement == elem) {
          this.setActiveTarget(outputTargetForElement(elem), true);
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

      // This fires the events we just registered for.
      this.page.install(this.engineConfig.attachType == 'manual');
    });
  }

  get activeTarget(): OutputTarget<any> {
    /*
     * Assumption:  the maintainingFocus flag may only be set when there is a current target.
     * This is not enforced proactively at present, but the assumption should hold.  (2023-05-03)
     */
    const maintainingFocus = this.focusAssistant.maintainingFocus;
    return this.currentTarget || (maintainingFocus ? this.mostRecentTarget : null);
  }

  get lastActiveTarget(): OutputTarget<any> {
    return this.mostRecentTarget;
  }

  public deactivateCurrentTarget() {
    const priorTarget = this.activeTarget || this.lastActiveTarget;

    /* During integrated tests, it was possible in the past for a `beforeAll`
     * -initialized KMW to reach this state between tests.  The target fixture
     * got cleared, but the `mostRecentTarget` / `lastActiveTarget` was not
     * - just the `currentTarget` / `activeTarget`.  See #9718.
     *
     * Newly-added code in `forgetActiveTarget` seeks to prevent this scenario,
     * but as there's no consistent repro to prove it sufficient, an appropriate
     * guard-condition has been added here too.
     */
    if(priorTarget && this.page.isAttached(priorTarget.getElement())) {
      this._BlurKeyboardSettings(priorTarget.getElement());
    }

    // Because of focus-maintenance effects
    if(!this.activeTarget) {
      this.setActiveTarget(null, true);
    }
  }

  public forgetActiveTarget() {
    this.focusAssistant.maintainingFocus = false;
    this.focusAssistant.restoringFocus = false;

    const priorTarget = this.activeTarget || this.mostRecentTarget;
    if(priorTarget) {
      this._BlurKeyboardSettings(priorTarget.getElement());
    }

    // Will ensure that the element is no longer active.  Does not erase
    // it from being the `lastActiveTarget`, though.
    this.setActiveTarget(null, true);

    // So we erase it here.
    if(priorTarget == this.lastActiveTarget) {
      this.mostRecentTarget = null;
    }
  }

  public setActiveTarget(target: OutputTarget<any>, sendEvents: boolean) {
    const previousTarget = this.mostRecentTarget;
    const originalTarget = this.activeTarget; // may differ, depending on focus state.

    if(target == originalTarget) {
      // A focus state may have .currentTarget as null at this stage; if the func
      // is being called with a non-null parameter, we want this SET. #9404
      if(originalTarget) {
        this.currentTarget = originalTarget;
      }

      /**
       * If it's already active, we should cancel early.
       *
       * The #1 reason - we don't want .resetContext calls in this scenario.
       * In particular, moving the caret or setting the selection range of an
       * <input> or <textarea> in desktop Safari programmatically WILL trigger
       * focus events!
       *
       * https://bugs.webkit.org/show_bug.cgi?id=224425
       *
       * > In WebKit, focus follows selection so if you modify selection, then the
       *   focus will be moved there.
       *
       * Caret manipulation in the browser, as needed by certain keyboard text
       * operations, IS text selection - of width zero, but still selection.
       *
       * At present, even if setting selection on the focused element, Safari will
       * still trigger a focus event upon it... which can cascade here if uncaught
       * and trigger a contextReset DURING keyboard rule processing without this
       * guard.
       *
       * The #2 reason:  the `forceScroll` method used within the Input and Textarea
       * types whenever the selection must be programatically updated.  The blur
       * is 'swallowed', preventing it from being dropped as 'active'. However, the
       * corresponding focus is not swallowed... until this if-condition's check.
       */
      return;
    }

    // We condition on 'priorElement' below as a check to allow KMW to set a default active keyboard.
    let hadRecentElement = !!previousTarget;

    // Must set before _Blur / _Focus to avoid infinite recursion due to complications
    // in setActiveKeyboard behavior with managed keyboard settings.
    this.currentTarget = this.mostRecentTarget = target; // I3363 (Build 301)
    this.predictionContext.setCurrentTarget(target);

    if(this.focusAssistant.restoringFocus) {
      this._BlurKeyboardSettings(target.getElement());
    } else if(target) {
      this._FocusKeyboardSettings(target.getElement(), !hadRecentElement);
    }

    // Always do the common focus stuff, instantly returning if we're in an editable iframe.
    if(this._CommonFocusHelper(target)) {
      return true;
    };

    // Set element directionality (but only if element is empty)
    let focusedElement = target?.getElement();
    if(target instanceof DesignIFrame) {
      focusedElement = target.docRoot;
    }
    if(focusedElement && focusedElement.ownerDocument && focusedElement instanceof focusedElement.ownerDocument.defaultView.HTMLElement) {
      _SetTargDir(focusedElement, this.activeKeyboard?.keyboard);
    }

    if(target != originalTarget) {
      this.emit('targetchange', target);
    }

    //Execute external (UI) code needed on focus if required
    if(sendEvents) {
      let blurredElement = previousTarget?.getElement();
      if(previousTarget instanceof DesignIFrame) {
        blurredElement = previousTarget.docRoot;
      }

      if(!focusedElement) {
        if(blurredElement) {
          this.apiEvents.callEvent('controlblurred', {
            target: blurredElement,
            event: null,
            isActivating: this.focusAssistant.maintainingFocus
          });
        }
      } else {
        // Note:  indicates the previous control being blurred (as
        // `activeControl`). 'controlfocused' and 'controlblurred' are
        // treated as mutually exclusive, with the latter only happening
        // when nothing KMW-related is focused.
        this.apiEvents.callEvent('controlfocused', {
          target: focusedElement,
          activeControl: blurredElement
        });
      }
    }
  }

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
   * Determines the 'target' currently used to determine which keyboard should be active.
   * When `null`, keyboard-activation operations will affect the global default; otherwise,
   * such operations affect only the specified `target`.
   *
   * This is based on the current `.activeTarget` and its related attachment metadata.
   */
  protected currentKeyboardSrcTarget(): OutputTarget<any> {
    let target = this.currentTarget || this.mostRecentTarget;

    if(this.isTargetKeyboardIndependent(target)) {
      return target;
    } else {
      return null;
    }
  }

  private isTargetKeyboardIndependent(target: OutputTarget<any>): boolean {
    let attachmentInfo = target?.getElement()._kmwAttachment;

    // If null or undefined, we're in 'global' mode.
    return !!(attachmentInfo?.keyboard || attachmentInfo?.keyboard === '');
  }

  // Note:  is part of the keyboard activation process.  Not to be called directly by published API.
  activateKeyboardForTarget(kbd: {keyboard: Keyboard, metadata: KeyboardStub}, target: OutputTarget<any>) {
    let attachment = target?.getElement()._kmwAttachment;

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

    if(this.currentKeyboardSrcTarget() == target) {
      this._activeKeyboard = kbd;

      // Also, update with appropriate styling.
      const activeStub = kbd?.metadata;
      this.page.setAttachmentFont(activeStub?.KFont, this.engineConfig.paths.fonts, this.engineConfig.hostDevice.OS);
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

    // Catches if the target is already in independent-mode, even if it's being cancelled
    // during this call.
    const wasPriorTarget = this.currentKeyboardSrcTarget() == target;

    if(!attachment) {
      return;
    } else {
      // Either establishes or cancels independent-keyboard mode by setting the
      // associated metadata.  This will have direct effects on the results
      // of .currentKeyboardSrcTarget().
      attachment.keyboard = kbdId || null;
      attachment.languageCode = langId || null;

      // If it has just entered independent-keyboard mode, we need the second check.
      if(wasPriorTarget || this.currentKeyboardSrcTarget() == target) {
        const globalKbd = this.globalKeyboard.metadata;

        // The `||` bits below - in case we're cancelling independent-keyboard mode.
        this.activateKeyboard(
          attachment.keyboard || globalKbd.id,
          attachment.languageCode || globalKbd.langId,
          true
        );
      }
    }
  }

  public getKeyboardStubForTarget(target: OutputTarget<any>) {
    if(!this.isTargetKeyboardIndependent(target)) {
      return this.globalKeyboard.metadata;
    } else {
      const attachment = target.getElement()._kmwAttachment;
      return this.keyboardCache.getStub(attachment.keyboard, attachment.languageCode);
    }
  }

  protected getFallbackStubKey() {
    const emptyCodes = {
      id: '',
      langId: ''
    };

    if(this.engineConfig.hostDevice.touchable) {
      /* Fallback behavior - if on a touch device, we need to keep a keyboard visible
       * if one is available.
       *
       * When literally none are available, setting `emptyCodes` will ensure that `globalKeyboard`
       * is unset properly and that relevant keyboard events are still generated.  (engine/main
       * delegates 'fallback behavior' to its derived classes, so the parent class won't undo it.)
       */
      return this.keyboardCache.defaultStub || emptyCodes;
    } else {
      // Fallback behavior - if on a desktop device, the user still has a physical keyboard.
      // Just clear out the active keyboard & OSK.
      return emptyCodes;
    }
  }

  public async activateKeyboard(keyboardId: string, languageCode?: string, saveCookie?: boolean): Promise<boolean> {
    saveCookie ||= false;
    const originalKeyboardTarget = this.currentKeyboardSrcTarget();

    // Must do here b/c of fallback behavior stuff defined below.
    // If the default keyboard is requested, load that.  May vary based on form-factor, which is
    // part of what .getFallbackStubKey() handles.
    if(!keyboardId) {
      keyboardId = this.getFallbackStubKey().id;
      languageCode = this.getFallbackStubKey().langId;
    }

    try {
      let result = await super.activateKeyboard(keyboardId, languageCode, saveCookie);

      this.engineConfig.alertHost?.wait(); // clear any pending waits.

      if(saveCookie && !originalKeyboardTarget) { // if the active target uses global keyboard settings
        this.cookieManager.save({current: `${keyboardId}:${languageCode}`});
      }

      // Only do these if the active keyboard-target still matches the original keyboard-target;
      // otherwise, maintain what's correct for the currently active one.
      if(originalKeyboardTarget == this.currentKeyboardSrcTarget()) {
        _SetTargDir(this.currentTarget?.getElement(), this.keyboardCache.getKeyboard(keyboardId));
        this.page.setAttachmentFont(this.activeKeyboard?.metadata?.KFont, this.engineConfig.paths.fonts, this.engineConfig.hostDevice.OS);

        this.restoreLastActiveTarget();
      }

      return result;
    } catch(err) {
      // non-embedded:  if keyboard activation failed, deactivate the keyboard.

      const fallback = async () => {
        // Make sure we don't infinite-recursion should the deactivate somehow fail.
        const fallbackCodes = this.getFallbackStubKey();
        if((fallbackCodes.id != keyboardId)) {
          await this.activateKeyboard(fallbackCodes.id, fallbackCodes.langId, true).catch(() => {});
        } // else "We already failed, so give up."
      }

      this.engineConfig.alertHost?.wait(); // clear the wait message box, either way.

      const message = (err as Error)?.message ||
                      'Sorry, the ' + keyboardId + ' keyboard for ' + languageCode + ' is not currently available.';

      if(err instanceof KeyboardScriptError) {
        // We get signaled about error log messages if the site is connected to our Sentry error reporting
        // system; we want to know if we have a broken keyboard that's been published.
        console.error(err || message);
      } else {
        // If it's just internet connectivity or "file not found" issues, that's not worth reporting
        // to Sentry.
        console.warn(err || message);
      }

      if(this.engineConfig.alertHost) {
        // Possible future TODO:  have it return a Promise that resolves on completion of `fallback`?
        // Though, we're talking about dropping the 'alert' subsystem entirely at some point.
        this.engineConfig.alertHost?.alert(message, fallback);
      } else {
        await fallback();
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
    } else if(!blockGlobalChange && (global?.metadata != this._activeKeyboard?.metadata)) {
      // TODO:  can we drop `!blockGlobalChange` in favor of the latter check?
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

    let activeKeyboard = this.activeKeyboard?.keyboard;
    if(!focusAssistant.restoringFocus) {
      outputTarget?.deadkeys().clear();
      activeKeyboard?.notify(0, outputTarget, 1);  // I2187
    }

    if(!focusAssistant.restoringFocus && this.mostRecentTarget != outputTarget) {
      focusAssistant.maintainingFocus = false;
    }
    focusAssistant.restoringFocus = false;

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

    // Step 2:  Make the newly-focused control the active control, and thus the active context.
    this.setActiveTarget(target, true);

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

    if(this.focusAssistant.isTargetForcingScroll()) {
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

    // After a .forgetActiveTarget call occurs before _ControlBlur is called on the corresponding element,
    // we should avoid accidentally 'remembering' it here.
    if(previousTarget || this.lastActiveTarget) {
      this.mostRecentTarget = target;
    }

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
   * Gets the 'saved keyboard' cookie value for the last keyboard used in the
   * iser's previous session.
   **/
  getSavedKeyboardRaw(): string {
    const cookie = new CookieSerializer<KeyboardCookie>('KeymanWeb_Keyboard');
    var v = cookie.load(decodeURIComponent);

    if(typeof(v.current) != 'string') {
      return 'Keyboard_us:en';
    } else if(v.current == 'Keyboard_us:eng') {
      // 16.0 used the :eng variant!
      return 'Keyboard_us:en';
    } else {
      return v.current;
    }
  }

  /**
   * Gets the cookie for the name and language code of the most recently active keyboard
   *
   *  Defaults to US English, but this needs to be user-set in later revision (TODO)
   *
   * @return      {string}          InternalName:LanguageCode
   **/
  getSavedKeyboard(): string {
    let cookieValue = this.getSavedKeyboardRaw();

    // Check that the requested keyboard is included in the available keyboard stubs
    const stubs = this.keyboardCache.getStubList()
    let kd: string;

    for(let n=0; n<stubs.length; n++) {
      kd=stubs[n]['KI']+':'+stubs[n]['KLC'];
      if(kd == cookieValue) {
        return kd;
      }
    }

    // Default to US English if available (but don't assume it is first)
    for(let n=0; n<stubs.length; n++) {
      kd=stubs[n]['KI']+':'+stubs[n]['KLC'];
      if(kd == 'Keyboard_us:en') {
        return kd;
      }
    }

    // Otherwise use the first keyboard stub
    if(stubs.length > 0) {
      return stubs[0]['KI']+':'+stubs[0]['KLC'];
    }

    // Or US English if no stubs loaded (should never happen)
    return 'Keyboard_us:en';
  }

  /**
   * Restore the most recently used keyboard, if still available
   */
  restoreSavedKeyboard(kbd) {
    // If no saved keyboard, defaults to US English
    const d=kbd;

    // Identify the stub with the saved keyboard
    let t=d.split(':');
    if(t.length < 2) {
      t[1]='';
    }

    // Find the matching stub; if it doesn't exist, default to the first available stub.
    let stub = this.keyboardCache.getStub(t[0], t[1]) || this.keyboardCache.defaultStub;

    // Sets the default stub (as specified with the `getSavedKeyboard` call) as active.
    if(stub) {
      return this.activateKeyboard(stub.id, stub.langId);
    } else {
      return null;
    }
  }

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