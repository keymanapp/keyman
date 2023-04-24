import { type Keyboard, Mock } from '@keymanapp/keyboard-processor';
import { type KeyboardStub } from 'keyman/engine/package-cache';
import { CookieSerializer } from 'keyman/engine/dom-utils';
import { PageContextAttachment } from 'keyman/engine/attachment';
import { DesignIFrame, OutputTarget, nestedInstanceOf } from 'keyman/engine/element-wrappers';
import {
  ContextManagerBase,
  type KeyboardInterface
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

  constructor(engineConfig: BrowserConfiguration) {
    super(engineConfig);

    this.page = new PageContextAttachment(window.document, {
      hostDevice: this.config.hostDevice,
      isTopLevel: true
    });

    this.engineConfig.deferForInitialization.then(() => {
      // TODO: set up attachment-listeners here that can add necessary event-hooks
      // for focus management here!

      this.page.install(this.engineConfig.attachType == 'manual');
    });
  }

  private mostRecentTarget: OutputTarget<any>;
  private currentTarget: OutputTarget<any>;

  private globalKeyboard: {keyboard: Keyboard, metadata: KeyboardStub};

  initialize(): void {
    this.on('keyboardasyncload', (stub, completion) => {
      // TODO:  app/browser - display the loader UI if configured?
      // util.wait('Installing keyboard<br/>' + kbdName);

      completion.then(() => {
        // Cancel the loader UI.
      });
    });

    // TBD:  keyman.domManager.init (the page-integration parts)
    // CTRL+F: `// Exit initialization here if we're using an embedded code path.`
    // EVERYTHING after that block will likely go here - DOMManager's role always
    // was context-management and the facilitation thereof.
    throw new Error('Method not implemented.');
  }

  get activeTarget(): OutputTarget<any> {
    return this.currentTarget;
  }

  get lastActiveTarget(): OutputTarget<any> {
    return this.mostRecentTarget;
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
      _SetTargDir(Ltarg, this.activeKeyboard.keyboard);
    }

    //Execute external (UI) code needed on focus if required
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

  insertText(kbdInterface: KeyboardInterface, Ptext: string, PdeadKey: number) {
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

    if(attachmentInfo?.keyboard) {
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
      attachment.keyboard = kbd.metadata.id;
      attachment.languageCode = kbd.metadata.langId;
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
  public setKeyboardForTarget(target: OutputTarget<any>, metadata: KeyboardStub) {
    if(target instanceof DesignIFrame) {
      console.warn("'keymanweb.setKeyboardForControl' cannot set keyboard on iframes.");
      return;
    }

    let attachment = target.getElement()._kmwAttachment;

    if(!attachment) {
      return;
    } else {
      attachment.keyboard = metadata.id;
      attachment.languageCode = metadata.langId;

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
        // TODO: app/browser - _SetTargDir (within its ContextManager)
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
    var langCode = this.activeKeyboard.metadata.langId;

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
      this.activateKeyboard(global.metadata.id, global.metadata.langId, true);
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

    let activeKeyboard = this.activeKeyboard.keyboard;
    if(!focusAssistant.restoringFocus) {
      if(outputTarget) {
        outputTarget.deadkeys().clear();
      }

      if(activeKeyboard) {
        activeKeyboard.notify(0, outputTarget, 1);  // I2187
      }
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
  _ControlFocus(e: FocusEvent): boolean {
    let Ltarg: HTMLElement = e?.target as HTMLElement;
    if (Ltarg == null) {
      return true;
    }

    // if(Ltarg['body']) {
    //   Ltarg = Ltarg['body']; // Occurs in Firefox for design-mode iframes.
    // }

    if (Ltarg.nodeType == 3) { // defeat Safari bug
      Ltarg = Ltarg.parentNode as HTMLElement;
    }

    // Or if not a remappable input field
    if(nestedInstanceOf(Ltarg, "HTMLInputElement")) {
      const et=(Ltarg as HTMLInputElement).type.toLowerCase();
      if(!(et == 'text' || et == 'search')) {
        return true;
      }
    }

    const target = Ltarg._kmwAttachment.interface;
    if(!target) {
      // Probably should also make a warning or error?
      return true;
    }

    // if(target instanceof DesignIFrame) { //**TODO: check case reference
    //   // Should already have been done...
    //   // attachmentEngine._AttachToIframe(Ltarg as HTMLIFrameElement);
    //   target.docRoot
    //   Ltarg=Ltarg.contentWindow.document.body;
    // }

    this.setActiveTarget(target);

    return true;
  }
}