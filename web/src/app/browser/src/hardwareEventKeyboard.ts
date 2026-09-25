import { DeviceSpec } from 'keyman/common/web-utils';
import { Codes, KeyEvent, KeyMapping, KeyboardProcessor, Keyboard } from 'keyman/engine/keyboard';
import { ModifierKeyConstants } from '@keymanapp/common-types';

import { HardKeyboardBase, processForMnemonicsAndLegacy } from 'keyman/engine/main';
import { DomEventTracker } from 'keyman/engine/events';
import { DesignIFrameElementTextStore, nestedInstanceOf } from 'keyman/engine/element-text-stores';
import { textStoreForEvent, textStoreForElement } from 'keyman/engine/attachment';

import { ContextManager } from './contextManager.js';

type KeyboardState = {
  activeKeyboard: Keyboard,
  modStateFlags: number,
  baseLayout: string
}

const DOM_KEY_LOCATION = {
  STANDARD: 0,
  LEFT: 1,
  RIGHT: 2,
};

// Important:  the following two lines should not cause a compile error if left uncommented.
// let dummy1: KeyboardProcessor;
// let dummy2: KeyboardState = dummy1;

/**
 * Function     _GetEventKeyCode
 * Scope        Private
 * @param       {Event}       e         Event object
 * Description  Finds the key code represented by the event.
 */
export function _GetEventKeyCode(e: KeyboardEvent) {
  if (e.keyCode) {
    return e.keyCode;
  } else if (e.which) {
    return e.which;
  } else {
    return null;
  }
}

// Keeping this as a separate function affords us the opportunity to unit-test the method more simply.

/**
 * Translate a browser KeyboardEvent into a KeymanWeb KeyEvent
 *
 * The function calculates the true state of the keyboard's modifiers, taking
 * into account chiral modifiers where applicable.
 * Special handling is included for AltGr emulation and browser quirks, particularly
 * for Firefox, which may use different key codes for certain keys. The code remaps
 * these as needed for consistency. It adds the OS meta key if pressed, ensuring
 * that system shortcuts can bypass Keyman processing. Adjustments are made for
 * mnemonic and legacy keyboards, before finally returning a KeyEvent object
 * with the computed key code, modifiers and state.
 *
 * @param  {KeyboardEvent}  e              Event object
 * @param  {KeyboardState}  keyboardState  Keyboard state object
 * @param  {DeviceSpec}     device         Device object
 *
 * @return {KeyEvent}       KeymanWeb KeyEvent object, or null for duplicate/spurious
 *                          events or if there is no key code.
 */
export function preprocessKeyboardEvent(e: KeyboardEvent, keyboardState: KeyboardState, device: DeviceSpec): KeyEvent {
  if(e.cancelBubble === true) {
    return null; // I2457 - Facebook meta-event generation mess -- two events generated for a keydown in Facebook contentEditable divs
  }

  let Lcode = _GetEventKeyCode(e);
  if (Lcode == null) {
    return null;
  }

  // Stage 1 - track the true state of the keyboard's modifiers.
  const prevModState = keyboardState.modStateFlags;
  let ctrlEvent = false, altEvent = false;

  const keyCodes = Codes.keyCodes;
  switch(Lcode) {
    case keyCodes['K_CTRL']:      // The 3 shorter "K_*CTRL" entries exist in some legacy keyboards.
    case keyCodes['K_LCTRL']:
    case keyCodes['K_RCTRL']:
    case keyCodes['K_CONTROL']:
    case keyCodes['K_LCONTROL']:
    case keyCodes['K_RCONTROL']:
      ctrlEvent = true;
      break;
    case keyCodes['K_LMENU']:     // The 2 "K_*MENU" entries exist in some legacy keyboards.
    case keyCodes['K_RMENU']:
    case keyCodes['K_ALT']:
    case keyCodes['K_LALT']:
    case keyCodes['K_RALT']:
      altEvent = true;
      break;
  }

  /**
   * Two separate conditions exist that should trigger chiral modifier detection.  Examples below use CTRL but also work for ALT.
   *
   * 1.  The user literally just pressed CTRL, so the event has a valid `location` property we can utilize.
   *     Problem: its layer isn't presently activated within the OSK.
   *
   * 2.  CTRL has been held a while, so the OSK layer is valid, but the key event doesn't tell us the chirality of the active CTRL press.
   *     Bonus issue:  RAlt simulation may cause erasure of this location property, but it should ONLY be empty if pressed in this case.
   *     We default to the 'left' variants since they're more likely to exist and cause less issues with RAlt simulation handling.
   *
   * In either case, `e.getModifierState("Control")` is set to true, but as a result does nothing to tell us which case is active.
   *
   * `e.location != 0` if true matches condition 1 and matches condition 2 if false.
   */

  let curModState = 0x0000;
  curModState |= (e.getModifierState("Shift") ? ModifierKeyConstants.K_SHIFTFLAG : 0);

  if(e.getModifierState("Control")) {
    curModState |= ((e.location != DOM_KEY_LOCATION.STANDARD && ctrlEvent)
      ? (e.location == DOM_KEY_LOCATION.LEFT
        ? ModifierKeyConstants.LCTRLFLAG
        : ModifierKeyConstants.RCTRLFLAG) // Condition 1
      : prevModState & (ModifierKeyConstants.LCTRLFLAG | ModifierKeyConstants.RCTRLFLAG)); // Condition 2
  }
  if(e.getModifierState("Alt")) {
    curModState |= ((e.location != DOM_KEY_LOCATION.STANDARD && altEvent)
      ? (e.location == DOM_KEY_LOCATION.LEFT
        ? ModifierKeyConstants.LALTFLAG
        : ModifierKeyConstants.RALTFLAG)   // Condition 1
      : prevModState & (ModifierKeyConstants.LALTFLAG | ModifierKeyConstants.RALTFLAG));  // Condition 2
  }

  // Stage 2 - detect state key information.  It can be looked up per keypress with no issue.
  let Lstates = 0;

  Lstates |= e.getModifierState('CapsLock') ? ModifierKeyConstants.CAPITALFLAG : ModifierKeyConstants.NOTCAPITALFLAG;
  Lstates |= e.getModifierState('NumLock') ? ModifierKeyConstants.NUMLOCKFLAG : ModifierKeyConstants.NOTNUMLOCKFLAG;
  Lstates |= (e.getModifierState('ScrollLock'))
    ? ModifierKeyConstants.SCROLLFLAG : ModifierKeyConstants.NOTSCROLLFLAG;

  // We need these states to be tracked as well for proper OSK updates.
  curModState |= Lstates;

  // Stage 3 - Set our modifier state tracking variable and perform basic AltGr-related management.
  const LmodifierChanged = keyboardState.modStateFlags != curModState;

  // KeyboardState update:  save our known modifier/state analysis bits.
  // Note:  `keyboardState` is typically the full-fledged KeyboardProcessor instance.  As a result,
  // changes here persist across calls (as we only ever make the one instance).
  keyboardState.modStateFlags = curModState;

  // For European keyboards, not all browsers properly send both key-up events for the AltGr combo.
  const altGrMask = ModifierKeyConstants.RALTFLAG | ModifierKeyConstants.LCTRLFLAG;
  if((prevModState & altGrMask) == altGrMask && (curModState & altGrMask) != altGrMask) {
    // We just released AltGr - make sure it's all released.
    curModState &= ~ altGrMask;
  }
  // Perform basic filtering for Windows-based ALT_GR emulation on European keyboards.
  if((curModState & ModifierKeyConstants.RALTFLAG) != 0) {
    curModState &= ~ModifierKeyConstants.LCTRLFLAG;
  }

  // Stage 4 - map the modifier set to the appropriate keystroke's modifiers.
  const modifierBitmasks = Codes.modifierBitmasks;
  const activeKeyboard = keyboardState.activeKeyboard;
  let Lmodifiers: number;
  if(activeKeyboard?.isChiral) {
    Lmodifiers = curModState & modifierBitmasks.CHIRAL;

    // Note for future - embedding a kill switch here would facilitate disabling AltGr / Right-alt simulation.
    if(activeKeyboard.emulatesAltGr && (Lmodifiers & modifierBitmasks['ALT_GR_SIM']) == modifierBitmasks['ALT_GR_SIM']) {
      Lmodifiers ^= modifierBitmasks['ALT_GR_SIM'];
      Lmodifiers |= ModifierKeyConstants.RALTFLAG;
    }
  } else {
    // No need to sim AltGr here; we don't need chiral ALTs.
    Lmodifiers =
      (curModState & ModifierKeyConstants.K_SHIFTFLAG) |
      ((curModState & (ModifierKeyConstants.LCTRLFLAG | ModifierKeyConstants.RCTRLFLAG)) != 0 ? ModifierKeyConstants.K_CTRLFLAG : 0) |
      ((curModState & (ModifierKeyConstants.LALTFLAG | ModifierKeyConstants.RALTFLAG)) != 0 ? ModifierKeyConstants.K_ALTFLAG : 0);
  }


  /* Tweak the modifiers if an OS meta key is detected; this will allow meta-key-based
    * hotkeys to bypass Keyman processing.  We do this AFTER the chiral modifier filtering
    * because some keyboards specify their own modifierBitmask, which won't include it.
    * We don't currently use that reference in this method, but that may change in the future.
    */
  Lmodifiers |= (e.metaKey ? ModifierKeyConstants.K_METAFLAG: 0);

  // Physically-typed keys require use of a 'desktop' form factor and thus are based on a virtual "physical" Device.

  // Perform any browser-specific key remapping before other remaps and mnemonic transforms.
  // (See https://github.com/keymanapp/keyman/issues/1125.)
  if(device.browser == DeviceSpec.Browser.Firefox) {
  // Browser key identifiers are not completely consistent; Firefox has a few (for US punctuation)
  // that differ from the norm.  Refer to https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/keyCode.
    if(KeyMapping.browserMap.FF['k'+Lcode]) {
      Lcode = KeyMapping.browserMap.FF['k'+Lcode];
    }
  }

  // We now have enough properties to properly specify a KeyEvent object.
  let s = new KeyEvent({
    device: device,
    kName: '',
    Lcode: Lcode,
    Lmodifiers: Lmodifiers,
    Lstates: Lstates,
    LmodifierChange: LmodifierChanged,
    // This is based on a KeyboardEvent, so it's not considered 'synthetic' within web-core.
    isSynthetic: false
  });

  // The 0x6F used to be 0x60 - this adjustment now includes the chiral alt and ctrl modifiers in that check.
  const LisVirtualKeyCode = (typeof e.charCode != 'undefined' && e.charCode != null  &&  (e.charCode == 0 || (Lmodifiers & 0x6F) != 0));
  s.LisVirtualKey = LisVirtualKeyCode || e.type != 'keypress';

  s = processForMnemonicsAndLegacy(s, activeKeyboard, keyboardState.baseLayout);

  const processedEvent = new KeyEvent(s);
  processedEvent.source = e;
  return processedEvent;
}

export class HardwareEventKeyboard extends HardKeyboardBase {
  private readonly hardDevice: DeviceSpec;

  // Needed properties & methods:
  // - `modStateFlags`
  // - `baseLayout`
  // - `doModifierPress()` - for modifier updates on key-up.
  private readonly processor: KeyboardProcessor;
  private readonly contextManager: ContextManager;
  private domEventTracker = new DomEventTracker();

  private swallowKeypress: boolean = false;

  constructor(hardDevice: DeviceSpec, processor: KeyboardProcessor, contextManager: ContextManager) {
    super();
    this.hardDevice = hardDevice;
    this.contextManager = contextManager;
    this.processor = processor;

    const {page} = contextManager;

    const eventTracker = this.domEventTracker;

    page.on('enabled', (Pelem) => {
      const textStore = textStoreForElement(Pelem);

      if(!(textStore instanceof DesignIFrameElementTextStore)) {
        // These need to be on the actual input element, as otherwise the keyboard will disappear on touch.
        eventTracker.attachDOMEvent(Pelem, 'keypress', this._KeyPress);
        eventTracker.attachDOMEvent(Pelem, 'keydown', this._KeyDown);
        eventTracker.attachDOMEvent(Pelem, 'keyup', this._KeyUp);
      } else {
        const Lelem = textStore.getElement().contentDocument;
        eventTracker.attachDOMEvent(Lelem.body,'keydown', this._KeyDown);
        eventTracker.attachDOMEvent(Lelem.body,'keypress', this._KeyPress);
        eventTracker.attachDOMEvent(Lelem.body,'keyup', this._KeyUp);
      }
    });

    page.on('disabled', (Pelem) => {
      const textStore = textStoreForElement(Pelem);

      if(!(textStore instanceof DesignIFrameElementTextStore)) {
        eventTracker.detachDOMEvent(Pelem, 'keypress', this._KeyPress);
        eventTracker.detachDOMEvent(Pelem, 'keydown', this._KeyDown);
        eventTracker.detachDOMEvent(Pelem, 'keyup', this._KeyUp);
      } else {
        const Lelem = textStore.getElement().contentDocument;
        eventTracker.detachDOMEvent(Lelem.body,'keydown', this._KeyDown);
        eventTracker.detachDOMEvent(Lelem.body,'keypress', this._KeyPress);
        eventTracker.detachDOMEvent(Lelem.body,'keyup', this._KeyUp);
      }
    });
  }

  /**
   * Function     _KeyDown
   * Scope        Private
   * Description  Processes keydown event and passes data to keyboard.
   *
   * Note that the test-case oriented 'recorder' stubs this method to facilitate keystroke
   * recording for use in test cases.  If changing this function, please ensure the recorder is
   * not affected.
   */
  _KeyDown: (e: KeyboardEvent) => boolean = (e) => {
    const {activeKeyboard} = this.contextManager;
    const textStore = textStoreForEvent(e);

    if(!textStore || activeKeyboard == null) {
      return true;
    }

    // Prevent mapping element is readonly or tagged as kmw-disabled
    const el = textStore.getElement();
    if(el?.getAttribute('class')?.indexOf('kmw-disabled') >= 0) {
      return true;
    }

    return this.keyDown(e);
  }

  /**
   * Function     _KeyPress
   * Scope        Private
   * Description Processes keypress event (does not pass data to keyboard)
   */
  _KeyPress: (e: KeyboardEvent) => boolean = (e) => {
    const textStore = textStoreForEvent(e);
    if(!textStore || this.contextManager.activeKeyboard?.keyboard == null) {
      return true;
    }

    return this.keyPress(e);
  }

  /**
   * Function     _KeyUp
   * Scope        Private
   * Description Processes keyup event and passes event data to keyboard
   */
  _KeyUp: (e: KeyboardEvent) => boolean = (e) => {
    const textStore = textStoreForEvent(e);
    const Levent = preprocessKeyboardEvent(e, this.processor, this.hardDevice);
    if(Levent == null || textStore == null) {
      return true;
    }

    const inputEle = textStore.getElement();

    // Since this part concerns DOM element + browser interaction management, we preprocess it for
    // browser form commands before passing control to the Processor module.
    if(Levent.Lcode == 13) {
      let ignore = false;
      if(nestedInstanceOf(inputEle, "HTMLTextAreaElement")) {
        ignore = true;
      }

      if(!ignore) {
        // For input fields, move to next input element
        if(inputEle instanceof inputEle.ownerDocument.defaultView.HTMLInputElement) {
          if(inputEle.form && (inputEle.type == 'search' || inputEle.type == 'submit')) {
            inputEle.form.submit();
          } else {
            const nextElement = this.contextManager.page.findNeighboringInput(inputEle, false);
            nextElement?.focus();
          }
        }
        return true;
      }
    }

    return this.keyUp(e);
  }

  /**
   * Function     keyDown
   * Scope        Public
   * Description  Processes keydown event and passes data to keyboard.
   *
   * Note that the test-case oriented 'recorder' stubs this method to facilitate keystroke
   * recording for use in test cases.  If changing this function, please ensure the recorder is
   * not affected.
   */
  private keyDown(e: KeyboardEvent): boolean {
    this.swallowKeypress = false;

    // Get event properties
    const Levent = preprocessKeyboardEvent(e, this.processor, this.hardDevice);
    if(Levent == null) {
      return true;
    }

    const resultCapture: { LeventMatched: boolean } = {
      LeventMatched: false
    }

    // Is synchronous.
    this.emit('keyevent', Levent, (ruleBehavior, error) => {
      resultCapture.LeventMatched = ruleBehavior && !ruleBehavior.triggerKeyDefault;

      if(resultCapture.LeventMatched) {
        if(e  &&  e.preventDefault) {
          e.preventDefault();
          e.stopPropagation();
        }

        this.swallowKeypress = !!Levent.Lcode;
        // Don't swallow backspaces on keypresses; this allows physical BKSP presses to repeat.
        if(Levent.Lcode == 8) {
          this.swallowKeypress = false;
        }
      } else {
        this.swallowKeypress = false;
      }
    });

    return !resultCapture.LeventMatched;
  }

  // KeyUp basically exists for two purposes:
  // 1)  To detect browser form submissions (handled in kmwdomevents.ts)
  // 2)  To detect modifier state changes.
  private keyUp(e: KeyboardEvent): boolean {
    const Levent = preprocessKeyboardEvent(e, this.processor, this.hardDevice);
    if(Levent == null) {
      return true;
    }

    const textStore = textStoreForEvent(e);
    return this.processor.doModifierPress(Levent, textStore, false);
  }

  private keyPress(e: KeyboardEvent): boolean {
    const Levent = preprocessKeyboardEvent(e, this.processor, this.hardDevice);
    if(Levent == null || Levent.LisVirtualKey) {
      return true;
    }

    // _Debug('KeyPress code='+Levent.Lcode+'; Ltarg='+Levent.Ltarg.tagName+'; LisVirtualKey='+Levent.LisVirtualKey+'; _KeyPressToSwallow='+keymanweb._KeyPressToSwallow+'; keyCode='+(e?e.keyCode:'nothing'));

    /* I732 START - 13/03/2007 MCD: Swedish: Start positional keyboard layout code: prevent keystroke */
    if(!this.contextManager.activeKeyboard?.keyboard.isMnemonic) {
      if(!this.swallowKeypress) {
        return true;
      }
      if(Levent.Lcode < 0x20 || (this.hardDevice.browser == DeviceSpec.Browser.Safari && (Levent.Lcode > 0xF700  &&  Levent.Lcode < 0xF900))) {
        return true;
      }

      return false;
    }
    /* I732 END - 13/03/2007 MCD: Swedish: End positional keyboard layout code */

    // Only reached if it's a mnemonic keyboard.

    const resultCapture: { preventDefaultKeystroke?: boolean } = {};

    // Should only be run if `preventDefaultKeystroke` is required by the following conditional
    // block.  If it isn't - that is, swallowKeypress == true, we want to swallow that keypress
    // interpretation as well by _not_ evaluating it during this pass.
    if(!this.swallowKeypress) {
      // is synchronous
      this.emit('keyevent', Levent, (result, error) => {
        resultCapture.preventDefaultKeystroke = !!result;
      })
    }

    // If we actively prevented a keystroke or if we processed one successfully,
    // prevent the browser from producing its default text output for the event.
    if(this.swallowKeypress || resultCapture.preventDefaultKeystroke) {
      this.swallowKeypress = false;
      if(e && e.preventDefault) {
        e.preventDefault();
        e.stopPropagation();
      }
      return false;
    }

    this.swallowKeypress = false;
    return true;
  }

  shutdown() {
    this.domEventTracker.shutdown();
  }
}