import { Codes } from "@keymanapp/common-types";
import { DeviceSpec, KeyEvent, KeyMapping, Keyboard, KeyboardProcessor } from '@keymanapp/keyboard-processor';

import { HardKeyboard, processForMnemonicsAndLegacy } from 'keyman/engine/main';
import { DomEventTracker } from 'keyman/engine/events';
import { DesignIFrame, nestedInstanceOf } from 'keyman/engine/element-wrappers';
import { eventOutputTarget, outputTargetForElement } from 'keyman/engine/attachment';

import ContextManager from './contextManager.js';

type KeyboardState = {
  activeKeyboard: Keyboard,
  modStateFlags: number,
  baseLayout: string
}

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
 * Function     _GetKeyEventProperties
 * Scope        Private
 * @param       {Event}       e         Event object
 * @return      {Object.<string,*>}     KMW keyboard event object:
 * Description  Get object with target element, key code, shift state, virtual key state
 *                Lcode=keyCode
 *                Lmodifiers=shiftState
 *                LisVirtualKeyCode e.g. ctrl/alt key
 *                LisVirtualKey     e.g. Virtual key or non-keypress event
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
  var prevModState = keyboardState.modStateFlags, curModState = 0x0000;
  var ctrlEvent = false, altEvent = false;

  let keyCodes = Codes.keyCodes;
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

  curModState |= (e.getModifierState("Shift") ? 0x10 : 0);

  let modifierCodes = Codes.modifierCodes;
  if(e.getModifierState("Control")) {
    curModState |= ((e.location != 0 && ctrlEvent) ?
      (e.location == 1 ? modifierCodes['LCTRL'] : modifierCodes['RCTRL']) : // Condition 1
      prevModState & 0x0003);                                                       // Condition 2
  }
  if(e.getModifierState("Alt")) {
    curModState |= ((e.location != 0 && altEvent) ?
      (e.location == 1 ? modifierCodes['LALT'] : modifierCodes['RALT']) :   // Condition 1
      prevModState & 0x000C);                                                       // Condition 2
  }

  // Stage 2 - detect state key information.  It can be looked up per keypress with no issue.
  let Lstates = 0;

  Lstates |= e.getModifierState('CapsLock') ? modifierCodes['CAPS'] : modifierCodes['NO_CAPS'];
  Lstates |= e.getModifierState('NumLock') ? modifierCodes['NUM_LOCK'] : modifierCodes['NO_NUM_LOCK'];
  Lstates |= (e.getModifierState('ScrollLock'))
    ? modifierCodes['SCROLL_LOCK'] : modifierCodes['NO_SCROLL_LOCK'];

  // We need these states to be tracked as well for proper OSK updates.
  curModState |= Lstates;

  // Stage 3 - Set our modifier state tracking variable and perform basic AltGr-related management.
  const LmodifierChange = keyboardState.modStateFlags != curModState;

  // KeyboardState update:  save our known modifier/state analysis bits.
  // Note:  `keyboardState` is typically the full-fledged KeyboardProcessor instance.  As a result,
  // changes here persist across calls (as we only ever make the one instance).
  keyboardState.modStateFlags = curModState;

  // For European keyboards, not all browsers properly send both key-up events for the AltGr combo.
  let altGrMask = modifierCodes['RALT'] | modifierCodes['LCTRL'];
  if((prevModState & altGrMask) == altGrMask && (curModState & altGrMask) != altGrMask) {
    // We just released AltGr - make sure it's all released.
    curModState &= ~ altGrMask;
  }
  // Perform basic filtering for Windows-based ALT_GR emulation on European keyboards.
  if(curModState & modifierCodes['RALT']) {
    curModState &= ~modifierCodes['LCTRL'];
  }

  let modifierBitmasks = Codes.modifierBitmasks;
  // Stage 4 - map the modifier set to the appropriate keystroke's modifiers.
  const activeKeyboard = keyboardState.activeKeyboard;
  let Lmodifiers: number;
  if(activeKeyboard && activeKeyboard.isChiral) {
    Lmodifiers = curModState & modifierBitmasks.CHIRAL;

    // Note for future - embedding a kill switch here would facilitate disabling AltGr / Right-alt simulation.
    if(activeKeyboard.emulatesAltGr && (Lmodifiers & modifierBitmasks['ALT_GR_SIM']) == modifierBitmasks['ALT_GR_SIM']) {
      Lmodifiers ^= modifierBitmasks['ALT_GR_SIM'];
      Lmodifiers |= modifierCodes['RALT'];
    }
  } else {
    // No need to sim AltGr here; we don't need chiral ALTs.
    Lmodifiers =
      (curModState & 0x10) | // SHIFT
      ((curModState & (modifierCodes['LCTRL'] | modifierCodes['RCTRL'])) ? 0x20 : 0) |
      ((curModState & (modifierCodes['LALT'] | modifierCodes['RALT']))   ? 0x40 : 0);
  }


  /* Tweak the modifiers if an OS meta key is detected; this will allow meta-key-based
    * hotkeys to bypass Keyman processing.  We do this AFTER the chiral modifier filtering
    * because some keyboards specify their own modifierBitmask, which won't include it.
    * We don't currently use that reference in this method, but that may change in the future.
    */
  Lmodifiers |= (e.metaKey ? modifierCodes['META']: 0);

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
    LmodifierChange: LmodifierChange,
    // This is based on a KeyboardEvent, so it's not considered 'synthetic' within web-core.
    isSynthetic: false
  });

  // The 0x6F used to be 0x60 - this adjustment now includes the chiral alt and ctrl modifiers in that check.
  let LisVirtualKeyCode = (typeof e.charCode != 'undefined' && e.charCode != null  &&  (e.charCode == 0 || (Lmodifiers & 0x6F) != 0));
  s.LisVirtualKey = LisVirtualKeyCode || e.type != 'keypress';

  s = processForMnemonicsAndLegacy(s, activeKeyboard, keyboardState.baseLayout);

  let processedEvent = new KeyEvent(s);
  processedEvent.source = e;
  return processedEvent;
}

export default class HardwareEventKeyboard extends HardKeyboard {
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

    const page = contextManager.page;

    const eventTracker = this.domEventTracker;

    page.on('enabled', (Pelem) => {
      const target = outputTargetForElement(Pelem);

      if(!(target instanceof DesignIFrame)) {
        // These need to be on the actual input element, as otherwise the keyboard will disappear on touch.
        eventTracker.attachDOMEvent(Pelem, 'keypress', this._KeyPress);
        eventTracker.attachDOMEvent(Pelem, 'keydown', this._KeyDown);
        eventTracker.attachDOMEvent(Pelem, 'keyup', this._KeyUp);
      } else {
        const Lelem = target.getElement().contentDocument;
        eventTracker.attachDOMEvent(Lelem.body,'keydown', this._KeyDown);
        eventTracker.attachDOMEvent(Lelem.body,'keypress', this._KeyPress);
        eventTracker.attachDOMEvent(Lelem.body,'keyup', this._KeyUp);
      }
    });

    page.on('disabled', (Pelem) => {
      const target = outputTargetForElement(Pelem);

      if(!(target instanceof DesignIFrame)) {
        eventTracker.detachDOMEvent(Pelem, 'keypress', this._KeyPress);
        eventTracker.detachDOMEvent(Pelem, 'keydown', this._KeyDown);
        eventTracker.detachDOMEvent(Pelem, 'keyup', this._KeyUp);
      } else {
        const Lelem = target.getElement().contentDocument;
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
    const activeKeyboard = this.contextManager.activeKeyboard;
    const target = eventOutputTarget(e);

    if(!target || activeKeyboard == null) {
      return true;
    }

    // Prevent mapping element is readonly or tagged as kmw-disabled
    const el = target.getElement();
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
    const target = eventOutputTarget(e);
    if(!target || this.contextManager.activeKeyboard?.keyboard == null) {
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
    const target = eventOutputTarget(e);
    var Levent = preprocessKeyboardEvent(e, this.processor, this.hardDevice);
    if(Levent == null || target == null) {
      return true;
    }

    var inputEle = target.getElement();

    // Since this part concerns DOM element + browser interaction management, we preprocess it for
    // browser form commands before passing control to the Processor module.
    if(Levent.Lcode == 13) {
      var ignore = false;
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
            nextElement.focus();
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
    var Levent = preprocessKeyboardEvent(e, this.processor, this.hardDevice);
    if(Levent == null) {
      return true;
    }

    let resultCapture: { LeventMatched: boolean } = {
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
    var Levent = preprocessKeyboardEvent(e, this.processor, this.hardDevice);
    if(Levent == null) {
      return true;
    }

    let outputTarget = eventOutputTarget(e);
    return this.processor.doModifierPress(Levent, outputTarget, false);
  }

  private keyPress(e: KeyboardEvent): boolean {
    var Levent = preprocessKeyboardEvent(e, this.processor, this.hardDevice);
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

    let resultCapture: { preventDefaultKeystroke?: boolean } = {};

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