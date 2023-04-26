import { DeviceSpec, KeyEvent, Keyboard, ManagedPromise } from '@keymanapp/keyboard-processor';

import { HardKeyboard } from 'keyman/engine/main';
import { eventOutputTarget } from 'keyman/engine/attachment';

import ContextManager from './contextManager.js';
import { nestedInstanceOf } from '../../../../build/engine/element-wrappers/obj/utils.js';

export default class HardwareEventKeyboard extends HardKeyboard {
  private readonly hardDevice: DeviceSpec;
  private readonly contextManager: ContextManager;

  private swallowKeypress: boolean = false;

  constructor(hardDevice: DeviceSpec, contextManager: ContextManager) {
    super();
    this.hardDevice = hardDevice;
    this.contextManager = contextManager;

    const page = contextManager.page;

    page.on('enabled', (elem) => {

    });

    page.off('disabled', (elem) => {

    });
  }

  get activeKeyboard(): Keyboard {
    return this.contextManager.activeKeyboard.keyboard;
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
    if(el?.className?.indexOf('kmw-disabled') >= 0) {
      return true;
    }

    return PreProcessor.keyDown(e);
  }

  /**
   * Function     _KeyPress
   * Scope        Private
   * Description Processes keypress event (does not pass data to keyboard)
   */
  _KeyPress: (e: KeyboardEvent) => boolean = (e) => {
    const target = eventOutputTarget(e);
    if(!target || this.activeKeyboard == null) {
      return true;
    }

    return PreProcessor.keyPress(e);
  }

  /**
   * Function     _KeyUp
   * Scope        Private
   * Description Processes keyup event and passes event data to keyboard
   */
  _KeyUp: (e: KeyboardEvent) => boolean = (e) => {
    const target = eventOutputTarget(e);
    var Levent = PreProcessor._GetKeyEventProperties(e, false);
    if(Levent == null) {
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

    return PreProcessor.keyUp(e);
  }

  // TODO:  actually implement
}