import {
  ContextManager as ContextManagerBase,
  type KeyboardInterface
} from 'keyman/engine/main';

export default class ContextManager extends ContextManagerBase {
  insertText(kbdInterface: KeyboardInterface, Ptext: string, PdeadKey: number) {
    // Find the correct output target to manipulate.
    const outputTarget = this.activeTarget;

    if(outputTarget != null) {
      // While not yet fully connected, ContextManager and its subclasses will be responsible for maintaining
      // active elements & their focus... something that should only really matter for the website-integrating
      // subclass.

      // Required for the `sil_euro_latin` keyboard's desktop OSK/table to function properly.
      keyman.uiManager.setActivatingUI(true);
      dom.DOMEventHandlers.states._IgnoreNextSelChange = 100;
      keyman.domManager.focusLastActiveElement();
      dom.DOMEventHandlers.states._IgnoreNextSelChange = 0;

      return super.insertText(kbdInterface, Ptext, PdeadKey);
    }
    return false;
  }
}