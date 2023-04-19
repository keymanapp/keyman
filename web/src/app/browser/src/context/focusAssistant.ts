/**
 * The return object documented for
 * https://help.keyman.com/DEVELOPER/ENGINE/WEB/16.0/reference/core/getUIState.
 *
 * As it has long been documented in this format, property names should not be adjusted!
 */
export class FocusStateAPIObject {
  /**
   * Indicates that KMW is in a state of actively restoring focus to a previously-active element.
   */
  activated: boolean;

  /**
   * Indicates that KMW is actively maintaining focus on the currently active OutputTarget control
   * while some UI element (the OSK, a keyboard-change UI) is the current focus of user-interaction.
   */
  activationPending: boolean;

  constructor(pending: boolean, activated: boolean) {
    this.activationPending = pending;
    this.activated = activated;
  }
}

// Formerly handled under "UIManager".
/**
 * This class provides fields and methods useful for assisting context management.  Control focus (and
 * thus, activation of the corresponding OutputTarget) should not be lost to non-context components of
 * KMW, such as the OSK or a keyboard selector.
 */
export class FocusAssistant {
  /*
   * Long-term idea here: about all of the relevant OSK events that would interact with this have "enter" and
   * "leave" variants - we could take a stack of `Promise`s.  On a `Promise` fulfillment, remove it from the
   * stack.  When the last one is removed, the focus-maintenance state would end, allowing further events
   * to deactivate the active OutputTarget.
   */

  /**
   * Indicates that KMW is actively maintaining focus on the currently active OutputTarget control, rather
   * than losing focus while some UI element (the OSK, a keyboard-change UI) is the most direct recipient
   * of browser focus due to user-interaction - generally, with non-context engine components.
   *
   * While the flag is active, the context-management system should not deactivate an OutputTarget upon
   * its element's loss of focus within the page unless setting a different OutputTarget as active.
   */
  isActivating: boolean = false;    // ActivatingKeymanWebUI - Does the OSK have active focus / an active interaction?

  /*
   * Long-term idea here:  as (aside from OSK title/resize bar interactions) it's always used to actively
   * RESTORE focus, taking in a closure to perform during the 'focus restoration state' would make a nice
   * design.  Something like .performRefocus(closure: () => void):
   * - would set this field before calling the closure
   * - would unset this field after calling the closure
   * - would try-catch to guarantee the 'unset'.
   *
   * OSK drag handlers should utilize the other field, anyway.
   */

  /**
   * Indicates that KMW is in a state of actively restoring focus to a previously-active element.
   * This is most commonly utilized whenever a keyboard is newly activated, generally due to
   * user interaction to select the new keyboard.
   */
  justActivated: boolean = false;   // JustActivatedKeymanWebUI - focussing back to control after KeymanWeb UI interaction

  constructor() {
  }

  /**
   * Function     getUIState
   * Scope        Public
   * @return      {Object.<string,boolean>}
   * Description  Return object with activation state of UI:
   *                activationPending (bool):   KMW being activated
   *                activated         (bool):   KMW active
   */
  getUIState(): FocusStateAPIObject {
    return new FocusStateAPIObject(this.isActivating, this.justActivated);
  }

  /**
   * Set or clear the IsActivatingKeymanWebUI flag (exposed function)
   *
   * @param       {(boolean|number)}  state  Activate (true,false)
   */
  setActivatingUI(state: boolean) {
    this.isActivating = state ? true : false;
  }
}