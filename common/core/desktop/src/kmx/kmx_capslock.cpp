/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include <keyman/keyboardprocessor_consts.h>
#include <kmx/kmx_processevent.h>

using namespace km::kbp;
using namespace kmx;

/**
 * Set caps lock state.
 * Called by ProcessEvent.
 * May update `modifiers` to add or remove `CAPITALFLAG` according to capsLockOn
 * parameter. May queue actions to set caps lock state.
 *
 * @param[in,out] modifiers   The modifier key bitmap
 * @param         capsLockOn  The desired caps lock state
 */
void KMX_ProcessEvent::SetCapsLock(KMX_DWORD &modifiers, KMX_BOOL capsLockOn) {
  KMX_BOOL capsLockCurrentlyOn = IsCapsLockOn(modifiers);
  if (capsLockCurrentlyOn == capsLockOn) {
    return;
  }

  DebugLog("Caps lock is %s, switching %s", capsLockCurrentlyOn ? "on" : "off", capsLockOn ? "on" : "off");

  m_actions.QueueAction(QIT_CAPSLOCK, capsLockOn);
  if (capsLockOn) {
    modifiers |= CAPITALFLAG;
  } else {
    modifiers &= ~CAPITALFLAG;
  }
}

/**
 * Deal with CapsAlwaysOff store option and turns caps lock off if necessary.
 * Called by ProcessEvent.
 * May update `modifiers` to add or remove `CAPITALFLAG` according to keyboard
 * requirements. May queue actions to set caps lock state.
 *
 * @param[in,out]  modifiers    The modifier key bitmap
 */
void KMX_ProcessEvent::ResetCapsLock(KMX_DWORD &modifiers) {
  if (m_keyboard.Keyboard->dwFlags & KF_CAPSALWAYSOFF) {
    DebugLog("ResetCapsLock: caps lock should be always off");
    SetCapsLock(modifiers, FALSE);
  }
}

/**
 * Deal with CapsLock store options on CapsLock key press. Called by ProcessEvent.
 * May update `modifiers` to add or remove `CAPITALFLAG` according to keyboard
 * requirements. May queue actions to set caps lock state.
 *
 * @param[in,out]  modifiers    The modifier key bitmap
 * @param          isKeyDown    TRUE if this is called on KeyDown event, FALSE if
 *                              called on KeyUp event
 */
void KMX_ProcessEvent::KeyCapsLockPress(KMX_DWORD &modifiers, KMX_BOOL isKeyDown) {
  if (m_keyboard.Keyboard->dwFlags & KF_CAPSONONLY && !isKeyDown) {
    SetCapsLock(modifiers, TRUE);
  } else if (m_keyboard.Keyboard->dwFlags & KF_CAPSALWAYSOFF && isKeyDown) {
    SetCapsLock(modifiers, FALSE);
  }
}

/**
 * Deal with CapsLock store options on Shift key press. Called by ProcessEvent.
 * May update `modifiers` to remove `CAPITALFLAG` according to keyboard
 * requirements. May queue actions to reset caps lock state.
 *
 * @param[in,out]  modifiers    The modifier key bitmap
 * @param          isKeyDown    TRUE if this is called on KeyDown event, FALSE if
 *                              called on KeyUp event
 */
void KMX_ProcessEvent::KeyShiftPress(KMX_DWORD &modifiers, KMX_BOOL isKeyDown) {
  if (m_keyboard.Keyboard->dwFlags & KF_SHIFTFREESCAPS && isKeyDown) {
    SetCapsLock(modifiers, FALSE);
  }
}
