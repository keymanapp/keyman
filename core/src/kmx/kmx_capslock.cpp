/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include <keyman/keyman_core_api_consts.h>
#include <kmx/kmx_processevent.h>

using namespace km::core;
using namespace kmx;

/**
 * Set caps lock state.
 * Called by ProcessEvent.
 * May update `modifiers` to add or remove `CAPITALFLAG` according to capsLockOn
 * parameter. May queue actions to set caps lock state.
 *
 * @param[in,out] modifiers   The modifier key bitmap
 * @param         capsLockOn  The desired caps lock state
 * @param         force       Set caps lock state even if modifiers already show correct state
 */
void KMX_ProcessEvent::SetCapsLock(KMX_DWORD &modifiers, KMX_BOOL capsLockOn, KMX_BOOL force) {
  KMX_BOOL capsLockCurrentlyOn = IsCapsLockOn(modifiers);
  if (capsLockCurrentlyOn == capsLockOn && !force) {
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
 * @param          isKeyDown    TRUE if this is called on KeyDown event, FALSE if
 *                              called on KeyUp event
 */
void KMX_ProcessEvent::ResetCapsLock(KMX_DWORD &modifiers, KMX_BOOL isKeyDown) {
  if (m_keyboard.Keyboard->dwFlags & KF_SHIFTFREESCAPS && modifiers & K_SHIFTFLAG && !isKeyDown) {
    SetCapsLock(modifiers, FALSE);
  } else if (m_keyboard.Keyboard->dwFlags & KF_CAPSALWAYSOFF) {
    DebugLog("ResetCapsLock: caps lock should be always off");
    SetCapsLock(modifiers, FALSE, TRUE);
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
 * @return TRUE to skip further processing, FALSE to continue with normal key handling
 */
KMX_BOOL KMX_ProcessEvent::KeyCapsLockPress(KMX_DWORD &modifiers, KMX_BOOL isKeyDown) {
  if (m_keyboard.Keyboard->dwFlags & KF_CAPSONONLY) {
    // It seems wrong to do this in KeyUp since the system turns capslock on in KeyDown, and
    // for CapsAlwaysOff we turn it off in KeyDown as well. However, changing to KeyDown
    // here doesn't work: capslock still turns off even if we tell it otherwise.
    if (!isKeyDown)
      SetCapsLock(modifiers, TRUE, TRUE);
    return TRUE;
  } else if (m_keyboard.Keyboard->dwFlags & KF_CAPSALWAYSOFF) {
    if (isKeyDown)
      SetCapsLock(modifiers, FALSE, TRUE);
    return TRUE;
  }
  return FALSE;
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
