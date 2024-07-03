/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Description: Used for apply core processor actions to
 *              Keyman for Windows engine.
 */
#include "pch.h"

/**
* Adds characters to the queue to be inserted into the application
* @param app           A pointer to the AITIP instance.
* @param outputString  A null-terminated string of characters to insert into the application
*/

static void
processUnicodeChar(AITIP* app, const km_core_usv* outputString) {
  while (*outputString) {
    if (Uni_IsSMP(*outputString)) {
      app->QueueAction(QIT_CHAR, (Uni_UTF32ToSurrogate1(*outputString)));
      app->QueueAction(QIT_CHAR, (Uni_UTF32ToSurrogate2(*outputString)));
    } else {
      app->QueueAction(QIT_CHAR, *outputString);
    }
    outputString++;
  }
}

static void processAlert(AITIP* app) {
  app->QueueAction(QIT_BELL, 0);
}

static void
processBack(AITIP* app, const unsigned int code_points_to_delete, const km_core_usv* delete_context) {
  if (app->IsLegacy()) {
    for (unsigned int i = 0; i < code_points_to_delete; i++) {
      app->QueueAction(QIT_BACK, BK_DEFAULT);
    }
  }
  else {
    km_core_usv const* delete_context_ptr = delete_context;
    while (*delete_context_ptr) {
      delete_context_ptr++;
    }
    delete_context_ptr--;
    for (; delete_context_ptr >= delete_context; delete_context_ptr--) {
      if (Uni_IsSMP(*delete_context_ptr)) {
        app->QueueAction(QIT_BACK, BK_DEFAULT | BK_SURROGATE);
      }
      else {
        app->QueueAction(QIT_BACK, BK_DEFAULT);
      }
    }
  }
}



static void
processPersistOpt(km_core_actions const* actions, LPINTKEYBOARDINFO activeKeyboard
) {
  for (auto option = actions->persist_options; option->key; option++) {
    SendDebugMessageFormat("Saving option to registry for keyboard [%s].", activeKeyboard->Name);
    SaveKeyboardOptionCoretoRegistry(
        activeKeyboard, reinterpret_cast<LPCWSTR>(option->key), reinterpret_cast<LPCWSTR>(option->value));
  }
}

static void processCapsLock(const km_core_caps_state caps_lock_state, BOOL isUp, BOOL Updateable, BOOL externalEvent) {

  // We only want to process the Caps Lock key event once --
  // in the first pass (!Updateable).
  if (Updateable){
    return;
  }

  if (caps_lock_state == KM_CORE_CAPS_ON) {
    // This case would occur for the keyboard system store setting `store(&CapsOnOnly) '1'`
    if (isUp && !IsCapsLockOn()) {  // I267 - 24/11/2006 invert GetKeyState test
      SendDebugMessageFormat("TURN CAPS ON: FIsUp=%d CapsState=%d", isUp, IsCapsLockOn());
      keybd_event(VK_CAPITAL, SCAN_FLAG_KEYMAN_KEY_EVENT, 0, 0);
      keybd_event(VK_CAPITAL, SCAN_FLAG_KEYMAN_KEY_EVENT, KEYEVENTF_KEYUP, 0);
    }

    // This case would occur for the keyboard system store setting `store(&CapsAlwaysOff) '1'`
    // A trick is being played here of synthesising a release the CAPSLOCK key event
    // then a depress CAPSLOCK key event
    else if (!isUp && IsCapsLockOn()) {  // I267 - 24/11/2006 invert GetKeyState test
      SendDebugMessageFormat("TURN CAPS OFF: FIsUp=%d CapsState=%d", isUp, IsCapsLockOn());
      keybd_event(VK_CAPITAL, SCAN_FLAG_KEYMAN_KEY_EVENT, KEYEVENTF_KEYUP, 0);
      keybd_event(VK_CAPITAL, SCAN_FLAG_KEYMAN_KEY_EVENT, 0, 0);
    }
  } else if (caps_lock_state == KM_CORE_CAPS_OFF) {
    // This case would occur for the keyboard system store setting `store(&ShiftFreesCaps) '1'`
    // OR selecting a keyboard with CAPs always off rule
    if ((!isUp && IsCapsLockOn()) || (externalEvent && IsCapsLockOn())) {
      SendDebugMessageFormat("TURN CAPS OFF: FIsUp=%d CapsState=%d", isUp, IsCapsLockOn());
      keybd_event(VK_CAPITAL, SCAN_FLAG_KEYMAN_KEY_EVENT, 0, 0);
      keybd_event(VK_CAPITAL, SCAN_FLAG_KEYMAN_KEY_EVENT, KEYEVENTF_KEYUP, 0);
    }
  }
}

BOOL ProcessActions(BOOL* emitKeystroke)
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) return FALSE;

  km_core_actions const* core_actions = km_core_state_get_actions(_td->lpActiveKeyboard->lpCoreKeyboardState);

  _td->CoreProcessEventRun = FALSE;

  processBack(_td->app, core_actions->code_points_to_delete, core_actions->deleted_context);
  processUnicodeChar(_td->app, core_actions->output);
  if (core_actions->persist_options != NULL) {
    processPersistOpt(core_actions, _td->lpActiveKeyboard);
  }
  if (core_actions->do_alert) {
    processAlert(_td->app);
  }
  if (core_actions->emit_keystroke) {
    *emitKeystroke = TRUE;
  }
  processCapsLock(core_actions->new_caps_lock_state, !_td->state.isDown, _td->TIPFUpdateable, FALSE);

  return TRUE;
}

BOOL
ProcessActionsNonUpdatableParse(BOOL* emitKeystroke) {
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) {
    return FALSE;
  }

  if (_td->TIPFUpdateable) {  // ensure only run when not updateable
    return FALSE;
  }
  _td->CoreProcessEventRun = TRUE;

  km_core_actions const* core_actions = km_core_state_get_actions(_td->lpActiveKeyboard->lpCoreKeyboardState);

  processCapsLock(core_actions->new_caps_lock_state, !_td->state.isDown, _td->TIPFUpdateable, FALSE);
  if (core_actions->emit_keystroke) {
    *emitKeystroke = TRUE;
    SendDebugMessageFormat("EMIT_KEYSTROKE");
    _td->CoreProcessEventRun  = FALSE;  // If we emit the key stroke on this parse we don't need the second parse
  }
  return TRUE;
}

BOOL
ProcessActionsExternalEvent() {
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) {
    return FALSE;
  }
  km_core_actions const* core_actions = km_core_state_get_actions(_td->lpActiveKeyboard->lpCoreKeyboardState);
  processCapsLock(core_actions->new_caps_lock_state, !_td->state.isDown, FALSE, TRUE);
  return TRUE;
}
