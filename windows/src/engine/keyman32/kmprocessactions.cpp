/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Description: Used for apply core processor actions to
 *              Keyman for Windows engine.
 */
#include "pch.h"

/**
* Adds converts the characters to UTF-16 and inserts CR as it places them
* in the queue. 
* @param app           A pointer to the AITIP instance.
* @param core_output   A null-terminated string of characters in UTF-32 format to insert into the application
*/

// When the windows platform code is updated to not use QueueAction
// This function would be better to take as input the km_core_usv*
// string form the core and generate a new buffer that is
// windows formated (CRLF) and encoded as UTF-16. Currently it just
// does both in one parse while adding it to the queue to reduce the double
// processing.

//static void
//process_output_string(AITIP* app, const km_core_usv* core_output) {
//  while (*core_output) {
//    if (*core_output == L'\n') {
//      // Insert '\r' for Windows platform applications
//      app->QueueAction(QIT_CHAR, L'\r');
//    }
//    if (Uni_IsSMP(*core_output)) {
//      app->QueueAction(QIT_CHAR, (Uni_UTF32ToSurrogate1(*core_output)));
//      app->QueueAction(QIT_CHAR, (Uni_UTF32ToSurrogate2(*core_output)));
//    } else {
//      app->QueueAction(QIT_CHAR, *core_output);
//    }
//    core_output++;
//  }
//}

static void
process_output_string_2(AITIP* app, LPWSTR win_output) {
  while (*win_output) {
    app->QueueAction(QIT_CHAR, *win_output);
    win_output++;
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
    SendDebugMessageFormat(0, sdmGlobal, 0, "ProcessPersistOpt: Saving option to registry for keyboard [%s].", activeKeyboard->Name);
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
      SendDebugMessageFormat(0, sdmGlobal, 0, "processCapsLock: TURN CAPS ON: FIsUp=%d CapsState=%d", isUp, IsCapsLockOn());
      keybd_event(VK_CAPITAL, SCAN_FLAG_KEYMAN_KEY_EVENT, 0, 0);
      keybd_event(VK_CAPITAL, SCAN_FLAG_KEYMAN_KEY_EVENT, KEYEVENTF_KEYUP, 0);
    }

    // This case would occur for the keyboard system store setting `store(&CapsAlwaysOff) '1'`
    // A trick is being played here of synthesising a release the CAPSLOCK key event
    // then a depress CAPSLOCK key event
    else if (!isUp && IsCapsLockOn()) {  // I267 - 24/11/2006 invert GetKeyState test
      SendDebugMessageFormat(0, sdmGlobal, 0, "processCapsLock: TURN CAPS OFF: FIsUp=%d CapsState=%d", isUp, IsCapsLockOn());
      keybd_event(VK_CAPITAL, SCAN_FLAG_KEYMAN_KEY_EVENT, KEYEVENTF_KEYUP, 0);
      keybd_event(VK_CAPITAL, SCAN_FLAG_KEYMAN_KEY_EVENT, 0, 0);
    }
  } else if (caps_lock_state == KM_CORE_CAPS_OFF) {
    // This case would occur for the keyboard system store setting `store(&ShiftFreesCaps) '1'`
    // OR selecting a keyboard with CAPs always off rule
    if ((!isUp && IsCapsLockOn()) || (externalEvent && IsCapsLockOn())) {
      SendDebugMessageFormat(0, sdmGlobal, 0, "processCapsLock: TURN CAPS OFF: FIsUp=%d CapsState=%d", isUp, IsCapsLockOn());
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
  WCHAR win_out_str[MAXCONTEXT];
  context_char32_char16(core_actions->output, win_out_str, MAXCONTEXT);
  restore_line_breaks(win_out_str, MAXCONTEXT, _td->line_break, lbCRLF);
  //process_output_string(_td->app, core_actions->output);
  process_output_string_2(_td->app, win_out_str);
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
    SendDebugMessageFormat(0, sdmGlobal, 0, "ProcessActionsNonUpdatableParse EMIT_KEYSTROKE");
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
