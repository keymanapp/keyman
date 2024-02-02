/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Description: Used for apply core processor actions to
 *              Keyman for Windows engine.
 */
#include "pch.h"

static void
processUnicodeChar(AITIP* app, const km_core_usv* actionItem) {
  while (*actionItem) {
    if (Uni_IsSMP(*actionItem)) {
      app->QueueAction(QIT_CHAR, (Uni_UTF32ToSurrogate1(*actionItem)));
      app->QueueAction(QIT_CHAR, (Uni_UTF32ToSurrogate2(*actionItem)));
    } else {
      app->QueueAction(QIT_CHAR, *actionItem);
    }
    actionItem++;
  }
}

static void processAlert(AITIP* app) {
  app->QueueAction(QIT_BELL, 0);
}

static BOOL
processBack(AITIP* app, const unsigned int code_points_to_delete, const km_core_usv* delete_context) {
  if (app->IsLegacy()) {
    SendDebugMessageFormat(0, sdmGlobal, 0, "processBack: Legacy app cptd [%d].", code_points_to_delete);
    for (unsigned int i = 0; i < code_points_to_delete; i++) {
      app->QueueAction(QIT_BACK, BK_DEFAULT);
    }
    return TRUE;
  }
  if (!app->IsLegacy()) {
    SendDebugMessageFormat(0, sdmGlobal, 0, "processBack: TSF app cptd [%d].", code_points_to_delete);
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
    return TRUE;
  }
  return FALSE;
}



static void
processPersistOpt(km_core_actions const* actions, LPINTKEYBOARDINFO activeKeyboard
) {
  for (auto option = actions->persist_options; option->key; option++) {
    // Put the keyboard option into Windows Registry
    // log"Saving keyboard option to registry");
    SendDebugMessageFormat(0, sdmGlobal, 0, "ProcessPersistOpt: Saving option to registry for keyboard [%s].", activeKeyboard->Name);
    size_t value_length = wcslen(reinterpret_cast<LPCWSTR>(option->value));
    LPWSTR value = new WCHAR[value_length + 1];
    wcscpy_s(value, value_length + 1, reinterpret_cast<LPCWSTR>(option->value));
    SaveKeyboardOptionCoretoRegistry(activeKeyboard, reinterpret_cast<LPCWSTR>(option->key), value);
    delete[] value;
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

BOOL ProcessActions(BOOL* emitKeyStroke)
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) return FALSE;
  SendDebugMessageFormat(0, sdmGlobal, 0, "ProcessActions: Enter");

  // TODO: #10583  Remove caching action_struct
  if (!_td->core_actions) {
    SendDebugMessageFormat(0, sdmGlobal, 0, "ProcessActions: core_actions not set");
    _td->core_actions = km_core_state_get_actions(_td->lpActiveKeyboard->lpCoreKeyboardState);
  }

  _td->CoreProcessEventRun = FALSE;



  processBack(_td->app, _td->core_actions->code_points_to_delete, _td->core_actions->deleted_context);
  processUnicodeChar(_td->app, _td->core_actions->output);
  if (_td->core_actions->persist_options != NULL) {
    processPersistOpt(_td->core_actions, _td->lpActiveKeyboard);
  }
  if (_td->core_actions->do_alert) {
    processAlert(_td->app);
  }
  if (_td->core_actions->emit_keystroke) {
    *emitKeyStroke = TRUE;
  }
  processCapsLock(_td->core_actions->new_caps_lock_state, !_td->state.isDown, _td->TIPFUpdateable, FALSE);
  // TODO: #10583 remove dispose
  km_core_actions_dispose(_td->core_actions);
  _td->core_actions = nullptr;

  return TRUE;
}

BOOL
ProcessActionsNonUpdatableParse(BOOL* emitKeyStroke) {
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) {
    return FALSE;
  }
  SendDebugMessageFormat(0, sdmGlobal, 0, "ProcessActionsNonUpdatableParse: Enter");
  if (_td->TIPFUpdateable) {  // ensure only run when not updateable
    return FALSE;
  }
  _td->CoreProcessEventRun = TRUE;
  // TODO: #10583 remove dispose
  if (_td->core_actions) {
    km_core_actions_dispose(_td->core_actions);
    _td->core_actions = nullptr;
  }

  _td->core_actions   = km_core_state_get_actions(_td->lpActiveKeyboard->lpCoreKeyboardState);
  SendDebugMessageFormat(0, sdmGlobal, 0, "ProcessActionsNonUpdatableParse: km_core_state_get_actions");
  processCapsLock(_td->core_actions->new_caps_lock_state, !_td->state.isDown, _td->TIPFUpdateable, FALSE);
  if (_td->core_actions->emit_keystroke) {
    *emitKeyStroke = TRUE;
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
  if (!_td->core_actions) {   // when ideponent we will not need this
    return FALSE;
  }
  // TODO: #10583 remove dispose
  //km_core_actions const* acts = km_core_state_get_actions(_td->lpActiveKeyboard->lpCoreKeyboardState);
  processCapsLock(_td->core_actions->new_caps_lock_state, !_td->state.isDown, FALSE, TRUE);
  return TRUE;
}
