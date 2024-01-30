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
processBack(AITIP* app, const unsigned int code_points_to_delete) {

  if (app->IsLegacy()) {
    for (unsigned int i = 0; i < code_points_to_delete; i++) {
        app->QueueAction(QIT_BACK, BK_DEFAULT);
    }
    return TRUE;
  }

  if (!app->IsLegacy()) {
    WCHAR application_context[MAXCONTEXT];
    DWORD cp_to_delete = code_points_to_delete;
    if (!app->ReadContext(application_context)) {
      SendDebugMessageFormat(0, sdmGlobal, 0, "processBack: Error reading context from application.");
    } else {
      // Find the length of the string
      int length = 0;
      while (application_context[length] != L'\0') {
        length++;
      }
      // Read each character starting from caret
      for (int i = length - 1; i >= 0 && cp_to_delete > 0; i--) {
        // Need to consider malformed context where only one surrogate is present, at either the start or end of the context.
        // In both these scenarios just perform one backspace one the same as if it was a non surrogate pair.
        if ((i > 0) && (Uni_IsSurrogate1(application_context[i - 1]) && Uni_IsSurrogate2(application_context[i]))) {
          app->QueueAction(QIT_BACK, BK_DEFAULT | BK_SURROGATE);
          i--;
          cp_to_delete--;
          }
        else {
          app->QueueAction(QIT_BACK, BK_DEFAULT);
          cp_to_delete--;
        }
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
    SendDebugMessageFormat(0, sdmGlobal, 0, "ProcessHook: Saving option to registry for keyboard [%s].", activeKeyboard->Name);
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

  _td->CoreProcessEventRun = FALSE;
  // Process the action items from the core. This actions will modify the windows context (AppContext).
  // Therefore it is not required to copy the context from the core to the windows context.
  km_core_actions const* actions = km_core_state_get_actions(_td->lpActiveKeyboard->lpCoreKeyboardState);

  processBack(_td->app, actions->code_points_to_delete);
  processUnicodeChar(_td->app, actions->output);
  if (actions->persist_options != NULL) {
    processPersistOpt(actions, _td->lpActiveKeyboard);
  }
  if (actions->do_alert) {
    processAlert(_td->app);
  }
  if (actions->emit_keystroke) {
    *emitKeyStroke = TRUE;
  }
  processCapsLock(actions->new_caps_lock_state, !_td->state.isDown, _td->TIPFUpdateable, FALSE);
  return TRUE;
}

BOOL
ProcessActionsNonUpdatableParse(BOOL* emitKeyStroke) {
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) {
    return FALSE;
  }

  if (_td->TIPFUpdateable) {  // ensure only run when not updateable
    return FALSE;
  }
  _td->CoreProcessEventRun = TRUE;

  km_core_actions const* acts    = km_core_state_get_actions(_td->lpActiveKeyboard->lpCoreKeyboardState);
  processCapsLock(acts->new_caps_lock_state, !_td->state.isDown, _td->TIPFUpdateable, FALSE);
  if (acts->emit_keystroke) {
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
  km_core_actions const* acts = km_core_state_get_actions(_td->lpActiveKeyboard->lpCoreKeyboardState);
  processCapsLock(acts->new_caps_lock_state, !_td->state.isDown, FALSE, TRUE);
  return TRUE;
}
