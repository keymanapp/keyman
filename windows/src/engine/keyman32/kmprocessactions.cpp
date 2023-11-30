/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Description: Used for apply core processor actions to
 *              Keyman for Windows engine.
 */
#include "pch.h"

static BOOL processUnicodeChar(AITIP* app, const km_core_action_item* actionItem) {
  if (Uni_IsSMP(actionItem->character)) {
    app->QueueAction(QIT_CHAR, (Uni_UTF32ToSurrogate1(actionItem->character)));
    app->QueueAction(QIT_CHAR, (Uni_UTF32ToSurrogate2(actionItem->character)));
  }
  else {
    app->QueueAction(QIT_CHAR, actionItem->character);
  }
  return TRUE;
}

static BOOL processMarker(AITIP* app, const km_core_action_item* actionItem) {
  app->QueueAction(QIT_DEADKEY, (DWORD)actionItem->marker);
  return TRUE;
}

static BOOL processAlert(AITIP* app) {
  app->QueueAction(QIT_BELL, 0);
  return TRUE;
}

static BOOL processBack(AITIP* app, const km_core_action_item* actionItem) {
  if (actionItem->backspace.expected_type == KM_CORE_BT_MARKER) {
    app->QueueAction(QIT_BACK, BK_DEADKEY);
  } else if(actionItem->backspace.expected_type == KM_CORE_BT_CHAR) {
    // If this is a TSF-aware app we need to set the BK_SURROGATE flag to delete
    // both parts of the surrogate pair. Legacy apps receive a BKSP WM_KEYDOWN event
    // which results in deleting both parts in one action.
    if (!app->IsLegacy() && Uni_IsSMP(actionItem->backspace.expected_value)) {
      app->QueueAction(QIT_BACK, BK_DEFAULT | BK_SURROGATE);
    } else {
      app->QueueAction(QIT_BACK, BK_DEFAULT);
    }
  } else { // KM_CORE_BT_UNKNOWN
    app->QueueAction(QIT_BACK, BK_DEFAULT);
  }
  return TRUE;
}

static BOOL processPersistOpt(
  const km_core_action_item* actionItem,
  km_core_state* keyboardState,
  LPINTKEYBOARDINFO activeKeyboard
) {
  if (actionItem->option != NULL)
  {
    // Allocate for 1 option plus 1 pad struct of 0's for KM_CORE_IT_END
    km_core_option_item keyboardOpts[2] = { 0 };
    keyboardOpts[0].key = actionItem->option->key;
    keyboardOpts[0].value     = actionItem->option->value;
    km_core_status eventStatus = (km_core_status_codes)km_core_state_options_update(keyboardState, keyboardOpts);
    if (eventStatus != KM_CORE_STATUS_OK)
    {
      // log warning "problem saving option for km_core_keyboard");
      SendDebugMessageFormat(0, sdmGlobal, 0, "ProcessHook: Error %d saving option for keyboard [%s].", eventStatus, activeKeyboard->Name);
    }

    // Put the keyboard option into Windows Registry
    if (actionItem->option != NULL && actionItem->option->key != NULL &&
      actionItem->option->value != NULL)
    {
      // log"Saving keyboard option to registry");
      SendDebugMessageFormat(0, sdmGlobal, 0, "ProcessHook: Saving option to registry for keyboard [%s].", activeKeyboard->Name);
      LPWSTR value = new WCHAR[sizeof(actionItem->option->value) + 1];
      wcscpy_s(value, sizeof(actionItem->option->value) + 1, reinterpret_cast<LPCWSTR>(actionItem->option->value));
      SaveKeyboardOptionCoretoRegistry(activeKeyboard, reinterpret_cast<LPCWSTR>(actionItem->option->key), value);
    }
  }
  return TRUE;
}

static BOOL processInvalidateContext(
  AITIP* app
) {
  app->ResetContext();
  return TRUE;
}

static BOOL
processCapsLock(const km_core_action_item* actionItem, BOOL isUp, BOOL Updateable, BOOL externalEvent) {

  // We only want to process the Caps Lock key event once --
  // in the first pass (!Updateable).
  if (Updateable){
    return TRUE;
  }

  if (actionItem->capsLock) {
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
  }
  else {
    // This case would occur for the keyboard system store setting `store(&ShiftFreesCaps) '1'`
    // OR selecting a keyboard with CAPs always off rule
    if ((!isUp && IsCapsLockOn()) || (externalEvent && IsCapsLockOn())) {
      SendDebugMessageFormat(0, sdmGlobal, 0, "processCapsLock: TURN CAPS OFF: FIsUp=%d CapsState=%d", isUp, IsCapsLockOn());
      keybd_event(VK_CAPITAL, SCAN_FLAG_KEYMAN_KEY_EVENT, 0, 0);
      keybd_event(VK_CAPITAL, SCAN_FLAG_KEYMAN_KEY_EVENT, KEYEVENTF_KEYUP, 0);
    }
  }

  return TRUE;
}

BOOL ProcessActions(BOOL* emitKeyStroke)
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) return FALSE;

  _td->CoreProcessEventRun = FALSE;
  // Process the action items from the core. This actions will modify the windows context (AppContext).
  // Therefore it is not required to copy the context from the core to the windows context.

  for (auto act = km_core_state_action_items(_td->lpActiveKeyboard->lpCoreKeyboardState, nullptr); act->type != KM_CORE_IT_END; act++) {
    BOOL continueProcessingActions = TRUE;
    SendDebugMessageFormat(0, sdmGlobal, 0, "ProcessActions : act->type=%d", act->type);
    switch (act->type) {
    case KM_CORE_IT_CHAR:
      continueProcessingActions = processUnicodeChar(_td->app, act);
      break;
    case KM_CORE_IT_MARKER:
      continueProcessingActions = processMarker(_td->app, act);
      break;
    case KM_CORE_IT_ALERT:
      continueProcessingActions = processAlert(_td->app);
      break;
    case KM_CORE_IT_BACK:
      continueProcessingActions = processBack(_td->app, act);
      break;
    case KM_CORE_IT_PERSIST_OPT:
      continueProcessingActions = processPersistOpt(act, _td->lpActiveKeyboard->lpCoreKeyboardState, _td->lpActiveKeyboard);
      break;
    case KM_CORE_IT_EMIT_KEYSTROKE:
      *emitKeyStroke = TRUE;
      continueProcessingActions = TRUE;
      break;
    case KM_CORE_IT_INVALIDATE_CONTEXT:
      continueProcessingActions = processInvalidateContext(_td->app);
      break;
    case KM_CORE_IT_CAPSLOCK:
      continueProcessingActions = processCapsLock(act, !_td->state.isDown, _td->TIPFUpdateable, FALSE);
      break;
    case KM_CORE_IT_END:
      // fallthrough
    default:
      assert(false); // NOT SUPPORTED
      break;
    }
    if (!continueProcessingActions) {
      return FALSE;
    }
  }
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

  BOOL continueProcessingActions = TRUE;
  for (auto act = km_core_state_action_items(_td->lpActiveKeyboard->lpCoreKeyboardState, nullptr); act->type != KM_CORE_IT_END; act++) {
    switch (act->type) {
    case KM_CORE_IT_EMIT_KEYSTROKE:
      *emitKeyStroke = TRUE;
      SendDebugMessageFormat(0, sdmGlobal, 0, "ProcessActionsNonUpdatableParse EMIT_KEYSTROKE: act->type=[%d]", act->type);
      continueProcessingActions = TRUE;
      _td->CoreProcessEventRun = FALSE; // If we emit the key stroke on this parse we don't need the second parse
      break;
    case KM_CORE_IT_CAPSLOCK:
      continueProcessingActions = processCapsLock(act, !_td->state.isDown, _td->TIPFUpdateable, FALSE);
      break;
    case KM_CORE_IT_INVALIDATE_CONTEXT:
      continueProcessingActions = processInvalidateContext(_td->app);
      break;
    }
    if (!continueProcessingActions) {
      return FALSE;
    }
  }
  return TRUE;
}

BOOL
ProcessActionsExternalEvent() {
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) {
    return FALSE;
  }
  // Currently only a subset of actions are handled.
  // Other actions will be added when needed.
  BOOL continueProcessingActions = TRUE;
  for (auto act = km_core_state_action_items(_td->lpActiveKeyboard->lpCoreKeyboardState, nullptr); act->type != KM_CORE_IT_END;
       act++) {
    switch (act->type) {
    case KM_CORE_IT_CAPSLOCK:
      continueProcessingActions = processCapsLock(act, !_td->state.isDown, FALSE, TRUE);
      break;
    case KM_CORE_IT_INVALIDATE_CONTEXT:
      continueProcessingActions = processInvalidateContext(_td->app);
      break;
    }
    if (!continueProcessingActions) {
      return FALSE;
    }
  }
  return TRUE;
}
