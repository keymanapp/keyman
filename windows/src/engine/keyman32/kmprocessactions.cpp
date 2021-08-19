/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Description: Used for apply core processor actions to
 *              Keyman for Windows engine.
 */
#include "pch.h"

static BOOL processUnicodeChar(AITIP* app, const km_kbp_action_item* actionItem) {
  if (Uni_IsSMP(actionItem->character)) {
    app->QueueAction(QIT_CHAR, (Uni_UTF32ToSurrogate1(actionItem->character)));
    app->QueueAction(QIT_CHAR, (Uni_UTF32ToSurrogate2(actionItem->character)));
  }
  else {
    app->QueueAction(QIT_CHAR, actionItem->character);
  }
  return TRUE;
}

static BOOL processMarker(AITIP* app, const km_kbp_action_item* actionItem) {
  app->QueueAction(QIT_DEADKEY, (DWORD)actionItem->marker);
  return TRUE;
}

static BOOL processAlert(AITIP* app) {
  app->QueueAction(QIT_BELL, 0);
  return TRUE;
}

static BOOL processBack(AITIP* app, const km_kbp_action_item* actionItem) {
  if (actionItem->backspace.expected_type == KM_KBP_BT_MARKER) {
    app->QueueAction(QIT_BACK, BK_DEADKEY);
  } else /* actionItem->backspace.expected_type == KM_KBP_BT_CHAR, KM_KBP_BT_UNKNOWN */ {
    app->QueueAction(QIT_BACK, 0);
  }
  return TRUE;
}

static BOOL processPersistOpt(
  const km_kbp_action_item* actionItem,
  km_kbp_state* keyboardState,
  LPINTKEYBOARDINFO activeKeyboard
) {
  if (actionItem->option != NULL)
  {
    // Allocate for 1 option plus 1 pad struct of 0's for KM_KBP_IT_END
    km_kbp_option_item keyboardOpts[2] = { 0 };

    km_kbp_cp* cp = new km_kbp_cp[sizeof(actionItem->option->key) + 1];
    wcscpy_s(reinterpret_cast<LPWSTR>(cp), sizeof(actionItem->option->key) + 1, reinterpret_cast<LPCWSTR>(actionItem->option->key));
    keyboardOpts[0].key = cp;

    cp = new km_kbp_cp[sizeof(actionItem->option->value) + 1];
    wcscpy_s(reinterpret_cast<LPWSTR>(cp), sizeof(actionItem->option->value) + 1, reinterpret_cast<LPCWSTR>(actionItem->option->value));
    keyboardOpts[0].value = cp;
    keyboardOpts[1] = KM_KBP_OPTIONS_END;
    km_kbp_status eventStatus = km_kbp_state_options_update(keyboardState, keyboardOpts);
    if (eventStatus != KM_KBP_STATUS_OK)
    {
      // log warning "problem saving option for km_kbp_keyboard");
      SendDebugMessageFormat(0, sdmGlobal, 0, "ProcessHook: Error %d saving option for keyboard [%s].", eventStatus, activeKeyboard->Name);
    }

    for (int i = 0; i < 2; i++) {
      delete[] keyboardOpts[i].key;
      delete[] keyboardOpts[i].value;
    }
    // Put the keyboard option into Windows Registry
    if (actionItem->option != NULL && actionItem->option->key != NULL &&
      actionItem->option->value != NULL)
    {
      // log"Saving keyboard option to registry");
      SendDebugMessageFormat(0, sdmGlobal, 0, "ProcessHook: Saving option to registry for keybooard [%s].", activeKeyboard->Name);
      LPWSTR value = new WCHAR[sizeof(actionItem->option->value) + 1];
      wcscpy_s(value, sizeof(actionItem->option->value) + 1, reinterpret_cast<LPCWSTR>(actionItem->option->value));
      SaveKeyboardOptionREGCore(activeKeyboard, reinterpret_cast<LPCWSTR>(actionItem->option->key), value);
    }
  }
  return TRUE;
}

static BOOL processInvalidateContext(
  AITIP* app,
  km_kbp_state* keyboardState
) {
  km_kbp_context_clear(km_kbp_state_context(keyboardState));
  app->ResetContext();
  return TRUE;
}

 // TODO: 5011  make this function a new file
BOOL ProcessActions(BOOL* emitKeyStroke)
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) return FALSE;

  // Process the action items from the core. This actions will modify the windows context (AppContext).
  // Therefore it is not required to copy the context from the core to the windows context.

  for (auto act = km_kbp_state_action_items(_td->lpActiveKeyboard->lpActiveKBState, nullptr); act->type != KM_KBP_IT_END; act++) {
    BOOL continueProcessingActions = TRUE;
    switch (act->type) {
    case KM_KBP_IT_CHAR:
      continueProcessingActions = processUnicodeChar(_td->app, act);
      break;
    case KM_KBP_IT_MARKER:
      continueProcessingActions = processMarker(_td->app, act);
      break;
    case KM_KBP_IT_ALERT:
      continueProcessingActions = processAlert(_td->app);
      break;
    case KM_KBP_IT_BACK:
      continueProcessingActions = processBack(_td->app, act);
      break;
    case KM_KBP_IT_PERSIST_OPT:
      continueProcessingActions = processPersistOpt(act, _td->lpActiveKeyboard->lpActiveKBState, _td->lpActiveKeyboard);
      break;
    case KM_KBP_IT_EMIT_KEYSTROKE:
      *emitKeyStroke = TRUE;
      continueProcessingActions = TRUE;
      break;
    case KM_KBP_IT_INVALIDATE_CONTEXT:
      continueProcessingActions = processInvalidateContext(_td->app, _td->lpActiveKeyboard->lpActiveKBState);
      break;
    case KM_KBP_IT_END:
      // fallthrough
    default:
      assert(false); // NOT SUPPORTED
      break;
    }
    if (!continueProcessingActions)
      return FALSE;
  }
  return TRUE;
}
