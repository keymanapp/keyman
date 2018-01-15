#include "keyman64.h"
#include "KmnKeymanRuleProcessor.h"



KmnKeymanRuleProcessor::KmnKeymanRuleProcessor(LPKEYBOARD keyboard) {
  this->keyboard = keyboard;
}


KmnKeymanRuleProcessor::~KmnKeymanRuleProcessor() {
}

extern BOOL fOutputKeystroke;

BOOL KmnKeymanRuleProcessor::ProcessEvent(const KeymanRuleEvent *event, KeymanRuleActionList *actions) {
  UNREFERENCED_PARAMETER(event);
  UNREFERENCED_PARAMETER(actions);

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) return FALSE;

  LPGROUP gp = _td->state.startgroup;

  fOutputKeystroke = FALSE;

  //
  // If we are running in the debugger, don't do a second run through
  //

  if (_td->app->DebugControlled() && !_td->TIPFUpdateable) {   // I4287
    if (_td->state.vkey == VK_ESCAPE || (_td->state.vkey >= VK_PRIOR && _td->state.vkey <= VK_DOWN) || (_td->state.vkey == VK_DELETE)) return FALSE;   // I4033   // I4826   // I4845
    else return TRUE;
  }

  _td->app->ReadContext();

  if (_td->state.msg.message == wm_keymankeydown) {   // I4827
    if (ShouldDebug(sdmKeyboard)) {
//      SendDebugMessageFormat(_td->state.msg.hwnd, sdmKeyboard, 0, "Key pressed: %s Context '%s'",
//        Debug_VirtualKey(_td->state.vkey), getcontext_debug());
    }

    AIDEBUGKEYINFO keyinfo;
    keyinfo.shiftFlags = Globals::get_ShiftState();
    keyinfo.VirtualKey = _td->state.vkey;
    keyinfo.Character = _td->state.charCode;
    keyinfo.DeadKeyCharacter = 0;   // I4582
    keyinfo.IsUp = _td->state.msg.message == wm_keymankeyup;
    if (_td->app->IsUnicode())
      _td->app->QueueDebugInformation(QID_BEGIN_UNICODE, NULL, NULL, NULL, NULL, (DWORD_PTR)&keyinfo);
    else
      _td->app->QueueDebugInformation(QID_BEGIN_ANSI, NULL, NULL, NULL, NULL, (DWORD_PTR)&keyinfo);
  }

  ProcessGroup(gp);

  if (*Globals::hwndIM() == 0 || *Globals::hwndIMAlways())
  {
    _td->app->SetCurrentShiftState(Globals::get_ShiftState());
    _td->app->SendActions();   // I4196
  }

  _td->app->QueueDebugInformation(QID_END, NULL, NULL, NULL, NULL, 0);

  return !fOutputKeystroke;
}
