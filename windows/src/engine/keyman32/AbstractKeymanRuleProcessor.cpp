#include "keyman64.h"
#include "AbstractKeymanRuleProcessor.h"
#include <stdio.h>
#include <stdarg.h>

void AbstractKeymanRuleProcessor::DebugLogFormat(char *fmt, ...) {
  char fmtbuf[256];

  va_list vars;
  va_start(vars, fmt);
  vsnprintf_s(fmtbuf, _countof(fmtbuf), _TRUNCATE, fmt, vars);  // I2248   // I3547
  fmtbuf[255] = 0;
  DebugLog(fmtbuf);
}

void AbstractKeymanRuleProcessor::DebugLog(char *msg) {
  SendDebugMessage(0, sdmKeyboard, 0, msg);
}

BOOL KeymanRuleActionList::QueueDebugInformation(int ItemType, LPGROUP Group, LPKEY Rule, PWSTR fcontext, PWSTR foutput, DWORD_PTR dwExtraFlags) {
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) return FALSE;
  return _td->app->QueueDebugInformation(ItemType, Group, Rule, fcontext, foutput, dwExtraFlags);
}
