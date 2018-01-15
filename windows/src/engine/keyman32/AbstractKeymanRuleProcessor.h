#pragma once

#include <list>
#include "keyman64.h"

struct KeymanRuleEvent {
  // Keystroke event properties
  WORD vk;
  DWORD shiftState;
  WCHAR charCode;
  BOOL isKeyDown;
  BOOL isExtended;
  MSG msg;  //TODO: refactor into a keyDownState and other properties?

  // Context data
  WCHAR *context; // up to 256 elements?

  // Environmental data

};

struct KeymanRuleAction {
  // Use current action items?
};

typedef std::list<KeymanRuleAction> KeymanRuleActionList;

class AbstractKeymanRuleProcessor
{
protected:
  void DebugLogFormat(char *fmt, ...);
  void DebugLog(char *msg);
public:
  virtual BOOL ProcessEvent(const KeymanRuleEvent *event, KeymanRuleActionList *actions) = 0;
};

