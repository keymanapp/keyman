#pragma once

#include <list>
#include "keyman64.h"

struct KeymanRuleEvent {
  // Keystroke event properties
  WORD vk;
  DWORD shiftState;

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
public:
  virtual BOOL ProcessEvent(const KeymanRuleEvent *event, KeymanRuleActionList *actions) = 0;
};

