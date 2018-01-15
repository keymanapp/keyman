#pragma once
#include "AbstractKeymanRuleProcessor.h"
class KmnKeymanRuleProcessor :
  public AbstractKeymanRuleProcessor
{
private:
  LPKEYBOARD keyboard;
protected:
  virtual BOOL ProcessEvent(const KeymanRuleEvent *event, KeymanRuleActionList *actions);
public:
  KmnKeymanRuleProcessor(LPKEYBOARD keyboard);
  ~KmnKeymanRuleProcessor();

};

