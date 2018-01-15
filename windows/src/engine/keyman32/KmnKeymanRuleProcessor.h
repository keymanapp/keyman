#pragma once
#include "AbstractKeymanRuleProcessor.h"
class KmnKeymanRuleProcessor :
  public AbstractKeymanRuleProcessor
{
private:
  LPKEYBOARD keyboard;
  BOOL ContextMatch(LPKEY kkp);
  BOOL IsMatchingPlatform(LPSTORE s); // I3432
  BOOL IsMatchingBaseLayout(PWCHAR layoutName);  // I3432
  BOOL IsMatchingPlatformString(PWCHAR platform);  // I3432
  int PostString(PWSTR str, LPMSG mp, LPKEYBOARD lpkb, PWSTR endstr);
  BOOL ProcessGroup(LPGROUP gp);
  char *getcontext_debug();
protected:
  virtual BOOL ProcessEvent(const KeymanRuleEvent *event, KeymanRuleActionList *actions);
public:
  KmnKeymanRuleProcessor(LPKEYBOARD keyboard);
  ~KmnKeymanRuleProcessor();

};

