#include "keyman64.h"
#include "KmnKeymanRuleProcessor.h"



KmnKeymanRuleProcessor::KmnKeymanRuleProcessor(LPKEYBOARD keyboard) {
  this->keyboard = keyboard;
}


KmnKeymanRuleProcessor::~KmnKeymanRuleProcessor() {
}

BOOL fOutputKeystroke;

char *KmnKeymanRuleProcessor::getcontext_debug() {
  //return "";
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) return "";
  return Debug_UnicodeString(_td->app->ContextBufMax(128));
}

BOOL KmnKeymanRuleProcessor::ProcessEvent(const KeymanRuleEvent *event, KeymanRuleActionList *actions) {
  UNREFERENCED_PARAMETER(actions);

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) return FALSE;

  LPGROUP gp = &keyboard->dpGroupArray[keyboard->StartGroup[BEGIN_UNICODE]];

  fOutputKeystroke = FALSE;

  //
  // If we are running in the debugger, don't do a second run through
  //

  if (_td->app->DebugControlled() && !_td->TIPFUpdateable) {   // I4287
    if (event->vk == VK_ESCAPE || (event->vk >= VK_PRIOR && event->vk <= VK_DOWN) || (event->vk == VK_DELETE)) return FALSE;   // I4033   // I4826   // I4845
    else return TRUE;
  }

  _td->app->ReadContext();

  if (event->isKeyDown) {   // I4827
    if (ShouldDebug(sdmKeyboard)) {
      DebugLogFormat("Key pressed: %s Context '%s'", Debug_VirtualKey(event->vk), getcontext_debug());
    }

    AIDEBUGKEYINFO keyinfo;
    keyinfo.shiftFlags = event->shiftState;
    keyinfo.VirtualKey = event->vk;
    keyinfo.Character = event->charCode;
    keyinfo.DeadKeyCharacter = 0;   // I4582
    keyinfo.IsUp = !event->isKeyDown;
    if (_td->app->IsUnicode())
      _td->app->QueueDebugInformation(QID_BEGIN_UNICODE, NULL, NULL, NULL, NULL, (DWORD_PTR)&keyinfo);
    else
      _td->app->QueueDebugInformation(QID_BEGIN_ANSI, NULL, NULL, NULL, NULL, (DWORD_PTR)&keyinfo);
  }

  this->currentEvent = event;
  this->currentActions = actions;
  ProcessGroup(gp);
  this->currentEvent = NULL;
  this->currentActions = NULL;

  if (*Globals::hwndIM() == 0 || *Globals::hwndIMAlways())
  {
    _td->app->SetCurrentShiftState(event->shiftState);
    _td->app->SendActions();   // I4196
  }

  _td->app->QueueDebugInformation(QID_END, NULL, NULL, NULL, NULL, 0);

  return !fOutputKeystroke;
}

/*
*	PRIVATE BOOL ProcessGroup(LPGROUP gp);
*
*	Parameters:	gp		Pointer to group to process inside
*
*	Returns:	TRUE if messages are to be sent,
*				and FALSE if no messages are to be sent.
*
*   Called by:  ProcessHook, recursive inside groups
*
*	ProcessKey is where the keystroke conversion and output takes place.  This routine
*	has a lot of crucial code in it!
*/

BOOL KmnKeymanRuleProcessor::ProcessGroup(LPGROUP gp)
{
  DWORD i;
  LPKEY kkp = NULL;
  PWSTR p;
  int sdmfI;

  /*
  If the number of nested groups goes higher than 50, then break out - this is
  a limitation of stack size.  This is basically a catch-all for freaky apps that
  cause message loopbacks and nasty things like that.  Okay, it's really a catch all
  for bugs!  This means the user's system shouldn't hang.
  */

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) return FALSE;

  _td->app->QueueDebugInformation(QID_GROUP_ENTER, gp, NULL, NULL, NULL, 0);

  sdmfI = -1;

  for (i = 0; i < keyboard->cxGroupArray; i++)
    if (gp == &keyboard->dpGroupArray[i])
    {
      if (currentEvent->isKeyDown && ShouldDebug(sdmKeyboard))
        DebugLogFormat("Entering group %d of %d, context '%s'", i + 1, keyboard->cxGroupArray, getcontext_debug());
      sdmfI = i;
      break;
    }

  if (++_td->state.LoopTimes > 50)
  {
    if (currentEvent->isKeyDown) DebugLogFormat("Aborting output: state.LoopTimes exceeded.");
    _td->state.StopOutput = TRUE;
    _td->app->QueueDebugInformation(QID_GROUP_EXIT, gp, NULL, NULL, NULL, QID_FLAG_RECURSIVE_OVERFLOW);
    return FALSE;
  }

  _td->state.NoMatches = TRUE;

  /*
  The rule matching loop.

  This loop iterates through all the rules in the group that is currently being
  processed.  Each rule in a group can be of three different types:
  1. A virtual key rule, where the key to be matched is a virtual key
  2. A normal key rule (WM_CHAR), where the key to be matched is an Ascii char.
  3. A rule in a keyless group, where only the context is matched.

  The loop goes through and checks the rules like that.  This loop could be optimized
  with standard searching techniques - the ContextMatch may be difficult.
  */

  if (ShouldDebug(sdmKeyboard))
    DebugLogFormat("event->vk: %s event->shiftState: %x; charCode: %X",
      Debug_VirtualKey(currentEvent->vk), currentEvent->shiftState, currentEvent->charCode);   // I4582

  if (gp)
  {
    for (kkp = gp->dpKeyArray, i = 0; i < gp->cxKeyArray; i++, kkp++)
    {
      if (!ContextMatch(kkp)) continue;
      if (!gp->fUsingKeys)
      {
        if (kkp->dpContext[0] != 0) break; else continue;
      }

      /* Keyman 6.0: support Virtual Characters */
      if (IsEquivalentShift(kkp->ShiftFlags, currentEvent->shiftState))
      {
        if (kkp->Key > VK__MAX && kkp->Key == currentEvent->vk) break;	// I3438   // I4582
        else if (kkp->Key == currentEvent->vk) break;   // I4169
      }
      else if (kkp->ShiftFlags == 0 && kkp->Key == currentEvent->charCode && currentEvent->charCode != 0) break;
    }
  }

  if (!gp || i == gp->cxKeyArray)
  {
    /*
    No rule was found that corresponded to the current state of the context and
    keyboard.  NoMatch should be checked for everything except virtual keys; and
    context should also be kept.

    If the message was a virtual key, then just return without checking NoMatch.
    NoMatch shouldn't be used for virtual keys because it will mean that no key
    can ever get through that isn't matched - including arrows, func. keys, etc !!
    Context is not kept for virtual keys being output.
    */

    if (currentEvent->isKeyDown && ShouldDebug(sdmKeyboard))
      DebugLogFormat("No match was found in group %d of %d", sdmfI, keyboard->cxGroupArray);

    if (!gp || (currentEvent->charCode == 0 && gp->fUsingKeys))   // I4585
                                                               // 7.0.241.0: I1133 - Fix mismatched parentheses on state.charCode - ie. we don't want to output this letter if !gp->fUsingKeys
    {
      BOOL fIsBackspace = currentEvent->vk == VK_BACK && (currentEvent->shiftState & (LCTRLFLAG | RCTRLFLAG | LALTFLAG | RALTFLAG)) == 0;   // I4128

      if (/*_td->app->DebugControlled() &&*/ fIsBackspace) {   // I4838   // I4933
                                                               //if(currentEvent->isKeyDown) 
                                                               //	_td->app->QueueAction(QIT_BACK, BK_BACKSPACE);
        if (currentEvent->isKeyDown) {   // I4933
          if (!_td->app->IsLegacy()) {   // I4933
            PWCHAR pdeletecontext = _td->app->ContextBuf(1);   // I4933
            if (!pdeletecontext || *pdeletecontext == 0) {   // I4933
              _td->app->ResetContext();   // I4933
              fOutputKeystroke = TRUE;   // I4933
              return FALSE;   // I4933
            }
          }
          _td->app->QueueAction(QIT_BACK, BK_BACKSPACE);   // I4933
        }
      }
      else if ((!_td->app->IsLegacy() || !fIsBackspace) && !_td->TIPFPreserved) {   // I4024   // I4128   // I4287   // I4290
        DebugLogFormat(" ... IsLegacy = FALSE; IsTIP = TRUE");   // I4128
        if (currentEvent->charCode == 0) _td->app->ResetContext();    // I3573   // I3577   // I4585
        fOutputKeystroke = TRUE;
        return FALSE;
      }
      //fOutputKeystroke = TRUE; return FALSE; // Don't swallow keystroke   // I3577
      ///DebugLogFormat(" ... IsLegacy = TRUE; IsTIP = TRUE");

      /*
      If the key is not a character key (white keys), or not processing, then we must init the stack -
      unknown keys do things like moving position in the context, so must clear.
      */
      if (fIsBackspace)   // I4128
      {
        /*
        Must have special handling for VK_BACK: delete a character from the context stack
        This only fires if the keyboard has no rule for backspace.
        */

        //				if(currentEvent->isKeyDown)    // I4933
        //				_td->app->QueueAction(QIT_BACK, BK_BACKSPACE);   // I4933
      }
      else
      {
        //app->NoSetShift = FALSE;

        DWORD dw = currentEvent->vk;
        if (dw == 0x05) dw = VK_RETURN;    // I649 - VK_ENTER and K_NPENTER

        if (_td->state.msg.lParam & (1 << 24)) dw |= QVK_EXTENDED;	// Extended key flag  // I3438

        if (currentEvent->charCode == 0) {
          _td->app->ResetContext();
        }

        if (_td->TIPFPreserved) {   // I4290
          if (currentEvent->charCode != 0) {
            _td->app->QueueAction(QIT_CHAR, currentEvent->charCode);
          }
        }
        else {
          if (currentEvent->isKeyDown)
          {
            _td->app->QueueAction(QIT_VSHIFTDOWN, currentEvent->shiftState);  // 15/05/2001 - fixing I201 -- enabled line
            _td->app->QueueAction(QIT_VKEYDOWN, dw);
          }

          if (!currentEvent->isKeyDown) {
            _td->app->QueueAction(QIT_VKEYUP, dw);
            _td->app->QueueAction(QIT_VSHIFTUP, currentEvent->shiftState);
          }
        }
      }
    }
    else if (gp->dpNoMatch != NULL && *gp->dpNoMatch != 0 && currentEvent->isKeyDown)
    {
      /* NoMatch rule found, and is a character key */
      _td->app->QueueDebugInformation(QID_NOMATCH_ENTER, gp, NULL, NULL, gp->dpNoMatch, 0);
      PostString(gp->dpNoMatch, &_td->state.msg, keyboard, NULL);
      _td->app->QueueDebugInformation(QID_NOMATCH_EXIT, gp, NULL, NULL, gp->dpNoMatch, 0);
    }
    else if (currentEvent->charCode != 0 && currentEvent->charCode != 0xFFFF && currentEvent->isKeyDown && gp->fUsingKeys)
    {
      /* No rule found, is a character key */
      // 7.0.239.0: I994 - Workaround output order issues - we will use the TSF to output all characters...
      // if(app->Type1() == AIType_TIP) { fOutputKeystroke = TRUE; return FALSE; } // Don't swallow keystroke

      _td->app->QueueAction(QIT_CHAR, currentEvent->charCode);
    }

    _td->app->QueueDebugInformation(QID_GROUP_EXIT, gp, NULL, NULL, NULL, QID_FLAG_NOMATCH);
    return TRUE;
  }

  if (_td->state.msg.message == wm_keymankeyup)
    return TRUE;

  DebugLogFormat("match found in rule %d", i);

  _td->state.NoMatches = FALSE;

  /*
  Save the context that will be used for output when the 'context' keyword is used.
  For each deadkey, we need to add 2 characters; look in related stores as well...
  */

  assert(kkp != NULL);

  // 11 Aug 2003 - I25(v6) - mcdurdin - CODE_NUL context support
  if (*kkp->dpContext == UC_SENTINEL && *(kkp->dpContext + 1) == CODE_NUL)
    wcsncpy_s(_td->miniContext, GLOBAL_ContextStackSize, _td->app->ContextBuf(xstrlen_ignoreifopt(kkp->dpContext) - 1), GLOBAL_ContextStackSize);  // I3162   // I3536
  else
    wcsncpy_s(_td->miniContext, GLOBAL_ContextStackSize, _td->app->ContextBuf(xstrlen_ignoreifopt(kkp->dpContext)), GLOBAL_ContextStackSize);  // I3162   // I3536

  _td->miniContext[GLOBAL_ContextStackSize - 1] = 0;

  _td->app->QueueDebugInformation(QID_RULE_ENTER, gp, kkp, _td->miniContext, NULL, 0);

  /*
  The next section includes several optimizations that make the code a little harder
  to read, but are probably worth it in the time that they save.

  If the output string doesn't have a "context" byte at the start, post backspaces
  to erase the appropriate number of characters in the application.  If it does have
  a "context" byte at the start, then the string won't change, and no backspaces are
  necessary.  You could go one step further with this optimization, in PostAllKeys,
  by comparing the starts of the strings to see what is same, and not backspacing
  that, but it is probably not necessary.
  */

  p = kkp->dpOutput;
  if (*p != UC_SENTINEL || *(p + 1) != CODE_CONTEXT)
  {
    for (p = _td->miniContext; *p; p = incxstr(p))
    {
      if (*p == UC_SENTINEL)
        switch (*(p + 1))
        {
        case CODE_DEADKEY: _td->app->QueueAction(QIT_BACK, BK_DEADKEY); break;
        case CODE_NUL: break;	// 11 Aug 2003 - I25(v6) - mcdurdin - CODE_NUL context support
        }
      else
      {
        _td->app->QueueAction(QIT_BACK, 0);
        if (*p >= 0xD800 && *p <= 0xDBFF)
          _td->app->QueueAction(QIT_BACK, BK_SUPP2);  // I1389 - supp chars on vista default to single backspace - use BK_SUPP2 flag
      }
    }
    p = kkp->dpOutput;
  }
  else p += 2;				// otherwise, the "context" entry has to be jumped over

                      /* Use PostString to post the rest of the output string. */

  if (PostString(p, &_td->state.msg, keyboard, NULL) == psrCheckMatches)
  {
    _td->app->QueueDebugInformation(QID_RULE_EXIT, gp, kkp, _td->miniContext, NULL, 0);

    if (gp->dpMatch && *gp->dpMatch)
    {
      _td->app->QueueDebugInformation(QID_MATCH_ENTER, gp, NULL, NULL, gp->dpMatch, 0);
      PostString(gp->dpMatch, &_td->state.msg, keyboard, NULL);
      _td->app->QueueDebugInformation(QID_MATCH_EXIT, gp, NULL, NULL, gp->dpMatch, 0);
    }
  }
  else
    _td->app->QueueDebugInformation(QID_RULE_EXIT, gp, kkp, _td->miniContext, NULL, 0);
  _td->app->QueueDebugInformation(QID_GROUP_EXIT, gp, NULL, NULL, NULL, 0);

  return TRUE;
}

/*
*	int PostString( LPSTR str, BOOL *useMode, LPMSG mp,
*	LPKEYBOARD lpkb );
*
*	Parameters:	str		Pointer to string to send
*				useMode	Pointer to BOOL about whether a "use" command was found
*				mp		Pointer to MSG structure to copy in outputting messages
*				lpkb	Pointer to global keyboard structure
*
*	Returns:	0 to continue, 1 and 2 to return.
*
*   Called by:  ProcessKey
*
*	PostString posts a string of "context", "index", "beep", characters and virtual keys
*	to the active application, via the Keyman PostKey buffer.
*/

int KmnKeymanRuleProcessor::PostString(PWSTR str, LPMSG mp, LPKEYBOARD lpkb, PWSTR endstr)
{
  PWSTR p, q, temp;
  LPSTORE s;
  int n1, n2;
  int i, n, shift;
  BOOL FoundUse = FALSE;

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) return FALSE;

  // TODO: Refactor to use incxstr
  for (p = str; *p && (p < endstr || !endstr); p++)
  {
    if (*p == UC_SENTINEL)
      switch (*(++p))
      {
      case CODE_EXTENDED:				// Start of a virtual key section w/shift codes
        p++;

        shift = *p; //(*p<<8) | *(p+1);
        _td->app->QueueAction(QIT_VSHIFTDOWN, shift);

        p++;

        _td->app->QueueAction(QIT_VKEYDOWN, *p);
        _td->app->QueueAction(QIT_VKEYUP, *p);

        _td->app->QueueAction(QIT_VSHIFTUP, shift);

        p++; // CODE_EXTENDEDEND
             ////// CODE_EXTENDEDEND will be incremented by loop

             //app->QueueAction(QIT_VSHIFTUP, shift);
        break;

      case CODE_DEADKEY:				// A deadkey to be output
        p++;
        _td->app->QueueAction(QIT_DEADKEY, *p);
        break;
      case CODE_BEEP:					// Sound an 'iconasterisk' beep
        _td->app->QueueAction(QIT_BELL, 0);
        break;
      case CODE_CONTEXT:				// copy the context to the output
        for (q = _td->miniContext; *q; q++) {
          _td->app->QueueAction(QIT_CHAR, *q);
        }
        break;
      case CODE_CONTEXTEX:
        p++;
        for (q = _td->miniContext, i = 0; *q && i < *p - 1; i++, q = incxstr(q));
        if (*q) {
          _td->app->QueueAction(QIT_CHAR, *q);
          if (Uni_IsSurrogate1(*q) && Uni_IsSurrogate2(*(q + 1))) {
            _td->app->QueueAction(QIT_CHAR, *(q + 1));
          }
        }
        break;
      case CODE_RETURN:				// stop processing and start PostAllKeys
        _td->state.StopOutput = TRUE;
        return psrPostMessages;

      case CODE_CALL:
        p++;
        CallDLL(_td->lpActiveKeyboard, *p - 1);
        if (_td->state.StopOutput) return psrPostMessages;
        FoundUse = TRUE;
        break;
      case CODE_USE:					// use another group
        p++;
        ProcessGroup(&lpkb->dpGroupArray[*p - 1]);
        if (_td->state.StopOutput) return psrPostMessages;
        FoundUse = TRUE;
        break;
      case CODE_CLEARCONTEXT:
        _td->app->ResetContext();
        _td->app->ReadContext();
        break;
      case CODE_INDEX:
        p++;
        s = &_td->lpActiveKeyboard->Keyboard->dpStoreArray[*p - 1];
        p++;

        n = _td->IndexStack[*p - 1];
        for (temp = s->dpString; *temp && n > 0; temp = incxstr(temp), n--);
        PostString(temp, mp, lpkb, incxstr(temp));
        break;
      case CODE_SETOPT:
        p++;
        n1 = *p - 1;
        p++;
        n2 = *p - 1;
        SetKeyboardOption(_td->lpActiveKeyboard, n1, n2);
        break;
      case CODE_RESETOPT:
        p++;
        n1 = *p - 1;
        ResetKeyboardOption(_td->lpActiveKeyboard, n1);
        break;
      case CODE_SAVEOPT:
        p++;
        n1 = *p - 1;
        SaveKeyboardOption(_td->lpActiveKeyboard, n1);
        break;
      case CODE_IFSYSTEMSTORE:
        p += 3;
        break;
      case CODE_SETSYSTEMSTORE:
        p += 2;
        break;
      }
    else
      _td->app->QueueAction(QIT_CHAR, *p);
  }
  return FoundUse ? psrPostMessages : psrCheckMatches;
}


BOOL KmnKeymanRuleProcessor::IsMatchingBaseLayout(PWCHAR layoutName)  // I3432
{
  BOOL bEqual = _wcsicmp(layoutName, Globals::get_BaseKeyboardName()) == 0 ||   // I4583
    _wcsicmp(layoutName, Globals::get_BaseKeyboardNameAlt()) == 0;   // I4583

  return bEqual;
}

#pragma warning(disable: 4996)
BOOL KmnKeymanRuleProcessor::IsMatchingPlatformString(PWCHAR platform)  // I3432
{
  if (_wcsicmp(platform, L"windows") == 0 ||
    _wcsicmp(platform, L"desktop") == 0 ||
    _wcsicmp(platform, L"native") == 0) return TRUE;

  PWCHAR t = wcschr(platform, 0) - 1;
  BOOL OrNewer = (t > platform && *t == L'+');
  if (OrNewer) *t = 0;

  WORD version;
  if (_wcsicmp(platform, L"xp") == 0) version = 0x0105;
  else if (_wcsicmp(platform, L"vista") == 0) version = 0x0006;
  else if (_wcsicmp(platform, L"seven") == 0) version = 0x0106;
  else if (_wcsicmp(platform, L"eight") == 0) version = 0x0206;
  else if (_wcsicmp(platform, L"2000") == 0) version = 0x0005;
  else if (_wcsicmp(platform, L"2003") == 0) version = 0x0205;
  else if (_wcsicmp(platform, L"2008") == 0) version = 0x0006;
  else if (_wcsicmp(platform, L"2008r2") == 0) version = 0x0106;
  else if (_wcsicmp(platform, L"2012") == 0) version = 0x0206;
  else if (_wcsicmp(platform, L"10") == 0) version = 0x0306;

  else if (_wcsicmp(platform, L"5.0") == 0) version = 0x0005;
  else if (_wcsicmp(platform, L"5.1") == 0) version = 0x0105;
  else if (_wcsicmp(platform, L"5.2") == 0) version = 0x0205;
  else if (_wcsicmp(platform, L"6.0") == 0) version = 0x0006;
  else if (_wcsicmp(platform, L"6.1") == 0) version = 0x0106;
  else if (_wcsicmp(platform, L"6.2") == 0) version = 0x0206;

  else return FALSE;

  return
    OrNewer ? LOWORD(GetVersion()) >= version : LOWORD(GetVersion()) == version;
}
#pragma warning(default: 4996)

BOOL KmnKeymanRuleProcessor::IsMatchingPlatform(LPSTORE s)  // I3432
{
  PWCHAR t = new WCHAR[wcslen(s->dpString) + 1];
  wcscpy_s(t, wcslen(s->dpString) + 1, s->dpString);
  PWCHAR context = NULL;
  PWCHAR platform = wcstok_s(t, L" ", &context);
  while (platform != NULL)
  {
    if (!IsMatchingPlatformString(platform))
    {
      s->dwSystemID = TSS_PLATFORM_NOMATCH;
      delete t;
      return FALSE;
    }
    platform = wcstok_s(NULL, L" ", &context);
  }

  s->dwSystemID = TSS_PLATFORM_MATCH;
  delete t;
  return TRUE;
}

/*
*	BOOL ContextMatch( LPKEY kkp );
*
*	Parameters:	kkp		Rule to compare
*
*	Returns:    0 on OK, 1 on not equal
*
*   Called by:	ProcessKey
*
*	ContextMatch compares the context of a rule with the current context.
*/

BOOL KmnKeymanRuleProcessor::ContextMatch(LPKEY kkp)
{
  WORD /*i,*/ n;
  PWSTR p, q, qbuf, temp;
  LPWORD indexp;
  LPSTORE s, t;
  BOOL bEqual;

  //DebugLogFormat("ContextMatch: ENTER [%d]", kkp->Line);
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) return FALSE;

  memset(_td->IndexStack, 0, GLOBAL_ContextStackSize * sizeof(WORD));  // I3158   // I3524

  p = kkp->dpContext;

  if (*p == 0)
  {
    //DebugLogFormat("ContextMatch: EXIT TRUE -> no rule context");
    return TRUE;
  }

  /* 11 Aug 2003 - I25(v6) - mcdurdin - test for CODE_NUL */

  if (*p == UC_SENTINEL && *(p + 1) == CODE_NUL)
  {
    // If context buf is longer than the context, then obviously not start of doc.
    if (_td->app->ContextBuf(xstrlen_ignoreifopt(p))) return FALSE;  // I2484 - Fix bug with if() following nul in same statement
    p = incxstr(p);
    if (*p == 0) return TRUE;
  }

  for (PWCHAR pp = p; pp && *pp; pp = incxstr(pp))
  {
    if (*pp == UC_SENTINEL && *(pp + 1) == CODE_IFOPT)
    {
      s = &_td->lpActiveKeyboard->Keyboard->dpStoreArray[(*(pp + 2)) - 1];  // I2590
      t = &_td->lpActiveKeyboard->Keyboard->dpStoreArray[(*(pp + 4)) - 1];  // I2590

      bEqual = wcscmp(s->dpString, t->dpString) == 0;
      if (*(pp + 3) == 1 && bEqual) return FALSE;  // I2590
      if (*(pp + 3) == 2 && !bEqual) return FALSE;  // I2590
    }
    else if (*pp == UC_SENTINEL && *(pp + 1) == CODE_IFSYSTEMSTORE)  // I3432
    {
      DWORD dwSystemID = *(pp + 2) - 1;
      t = &_td->lpActiveKeyboard->Keyboard->dpStoreArray[(*(pp + 4)) - 1];  // I2590
      switch (dwSystemID)
      {
      case TSS_PLATFORM_MATCH:  // Cached platform result - a matching platform
        bEqual = TRUE;
        break;
      case TSS_PLATFORM_NOMATCH:  // Cached platform result - not a matching platform
        bEqual = FALSE;
        break;
      case TSS_PLATFORM:
        bEqual = IsMatchingPlatform(t);
        break;
      case TSS_BASELAYOUT:
        bEqual = IsMatchingBaseLayout(t->dpString);
        break;
      default:
      {
        PWCHAR ss = GetSystemStore(_td->lpActiveKeyboard->Keyboard, dwSystemID);
        if (ss == NULL) return FALSE;
        bEqual = wcscmp(ss, t->dpString) == 0;
      }
      }

      if (*(pp + 3) == 1 && bEqual) return FALSE;  // I2590
      if (*(pp + 3) == 2 && !bEqual) return FALSE;  // I2590
    }
  }

  q = qbuf = _td->app->ContextBuf(xstrlen_ignoreifopt(p));
  if (!q)
  {
    //DebugLogFormat("ContextMatch: EXIT FALSE -> context too short");
    return FALSE;	// context buf is too short!
  }
  indexp = _td->IndexStack;

  //DebugLogFormat("ContextMatch: [%d] Rule: %s", kkp->Line, format_unicode_debug(kkp->dpContext));
  //DebugLogFormat("ContextMatch: [%d] Test: %s", kkp->Line, format_unicode_debug(q));

  for (; *p && *q; p = incxstr(p))
  {
    //DebugLogFormat("ContextMatch: p:%x q:%x", *p, *q);
    *indexp = 0;

    if (*p == UC_SENTINEL)
    {
      switch (*(p + 1))
      {
      case CODE_DEADKEY:
        if (*q != UC_SENTINEL || *(q + 1) != CODE_DEADKEY || *(q + 2) != *(p + 2))
        {
          //	DebugLogFormat("ContextMatch: EXIT FALSE -> deadkeys don't match %x %x %x != %x %x %x",
          //				*p, *(p+1), *(p+2), *q, *(q+1), *(q+2));
          return FALSE;
        }
        break;
      case CODE_ANY:
        s = &_td->lpActiveKeyboard->Keyboard->dpStoreArray[(*(p + 2)) - 1];

        temp = xstrchr(s->dpString, q);

        /*DebugLogFormat("ContextMatch: CODE_ANY [%x %x %x %x %x %x %x %x %x %x] [%x %x %x] %d",
        s->dpString[0], s->dpString[1], s->dpString[2],
        s->dpString[3], s->dpString[4], s->dpString[5],
        s->dpString[6], s->dpString[7], s->dpString[8],
        s->dpString[9],
        q[0], q[1], q[2],
        (temp ? (int)(temp-s->dpString) : 0));*/

        if (temp != NULL)  // I1622
          *indexp = (WORD)xstrpos(temp, s->dpString);

        //if((temp = xstrchr(s->dpString, GetSuppChar(q))) != NULL)
        //	    *indexp = xstrpos(temp, s->dpString);
        else
          return FALSE;
        break;
      case CODE_NOTANY:
        s = &_td->lpActiveKeyboard->Keyboard->dpStoreArray[(*(p + 2)) - 1];

        if ((temp = xstrchr(s->dpString, q)) != NULL)   // I1622
          return FALSE;

        //if((temp = xstrchr(s->dpString, GetSuppChar(q))) != NULL)
        //  return FALSE;
        break;
      case CODE_INDEX:
        s = &_td->lpActiveKeyboard->Keyboard->dpStoreArray[(*(p + 2)) - 1];
        *indexp = n = _td->IndexStack[(*(p + 3)) - 1];

        for (temp = s->dpString; *temp && n > 0; temp = incxstr(temp), n--);
        if (n != 0) return FALSE;
        if (xchrcmp(temp, q) != 0) return FALSE;
        ////if(GetSuppChar(temp) != GetSuppChar(q)) return FALSE; // I1622
        break;
      case CODE_CONTEXTEX:
        // only the nth character
        for (n = *(p + 2) - 1, temp = qbuf; temp < q && n > 0; n--, temp = incxstr(temp));
        if (n == 0)
          if (xchrcmp(temp, q) != 0) return FALSE;
        //if(GetSuppChar(temp) != GetSuppChar(q)) return FALSE;
        break;
      case CODE_IFOPT:
      case CODE_IFSYSTEMSTORE:  // I3432
        indexp++;
        continue; // don't increment q
      default:
        return FALSE;
      }
    }
    else if (xchrcmp(p, q) != 0) //GetSuppChar(p) != GetSuppChar(q)) 
    {
      //DebugLogFormat("ContextMatch: [%d] FAIL: %s", kkp->Line, format_unicode_debug(p));
      //DebugLogFormat("ContextMatch: [%d] FAIL: %s", kkp->Line, format_unicode_debug(q));
      //DebugLogFormat("ContextMatch: EXIT FALSE -> chrs don't match");
      return FALSE;
    }
    indexp++;
    q = incxstr(q);
  }

  while (*p == UC_SENTINEL && (*(p + 1) == CODE_IFOPT || *(p + 1) == CODE_IFSYSTEMSTORE)) p = incxstr(p); // already tested  // I3432

  //DebugLogFormat("ContextMatch: EXIT %s -> END OF FUNCTION", *p == *q ? "TRUE" : "FALSE");
  return *p == *q; /*at least one must ==0 at this point*/
}
