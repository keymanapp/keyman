/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include "kmx_processor.h"
#include "state.hpp"

using namespace km::kbp;
using namespace kmx;

/* Globals */

KMX_BOOL km::kbp::kmx::g_debug_ToConsole = TRUE, km::kbp::kmx::g_debug_KeymanLog = TRUE;
KMX_BOOL km::kbp::kmx::g_silent = FALSE;

/*
* KMX_Processor
*/

KMX_Processor::KMX_Processor() : m_actions(&m_context), m_options(&m_keyboard) {
  m_indexStack = new KMX_WORD[GLOBAL_ContextStackSize];
  m_miniContext = new KMX_WCHAR[GLOBAL_ContextStackSize];
}

KMX_Processor::~KMX_Processor() {
  delete[] m_indexStack;
  delete[] m_miniContext;
}

char VKeyToChar(KMX_UINT modifiers, KMX_UINT vk) {
  // We only map SHIFT and UNSHIFTED, and CAPS LOCK

  if ((modifiers & ~(K_SHIFTFLAG | CAPITALFLAG)) != 0) {
    return 0;
  }

  bool
    shifted = (modifiers & K_SHIFTFLAG) == K_SHIFTFLAG,
    caps = (modifiers & CAPITALFLAG) == CAPITALFLAG;

  if (vk == KM_KBP_VKEY_SPACE) {
    // Override for space because it is the same for
    // shifted and unshifted.
    return 32;
  }

  for (int i = 0; s_char_to_vkey[i].vk; i++) {
    if (s_char_to_vkey[i].caps && caps) {
      if (s_char_to_vkey[i].vk == vk && s_char_to_vkey[i].shifted == !shifted) return i + 32;
    }
    else {
      if (s_char_to_vkey[i].vk == vk && s_char_to_vkey[i].shifted == shifted) return i + 32;
    }
  }
  return 0;
}

/*
* KMX_BOOL ProcessEvent();
*
* Parameters: none
*
* Returns:  TRUE if keystroke should be eaten
*
*   Called by:  FilterFunc
*
* ProcessEvent organizes the messages and gives them to the appropriate routines to
* process, and checks the state of Windows for the keyboard handling.
*/

KMX_BOOL KMX_Processor::ProcessEvent(km_kbp_state *state, KMX_UINT vkey, KMX_DWORD modifiers)
{
  LPKEYBOARD kbd = m_keyboard.Keyboard;

  m_kbp_state = state;

  if (m_environment.capsLock())
    modifiers |= CAPITALFLAG;

  m_state.vkey = vkey;
  m_state.charCode = VKeyToChar(modifiers, vkey);
  m_modifiers = modifiers;
  m_state.LoopTimes = 0;

  if (kbd->StartGroup[BEGIN_UNICODE] == (KMX_DWORD) -1) {
    DebugLog("Non-Unicode keyboards are not supported.");
    return FALSE;
  }

  LPGROUP gp = &kbd->dpGroupArray[kbd->StartGroup[BEGIN_UNICODE]];

  KMX_BOOL fOutputKeystroke = FALSE;

  ProcessGroup(gp, &fOutputKeystroke);

  m_kbp_state = nullptr;

  return !fOutputKeystroke;
}

/*
* PRIVATE KMX_BOOL ProcessGroup(LPGROUP gp);
*
* Parameters: gp    Pointer to group to process inside
*
* Returns:  TRUE if messages are to be sent,
*       and FALSE if no messages are to be sent.
*
*   Called by:  ProcessEvent, recursive inside groups
*
* ProcessKey is where the keystroke conversion and output takes place.  This routine
* has a lot of crucial code in it!
*/

KMX_BOOL KMX_Processor::ProcessGroup(LPGROUP gp, KMX_BOOL *pOutputKeystroke)
{
  KMX_DWORD i;
  LPKEY kkp = NULL;
  PKMX_WCHAR p;
  int sdmfI;

    /*
   If the number of nested groups goes higher than 50, then break out - this is
   a limitation of stack size.  This is basically a catch-all for freaky apps that
   cause message loopbacks and nasty things like that.  Okay, it's really a catch all
   for bugs!  This means the user's system shouldn't hang.
  */

  LPKEYBOARD kbd = m_keyboard.Keyboard;

  sdmfI = -1;

  for(i = 0; i < kbd->cxGroupArray; i++)
    if(gp == &kbd->dpGroupArray[i])
    {
      sdmfI = i;
      break;
    }

  if(++m_state.LoopTimes > 50)
  {
    DebugLog("Aborting output: m_state.LoopTimes exceeded.");
    m_state.StopOutput = TRUE;
    return FALSE;
  }

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

  DebugLog("m_state.vkey: %s shiftFlags: %x; charCode: %X", Debug_VirtualKey(m_state.vkey), m_modifiers, m_state.charCode);   // I4582

  if(gp)
  {
    for(kkp = gp->dpKeyArray, i=0; i < gp->cxKeyArray; i++, kkp++)
    {
      if(!ContextMatch(kkp)) continue;
      if(!gp->fUsingKeys)
      {
        if(kkp->dpContext[0] != 0) break; else continue;
      }

      //if(kkp->Key == m_state.vkey)
      //SendDebugMessageFormat(m_state.msg.hwnd, sdmKeyboard, 0, "kkp->Key: %d kkp->ShiftFlags: %x",
      //  kkp->Key, kkp->ShiftFlags);

      /* Keyman 6.0: support Virtual Characters */
      if(IsEquivalentShift(kkp->ShiftFlags, m_modifiers))
      {
        if(kkp->Key > VK__MAX && kkp->Key == m_state.vkey) break; // I3438   // I4582
        else if(kkp->Key == m_state.vkey) break;   // I4169
      }
      else if(kkp->ShiftFlags == 0 && kkp->Key == m_state.charCode && m_state.charCode != 0) break;
    }
  }

  if(!gp || i == gp->cxKeyArray)
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

    DebugLog("No match was found in group %d of %d", sdmfI, m_keyboard.Keyboard->cxGroupArray);

    if(!gp || (m_state.charCode == 0 && gp->fUsingKeys))   // I4585
        // 7.0.241.0: I1133 - Fix mismatched parentheses on m_state.charCode - ie. we don't want to output this letter if !gp->fUsingKeys
    {
      KMX_BOOL fIsBackspace = m_state.vkey == KM_KBP_VKEY_BKSP && (m_modifiers & (LCTRLFLAG|RCTRLFLAG|LALTFLAG|RALTFLAG)) == 0;   // I4128

      if(fIsBackspace) {   // I4838   // I4933
        PKMX_WCHAR pdeletecontext = m_context.Buf(1);   // I4933
        if(!pdeletecontext || *pdeletecontext == 0) {   // I4933
          m_actions.QueueAction(QIT_INVALIDATECONTEXT, 0);
          *pOutputKeystroke = TRUE;   // I4933
          return FALSE;   // I4933
        }
        m_actions.QueueAction(QIT_BACK, BK_BACKSPACE);   // I4933
      } else {   // I4024   // I4128   // I4287   // I4290
        DebugLog(" ... IsLegacy = FALSE; IsTIP = TRUE");   // I4128
        m_actions.QueueAction(QIT_INVALIDATECONTEXT, 0);
        *pOutputKeystroke = TRUE;
        return FALSE;
      }
    }
    else if (gp->dpNoMatch != NULL && *gp->dpNoMatch != 0)
    {
      /* NoMatch rule found, and is a character key */
      PostString(gp->dpNoMatch, m_keyboard.Keyboard, NULL, pOutputKeystroke);
    }
    else if (m_state.charCode != 0 && m_state.charCode != 0xFFFF && gp->fUsingKeys)
    {
      /* No rule found, is a character key */
      m_actions.QueueAction(QIT_CHAR, m_state.charCode);
    }

    return TRUE;
  }

  DebugLog("match found in rule %d", i);

  /*
   Save the context that will be used for output when the 'context' keyword is used.
   For each deadkey, we need to add 2 characters; look in related stores as well...
  */

  assert(kkp != NULL);

  // 11 Aug 2003 - I25(v6) - mcdurdin - CODE_NUL context support
  if(*kkp->dpContext == UC_SENTINEL && *(kkp->dpContext+1) == CODE_NUL)
    u16cpy(m_miniContext, /*GLOBAL_ContextStackSize,*/ m_context.Buf(xstrlen_ignoreifopt(kkp->dpContext)-1) /*, GLOBAL_ContextStackSize*/);  // I3162   // I3536
  else
    u16cpy(m_miniContext, /*GLOBAL_ContextStackSize,*/ m_context.Buf(xstrlen_ignoreifopt(kkp->dpContext)) /*, GLOBAL_ContextStackSize*/);  // I3162   // I3536

  m_miniContext[GLOBAL_ContextStackSize-1] = 0;

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
  if(*p != UC_SENTINEL || *(p+1) != CODE_CONTEXT)
  {
    for(p = decxstr((KMX_WCHAR *) u16chr(m_miniContext, 0)); p >= m_miniContext; p = decxstr(p))
    {
      if(*p == UC_SENTINEL)
        switch(*(p+1))
        {
          case CODE_DEADKEY: m_actions.QueueAction(QIT_BACK, BK_DEADKEY); break;
          case CODE_NUL: break; // 11 Aug 2003 - I25(v6) - mcdurdin - CODE_NUL context support
        }
      else
      {
        m_actions.QueueAction(QIT_BACK, 0);
      }
    }
    p = kkp->dpOutput;
  }
  else
    p = incxstr(p);       // otherwise, the "context" entry has to be jumped over

  /* Use PostString to post the rest of the output string. */

  if(PostString(p, m_keyboard.Keyboard, NULL, pOutputKeystroke) == psrCheckMatches)
  {
    if(gp->dpMatch && *gp->dpMatch)
    {
      PostString(gp->dpMatch, m_keyboard.Keyboard, NULL, pOutputKeystroke);
    }
  }

  return TRUE;
}

/*
* int PostString( PKMX_CHAR str, KMX_BOOL *useMode, LPMSG mp,
* LPKEYBOARD lpkb );
*
* Parameters: str   Pointer to string to send
*       useMode Pointer to KMX_BOOL about whether a "use" command was found
*       mp    Pointer to MSG structure to copy in outputting messages
*       lpkb  Pointer to global keyboard structure
*
* Returns:  0 to continue, 1 and 2 to return.
*
*   Called by:  ProcessKey
*
* PostString posts a string of "context", "index", "beep", characters and virtual keys
* to the active application, via the Keyman PostKey buffer.
*/

int KMX_Processor::PostString(PKMX_WCHAR str, LPKEYBOARD lpkb, PKMX_WCHAR endstr, KMX_BOOL *pOutputKeystroke)
{
  PKMX_WCHAR p, q, temp;
  LPSTORE s;
  int n1, n2;
  int i, n, shift;
  KMX_BOOL FoundUse = FALSE;

  // TODO: Refactor to use incxstr
  for(p = str; *p && (p < endstr || !endstr); p++)
  {
    if(*p == UC_SENTINEL)
     switch(*(++p))
     {
      case CODE_EXTENDED:       // Start of a virtual key section w/shift codes
        p++;

        shift = *p; //(*p<<8) | *(p+1);
        m_actions.QueueAction(QIT_VSHIFTDOWN, shift);

        p++;

        m_actions.QueueAction(QIT_VKEYDOWN, *p);
        m_actions.QueueAction(QIT_VKEYUP, *p);

        m_actions.QueueAction(QIT_VSHIFTUP, shift);

        p++; // CODE_EXTENDEDEND
        ////// CODE_EXTENDEDEND will be incremented by loop

        break;

      case CODE_DEADKEY:        // A deadkey to be output
        p++;
        m_actions.QueueAction(QIT_DEADKEY, *p);
        break;
      case CODE_BEEP:         // Sound an 'iconasterisk' beep
        m_actions.QueueAction(QIT_BELL, 0);
        break;
      case CODE_CONTEXT:        // copy the context to the output
        for(q = m_miniContext; *q; q++) {
          m_actions.QueueAction(QIT_CHAR, *q);
        }
        break;
      case CODE_CONTEXTEX:
        p++;
        for(q = m_miniContext, i = 0; *q && i < *p-1; i++, q=incxstr(q));
        if(*q) {
          m_actions.QueueAction(QIT_CHAR, *q);
          if(Uni_IsSurrogate1(*q) && Uni_IsSurrogate2(*(q+1))) {
            m_actions.QueueAction(QIT_CHAR, *(q+1));
          }
        }
        break;
      case CODE_RETURN:       // stop processing and start PostAllKeys
        m_state.StopOutput = TRUE;
        return psrPostMessages;

      case CODE_CALL:
        p++;
        DebugLog("CallDLL not supported [store=%d].\n", *p-1);
        FoundUse = TRUE;
        break;
      case CODE_USE:          // use another group
        p++;
        ProcessGroup(&lpkb->dpGroupArray[*p-1], pOutputKeystroke);
        if(m_state.StopOutput) return psrPostMessages;
        FoundUse = TRUE;
        break;
      case CODE_CLEARCONTEXT:
        // no longer supported, no-op
        break;
      case CODE_INDEX:
        p++;
        s = &m_keyboard.Keyboard->dpStoreArray[*p - 1];
        p++;

        n = m_indexStack[*p - 1];
        for(temp = s->dpString; *temp && n > 0; temp = incxstr(temp), n--);
        PostString(temp, lpkb, incxstr(temp), pOutputKeystroke);
        break;
      case CODE_SETOPT:
        p++;
        n1 = *p - 1;
        p++;
        n2 = *p - 1;
        GetOptions()->Set(n1, n2);
        break;
      case CODE_RESETOPT:
        p++;
        n1 = *p - 1;
        GetOptions()->Reset(m_kbp_state->processor(), n1);
        break;
      case CODE_SAVEOPT:
        p++;
        n1 = *p - 1;
        GetOptions()->Save(*m_kbp_state, n1);
        break;
      case CODE_IFSYSTEMSTORE:
        p+=3;
        break;
      case CODE_SETSYSTEMSTORE:
        p+=2;
        break;
      }
    else {
      m_actions.QueueAction(QIT_CHAR, *p);
    }
  }
  return FoundUse ? psrPostMessages : psrCheckMatches;
}


KMX_BOOL KMX_Processor::IsMatchingBaseLayout(PKMX_WCHAR layoutName)  // I3432
{
  KMX_BOOL bEqual = u16icmp(layoutName, static_cast<const km_kbp_cp *>(m_environment.baseLayout().c_str())) == 0 ||   // I4583
                u16icmp(layoutName, static_cast<const km_kbp_cp*>(m_environment.baseLayoutAlt().c_str())) == 0;   // I4583

  return bEqual;
}

KMX_BOOL KMX_Processor::IsMatchingPlatformString(PKMX_WCHAR platform)  // I3432
{
  // TODO retrieve platform string from client environment
  // TODO cleanup to use a vector<string>.
  PKMX_WCHAR t = new KMX_WCHAR[u16len(m_environment.platform().c_str()) + 1], context = NULL;
  u16cpy(t, /*wcslen(s->dpString)+1,*/ m_environment.platform().c_str());
  PKMX_WCHAR p = u16tok(t, u' ', &context);
  while (p != NULL) {
    if (u16icmp(platform, p) == 0) {
      delete [] t;
      return TRUE;
    }
    p = u16tok(NULL, u' ', &context);
  }

  delete [] t;
  return FALSE;
}

KMX_BOOL KMX_Processor::IsMatchingPlatform(LPSTORE s)  // I3432
{
  PKMX_WCHAR t = new KMX_WCHAR[u16len(s->dpString)+1];
  u16cpy(t, /*wcslen(s->dpString)+1,*/ s->dpString);
  PKMX_WCHAR context = NULL;
  PKMX_WCHAR platform = u16tok(t, u' ', &context);
  while(platform != NULL)
  {
    if(!IsMatchingPlatformString(platform))
    {
      s->dwSystemID = TSS_PLATFORM_NOMATCH;
      delete [] t;
      return FALSE;
    }
    platform = u16tok(NULL, u' ', &context);
  }

  s->dwSystemID = TSS_PLATFORM_MATCH;
  delete [] t;
  return TRUE;
}

/*
* KMX_BOOL ContextMatch( LPKEY kkp );
*
* Parameters: kkp   Rule to compare
*
* Returns:    0 on OK, 1 on not equal
*
*   Called by:  ProcessKey
*
* ContextMatch compares the context of a rule with the current context.
*/

KMX_BOOL KMX_Processor::ContextMatch(LPKEY kkp)
{
  KMX_WORD /*i,*/ n;
  PKMX_WCHAR p, q, qbuf, temp;
  PKMX_WORD indexp;
  LPSTORE s, t;
  KMX_BOOL bEqual;

  memset(m_indexStack, 0, GLOBAL_ContextStackSize*sizeof(KMX_WORD));

  p = kkp->dpContext;

  if(*p == 0)
  {
    return TRUE;
  }

  if(*p == UC_SENTINEL && *(p+1) == CODE_NUL)
  {
    // If context buf is longer than the context, then obviously not start of doc.
    if(m_context.Buf(xstrlen_ignoreifopt(p))) return FALSE;
    p = incxstr(p);
    if(*p == 0) return TRUE;
  }

  for(PKMX_WCHAR pp = p; pp && *pp; pp = incxstr(pp))
  {
    if(*pp == UC_SENTINEL && *(pp+1) == CODE_IFOPT)
    {
      s = &m_keyboard.Keyboard->dpStoreArray[(*(pp+2))-1];
      t = &m_keyboard.Keyboard->dpStoreArray[(*(pp+4))-1];

      bEqual = u16cmp(s->dpString, t->dpString) == 0;
      if(*(pp+3) == 1 && bEqual) return FALSE;
      if(*(pp+3) == 2 && !bEqual) return FALSE;
    }
    else if(*pp == UC_SENTINEL && *(pp+1) == CODE_IFSYSTEMSTORE)
    {
      KMX_DWORD dwSystemID = *(pp+2)-1;
      t = &m_keyboard.Keyboard->dpStoreArray[(*(pp+4))-1];
      switch(dwSystemID)
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
          PKMX_WCHAR ss = GetSystemStore(m_keyboard.Keyboard, dwSystemID);
          if(ss == NULL) return FALSE;
          bEqual = u16cmp(ss, t->dpString) == 0;
        }
      }

      if(*(pp+3) == 1 && bEqual) return FALSE;
      if(*(pp+3) == 2 && !bEqual) return FALSE;
    }
  }

  q = qbuf = m_context.Buf(xstrlen_ignoreifopt(p));
  if(!q)
  {
    return FALSE; // context buf is too short!
  }
  indexp = m_indexStack;

  for(; *p && *q; p = incxstr(p))
  {
    *indexp = 0;

    if(*p == UC_SENTINEL)
    {
      switch(*(p+1))
      {
      case CODE_DEADKEY:
        if(*q != UC_SENTINEL || *(q+1) != CODE_DEADKEY || *(q+2) != *(p+2))
        {
          return FALSE;
        }
        break;
      case CODE_ANY:
        s = &m_keyboard.Keyboard->dpStoreArray[(*(p+2))-1];

        temp = xstrchr(s->dpString, q);

        if(temp != NULL)
          *indexp = (KMX_WORD) xstrpos(temp, s->dpString);

        else
          return FALSE;
        break;
      case CODE_NOTANY:
        s = &m_keyboard.Keyboard->dpStoreArray[(*(p+2))-1];

        if((temp = xstrchr(s->dpString, q)) != NULL)
          return FALSE;

        break;
      case CODE_INDEX:
        s = &m_keyboard.Keyboard->dpStoreArray[(*(p+2))-1];
        *indexp = n = m_indexStack[(*(p+3))-1];

        for(temp = s->dpString; *temp && n > 0; temp = incxstr(temp), n--);
        if(n != 0) return FALSE;
        if(xchrcmp(temp, q) != 0) return FALSE;
        break;
      case CODE_CONTEXTEX:
        // only the nth character
        for(n = *(p+2) - 1, temp = qbuf; temp < q && n > 0; n--, temp = incxstr(temp));
        if(n == 0)
          if(xchrcmp(temp, q) != 0) return FALSE;
        break;
      case CODE_IFOPT:
      case CODE_IFSYSTEMSTORE:
        indexp++;
        continue; // don't increment q
      default:
        return FALSE;
      }
    }
    else if(xchrcmp(p, q) != 0)
    {
      return FALSE;
    }
    indexp++;
    q = incxstr(q);
  }

  while(*p == UC_SENTINEL && (*(p+1) == CODE_IFOPT || *(p+1) == CODE_IFSYSTEMSTORE)) p = incxstr(p); // already tested

  return *p == *q; /*at least one must ==0 at this point*/
}

KMX_Actions *KMX_Processor::GetActions() {
  return &m_actions;
}

KMX_Context *KMX_Processor::GetContext() {
  return &m_context;
}

KMX_Options *KMX_Processor::GetOptions() {
  return &m_options;
}

KMX_Options const *KMX_Processor::GetOptions() const {
  return &m_options;
}

KMX_Environment *KMX_Processor::GetEnvironment() {
  return &m_environment;
}

KMX_Environment const *KMX_Processor::GetEnvironment() const {
  return &m_environment;
}

LPINTKEYBOARDINFO KMX_Processor::GetKeyboard() {
  return &m_keyboard;
}


KMX_BOOL KMX_Processor::ReleaseKeyboardMemory(LPKEYBOARD kbd)
{
  if (!kbd) return TRUE;
  delete kbd;
  return TRUE;
}

PKMX_WCHAR KMX_Processor::GetSystemStore(LPKEYBOARD kb, KMX_DWORD SystemID)
{
  for (KMX_DWORD i = 0; i < kb->cxStoreArray; i++)
    if (kb->dpStoreArray[i].dwSystemID == SystemID) return kb->dpStoreArray[i].dpString;

  return NULL;
}
