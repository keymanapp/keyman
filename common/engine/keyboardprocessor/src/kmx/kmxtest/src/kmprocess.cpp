#include "pch.h"   // I4575

BOOL fOutputKeystroke;

char *getcontext_debug() {
  //return "";
  AIWin2000Unicode *app = GetApp();
	return Debug_UnicodeString(app->ContextBufMax(128));
}

/*
*	BOOL ProcessHook();
*
*	Parameters:	none
*
*	Returns:	TRUE if keystroke should be eaten
*
*   Called by:  FilterFunc
*
*	ProcessHook organizes the messages and gives them to the appropriate routines to
*	process, and checks the state of Windows for the keyboard handling.
*/

BOOL ProcessHook()
{
  AIWin2000Unicode *app = GetApp();
  LPKEYBOARD kbd = GetKeyboard()->Keyboard;
  LPGROUP gp = &kbd->dpGroupArray[kbd->StartGroup[BEGIN_UNICODE]];

	fOutputKeystroke = FALSE;
   
	ProcessGroup(gp);

  app->SetCurrentShiftState(g_shiftState);
	app->SendActions();   // I4196

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

BOOL ProcessGroup(LPGROUP gp)
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
  AIWin2000Unicode *app = GetApp();
  LPKEYBOARD kbd = GetKeyboard()->Keyboard;

	sdmfI = -1;

	for(i = 0; i < kbd->cxGroupArray; i++)
		if(gp == &kbd->dpGroupArray[i])
		{
			sdmfI = i;
			break;
		}

	if(++_td->state.LoopTimes > 50)
	{
    DebugLog("Aborting output: state.LoopTimes exceeded.");
		_td->state.StopOutput = TRUE;
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

  DebugLog("state.vkey: %s shiftFlags: %x; charCode: %X", Debug_VirtualKey(_td->state.vkey), g_shiftState, _td->state.charCode);   // I4582

	if(gp)
	{
		for(kkp = gp->dpKeyArray, i=0; i < gp->cxKeyArray; i++, kkp++)
		{
			if(!ContextMatch(kkp)) continue;
			if(!gp->fUsingKeys)
			{
				if(kkp->dpContext[0] != 0) break; else continue;
			}
	
			//if(kkp->Key == state.vkey)
			//SendDebugMessageFormat(state.msg.hwnd, sdmKeyboard, 0, "kkp->Key: %d kkp->ShiftFlags: %x", 
			//	kkp->Key, kkp->ShiftFlags);

			/* Keyman 6.0: support Virtual Characters */ 
			if(IsEquivalentShift(kkp->ShiftFlags, g_shiftState))
			{
        if(kkp->Key > VK__MAX && kkp->Key == _td->state.vkey) break;	// I3438   // I4582
				else if(kkp->Key == _td->state.vkey) break;   // I4169
			}
			else if(kkp->ShiftFlags == 0 && kkp->Key == _td->state.charCode && _td->state.charCode != 0) break;
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

    DebugLog("No match was found in group %d of %d", sdmfI, g_keyboard.Keyboard->cxGroupArray);

		if(!gp || (_td->state.charCode == 0 && gp->fUsingKeys))   // I4585
        // 7.0.241.0: I1133 - Fix mismatched parentheses on state.charCode - ie. we don't want to output this letter if !gp->fUsingKeys
		{
      BOOL fIsBackspace = _td->state.vkey == VK_BACK && (g_shiftState & (LCTRLFLAG|RCTRLFLAG|LALTFLAG|RALTFLAG)) == 0;   // I4128

      if(/*_td->app->DebugControlled() &&*/ fIsBackspace) {   // I4838   // I4933
        	//if(_td->state.msg.message == wm_keymankeydown) 
				  //	_td->app->QueueAction(QIT_BACK, BK_BACKSPACE);
        PWCHAR pdeletecontext = app->ContextBuf(1);   // I4933
        if(!pdeletecontext || *pdeletecontext == 0) {   // I4933
          fOutputKeystroke = TRUE;   // I4933
          return FALSE;   // I4933
        }
				app->QueueAction(QIT_BACK, BK_BACKSPACE);   // I4933
      } else if(!fIsBackspace) {   // I4024   // I4128   // I4287   // I4290
        DebugLog(" ... IsLegacy = FALSE; IsTIP = TRUE");   // I4128
        fOutputKeystroke = TRUE;
        return FALSE;
      }
			  //fOutputKeystroke = TRUE; return FALSE; // Don't swallow keystroke   // I3577
        ///SendDebugMessageFormat(_td->state.msg.hwnd, sdmKeyboard, 0, " ... IsLegacy = TRUE; IsTIP = TRUE");

			/*
             If the key is not a character key (white keys), or not processing, then we must init the stack -
             unknown keys do things like moving position in the context, so must clear.
            */
			if(fIsBackspace)   // I4128
			{
				/*
				 Must have special handling for VK_BACK: delete a character from the context stack
				 This only fires if the keyboard has no rule for backspace.
				*/

//				if(_td->state.msg.message == wm_keymankeydown)    // I4933
	//				_td->app->QueueAction(QIT_BACK, BK_BACKSPACE);   // I4933
			}
			else
			{
				//app->NoSetShift = FALSE;

				DWORD dw = _td->state.vkey;
				if(dw == 0x05) dw = VK_RETURN;    // I649 - VK_ENTER and K_NPENTER

				if(_td->state.isExtended) dw |= QVK_EXTENDED;	// Extended key flag  // I3438

				app->QueueAction(QIT_VSHIFTDOWN, g_shiftState);  // 15/05/2001 - fixing I201 -- enabled line
        app->QueueAction(QIT_VKEYDOWN, dw);
        app->QueueAction(QIT_VKEYUP, dw);
  		  app->QueueAction(QIT_VSHIFTUP, g_shiftState);
			}
		}
		else if (gp->dpNoMatch != NULL && *gp->dpNoMatch != 0)
		{
			/* NoMatch rule found, and is a character key */ 
			PostString(gp->dpNoMatch, g_keyboard.Keyboard, NULL);
		}
		else if (_td->state.charCode != 0 && _td->state.charCode != 0xFFFF && gp->fUsingKeys)
		{
			/* No rule found, is a character key */ 
			// 7.0.239.0: I994 - Workaround output order issues - we will use the TSF to output all characters...
			// if(app->Type1() == AIType_TIP) { fOutputKeystroke = TRUE; return FALSE; } // Don't swallow keystroke

			app->QueueAction(QIT_CHAR, _td->state.charCode);
		}

		return TRUE;
	}

  DebugLog("match found in rule %d", i);

	_td->state.NoMatches = FALSE;

	/*
	 Save the context that will be used for output when the 'context' keyword is used.
	 For each deadkey, we need to add 2 characters; look in related stores as well...
	*/

  assert(kkp != NULL);

	// 11 Aug 2003 - I25(v6) - mcdurdin - CODE_NUL context support
	if(*kkp->dpContext == UC_SENTINEL && *(kkp->dpContext+1) == CODE_NUL)
    wcsncpy_s(_td->miniContext, GLOBAL_ContextStackSize, app->ContextBuf(xstrlen_ignoreifopt(kkp->dpContext)-1), GLOBAL_ContextStackSize);  // I3162   // I3536
	else
	  wcsncpy_s(_td->miniContext, GLOBAL_ContextStackSize, app->ContextBuf(xstrlen_ignoreifopt(kkp->dpContext)), GLOBAL_ContextStackSize);  // I3162   // I3536

  _td->miniContext[GLOBAL_ContextStackSize-1] = 0;

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
		for(p = _td->miniContext; *p; p = incxstr(p))
		{
			if(*p == UC_SENTINEL)
				switch(*(p+1))
				{
			    case CODE_DEADKEY: app->QueueAction(QIT_BACK, BK_DEADKEY); break;
					case CODE_NUL: break;	// 11 Aug 2003 - I25(v6) - mcdurdin - CODE_NUL context support
        }
      else 
			{
				app->QueueAction(QIT_BACK, 0);
			}
		}
    p = kkp->dpOutput;
	}
	else 
    p = incxstr(p);				// otherwise, the "context" entry has to be jumped over

	/* Use PostString to post the rest of the output string. */

	if(PostString(p, g_keyboard.Keyboard, NULL) == psrCheckMatches)
	{
		if(gp->dpMatch && *gp->dpMatch)
		{
			PostString(gp->dpMatch, g_keyboard.Keyboard, NULL);
		}
	}

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

int PostString(PWSTR str, LPKEYBOARD lpkb, PWSTR endstr)
{
	PWSTR p, q, temp;
  LPSTORE s;
  int n1, n2;
	int i, n, shift;
	BOOL FoundUse = FALSE;

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;
  AIWin2000Unicode *app = GetApp();

  // TODO: Refactor to use incxstr
	for(p = str; *p && (p < endstr || !endstr); p++)
    {
        if(*p == UC_SENTINEL)
		    switch(*(++p))
            {
		    case CODE_EXTENDED:				// Start of a virtual key section w/shift codes
			    p++;
				
				shift = *p; //(*p<<8) | *(p+1);
				app->QueueAction(QIT_VSHIFTDOWN, shift);

				p++;

				app->QueueAction(QIT_VKEYDOWN, *p);
				app->QueueAction(QIT_VKEYUP, *p);

				app->QueueAction(QIT_VSHIFTUP, shift);
				
				p++; // CODE_EXTENDEDEND
				////// CODE_EXTENDEDEND will be incremented by loop

				//app->QueueAction(QIT_VSHIFTUP, shift);
				break;

			case CODE_DEADKEY:				// A deadkey to be output
			  p++;
				app->QueueAction(QIT_DEADKEY, *p);
			  break;
		  case CODE_BEEP:					// Sound an 'iconasterisk' beep
			  app->QueueAction(QIT_BELL, 0);
			  break;
		  case CODE_CONTEXT:				// copy the context to the output
			  for(q = _td->miniContext; *q; q++) {
			    app->QueueAction(QIT_CHAR, *q);
        }
			  break;
			case CODE_CONTEXTEX:
				p++;
				for(q = _td->miniContext, i = 0; *q && i < *p-1; i++, q=incxstr(q));
				if(*q) {
          app->QueueAction(QIT_CHAR, *q);
          if(Uni_IsSurrogate1(*q) && Uni_IsSurrogate2(*(q+1))) {
            app->QueueAction(QIT_CHAR, *(q+1));
          }
        }
				break;
		  case CODE_RETURN:				// stop processing and start PostAllKeys
			  _td->state.StopOutput = TRUE;
			  return psrPostMessages;

			case CODE_CALL:
				p++;
        DebugLog("CallDLL not supported [store=%d].\n", *p-1);
				FoundUse = TRUE;
				break; 
			case CODE_USE:					// use another group
			  p++;
			  ProcessGroup(&lpkb->dpGroupArray[*p-1]);
			  if(_td->state.StopOutput) return psrPostMessages;
				FoundUse = TRUE;
			  break;
		  case CODE_CLEARCONTEXT:
        // no longer supported, no-op
			  break;
      case CODE_INDEX:
       	p++;
        s = &g_keyboard.Keyboard->dpStoreArray[*p - 1];
		    p++;

				n = _td->IndexStack[*p - 1];
				for(temp = s->dpString; *temp && n > 0; temp = incxstr(temp), n--);
				PostString(temp, lpkb, incxstr(temp));
				break;
      case CODE_SETOPT:
        p++;
        n1 = *p - 1;
		    p++;
        n2 = *p - 1;
        SetKeyboardOption(&g_keyboard, n1, n2);
        break;
      case CODE_RESETOPT:
        p++;
        n1 = *p - 1;
        ResetKeyboardOption(&g_keyboard, n1);
        break;
      case CODE_SAVEOPT:
        p++;
        n1 = *p - 1;
        SaveKeyboardOption(&g_keyboard, n1);
        break;
      case CODE_IFSYSTEMSTORE:
        p+=3;
        break;
      case CODE_SETSYSTEMSTORE:
        p+=2;
        break;
      }
    else
			app->QueueAction(QIT_CHAR, *p);
  }
	return FoundUse ? psrPostMessages : psrCheckMatches;
}


BOOL IsMatchingBaseLayout(PWCHAR layoutName)  // I3432
{
  BOOL bEqual = _wcsicmp(layoutName, g_baseLayout) == 0 ||   // I4583
                _wcsicmp(layoutName, g_baseLayoutAlt) == 0;   // I4583

  return bEqual;
}

BOOL IsMatchingPlatformString(PWCHAR platform)  // I3432
{
  // TODO retrieve platform string from client environment
  return
    _wcsicmp(platform, L"windows") == 0 ||
    _wcsicmp(platform, L"desktop") == 0 ||
    _wcsicmp(platform, L"hardware") == 0 ||
    _wcsicmp(platform, L"native") == 0;
}

BOOL IsMatchingPlatform(LPSTORE s)  // I3432
{
  PWCHAR t = new WCHAR[wcslen(s->dpString)+1];
  wcscpy_s(t, wcslen(s->dpString)+1, s->dpString);
  PWCHAR context = NULL;
  PWCHAR platform = wcstok_s(t, L" ", &context);
  while(platform != NULL)
  {
    if(!IsMatchingPlatformString(platform))
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

BOOL ContextMatch(LPKEY kkp)
{
	WORD /*i,*/ n;
	PWSTR p, q, qbuf, temp;
	LPWORD indexp;
	LPSTORE s, t;
  BOOL bEqual;

	//SendDebugMessageFormat(state.msg.hwnd, sdmKeyboard, kkp->Line, "ContextMatch: ENTER [%d]", kkp->Line);
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;
  AIWin2000Unicode *app = GetApp();

	memset(_td->IndexStack, 0, GLOBAL_ContextStackSize*sizeof(WORD));  // I3158   // I3524
	
	p = kkp->dpContext;

	if(*p == 0)
	{
		//SendDebugMessageFormat(state.msg.hwnd, sdmKeyboard, kkp->Line, "ContextMatch: EXIT TRUE -> no rule context");
		return TRUE;
	}

	/* 11 Aug 2003 - I25(v6) - mcdurdin - test for CODE_NUL */

	if(*p == UC_SENTINEL && *(p+1) == CODE_NUL)
	{
		// If context buf is longer than the context, then obviously not start of doc.
		if(app->ContextBuf(xstrlen_ignoreifopt(p))) return FALSE;  // I2484 - Fix bug with if() following nul in same statement
		p = incxstr(p);
		if(*p == 0) return TRUE;
	}

  for(PWCHAR pp = p; pp && *pp; pp = incxstr(pp))
  {
    if(*pp == UC_SENTINEL && *(pp+1) == CODE_IFOPT)
    {
    	s = &g_keyboard.Keyboard->dpStoreArray[(*(pp+2))-1];  // I2590
      t = &g_keyboard.Keyboard->dpStoreArray[(*(pp+4))-1];  // I2590
      
      bEqual = wcscmp(s->dpString, t->dpString) == 0;
      if(*(pp+3) == 1 && bEqual) return FALSE;  // I2590
      if(*(pp+3) == 2 && !bEqual) return FALSE;  // I2590
    }
    else if(*pp == UC_SENTINEL && *(pp+1) == CODE_IFSYSTEMSTORE)  // I3432
    {
      DWORD dwSystemID = *(pp+2)-1;
      t = &g_keyboard.Keyboard->dpStoreArray[(*(pp+4))-1];  // I2590
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
          PWCHAR ss = GetSystemStore(g_keyboard.Keyboard, dwSystemID);
          if(ss == NULL) return FALSE;
          bEqual = wcscmp(ss, t->dpString) == 0;
        }
      }
      
      if(*(pp+3) == 1 && bEqual) return FALSE;  // I2590
      if(*(pp+3) == 2 && !bEqual) return FALSE;  // I2590
    }
  }

	q = qbuf = app->ContextBuf(xstrlen_ignoreifopt(p));
	if(!q)
	{
		//SendDebugMessageFormat(state.msg.hwnd, sdmKeyboard, kkp->Line, "ContextMatch: EXIT FALSE -> context too short");
		return FALSE;	// context buf is too short!
	}
	indexp = _td->IndexStack;

	//SendDebugMessageFormat(state.msg.hwnd, sdmKeyboard, kkp->Line, "ContextMatch: [%d] Rule: %s", kkp->Line, format_unicode_debug(kkp->dpContext));
	//SendDebugMessageFormat(state.msg.hwnd, sdmKeyboard, kkp->Line, "ContextMatch: [%d] Test: %s", kkp->Line, format_unicode_debug(q));

	for(; *p && *q; p = incxstr(p))
	{
		//SendDebugMessageFormat(state.msg.hwnd, sdmKeyboard, kkp->Line, "ContextMatch: p:%x q:%x", *p, *q);
		*indexp = 0;

		if(*p == UC_SENTINEL)
		{
			switch(*(p+1))
			{
			case CODE_DEADKEY:
				if(*q != UC_SENTINEL || *(q+1) != CODE_DEADKEY || *(q+2) != *(p+2))
				{
	//	SendDebugMessageFormat(state.msg.hwnd, sdmKeyboard, kkp->Line, "ContextMatch: EXIT FALSE -> deadkeys don't match %x %x %x != %x %x %x",
	//				*p, *(p+1), *(p+2), *q, *(q+1), *(q+2));
					return FALSE;
				}
				break;
			case CODE_ANY:
	      s = &g_keyboard.Keyboard->dpStoreArray[(*(p+2))-1];

        temp = xstrchr(s->dpString, q);

        /*SendDebugMessageFormat(state.msg.hwnd, sdmKeyboard ,kkp->Line, "ContextMatch: CODE_ANY [%x %x %x %x %x %x %x %x %x %x] [%x %x %x] %d", 
          s->dpString[0], s->dpString[1], s->dpString[2], 
          s->dpString[3], s->dpString[4], s->dpString[5], 
          s->dpString[6], s->dpString[7], s->dpString[8], 
          s->dpString[9],
          q[0], q[1], q[2],
          (temp ? (int)(temp-s->dpString) : 0));*/
        
        if(temp != NULL)  // I1622
          *indexp = (WORD) xstrpos(temp, s->dpString);

				else
					return FALSE;
				break;
			case CODE_NOTANY:
	      s = &g_keyboard.Keyboard->dpStoreArray[(*(p+2))-1];

        if((temp = xstrchr(s->dpString, q)) != NULL)   // I1622
          return FALSE; 

				break;
			case CODE_INDEX:
	    	s = &g_keyboard.Keyboard->dpStoreArray[(*(p+2))-1];
				*indexp = n = _td->IndexStack[(*(p+3))-1];

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
      case CODE_IFSYSTEMSTORE:  // I3432
        indexp++;
        continue; // don't increment q
			default:
				return FALSE;
			}
		}
		else if(xchrcmp(p, q) != 0) //GetSuppChar(p) != GetSuppChar(q)) 
		{
    	//SendDebugMessageFormat(state.msg.hwnd, sdmKeyboard, kkp->Line, "ContextMatch: [%d] FAIL: %s", kkp->Line, format_unicode_debug(p));
	    //SendDebugMessageFormat(state.msg.hwnd, sdmKeyboard, kkp->Line, "ContextMatch: [%d] FAIL: %s", kkp->Line, format_unicode_debug(q));
			//SendDebugMessageFormat(state.msg.hwnd, sdmKeyboard, kkp->Line, "ContextMatch: EXIT FALSE -> chrs don't match");
			return FALSE;
		}
		indexp++;
    q = incxstr(q);
	}

  while(*p == UC_SENTINEL && (*(p+1) == CODE_IFOPT || *(p+1) == CODE_IFSYSTEMSTORE)) p = incxstr(p); // already tested  // I3432

	//SendDebugMessageFormat(state.msg.hwnd, sdmKeyboard, kkp->Line, "ContextMatch: EXIT %s -> END OF FUNCTION",
	//	*p == *q ? "TRUE" : "FALSE");
  return *p == *q; /*at least one must ==0 at this point*/
}


PWSTR strtowstr(PSTR in)
{
    PWSTR result;
    size_t len;

    mbstowcs_s(&len, NULL, 0, in, strlen(in));
    result = new WCHAR[len+1];
    mbstowcs_s(&len, result, len, in, strlen(in));
    result[len] = 0;
    return result;
}


PSTR wstrtostr(PWSTR in)
{
    PSTR result;
    size_t len;

    wcstombs_s(&len, NULL, 0, in, wcslen(in));
    result = new CHAR[len+1];
    wcstombs_s(&len, result, len, in, wcslen(in));
    result[len] = 0;
    return result;
}

