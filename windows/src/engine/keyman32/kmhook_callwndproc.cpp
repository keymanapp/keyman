/*
  Name:             kmhook_callwndproc
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      1 Aug 2006

  Modified Date:    22 Apr 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Aug 2006 - mcdurdin - Visual keyboard updating
                    30 May 2007 - mcdurdin - I864 - Log exceptions in hook procedures
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    11 Dec 2009 - mcdurdin - I934 - x64 - keyboard hook per thread
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    29 Mar 2010 - mcdurdin - I2266 - Reduce chatter when switching languages
                    09 Apr 2010 - mcdurdin - I2292 - Keyman init was re-entrant
                    19 Apr 2010 - mcdurdin - I2297 - Use global cache to remember last active keyboard for all apps
                    04 May 2010 - mcdurdin - I2351 - Robustness - verify _td return value
                    04 May 2010 - mcdurdin - I2356 - Don't clear keyboard hook if new focus is in same thread
                    04 May 2010 - mcdurdin - I2297 - Focus and keyboard switching reliability
                    10 May 2010 - mcdurdin - Enable exception logging
                    29 Jun 2010 - mcdurdin - I2443 - Always do focus change so language switch affects keyman.exe too
                    29 Jun 2010 - mcdurdin - I2435 - Make Keyman Engine unload more reliably by sending rather than posting KM_EXIT
                    29 Jun 2010 - mcdurdin - I2448 - Regression - keyboard switching in .285
                    30 Nov 2010 - mcdurdin - I2546 - When switching languages, Keyman Engine did not get notified of new language reliably
                    30 Nov 2010 - mcdurdin - I2547 - Language switch window sometimes appears after Alt+Tab, Shift
                    19 Aug 2011 - mcdurdin - I3025 - Alt+LShift, Esc should cancel language switch window
                    06 Feb 2012 - mcdurdin - I3226 - Keyman Engine appears to not be receiving some WM_SETFOCUS notifications
                    04 Nov 2012 - mcdurdin - I3531 - V9.0 - Merge of I3226 - Keyman Engine appears to not be receiving some WM_SETFOCUS notifications
                    28 Nov 2012 - mcdurdin - I3594 - V9.0 - Remove old SelectKeyboard code and related messages
                    01 Dec 2012 - mcdurdin - I3617 - V9.0 - Keyboard hook obsolete, strip out code
                    07 Nov 2013 - mcdurdin - I3949 - V9.0 - Keyboard selection and notification needs consolidation
                    16 Jun 2014 - mcdurdin - I4271 - V9.0 - Switch language for all applications is not working
                    23 Jun 2014 - mcdurdin - I4288 - V9.0 - Consolidate reporting of active keyboard into ReportActiveKeyboard
                    03 Aug 2014 - mcdurdin - I4326 - V9.0 - Switch-off hotkey not working, then keyboard hotkey stopped working (win 8.1 jeremy) [High]
                    22 Apr 2015 - mcdurdin - I4674 - V9.0 - Hotkeys do not always work consistently
*/

#include "pch.h"

/*
*	kmnCallWndProc
*
*	Parameters:	nCode	Specifies whether to process or ignore the message
*				wParam	(unused)
*				lParam	Points to a message structure to be processed
*
*	Returns:    Should return 0
*
*   Called by:  Windows
*
*	CallWndFunc handles non-client window messages and window activation
*	messages that aren't Sent or Posted.
*/

LRESULT _kmnCallWndProc(int nCode, WPARAM wParam, LPARAM lParam);

LRESULT CALLBACK kmnCallWndProc(int nCode, WPARAM wParam, LPARAM lParam)
{
  LRESULT res = 0;

#ifdef _DEBUG_EXCEPTION
	res = _kmnCallWndProc(nCode,wParam,lParam);
#else
  __try
  {
	  res = _kmnCallWndProc(nCode,wParam,lParam);
	}
	__except(ExceptionMessage("kmnCallWndProc", GetExceptionInformation()))
	{
	}
#endif
  return res;
}
   // I3617
BOOL IsSysTrayWindow(HWND hwnd);

void ProcessWMKeyman(HWND hwnd, WPARAM wParam, LPARAM lParam);
void ProcessWMKeymanControlInternal(HWND hwnd, WPARAM wParam, LPARAM lParam);


/* Note: following copied from kmhook_keyboard for now */
HWND IsLanguageSwitchWindowVisible() {   // I4326
  HWND hwndLanguageSwitch = FindWindow("TfrmLanguageSwitch", NULL);
  if(hwndLanguageSwitch == NULL) return FALSE;
  return IsWindowVisible(hwndLanguageSwitch) ? hwndLanguageSwitch : NULL;
}

void SendToLanguageSwitchWindow(HWND hwndLanguageSwitch, WPARAM wParam, LPARAM lParam) {   // I4326
  DWORD keyFlags = (DWORD) lParam;   // I4124
  PostMessage(hwndLanguageSwitch, wm_keyman_control, keyFlags & LLKHF_UP ? KMC_KEYUP : KMC_KEYDOWN, wParam);   // I4124
}


LRESULT _kmnCallWndProc(int nCode, WPARAM wParam, LPARAM lParam)
{
  CWPSTRUCT *cp;

  if(nCode >= 0)
  {
		cp = (CWPSTRUCT *)lParam;
    if(!Globals::get_Keyman_Initialised())
    {
      //DebugLastError("get_Keyman_Initialised");
    }
	  else
	  {
  /*
		  SendDebugMessageFormat("kmnCallWndProc %s %x %x "
		  "{hwnd:%d msg:%d wp:%d lp:%d} ctrl: %x/%x alt: %x/%x shift: %x/%x",
		  MessageName(cp->message), cp->wParam, cp->lParam,
		  cp->hwnd, cp->message, cp->wParam, cp->lParam,
		  GetKeyState(VK_CONTROL), GetAsyncKeyState(VK_CONTROL),
		  GetKeyState(VK_MENU), GetAsyncKeyState(VK_MENU),
		  GetKeyState(VK_SHIFT), GetAsyncKeyState(VK_SHIFT));
  */
      PKEYMAN64THREADDATA _td = ThreadGlobals();
      if(!_td)
      {
        if(!Globals::CheckControllers())
        {
          DebugLastError("CheckControllers");
          return CallNextHookEx(Globals::get_hhookCallWndProc(), nCode, wParam, lParam);
        }

        if(!Globals_InitThread())
        {
          DebugLastError("Globals_InitThread");
          return CallNextHookEx(Globals::get_hhookCallWndProc(), nCode, wParam, lParam);
        }

        _td = ThreadGlobals();
	      if(!_td)
        {
          DebugLastError("ThreadGlobals");
          return CallNextHookEx(Globals::get_hhookCallWndProc(), nCode, wParam, lParam);
        }
      }

      if(!_td->FInitialised)			// Keyman keyboard is being selected for first time in active process
      {
        if(!Globals::CheckControllers())
        {
          DebugLastError("CheckControllers");
          return CallNextHookEx(Globals::get_hhookCallWndProc(), nCode, wParam, lParam);
        }
        if(!InitialiseProcess())
        {
          DebugLastError("InitialiseProcess");
          return CallNextHookEx(Globals::get_hhookCallWndProc(), nCode, wParam, lParam);
        }
      }

		  switch(cp->message)
		  {
		  case WM_KILLFOCUS:
        {
          CheckScheduledRefresh();
          HWND hwnd = IsLanguageSwitchWindowVisible();   // I4326
          if(hwnd != NULL && !Globals::IsControllerThread(GetCurrentThreadId())) SendToLanguageSwitchWindow(hwnd, VK_ESCAPE, 0); // I3025

          SendDebugMessageFormat("WM_KILLFOCUS %x -> %x", cp->hwnd, cp->wParam);  // I3226   // I3531
          if(!IsSysTrayWindow(cp->hwnd) && !Globals::IsControllerThread(GetCurrentThreadId()))   // I2443 - always do the focus change now? really unsure about this one
          {
  			    PostMessage(cp->hwnd, wm_keyman, KM_FOCUSCHANGED, 0);
	  		    SendDebugMessageFormat("WM_KILLFOCUS -- Telling Keyman focus has changed");
  			    KMHideIM();
	  		    if(_td->app) _td->app->ResetContext();
          }
        }
			  break;
      case WM_SETFOCUS:
        CheckScheduledRefresh();
        if (IsSysTrayWindow(cp->hwnd))      // I2443 - always do the focus change now? really unsure about this one
          SendDebugMessageFormat("WM_SETFOCUS -- not hooking because IsSysTrayWindow");
        else if (Globals::IsControllerThread(GetCurrentThreadId()))
          SendDebugMessageFormat("WM_SETFOCUS -- not hooking because IsControllerThread");
        else
        {
          PostMessage(cp->hwnd, wm_keyman, KM_FOCUSCHANGED, KMF_WINDOWCHANGED);
          SendDebugMessageFormat("WM_SETFOCUS %x <- %x", cp->hwnd, cp->wParam);  // I3226   // I3531
          if (_td->app) _td->app->ResetContext();
        }
        break;
      case WM_ACTIVATE:
        if(cp->wParam == WA_ACTIVE || cp->wParam == WA_CLICKACTIVE)
        {
          CheckScheduledRefresh();
        }
        break;
		  case WM_INPUTLANGCHANGE:
        CheckScheduledRefresh();
        SendDebugMessageFormat("WM_INPUTLANGCHANGE %x %x Hwnd=%x Parent=%x Focus=%x Active=%x", cp->wParam, cp->lParam, cp->hwnd, GetParent(cp->hwnd), GetFocus(), GetActiveWindow());
          ReportActiveKeyboard(PC_UPDATE);   // I4288

          // The input language has changed, so tell Keyman window about it
        break;
      default:
        if(cp->message == wm_keyman)    // I2435 - Make Keyman Engine unload more reliably by sending a broadcast message to all apps rather than posting
        {
          SendDebugMessageFormat("wm_keyman hwnd=%x %x %x", cp->hwnd, cp->wParam, cp->lParam);   // I4674
          ProcessWMKeyman(cp->hwnd, cp->wParam, cp->lParam);
        } else if(cp->message == wm_keyman_control_internal) {
          SendDebugMessageFormat("wm_keyman_control_internal hwnd=%x %x %x", cp->hwnd, cp->wParam, cp->lParam);   // I4674
      		ProcessWMKeymanControlInternal(cp->hwnd, cp->wParam, cp->lParam);   // I4271
        }
		  }
	  }
  }

	return CallNextHookEx(Globals::get_hhookCallWndProc(), nCode, wParam, lParam );
}
