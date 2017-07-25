/*
  Name:             aiDebugger
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    27 Aug 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Jan 2007 - mcdurdin - Add CODE_NOTANY support
                    13 Jul 2007 - mcdurdin - I934 - Prep for x64
                    16 Jan 2009 - mcdurdin - Use SendMessageTimeout
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    04 May 2010 - mcdurdin - I2351 - Robustness - verify _td return value
                    24 Jun 2010 - mcdurdin - I2436 - Add space to context for AIWin2000Unicode when not matched
                    27 Aug 2012 - mcdurdin - I3438 - V9.0 - Add support for custom virtual keys Created
*/
#include "keyman64.h"

AIDebugger::AIDebugger()
{
	WM_KEYMANDEBUG_CANDEBUG         = RegisterWindowMessage("WM_KEYMANDEBUG_CANDEBUG");
	WM_KEYMANDEBUG_GETUNICODESTATUS = RegisterWindowMessage("WM_KEYMANDEBUG_GETUNICODESTATUS");
	WM_KEYMANDEBUG_GETCONTEXT       = RegisterWindowMessage("WM_KEYMANDEBUG_GETCONTEXT");
	WM_KEYMANDEBUG_ACTION           = RegisterWindowMessage("WM_KEYMANDEBUG_ACTION");
	WM_KEYMANDEBUG_RULEMATCH        = RegisterWindowMessage("WM_KEYMANDEBUG_RULEMATCH");
	context = new AppContext;
}

AIDebugger::~AIDebugger()
{
	delete context;
}

/* Debugger functions */

BOOL AIDebugger::IsDebugControlWindow(HWND hwnd)
{
	static int WM_KEYMANDEBUG_CANDEBUG = RegisterWindowMessage("WM_KEYMANDEBUG_CANDEBUG");
  DWORD_PTR dwResult;
	SendMessageTimeout(hwnd, WM_KEYMANDEBUG_CANDEBUG, 0, 0, SMTO_BLOCK, 50, &dwResult);
  return dwResult != 0;
}

HWND AIDebugger::GetDebugControlWindow()
{
	if(!FIsDebugControlWindow) return NULL;
	return hwnd;
}

BOOL AIDebugger::DebugControlled()
{
	return FIsDebugControlWindow;
}

/* Information functions */


BOOL AIDebugger::CanHandleWindow(HWND ahwnd)
{
	return FIsDebugControlWindow = IsDebugControlWindow(ahwnd);
}

BOOL AIDebugger::HandleWindow(HWND ahwnd)
{ 
	FIsDebugControlWindow = TRUE;
	hwnd = ahwnd; 
	return TRUE; 
}

BOOL AIDebugger::IsWindowHandled(HWND ahwnd)
{
	return (hwnd == ahwnd);
}

BOOL AIDebugger::IsUnicode() 
{ 
	return SendMessage(GetDebugControlWindow(), WM_KEYMANDEBUG_GETUNICODESTATUS, 0, 0) != 0;
}

/* Context functions */

void AIDebugger::FillContextBuffer()
{
	WCHAR buf[MAXCONTEXT];
	if(DebugControlled() &&
			SendMessage(GetDebugControlWindow(), WM_KEYMANDEBUG_GETCONTEXT, MAXCONTEXT, (LPARAM) buf))
	{
		context->Set(buf); 
		SendDebugMessageFormat(0, sdmKeyboard, 0, "AIDebugger::FillContextBuffer(%ls)", buf);
	}
	else
	{
		context->Reset();
		SendDebugMessageFormat(0, sdmKeyboard, 0, "AIDebugger::FillContextBuffer()-Reset");
	}
}
	
void AIDebugger::ReadContext()
{
	FillContextBuffer();
}

BOOL AIDebugger::GetWindowContext(LPWSTR buf, int bufsize) 
{ 
	context->Get(buf, bufsize);
	return TRUE;
}

void AIDebugger::AddContext(WCHAR ch)  //I2436
{
  UNREFERENCED_PARAMETER(ch);
}

void AIDebugger::ResetContext()
{
//	FillContextBuffer();
}

WCHAR *AIDebugger::ContextBuf(int n)
{
	return context->Buf(n);
}

WCHAR *AIDebugger::ContextBufMax(int n)
{
	return context->BufMax(n);
}
	
BOOL AIDebugger::SendActions(int send_context, LPMSG send_msg)
{
  UNREFERENCED_PARAMETER(send_msg);
	SendDebugMessageFormat(0, sdmAIDefault, 0, "AIDebugger::SendActions context=%d", send_context);
	if(send_context == ASA_EnterMessageHook) return FALSE;
	return TRUE;
}

extern const int VKContextReset[256];

BOOL AIDebugger::QueueAction(int ItemType, DWORD dwData)
{
	SendDebugMessageFormat(0, sdmAIDefault, 0, "AIDebugger::QueueAction: %s %x", ItemTypes[ItemType], dwData);

	/* Update the internal context - used with multiple groups */ 
	switch(ItemType)
	{
	case QIT_VKEYDOWN:
    if((dwData & QVK_KEYMASK) <= VK__MAX && VKContextReset[(BYTE) dwData]) context->Reset();  // I3438
		break;

	case QIT_DEADKEY:
		context->Add(UC_SENTINEL);
		context->Add(CODE_DEADKEY);
		context->Add((WORD) dwData);
		break;

	case QIT_CHAR:
		context->Add((WORD) dwData);
		break;

	case QIT_BACK:
		if(dwData == BK_BACKSPACE)
			while(context->CharIsDeadkey()) context->Delete();
		//if(dwData == CODE_DEADKEY) break;
		context->Delete();
		if(dwData == BK_BACKSPACE)
			while(context->CharIsDeadkey()) context->Delete();
		break;
	}
	
	if(DebugControlled())
		SendMessage(GetDebugControlWindow(), WM_KEYMANDEBUG_ACTION, ItemType, dwData);
	return TRUE;
}

#define MAXSTOREOFFSETS	20

struct AIDEBUGINFO
{
	int cbSize;
	int ItemType;
	PWSTR Context, Output;
	LPKEY Rule;
	LPGROUP Group;
	DWORD_PTR Flags;
	WORD StoreOffsets[MAXSTOREOFFSETS*2+1];	// pairs--store, char position, terminated by 0xFFFF
};

void FillStoreOffsets(AIDEBUGINFO *di)
{
	int i, n;
	PWSTR p;

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return;

	for(i = n = 0, p = di->Rule->dpContext; *p; p = incxstr(p), i++)
	{
		if(*p == UC_SENTINEL && (*(p+1) == CODE_ANY || *(p+1) == CODE_NOTANY))
		{
			di->StoreOffsets[n++] = *(p+2) - 1;
			di->StoreOffsets[n++] = _td->IndexStack[i];
		}
		if(*p == UC_SENTINEL && *(p+1) == CODE_INDEX)
		{
			di->StoreOffsets[n++] = *(p+2) - 1;
			di->StoreOffsets[n++] = _td->IndexStack[*(p+3) - 1];
		}
		if(n == MAXSTOREOFFSETS*2) break;
	}

	if(n < MAXSTOREOFFSETS*2)
		for(p = di->Rule->dpOutput; *p; p = incxstr(p))
		{
			if(*p == UC_SENTINEL && *(p+1) == CODE_INDEX)
			{
				di->StoreOffsets[n++] = *(p+2) - 1;
				di->StoreOffsets[n++] = _td->IndexStack[*(p+3) - 1];
			}
			if(n == MAXSTOREOFFSETS*2) break;
		}
	di->StoreOffsets[n] = 0xFFFF;
}


BOOL AIDebugger::QueueDebugInformation(int ItemType, LPGROUP Group, LPKEY Rule, PWSTR fcontext, PWSTR foutput, DWORD_PTR dwExtraFlags)
{
	SendDebugMessageFormat(0, sdmAIDefault, 0, "AIDebugger::QueueDebugInformation ItemType=%d", ItemType);
	AIDEBUGINFO di;
	di.cbSize = sizeof(AIDEBUGINFO);
	di.ItemType = ItemType;		// int
	di.Context = fcontext;		// PWSTR
	di.Rule = Rule;				// LPKEY
	di.Group = Group;			// LPGROUP
	di.Output = foutput;		// PWSTR
	di.Flags = dwExtraFlags;	// DWORD
	
	if(di.Rule) FillStoreOffsets(&di);

	// data required
	// keystroke
	// context for rule
	// if rule, then output of rule
	// match positions for all stores in rule

	if(DebugControlled())
		SendMessage(GetDebugControlWindow(), WM_KEYMANDEBUG_RULEMATCH, ItemType, (LPARAM) &di);

	return TRUE;
}

