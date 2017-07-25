/*
  Name:             aiDebugger
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      12 Mar 2010

  Modified Date:    24 Jun 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    24 Jun 2010 - mcdurdin - I2436 - Add space to context for AIWin2000Unicode when not matched
*/

#ifndef _AIDEBUGGER_H
#define _AIDEBUGGER_H

#define AIType_Debugger	2

struct AIDEBUGKEYINFO
{
	UINT VirtualKey;
	DWORD shiftFlags;
	WCHAR Character, DeadKeyCharacter;
	BOOL IsUp;
};

class AIDebugger: public AppIntegration
{
private:
	int WM_KEYMANDEBUG_CANDEBUG,
		WM_KEYMANDEBUG_GETUNICODESTATUS,
		WM_KEYMANDEBUG_GETCONTEXT,
		WM_KEYMANDEBUG_ACTION,
		WM_KEYMANDEBUG_RULEMATCH;

	AppContext *context;
	BOOL FIsDebugControlWindow;

	HWND GetDebugControlWindow();
	BOOL DebugControlled();
	void FillContextBuffer();

public:
	AIDebugger();
	~AIDebugger();

	static BOOL IsDebugControlWindow(HWND hwnd);

	virtual BOOL QueueAction(int ItemType, DWORD dwData);
	virtual BOOL QueueDebugInformation(int ItemType, LPGROUP Group, LPKEY Rule, PWSTR fcontext, PWSTR foutput, DWORD_PTR dwExtraFlags);

	/* Information functions */
	
	BOOL CanHandleWindow(HWND ahwnd);
	BOOL IsWindowHandled(HWND ahwnd);
	BOOL HandleWindow(HWND ahwnd);
	BOOL IsUnicode();

	/* Context functions */
	
	virtual void ReadContext();
	virtual BOOL GetWindowContext(LPWSTR buf, int bufsize);
  virtual void AddContext(WCHAR ch);  //I2436
	virtual void ResetContext();
	virtual WCHAR *ContextBuf(int n);
	virtual WCHAR *ContextBufMax(int n);

	/* Queue and sending functions */
	
	virtual BOOL SendActions(int send_context, LPMSG send_msg);

	static int Type() { return AIType_Debugger; }
	virtual int Type1() { return AIType_Debugger; }
};

#endif
