/*
  Name:             aiTIP
  Copyright:        Copyright (C) SIL International.
 Documentation:
  Description:
  Create Date:      11 Dec 2009

  Modified Date:    23 Feb 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    24 Jun 2010 - mcdurdin - I2436 - Add space to context for AIWin2000Unicode when not matched
                    24 Apr 2014 - mcdurdin - I4196 - V9.0 - wm_kmmoreposting must be refactored for TIP work as it is not sequential
                    10 Jun 2014 - mcdurdin - I4262 - V9.0 - TSF deadkeys do not function correctly
                    23 Jun 2014 - mcdurdin - I4287 - V9.0 - Remove extraneous AppIntegration class type tests
                    13 Aug 2014 - mcdurdin - I4370 - Deadkeys are still not working in Winword TIP mode
                    09 Aug 2015 - mcdurdin - I4844 - Tidy up PostDummyKeyEvent calls
                    23 Feb 2016 - mcdurdin - I4978 - Keyboard options do not apply correctly because they are set twice accidentally
*/
   // I4287
#ifndef _AITIP_H
#define _AITIP_H

#include "appint.h"
#include "aiWin2000Unicode.h"

#define AIType_TIP	3

// _VK_PREFIX is used to mask out ALT key seemingly pressed by itself when Keyman swallows the next key, which would activate a menu.   // I4844
// This can be customised with HKLM\Software\Keyman\Keyman Engine\zap virtual key code
#define _VK_PREFIX_DEFAULT    0x0E

struct AIDEBUGKEYINFO
{
	UINT VirtualKey;
	DWORD shiftFlags;
	WCHAR Character, DeadKeyCharacter;
	BOOL IsUp;
};

class AITIP : public AIWin2000Unicode
{
private:
	int WM_KEYMANDEBUG_CANDEBUG,
		WM_KEYMANDEBUG_GETUNICODESTATUS,
		WM_KEYMANDEBUG_GETCONTEXT,
		WM_KEYMANDEBUG_ACTION,
		WM_KEYMANDEBUG_RULEMATCH;

	BOOL FIsDebugControlWindow;
	HWND GetDebugControlWindow();
	void Debug_FillContextBuffer();
  void MergeContextWithCache(PWSTR buf, AppContext *context);   // I4262

	static BOOL IsDebugControlWindow(HWND hwnd);

private:
  BOOL useLegacy;

	BOOL PostKeys();
//TOUCH  	void PostTouchContext();

public:
	AITIP();
	~AITIP();

  BOOL DebugControlled();

  /**
   * Copy the member context
   *
   * @param[out]  savedContext  the copied context
   */
  void CopyContext(AppContext *savedContext);

   /**
   * Restore the passed context to the member context
   *
   * @param  savedContext  the context to restore
   */
  void RestoreContextOnly(AppContext *savedContext);

	virtual BOOL QueueAction(int ItemType, DWORD dwData);

	/* Information functions */

	virtual BOOL CanHandleWindow(HWND ahwnd);
	virtual BOOL IsWindowHandled(HWND ahwnd);
	virtual BOOL HandleWindow(HWND ahwnd);
	virtual BOOL IsUnicode();

	/* Context functions */

	virtual void ReadContext();

	/* Queue and sending functions */

	virtual BOOL SendActions();   // I4196

	/* TIP interactions */

  BOOL IsLegacy() { return useLegacy; }
};

/**
 * ProcessToggleChange
 * Toggles the state of FLAGS in the Globals::ShiftState bit mask
 * Supports VK_CAPITAL and VK_NUMLOCK
 * It DOES NOT generate a system event change for these flags
 * @param  key
 */
void ProcessToggleChange(UINT key);
#endif
