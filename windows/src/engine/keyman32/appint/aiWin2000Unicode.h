/*
  Name:             aiWin2000Unicode
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      27 Jan 2009

  Modified Date:    23 Jun 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          27 Jan 2009 - mcdurdin - I1797 - Add fallback for AIWin2000 app integration
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    24 Jun 2010 - mcdurdin - I2436 - Add space to context for AIWin2000Unicode when not matched
                    24 Apr 2014 - mcdurdin - I4196 - V9.0 - wm_kmmoreposting must be refactored for TIP work as it is not sequential
                    01 May 2014 - mcdurdin - I4128 - V9.0 - Shift states still not working with unprocessed keys in V9
                    23 Jun 2014 - mcdurdin - I4287 - V9.0 - Remove extraneous AppIntegration class type tests
*/
   // I4128   // I4287
#ifndef _AIWIN2000UNICODE_H
#define _AIWIN2000UNICODE_H

#define AIType_Win2000Unicode	4

class AIWin2000Unicode:public AppIntegration
{
private:

	BOOL PostKeys();

public:
  AIWin2000Unicode();
  ~AIWin2000Unicode();

	/* Information functions */

	virtual BOOL CanHandleWindow(HWND ahwnd);
	virtual BOOL IsWindowHandled(HWND ahwnd);
	virtual BOOL HandleWindow(HWND ahwnd);
	virtual BOOL IsUnicode();

	/* Context functions */

	virtual BOOL ReadContext(PWSTR buf);
	virtual void ResetContext();
 
	/* Queue and sending functions */

	virtual BOOL SendActions();   // I4196
};

#endif
