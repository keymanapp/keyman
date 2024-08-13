/*
  Name:             appint
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      14 Sep 2006

  Modified Date:    23 Feb 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          14 Sep 2006 - mcdurdin - Remove NoSetShift flag
                    27 Jan 2009 - mcdurdin - I1797 - Add fallback for AIWin2000 app integration
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    24 Jun 2010 - mcdurdin - I2436 - Add space to context for AIWin2000Unicode when not matched
                    24 Apr 2014 - mcdurdin - I4196 - V9.0 - wm_kmmoreposting must be refactored for TIP work as it is not sequential
                    23 Jun 2014 - mcdurdin - I4287 - V9.0 - Remove extraneous AppIntegration class type tests
                    23 Feb 2016 - mcdurdin - I4978 - Keyboard options do not apply correctly because they are set twice accidentally
*/
   // I4287
#ifndef _APPINT_H
#define _APPINT_H

typedef struct
{
	int ItemType;
	DWORD dwData;
} APPACTIONQUEUEITEM;

// QueueAction ItemTypes
#define QIT_VKEYDOWN	0
#define QIT_VKEYUP		1
//#define QIT_VSHIFTDOWN	2  // deprecated see #11925
//#define QIT_VSHIFTUP	3    // deprecated see #11925
#define QIT_CHAR		4
#define QIT_DEADKEY		5
#define QIT_BELL		6
#define QIT_BACK		7
#define QIT_CAPSLOCK 8
#define QIT_INVALIDATECONTEXT 9

#define QVK_EXTENDED 0x00010000 // Flag for QIT_VKEYDOWN to indicate an extended key
#define QVK_KEYMASK  0x0000FFFF
#define QVK_FLAGMASK 0xFFFF0000

#define MAXCONTEXT 64
#define MAXACTIONQUEUE	1024

class AppActionQueue
{
protected:
	APPACTIONQUEUEITEM Queue[MAXACTIONQUEUE];
	int QueueSize;

public:
	AppActionQueue();
	virtual void ResetQueue();
	virtual BOOL QueueAction(int ItemType, DWORD dwData);

  BOOL IsQueueEmpty() { return QueueSize == 0; }
  int GetQueueSize() { return QueueSize; }
};

class AppIntegration:public AppActionQueue
{
protected:
	HWND hwnd;
	int FShiftFlags;
public:
	AppIntegration();

	//BOOL NoSetShift;

	/* Information functions */

	virtual BOOL CanHandleWindow(HWND ahwnd) = 0;
	virtual BOOL IsWindowHandled(HWND ahwnd) = 0;
	virtual BOOL HandleWindow(HWND ahwnd) = 0;
	virtual BOOL IsUnicode() = 0;

	/* Context functions */
  /**
    * Reads the current application context upto MAXCONTEXT length into the supplied buffer.
    * @param  buf      The data buffer to copy current application context
    */
	virtual BOOL ReadContext(PWSTR buf) = 0;
	virtual void ResetContext() = 0;

	/* Queue and sending functions */

	void SetCurrentShiftState(int ShiftFlags) { FShiftFlags = ShiftFlags; }
	virtual BOOL SendActions() = 0;   // I4196
};

extern const LPSTR ItemTypes[];

#endif
