/*
  Name:             appint
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      5 Nov 2007

  Modified Date:    23 Jun 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          05 Nov 2007 - mcdurdin - I1129 - Fix irregular behaviour with context rules in debugger
                    27 Jan 2009 - mcdurdin - I1797 - Add fallback for AIWin2000 app integration
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    03 Oct 2011 - mcdurdin - I3091 - IMX DLLs cause an assertion failure due to buffer size mismatch
                    17 Nov 2012 - mcdurdin - I3575 - V9.0 - context must be cached for legacy mode in TSF as rules are processed twice
                    10 Jun 2014 - mcdurdin - I4262 - V9.0 - TSF deadkeys do not function correctly
                    23 Jun 2014 - mcdurdin - I4287 - V9.0 - Remove extraneous AppIntegration class type tests
*/
   // I4287
#include "pch.h"

PWSTR decxstr(PWSTR p, PWSTR pStart);

const LPSTR ItemTypes[8] = {
	"QIT_VKEYDOWN", "QIT_VKEYUP", "QIT_VSHIFTDOWN", "QIT_VSHIFTUP",
    "QIT_CHAR", "QIT_DEADKEY", "QIT_BELL", "QIT_BACK" };

/* AppActionQueue */

AppActionQueue::AppActionQueue()
{
  memset(Queue, 0, sizeof(APPACTIONQUEUEITEM) * MAXACTIONQUEUE);
	ResetQueue();
}

void AppActionQueue::ResetQueue()
{
	//SendDebugMessageFormat(0, sdmAIDefault, 0, "App::ResetQueue");
	QueueSize = 0;   // I4262
}

BOOL AppActionQueue::QueueAction(int ItemType, DWORD dwData)
{
	if(QueueSize > MAXACTIONQUEUE - 1)
	{
		MessageBeep(0xFFFFFFFF);
		return FALSE;
	}

	Queue[QueueSize].ItemType = ItemType;
	Queue[QueueSize].dwData   = dwData;

	QueueSize++;

	SendDebugMessageFormat(0, sdmAIDefault, 0, "App::QueueAction: %s %x", ItemTypes[ItemType], dwData);

	return TRUE;
}

/* AppIntegration */

AppIntegration::AppIntegration()
{
	hwnd = NULL;
  FShiftFlags = 0;
}
