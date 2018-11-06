/*
  Name:             AIWin2000Unicode
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      22 Jan 2007

  Modified Date:    9 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          22 Jan 2007 - mcdurdin - Fix for K_NPENTER
                    13 Jul 2007 - mcdurdin - I934 - Prep fox x64
                    23 Aug 2007 - mcdurdin - I719 - Fix Alt+LeftShift and Word interactions
                    14 Jun 2008 - mcdurdin - I1389 - Supp chars on Vista+ default to single BKSP
                    20 Jul 2008 - mcdurdin - I1546 - Language switch not working with some keyboard IDs
                    16 Jan 2009 - mcdurdin - I1512 - SendInput support
                    27 Jan 2009 - mcdurdin - I1797 - Add fallback for AIWin2000 app integration
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    15 Jun 2010 - mcdurdin - I2426 - Remove incorrectl KEYMAN_CHARFLAG
                    24 Jun 2010 - mcdurdin - I2436 - Add space to context for AIWin2000Unicode when not matched
                    27 Aug 2012 - mcdurdin - I3438 - V9.0 - Add support for custom virtual keys Created
                    18 Feb 2012 - mcdurdin - I3242 - Arrow keys do not function correctly in Internet Explorer
                    26 Feb 2012 - mcdurdin - I2787 - Reset keyboard state for a key when key down is sent to avoid confusion for Chrome
                    04 Nov 2012 - mcdurdin - I3527 - V9.0 - Merge of I3242 - Arrow keys do not function correctly in Internet Explorer
                    04 Nov 2012 - mcdurdin - I3528 - V9.0 - Merge of I2787 - Reset keyboard state for a key when key down is sent to avoid confusion for Chrome
                    17 Dec 2013 - mcdurdin - I4006 - V9.0 - Remove old aiDefault code
                    24 Apr 2014 - mcdurdin - I4196 - V9.0 - wm_kmmoreposting must be refactored for TIP work as it is not sequential
                    01 May 2014 - mcdurdin - I4128 - V9.0 - Shift states still not working with unprocessed keys in V9
                    23 Jun 2014 - mcdurdin - I4287 - V9.0 - Remove extraneous AppIntegration class type tests
                    13 Aug 2014 - mcdurdin - I4370 - Deadkeys are still not working in Winword TIP mode
                    14 Aug 2014 - mcdurdin - I4378 - V9.0 - Rapid typing in legacy mode breaks Keyman input
                    13 Oct 2014 - mcdurdin - I4452 - V9.0 - Chinese keyboard is not working correctly
                    31 Dec 2014 - mcdurdin - I4548 - V9.0 - When Alt is down, release of Ctrl, Shift is not detectable within TIP in some languages
                    09 Aug 2015 - mcdurdin - I4844 - Tidy up PostDummyKeyEvent calls
*/
#include "pch.h"   // I4128   // I4287

AIWin2000Unicode::AIWin2000Unicode()
{
	context = new AppContext;
}

AIWin2000Unicode::~AIWin2000Unicode()
{
	delete context;
}

/* Context functions */

void AIWin2000Unicode::ReadContext()
{
}
	
void AIWin2000Unicode::AddContext(WCHAR ch)  //I2436
{
  context->Add(ch);
}

void AIWin2000Unicode::ResetContext()
{
	context->Reset();
}

WCHAR *AIWin2000Unicode::ContextBuf(int n)
{
	return context->Buf(n);
}

WCHAR *AIWin2000Unicode::ContextBufMax(int n)
{
	return context->BufMax(n);
}
	
BYTE SavedKbdState[256];

BOOL AIWin2000Unicode::SendActions()   // I4196
{
	return PostKeys();
}

BOOL AIWin2000Unicode::QueueAction(int ItemType, DWORD dwData)
{
	int result = AppIntegration::QueueAction(ItemType, dwData);
	
	switch(ItemType)
	{
	case QIT_VKEYDOWN:
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

	return result;
}

BOOL AIWin2000Unicode::QueueDebugInformation(int ItemType, LPGROUP Group, LPKEY Rule, PWSTR fcontext, PWSTR foutput, DWORD_PTR dwExtraFlags)
{
  UNREFERENCED_PARAMETER(ItemType);
  UNREFERENCED_PARAMETER(Group);
  UNREFERENCED_PARAMETER(Rule);
  UNREFERENCED_PARAMETER(fcontext);
  UNREFERENCED_PARAMETER(foutput);
  UNREFERENCED_PARAMETER(dwExtraFlags);
	return TRUE;
}

// I1512 - SendInput with VK_PACKET for greater robustness

BOOL AIWin2000Unicode::PostKeys()
{	
  if(QueueSize == 0) 
	{
		return TRUE;
	}

  int n = 0;
  int i = 0;

  for(; n < QueueSize; n++)
  {
	  switch(Queue[n].ItemType) {
	  case QIT_VKEYDOWN:
		  if((Queue[n].dwData & QVK_KEYMASK) == 0x05) Queue[n].dwData = (Queue[n].dwData & QVK_FLAGMASK) | VK_RETURN; // I649  // I3438
  		
		  /* 6.0.153.0: Fix repeat state for virtual keys */

      if((Queue[n].dwData & QVK_KEYMASK) <= VK__MAX)  // I3438
      {
        printf("KEYDOWN: %x (flags=%x)\n", Queue[n].dwData & 0xFF, (Queue[n].dwData & QVK_FLAGMASK) >> 16);
      }

		  break;
	  case QIT_VKEYUP:
		  if((Queue[n].dwData & QVK_KEYMASK) == 0x05) Queue[n].dwData = (Queue[n].dwData & QVK_FLAGMASK) | VK_RETURN; // I649  // I3438

      if((Queue[n].dwData & QVK_KEYMASK) <= VK__MAX)  // I3438
      {
        printf("KEYUP: %x (flags=%x)\n", Queue[n].dwData & 0xFF, (Queue[n].dwData & QVK_FLAGMASK) >> 16);
      }

		  break;
	  case QIT_VSHIFTDOWN:
      printf("VSHIFTDOWN\n");
		  break;
	  case QIT_VSHIFTUP:
      printf("VSHIFTUP\n");
      break;
	  case QIT_CHAR:
      wprintf(L"CHAR %x (%lc)\n", Queue[n].dwData, Queue[n].dwData);
      break;
	  case QIT_DEADKEY:
      printf("DEADKEY\n");
		  break;
	  case QIT_BELL:
      printf("BELL\n");
		  break;
	  case QIT_BACK:
      printf("BKSP (%x)\n", Queue[n].dwData);
		  if(Queue[n].dwData == BK_DEADKEY) break;
      if(Queue[n].dwData == BK_SUPP2) break;  // I1389 - supp chars on vista default to single backspace //TODO: eliminate BK_SUPP2
      break;
	  }
  }

  QueueSize = 0;

  return TRUE;
}

