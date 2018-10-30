/*
  Name:             K32_info
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      11 Mar 2009

  Modified Date:    25 Oct 2016
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    04 May 2010 - mcdurdin - I2351 - Robustness - verify _td return value
                    01 Dec 2012 - mcdurdin - I3613 - V9.0 - System shadow keyboards obsolete, strip out remaining code
                    25 Oct 2016 - mcdurdin - I5136 - Remove additional product references from Keyman Engine
*/

#include "pch.h"

extern "C" BOOL _declspec(dllexport) WINAPI GetKeymanInfo( LPKEYMANINFO ki )
{
  UNREFERENCED_PARAMETER(ki);
  return TRUE;
}

// The user must delete the hBitmap if it exists
extern "C" BOOL  _declspec(dllexport) WINAPI GetKeymanKeyboardInfo(LPKEYMANKEYBOARDINFO kki)
{
	LPKEYBOARD kbp;
	char *p;
	PWSTR q;

	if(!kki) return FALSE;
  int i;

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;

	for(i = 0; i < _td->nKeyboards; i++)
		if(_td->lpKeyboards[i].KeymanID == kki->KeymanID) break;

	if(i == _td->nKeyboards) return FALSE;

	kki->hBitmap = NULL;
	kki->szMessage[0] = 0;
	kki->szCopyright[0] = 0;

	RegistryReadOnly *reg = Reg_GetKeymanInstalledKeyboard(_td->lpKeyboards[i].Name);
	if(!reg) return FALSE;

	reg->ReadString(REGSZ_KeymanFile, kki->szFileName, 256);
	kki->layout = 0;   // I3613

	if(LoadKeyboard(kki->szFileName, &kbp))   // I5136
	{
		kki->hBitmap = kbp->hBitmap;
		q = GetSystemStore(kbp, TSS_MESSAGE); 
		if(q) { p = wstrtostr(q); strcpy(kki->szMessage, p); delete p; }
		q = GetSystemStore(kbp, TSS_COPYRIGHT); 
		if(q) { p = wstrtostr(q); strcpy(kki->szCopyright, p); delete p; }
		q = GetSystemStore(kbp, TSS_NAME); 
		if(q) { p = wstrtostr(q); strcpy(kki->szName, p); delete p; }

		delete kbp;
	}
	else
		strcpy(kki->szName, "Unknown Keyboard (Error loading?)");

	delete reg;

	return TRUE;
}


extern "C" PWSTR _declspec(dllexport) WINAPI GetSystemStore(LPKEYBOARD kb, DWORD SystemID)
{
	for(DWORD i = 0; i < kb->cxStoreArray; i++)
		if(kb->dpStoreArray[i].dwSystemID == SystemID) return kb->dpStoreArray[i].dpString;

	return NULL;
}
