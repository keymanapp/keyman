/*
  Name:             addins
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Jun 2008

  Modified Date:    14 May 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Jun 2008 - mcdurdin - I1488 - Fix registry handle leak
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    04 May 2010 - mcdurdin - I2351 - Robustness - verify _td return value
                    14 May 2010 - mcdurdin - I2374 - Fix crash in some situations
*/

#include "pch.h"

void Addin_Release()
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return;

  //SendDebugMessageFormat(GetFocus(), sdmGlobal, 0, "Addin_Release: ENTER [%d]", nAddins);
	if(_td->Addins)
	{
		for(int i = 0; i < _td->nAddins; i++)
			if(_td->Addins[i].hAddin) 
			{
				if(_td->Addins[i].Uninitialise) (*_td->Addins[i].Uninitialise)();
				FreeLibrary(_td->Addins[i].hAddin);
			}
		delete[] _td->Addins;
	}
	_td->Addins = NULL;
	_td->nAddins = 0;
	_td->CurrentAddin = -1;
	//SendDebugMessageFormat(GetFocus(), sdmGlobal, 0, "Addin_Release: EXIT");
}

void ReadAddins(HKEY hkey)
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return;

	//SendDebugMessageFormat(GetFocus(), sdmGlobal, 0, "Addins: ReadAddins: ENTER");
	RegistryReadOnly *reg = new RegistryReadOnly(hkey);
  if(reg->OpenKeyReadOnly(hkey == HKEY_CURRENT_USER ? REGSZ_KeymanAddinsCU : REGSZ_KeymanAddinsLM))
	{
		int n = _td->nAddins;
    char buf[128];
		while(reg->GetValueNames(buf, 128, n))
		{
			Addin *a = new Addin[n+1];
			if(_td->Addins) 
			{
				memcpy(a, _td->Addins, n * sizeof(Addin));
				delete[] _td->Addins;
			}
			_td->Addins = a;
			_td->Addins[n].hAddin = 0;
			_td->Addins[n].FocusChanged = NULL;
			_td->Addins[n].Initialise = NULL;
			_td->Addins[n].OutputBackspace = NULL;
			_td->Addins[n].OutputChar = NULL;
			_td->Addins[n].Uninitialise = NULL;
			_td->Addins[n].ShouldProcess = NULL;
			strcpy(_td->Addins[n].ClassName, buf);
			reg->ReadString(buf, _td->Addins[n].AddinName, 260);
	  	_td->Addins[n].Application[0] = 0;
			n++;
		}
		_td->nAddins = n;
	}
  delete reg;
	//SendDebugMessageFormat(GetFocus(), sdmGlobal, 0, "Addins: ReadAddins: EXIT");
}

void Addin_Refresh()
{
	//SendDebugMessageFormat(GetFocus(), sdmGlobal, 0, "Addin_Refresh: ENTER");
	Addin_Release();
	ReadAddins(HKEY_CURRENT_USER);
	ReadAddins(HKEY_LOCAL_MACHINE);
	//SendDebugMessageFormat(GetFocus(), sdmGlobal, 0, "Addin_Refresh: EXIT");
}

BOOL LoadAddin()
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;
  if(_td->CurrentAddin == -1) return FALSE;

  Addin *a = &_td->Addins[_td->CurrentAddin];
	if(!a->hAddin)
	{
		//SendDebugMessageFormat(GetFocus(), sdmGlobal, 0, "Addins: LoadAddin: ENTER");
		a->hAddin = LoadLibrary(a->AddinName);
		if(!a->hAddin)
		{
			a->hAddin = 0;
			a->ClassName[0] = 0;	// prevent add-in attempting to load again
			_td->CurrenthWnd = 0;
			_td->CurrentAddin = -1;
			//SendDebugMessageFormat(GetFocus(), sdmGlobal, 0, "Addins: LoadAddin: EXIT - FALSE - LoadLibrary");
			return FALSE;
		}
		a->OutputBackspace = (PKeymanOutputBackspace) GetProcAddress(a->hAddin, "KeymanOutputBackspace");
		a->OutputChar = (PKeymanOutputChar) GetProcAddress(a->hAddin, "KeymanOutputChar");
		a->FocusChanged = (PKeymanFocusChanged) GetProcAddress(a->hAddin, "KeymanFocusChanged");
		a->ShouldProcess = (PKeymanShouldProcess) GetProcAddress(a->hAddin, "KeymanShouldProcess");
		a->Initialise = (PKeymanInit) GetProcAddress(a->hAddin, "KeymanInitialise");
		a->Uninitialise = (PKeymanInit) GetProcAddress(a->hAddin, "KeymanUninitialise");

		if(a->Initialise && !(*a->Initialise)())
		{
			FreeLibrary(a->hAddin);
			a->hAddin = 0;
			a->ClassName[0] = 0;
			_td->CurrenthWnd = 0;
			_td->CurrentAddin = -1;
			//SendDebugMessageFormat(GetFocus(), sdmGlobal, 0, "Addins: LoadAddin: EXIT - FALSE - Initialise");
			return FALSE;
		}
		//SendDebugMessageFormat(GetFocus(), sdmGlobal, 0, "Addins: LoadAddin: EXIT - TRUE - Loaded");
	}
	return TRUE;
}

BOOL Addin_ShouldProcessUnichar(HWND hwnd)
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;

	//SendDebugMessageFormat(hwnd, sdmGlobal, 0, "Addin_ShouldProcessUnichar: ENTER");
	_td->CurrenthWnd = hwnd;
	GetClassName(hwnd, _td->CurrentClassName, 128);
	if(_td->CurrentAddin >= 0 && !_strcmpi(_td->CurrentClassName, _td->Addins[_td->CurrentAddin].ClassName)) 
	{
		//SendDebugMessageFormat(hwnd, sdmGlobal, 0, "Addin_ShouldProcessUnichar: EXIT - TRUE - CurrentAddin okay");
		return TRUE;
	}

	for(int i = 0; i < _td->nAddins; i++)
		if(!_strcmpi(_td->CurrentClassName, _td->Addins[i].ClassName))
		{
			_td->CurrentAddin = i;
			if(!LoadAddin())
			{
				_td->CurrentAddin = -1;
				_td->Addins[i].ClassName[0] = 0; // prevent add-in attempting to load again
			}
			else if(_td->Addins[i].ShouldProcess && !(*_td->Addins[i].ShouldProcess)(hwnd))
				_td->CurrentAddin = -1;
			else
			{
				//SendDebugMessageFormat(hwnd, sdmGlobal, 0, "Addin_ShouldProcessUnichar: EXIT - TRUE - FoundAddin");
				return TRUE;
			}
		}
	_td->CurrentAddin = -1;
	_td->CurrenthWnd = 0;
	//SendDebugMessageFormat(hwnd, sdmGlobal, 0, "Addin_ShouldProcessUnichar: EXIT - FALSE");
	return FALSE;
}

BOOL Addin_ProcessUnichar(HWND hwnd, DWORD chr)
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;

	//SendDebugMessageFormat(hwnd, sdmGlobal, 0, "Addin_ProcessUnichar: ENTER");
	if(_td->CurrenthWnd != hwnd)
		if(!Addin_ShouldProcessUnichar(hwnd)) 
		{
			//SendDebugMessageFormat(hwnd, sdmGlobal, 0, "Addin_ProcessUnichar: EXIT - FALSE - Addin_ShouldProcessUnichar");
			return FALSE;
		}

	if(!LoadAddin())
	{
		//SendDebugMessageFormat(hwnd, sdmGlobal, 0, "Addin_ProcessUnichar: EXIT - FALSE - !LoadAddin)");
		return FALSE;
	}
	if(!_td->Addins[_td->CurrentAddin].OutputChar) 
	{
		//SendDebugMessageFormat(hwnd, sdmGlobal, 0, "Addin_ProcessUnichar: EXIT - FALSE !OutputChar");
		return FALSE;
	}

	BOOL b = (*_td->Addins[_td->CurrentAddin].OutputChar)(hwnd, chr);
	//SendDebugMessageFormat(hwnd, sdmGlobal, 0, "Addin_ProcessUnichar: EXIT (b) == %d", b);
	return b; 
}

BOOL Addin_ProcessBackspace(HWND hwnd)
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;

	//SendDebugMessageFormat(hwnd, sdmGlobal, 0, "Addin_ProcessBackspace: ENTER");
	if(_td->CurrenthWnd != hwnd)
		if(!Addin_ShouldProcessUnichar(hwnd)) 
		{
			//SendDebugMessageFormat(hwnd, sdmGlobal, 0, "Addin_ProcessBackspace: EXIT - !ShouldProcess");
			return FALSE;
		}

	if(!LoadAddin()) 
	{
		//SendDebugMessageFormat(hwnd, sdmGlobal, 0, "Addin_ProcessBackspace: EXIT - !LoadAddin");
		return FALSE;
	}
	if(!_td->Addins[_td->CurrentAddin].OutputBackspace) 
	{
		//SendDebugMessageFormat(hwnd, sdmGlobal, 0, "Addin_ProcessBackspace: EXIT - !OutputBackspace");
		return FALSE;
	}

	BOOL b = (*_td->Addins[_td->CurrentAddin].OutputBackspace)(hwnd);
	//SendDebugMessageFormat(hwnd, sdmGlobal, 0, "Addin_ProcessBackspace: EXIT - (b) = %d", b);
	return b;
}

void Addin_FocusChanged(HWND hwnd)
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return;

	//SendDebugMessageFormat(hwnd, sdmGlobal, 0, "Addin_FocusChanged: ENTER");

	if(_td->CurrenthWnd != hwnd)
		if(!Addin_ShouldProcessUnichar(hwnd)) 
		{
			//SendDebugMessageFormat(hwnd, sdmGlobal, 0, "Addin_FocusChanged: EXIT - !ShouldProcess");
			return;
		}

	if(!LoadAddin()) 
	{
		//SendDebugMessageFormat(hwnd, sdmGlobal, 0, "Addin_FocusChanged: EXIT - !LoadAddin");
		return;
	}

  // Addin variables must be valid now
	
	if(!_td->Addins[_td->CurrentAddin].FocusChanged) 
	{
		//SendDebugMessageFormat(hwnd, sdmGlobal, 0, "Addin_FocusChanged: EXIT - !FocusChanged");
		return;
	}

	(*_td->Addins[_td->CurrentAddin].FocusChanged)();
	//SendDebugMessageFormat(hwnd, sdmGlobal, 0, "Addin_FocusChanged: EXIT");
}
