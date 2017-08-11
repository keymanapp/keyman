/*
  Name:             dllmain
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      7 Sep 2009

  Modified Date:    20 Nov 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          07 Sep 2009 - mcdurdin - I2095 - Fixup thread-local variables
                    20 Nov 2012 - mcdurdin - I3580 - V9.0 - KMTip no longer needs global process and thread initialisation
*/
//
// dllmain.cpp
//
// DllMain module entry point.
//

#include "globals.h"

//+---------------------------------------------------------------------------
//
// DllMain
//
//----------------------------------------------------------------------------

BOOL WINAPI DllMain(HINSTANCE hInstance, DWORD dwReason, LPVOID pvReserved)
{
    switch (dwReason)
    {
        case DLL_PROCESS_ATTACH:
            g_hInst = hInstance;
            InitializeCriticalSection(&g_cs);
            //DisableThreadLibraryCalls(hInstance);  per documentation, required for static link of CRT   // I3580
            break;

        case DLL_PROCESS_DETACH:
            DeleteCriticalSection(&g_cs);
            break;
    }

    return TRUE;
}
