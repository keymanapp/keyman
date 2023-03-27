/*
  Name:             versioninfo
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      11 Dec 2009

  Modified Date:    4 May 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    04 May 2010 - mcdurdin - I2357 - Version info update for Windows 7
*/

#include "pch.h"

#pragma warning(disable: 4996)
void GetWindowsVersion(char *buf)
{
	OSVERSIONINFOEX osvi;

	char *pplatform = "unknown", *pos = "unknown", *pextra = "";
	char verinfo[256];
	verinfo[0] = 0;

	osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
  GetVersionEx((OSVERSIONINFO *)&osvi);
	
	switch(osvi.dwPlatformId)
	{
	case VER_PLATFORM_WIN32_NT:
		pplatform = "NT";
		if ( osvi.dwMajorVersion <= 4 )
			pos = "Microsoft Windows NT";

		if ( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0 )
			pos = "Microsoft Windows 2000";

		if ( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1 )
			pos = "Microsoft Windows XP";

		if ( osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 0 )
			pos = "Microsoft Windows Vista";

		if ( osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 1 )
			pos = "Microsoft Windows 7";

    // TODO: Update this for Win8, Win10, etc

		// Test for product type.

    if ( osvi.wProductType == VER_NT_WORKSTATION )
    {
       if( osvi.wSuiteMask & VER_SUITE_PERSONAL )
          pextra = "Personal";
       else
          pextra = "Professional";
    }

    else if ( osvi.wProductType == VER_NT_SERVER )
    {
       if( osvi.wSuiteMask & VER_SUITE_DATACENTER )
          pextra = "DataCenter Server";
       else if( osvi.wSuiteMask & VER_SUITE_ENTERPRISE )
          pextra = "Advanced Server";
       else
          pextra = "Server";
    }

    // Display version, service pack (if any), and build number.

    if ( osvi.dwMajorVersion <= 4 )
    {
      wsprintf (verinfo, "version %d.%d %s (Build %d)",
         (int) osvi.dwMajorVersion,
         (int) osvi.dwMinorVersion,
         osvi.szCSDVersion,
         (int)(osvi.dwBuildNumber & 0xFFFF));
    }
    else
    { 
      wsprintf (verinfo, "version %s (Build %d)",
         osvi.szCSDVersion,
         (int)(osvi.dwBuildNumber & 0xFFFF));
    }
    break;

  case VER_PLATFORM_WIN32_WINDOWS:
	
		 pplatform = "9x";

         if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0)
         {
             pos = "Microsoft Windows 95";
             if ( osvi.szCSDVersion[1] == 'C' || osvi.szCSDVersion[1] == 'B' )
                pextra = "OSR2";
         } 

         if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 10)
         {
             pos = "Microsoft Windows 98";
             if ( osvi.szCSDVersion[1] == 'A' )
                pextra = "SE";
         } 

         if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 90)
         {
             pos = "Microsoft Windows Me";
         } 
         break;

      case VER_PLATFORM_WIN32s:

         pplatform = "Win32s";
		 pos = "Microsoft Win32s";
         break;
   }

   wsprintf(buf, "Platform %s OS %s %s %s", pplatform, pos, *pextra ? " " : "", pextra, verinfo);
}
#pragma warning(default: 4996) 
