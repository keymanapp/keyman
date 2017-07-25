(*
  Name:             GetOsVersion
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Dec 2006

  Modified Date:    28 May 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Dec 2006 - mcdurdin - Add osVista
                    04 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    24 Oct 2012 - mcdurdin - I3487 - V9.0 - Add detection of Win8
                    13 Dec 2012 - mcdurdin - I3669 - V9.0 - Eliminate old Windows version tests and constants
                    28 May 2014 - mcdurdin - I4222 - V9.0 - Deprecate osWin2000, osWinXP, osWin2003Server
*)
unit GetOsVersion;  // I3306

interface

uses
  Winapi.Windows;

type
  TOS = (osLegacy, osVista, osWin7, osWin8, osWin10, osOther);   // I3669   // I4222

function GetOs: TOS;

implementation

function GetOs: TOS;
var
  osv: TOSVersionInfo;
begin
  osv.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(osv);

  Result := osOther;

  if osv.dwPlatformId = VER_PLATFORM_WIN32_NT then   // I3669
  begin
    case osv.dwMajorVersion of
      5: Result := osLegacy;
      6: case osv.dwMinorVersion of
           0: Result := osVista;
           1: Result := osWin7;
           2: Result := osWin8;  // I3487
           else Result := osWin8;  // I3669
         end;
      10: Result := osWin10;
    else
      Result := osWin10;
    end;
  end;
end;

end.

