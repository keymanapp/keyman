(*
  Name:             preuninstallunit
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      3 May 2011

  Modified Date:    26 Jun 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    26 Jun 2014 - mcdurdin - I4302 - V9 - Refactor uninstall of TIP and its profiles into separate unit
*)
unit preuninstallunit;

interface

uses JwaWinType, JwaMsi, JwaMsiQuery, JwaWinError, JwaWinReg, JwaWinUser, JwaWinBase;

function PreUninstall(hInstall: MSIHANDLE): UINT; stdcall;   // I4302

implementation

uses
  System.Classes,
  System.SysUtils,
  Winapi.ActiveX,
  input_installlayoutortip,
  utiltsf;

function PreUninstall(hInstall: MSIHANDLE): UINT;
begin
  try
    CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
    try
      UnregisterTIPAndItsProfiles(c_clsidKMTipTextService);
    finally
      CoUninitialize;
    end;
  except
    ; // Swallow exceptions so we don't break the uninstall
  end;

  Result := ERROR_SUCCESS;
end;

end.
