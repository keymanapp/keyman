(*
  Name:             UImportOlderVersionKeyboards9
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      30 Nov 2010

  Modified Date:    4 Nov 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          30 Nov 2010 - mcdurdin - I2548 - Support for upgrading Desktop 7 to Desktop 8
                    10 Dec 2010 - mcdurdin - I2361 - Support admin and non-admin modes (relates to setup elevation)
                    11 Jan 2011 - mcdurdin - I2642 - Installer uninstalls KM7 keyboards before upgrade can happen
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    25 Jun 2014 - mcdurdin - I4296 - CrashID:kmshell.exe_9.0.456.0_2C405C69_EInvalidPointer
                    25 Jun 2014 - mcdurdin - I4297 - V9.0 - Upgrade of keyboards fails to register in right hive
                    25 Jun 2014 - mcdurdin - I4298 - V9.0 - Old TSF addin remains registered when upgrading
                    25 Jun 2014 - mcdurdin - I4299 - V9.0 - Keyman-installed Windows languages should be removed when upgrading
                    26 Jun 2014 - mcdurdin - I4302 - V9 - Refactor uninstall of TIP and its profiles into separate unit
                    04 Nov 2014 - mcdurdin - I4494 - Crash calling TSF [CrashID:kmshell.exe_9.0.473.0_2C45D42D_EOleSysError]
*)
unit UImportOlderVersionKeyboards9;

interface

procedure ImportOlderVersionKeyboards9(DoAdmin: Boolean);   // I2361

implementation

uses
  Keyman.System.UpgradeRegistryKeys,
  UImportOlderVersionKeyboards9Plus,
  UImportOlderKeyboardUtils,
  utiltsf;

procedure ImportOlderVersionKeyboards9(DoAdmin: Boolean);  // I2361
begin
  with TImportKeyman9PlusKeyboard.Create(DoAdmin, True) do  // I2361
  try
    SRegKey_KeymanEngineOld_InstalledPackages_CU := SRegKey_KeymanEngine90_InstalledPackages_CU;
    SRegKey_KeymanEngineOld_InstalledKeyboards_CU := SRegKey_KeymanEngine90_InstalledKeyboards_CU;
    SRegKey_KeymanEngineOld_ActiveKeyboards_CU := SRegKey_KeymanEngine90_ActiveKeyboards_CU;
    c_clsidKMTipTextService_Old := c_clsidKMTipTextService_90;
    Import;
    UnregisterKMTIPFromOldVersion;
  finally
    Free;
  end;
end;

end.
