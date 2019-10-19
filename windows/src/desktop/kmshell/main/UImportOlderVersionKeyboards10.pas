(*
  Name:             UImportOlderVersionKeyboards10
  Copyright:        Copyright (C) 2003-2017 SIL International.
*)
unit UImportOlderVersionKeyboards10;

interface

procedure ImportOlderVersionKeyboards10(DoAdmin: Boolean);   // I2361

implementation

uses
  Keyman.System.UpgradeRegistryKeys,
  UImportOlderVersionKeyboards9Plus,
  UImportOlderKeyboardUtils;

procedure ImportOlderVersionKeyboards10(DoAdmin: Boolean);  // I2361
begin
  with TImportKeyman9PlusKeyboard.Create(DoAdmin, True) do  // I2361
  try
    SRegKey_KeymanEngineOld_InstalledPackages_CU := SRegKey_KeymanEngine100_InstalledPackages_CU;
    SRegKey_KeymanEngineOld_InstalledKeyboards_CU := SRegKey_KeymanEngine100_InstalledKeyboards_CU;
    SRegKey_KeymanEngineOld_ActiveKeyboards_CU := SRegKey_KeymanEngine100_ActiveKeyboards_CU;
    Import;
  finally
    Free;
  end;
end;

end.
