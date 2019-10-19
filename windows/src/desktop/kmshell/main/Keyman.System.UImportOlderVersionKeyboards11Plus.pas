unit Keyman.System.UImportOlderVersionKeyboards11Plus;
(*
  Name:             UImportOlderVersionKeyboards11Plus
  Copyright:        Copyright (C) 2019 SIL International.
*)

interface

procedure ImportOlderVersionKeyboards11Plus(DoAdmin: Boolean);

implementation

uses
  Keyman.System.UpgradeRegistryKeys,
  UImportOlderVersionKeyboards9Plus,
  UImportOlderKeyboardUtils;

//
// All Keyman versions from 11.0 onward use the same registry + file layout and
// aim to be forward-and-back compatible with API and files. This means we
// will be able to run earlier versions of Developer with later versions of
// Desktop, etc. So this upgrade process should not need to be duplicated in the
// future as we have had to do with earlier versions.
//

procedure ImportOlderVersionKeyboards11Plus(DoAdmin: Boolean);
begin
  with TImportKeyman9PlusKeyboard.Create(DoAdmin, False) do
  try
    SRegKey_KeymanEngineOld_InstalledPackages_CU := SRegKey_KeymanEngine110Plus_InstalledPackages_CU;
    SRegKey_KeymanEngineOld_InstalledKeyboards_CU := SRegKey_KeymanEngine110Plus_InstalledKeyboards_CU;
    SRegKey_KeymanEngineOld_ActiveKeyboards_CU := SRegKey_KeymanEngine110Plus_ActiveKeyboards_CU;
    Import;
  finally
    Free;
  end;
end;

end.
