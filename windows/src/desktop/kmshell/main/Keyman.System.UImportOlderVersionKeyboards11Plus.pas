unit Keyman.System.UImportOlderVersionKeyboards11Plus;
(*
  Name:             UImportOlderVersionKeyboards11Plus
  Copyright:        Copyright (C) 2019 SIL International.
*)

interface

procedure ImportOlderVersionKeyboards11Plus(DoAdmin: Boolean);

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Win.Registry,

  kmint,
  keymanapi_TLB,
  Keyman.System.UpgradeRegistryKeys,
  RegistryKeys,
  UImportOlderVersionKeyboards9Plus,
  UImportOlderKeyboardUtils;

procedure TransferLanguageHotkeys; forward;

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

  TransferLanguageHotkeys;
end;

procedure TransferLanguageHotkeys;
var
  bcp47: string;
  id: string;
  rcu: TRegistry;
  s: TStringList;
//  fprofiles: TDictionary<string,TProfileInfo>;
  n, i: Integer;
  lang: IKeymanLanguage;
  value: Integer;
  j: Integer;
begin
  rcu := TRegistry.Create;
  s := TStringList.Create;

  kmcom.Refresh;

  try
    if rcu.OpenKeyReadOnly(SRegKey_UpgradeBackupPath_CU + '\' + SRegSubKey_LanguageHotkeys_CU) then
    begin
      rcu.GetValueNames(s);
      for i := 0 to s.Count - 1 do
      begin
        n := Pos(s[i], ';');
        if n = 0 then
          Continue;
        bcp47 := Copy(s[i], 1, n-1);
        id := Copy(s[i], n+1, MAXINT);
        if not TryStrToInt(rcu.ReadString(s[i]), value) then
          Continue;

        for j := 0 to kmcom.Languages.Count - 1 do
        begin
          lang := kmcom.Languages[j];
          if SameText(lang.BCP47Code, bcp47) and
            (lang.KeymanKeyboardLanguage <> nil) and
            (lang.KeymanKeyboardLanguage.OwnerKeyboard <> nil) and
            SameText(lang.KeymanKeyboardLanguage.OwnerKeyboard.ID, id) then
          begin
            lang.Hotkey.RawValue := value;
            Break;
          end;
        end;
      end;
    end;
  finally
    rcu.Free;
    s.Free;
  end;

  kmcom.Apply;
end;

end.
