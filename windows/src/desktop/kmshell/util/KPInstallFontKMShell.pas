unit KPInstallFontKMShell;

interface

type
  // Dup from KMCOMAPI
  TKPInstallFont = class
    function Execute(const src_filename: string): Boolean;
  end;

implementation

uses
  windows, sysutils, shellapi, shlobj, ttinfo, utilsystem,
  ErrorControlledRegistry, registrykeys, getosversion, keymanerrorcodes, Variants;

function TKPInstallFont.Execute(const src_filename: string): Boolean;
var
  filename, fontnm: string;
begin
  Result := False;

  with TTTInfo.Create(src_filename, [tfNames]) do
  try
    fontnm := FullName;
  finally
    Free;
  end;

  filename := GetFolderPath(CSIDL_FONTS) + ExtractFileName(src_filename);

  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if not OpenKeyReadOnly(SRegKey_NTFontList_LM) then
    begin
//      WarnFmt(KMN_W_InstallFont_CannotInstallFont,
//        VarArrayOf([ExtractFileName(filename), SysErrorMessage(GetLastError), Integer(GetLastError)]));
      Exit;
    end;

    if ValueExists(fontnm + ' (TrueType)') then
    begin
      // we'll assume we don't need to update for now?
      Result := False; // not installed, so don't uninstall
      Exit;
    end;
  finally
    Free;
  end;

  if CopyFile(PChar(src_filename), PChar(filename), True) then  // I2751
  begin
    with TRegistryErrorControlled.Create do  // I2890
    try
      RootKey := HKEY_LOCAL_MACHINE;

      if not OpenKey(SRegKey_NTFontList_LM, False) then
      begin
//        WarnFmt(KMN_W_InstallFont_CannotInstallFontAdmin, VarArrayOf([ExtractFileName(filename)]));
//        Exit;
      end;

      if AddFontResource(PChar(filename)) = 0 then
      begin
//        WarnFmt(KMN_W_InstallFont_CannotInstallFont,
//          VarArrayOf([ExtractFileName(filename), SysErrorMessage(GetLastError), Integer(GetLastError)]));
        Exit;
      end;

      WriteString(fontnm + ' (TrueType)', ExtractFileName(filename));

      Result := True; // font should be uninstalled.
    finally
      Free;
    end;
  end
  else
  begin
    // Not IsAdmin
    if AddFontResource(PChar(src_filename)) = 0 then  // I2751
    begin
//      WarnFmt(KMN_W_InstallFont_CannotInstallFont,
//        VarArrayOf([ExtractFileName(filename), SysErrorMessage(GetLastError), Integer(GetLastError)]));
      Exit;
    end;

    Result := True; // font should be uninstalled - it will be loaded by Keyman (I815)
  end;
end;

end.
