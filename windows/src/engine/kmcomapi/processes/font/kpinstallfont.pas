(*
  Name:             kpinstallfont
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    3 Feb 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Fix bug where registry was still in read-only state
                    30 May 2007 - mcdurdin - I850 -  Fix font installation under Vista
                    19 Jun 2007 - mcdurdin - I815 - Install font for current user
                    18 Mar 2011 - mcdurdin - I2751 - When font is in use, strange errors can appear during install/uninstall
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    03 Feb 2015 - mcdurdin - I4574 - V9.0 - If any files are read-only, they need the read-only flag removed on install
*)
unit kpinstallfont;

interface

uses kpbase;

type
  TKPInstallFont = class(TKPBase)
    function Execute(const src_filename: string): Boolean;
  end;

implementation

uses
  windows, sysutils, shellapi, shlobj, ttinfo, utildir, utilsystem,
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
      WarnFmt(KMN_W_InstallFont_CannotInstallFont,
        VarArrayOf([ExtractFileName(filename), SysErrorMessage(GetLastError), Integer(GetLastError)]));
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

  if CopyFileCleanAttr(PChar(src_filename), PChar(filename), True) then  // I2751   // I4574
  begin
    with TRegistryErrorControlled.Create do  // I2890
    try
      RootKey := HKEY_LOCAL_MACHINE;

      if not OpenKey(SRegKey_NTFontList_LM, False) then
      begin
        WarnFmt(KMN_W_InstallFont_CannotInstallFontAdmin, VarArrayOf([ExtractFileName(filename)]));
        Exit;
      end;

      if AddFontResource(PChar(filename)) = 0 then
      begin
        WarnFmt(KMN_W_InstallFont_CannotInstallFont,
          VarArrayOf([ExtractFileName(filename), SysErrorMessage(GetLastError), Integer(GetLastError)]));
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
      WarnFmt(KMN_W_InstallFont_CannotInstallFont,
        VarArrayOf([ExtractFileName(filename), SysErrorMessage(GetLastError), Integer(GetLastError)]));
      Exit;
    end;
    
    Result := True; // font should be uninstalled - it will be loaded by Keyman (I815)
  end;
end;

end.
