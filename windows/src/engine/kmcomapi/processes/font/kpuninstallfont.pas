(*
  Name:             kpuninstallfont
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
  History:          14 Sep 2006 - mcdurdin - If unable to get font details, add a warning rather than exception
                    30 May 2007 - mcdurdin - I850 - Fix font uninstall in Vista
                    28 Feb 2011 - mcdurdin - I2751 - When a font is in use, a strange error can be displayed during package uninstall
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
                    03 Feb 2015 - mcdurdin - I4574 - V9.0 - If any files are read-only, they need the read-only flag removed on install
*)
unit kpuninstallfont;

interface

uses kpbase;

type
  TKPUninstallFont = class(TKPBase)
    procedure Execute(const src_filename: string; FIsAdmin: Boolean);
  end;

implementation

uses
  windows, sysutils, shellapi, shlobj, ttinfo,
  utildir, utilsystem,
  ErrorControlledRegistry, registrykeys, getosversion, keymanerrorcodes, Variants;

procedure TKPUninstallFont.Execute(const src_FileName: string; FIsAdmin: Boolean);
var
  filename, fontnm: string;
begin
  if not FIsAdmin then  // I2751
  begin
    if not RemoveFontResource(PChar(src_FileName)) then
      WarnFmt(KMN_W_UninstallFont_FontInUse, VarArrayOf([fontnm]));
  end
  else
  begin
    filename := GetFolderPath(CSIDL_FONTS) + ExtractFileName(src_filename);
    try
      with TTTInfo.Create(FileName, [tfNames]) do
      try
        fontnm := FullName;
      finally
        Free;
      end;
    except
      on E:Exception do
      begin
        WarnFmt(KMN_W_UninstallFileNotFound, VarArrayOf([FileName]));
        Exit;
      end;
    end;

    with TRegistryErrorControlled.Create do  // I2890
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey(SRegKey_NTFontList_LM, False) and ValueExists(fontnm + ' (TrueType)') then
      begin
        if LowerCase(ReadString(fontnm + ' (TrueType)')) = LowerCase(ExtractFileName(FileName)) then
        begin
          DeleteValue(fontnm + ' (TrueType)');
          if not RemoveFontResource(PChar(FileName)) then  // I2751
          begin
            WarnFmt(KMN_W_UninstallFont_FontInUse, VarArrayOf([fontnm]));
          end;
          if not DeleteFileCleanAttr(filename) then   // I4574
          begin
            WarnFmt(KMN_W_UninstallFont_FontInUse, VarArrayOf([fontnm]));   // I4181
          end;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

end.
