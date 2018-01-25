(*
  Name:             kpuninstallpackage
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    3 Feb 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Add AutoApply call
                    04 Dec 2006 - mcdurdin - Remove links properly
                    04 Jan 2007 - mcdurdin - Remove uninst.vbs when uninstalling package
                    05 Nov 2007 - mcdurdin - I654 - Avoid crashes when uninstalling a package
                    28 Feb 2011 - mcdurdin - I2751 - When a font is in use, a strange error can be displayed during package uninstall
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    24 Apr 2014 - mcdurdin - I4173 - V9 Uninstalling a keyboard leaves the mnemonic recompiled layouts behind
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
                    03 Jun 2014 - mcdurdin - I4248 - V9.0 - Refactor of kmtip
                    12 Aug 2014 - mcdurdin - I4324 - V9.0 - Keyboard Upgrade from 6.0, 7.0, 8.0
                    04 Nov 2014 - mcdurdin - I4495 - Damaged package causes crash when trying to uninstall [CrashID:kmshell.exe_9.0.473.0_2C6B80C4_EOleException]
                    03 Feb 2015 - mcdurdin - I4574 - V9.0 - If any files are read-only, they need the read-only flag removed on install
*)
unit kpuninstallpackage;  // I3306   // I4248

interface

uses kpbase;

type
  TKPUninstallPackage = class(TKPBase)
    procedure Execute(const PackageName: string; const RemoveFonts: Boolean);
  end;

implementation

uses
  Windows, SysUtils, Messages,
  keymancontext, comobj,
  kpuninstallpackagestartmenu, kpuninstallkeyboard, kpuninstallfont, ErrorControlledRegistry, RegistryKeys,
  utilkeyman, utilsystem, KMPInfFile, packageinfo, keymanerrorcodes, utilfiletypes,
  isadmin, IniFiles, GetOSVersion, shellapi, shlobj, utildir, Variants;

{ TKPUninstallPackage }

procedure TKPUninstallPackage.Execute(const PackageName: string; const RemoveFonts: Boolean);
var
  programs: string;
  SUninstallKey, FName, FilePath, FileName: string;
  i: Integer;
  FIsAdmin: Boolean;
  inf: TKMPInfFile;
  FAutoApply: Boolean;
  Path: string;
begin
  FAutoApply := Context.Control.AutoApply;
  try
    Context.Control.AutoApply := False;

    { Get package registry path }

    if not PackageInstalled(PackageName, FIsAdmin) then
      Error(KMN_E_PackageUninstall_NotFound);

    if FIsAdmin and not IsAdministrator then
      Error(KMN_E_PackageUninstall_AdminRequired);

    with TRegistryErrorControlled.Create do  // I2890
    try
      if FIsAdmin then
      begin
        RootKey := HKEY_LOCAL_MACHINE;
        SUninstallKey := GetRegistryPackageInstallKey_LM(PackageName);
      end
      else
      begin
        RootKey := HKEY_CURRENT_USER;
        SUninstallKey := GetRegistryPackageInstallKey_CU(PackageName);
      end;

      if not OpenKey(SUninstallKey, False) then
        Error(KMN_E_PackageUninstall_NotFound);

      { Get uninstall details }

      if ValueExists(SRegValue_PackageFile) then   // I4495
      begin
//        Error(KMN_E_PackageUninstall_NotFound);

        FileName := ReadString(SRegValue_PackageFile);
        FName := ReadString(SRegValue_PackageDescription);
        FilePath := ExtractFilePath(FileName);

        { Read uninstall options }

        inf := TKMPInfFile.Create;
        try
          inf.FileName := FileName;
          inf.LoadIni;

          { Remove Start Menu items }
          with TIniFile.Create(FileName) do
          try
            programs := IncludeTrailingPathDelimiter(ReadString('Uninstall','StartMenuPath',''));
          finally
            Free;
          end;

          with TKPUninstallPackageStartMenu.Create(inf, FilePath) do   // I4324
          try
            Execute(FIsAdmin, programs);
          finally
            Free;
          end;

          { Uninstall fonts }

          if RemoveFonts then
            with TIniFile.Create(FilePath+'fonts.inf') do
            try
              for i := 0 to inf.Files.Count - 1 do
                if inf.Files[i].FileType = ftFont then
                  if LowerCase(ReadString('UninstallFonts', inf.Files[i].FileName, 'true')) = 'true' then
                    with TKPUninstallFont.Create(Context) do
                    try
                      Execute(FilePath+inf.Files[i].FileName, FIsAdmin);  // I2751
                    finally
                      Free;
                    end;
            finally
              Free;
            end;

          if FileExists(FilePath+'fonts.inf') then
            if not DeleteFileCleanAttr(FilePath+'fonts.inf') then   // I4181   // I4574
            begin
              WarnFmt(KMN_W_PackageUninstall_FileInUse, VarArrayOf([FilePath+'fonts.inf']));
            end;

          { Uninstall sub packages and keyboards }

          for i := 0 to inf.Files.Count - 1 do
            try
              if inf.Files[i].FileType = ftKeymanFile then
                with TKPUninstallKeyboard.Create(Context) do
                try
                  Execute(inf.Files[i].FileName, [upPartOfPackage]);
                finally
                  Free;
                end;
            except
              on E:EOleException do
                WarnFmt(KMN_W_UninstallFileInUse, VarArrayOf([FilePath+inf.Files[i].FileName]));
            end;

          { Delete installed files }

          for i := 0 to inf.Files.Count - 1 do
            if FileExists(FilePath + inf.Files[i].FileName) then
              if not DeleteFileCleanAttr(FilePath + inf.Files[i].FileName) then   // I4181   // I4574
              begin
                WarnFmt(KMN_W_PackageUninstall_FileInUse, VarArrayOf([FilePath + inf.Files[i].FileName]));
              end;
        finally
          inf.Free;
        end;
      end
      else
      begin
        FileName := GetPackageInstallPath(PackageName) + '\kmp.inf';
      end;

      if FIsAdmin then
      begin
        RootKey := HKEY_LOCAL_MACHINE;
        Path := '\'+SRegKey_InstalledPackages_LM+'\'+GetShortPackageName(PackageName);
      end
      else
      begin
        RootKey := HKEY_CURRENT_USER;
        Path := '\'+SRegKey_InstalledPackages_CU+'\'+GetShortPackageName(PackageName);
      end;

      DeleteKey(Path);

      if not RecursiveDelete(ExtractFileDir(FileName)) then   // I4173   // I4181
      begin
        WarnFmt(KMN_W_PackageUninstall_FileInUse, VarArrayOf([ExtractFileDir(FileName)]));
      end;

      PostMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
    finally
      Free;
    end;
  finally
    Context.Control.AutoApply := FAutoApply;
    Context.Control.AutoApplyKeyman;
  end;
end;

end.

