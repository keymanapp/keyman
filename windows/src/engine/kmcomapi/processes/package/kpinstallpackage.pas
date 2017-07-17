(*
  Name:             kpinstallpackage
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    3 Feb 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Add AutoApplyKeyman call, avoid processmessages in unzip
                    14 Sep 2006 - mcdurdin - Send WM_FONTCHANGE after font install, not before!
                    06 Oct 2006 - mcdurdin - Fix all users/current user mismatch for keyboard and package
                    04 Dec 2006 - mcdurdin - Add uninstall link support
                    12 Dec 2006 - mcdurdin - Test for installed keyboard count
                    15 Jan 2007 - mcdurdin - Fix (Start Product) and related targets and icons thereof
                    07 Feb 2007 - mcdurdin - Fix install of packages when LIGHT Edition was installe
                    16 May 2007 - mcdurdin - Fix widestring mismatch
                    23 Aug 2007 - mcdurdin - I956 - Support locale.xml in kmp files
                    05 Nov 2007 - mcdurdin - I1109 - Selected keyboards for Light
                    14 Jun 2008 - mcdurdin - I1271 - OSK tries to install when associated keyboard is not installed
                    14 Jun 2008 - mcdurdin - I1320 - Fixup locale.xml doctype parsing and replacement
                    16 Jan 2009 - mcdurdin - I1636 - Fix crash trying to read a missing package file
                    04 Jun 2009 - mcdurdin - I2003 - UTF8Encode replacement
                    26 Jul 2010 - mcdurdin - I2467 - 8.0 renumber
                    18 Mar 2011 - mcdurdin - I2176 - uninst.vbs does not work in Win64
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3337 - V9.0 - Review of input/output for Unicode
                    17 Aug 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    01 Dec 2012 - mcdurdin - I3612 - V9.0 - Keyboard install should run as Admin only
                    03 Jun 2014 - mcdurdin - I4248 - V9.0 - Refactor of kmtip
                    12 Aug 2014 - mcdurdin - I4324 - V9.0 - Keyboard Upgrade from 6.0, 7.0, 8.0
                    03 Feb 2015 - mcdurdin - I4574 - V9.0 - If any files are read-only, they need the read-only flag removed on install
*)
unit kpinstallpackage;  // I3306   // I4248

interface

uses kpbase, keymanapi_TLB;

type
  TKPInstallPackage = class(TKPBase)
  private
    procedure UpdateLocaleDoctype(const path: WideString);
  public
    procedure Execute(const FileName: string; Force: Boolean);
  end;

implementation

uses
  Windows,
  Classes,
  System.Zip,
  custinterfaces,
  Unicode,
  SysUtils,
  StrUtils,
  KLog,
  keymanerrorcodes,
  keymancontext,
  KeymanPaths,
  kmxfile,
  KPInstallKeyboard, KPInstallVisualKeyboard,
  KPInstallPackageStartMenu,
  kmpinffile, getosversion, utilsystem, shlobj, inifiles, messages,
  OnlineConstants,
  kpinstallfont, utilfiletypes, PackageInfo, ErrorControlledRegistry,
  RegistryKeys, utilkeyman, utildir,

  isadmin, Variants, keymancontrol,
  VisualKeyboard,
  xmldoc,
  xmlintf, WideStrings;

{ TKPInstallPackage }


procedure TKPInstallPackage.Execute(const FileName: string; Force: Boolean);
      function GetHHIcon: string;
      var
        buf: array[0..260] of char;
      begin
        GetWindowsDirectory(buf, 260);
        Result := IncludeTrailingPathDelimiter(buf)+'hh.exe';
        if FileExists(Result) then Exit;
        GetSystemDirectory(buf, 260);
        Result := IncludeTrailingPathDelimiter(buf)+'hh.exe';
        if FileExists(Result) then Exit;
        Result := '';
      end;
var
  FAutoApply: Boolean;
  FZip: TZipFile;
  FTempOutPath: string;
  i: Integer;
  FInstByAdmin: Boolean;
  buf: array[0..260] of Char;
  InfFile: string;
  inf: TKMPInfFile;
  ki: array of TKeyboardInfo;
  PackageName, dest, prog, errmsg: string;
  FErrorValue: Cardinal;
  FSrcFileName: string;
begin
  KL.MethodEnter(Self, 'Execute', [FileName, Force]);
  try
    if not IsAdministrator then
      Error(KMN_E_Install_KeyboardMustBeInstalledByAdmin);   // I3612

    FAutoApply := Context.Control.AutoApply;
    try
      Context.Control.AutoApply := False;

      if GetTempPath(260, buf) = 0 then
        Error(KMN_E_PackageInstall_UnableToGetTempPath);
      FTempOutPath := buf;
      if GetTempFileName(PChar(FTempOutPath), 'kmp', 0, buf) = 0 then
        Error(KMN_E_PackageInstall_UnableToGetTempFileName);
      if not DeleteFileCleanAttr(buf) then      // TODO: Resolve race condition: temp file deleted and could be reused before temp directory is created.   // I4574
        Error(KMN_E_PackageInstall_UnableToGetTempFilename);
      if not CreateDir(buf) then
        Error(KMN_E_PackageInstall_UnableToCreateTemporaryDirectory);

      FTempOutPath := buf; FTempOutPath := FTempOutPath + '\';

      inf := nil;
      FZip := TZipFile.Create;
      try
        FZip.Open(FileName, TZipMode.zmRead);

        InfFile := '';

        for i := 0 to FZip.FileCount - 1 do
        begin
          FZip.Extract(i, buf, False);
          if LowerCase(FZip.Filename[i]) = 'kmp.inf' then
            InfFile := FZip.Filename[i];
        end;

        if InfFile = '' then
          Error(KMN_E_PackageInstall_UnableToFindInfFile);

        inf := TKMPInfFile.Create;
        inf.FileName := FTempOutPath + InfFile;
        inf.LoadIni;

        inf.CheckFiles(FTempOutPath);

        { Check keyboards to install are valid }

        SetLength(ki, inf.Files.Count);
        for i := 0 to inf.Files.Count - 1 do
        begin
          if inf.Files[i].FileType = ftKeymanFile then
          begin
            try
              GetKeyboardInfo(FTempOutPath + inf.Files[i].FileName, False, ki[i]);
            except
              on E:EKMXError do
                ErrorFmt(KMN_E_Install_InvalidFile, VarArrayOf([ExtractFileName(FileName), E.Message]));
            end;
          end;
        end;

        { Install registry entries }

        PackageName := GetShortPackageName(FileName);
        dest := GetPackageInstallPath(FileName) + '\';

        if PackageInstalled(PackageName, FInstByAdmin) and not Force then
          Error(KMN_E_PackageInstall_PackageAlreadyInstalled);

        with TRegistryErrorControlled.Create do  // I2890
        try
          RootKey := HKEY_LOCAL_MACHINE;

          if not OpenKey(SRegKey_InstalledPackages+'\'+PackageName, True) then  // I2890
            RaiseLastRegistryError;

          WriteString(SRegValue_PackageFile, dest + 'kmp.inf');
          WriteString(SRegValue_PackageDescription, inf.Info.Desc[PackageInfoEntryTypeNames[pietName]]);
        finally
          Free;
        end;

        { Copy files }

        ForceDirectories(ExtractFileDir(dest));

        for i := 0 to inf.Files.Count - 1 do
        begin
          FSrcFileName := inf.Files[i].FileName;

          if not CopyFileCleanAttr(PChar(FTempOutPath + FSrcFileName), PChar(dest + FSrcFileName), False) then   // I4574
          begin
            FErrorValue := GetLastError;
            if inf.Files[i].FileType = ftFont
              then WarnFmt(KMN_E_PackageInstall_UnableToCopyFile, VarArrayOf([FTempOutPath + inf.Files[i].FileName + ' ['+SysErrorMessage(FErrorValue)+']', dest + inf.Files[i].FileName]))
              else ErrorFmt(KMN_E_PackageInstall_UnableToCopyFile, VarArrayOf([FTempOutPath + inf.Files[i].FileName + ' ['+SysErrorMessage(FErrorValue)+']', dest + inf.Files[i].FileName]));
          end;
        end;

        { Install the keyboards, packages and fonts }

        for i := 0 to inf.Files.Count - 1 do
        begin
          case inf.Files[i].FileType of
            ftOther, ftXMLFile:
              begin
                if Copy(inf.Files[i].FileName, 1, 7) = 'locale-' then
                begin
                  // Update the locale file's
                  UpdateLocaleDoctype(dest + inf.Files[i].FileName);
                end;
              end;
            ftKeymanFile:
              // I1109: Don't install non-selected keyboards for Light
              with TKPInstallKeyboard.Create(Context) do
              try
                Execute(dest + inf.Files[i].FileName, PackageName, [ikPartOfPackage], Force);
              finally
                Free;
              end;
            ftPackageFile:
              with TKPInstallPackage.Create(Context) do
              try
                Execute(dest + inf.Files[i].FileName, Force);
              finally
                Free;
              end;
          end;
        end;

        { Install the visual keyboards -- must be after keyboard installs }

        for i := 0 to inf.Files.Count - 1 do
        begin
          if inf.Files[i].FileType = ftVisualKeyboard then
            with TKPInstallVisualKeyboard.Create(Context) do
            try
              Execute(FTempOutPath + inf.Files[i].FileName, '');
            finally
              Free;
            end;
        end;

        { Install start menu items }

        with TKPInstallPackageStartMenu.Create(inf, PackageName, dest) do   // I4324
        try
          Execute(True, '');
        finally
          Free;
        end;

        { Install fonts }

        with TIniFile.Create(dest + 'fonts.inf') do
        try
          for i := 0 to inf.Files.Count - 1 do
            if inf.Files[i].FileType = ftFont then
              with TKPInstallFont.Create(Context) do
              try
                if not Execute(dest + inf.Files[i].FileName)
                  then WriteString('UninstallFonts', inf.Files[i].FileName, 'False')
                  else WriteString('UninstallFonts', inf.Files[i].FileName, 'True');
              finally
                Free;
              end;
          UpdateFile;
        finally
          Free;
        end;

        PostMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);

        { Run the commandline, if set }

        prog := Trim(inf.Options.ExecuteProgram);
        if prog <> '' then
        begin
          prog := StringReplace(prog, '$keyman', ExtractFileDir(GetKeymanInstallPath), [rfIgnoreCase, rfReplaceAll]);
          if not ExecuteProgram(prog, PChar(ExtractFileDir(dest)), errmsg) then
            WarnFmt(KMN_W_InstallPackage_CannotRunExternalProgram, VarArrayOf([prog, errmsg]));
        end;

        { Complete }
      finally
        for i := 0 to FZip.FileCount - 1 do
          DeleteFileCleanAttr(FTempOutPath + FZip.FileName[i]);   // I4574
        RemoveDir(Copy(FTempOutPath, 1, Length(FTempOutPath)-1));
        FZip.Free;
        inf.Free;
      end;
    finally
      Context.Control.AutoApply := FAutoApply;
      Context.Control.AutoApplyKeyman;
    end;
  finally
    KL.MethodExit(Self, 'Execute');
  end;
end;

procedure TKPInstallPackage.UpdateLocaleDoctype(const path: WideString);
var
  I: Integer;
  localedefpath: WideString;
  ss: Widestring;
  n1: Integer;
  n2: Integer;
  l: Integer;
begin
  localedefpath := TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_Xml_LocaleDef);

  with TStringList.Create do
  try
    LoadFromFile(path);  // Rely on preamble for encoding
    ss := Text;   // I1320 - fix locale.xml doctype parsing and replacement

    n1 := Pos('<!DOCTYPE', string(ss));
    if n1 > 0 then
    begin
      i := 1; n2 := n1 + 8; l := Length(ss);
      while (i > 0) and (n2 <= l) do
      begin
        case ss[n2] of
          '<': Inc(i);
          '>': Dec(i);
        end;
        Inc(n2);
      end;

      if n2 <= l then
      begin
        Text := Copy(ss, 1, n1-1) + '<!DOCTYPE Locale SYSTEM '''+localedefpath+'''>' + Copy(ss, n2+1, l);

        SaveToFile(path);  // Stay with previous encoding
      end;
    end;
  finally
    Free;
  end;
end;

end.
