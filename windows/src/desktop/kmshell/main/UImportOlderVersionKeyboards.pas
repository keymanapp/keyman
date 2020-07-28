(*
  Name:             UImportOlderVersionKeyboards
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      28 Aug 2008

  Modified Date:    17 Mar 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          28 Aug 2008 - mcdurdin - I1616 - Upgrade keyboards from 6.x
                    30 Nov 2010 - mcdurdin - I2548 - Support for upgrading Desktop 7 to Desktop 8
                    10 Dec 2010 - mcdurdin - I2361 - Support admin and non-admin modes (relates to setup elevation)
                    18 Mar 2011 - mcdurdin - I2792 - Keyman Configuration sometimes crashes when importing older keyboards
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    01 Dec 2012 - mcdurdin - I3613 - V9.0 - System shadow keyboards obsolete, strip out remaining code
                    28 May 2014 - mcdurdin - I4220 - V9.0 - Remove references to LoadKeyboardLayout, Preload, Substitutes, etc. and use only TSF
                    16 Jun 2014 - mcdurdin - I4185 - V9.0 - Upgrade V8.0 keyboards to 9.0
                    12 Aug 2014 - mcdurdin - I4324 - V9.0 - Keyboard Upgrade from 6.0, 7.0, 8.0
                    13 Mar 2015 - mcdurdin - I4617 - V9.0 - keymanimport.log is generated incorrectly as unicode strings in an ansi file
                    17 Mar 2015 - mcdurdin - I4623 - .kmx installs upgraded from earlier versions are placed in the wrong folder [CrashID:kmshell.exe_9.0.486.0_2C6795C6_EOleException]
*)
unit UImportOlderVersionKeyboards;   // I3613

interface

uses
  Windows,
  Classes,
  ErrorControlledRegistry,
  SysUtils;

type
  TImportKeymanKeyboardBase = class
  private
    FLogFile: TFileStream;
    function MoveStartMenu(packagename, srcinf, dstinf: string): Boolean;   // I4324
    function CopyAndInstallFonts(dstinf: string): Boolean;   // I4324

  public
    constructor Create(AAdmin: Boolean);  // I2361
    destructor Destroy; override;

  protected
    FAdmin: Boolean;
    regRead, regWrite: TRegistryErrorControlled;  // I2890
    rootRead, rootWrite: string;
    values: TStringList;

    procedure Log(const s: string);
    procedure LogFailure(const src: string);

    function CopyFiles(const src, dst: string): Boolean;
    function CopyKeyboard(const keyboardname: string): Boolean;
    procedure CopyKeyboardActive(const keyboardname: string);
    function CopyPackage(const packagename: string): Boolean;
    procedure CopyValue(const name: string);

    //TODO: procedure InstallKeyboardProfile
  end;

implementation

uses
  Winapi.Messages,

  GetOsVersion,
  IniFiles,
  KeymanPaths,
  kmpinffile,
  Keyman.System.UpgradeRegistryKeys,
  KPInstallFontKMShell,
  KPInstallPackageStartMenu,
  KPUninstallPackageStartMenu,
  RegistryKeys,
  StrUtils,
  utildir,
  utilfiletypes,
  utilsystem,
  UImportOlderKeyboardUtils;

function TImportKeymanKeyboardBase.CopyFiles(const src, dst: string): Boolean;
var
  f: TSearchRec;
begin
  Result := False;
  Log('Copying files from '+src+' to '+dst);
  if not ForceDirectories(dst) then
  begin
    LogFailure('ForceDirectories');
    Exit;
  end;
  if FindFirst(src + '\*', 0, f) = 0 then
  begin
    repeat
      if not CopyFile(PChar(src + '\' + f.Name), PChar(dst + '\' + f.Name), False) then
      begin
        LogFailure('CopyFile from '+src + '\' + f.Name+' to '+dst + '\' + f.Name);
        Exit;
      end;
    until FindNext(f) <> 0;
    FindClose(f);
    Result := True;
  end
  else
    LogFailure('Could not find any files to copy from '+src);
end;

function TImportKeymanKeyboardBase.CopyAndInstallFonts(dstinf: string): Boolean;   // I4324
var
  dest: string;
  i: Integer;
  inf: TKMPInfFile;
begin
  dest := ExtractFilePath(dstinf);
  { Install fonts }

  inf := TKMPInfFile.Create;
  try
    inf.FileName := dstinf;
    inf.LoadIni;

    with TIniFile.Create(dest + 'fonts.inf') do
    try
      for i := 0 to inf.Files.Count - 1 do
        if inf.Files[i].FileType = ftFont then
          with TKPInstallFont.Create do
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
  finally
    inf.Free;
  end;

  PostMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);

  Result := True;
end;

function TImportKeymanKeyboardBase.MoveStartMenu(packagename, srcinf, dstinf: string): Boolean;   // I4324
var
  inf: TKMPInfFile;
  programs: string;
begin
  { Remove Start Menu items, either as admin or current user }

  inf := TKMPInfFile.Create;
  try
    inf.FileName := srcinf;
    inf.LoadIni;

    with TIniFile.Create(srcinf) do
    try
      programs := IncludeTrailingPathDelimiter(ReadString('Uninstall','StartMenuPath',''));
    finally
      Free;
    end;

    with TKPUninstallPackageStartMenu.Create(inf, ExtractFilePath(srcinf)) do
    try
      Execute(FAdmin, programs);
    finally
      Free;
    end;
  finally
    inf.Free;
  end;

  { Add start menu items, as admin }

  inf := TKMPInfFile.Create;
  try
    inf.FileName := dstinf;
    inf.LoadIni;

    with TKPInstallPackageStartMenu.Create(inf, packagename, ExtractFilePath(dstinf)) do
    try
      Execute(True, '');
    finally
      Free;
    end;
  finally
    inf.Free;
  end;

  Result := True;
end;


function TImportKeymanKeyboardBase.CopyPackage(const packagename: string): Boolean;
var
  pathWrite, desc, inf: string;
begin
  Result := False;

  Log('Copying registry from '+rootRead+'\'+packagename+' to '+rootWrite+'\'+packagename);

  if regWrite.KeyExists(rootWrite+'\'+packagename) then
  begin
    Log('Package registration already exists in '+rootWrite+'\'+packagename);
    Exit;
  end;

  if regRead.OpenKeyReadOnly(rootRead+'\'+packagename) then
  begin
    if regWrite.OpenKey(rootWrite+'\'+packagename, True) then
    begin
      if regRead.ValueExists(SRegValue_PackageDescription) and
        regRead.ValueExists(SRegValue_PackageFile) then
      begin
        desc := regRead.ReadString(SRegValue_PackageDescription);
        inf := regRead.ReadString(SRegValue_PackageFile);

        pathWrite := TImportOlderKeyboardUtils.GetPackageInstallPath;   // I4185

        { Copy files from old package folder to new package folder }
        if not CopyFiles(ExtractFileDir(inf), pathWrite + packagename) then Exit;

        { Don't allow old font references to be removed }
        if FileExists(ExtractFilePath(inf) + 'fonts.inf') then
          DeleteFile(ExtractFilePath(inf) + 'fonts.inf');

        { Create new registry entries }
        regWrite.WriteString(SRegValue_PackageDescription, desc);
        regWrite.WriteString(SRegValue_PackageFile, pathWrite + packagename + '\kmp.inf');

        if not MoveStartMenu(packagename, inf, pathWrite + packagename + '\kmp.inf') then   // I4324
        begin
          Log('Warning: Start Menu entries failed to transfer');
        end;

        if not FAdmin then   // I4324
          if not CopyAndInstallFonts(inf) then
          begin
            Log('Warning: Fonts failed to copy and install');
          end;

        Result := True;
      end
      else
        LogFailure('Registry values did not exist');
    end
    else
      LogFailure('Could not create registry keys');
  end
  else
    LogFailure('Could not open registry keys');
end;

procedure TImportKeymanKeyboardBase.CopyValue(const name: string);
begin
  if regRead.ValueExists(name) then
    regWrite.WriteString(name, regRead.ReadString(name));
end;

function TImportKeymanKeyboardBase.CopyKeyboard(const keyboardname: string): Boolean;
var
  pathname, filename: string;
  package: string;
begin
  Result := False;

  Log('Copying registry from '+rootRead+'\'+keyboardname+' to '+rootWrite+'\'+keyboardname);

  if regWrite.KeyExists(rootWrite+'\'+keyboardname) then
  begin
    Log('Keyboard registration already exists in '+rootWrite+'\'+keyboardname);
    Exit;
  end;

  if regRead.OpenKeyReadOnly(rootRead+'\'+keyboardname) then
  begin
    filename := regRead.ReadString(SRegValue_KeymanFile);
    if not FileExists(filename) then
    begin
      LogFailure('File '+filename+' does not exist');
      Exit;
    end;
    if regWrite.OpenKey(rootWrite+'\'+keyboardname, True) then
    begin
      if regRead.ValueExists(SRegValue_PackageName) then
      begin
        package := regRead.ReadString(SRegValue_PackageName);
        pathname := TImportOlderKeyboardUtils.GetPackageInstallPath + package + '\';   // I4185
      end
      else
      begin
        pathname := TImportOlderKeyboardUtils.GetKeyboardInstallPath + keyboardname;   // I4185   // I4623
        if not CopyFiles(ExtractFileDir(filename), pathname) then Exit;
        pathname := pathname + '\'
      end;

      regWrite.WriteString(SRegValue_KeymanFile, pathname + ExtractFileName(filename));

      CopyValue(SRegValue_Legacy_DefaultLanguageID);   // I4220

      CopyValue(SRegValue_PackageName);

      if regRead.ValueExists(SRegValue_VisualKeyboard) then
      begin
        filename := regRead.ReadString(SRegValue_VisualKeyboard);
        regWrite.WriteString(SRegValue_VisualKeyboard, pathname + ExtractFileName(filename));
      end;

      Result := True;
    end
    else
      LogFailure('Could not create registry keys');
  end
  else
    LogFailure('Could not open registry keys');
end;

procedure TImportKeymanKeyboardBase.CopyKeyboardActive(const keyboardname: string);
var
  v: Integer;
  FProfiles: TStringList;
  s: string;
begin
  if regRead.OpenKeyReadOnly(rootRead + '\' + keyboardname) and
    not regWrite.KeyExists(rootWrite + '\' + keyboardname) and
    regWrite.OpenKey(rootWrite + '\' + keyboardname, True) then
  begin
    // Copy KeymanID over
    if regRead.ValueExists(SRegValue_KeymanID) and TryStrToInt(values.Values[keyboardname], v) then
      regWrite.WriteString(SRegValue_KeymanID, values.Values[keyboardname]);

    // Find first associated language and associate hotkey with it
    if regRead.ValueExists(SRegValue_Legacy_KeymanActiveHotkey) then
    begin
      s := '';
      FProfiles := TStringList.Create;
      with TRegistryErrorControlled.Create do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKeyReadOnly(BuildKeyboardLanguageProfilesKey_LM(keyboardname)) then
        begin
          GetKeyNames(FProfiles);
          if FProfiles.Count > 0 then
          begin
            if OpenKeyReadOnly(FProfiles[0]) and ValueExists(SRegValue_KeymanProfileGUID) then
            begin
              s := ReadString(SRegValue_KeymanProfileGUID);
            end;
          end;
        end;
      finally
        Free;
        FProfiles.Free;
      end;

      if s <> '' then
        with TRegistryErrorControlled.Create do
        try
          if OpenKey(SRegKey_LanguageHotkeys_CU, True) then
            WriteString(s, regRead.ReadString(SRegValue_Legacy_KeymanActiveHotkey));
        finally
          Free;
        end;
    end;
  end;
end;

{ TImportKeymanKeyboardBase }

constructor TImportKeymanKeyboardBase.Create(AAdmin: Boolean);
var
  i: Integer;  // I2361
begin
  inherited Create;
  FAdmin := AAdmin;  // I2361
  for i := 0 to 5 do
  try
    FLogFile := TFileStream.Create(TKeymanPaths.ErrorLogPath('keymanimport'), fmCreate);  // I2792
    Break;
  except
    on E:EFCreateError do
    begin
      if i = 5 then raise;
      Sleep(500); // I2792 - wait for file to become available?
    end;
  end;
end;

destructor TImportKeymanKeyboardBase.Destroy;
begin
  FLogFile.Free;
  inherited Destroy;
end;

procedure TImportKeymanKeyboardBase.Log(const s: string);
var
  t: AnsiString;
begin
  t := UTF8Encode(s) + #13#10;   // I4617
  FLogFile.Write(PAnsiChar(t)^, Length(t));   // I4617
end;

procedure TImportKeymanKeyboardBase.LogFailure(const src: string);
begin
  Log('FAILURE: '+src+' ('+SysErrorMessage(GetLastError));
end;

end.
