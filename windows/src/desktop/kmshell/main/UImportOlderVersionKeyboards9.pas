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
  Vcl.Graphics,
  System.Win.ComObj,
  Winapi.ActiveX,
  Winapi.MsCTF,
  input_installlayoutortip,
  keyman_msctf,
  UImportOlderVersionKeyboards,
  utilexecute,
  utilolepicture,
  GetOsVersion,
  Glossary,
  KeymanVersion,
  kmint,
  Windows,
  Classes,
  ErrorControlledRegistry,
  kmxfile,
  KeymanPaths,
  RegistryKeys,
  StrUtils,
  SysUtils,
  utildir,
  utilsystem,
  utiltsf,
  UImportOlderKeyboardUtils;

type
  TImportKeyman9Keyboard = class(TImportKeymanKeyboardBase)
  private
    pInputProcessorProfiles: ITfInputProcessorProfiles;
    pInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
    function InstallLanguageProfile(ALangID: Integer; const ALocale, AKeyboardName: string): TGUID;
    procedure InstallLanguageProfiles(const KeyboardName: string);
    procedure UnregisterKMTIPFromVersion9;
  public
    procedure Import;
  end;

procedure ImportOlderVersionKeyboards9(DoAdmin: Boolean);  // I2361
begin
  with TImportKeyman9Keyboard.Create(DoAdmin) do  // I2361
  try
    Import;
    UnregisterKMTIPFromVersion9;
  finally
    Free;
  end;
end;

function ConvertLangIDToLocale(LangID: Integer; var Locale: string): Boolean;
var
  buf: array[0..LOCALE_NAME_MAX_LENGTH-1] of char;
begin
  Result := LCIDToLocaleName(LangID, buf, LOCALE_NAME_MAX_LENGTH, 0) > 0;
  if Result
    then Locale := buf
    else Locale := '';
end;

function GetDefaultHKL: HKL;   // I3581   // I3619   // I3619
begin
  if not SystemParametersInfo(SPI_GETDEFAULTINPUTLANG, 0, @Result, 0) then
    Result := 0;
end;

procedure TImportKeyman9Keyboard.Import;
var
  keysWrite, keys: TStringList;
  i: Integer;
  n: Integer;
  s: string;
begin
  OleCheck(CoCreateInstance(CLASS_TF_InputProcessorProfiles, nil, CLSCTX_INPROC_SERVER,
                          IID_ITfInputProcessorProfiles, pInputProcessorProfiles));

  if not Supports(pInputProcessorProfiles, IID_ITfInputProcessorProfileMgr, pInputProcessorProfileMgr) then   // I3743
  begin
    LogFailure('Missing interface IID_ITfInputProcessorProfileMgr');
    Exit;
  end;

  values := TStringList.Create;
  keys := TStringList.Create;
  keysWrite := TStringList.Create;
  regRead := TRegistryErrorControlled.Create;  // I2890
  regWrite := TRegistryErrorControlled.Create;  // I2890
  try
    if FAdmin then  // I2361
    begin
      regRead.RootKey := HKEY_LOCAL_MACHINE;
      regWrite.RootKey := HKEY_LOCAL_MACHINE;
    end
    else
    begin
      regRead.RootKey := HKEY_CURRENT_USER;
      regWrite.RootKey := HKEY_LOCAL_MACHINE;
    end;

    { Copy Installed Keyboards }

    rootRead := SRegKey_UpgradeBackupPath + SRegKey_KeymanEngine90_InstalledPackages;  // I2642
    rootWrite := '\'+SRegKey_InstalledPackages; // '\Software\...\Keyman Engine\x.x\Installed Packages';

    if FAdmin then
    begin
      if regRead.OpenKeyReadOnly(rootRead) then
      begin
        keys.Clear;
        regRead.GetKeyNames(keys);
        for i := 0 to keys.Count - 1 do
        begin
          Log('Copying package '+keys[i]+' (LM)');
          CopyPackage(keys[i]);
        end;
      end;

      rootRead := SRegKey_UpgradeBackupPath + SRegKey_KeymanEngine90_InstalledKeyboards;  // I2642
      rootWrite := '\'+SRegKey_InstalledKeyboards;  // '\Software\...\Keyman Engine\x.x\Installed Keyboards';

      if regRead.OpenKeyReadOnly(rootRead) then
      begin
        keys.Clear;
        regRead.GetKeyNames(keys);
        for i := 0 to keys.Count - 1 do
        begin
          CopyKeyboard(keys[i]);
          InstallLanguageProfiles(keys[i]);
        end;
      end;
    end;

    { Copy Active Keyboards }
    if not FAdmin then  // I2361
    begin
      regWrite.RootKey := HKEY_CURRENT_USER;   // I4297
      rootRead := SRegKey_UpgradeBackupPath + SRegKey_KeymanEngine90_ActiveKeyboards;  // I2642
      rootWrite := '\'+SRegKey_ActiveKeyboards; //'\Software\...\Keyman Engine\x.x\Active Keyboards';

      if regRead.OpenKeyReadOnly(rootRead) and regWrite.OpenKey(rootWrite, True) then
      begin
        values.Clear;
        keys.Clear;
        keysWrite.Clear;
        regWrite.GetValueNames(keysWrite);
        regRead.GetValueNames(keys);
        n := 0;
        for i := 0 to keysWrite.Count - 1 do
          values.Add(regWrite.ReadString(keysWrite[i]));

        { Write active keyboards }
        for i := 0 to keys.Count - 1 do
        begin
          s := regRead.ReadString(keys[i]);
          if values.IndexOf(s) < 0 then
          begin
            while keysWrite.IndexOf(IntToStr(n)) >= 0 do Inc(n);  // Find next available KeymanID
            regWrite.WriteString(IntToStr(n), s);
            values.Add(s + '=' + IntToStr(n));
            Inc(n);
          end;
        end;

        keys.Clear;
        regRead.GetKeyNames(keys);
        for i := 0 to Keys.Count - 1 do
          CopyKeyboardActive(keys[i]);
      end;
    end;

  finally
    regRead.Free;
    regWrite.Free;
    keys.Free;
    keysWrite.Free;
    values.Free;
  end;
end;

procedure TImportKeyman9Keyboard.UnregisterKMTIPFromVersion9;   // I4298
begin
  try   // I4494
    UnregisterTIPAndItsProfiles(c_clsidKMTipTextService_90);   // I4302
  except
    on E:EOleException do
    begin
      LogFailure(Format('Failed to call UnregisterTIPAndItsProfiles: %s', ['EOleException: '+E.Message+' ('+E.Source+', '+IntToHex(E.ErrorCode,8)+')']));
    end;
    on E:EOleSysError do
    begin
      LogFailure(Format('Failed to call UnregisterTIPAndItsProfiles: %s', ['EOleSysError: '+E.Message+' ('+IntToHex(E.ErrorCode,8)+')']));
    end;
    on E:Exception do
    begin
      LogFailure(Format('Failed to call UnregisterTIPAndItsProfiles: %s', [E.Message]));
    end;
  end;
end;


procedure TImportKeyman9Keyboard.InstallLanguageProfiles(const KeyboardName: string);
var
  profilenames: TStringList;
  i: Integer;
  FLangID: Integer;
  FLocale: string;
  FGUID: TGUID;
  //FProfileGUID: string;
begin
    // Copy language profiles
  if regRead.OpenKeyReadOnly(rootRead+'\'+keyboardname+'\'+SRegKey_LanguageProfiles) then
  begin
    profilenames := TStringList.Create;
    try
      regRead.GetKeyNames(profilenames);
      for i := 0 to profilenames.Count - 1 do
      begin
        if regRead.OpenKeyReadOnly(rootRead+'\'+keyboardname+'\'+SRegKey_LanguageProfiles+'\'+profilenames[i]) then
        begin
          FLangID := regRead.ReadInteger(SRegValue_LanguageProfileLangID);
          FLocale := regRead.ReadString(SRegValue_LanguageProfileLocale);
          //FProfileGUID := regRead.ReadString(SRegValue_KeymanProfileGUID);
          FGUID := InstallLanguageProfile(FLangID, FLocale, KeyboardName);
        end;
      end;
    finally
      profilenames.Free;
    end;
  end;
end;

function TImportKeyman9Keyboard.InstallLanguageProfile(ALangID: Integer; const ALocale, AKeyboardName: string): TGUID;
var
  ki: TKeyboardInfo;
  guid: TGUID;
  FLayoutInstallString: string;
  FIconFileName: string;
  FFileName: string;
  FIcon: TIcon;
  FLogText: string;
  FExitCode: Integer;
  FMCompilePath: string;
  localRootWrite: string;
  localRegWrite: TRegistryErrorControlled;   // I4296
begin
  localRootWrite := TImportOlderKeyboardUtils.GetRegistryKeyboardInstallKey(AKeyboardName);

  with TRegistryErrorControlled.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if not OpenKeyReadOnly(localRootWrite) or not ValueExists(SRegValue_KeymanFile) then
    begin
      LogFailure('Unable to retrieve keyboard details for profile install at '+localRootWrite);
      Exit;
    end;
    FFileName := ReadString(SRegValue_KeymanFile);
  finally
    Free;
  end;

  try
    GetKeyboardInfo(FFileName, True, ki, True);   // I3581
  except
    on E:Exception do // I1626
    begin
      LogFailure(ExtractFileName(FFileName) + ', '+E.ClassName+':'+E.Message);
      Exit;
    end;
  end;

  try
    if not (keUnicode in ki.Encodings) then
    begin
      FMCompilePath := TKeymanPaths.KeymanEngineInstallPath(TKeymanPaths.S_MCompileExe);
      // Run mcompile to convert to Unicode, in place
      if not TUtilExecute.Console(Format('"%s" -u "%s" "%s"', [FMCompilePath, FFileName, FFileName]),
          ExtractFilePath(FFileName), FLogText, FExitCode) then
      begin
        LogFailure('Could not recompile keyboard for Unicode: '+FLogText);
        Exit;
      end;
      Log(FLogText);
    end;

    { Extract icon from keyboard and save to the folder }

    FIconFileName := TImportOlderKeyboardUtils.GetKeyboardIconFileName(FFileName);   // I3581   // I3599

    if Assigned(ki.Bitmap) then
    begin
      FIcon := TIcon.Create;
      try
        LoadIconFromBitmap(FIcon, ki.Bitmap);
        FIcon.SaveToFile(FIconFileName);
      finally
        FIcon.Free;
      end;
      FreeAndNil(ki.Bitmap);
    end
    else if Assigned(ki.Icon) then
    begin
      ki.Icon.SaveToFile(FIconFileName);
      FreeAndNil(ki.Icon);
    end
    else
      FIconFileName := '';

    localRegWrite := TRegistryErrorControlled.Create;  // I2890   // I4296
    try
      localRegWrite.RootKey := HKEY_LOCAL_MACHINE;   // I4297
      localRegWrite.OpenKey('\' + localRootWrite + '\' + SRegKey_LanguageProfiles + '\' + ALocale, True);   // I4296
      localRegWrite.WriteInteger(SRegValue_LanguageProfileLangID, ALangID);
      localRegWrite.WriteString(SRegValue_LanguageProfileLocale, ALocale);

      if not localRegWrite.ValueExists(SRegValue_KeymanProfileGUID) then
      begin
        CreateGuid(&guid);
        localRegWrite.WriteString(SRegValue_KeymanProfileGUID, GuidToString(guid));
      end
      else
      begin
        guid := StringToGUID(localRegWrite.ReadString(SRegValue_KeymanProfileGUID));
      end;

      Result := guid;

      OleCheck(pInputProcessorProfileMgr.RegisterProfile(   // I3743
        c_clsidKMTipTextService,
        ALangID,
        guid,
        PWideChar(ki.KeyboardName),
        Length(ki.KeyboardName),
        PWideChar(FIconFileName),
        Length(FIconFileName),
        0,
        0,
        0,
        1,
        0));

      FLayoutInstallString := Format('%04.4x:%s%s', [ALangID, GuidToString(c_clsidKMTipTextService),   // I4244
        GuidToString(guid)]);

      { Save TIP to registry }   // I4244

      try   // I4494
        if not InstallLayoutOrTip(PChar(FLayoutInstallString), 0) then
        begin
          LogFailure('Installing keyboard profile, installing layout or tip');
          Exit;
        end;
      except
        on E:EOleException do
        begin
          LogFailure(Format('Failed to call InstallLayoutOrTip(%s): %s', [FLayoutInstallString, 'EOleException: '+E.Message+' ('+E.Source+', '+IntToHex(E.ErrorCode,8)+')']));
        end;
        on E:EOleSysError do
        begin
          LogFailure(Format('Failed to call InstallLayoutOrTip(%s): %s', [FLayoutInstallString, 'EOleSysError: '+E.Message+' ('+IntToHex(E.ErrorCode,8)+')']));
        end;
        on E:Exception do
        begin
          LogFailure(Format('Failed to call InstallLayoutOrTip(%s): %s', [FLayoutInstallString, E.Message]));
        end;
      end;
    finally
      localRegWrite.Free;   // I4296
    end;
  finally
    ki.MemoryDump.Free;
  end;
end;

end.
