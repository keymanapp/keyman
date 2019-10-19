(*
  Name:             UImportOlderVersionKeyboards7
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
unit UImportOlderVersionKeyboards8;

interface

procedure ImportOlderVersionKeyboards8(DoAdmin: Boolean);   // I2361

implementation

uses
  Vcl.Graphics,
  System.Win.ComObj,
  Winapi.ActiveX,
  Winapi.MsCTF,

  input_installlayoutortip,
  Keyman.System.UpgradeRegistryKeys,
  keyman_msctf,
  UImportOlderVersionKeyboards,
  utilexecute,
  utilolepicture,
  GetOsVersion,
  Glossary,
  KeymanPaths,
  KeymanVersion,
  kmint,
  Windows,
  Classes,
  ErrorControlledRegistry,
  kmxfile,
  RegistryKeys,
  StrUtils,
  SysUtils,
  utildir,
  utilsystem,
  utiltsf,
  UImportOlderKeyboardUtils;

type
  TImportKeyman8Keyboard = class(TImportKeymanKeyboardBase)
  private
    pInputProcessorProfiles: ITfInputProcessorProfiles;
    pInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
    procedure InstallLanguageProfile(const AKeyboardLangID,
      AKeyboardName: string);
    procedure UnregisterKMTIPFromVersion7And8;   // I4298
  public
    procedure Import;
  end;

procedure ImportOlderVersionKeyboards8(DoAdmin: Boolean);  // I2361
begin
  with TImportKeyman8Keyboard.Create(DoAdmin, True) do  // I2361
  try
    Import;
  finally
    Free;
  end;
end;

procedure TImportKeyman8Keyboard.UnregisterKMTIPFromVersion7And8;   // I4298
const
  c_clsidKMTipTextService_7080: TGUID = (D1:$7ba04432; D2:$8609; D3: $4fe6; D4: ($bf,$f7, $97,$10,$91,$de,$09,$33));
begin
  try   // I4494
    UnregisterTIPAndItsProfiles(c_clsidKMTipTextService_7080);   // I4302
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

procedure TImportKeyman8Keyboard.Import;
var
  keysWrite, keys: TStringList;
  i: Integer;
  n: Integer;
  s: string;

  KeyboardName: string;
  DefaultLangID: string;
  FLayoutUninstallString: string;
  fhkl: Integer;
begin
  UnregisterKMTIPFromVersion7And8;   // I4298   // I4302

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
      rootRead := SRegKey_UpgradeBackupPath_LM + SRegKey_KeymanEngine80_InstalledPackages_CU;  // I2642
    end
    else
    begin
      regRead.RootKey := HKEY_CURRENT_USER;
      rootRead := SRegKey_UpgradeBackupPath_CU + SRegKey_KeymanEngine80_InstalledPackages_CU;  // I2642
    end;

    { Copy Installed Keyboards }


    regWrite.RootKey := HKEY_LOCAL_MACHINE;
    rootWrite := '\'+SRegKey_InstalledPackages_LM; // '\Software\...\Keyman Engine\x.x\Installed Packages';

    if regRead.OpenKeyReadOnly(rootRead) then
    begin
      keys.Clear;
      regRead.GetKeyNames(keys);
      for i := 0 to keys.Count - 1 do
      begin
        if FAdmin
          then Log('Copying package '+keys[i]+' (LM)')
          else Log('Copying package '+keys[i]+' (CU)');
        CopyPackage(keys[i]);
      end;
    end;

    if FAdmin
      then rootRead := SRegKey_UpgradeBackupPath_LM + SRegKey_KeymanEngine80_InstalledKeyboards_CU  // I2642
      else rootRead := SRegKey_UpgradeBackupPath_CU + SRegKey_KeymanEngine80_InstalledKeyboards_CU;  // I2642
    rootWrite := '\'+SRegKey_InstalledKeyboards_LM;  // '\Software\...\Keyman Engine\x.x\Installed Keyboards';

    if regRead.OpenKeyReadOnly(rootRead) then
    begin
      keys.Clear;
      regRead.GetKeyNames(keys);
      for i := 0 to keys.Count - 1 do
        CopyKeyboard(keys[i]);
    end;

    { Copy Active Keyboards }
    if not FAdmin then  // I2361
    begin
      regWrite.RootKey := HKEY_CURRENT_USER;   // I4297
      rootRead := SRegKey_UpgradeBackupPath_CU + SRegKey_KeymanEngine80_ActiveKeyboards_CU;  // I2642
      rootWrite := '\'+SRegKey_ActiveKeyboards_CU; //'\Software\...\Keyman Engine\x.x\Active Keyboards';

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

      { Copy language associations -- registration of language profiles }

      rootRead := SRegKey_UpgradeBackupPath_CU + SRegKey_KeymanEngine80_ActiveLanguages_CU;

      //
      //  1. For each language entry in the Active Languages list, register the keyboard profile
      //  2. Go through list of Active Keyboards and add any missing keyboard profiles to default language
      //

      if regRead.OpenKeyReadOnly(rootRead) then
      begin
        keys.Clear;
        regRead.GetValueNames(keysWrite);

        { Read active languages }
        for i := 0 to keysWrite.Count - 1 do
        begin
          KeyboardName := regRead.ReadString(keysWrite[i]);
          InstallLanguageProfile(keysWrite[i], KeyboardName);

          if TryStrToInt('$'+keysWrite[i], fhkl) then   // I4299
          begin
            FLayoutUninstallString := Format('%04.4x:%08.8x', [HKLToLanguageID(fhkl), HKLToKeyboardID(fhkl)]);
            try   // I4494
              if not InstallLayoutOrTip(PChar(FLayoutUninstallString), ILOT_UNINSTALL) then   // I4302
                LogFailure('Uninstalling keyboard profile');
            except
              on E:EOleException do
              begin
                LogFailure(Format('Failed to call InstallLayoutOrTip(%s): %s', [FLayoutUninstallString, 'EOleException: '+E.Message+' ('+E.Source+', '+IntToHex(E.ErrorCode,8)+')']));
              end;
              on E:EOleSysError do
              begin
                LogFailure(Format('Failed to call InstallLayoutOrTip(%s): %s', [FLayoutUninstallString, 'EOleSysError: '+E.Message+' ('+IntToHex(E.ErrorCode,8)+')']));
              end;
              on E:Exception do
              begin
                LogFailure(Format('Failed to call InstallLayoutOrTip(%s): %s', [FLayoutUninstallString, E.Message]));
              end;
            end;
            UnloadKeyboardLayout(fhkl);
          end;

          keys.Add(KeyboardName);
        end;

        DefaultLangID := IntToHex(HKLToLanguageID(GetDefaultHKL), 8);

        { Read missing languages }
        for i := 0 to values.Count - 1 do
        begin
          if keys.IndexOf(values.Names[i]) < 0 then
          begin
            InstallLanguageProfile(DefaultLangID, values.Names[i]);
          end;
        end;
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

procedure TImportKeyman8Keyboard.InstallLanguageProfile(const AKeyboardLangID, AKeyboardName: string);
var
  ki: TKeyboardInfo;
  guid: TGUID;
  FLayoutInstallString: string;
  FLangID: Word;
  FLocale: string;
  FKeyboardLangID: Integer;
  FIconFileName: string;
  FFileName: string;
  FIcon: TIcon;
  FLogText: string;
  FExitCode: Integer;
  FMCompilePath: string;
  localRegWrite: TRegistryErrorControlled;   // I4296
begin
  if not TryStrToInt('$' + AKeyboardLangID, FKeyboardLangID) then
  begin
    LogFailure('Invalid active language '+AKeyboardLangID);
    Exit;
  end;

  FLangID := LOWORD(FKeyboardLangID);
  if not ConvertLangIDToLocale(FLangID, FLocale) then
  begin
    LogFailure('Unable to find profile name for '+AKeyboardLangID);
    Exit;
  end;

  rootWrite := TImportOlderKeyboardUtils.GetRegistryKeyboardInstallKey_LM(AKeyboardName);

  with TRegistryErrorControlled.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if not OpenKeyReadOnly(rootWrite) or not ValueExists(SRegValue_KeymanFile) then
    begin
      LogFailure('Unable to retrieve keyboard details for profile install');
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
      localRegWrite.OpenKey('\' + rootWrite + '\' + SRegSubKey_LanguageProfiles + '\' + FLocale, True);   // I4296
      localRegWrite.WriteInteger(SRegValue_LanguageProfileLangID, FLangID);
      localRegWrite.WriteString(SRegValue_LanguageProfileLocale, FLocale);

      if not localRegWrite.ValueExists(SRegValue_KeymanProfileGUID) then
      begin
        CreateGuid(&guid);
        localRegWrite.WriteString(SRegValue_KeymanProfileGUID, GuidToString(guid));
      end;

      OleCheck(pInputProcessorProfileMgr.RegisterProfile(   // I3743
        c_clsidKMTipTextService,
        FLangID,
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

      FLayoutInstallString := Format('%04.4x:%s%s', [FLangID, GuidToString(c_clsidKMTipTextService),   // I4244
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

