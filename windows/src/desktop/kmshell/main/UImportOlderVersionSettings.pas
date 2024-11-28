(*
  Name:             UImportOlderVersionSettings
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:
  Description:
  Create Date:      22 Feb 2011

  Modified Date:    3 Jun 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          22 Feb 2011 - mcdurdin - I2651 - Install does not set desired default options
                    22 Feb 2011 - mcdurdin - I2753 - Firstrun crashes because start with windows and auto update check options are set in Engine instead of Desktop
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    02 Dec 2012 - mcdurdin - I3629 - V9.0 - importing older versions options would fail due to removed koSwitchLanguageWithKeyboard option
                    03 Jun 2014 - mcdurdin - I4250 - V9.0 - Keyman Configuration crashes on first run due to koKeymanUniscribeManager reference
*)
unit UImportOlderVersionSettings;

interface

function FirstRunInstallDefaults(DoDefaults,DoStartWithWindows,DoCheckForUpdates,DoAutomaticUpdates: Boolean; FDisablePackages, FDefaultUILanguage: string; DoAutomaticallyReportUsage: Boolean): Boolean;  // I2753

implementation

uses
  System.SysUtils,
  System.Win.ComObj,

  Hints,
  InterfaceHotkeys,
  Keyman.Configuration.System.KeymanUILanguageManager,
  Keyman.System.UpgradeRegistryKeys,
  KeymanVersion,
  kmint,
  keymanapi_TLB,
  ErrorControlledRegistry,
  RegistryKeys,
  Keyman.System.UpdateStateMachine,
  UImportOlderKeyboardUtils;

function FirstRunInstallDefaults(DoDefaults,DoStartWithWindows,DoCheckForUpdates,DoAutomaticUpdates: Boolean; FDisablePackages, FDefaultUILanguage: string; DoAutomaticallyReportUsage: Boolean): Boolean;  // I2753
var
  n, I: Integer;
  v: Integer;
  p: string;
  UpdateSM : TUpdateStateMachine;
begin
  // send event to statemachine (should result in setting state to idle)
  UpdateSM := TUpdateStateMachine.Create(False);
  try
    UpdateSM.HandleFirstRun;
  finally
    UpdateSM.Free;
  end;

  { Copy over all the user settings and set defaults for version 8.0: http://blog.tavultesoft.com/2011/02/keyman-desktop-80-default-options.html }

  if DoDefaults then  // I2753
  begin
    TKeymanUILanguageManager.Execute;
    if FDefaultUILanguage <> '' then
    begin
       kmint.KeymanCustomisation.CustMessages.LanguageCode := FDefaultUILanguage;
    end;

    { General }
    kmcom.Options['koKeyboardHotkeysAreToggle'].Value := True;
    kmcom.Options['koAltGrCtrlAlt'].Value := False;
    kmcom.Options['koShowHints'].Value := True;
    kmcom.Options['koSwitchLanguageForAllApplications'].Value := True;

    { Startup }
    kmcom.Options['koShowStartup'].Value := True;
    kmcom.Options['koTestKeymanFunctioning'].Value := True;

    { OSK }
    kmcom.Options['koReleaseShiftKeysAfterKeyPress'].Value := False;
    kmcom.Options['koAutoOpenOSK'].Value := False;
    kmcom.Options['koAutoSwitchOSKPages'].Value := True;
    //// TODO: kmcom.Options['koShowOSKOnStartup'] := True;

    { Advanced }   // I3629
    kmcom.Options['koDebugging'].Value := False;
    // I4250
    { Hotkeys }
    kmcom.Hotkeys.Items[khKeymanOff].VirtualKey := Ord('O');
    kmcom.Hotkeys.Items[khKeymanOff].Modifiers := HK_ALT or HK_SHIFT;
    kmcom.Hotkeys.Items[khKeyboardMenu].VirtualKey := Ord('M');
    kmcom.Hotkeys.Items[khKeyboardMenu].Modifiers := HK_ALT or HK_SHIFT;
    kmcom.Hotkeys.Items[khVisualKeyboard].VirtualKey := Ord('K');
    kmcom.Hotkeys.Items[khVisualKeyboard].Modifiers := HK_ALT or HK_SHIFT;
    kmcom.Hotkeys.Items[khLanguageSwitch].VirtualKey := 0;
    kmcom.Hotkeys.Items[khLanguageSwitch].Modifiers := HK_ALT or HK_SHIFT;

    with TRegistryErrorControlled.Create do  // I2890
    try
      if OpenKeyReadOnly(SRegKey_UpgradeBackupPath_CU + SRegKey_KeymanEngine70_CU) then
      begin
        kmcom.Options['koKeyboardHotkeysAreToggle'].Value := ValueExists(SRegValue_KeyboardHotKeysAreToggle) and ReadBool(SRegValue_KeyboardHotKeysAreToggle);
        kmcom.Options['koAltGrCtrlAlt'].Value := ValueExists(SRegValue_AltGrCtrlAlt) and ReadBool(SRegValue_AltGrCtrlAlt);
        kmcom.Options['koShowHints'].Value := not ValueExists(SRegValue_EnableHints) or ReadBool(SRegValue_EnableHints);

        kmcom.Options['koTestKeymanFunctioning'].Value := not ValueExists(SRegValue_TestKeymanFunctioning) or ReadBool(SRegValue_TestKeymanFunctioning);

        kmcom.Options['koReleaseShiftKeysAfterKeyPress'].Value := ValueExists(SRegValue_ReleaseShiftKeysAfterKeyPress) and ReadBool(SRegValue_ReleaseShiftKeysAfterKeyPress);
        kmcom.Options['koAutoSwitchOSKPages'].Value := not ValueExists(SRegValue_AutoSwitchOSKPages) or ReadBool(SRegValue_AutoSwitchOSKPages);

        if OpenKeyReadOnly(SRegKey_UpgradeBackupPath_CU + SRegKey_KeymanEngine70_CU+'\hotkeys\1') or OpenKeyReadOnly(SRegKey_UpgradeBackupPath_CU + SRegKey_KeymanEngine70_CU+'\hotkeys\8') then
        begin
          for I := 1 to 5 do
            if ValueExists(IntToStr(i)) then
            begin
              kmcom.Hotkeys[I].RawValue := ReadInteger(IntToStr(i));
            end;
        end;
      end
      else if OpenKeyReadOnly(SRegKey_UpgradeBackupPath_CU + SRegKey_Keyman60_CU) then
      begin
        kmcom.Options['koKeyboardHotkeysAreToggle'].Value := ValueExists(SRegValue_KeyboardHotKeysAreToggle) and ReadBool(SRegValue_KeyboardHotKeysAreToggle);
        kmcom.Options['koAltGrCtrlAlt'].Value := ValueExists(SRegValue_AltGrCtrlAlt) and ReadBool(SRegValue_AltGrCtrlAlt);

        kmcom.Options['koTestKeymanFunctioning'].Value := not ValueExists(SRegValue_TestKeymanFunctioning) or ReadBool(SRegValue_TestKeymanFunctioning);

        if ValueExists('keyman hotkey') then v := StrToIntDef(ReadString('keyman hotkey'), 0) else v := 0;
        if v <> 0 then kmcom.Hotkeys[khKeymanOff].RawValue := v;

        if ValueExists('keyboard menu hotkey') then v := StrToIntDef(ReadString('keyboard menu hotkey'), 0) else v := 0;
        if v <> 0 then kmcom.Hotkeys[khKeyboardMenu].RawValue := v;

        if ValueExists('visual keyboard hotkey') then v := StrToIntDef(ReadString('visual keyboard hotkey'), 0) else v := 0;
        if v <> 0 then kmcom.Hotkeys[khVisualKeyboard].RawValue := v;
      end;
    finally
      Free;
    end;

    if kmcom.Options['koShowHints'].Value then
      ResetAllHints;
  end;

  if DoStartWithWindows then kmcom.Options['koStartWithWindows'].Value := True; // I2753
  if DoCheckForUpdates then kmcom.Options['koCheckForUpdates'].Value := True;  // I2753
  if DoAutomaticUpdates then kmcom.Options['koAutomaticUpdate'].Value := True;
  if DoAutomaticallyReportUsage then kmcom.Options['koAutomaticallyReportUsage'].Value := True;


  if DoDefaults then
  begin
    // I2753
    kmcom.Errors.Clear;
    kmcom.Hotkeys.Apply;  // I2651 - hotkeys not saving, 8.0.309.0
    kmcom.Options.Apply;
    if kmcom.Errors.Count > 0 then
    begin
      // Handle error setting values, most likely Start With Windows, which can
      // be blocked by zealous security software. Note: there is an error that will
      // be bubbled through in the Errors array but we are not going to worry about
      // that one here for now; we can't really add a lot of detail without significant
      // refactoring, and this is the only error we are really concerned about at
      // present.
      Exit(False);
    end;
  end;

  // Disable packages per command line setting
  // Initially used for the FirstVoices Keyboards app -- installs all keyboards
  // but makes them not enabled by default for the user.
  if FDisablePackages <> '' then
  begin
    while FDisablePackages <> '' do
    begin
      if FDisablePackages[1] = '"' then
      begin
        Delete(FDisablePackages, 1, 1);
        n := Pos('"', FDisablePackages);
        if n = 0 then
          Break;
        p := Copy(FDisablePackages, 1, n-1);
        Delete(FDisablePackages, 1, n);
        if Copy(FDisablePackages, 1, 1) = ',' then
          Delete(FDisablePackages, 1, 1);
      end
      else
      begin
        n := Pos(',', FDisablePackages);
        if n = 0 then n := Length(FDisablePackages) + 1;
        p := Copy(FDisablePackages, 1, n - 1);
        Delete(FDisablePackages, 1, n);
      end;

      for i := 0 to kmcom.Keyboards.Count - 1 do
        if (kmcom.Keyboards[i].OwnerPackage <> nil) and
            SameText(kmcom.Keyboards[i].OwnerPackage.ID, p) then
          kmcom.Keyboards[i].Loaded := False;
    end;
  end;

  kmcom.Apply;
  Result := True;
end;

end.
