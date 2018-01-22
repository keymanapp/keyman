(*
  Name:             UpgradeMnemonicLayout
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      31 Dec 2014

  Modified Date:    2 Jun 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          31 Dec 2014 - mcdurdin - I4553 - V9.0 - Upgrade to 476 or later requires recompile of all mnemonic layouts
                    06 Feb 2015 - mcdurdin - I4552 - V9.0 - Add mnemonic recompile option to ignore deadkeys
                    08 Apr 2015 - mcdurdin - I4651 - V9.0 - Mnemonic layout recompiler maps AltGr+VK_BKSLASH rather than VK_OEM_102
                    02 Jun 2015 - mcdurdin - I4657 - CrashID:kmshell.exe_9.0.492.0_2C46973C_ERegistryException
*)
unit UpgradeMnemonicLayout;   // I4553

interface

uses
  keymanapi_TLB;

type
  TUpgradeMnemonicLayout = class
    class procedure UpgradeLayout(Keyboard: IKeymanKeyboardInstalled);
    class procedure Run;
  private
    class procedure UpgradeLayoutLanguage(Keyboard: IKeymanKeyboardInstalled;
      const OutputFileName: string); static;
  end;

implementation

uses
  System.SysUtils,
  System.Win.Registry,
  Winapi.Windows,

  ErrorControlledRegistry,
  KeymanPaths,
  kmint,
  RegistryKeys,
  utilexecute,
  utilkmshell;

const
  { CurrentMnemonicLayoutVersion = 476;  // First 9.0 build with fixes for mnemonic layouts }
  { CurrentMnemonicLayoutVersion = 480;  // Adds support for deadkey conversion   // I4552 }
  CurrentMnemonicLayoutVersion = 492;  // Fixes bkslash order   // I4651

{ TUpgradeMnemonicLayout }

class procedure TUpgradeMnemonicLayout.Run;
var
  i: Integer;
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeymanEngine_LM) and ValueExists(SRegValue_MnemonicLayoutVersion) then
      if ReadInteger(SRegValue_MnemonicLayoutVersion) >= CurrentMnemonicLayoutVersion then
        Exit;
  finally
    Free;
  end;

  if not kmcom.SystemInfo.IsAdministrator then
  begin
    WaitForElevatedConfiguration(0, '-upgrademnemoniclayout');
    Exit;
  end;

  //
  // Iterate through all installed layouts and rewrite any
  // mnemonic layout files in the folders.  We need to rewrite
  // all of them, not just the ones used by the current user,
  // in case other users have different base layouts.
  //

  for i := 0 to kmcom.Keyboards.Count - 1  do
    UpgradeLayout(kmcom.Keyboards[i]);

  with TRegistryErrorControlled.Create do   // I4657
  try
    RootKey := HKEY_LOCAL_MACHINE;
    try
      OpenKey(SRegKey_KeymanEngine_LM, True);
      WriteInteger(SRegValue_MnemonicLayoutVersion, CurrentMnemonicLayoutVersion);
    except
      on E:ERegistryException do
        RaiseLastRegistryError(E.Message);
    end;
  finally
    Free;
  end;
end;

class procedure TUpgradeMnemonicLayout.UpgradeLayout(
  Keyboard: IKeymanKeyboardInstalled);
var
  FBaseName: string;
  f: TSearchRec;
begin
  FBaseName := ChangeFileExt(Keyboard.Filename, '');
  if FindFirst(FBaseName + '-????????.kmx', 0, f) = 0 then   // I4552
  begin
    repeat
      UpgradeLayoutLanguage(Keyboard, f.Name);
    until FindNext(f) <> 0;
    System.SysUtils.FindClose(f);
  end;
end;

function GetKeyboardLayoutFileName(id: Integer): string;
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeyboardLayouts_LM+'\'+IntToHex(id,8)) and ValueExists(SRegValue_KeyboardLayoutFile) then
    begin
      Exit(ReadString(SRegValue_KeyboardLayoutFile));
    end;
  finally
    Free;
  end;

  Result := '';
end;

{ Extracted from utilkeyman }   // I4552

function GetShortKeyboardName(const FileName: string): string;
begin
  if (LowerCase(ExtractFileExt(FileName)) = '.kmx') or
      (LowerCase(ExtractFileExt(FileName)) = '.kxx') or
      (LowerCase(ExtractFileExt(FileName)) = '.kmp')
    then Result := ChangeFileExt(ExtractFileName(FileName), '')
    else Result := FileName;
end;

function GetRegistryKeyboardInstallKey_LM(const FileName: string): string;
begin
  Result := SRegKey_InstalledKeyboards_LM+'\'+GetShortKeyboardName(FileName);
end;


class procedure TUpgradeMnemonicLayout.UpgradeLayoutLanguage(
  Keyboard: IKeymanKeyboardInstalled; const OutputFileName: string);
var
  FMCompilePath: string;
  BaseKeyboardID: Integer;
  FBaseKeyboardIDHex: string;
  FBaseFileName: string;
  FDestFileName: string;
  FBaseKeyboardFileName: string;
  FDestPath: string;
  FLogText: string;
  FExitCode: Integer;
  FDestDeadkeyFileName: string;
begin
  // OutputFileName format is C:\ProgramData\Keyman Engine <ver>\...\package\keyboard-########.kmx

  if not TryStrToInt('$'+Copy(OutputFileName, Length(OutputFileName)-11, 8), BaseKeyboardID) then
    Exit;

  FBaseKeyboardIDHex := IntToHex(BaseKeyboardID, 8);
  FBaseFileName := Keyboard.Filename;
  FDestFileName := OutputFileName;
  FDestDeadkeyFileName := ChangeFileExt(FDestFileName, '') + '-d.kmx';   // I4552
  FMCompilePath := TKeymanPaths.KeymanEngineInstallPath(TKeymanPaths.S_MCompileExe);
  FDestPath := ExtractFileDir(Keyboard.Filename);

  FBaseKeyboardFileName := GetKeyboardLayoutFileName(BaseKeyboardID);
  if FBaseKeyboardFileName = '' then
    Exit;

  // Run mcompile to rebuild the mnemonic layout
  TUtilExecute.Console(Format('"%s" "%s" "%s" %s "%s"', [
      FMCompilePath, FBaseFileName, FBaseKeyboardFileName, FBaseKeyboardIDHex, FDestFileName]),
      FDestPath, FLogText, FExitCode);

  // Run mcompile to rebuild the mnemonic layout with deadkey conversion   // I4552
  TUtilExecute.Console(Format('"%s" -d "%s" "%s" %s "%s"', [
      FMCompilePath, FBaseFileName, FBaseKeyboardFileName, FBaseKeyboardIDHex, FDestDeadkeyFileName]),
      FDestPath, FLogText, FExitCode);

  with TRegistry.Create do   // I4552
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKey('\'+GetRegistryKeyboardInstallKey_LM(Keyboard.Filename), True) then
    begin
      WriteString(SRegValue_KeymanFile_MnemonicOverride, IncludeTrailingPathDelimiter(FDestPath)+FDestFileName);
      WriteString(SRegValue_KeymanFile_MnemonicOverride_Deadkey, IncludeTrailingPathDelimiter(FDestPath)+FDestDeadkeyFileName);
    end;
  finally
    Free;
  end;
end;

end.
