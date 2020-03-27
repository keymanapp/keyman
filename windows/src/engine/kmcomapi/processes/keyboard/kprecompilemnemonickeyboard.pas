(*
  Name:             kprecompilemnemonickeyboard
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      24 Apr 2014

  Modified Date:    13 Mar 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          24 Apr 2014 - mcdurdin - I4174 - V9 - mcompile logs should be stored in diag folder
                    06 Feb 2015 - mcdurdin - I4552 - V9.0 - Add mnemonic recompile option to ignore deadkeys
                    13 Mar 2015 - mcdurdin - I4615 - CrashID:kmshell.exe_9.0.481.0_2C6795CE_EOleException
*)
unit kprecompilemnemonickeyboard;

interface

uses
  kpbase,
  kpinstallkeyboard;

type
  TKPRecompileMnemonicKeyboard = class(TKPBase)
    procedure Execute(const FileName: string; const PackageName: string);
  end;

implementation

{ TKPRecompileMnemonicKeyboard }

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  System.Win.Registry,
  Winapi.Windows,

  errorcontrolledregistry,
  keymancontext,
  keymanerrorcodes,
  KeymanPaths,
  keymanapi_TLB,
  RegistryKeys,
  utilexecute,
  utilkeyman,
  utilsystem;

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

procedure TKPRecompileMnemonicKeyboard.Execute(const FileName,PackageName: string);
var
  FDestPath, FDestFileName: string;
  FBaseKeyboardIDHex: string;
  FBaseFileName: string;
  FLogText: string;
  FExitCode: Integer;
  FMCompilePath: string;
  FBaseKeyboardFileName: string;
  BaseKeyboardID: Cardinal;
  FDestDeadkeyFileName: string;
  FCommand: string;
begin
  if PackageName <> ''
    then FDestPath := GetPackageInstallPath(PackageName)   // I3581
    else FDestPath := GetKeyboardInstallPath(FileName);   // I3581

  with Context as TKeymanContext do
    BaseKeyboardID := (Options as IKeymanOptions).Items['koBaseLayout'].Value;

  FBaseKeyboardIDHex := IntToHex(BaseKeyboardID, 8);
  FBaseFileName := FDestPath + '\' + ExtractFileName(FileName);   // I3581
  FDestFileName := ChangeFileExt(FBaseFileName, '') + '-'+FBaseKeyboardIDHex + '.kmx';
  FDestDeadkeyFileName := ChangeFileExt(FBaseFileName, '') + '-'+FBaseKeyboardIDHex + '-d.kmx';   // I4552
  FMCompilePath := TKeymanPaths.KeymanEngineInstallPath(TKeymanPaths.S_MCompileExe);

  { Recompile with the traditional deadkey behaviour }

  FBaseKeyboardFileName := GetKeyboardLayoutFileName(BaseKeyboardID);
  if FBaseKeyboardFileName = '' then
    ErrorFmt(KMN_E_RecompileMnemonicLayout_mcompileUnexpected, VarArrayOf([FDestFileName]));

  FCommand := Format('"%s" "%s" "%s" %s "%s"', [FMCompilePath, FBaseFileName, FBaseKeyboardFileName, FBaseKeyboardIDHex, FDestFileName]);   // I4615
  if not TUtilExecute.Console(FCommand, FDestPath, FLogText, FExitCode) then
    ErrorFmt(KMN_E_RecompileMnemonicLayout_mcompileFailed, VarArrayOf([GetLastError, SysErrorMessage(GetLastError), FCommand+'|'+FDestPath]));

  with TStringStream.Create(FLogText, TEncoding.UTF8) do   // I4174
  try
    SaveToFile(TKeymanPaths.ErrorLogPath('mcompile-'+ChangeFileExt(ExtractFileName(FileName),'')));
  finally
    Free;
  end;

  if FExitCode <> 0 then
    ErrorFmt(KMN_E_RecompileMnemonicLayout_mcompileError, VarArrayOf([FExitCode, FLogText, FDestFileName]));

  if not FileExists(FDestFileName) then
    ErrorFmt(KMN_E_RecompileMnemonicLayout_mcompileUnexpected, VarArrayOf([FDestFileName]));

  { Recompile with deadkey conversion, treating base layout deadkeys as normal keys }   // I4552

  FCommand := Format('"%s" -d "%s" "%s" %s "%s"', [FMCompilePath, FBaseFileName, FBaseKeyboardFileName, FBaseKeyboardIDHex, FDestDeadkeyFileName]);   // I4615
  if not TUtilExecute.Console(FCommand, FDestPath, FLogText, FExitCode) then
    ErrorFmt(KMN_E_RecompileMnemonicLayout_mcompileFailed, VarArrayOf([GetLastError, SysErrorMessage(GetLastError), FCommand+'|'+FDestPath]));

  with TStringStream.Create(FLogText, TEncoding.UTF8) do   // I4174
  try
    SaveToFile(TKeymanPaths.ErrorLogPath('mcompile-d-'+ChangeFileExt(ExtractFileName(FileName),'')));
  finally
    Free;
  end;

  if FExitCode <> 0 then
    ErrorFmt(KMN_E_RecompileMnemonicLayout_mcompileError, VarArrayOf([FExitCode, FLogText, FDestDeadkeyFileName]));

  if not FileExists(FDestDeadkeyFileName) then
    ErrorFmt(KMN_E_RecompileMnemonicLayout_mcompileUnexpected, VarArrayOf([FDestDeadkeyFileName]));

  { Create registry entries }

  with TRegistryErrorControlled.Create do  // I2890
  try
    { Write Installed Keyboards entry }

    RootKey := HKEY_LOCAL_MACHINE;  // Only supporting HKLM for KM9 and later
    if not OpenKey('\'+GetRegistryKeyboardInstallKey_LM(FileName), True) then  // I2890
      RaiseLastRegistryError;

    WriteString(SRegValue_KeymanFile_MnemonicOverride, FDestFileName);
    WriteString(SRegValue_KeymanFile_MnemonicOverride_Deadkey, FDestDeadkeyFileName);   // I4552
  finally
    Free;
  end;

  Context.Control.AutoApplyKeyman;
end;

end.
