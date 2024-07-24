program insthelp;

uses
  main in 'main.pas',
  Winapi.ActiveX,
  klog in '..\..\..\..\common\windows\delphi\general\klog.pas',
  RegistryKeys in '..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  Keyman.System.InstHelp.KeymanStartTaskUninstall in 'Keyman.System.InstHelp.KeymanStartTaskUninstall.pas',
  TaskScheduler_TLB in '..\..\global\delphi\winapi\TaskScheduler_TLB.pas',
  ErrorControlledRegistry in '..\..\..\..\common\windows\delphi\vcl\ErrorControlledRegistry.pas',
  UserMessages in '..\..\..\..\common\windows\delphi\general\UserMessages.pas',
  DebugPaths in '..\..\..\..\common\windows\delphi\general\DebugPaths.pas',
  VersionInfo in '..\..\..\..\common\windows\delphi\general\VersionInfo.pas',
  Unicode in '..\..\..\..\common\windows\delphi\general\Unicode.pas',
  KeymanPaths in '..\..\..\..\common\windows\delphi\general\KeymanPaths.pas';

{$R version.res}
{-R manifest.res}

begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  try
    Run;
  finally
    CoUninitialize;
  end;
end.

