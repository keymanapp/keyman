program insthelp;

uses
  main in 'main.pas',
  Winapi.ActiveX,
  klog in '..\..\..\..\common\windows\delphi\general\klog.pas',
  RegistryKeys in '..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  Keyman.System.InstHelp.KeymanStartTaskUninstall in 'Keyman.System.InstHelp.KeymanStartTaskUninstall.pas',
  TaskScheduler_TLB in '..\..\global\delphi\winapi\TaskScheduler_TLB.pas',
  UserMessages in '..\..\..\..\common\windows\delphi\general\UserMessages.pas';

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

