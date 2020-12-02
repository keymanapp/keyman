program insthelp;

uses
  main in 'main.pas',
  Winapi.ActiveX,
//  System.Win.ComObj,
  klog in '..\..\global\delphi\general\klog.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  Keyman.System.InstHelp.KeymanStartTaskUninstall in 'Keyman.System.InstHelp.KeymanStartTaskUninstall.pas',
  TaskScheduler_TLB in '..\..\global\delphi\winapi\TaskScheduler_TLB.pas';

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

