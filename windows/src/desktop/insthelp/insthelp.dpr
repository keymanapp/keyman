program insthelp;

uses
  main in 'main.pas',
  ActiveX,
  comobj,
  klog in '..\..\global\delphi\general\klog.pas',
  utilfiletypes in '..\..\global\delphi\general\utilfiletypes.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  SystemDebugPath in '..\..\global\delphi\general\SystemDebugPath.pas',
  GetOsVersion in '..\..\global\delphi\general\GetOsVersion.pas',
  VersionInfo in '..\..\global\delphi\general\VersionInfo.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  utilexecute in '..\..\global\delphi\general\utilexecute.pas',
  Unicode in '..\..\global\delphi\general\Unicode.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  KeymanPaths in '..\..\global\delphi\general\KeymanPaths.pas',
  DebugPaths in '..\..\global\delphi\general\DebugPaths.pas',
  StockFileNames in '..\..\global\delphi\cust\StockFileNames.pas';

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

