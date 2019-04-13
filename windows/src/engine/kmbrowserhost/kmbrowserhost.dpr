program kmbrowserhost;

uses
  Keyman.System.CEFManager in '..\..\global\delphi\chromium\Keyman.System.CEFManager.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  KeymanPaths in '..\..\global\delphi\general\KeymanPaths.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  DebugPaths in '..\..\global\delphi\general\DebugPaths.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  ExternalExceptionHandler in '..\..\global\delphi\general\ExternalExceptionHandler.pas',
  VersionInfo in '..\..\global\delphi\general\VersionInfo.pas',
  ErrLogPath in '..\..\global\delphi\general\ErrLogPath.pas',
  utilsystem in '..\..\global\delphi\general\utilsystem.pas',
  utilexecute in '..\..\global\delphi\general\utilexecute.pas',
  Unicode in '..\..\global\delphi\general\Unicode.pas',
  GetOsVersion in '..\..\global\delphi\general\GetOsVersion.pas',
  klog in '..\..\global\delphi\general\klog.pas',
  utildir in '..\..\global\delphi\general\utildir.pas';

{R icons.res}
{$R version.res}
{$R manifest.res}

begin
  FInitializeCEF := TCEFManager.Create;
  try
    FInitializeCEF.StartSubProcess;
  finally
    FInitializeCEF.Free;
  end;
end.
