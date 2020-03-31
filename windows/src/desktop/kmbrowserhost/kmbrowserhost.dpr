program kmbrowserhost;

uses
  Keyman.System.CEFManager in '..\..\global\delphi\chromium\Keyman.System.CEFManager.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  KeymanPaths in '..\..\global\delphi\general\KeymanPaths.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  DebugPaths in '..\..\global\delphi\general\DebugPaths.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  VersionInfo in '..\..\global\delphi\general\VersionInfo.pas',
  utilsystem in '..\..\global\delphi\general\utilsystem.pas',
  utilexecute in '..\..\global\delphi\general\utilexecute.pas',
  Unicode in '..\..\global\delphi\general\Unicode.pas',
  GetOsVersion in '..\..\global\delphi\general\GetOsVersion.pas',
  klog in '..\..\global\delphi\general\klog.pas',
  utildir in '..\..\global\delphi\general\utildir.pas',
  Sentry.Client in '..\..\ext\sentry\Sentry.Client.pas',
  Sentry.Client.Vcl in '..\..\ext\sentry\Sentry.Client.Vcl.pas',
  sentry in '..\..\ext\sentry\sentry.pas',
  Keyman.System.KeymanSentryClient in '..\..\global\delphi\general\Keyman.System.KeymanSentryClient.pas';

{R icons.res}
{$R version.res}
{$R manifest.res}

begin
  TKeymanSentryClient.Start(TSentryClientVcl, kscpDesktop, [kscfCaptureExceptions, kscfTerminate]); // no ui wanted
  try
    FInitializeCEF := TCEFManager.Create;
    try
      FInitializeCEF.StartSubProcess;
    finally
      FInitializeCEF.Free;
    end;
  finally
    TKeymanSentryClient.Stop;
  end;
end.
