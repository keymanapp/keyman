program kmbrowserhost;

uses
  System.SysUtils,
  Winapi.Windows,
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
  sentry in '..\..\ext\sentry\sentry.pas',
  Keyman.System.KeymanSentryClient in '..\..\global\delphi\general\Keyman.System.KeymanSentryClient.pas';

{R icons.res}
{$R version.res}
{$R manifest.res}

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
// If you don't add this flag the rederer process will crash when you try to load large images.
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

const
  LOGGER_DESKTOP_KMBROWSERHOST = TKeymanSentryClient.LOGGER_DESKTOP + '.kmbrowserhost';
begin
  TKeymanSentryClient.Start(TSentryClient, kscpDesktop,
    LOGGER_DESKTOP_KMBROWSERHOST, [kscfCaptureExceptions, kscfTerminate]); // no ui wanted
  try
    try
      TKeymanSentryClient.Validate;
      FInitializeCEF := TCEFManager.Create;
      try
        FInitializeCEF.StartSubProcess;
      finally
        FInitializeCEF.Free;
      end;
    except
      on E:Exception do
        SentryHandleException(E);
    end;
  finally
    TKeymanSentryClient.Stop;
  end;
end.
