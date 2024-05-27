program kmdbrowserhost;

uses
  System.SysUtils,
  Winapi.Windows,
  KeymanDeveloperOptions in '..\tike\main\KeymanDeveloperOptions.pas',
  Keyman.Developer.System.KeymanDeveloperPaths in '..\tike\main\Keyman.Developer.System.KeymanDeveloperPaths.pas',
  JsonUtil in '..\..\..\common\windows\delphi\general\JsonUtil.pas',
  Keyman.System.CEFManager in '..\..\..\common\windows\delphi\chromium\Keyman.System.CEFManager.pas',
  RegistryKeys in '..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  KeymanPaths in '..\..\..\common\windows\delphi\general\KeymanPaths.pas',
  KeymanVersion in '..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  DebugPaths in '..\..\..\common\windows\delphi\general\DebugPaths.pas',
  ErrorControlledRegistry in '..\..\..\common\windows\delphi\vcl\ErrorControlledRegistry.pas',
  VersionInfo in '..\..\..\common\windows\delphi\general\VersionInfo.pas',
  utilsystem in '..\..\..\common\windows\delphi\general\utilsystem.pas',
  utilexecute in '..\..\..\common\windows\delphi\general\utilexecute.pas',
  Unicode in '..\..\..\common\windows\delphi\general\Unicode.pas',
  GetOsVersion in '..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  klog in '..\..\..\common\windows\delphi\general\klog.pas',
  utildir in '..\..\..\common\windows\delphi\general\utildir.pas',
  Sentry.Client in '..\..\..\common\windows\delphi\ext\sentry\Sentry.Client.pas',
  sentry in '..\..\..\common\windows\delphi\ext\sentry\sentry.pas',
  Keyman.System.KeymanSentryClient in '..\..\..\common\windows\delphi\general\Keyman.System.KeymanSentryClient.pas';

{R icons.res}
{$R version.res}
{$R manifest.res}

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
// If you don't add this flag the rederer process will crash when you try to load large images.
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

const
  LOGGER_DEVELOPER_KMDBROWSERHOST = TKeymanSentryClient.LOGGER_DEVELOPER_IDE + '.kmdbrowserhost';
begin
  TKeymanSentryClient.Start(TSentryClient, kscpDeveloper,
    LOGGER_DEVELOPER_KMDBROWSERHOST, LoadKeymanDeveloperSentryFlags - [kscfShowUI]); // no ui wanted
  try
    try
      TKeymanSentryClient.Validate;
      FInitializeCEF := TCEFManager.Create(True);
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
