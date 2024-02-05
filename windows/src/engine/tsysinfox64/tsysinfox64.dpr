program tsysinfox64;

uses
  System.SysUtils,
  fileversioninfo in 'fileversioninfo.pas',
  si_base in 'si_base.pas',
  si_processes in 'si_processes.pas',
  sysinfo_util in 'sysinfo_util.pas',
  main in 'main.pas',
  Sentry.Client in '..\..\..\..\common\windows\delphi\ext\sentry\Sentry.Client.pas',
  sentry in '..\..\..\..\common\windows\delphi\ext\sentry\sentry.pas',
  Keyman.System.KeymanSentryClient in '..\..\..\..\common\windows\delphi\general\Keyman.System.KeymanSentryClient.pas',
  KeymanPaths in '..\..\..\..\common\windows\delphi\general\KeymanPaths.pas',
  DebugPaths in '..\..\..\..\common\windows\delphi\general\DebugPaths.pas',
  ErrorControlledRegistry in '..\..\..\..\common\windows\delphi\vcl\ErrorControlledRegistry.pas',
  RegistryKeys in '..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  utilexecute in '..\..\..\..\common\windows\delphi\general\utilexecute.pas',
  Unicode in '..\..\..\..\common\windows\delphi\general\Unicode.pas';

{$R *.res}
{$R manifest.res}
{$R version.res}

const
  LOGGER_DESKTOP_ENGINE_TSYSINFOX64 = TKeymanSentryClient.LOGGER_DESKTOP_ENGINE + '.tsysinfox64';
begin
  TKeymanSentryClient.Start(TSentryClient, kscpDesktop, LOGGER_DESKTOP_ENGINE_TSYSINFOX64, LoadKeymanDesktopSentryFlags);
  try
    try
      TKeymanSentryClient.Validate;
      Run;
    except
      on E: Exception do
        SentryHandleException(E);
    end;
  finally
    TKeymanSentryClient.Stop;
  end;
end.
