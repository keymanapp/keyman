program tsysinfox64;

uses
  System.SysUtils,
  fileversioninfo in 'fileversioninfo.pas',
  si_base in 'si_base.pas',
  si_processes in 'si_processes.pas',
  sysinfo_util in 'sysinfo_util.pas',
  main in 'main.pas',
  Sentry.Client in '..\..\ext\sentry\Sentry.Client.pas',
  sentry in '..\..\ext\sentry\sentry.pas',
  Keyman.System.KeymanSentryClient in '..\..\global\delphi\general\Keyman.System.KeymanSentryClient.pas',
  KeymanPaths in '..\..\global\delphi\general\KeymanPaths.pas',
  DebugPaths in '..\..\global\delphi\general\DebugPaths.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  utilexecute in '..\..\global\delphi\general\utilexecute.pas',
  Unicode in '..\..\global\delphi\general\Unicode.pas';

{$R *.res}
{$R manifest.res}
{$R version.res}

const
  LOGGER_DESKTOP_ENGINE_TSYSINFOX64 = TKeymanSentryClient.LOGGER_DESKTOP_ENGINE + '.tsysinfox64';
begin
  TKeymanSentryClient.Start(TSentryClient, kscpDesktop, LOGGER_DESKTOP_ENGINE_TSYSINFOX64);
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
