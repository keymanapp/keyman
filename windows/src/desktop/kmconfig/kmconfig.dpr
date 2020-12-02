program kmconfig;

uses
  System.SysUtils,
  Keyman.System.SettingsManager in '..\..\global\delphi\general\Keyman.System.SettingsManager.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  Keyman.System.Settings in '..\..\global\delphi\general\Keyman.System.Settings.pas',
  Keyman.System.KeymanSentryClient in '..\..\global\delphi\general\Keyman.System.KeymanSentryClient.pas',
  Sentry.Client.Console in '..\..\ext\sentry\Sentry.Client.Console.pas',
  Sentry.Client in '..\..\ext\sentry\Sentry.Client.pas',
  sentry in '..\..\ext\sentry\sentry.pas',
  KeymanPaths in '..\..\global\delphi\general\KeymanPaths.pas',
  DebugPaths in '..\..\global\delphi\general\DebugPaths.pas',
  utilexecute in '..\..\global\delphi\general\utilexecute.pas',
  unicode in '..\..\global\delphi\general\unicode.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  Keyman.System.KMConfigMain in 'Keyman.System.KMConfigMain.pas',
  Keyman.System.SettingsManagerFile in '..\..\global\delphi\general\Keyman.System.SettingsManagerFile.pas';

{$R manifest.res}
{$R version.res}
{$APPTYPE CONSOLE}

const
  LOGGER_DESKTOP_KMCONFIG = TKeymanSentryClient.LOGGER_DESKTOP + '.kmconfig';
begin
  TKeymanSentryClient.Start(TSentryClientConsole, kscpDesktop, LOGGER_DESKTOP_KMCONFIG);
  try
    try
      TKeymanSentryClient.Validate;
      TKMConfig.Run;
    except
      on E: Exception do
      begin
        SentryHandleException(E);
        ExitCode := 1;
      end;
    end;
  finally
    TKeymanSentryClient.Stop;
  end;
end.
