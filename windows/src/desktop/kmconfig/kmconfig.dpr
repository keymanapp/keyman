program kmconfig;

uses
  System.SysUtils,
  Keyman.System.SettingsManager in '..\..\global\delphi\general\Keyman.System.SettingsManager.pas',
  RegistryKeys in '..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  Keyman.System.Settings in '..\..\global\delphi\general\Keyman.System.Settings.pas',
  Keyman.System.KeymanSentryClient in '..\..\..\..\common\windows\delphi\general\Keyman.System.KeymanSentryClient.pas',
  Sentry.Client.Console in '..\..\..\..\common\windows\delphi\ext\sentry\Sentry.Client.Console.pas',
  Sentry.Client in '..\..\..\..\common\windows\delphi\ext\sentry\Sentry.Client.pas',
  sentry in '..\..\..\..\common\windows\delphi\ext\sentry\sentry.pas',
  KeymanPaths in '..\..\..\..\common\windows\delphi\general\KeymanPaths.pas',
  DebugPaths in '..\..\..\..\common\windows\delphi\general\DebugPaths.pas',
  utilexecute in '..\..\..\..\common\windows\delphi\general\utilexecute.pas',
  unicode in '..\..\..\..\common\windows\delphi\general\Unicode.pas',
  ErrorControlledRegistry in '..\..\..\..\common\windows\delphi\vcl\ErrorControlledRegistry.pas',
  Keyman.System.KMConfigMain in 'Keyman.System.KMConfigMain.pas',
  Keyman.System.SettingsManagerFile in '..\..\global\delphi\general\Keyman.System.SettingsManagerFile.pas';

{$R manifest.res}
{$R version.res}
{$APPTYPE CONSOLE}

const
  LOGGER_DESKTOP_KMCONFIG = TKeymanSentryClient.LOGGER_DESKTOP + '.kmconfig';
begin
  TKeymanSentryClient.Start(TSentryClientConsole, kscpDesktop, LOGGER_DESKTOP_KMCONFIG, LoadKeymanDesktopSentryFlags);
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
