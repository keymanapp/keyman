unit  Keyman.Configuration.System.Main;

interface

uses
  comobj,
  Forms,
  SysUtils,
  ActiveX,

  Keyman.System.CEFManager,

  Keyman.System.KeymanSentryClient,
  Sentry.Client,
  Sentry.Client.Vcl,
  Keyman.Configuration.System.UmodWebHttpServer,
  initprog;

procedure RunKeymanConfiguration;

implementation

const
  LOGGER_DESKTOP_KMSHELL = TKeymanSentryClient.LOGGER_DESKTOP + '.kmshell';

procedure RunKeymanConfiguration;
begin
  TKeymanSentryClient.Start(TSentryClientVcl, kscpDesktop, LOGGER_DESKTOP_KMSHELL, LoadKeymanDesktopSentryFlags);
  try
    CoInitFlags := COINIT_APARTMENTTHREADED;
    FInitializeCEF := TCEFManager.Create(False);
    try
      if FInitializeCEF.Start then
      try
        Application.Initialize;
        Application.Title := 'Keyman Configuration';
        Application.CreateForm(TmodWebHttpServer, modWebHttpServer);
        try
          Run;
        finally
          FreeAndNil(modWebHttpServer);
        end;
      except
        on E:Exception do
          SentryHandleException(E);
      end;
    finally
      FInitializeCEF.Free;
    end;
  finally
    TKeymanSentryClient.Stop;
  end;
end;


end.
