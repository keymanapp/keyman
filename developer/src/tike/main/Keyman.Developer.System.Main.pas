unit Keyman.Developer.System.Main;

interface

uses
  System.SysUtils,
  System.Win.ComObj,
  Vcl.Forms,
  Winapi.ActiveX,
  Winapi.UxTheme,

  uCEFApplication,
  uCEFTypes,

  KeymanDeveloperOptions,
  Keyman.Developer.System.TikeCommandLine,
  Keyman.System.CEFManager,
  Keyman.System.KeymanSentryClient,
  Sentry.Client,
  Sentry.Client.Vcl,
  UfrmMain,
  UmodWebHttpServer;

procedure RunKeymanDeveloper;

implementation

const
  LOGGER_DEVELOPER_IDE_TIKE = TKeymanSentryClient.LOGGER_DEVELOPER_IDE + '.tike';

procedure RunWithExceptionsHandled; forward;

procedure RunKeymanDeveloper;
begin
  CoInitFlags := COINIT_APARTMENTTHREADED;
  Application.MainFormOnTaskBar := True;
  Application.Initialize;
  Application.Title := 'Keyman Developer';

  TKeymanSentryClient.Start(TSentryClientVcl, kscpDeveloper, LOGGER_DEVELOPER_IDE_TIKE, LoadKeymanDeveloperSentryFlags);
  try
    try
      RunWithExceptionsHandled;
    except
      on E:Exception do
        SentryHandleException(E);
    end;
  finally
    TKeymanSentryClient.Stop;
  end;
end;

procedure RunWithExceptionsHandled;
begin
  FInitializeCEF := TCEFManager.Create(True);
  try
    if GlobalCEFApp.ProcessType = ptBrowser then
    begin
      // We want to process the command line only if we are not a CEF
      // sub-process, because otherwise we lose the benefit of
      if TikeCommandLine.Process = pclExit then
      begin
        Exit;
      end;
    end;

    if FInitializeCEF.Start then
    begin
      InitThemeLibrary;
      SetThemeAppProperties(STAP_ALLOW_NONCLIENT or STAP_ALLOW_CONTROLS or STAP_ALLOW_WEBCONTENT);
      Application.CreateForm(TmodWebHttpServer, modWebHttpServer);
      try
        Application.CreateForm(TfrmKeymanDeveloper, frmKeymanDeveloper);
        try
          Application.Run;
        finally
          FreeAndNil(frmKeymanDeveloper);
        end;
      finally
        FreeAndNil(modWebHttpServer);
      end;
    end;
  finally
    FInitializeCEF.Free;
  end;
end;

end.
