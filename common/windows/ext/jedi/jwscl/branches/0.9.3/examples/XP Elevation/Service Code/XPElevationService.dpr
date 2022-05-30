program XPElevationService;

uses
  SvcMgr,
  Classes,
  MainUnit in 'MainUnit.pas' {XPService: TService},
  ThreadUnit in 'ThreadUnit.pas',
  Windows,
  JwsclKnownSID,
  JwsclLogging,
  HandleRequestThread in 'HandleRequestThread.pas',
  SessionPipe in '..\SessionPipe.pas',
  ElevationHandler in 'ElevationHandler.pas',
  ThreadedPasswords in 'ThreadedPasswords.pas',
  uLogging in 'uLogging.pas';

{$R *.RES}

begin
  JwInitWellknownSIDs;
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //
 { if not Application.DelayInitialize or Application.Installing then  }
  uLogging.ApplicationFileName := 'XPElevation';
  uLogging.InitFileLocation;

  uLogging.InitLog;

  //log by default in debug version
{$IFDEF DEBUG}
  uLogging.SwitchLog(true);
{$ENDIF DEBUG}

  try
    Application.Initialize;
    Application.CreateForm(TXPService, XPService);
    XPService.ServiceExecute(nil);
  //  Application.Run;
  finally
    DoneLog;
  end;

end.
