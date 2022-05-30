program WinLogonLogoService;

uses
  SvcMgr,
  WinLogonLogoServiceUnit in 'WinLogonLogoServiceUnit.pas' {SENSTestService: TService},
  WinLogonLogoServiceProxy in 'WinLogonLogoServiceProxy.pas' {SENSLogonProxy: CoClass},
  WinLogonLogoService_TLB in 'WinLogonLogoService_TLB.pas';

{$R *.TLB}

{$R *.RES}
var B: Boolean;
begin
  // Für Windows 2003 Server muss StartServiceCtrlDispatcher
  // CoRegisterClassObject aufgerufen werden, das von Application.Initialize
  // indirekt aufgerufen werden kann. TServiceApplication.DelayInitialize ermöglicht,
  // dass Application.Initialize von TService.Main aufgerufen werden kann
  // (nachdem StartServiceCtrlDispatcher aufgerufen wurde).
  //
  // Eine verzögerte Initialisierung des Application-Objekts könnte sich auf
  // Ereignisse auswirken, die dann vor der Initialisierung auftreten, wie z.B.
  // TService.OnCreate. Dies wird nur empfohlen, wenn ServiceApplication
  // ein Klassenobjekt bei OLE registriert und sollte nur in Verbindung mit
  // Windows 2003 Server verwendet werden.
  //
  // Application.DelayInitialize := True;
  //
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TSENSTestService, SENSTestService);
{$IFDEF LIVE_DEBUG}
  SENSTestService.ServiceCreate(nil);
  B := true;
  SENSTestService.ServiceStart(nil, B);
  if B then
  begin
    SENSTestService.ServiceExecute(nil);
    SENSTestService.ServiceShutdown(nil);
    SENSTestService.ServiceDestroy(nil);
  end;  
{$ELSE}
  Application.Run;
{$ENDIF LIVE_DEBUG}  
end.
