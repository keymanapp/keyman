unit Keyman.System.CEFManager;

interface

uses
  System.Classes,
  System.Generics.Collections,
  uCEFApplication;

//
// We wrap the TCefApplication so we can configure it
// outside the .dpr source and keep the .dpr source
// cleaner
//
type
  IKeymanCEFHost = interface;

  TShutdownCompletionHandlerEvent = procedure(Sender: IKeymanCEFHost) of object;

  IKeymanCEFHost = interface
    ['{DFABC8BF-803E-45E6-B7DC-522C7FEB08EB}']
    procedure StartShutdown(CompletionHandler: TShutdownCompletionHandlerEvent);
    function GetDebugInfo: string;
  end;

  TCEFManager = class
  private
    FWindows: TList<IKeymanCEFHost>;
    FShutdownCompletionHandler: TNotifyEvent;
    procedure CompletionHandler(Sender: IKeymanCEFHost);
    procedure Cleanup(Sender: TObject);
    procedure LaunchWatchdog;
  public
    constructor Create(IsDeveloper: Boolean);
    destructor Destroy; override;
    function Start: Boolean;
    function StartSubProcess: Boolean;  // Use for browser sub process only

    procedure RegisterWindow(cef: IKeymanCEFHost);
    procedure UnregisterWindow(cef: IKeymanCEFHost);
    function StartShutdown(CompletionHandler: TNotifyEvent): Boolean;
  end;

var
  FInitializeCEF: TCEFManager = nil;

implementation

uses
  System.SysUtils,
  Winapi.ShlObj,
  Winapi.Tlhelp32,
  Winapi.Windows,

  Keyman.System.KeymanSentryClient,
  KeymanPaths,
  KeymanVersion,
  RegistryKeys,
  VersionInfo,
  uCEFConstants,
  uCEFDOMVisitor,
  uCEFInterfaces,
  uCEFMiscFunctions,
  uCEFProcessMessage,
  uCEFTypes;

{ TInitializeCEF }

procedure TCEFManager.CompletionHandler(Sender: IKeymanCEFHost);
begin
//  OutputDebugString(PChar('TCEFManager.CompletionHandler'));
  FWindows.Remove(Sender);
  if FWindows.Count = 0 then
  begin
    Assert(@FShutdownCompletionHandler <> nil);
    FShutdownCompletionHandler(Self);
    FShutdownCompletionHandler := nil;
  end;
end;

constructor TCEFManager.Create(IsDeveloper: Boolean);
begin
  FWindows := TList<IKeymanCEFHost>.Create;
  // You *MUST* call GlobalCEFApp.StartMainProcess in a if..then clause
  // with the Application initialization inside the begin..end.
  // Read this https://www.briskbard.com/index.php?lang=en&pageid=cef
  GlobalCEFApp := TCefApplication.Create;

  GlobalCEFApp.CheckCEFFiles := False;

  GlobalCEFApp.LogFile := TKeymanPaths.ErrorLogPath + ExtractFileName(ChangeFileExt(ParamStr(0),''))+'-cef.log';
  GlobalCEFApp.LogSeverity := LOGSEVERITY_ERROR;

  // We run in debug mode when TIKE debug mode flag is set, so that we can
  // debug the CEF interactions more easily
//  GlobalCEFApp.SingleProcess        := TikeDebugMode;

  GlobalCEFApp.BrowserSubprocessPath := TKeymanPaths.CEFSubprocessPath(IsDeveloper);

  // In case you want to use custom directories for the CEF3 binaries, cache, cookies and user data.
  // If you don't set a cache directory the browser will use in-memory cache.
  GlobalCEFApp.FrameworkDirPath     := TKeymanPaths.CEFPath;
  GlobalCEFApp.ResourcesDirPath     := GlobalCEFApp.FrameworkDirPath;
  GlobalCEFApp.LocalesDirPath       := GlobalCEFApp.FrameworkDirPath + '\locales';
  GlobalCEFApp.EnableGPU            := True;      // Enable hardware acceleration
  GlobalCEFApp.cache                := TKeymanPaths.CEFDataPath('cache');
  GlobalCEFApp.UserDataPath         := TKeymanPaths.CEFDataPath('userdata');
  GlobalCEFApp.UserAgent            := 'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.114 Safari/537.36 (Keyman/'+SKeymanVersion+')';

  // We no longer attempt to cleanup before shutdown, and rely on the child cef
  // processes to do the cleanup, which is more reliable.
  // TKeymanSentryClient.OnBeforeShutdown := Self.Cleanup;
end;

procedure TCEFManager.Cleanup(Sender: TObject);
begin
  FreeAndNil(FWindows);
  GlobalCEFApp.Free;
  GlobalCEFApp := nil;
  FInitializeCEF := nil;
end;

destructor TCEFManager.Destroy;
var
  i: Integer;
begin
  if FWindows.Count > 0 then
  begin
    for i := 0 to FWindows.Count - 1 do
      TKeymanSentryClient.Breadcrumb('trace', 'TCEFManager.Destroy:Window['+IntToStr(i)+']='+FWindows[i].GetDebugInfo);
  end;
  Assert(FWindows.Count = 0);
  Cleanup(nil);
  inherited Destroy;
end;

procedure TCEFManager.RegisterWindow(cef: IKeymanCEFHost);
begin
  FWindows.Add(cef);
end;

function TCEFManager.Start: Boolean;
begin
  if GlobalCEFApp.ProcessType <> ptBrowser then
  begin
    LaunchWatchdog;
  end;

  Result := GlobalCEFApp.StartMainProcess;
end;

function TCEFManager.StartSubProcess: Boolean;
begin
  LaunchWatchdog;
  Result := GlobalCEFApp.StartSubProcess;
end;

/// Start a watchdog thread to terminate process when parent does
///
/// Description: This should be run only in the context of the child process.
/// It will normally only happen in an abnormal termination of the parent
/// process (because otherwise the main thread of the child process receives an
/// orderly request to shut down, and does so), but in the situation where the
/// parent process disappears without making that request, this lets the child
/// process do the work.
///
/// This is probably a gap in Chromium Embedded Framework but AFAICT it has not
/// been resolved and this was the suggested fix, e.g. at
/// https://magpcss.org/ceforum/viewtopic.php?p=37820&sid=11f6fcabc4089e41283bd5b9ec17267e#p37820
/// which I have taken and cleaned up because it seemed appropriate.
///
procedure TCEFManager.LaunchWatchdog;

  function GetParentProcess: THandle;
  var
    Snapshot: THandle;
    ProcessEntry: TProcessEntry32;
    CurrentProcessId: DWORD;
  begin
    Snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if Snapshot = INVALID_HANDLE_VALUE then
      Exit(0);

    FillChar(ProcessEntry, sizeof(TProcessEntry32), 0);
    ProcessEntry.dwSize := sizeof(TProcessEntry32);
    CurrentProcessId := GetCurrentProcessId;

    if Process32First(Snapshot, ProcessEntry) then
    begin
      repeat
        if ProcessEntry.th32ProcessID = CurrentProcessId then
          Break;
      until not Process32Next(Snapshot, ProcessEntry);
    end;

    CloseHandle(Snapshot);

    if ProcessEntry.th32ProcessID <> CurrentProcessId then
      Exit(0);

    Result := OpenProcess(SYNCHRONIZE, False, ProcessEntry.th32ParentProcessID);
  end;

var
  hParentProcess: THandle;
begin
  hParentProcess := GetParentProcess;
  if hParentProcess <> 0 then
  begin
    TThread.CreateAnonymousThread(
      procedure
      begin
        WaitForSingleObject(hParentProcess, INFINITE);
        ExitProcess(0);
      end
    ).Start;
  end;
end;

function TCEFManager.StartShutdown(CompletionHandler: TNotifyEvent): Boolean;
var
  i: Integer;
begin
//  OutputDebugString(PChar('TCEFManager.StartShutdown'));
  if FWindows.Count = 0 then
    Exit(True); // Can shutdown immediately
  FShutdownCompletionHandler := CompletionHandler;
  for i := 0 to FWindows.Count - 1 do
    FWindows[i].StartShutdown(Self.CompletionHandler);
  Result := False; // wait for completion handler
end;

procedure TCEFManager.UnregisterWindow(cef: IKeymanCEFHost);
begin
//  OutputDebugString(PChar('TCEFManager.UnregisterWindow'));
  FWindows.Remove(cef);
end;

end.
