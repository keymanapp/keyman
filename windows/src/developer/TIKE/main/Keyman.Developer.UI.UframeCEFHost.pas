unit Keyman.Developer.UI.UframeCEFHost;

interface

uses
  System.Classes,
  System.Contnrs,
  System.SysUtils,
  System.Types,
  System.UITypes,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Menus,
  Winapi.Messages,
  Winapi.Windows,

  uCEFChromium,
  uCEFChromiumEvents,
  uCEFChromiumWindow,
  uCEFInterfaces,
  uCEFTypes,
  uCEFWindowParent,

  Keyman.Developer.System.CEFManager,
  KeymanDeveloperUtils,
  UserMessages,
  UfrmTIKE;

const
  CEF_DESTROY = WM_USER + 300;
  CEF_AFTERDESTROY = WM_USER + 301;
  CEF_AFTERCREATE = WM_USER + 302;
  CEF_SHOW = WM_USER + 303;
  CEF_LOADEND = WM_USER + 304;
  CEF_SETFOCUS = WM_USER + 305;
  CEF_KEYEVENT = WM_USER + 306;
  CEF_BEFOREBROWSE = WM_USER + 307;
  CEF_CONSOLEMESSAGE = WM_USER + 308;

type
  TCEFHostKeyEventData = record
    browserid: Integer;
    event: TCefKeyEvent;
    osEvent: TMsg;
  end;

  PCEFHostKeyEventData = ^TCEFHostKeyEventData;

  TCEFConsoleMessageEventData = record
    browserid: Integer;
    level: Cardinal;
    message, source: ustring;
    line: Integer;
  end;

  PCEFConsoleMessageEventData = ^TCEFConsoleMessageEventData;

  TCEFHostBeforeBrowseSyncEvent = procedure(Sender: TObject; const Url: string; out Handled: Boolean) of object;
  TCEFHostBeforeBrowseEvent = procedure(Sender: TObject; const Url: string; params: TStringList; wasHandled: Boolean) of object;

  TCEFHostPreKeySyncEvent = procedure(Sender: TObject; e: TCEFHostKeyEventData; out isShortcut, Handled: Boolean) of object;
  TCEFHostKeyEvent = procedure(Sender: TObject; e: TCEFHostKeyEventData; wasShortcut, wasHandled: Boolean) of object;

  TframeCEFHost = class(TTikeForm, IKeymanCEFHost)
    tmrRefresh: TTimer;
    tmrCreateBrowser: TTimer;
    cefwp: TCEFWindowParent;
    cef: TChromium;
    procedure FormCreate(Sender: TObject);
    procedure tmrCreateBrowserTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cefAfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure cefBeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure cefClose(Sender: TObject; const browser: ICefBrowser;
      out Result: Boolean);
    procedure cefPreKeyEvent(Sender: TObject; const browser: ICefBrowser;
      const event: PCefKeyEvent; osEvent: PMsg; out isKeyboardShortcut,
      Result: Boolean);
    procedure cefConsoleMessage(Sender: TObject; const browser: ICefBrowser;
      level: Cardinal; const message, source: ustring; line: Integer;
      out Result: Boolean);   // I2986
    procedure cefLoadEnd(Sender: TObject; const browser: ICefBrowser;
                         const frame: ICefFrame; httpStatusCode: Integer);
    procedure cefBeforeBrowse(Sender: TObject; const browser: ICefBrowser;
                              const frame: ICefFrame; const request: ICefRequest; user_gesture,
                              isRedirect: Boolean; out Result: Boolean);
    procedure cefRunContextMenu(Sender: TObject; const browser: ICefBrowser;
                                const frame: ICefFrame; const params: ICefContextMenuParams;
                                const model: ICefMenuModel;
                                const callback: ICefRunContextMenuCallback;
                                var aResult : Boolean);
    procedure cefBeforePopup(Sender: TObject;
                             const browser: ICefBrowser;
                             const frame: ICefFrame;
                             const targetUrl,
                             targetFrameName: ustring;
                             targetDisposition: TCefWindowOpenDisposition;
                             userGesture: Boolean;
                             const popupFeatures: TCefPopupFeatures;
                             var windowInfo: TCefWindowInfo;
                             var client: ICefClient;
                             var settings: TCefBrowserSettings;
                             var noJavascriptAccess: Boolean;
                             var Result: Boolean);
    procedure cefWidgetCompMsg(var aMessage: TMessage; var aHandled: Boolean);
  private
    FApplicationHandle: THandle;
    FNextURL: string;
    FOnLoadEnd: TNotifyEvent;
    FOnBeforeBrowseSync: TCEFHostBeforeBrowseSyncEvent;
    FOnAfterCreated: TNotifyEvent;
    FShutdownCompletionHandler: TShutdownCompletionHandlerEvent;
    FIsClosing: Boolean;
    FShouldShowContextMenu: Boolean;

    FCallbackWnd: THandle;
    FOnPreKeySyncEvent: TCEFHostPreKeySyncEvent;
    FOnKeyEvent: TCEFHostKeyEvent;
    FOnBeforeBrowse: TCEFHostBeforeBrowseEvent;

    procedure CallbackWndProc(var Message: TMessage);

    // IKeymanCEFHost
    procedure StartShutdown(CompletionHandler: TShutdownCompletionHandlerEvent);

    procedure Handle_CEF_DESTROY(var Message: TMessage);
    procedure Handle_CEF_AFTERDESTROY(var Message: TMessage);
    procedure Handle_CEF_AFTERCREATE(var Message: TMessage);
    procedure Handle_CEF_SHOW(var message: TMessage);
    procedure Handle_CEF_LOADEND(var message: TMessage);
    procedure Handle_CEF_SETFOCUS(var message: TMessage);
    procedure Handle_CEF_KEYEVENT(var message: TMessage);
    procedure Handle_CEF_BEFOREBROWSE(var message: TMessage);
    procedure Handle_CEF_CONSOLEMESSAGE(var message: TMessage);

    // CEF: You have to handle this two messages to call NotifyMoveOrResizeStarted or some page elements will be misaligned.
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    // CEF: You also have to handle these two messages to set GlobalCEFApp.OsmodalLoop
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;

    procedure CreateBrowser;
    procedure Navigate; overload;
    procedure DoBeforeBrowse(const url: string; out Handled: Boolean; ShouldOpenUrlIfNotHandled: Boolean);
  public
    procedure SetFocus; override;
    procedure StartClose;
    procedure Navigate(const url: string); overload;
    function HasFocus: Boolean;
    property ShouldShowContextMenu: Boolean read FShouldShowContextMenu write FShouldShowContextMenu;
    property OnAfterCreated: TNotifyEvent read FOnAfterCreated write FOnAfterCreated;
    property OnBeforeBrowseSync: TCEFHostBeforeBrowseSyncEvent read FOnBeforeBrowseSync write FOnBeforeBrowseSync;
    property OnBeforeBrowse: TCEFHostBeforeBrowseEvent read FOnBeforeBrowse write FOnBeforeBrowse;
    property OnLoadEnd: TNotifyEvent read FOnLoadEnd write FOnLoadEnd;

    property OnPreKeySyncEvent: TCEFHostPreKeySyncEvent read FOnPreKeySyncEvent write FOnPreKeySyncEvent;
    property OnKeyEvent: TCEFHostKeyEvent read FOnKeyEvent write FOnKeyEvent;
  end;

// Helpers to make sure we don't accidentally code
// VCL references into non-VCL-thread functions
procedure AssertVclThread;
procedure AssertCefThread;

implementation

uses
  System.StrUtils,
  Winapi.ShellApi,

  Keyman.Developer.System.HelpTopics,
//  typinfo,
  ErrorControlledRegistry,
  ExternalExceptionHandler,
  UfrmMain,
  utilhttp,
  uCEFApplication,
  VersionInfo;

{$R *.DFM}

{ TfrmCEFHost }

// Destruction steps
// =================
// 1. The FormCloseQuery event sets CanClose to False and calls TChromiumWindow.CloseBrowser, which triggers the TChromiumWindow.OnClose event.
// 2. The TChromiumWindow.OnClose event calls TChromiumWindow.DestroyChildWindow which triggers the TChromiumWindow.OnBeforeClose event.
// 3. TChromiumWindow.OnBeforeClose sets FCanClose to True and closes the form.

procedure AssertVclThread;
begin
  Assert(GetCurrentThreadId = MainThreadID);
end;

procedure AssertCefThread;
begin
  Assert(GetCurrentThreadId <> MainThreadID);
end;

procedure TframeCEFHost.StartClose;
begin
  AssertVclThread;
  Visible := False;
  FIsClosing := True;
  cef.CloseBrowser(True);
end;

procedure TframeCEFHost.StartShutdown(CompletionHandler: TShutdownCompletionHandlerEvent);
begin
  AssertVclThread;
  OutputDebugString(PChar('TframeCEFHost.StartShutdown'));
  FIsClosing := True;
  FShutdownCompletionHandler := CompletionHandler;

  // If the browser has not been initialized, we'll not get the close signal, so we
  // post it to occur on next idle.
  if cef.Initialized
    then cef.CloseBrowser(False)
    else PostMessage(FCallbackWnd, CEF_AFTERDESTROY, 0, 0);
end;

procedure TframeCEFHost.FormCreate(Sender: TObject);
begin
  AssertVclThread;
  inherited;
  FApplicationHandle := Application.Handle; // take a copy to avoid Vcl thread mismatches in cef callbacks

  // We need our own window handle for events, because VCL windows can be destroyed
  // and recreated at any time. With our own handle, we can guarantee the lifetime
  // of it across threads.
  FCallbackWnd := AllocateHWnd(CallbackWndProc);
  FInitializeCEF.RegisterWindow(Self);
//  CreateBrowser;
end;

procedure TframeCEFHost.FormDestroy(Sender: TObject);
begin
  AssertVclThread;
//  OutputDebugString(PChar('TframeCEFHost.FormDestroy'));
  inherited;
  FInitializeCEF.UnregisterWindow(Self);
  DeallocateHWnd(FCallbackWnd);
end;

procedure TframeCEFHost.FormShow(Sender: TObject);
begin
  AssertVclThread;
  inherited;
  PostMessage(FCallbackWnd, CEF_SHOW, 0, 0);
end;

function TframeCEFHost.HasFocus: Boolean;
begin
  AssertVclThread;
  Result := cefwp.HandleAllocated and IsChild(cefwp.Handle, GetFocus);
end;

procedure TframeCEFHost.Handle_CEF_SHOW(var message: TMessage);
begin
  AssertVclThread;
  CreateBrowser;
end;

procedure TframeCEFHost.cefWidgetCompMsg(var aMessage: TMessage;
  var aHandled: Boolean);
begin
  AssertCefThread;
  if aMessage.Msg = WM_SETFOCUS then
    PostMessage(FCallbackWnd, CEF_SETFOCUS, 0, 0);
end;

procedure TframeCEFHost.CreateBrowser;
begin
  AssertVclThread;
  tmrCreateBrowser.Enabled := not cef.CreateBrowser(cefwp);
end;

procedure TframeCEFHost.Navigate(const url: string);
begin
  AssertVclThread;
  FNextURL := url;
  Navigate;
end;

procedure TframeCEFHost.Navigate;
begin
  AssertVclThread;
  if FNextURL = '' then
    Exit;

  if not cef.Initialized then
  begin
    // After initialization, refresh will happen
    // See cefAfterCreated
    Exit;
  end;

  cef.LoadURL(FNextURL);
end;

procedure TframeCEFHost.SetFocus;
begin
  AssertVclThread;
  inherited;
  if not FIsClosing and cefwp.CanFocus then
    cefwp.SetFocus;
end;

procedure TframeCEFHost.CallbackWndProc(var Message: TMessage);
begin
  AssertVclThread;
  case Message.Msg of
    CEF_DESTROY: Handle_CEF_DESTROY(Message);
    CEF_AFTERDESTROY: Handle_CEF_AFTERDESTROY(Message);
    CEF_AFTERCREATE: Handle_CEF_AFTERCREATE(Message);
    CEF_SHOW: Handle_CEF_SHOW(Message);
    CEF_LOADEND: Handle_CEF_LOADEND(Message);
    CEF_SETFOCUS: Handle_CEF_SETFOCUS(Message);
    CEF_KEYEVENT: Handle_CEF_KEYEVENT(Message);
    CEF_BEFOREBROWSE: Handle_CEF_BEFOREBROWSE(Message);
    CEF_CONSOLEMESSAGE: Handle_CEF_CONSOLEMESSAGE(Message);
  end;

  if Self <> nil then
    Message.Result := DefWindowProc(FCallbackWnd, Message.Msg, Message.WParam, Message.LParam);
end;

procedure TframeCEFHost.Handle_CEF_AFTERCREATE(var Message: TMessage);
begin
  AssertVclThread;
  Navigate;
  if Assigned(FOnAfterCreated) then
    FOnAfterCreated(Self);
end;

procedure TframeCEFHost.cefAfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  AssertCefThread;
  PostMessage(FCallbackWnd, CEF_AFTERCREATE, 0, 0);
end;

procedure TframeCEFHost.Handle_CEF_AFTERDESTROY(var Message: TMessage);
begin
  AssertVclThread;
  if Assigned(FShutdownCompletionHandler) then
  begin
    FShutdownCompletionHandler(Self);
    FShutdownCompletionHandler := nil;
  end;
end;

procedure TframeCEFHost.Handle_CEF_BEFOREBROWSE(var message: TMessage);
var
  params: TStringList;
  url: string;
  wasHandled,
  shouldOpenUrlIfNotHandled: Boolean;
begin
  AssertVclThread;
  params := TStringList(message.LParam);
  url := params[0];
  params.Delete(0);

  wasHandled := Boolean(message.WParamLo);
  shouldOpenUrlIfNotHandled := Boolean(message.WParamHi);

  if wasHandled then
  begin
    if Assigned(FOnBeforeBrowse) then
      FOnBeforeBrowse(Self, url, params, Boolean(message.WParam));
  end
  else if shouldOpenUrlIfNotHandled then
    cef.LoadURL(url);

  params.Free;
end;

procedure TframeCEFHost.Handle_CEF_CONSOLEMESSAGE(var message: TMessage);
var
  I: Integer;
  id: string;
  p: PCEFConsoleMessageEventData;
begin
  AssertVclThread;
  p := PCEFConsoleMessageEventData(message.LParam);

  id := LowerCase(ExtractFileName(ParamStr(0)))+'_'+GetVersionString+'_script_';
  if Assigned(Owner)
    then id := id + Owner.ClassName
    else id := id + 'unknown';

  I := string(p.source).LastDelimiter('/');
  id := id + '_' + string(p.source).SubString(I + 1) + '_' + IntToStr(p.line);

  LogExceptionToExternalHandler(id, 'Error occurred at line '+IntToStr(p.line)+' of '+p.source, p.message, '');

  FreeMem(p);
end;

procedure TframeCEFHost.cefBeforeBrowse(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; user_gesture, isRedirect: Boolean;
  out Result: Boolean);
begin
  AssertCefThread;

  DoBeforeBrowse(request.Url, Result, False);
end;

procedure TframeCEFHost.DoBeforeBrowse(const url: string; out Handled: Boolean; ShouldOpenUrlIfNotHandled: Boolean);
var
  params: TStringList;
begin
  AssertCefThread;

  Handled := False;
  if Assigned(FOnBeforeBrowseSync) then
  begin
    FOnBeforeBrowseSync(Self, url, Handled);
  end;

  params := nil;
  if not Handled then
    Handled := GetParamsFromURL(Url, params);

  if not Assigned(params) then
    params := TStringList.Create;

  params.Insert(0, Url);

  PostMessage(FCallbackWnd, CEF_BEFOREBROWSE, MAKELONG(WORD(Handled), WORD(ShouldOpenUrlIfNotHandled)), LPARAM(params));
end;

procedure TframeCEFHost.cefBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  AssertCefThread;
  PostMessage(FCallbackWnd, CEF_AFTERDESTROY, 0, 0);
end;

procedure TframeCEFHost.cefClose(Sender: TObject; const browser: ICefBrowser;
  out Result: Boolean);
begin
  AssertCefThread;
  PostMessage(FCallbackWnd, CEF_DESTROY, 0, 0);
  Result := True;
end;

procedure TframeCEFHost.cefConsoleMessage(Sender: TObject;
  const browser: ICefBrowser; level: Cardinal; const message, source: ustring;
  line: Integer; out Result: Boolean);
var
  p: PCEFConsoleMessageEventData;
begin
  AssertCefThread;

  p := AllocMem(SizeOf(TCEFConsoleMessageEventData));
  p.browserid := browser.Identifier;
  p.level := level;
  p.message := message;
  p.source := source;
  p.line := line;

  Result := True;

  PostMessage(FCallbackWnd, CEF_CONSOLEMESSAGE, WORD(Result), LPARAM(p));
end;

procedure TframeCEFHost.Handle_CEF_DESTROY(var Message: TMessage);
begin
  AssertVclThread;
  cefwp.DestroyChildWindow;
  FreeAndNil(cefwp);
end;

procedure TframeCEFHost.tmrCreateBrowserTimer(Sender: TObject);
begin
  AssertVclThread;
  tmrCreateBrowser.Enabled := False;
  CreateBrowser;
end;

procedure TframeCEFHost.cefLoadEnd(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer);
begin
  AssertCefThread;
  PostMessage(FCallbackWnd, CEF_LOADEND, httpStatusCode, 0);
end;

procedure TframeCEFHost.Handle_CEF_KEYEVENT(var message: TMessage);
var
  p: PCEFHostKeyEventData;
  wasHandled: Boolean;
  wasShortcut: Boolean;
begin
  p := PCEFHostKeyEventData(message.LParam);

  if p.event.windows_key_code = VK_F1 then
  begin
    frmKeymanDeveloper.HelpTopic(Self)
  end
  else if p.event.windows_key_code = VK_F12 then
  begin
    cef.ShowDevTools(Point(Low(Integer),Low(Integer)), nil);
  end
  else if Assigned(FOnKeyEvent) then
  begin
    wasHandled := message.WParamLo <> 0;
    wasShortcut := message.WParamHi <> 0;
    FOnKeyEvent(Self, p^, wasShortcut, wasHandled);
  end;
  FreeMem(p);
end;

procedure TframeCEFHost.Handle_CEF_LOADEND(var message: TMessage);
begin
  if csDestroying in ComponentState then
    Exit;
  if Assigned(FOnLoadEnd) then
    FOnLoadEnd(Self);
end;

procedure TframeCEFHost.Handle_CEF_SETFOCUS(var message: TMessage);
begin
  AssertVclThread;
  if Assigned(cefwp) and cefwp.Visible and cefwp.CanFocus then
    GetParentForm(cefwp).ActiveControl := cefwp;
end;

procedure TframeCEFHost.cefPreKeyEvent(Sender: TObject;
  const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: PMsg;
  out isKeyboardShortcut, Result: Boolean);
var
  p: PCEFHostKeyEventData;
begin
  AssertCefThread;
  Result := False;

  p := AllocMem(Sizeof(TCEFHostKeyEventData));
  p.browserid := browser.Identifier;
  p.event := event^;
  if Assigned(osEvent) then
    p.osEvent := osEvent^;

  if event.kind in [TCefKeyEventType.KEYEVENT_KEYDOWN, TCefKeyEventType.KEYEVENT_RAWKEYDOWN] then
  begin
    if Assigned(FOnPreKeySyncEvent) then
    begin
      FOnPreKeySyncEvent(Self, p^, isKeyboardShortcut, Result);
    end;

    if not Result then  // only run this if the prekeysyncevent didn't swallow the keystroke
    begin
      if event.windows_key_code = VK_F1 then
      begin
        isKeyboardShortcut := True;
        Result := True;
      end
      else if event.windows_key_code = VK_F12 then
      begin
        isKeyboardShortcut := True;
        Result := True;
      end
      else if event.windows_key_code <> VK_CONTROL then
      begin
        if SendMessage(FApplicationHandle, CM_APPKEYDOWN, event.windows_key_code, 0) = 1 then
        begin
          isKeyboardShortcut := True;
          Result := True;
        end;
      end;
    end;

    PostMessage(FCallbackWnd, CEF_KEYEVENT, MAKELONG(WORD(Result), WORD(isKeyboardShortcut)), LPARAM(p));
  end;

end;

procedure TframeCEFHost.cefBeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var noJavascriptAccess, Result: Boolean);
begin
  AssertCefThread;
  DoBeforeBrowse(targetUrl, Result, True);
end;

procedure TframeCEFHost.cefRunContextMenu(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel;
  const callback: ICefRunContextMenuCallback; var aResult: Boolean);
begin
  AssertCefThread;
  // Return FALSE to show default context menu
  aResult := not FShouldShowContextMenu and (GetKeyState(VK_SHIFT) >= 0);
end;

procedure TframeCEFHost.WMEnterMenuLoop(var aMessage: TMessage);
begin
  AssertVclThread;
  inherited;
  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TframeCEFHost.WMExitMenuLoop(var aMessage: TMessage);
begin
  AssertVclThread;
  inherited;
  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

procedure TframeCEFHost.WMMove(var aMessage: TWMMove);
begin
  AssertVclThread;
  inherited;
  if cef <> nil then cef.NotifyMoveOrResizeStarted;
end;

procedure TframeCEFHost.WMMoving(var aMessage: TMessage);
begin
  AssertVclThread;
  inherited;
  if cef <> nil then cef.NotifyMoveOrResizeStarted;
end;

end.
