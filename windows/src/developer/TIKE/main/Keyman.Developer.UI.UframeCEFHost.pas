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

  Keyman.Developer.System.CEFManager,
  KeymanDeveloperUtils,
  UserMessages,
  UfrmTIKE,
  uCEFInterfaces,
  uCEFWindowParent,
  uCEFChromiumWindow,
  uCEFTypes, uCEFChromium;

const
  CEF_DESTROY = WM_USER + 300;
  CEF_AFTERDESTROY = WM_USER + 301;
  CEF_AFTERCREATE = WM_USER + 302;
  CEF_SHOW = WM_USER + 303;

type
  TCEFHostBeforeBrowseEvent = procedure(Sender: TObject; const Url: string; out Result: Boolean) of object;

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
  private
    FNextURL: string;
    FOnLoadEnd: TNotifyEvent;
    FOnBeforeBrowse: TCEFHostBeforeBrowseEvent;
    FOnAfterCreated: TNotifyEvent;
    FShutdownCompletionHandler: TShutdownCompletionHandlerEvent;
    FIsClosing: Boolean;
    // IKeymanCEFHost
    procedure StartShutdown(CompletionHandler: TShutdownCompletionHandlerEvent);

    procedure CEFDestroy(var Message: TMessage); message CEF_DESTROY;
    procedure CEFAfterDestroy(var Message: TMessage); message CEF_AFTERDESTROY;
    procedure CEFAfterCreate(var Message: TMessage); message CEF_AFTERCREATE;

    // CEF: You have to handle this two messages to call NotifyMoveOrResizeStarted or some page elements will be misaligned.
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    // CEF: You also have to handle these two messages to set GlobalCEFApp.OsmodalLoop
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;

    procedure CreateBrowser;
    procedure Navigate; overload;
    procedure CEFShow(var message: TMessage); message CEF_SHOW;
  public
    procedure SetFocus; override;
    procedure StartClose;
    procedure Navigate(const url: string); overload;
    property OnAfterCreated: TNotifyEvent read FOnAfterCreated write FOnAfterCreated;
    property OnBeforeBrowse: TCEFHostBeforeBrowseEvent read FOnBeforeBrowse write FOnBeforeBrowse;
    property OnLoadEnd: TNotifyEvent read FOnLoadEnd write FOnLoadEnd;
  end;

implementation

uses
  System.StrUtils,
  Winapi.ShellApi,

  Keyman.Developer.System.HelpTopics,
//  typinfo,
  ErrorControlledRegistry,
  ExternalExceptionHandler,
  UfrmMain,
  uCEFApplication;

{$R *.DFM}

{ TfrmCEFHost }

// Destruction steps
// =================
// 1. The FormCloseQuery event sets CanClose to False and calls TChromiumWindow.CloseBrowser, which triggers the TChromiumWindow.OnClose event.
// 2. The TChromiumWindow.OnClose event calls TChromiumWindow.DestroyChildWindow which triggers the TChromiumWindow.OnBeforeClose event.
// 3. TChromiumWindow.OnBeforeClose sets FCanClose to True and closes the form.

procedure TframeCEFHost.StartClose;
begin
  Visible := False;
  FIsClosing := True;
  cef.CloseBrowser(True);
end;

procedure TframeCEFHost.StartShutdown(CompletionHandler: TShutdownCompletionHandlerEvent);
begin
  OutputDebugString(PChar('TframeCEFHost.StartShutdown'));
  FIsClosing := True;
  FShutdownCompletionHandler := CompletionHandler;

  // If the browser has not been initialized, we'll not get the close signal, so we
  // post it to occur on next idle.
  if cef.Initialized
    then cef.CloseBrowser(False)
    else PostMessage(Handle, CEF_AFTERDESTROY, 0, 0);
end;

procedure TframeCEFHost.FormCreate(Sender: TObject);
begin
  inherited;
  FInitializeCEF.RegisterWindow(Self);
//  CreateBrowser;
end;

procedure TframeCEFHost.FormDestroy(Sender: TObject);
begin
//  OutputDebugString(PChar('TframeCEFHost.FormDestroy'));
  inherited;
  FInitializeCEF.UnregisterWindow(Self);
end;

procedure TframeCEFHost.FormShow(Sender: TObject);
begin
  inherited;
  PostMessage(Handle, CEF_SHOW, 0, 0);
end;

procedure TframeCEFHost.CEFShow(var message: TMessage);
begin
  CreateBrowser;
end;

procedure TframeCEFHost.CreateBrowser;
begin
  tmrCreateBrowser.Enabled := not cef.CreateBrowser(cefwp);
end;

procedure TframeCEFHost.Navigate(const url: string);
begin
  FNextURL := url;
  Navigate;
end;

procedure TframeCEFHost.Navigate;
begin
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
  inherited;
  if not FIsClosing and cefwp.CanFocus then
    cefwp.SetFocus;
end;

procedure TframeCEFHost.CEFAfterCreate(var Message: TMessage);
begin
  Navigate;
end;

procedure TframeCEFHost.cefAfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATE, 0, 0);
end;

procedure TframeCEFHost.CEFAfterDestroy(var Message: TMessage);
begin
  if Assigned(FShutdownCompletionHandler) then
  begin
    FShutdownCompletionHandler(Self);
    FShutdownCompletionHandler := nil;
  end;
end;

procedure TframeCEFHost.cefBeforeBrowse(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; user_gesture, isRedirect: Boolean;
  out Result: Boolean);
begin
  if Assigned(FOnBeforeBrowse) then
    FOnBeforeBrowse(Self, request.Url, Result);
end;

procedure TframeCEFHost.cefBeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
//  OutputDebugString(PChar('TframeCEFHost.cefBeforeClose'));
  PostMessage(Handle, CEF_AFTERDESTROY, 0, 0);
end;

procedure TframeCEFHost.cefClose(Sender: TObject; const browser: ICefBrowser;
  out Result: Boolean);
begin
  PostMessage(Handle, CEF_DESTROY, 0, 0);
  Result := True;
//  OutputDebugString(PChar('TframeCEFHost.cefClose'));
  // DestroyChildWindow will destroy the child window created by CEF at the top of the Z order.
  cefwp.DestroyChildWindow;
end;

procedure TframeCEFHost.cefConsoleMessage(Sender: TObject;
  const browser: ICefBrowser; level: Cardinal; const message, source: ustring;
  line: Integer; out Result: Boolean);
begin
  try
    with TStringList.Create do
    try
      Text := '???'; //todo: get from browser
      LogExceptionToExternalHandler(
        'script_'+Self.ClassName+'_ScriptError',
        'Error occurred at line '+IntToStr(line)+' of '+source,
        message,
        'CEF'#13#10#13#10+Text);
    finally
      Free;
    end;
  except
    on E:Exception do
      LogExceptionToExternalHandler(
        'script_'+Self.ClassName+'_ScriptError',
        'Error occurred at line '+IntToStr(line)+' of '+source,
        message,
        'Exception '+E.Message+' trying to load for review');   // I4687
  end;

  Result := True;
end;

procedure TframeCEFHost.CEFDestroy(var Message: TMessage);
begin
  FreeAndNil(cefwp);
end;

procedure TframeCEFHost.tmrCreateBrowserTimer(Sender: TObject);
begin
  tmrCreateBrowser.Enabled := False;
  CreateBrowser;
end;

procedure TframeCEFHost.cefLoadEnd(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer);
begin
  if csDestroying in ComponentState then
    Exit;
  if Assigned(FOnLoadEnd) then
    FOnLoadEnd(Self);
end;

procedure TframeCEFHost.cefPreKeyEvent(Sender: TObject;
  const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: PMsg;
  out isKeyboardShortcut, Result: Boolean);
begin
  Result := False;
  if (event.windows_key_code <> VK_CONTROL) and (event.kind in [TCefKeyEventType.KEYEVENT_KEYDOWN, TCefKeyEventType.KEYEVENT_RAWKEYDOWN]) then
  begin
    if event.windows_key_code = VK_F1 then
    begin
      isKeyboardShortcut := True;
      Result := True;
      frmKeymanDeveloper.HelpTopic(Self);
    end
    else if event.windows_key_code = VK_F12 then
    begin
      cef.ShowDevTools(Point(Low(Integer),Low(Integer)), nil);
    end
    else if SendMessage(Application.Handle, CM_APPKEYDOWN, event.windows_key_code, 0) = 1 then
    begin
      isKeyboardShortcut := True;
      Result := True;
    end;
  end;
end;

procedure TframeCEFHost.cefBeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var noJavascriptAccess, Result: Boolean);
begin
  Result := True;
  if Assigned(FOnBeforeBrowse) then
  begin
    FOnBeforeBrowse(Self, targetUrl, Result);
    if not Result then
    begin
      cef.LoadURL(targetUrl);
      Result := True;
    end;
  end;
end;

procedure TframeCEFHost.cefRunContextMenu(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel;
  const callback: ICefRunContextMenuCallback; var aResult: Boolean);
begin
  aResult := GetKeyState(VK_SHIFT) >= 0;
end;

procedure TframeCEFHost.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;
  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TframeCEFHost.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;
  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

procedure TframeCEFHost.WMMove(var aMessage: TWMMove);
begin
  inherited;
  if cef <> nil then cef.NotifyMoveOrResizeStarted;
end;

procedure TframeCEFHost.WMMoving(var aMessage: TMessage);
begin
  inherited;
  if cef <> nil then cef.NotifyMoveOrResizeStarted;
end;

end.
