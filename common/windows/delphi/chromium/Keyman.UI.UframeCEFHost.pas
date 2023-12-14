unit Keyman.UI.UframeCEFHost;

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
  uCEFChromiumCore,
  uCEFChromiumEvents,
  uCEFChromiumWindow,
  uCEFInterfaces,
  uCEFTypes,
  uCEFWinControl,
  uCEFWindowParent,

  Keyman.System.CEFManager,
  UserMessages,
  utilexecute;

const
  CEF_DESTROY = WM_USER + 300;
  CEF_AFTERDESTROY = WM_USER + 301;
  CEF_AFTERCREATE = WM_USER + 302;
  CEF_SHOW = WM_USER + 303;
  CEF_LOADEND = WM_USER + 304;
  CEF_KEYEVENT = WM_USER + 306;
  CEF_BEFOREBROWSE = WM_USER + 307;
  CEF_TITLECHANGE = WM_USER + 309;
  CEF_COMMAND = WM_USER + 310;
  CEF_SETFOCUS = WM_USER + 312;
  CEF_LOADINGSTATECHANGE = WM_USER + 313;


  CEF_LOADINGSTATECHANGE_ISLOADING = $0001;
  CEF_LOADINGSTATECHANGE_CANGOBACK = $0002;
  CEF_LOADINGSTATECHANGE_CANGOFORWARD = $0004;

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

  TCEFTitleChangeEventData = record
    browserid: Integer;
    title: string;
  end;

  PCEFTitleChangeEventData = ^TCEFTitleChangeEventData;

  TCEFHostBeforeBrowseExSyncEvent = procedure(Sender: TObject; const Url: string; isMain, isPopup: Boolean; out Handled: Boolean) of object;
  TCEFHostBeforeBrowseSyncEvent = procedure(Sender: TObject; const Url: string; isPopup: Boolean; out Handled: Boolean) of object;
  TCEFHostBeforeBrowseExEvent = procedure(Sender: TObject; const Url: string; isMain, isPopup, wasHandled: Boolean) of object;
  TCEFHostBeforeBrowseEvent = procedure(Sender: TObject; const Url: string; isPopup, wasHandled: Boolean) of object;
  TCEFCommandEvent = procedure(Sender: TObject; const command: string; params: TStringList) of object;

  TCEFHostPreKeySyncEvent = procedure(Sender: TObject; e: TCEFHostKeyEventData; out isShortcut, Handled: Boolean) of object;
  TCEFHostKeyEvent = procedure(Sender: TObject; e: TCEFHostKeyEventData; wasShortcut, wasHandled: Boolean) of object;
  TCEFHostTitleChangeEvent = procedure(Sender: TObject; const title: string) of object;
  TCEFHostLoadingStateChangeEvent = procedure(Sender: TObject; isLoading, canGoBack, canGoForward: Boolean) of object;

  TframeCEFHost = class(TForm, IKeymanCEFHost)
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
      var aAction: TCefCloseBrowserAction);
    procedure cefPreKeyEvent(Sender: TObject; const browser: ICefBrowser;
      const event: PCefKeyEvent; osEvent: TCefEventHandle; out isKeyboardShortcut,
      Result: Boolean);   // I2986

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
                             var extra_info: ICefDictionaryValue;
                             var noJavascriptAccess: Boolean;
                             var Result: Boolean);
    procedure cefSetFocus(Sender: TObject; const browser: ICefBrowser;
      source: TCefFocusSource; out Result: Boolean);
    procedure cefTitleChange(Sender: TObject; const browser: ICefBrowser;
      const title: ustring);
    procedure cefWidgetCompMsg(Sender: TObject; var aMessage: TMessage; var aHandled: Boolean);
    procedure cefLoadingStateChange(Sender: TObject; const browser: ICefBrowser;
      isLoading, canGoBack, canGoForward: Boolean);
  private
    FApplicationHandle: THandle;
    FNextURL: string;
    FOnLoadEnd: TNotifyEvent;
    FOnBeforeBrowseSync: TCEFHostBeforeBrowseSyncEvent;
    FOnBeforeBrowseExSync: TCEFHostBeforeBrowseExSyncEvent;
    FOnAfterCreated: TNotifyEvent;
    FShutdownCompletionHandler: TShutdownCompletionHandlerEvent;
    FIsClosing: Boolean;
    FShouldShowContextMenu: Boolean;
    FShouldOpenRemoteUrlsInBrowser: Boolean;

    FCallbackWnd: THandle;
    FOnPreKeySyncEvent: TCEFHostPreKeySyncEvent;
    FOnKeyEvent: TCEFHostKeyEvent;
    FOnBeforeBrowse: TCEFHostBeforeBrowseEvent;
    FOnBeforeBrowseEx: TCEFHostBeforeBrowseExEvent;
    FOnTitleChange: TCEFHostTitleChangeEvent;
    FOnCommand: TCEFCommandEvent;
    FOnHelpTopic: TNotifyEvent;
    FIsCreated: Boolean;
    FOnLoadingStateChange: TCEFHostLoadingStateChangeEvent;

    procedure CallbackWndProc(var Message: TMessage);

    // IKeymanCEFHost
    procedure StartShutdown(CompletionHandler: TShutdownCompletionHandlerEvent);
    function GetDebugInfo: string;

    procedure Handle_CEF_DESTROY(var Message: TMessage);
    procedure Handle_CEF_AFTERDESTROY(var Message: TMessage);
    procedure Handle_CEF_AFTERCREATE(var Message: TMessage);
    procedure Handle_CEF_SHOW(var message: TMessage);
    procedure Handle_CEF_LOADEND(var message: TMessage);
    procedure Handle_CEF_KEYEVENT(var message: TMessage);
    procedure Handle_CEF_BEFOREBROWSE(var message: TMessage);
    procedure Handle_CEF_TITLECHANGE(var message: TMessage);
    procedure Handle_CEF_COMMAND(var message: TMessage);
    procedure Handle_CEF_SETFOCUS(var message: TMessage);
    procedure Handle_CEF_LOADINGSTATECHANGE(var message: TMessage);

    // CEF: You have to handle this two messages to call NotifyMoveOrResizeStarted or some page elements will be misaligned.
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    // CEF: You also have to handle these two messages to set GlobalCEFApp.OsmodalLoop
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;

    procedure CreateBrowser;
    procedure Navigate; overload;
    procedure DoBeforeBrowse(const url: string; isMain, isPopup, ShouldOpenUrlIfNotHandled: Boolean; out Handled: Boolean);
  public
    procedure SetFocus; override;
    procedure StartClose;
    procedure Navigate(const url: string); overload;
    function HasFocus: Boolean;
    property ShouldShowContextMenu: Boolean read FShouldShowContextMenu write FShouldShowContextMenu;
    property ShouldOpenRemoteUrlsInBrowser: Boolean read FShouldOpenRemoteUrlsInBrowser write FShouldOpenRemoteUrlsInBrowser;
    property OnAfterCreated: TNotifyEvent read FOnAfterCreated write FOnAfterCreated;
    property OnBeforeBrowseExSync: TCEFHostBeforeBrowseExSyncEvent read FOnBeforeBrowseExSync write FOnBeforeBrowseExSync;
    property OnBeforeBrowseSync: TCEFHostBeforeBrowseSyncEvent read FOnBeforeBrowseSync write FOnBeforeBrowseSync;
    property OnCommand: TCEFCommandEvent read FOnCommand write FOnCommand;
    property OnBeforeBrowseEx: TCEFHostBeforeBrowseExEvent read FOnBeforeBrowseEx write FOnBeforeBrowseEx;
    property OnBeforeBrowse: TCEFHostBeforeBrowseEvent read FOnBeforeBrowse write FOnBeforeBrowse;
    property OnHelpTopic: TNotifyEvent read FOnHelpTopic write FOnHelpTopic;
    property OnLoadEnd: TNotifyEvent read FOnLoadEnd write FOnLoadEnd;
    property OnTitleChange: TCEFHostTitleChangeEvent read FOnTitleChange write FOnTitleChange;
    property OnPreKeySyncEvent: TCEFHostPreKeySyncEvent read FOnPreKeySyncEvent write FOnPreKeySyncEvent;
    property OnKeyEvent: TCEFHostKeyEvent read FOnKeyEvent write FOnKeyEvent;
    property OnLoadingStateChange: TCEFHostLoadingStateChangeEvent read FOnLoadingStateChange write FOnLoadingStateChange;
  end;

// Helpers to make sure we don't accidentally code
// VCL references into non-VCL-thread functions
procedure AssertVclThread;
procedure AssertCefThread;

implementation

uses
  System.StrUtils,
  Winapi.ShellApi,

  ErrorControlledRegistry,
  utilhttp,
  uCEFApplication,
  uCEFConstants,
  uCEFProcessMessage,
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

function TframeCEFHost.GetDebugInfo: string;
begin
  Result := FNextURL;
  if Assigned(Owner) then Result := Owner.ClassName+':'+Result;
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

procedure TframeCEFHost.Handle_CEF_TITLECHANGE(var message: TMessage);
var
  p: PCEFTitleChangeEventData;
begin
  AssertVclThread;
  p := PCEFTitleChangeEventData(message.LParam);

  if Assigned(FOnTitleChange) then
    FOnTitleChange(Self, p.title);

  FreeMem(p);
end;

procedure TframeCEFHost.cefWidgetCompMsg(Sender: TObject; var aMessage: TMessage;
  var aHandled: Boolean);
begin
  AssertCefThread;
  if aMessage.Msg = WM_SETFOCUS then
    PostMessage(FCallbackWnd, CEF_SETFOCUS, 0, 0);
end;

procedure TframeCEFHost.CreateBrowser;
begin
  AssertVclThread;
  FIsCreated := True;
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

  if not FIsCreated then
  begin
    cef.DefaultUrl := FNextURL;
    FNextURL := '';
    Exit;
  end;

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
  if not FIsClosing and cefwp.CanFocus and Assigned(cef) then
  begin
    GetParentForm(Self).ActiveControl := Self;
    cef.SetFocus(True);
  end;
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
    CEF_KEYEVENT: Handle_CEF_KEYEVENT(Message);
    CEF_BEFOREBROWSE: Handle_CEF_BEFOREBROWSE(Message);
    CEF_TITLECHANGE: Handle_CEF_TITLECHANGE(Message);
    CEF_COMMAND: Handle_CEF_COMMAND(Message);
    CEF_SETFOCUS: Handle_CEF_SETFOCUS(Message);
    CEF_LOADINGSTATECHANGE: Handle_CEF_LOADINGSTATECHANGE(Message);
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

function IsLocalURL(URL: string): Boolean;
begin
  Result :=
    URL.StartsWith('file:') or
    URL.StartsWith('/') or
    URL.StartsWith('http://localhost:') or
    URL.StartsWith('http://localhost/') or
    URL.StartsWith('http://127.0.0.1:') or
    URL.StartsWith('http://127.0.0.1/');
end;

function IsPDFURL(URL: string): Boolean;
begin
  Result := LowerCase(URL).EndsWith('.pdf');
end;

procedure TframeCEFHost.Handle_CEF_BEFOREBROWSE(var message: TMessage);
var
  params: TStringList;
  url: string;
  isMain, isPopup, wasHandled,
  shouldOpenUrlIfNotHandled: Boolean;
begin
  AssertVclThread;

  params := TStringList(message.LParam);
  url := params[0];
  wasHandled := (message.WParam and 1) = 1;
  shouldOpenUrlIfNotHandled := (message.WParam and 2) = 2;
  isPopup := (message.WParam and 4) = 4;
  isMain := (message.WParam and 8) = 8;

  if wasHandled then
  begin
    if Assigned(FOnBeforeBrowseEx) then
    begin
      FOnBeforeBrowseEx(Self, url, isMain, isPopup, wasHandled);
    end
    else if Assigned(FOnBeforeBrowse) then
    begin
      FOnBeforeBrowse(Self, url, isPopup, wasHandled);
    end;

    if FShouldOpenRemoteUrlsInBrowser and (not IsLocalURL(URL) or IsPDFURL(URL)) then
    begin
      {$MESSAGE HINT 'Refactor how remote URLs are handled'}
      // TODO: refactor links
      TUtilExecute.URL(url);
    end;
  end
  else if shouldOpenUrlIfNotHandled then
    cef.LoadURL(url);

  params.Free;
end;

procedure TframeCEFHost.Handle_CEF_COMMAND(var message: TMessage);
var
  params: TStringList;
  command: string;
begin
  AssertVclThread;
  params := TStringList(message.LParam);

  if Assigned(FOnCommand) then
  begin
    params.Delete(0); // url; not really used currently
    command := params[0];
    params.Delete(0);
    FOnCommand(Self, command, params);
  end;

  params.Free;
end;

procedure TframeCEFHost.cefBeforeBrowse(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; user_gesture, isRedirect: Boolean;
  out Result: Boolean);
begin
  AssertCefThread;

  DoBeforeBrowse(request.Url, frame.IsMain, False, False, Result);
end;

procedure TframeCEFHost.DoBeforeBrowse(const url: string; isMain, isPopup, ShouldOpenUrlIfNotHandled: Boolean; out Handled: Boolean);
var
  params: TStringList;
  wParam: DWORD;
begin
  AssertCefThread;

  Handled := False;

  if Assigned(FOnBeforeBrowseExSync) then
  begin
    FOnBeforeBrowseExSync(Self, url, isMain, isPopup, Handled);
  end
  else if Assigned(FOnBeforeBrowseSync) then
  begin
    FOnBeforeBrowseSync(Self, url, isPopup, Handled);
  end;

  if not Handled and GetParamsFromURL(Url, params) then
  begin
    Handled := True;
    // Use OnCommand for keyman: URLs
    params.Insert(0, Url);
    PostMessage(FCallbackWnd, CEF_COMMAND, 0, LPARAM(params));
  end
  else
  begin
    // Use OnBeforeBrowse for other URLs
    if not Handled and FShouldOpenRemoteUrlsInBrowser and (not IsLocalURL(URL) or IsPDFURL(URL)) then
      Handled := True;

    params := TStringList.Create;
    params.Add(Url);
    wParam := 0;
    if Handled then
      wParam := wParam or 1;
    if ShouldOpenUrlIfNotHandled then
      wParam := wParam or 2;
    if isPopup then
      wParam := wParam or 4;
    if isMain then
      wParam := wParam or 8;
    PostMessage(FCallbackWnd, CEF_BEFOREBROWSE, wParam, LPARAM(params));
  end;
end;

procedure TframeCEFHost.cefBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  AssertCefThread;
  PostMessage(FCallbackWnd, CEF_AFTERDESTROY, 0, 0);
end;

procedure TframeCEFHost.cefClose(Sender: TObject; const browser: ICefBrowser;
  var aAction: TCefCloseBrowserAction);
begin
  AssertCefThread;
  PostMessage(FCallbackWnd, CEF_DESTROY, 0, 0);
  aAction := cbaClose;
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
  PostMessage(FCallbackWnd, CEF_LOADEND, WPARAM(httpStatusCode), 0);
end;

procedure TframeCEFHost.cefLoadingStateChange(Sender: TObject;
  const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
var
  v: Integer;
begin
  AssertCefThread;
  v := 0;
  if isLoading then v := v or CEF_LOADINGSTATECHANGE_ISLOADING;
  if canGoBack then v := v or CEF_LOADINGSTATECHANGE_CANGOBACK;
  if canGoForward then v := v or CEF_LOADINGSTATECHANGE_CANGOFORWARD;
  PostMessage(FCallbackWnd, CEF_LOADINGSTATECHANGE, v, 0);
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
    if Assigned(FOnHelpTopic) then FOnHelpTopic(Self); // TODO: frmKeymanDeveloper.HelpTopic(Self)
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

  // The focus needs to be set again for key events to
  // be passed to the CEF window, even if it appears
  // to already be focused to the expected window.
  if IsChild(Handle, GetFocus) then
    SetFocus;

  if Assigned(FOnLoadEnd) then
    FOnLoadEnd(Self);
end;

procedure TframeCEFHost.Handle_CEF_LOADINGSTATECHANGE(var message: TMessage);
begin
  if csDestroying in ComponentState then
    Exit;
  AssertVclThread;

  if Assigned(FOnLoadingStateChange) then
    FOnLoadingStateChange(Self,
      (message.WParam and CEF_LOADINGSTATECHANGE_ISLOADING) <> 0,
      (message.WParam and CEF_LOADINGSTATECHANGE_CANGOBACK) <> 0,
      (message.WParam and CEF_LOADINGSTATECHANGE_CANGOFORWARD) <> 0);
end;

procedure TframeCEFHost.Handle_CEF_SETFOCUS(var message: TMessage);
begin
  AssertVclThread;
  if Assigned(cefwp) and cefwp.Visible and cefwp.CanFocus then
    GetParentForm(cefwp).ActiveControl := cefwp;
end;

procedure TframeCEFHost.cefPreKeyEvent(Sender: TObject;
  const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle;
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
  var settings: TCefBrowserSettings;
  var extra_info: ICefDictionaryValue;
  var noJavascriptAccess, Result: Boolean);
begin
  AssertCefThread;
  DoBeforeBrowse(targetUrl, frame.IsMain, True, True, Result);
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

procedure TframeCEFHost.cefSetFocus(Sender: TObject; const browser: ICefBrowser;
  source: TCefFocusSource; out Result: Boolean);
begin
  Result := source = FOCUS_SOURCE_NAVIGATION;
end;

procedure TframeCEFHost.cefTitleChange(Sender: TObject;
  const browser: ICefBrowser; const title: ustring);
var
  p: PCEFTitleChangeEventData;
begin
  AssertCefThread;

  p := AllocMem(SizeOf(TCEFTitleChangeEventData));
  p.browserid := browser.Identifier;
  p.title := title;

  PostMessage(FCallbackWnd, CEF_TITLECHANGE, 0, LPARAM(p));
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
