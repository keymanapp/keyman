unit UfrmHelp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AppEvnts, StdCtrls, ActnList,
  UfrmTike,
  Xml.XMLDoc,
  Xml.XMLIntf,

  TempFileManager, UfrmTikeDock,
  System.Actions, JvComponentBase, JvDockControlForm, uCEFWindowParent,
  uCEFInterfaces, uCEFTypes,
  uCEFChromiumWindow, Vcl.ExtCtrls;

type
  TfrmHelp = class(TTIKEDockForm) // I2721
    ActionList1: TActionList;
    actHelpContextRefresh: TAction;
    cef: TChromiumWindow;
    tmrCreateBrowser: TTimer;
    procedure actHelpContextRefreshUpdate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrCreateBrowserTimer(Sender: TObject);
    procedure cefAfterCreated(Sender: TObject);
    procedure cefBeforeClose(Sender: TObject);
    procedure cefClose(Sender: TObject);
  private
    FRefreshQueued: Boolean;
    FHelpControl: TWinControl;
    FDocumentLoaded: Boolean;
    FHelpMissingFile: IXMLDocument;
    FHelpFileName: string;
    FHMFRoot: IXMLNode;
    FTempFile: TTempFile;
//    procedure AddUnmatchedContext(FormName, ControlName: string);
//    procedure DeleteMatchedContext(FormName, ControlName: string);
    procedure cefLoadEnd(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; httpStatusCode: Integer);
    procedure cefPreKeyEvent(Sender: TObject; const browser: ICefBrowser;
      const event: PCefKeyEvent; osEvent: TCefEventHandle;
      out isKeyboardShortcut, Result: Boolean);
    procedure cefBeforePopup(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
      targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
      const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess, Result: Boolean);
    procedure cefBeforeBrowse(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest;
      user_gesture, isRedirect: Boolean; out Result: Boolean);
    procedure cefRunContextMenu(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; const params: ICefContextMenuParams;
      const model: ICefMenuModel; const callback: ICefRunContextMenuCallback;
      var aResult: Boolean);

    procedure CreateBrowser;

    // CEF: You have to handle this two messages to call NotifyMoveOrResizeStarted or some page elements will be misaligned.
    procedure WMMove(var aMessage: TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage: TMessage); message WM_MOVING;
    // CEF: You also have to handle these two messages to set GlobalCEFApp.OsmodalLoop
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
  protected
    function GetHelpTopic: string; override;
  public
    procedure LoadHelp(ControlName, FormName: string);
    procedure StartClose;
    procedure QueueRefresh;
  end;

var
  frmHelp: TfrmHelp;

implementation

{$R *.dfm}

uses
  System.Types,
  Winapi.ShlObj,

  Keyman.Developer.System.HelpTopics,

  RedistFiles,
  RegistryKeys,
  uCEFApplication,
  UframeTextEditor,
  UfrmMain,
  UmodWebHTTPServer,
  utilsystem;

(*procedure TfrmHelp.AddUnmatchedContext(FormName, ControlName: string);
var
  i: Integer;
  n: IXMLNode;
begin
  for i := 0 to FHMFRoot.ChildNodes.Count - 1 do
    if (FHMFRoot.ChildNodes[i].Attributes['FormName'] = FormName) and
      (FHMFRoot.ChildNodes[i].Attributes['ControlName'] = ControlName) then
    begin
      FHMFRoot.ChildNodes[i].Attributes['LastVisited'] := Now;
      if FHMFRoot.ChildNodes[i].AttributeNodes.IndexOf('VisitCount') < 0 then
        FHMFRoot.ChildNodes[i].Attributes['VisitCount'] := 1
      else
        FHMFRoot.ChildNodes[i].Attributes['VisitCount'] := FHMFRoot.ChildNodes
          [i].Attributes['VisitCount'] + 1;
      Exit;
    end;

  n := FHMFRoot.AddChild('MissingTopic');
  n.Attributes['FormName'] := FormName;
  n.Attributes['ControlName'] := ControlName;
  n.Attributes['LastVisited'] := Now;
  n.Attributes['VisitCount'] := 1;
end;

procedure TfrmHelp.DeleteMatchedContext(FormName, ControlName: string);
var
  i: Integer;
begin
  for i := 0 to FHMFRoot.ChildNodes.Count - 1 do
    if (FHMFRoot.ChildNodes[i].Attributes['FormName'] = FormName) and
      (FHMFRoot.ChildNodes[i].Attributes['ControlName'] = ControlName) then
    begin
      FHMFRoot.ChildNodes.Delete(i);
      Exit;
    end;
end;*)

procedure TfrmHelp.actHelpContextRefreshUpdate(Sender: TObject);
var
  FormName, ControlName: string;
  o: TComponent;
begin
  if not FDocumentLoaded then
    Exit;

  if (Screen.ActiveControl <> FHelpControl) or FRefreshQueued then
    try
      FormName := '';
      ControlName := '';

      if Screen.ActiveControl <> nil then
      begin
        // Don't get help document focus
        if (Screen.ActiveControl = Self) or (Screen.ActiveControl.Owner = Self)
        then
        begin
          if not FRefreshQueued then
            Exit;
        end
        else
          FHelpControl := Screen.ActiveControl;

        if FHelpControl is TTIKEForm then
          FormName := (FHelpControl as TTIKEForm).HelpTopic
        else if FHelpControl is TCustomForm then
          FormName := FHelpControl.ClassName
        else
        begin
          o := FHelpControl.Owner;
          while not(o is TTIKEForm) and (o <> nil) do
            o := o.Owner;

          if o is TTIKEForm then
            FormName := (o as TTIKEForm).HelpTopic
          else if FHelpControl.Owner <> nil then
            FormName := FHelpControl.Owner.ClassName;

          ControlName := FHelpControl.Name;
        end;
      end;

      LoadHelp(ControlName, FormName);
    finally
      FRefreshQueued := False;
    end;
end;

procedure TfrmHelp.LoadHelp(ControlName, FormName: string);
begin
  if FormName = 'TfrmHelp' then
    Exit; // I2823

  // Call into the web browser control.
  try
    // TODO: record unmatched context
    cef.ChromiumBrowser.ExecuteJavaScript('ActivatePage("' + FormName + '", "' +
      ControlName + '")', '');
    {
      elem := doc3.getElementById(FormName+'-'+ControlName);
      if elem = nil
      then AddUnmatchedContext(FormName, ControlName)
      else DeleteMatchedContext(FormName, ControlName);
    }
  except
    ;
  end;
end;

procedure TfrmHelp.QueueRefresh;
begin
  FRefreshQueued := True;
end;

procedure TfrmHelp.StartClose;
begin
  cef.Visible := False;
  cef.CloseBrowser(True);
end;

procedure TfrmHelp.tmrCreateBrowserTimer(Sender: TObject);
begin
  tmrCreateBrowser.Enabled := False;
  CreateBrowser;
end;

procedure TfrmHelp.FormCreate(Sender: TObject);
begin
  inherited;
  FTempFile := nil;
  FHelpFileName := GetFolderPath(CSIDL_APPDATA) + SFolderKeymanDeveloper +
    '\helpmissing.xml';
  ForceDirectories(ExtractFileDir(FHelpFileName));
  if FileExists(FHelpFileName) then
  begin
    FHelpMissingFile := LoadXMLDocument(FHelpFileName);
    FHMFRoot := FHelpMissingFile.ChildNodes['MissingTopics'];
  end
  else
  begin
    FHelpMissingFile := NewXMLDocument;
    FHMFRoot := FHelpMissingFile.AddChild('MissingTopics');
  end;

  cef.ChromiumBrowser.OnBeforeBrowse := cefBeforeBrowse;
  cef.ChromiumBrowser.OnBeforePopup := cefBeforePopup;
  cef.ChromiumBrowser.OnRunContextMenu := cefRunContextMenu;
  cef.ChromiumBrowser.OnLoadEnd := cefLoadEnd;
  cef.ChromiumBrowser.OnPreKeyEvent := cefPreKeyEvent;
end;

procedure TfrmHelp.CreateBrowser;
begin
  tmrCreateBrowser.Enabled := not cef.CreateBrowser;
end;

procedure TfrmHelp.FormDestroy(Sender: TObject);
begin
  FHelpMissingFile.SaveToFile(FHelpFileName);
  FreeAndNil(FTempFile);
end;

procedure TfrmHelp.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;
  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := True;
end;

procedure TfrmHelp.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;
  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := False;
end;

procedure TfrmHelp.WMMove(var aMessage: TWMMove);
begin
  inherited;
  if cef <> nil then
    cef.NotifyMoveOrResizeStarted;
end;

procedure TfrmHelp.WMMoving(var aMessage: TMessage);
begin
  inherited;
  if cef <> nil then
    cef.NotifyMoveOrResizeStarted;
end;

procedure TfrmHelp.FormShow(Sender: TObject);
begin
  inherited;
//  CreateBrowser;
end;

procedure TfrmHelp.cefAfterCreated(Sender: TObject);
begin
  FDocumentLoaded := False;
  cef.LoadURL(modWebHttpServer.GetAppURL('help/'));
end;

function TfrmHelp.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_Help;
end;

procedure TfrmHelp.cefBeforeClose(Sender: TObject);
begin
  Close;
end;

procedure TfrmHelp.cefClose(Sender: TObject);
begin
  // DestroyChildWindow will destroy the child window created by CEF at the top of the Z order.
  if not cef.DestroyChildWindow then
  begin
    Close;
  end;
end;

procedure TfrmHelp.cefBeforeBrowse(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  user_gesture, isRedirect: Boolean; out Result: Boolean);
var
  frm: TTIKEForm;
begin
  Result := False;
  if Copy(request.Url, 1, 5) = 'help:' then
  begin
    Result := True;
    if FHelpControl is TTIKEForm then
      frm := FHelpControl as TTIKEForm
    else if FHelpControl.Owner is TTIKEForm then
      frm := FHelpControl.Owner as TTIKEForm
    else
      frm := nil;

    if frm <> nil then
      frmKeymanDeveloper.HelpTopic(frm.HelpTopic)
    else
      frmKeymanDeveloper.HelpTopic('index')
  end;
end;

procedure TfrmHelp.cefLoadEnd(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer);
begin
  if csDestroying in ComponentState then
    Exit;
  FDocumentLoaded := True;
  QueueRefresh;
end;

procedure TfrmHelp.cefPreKeyEvent(Sender: TObject; const browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle;
  out isKeyboardShortcut, Result: Boolean);
begin
  Result := False;
  if (event.windows_key_code <> VK_CONTROL) and
    (event.kind in [TCefKeyEventType.KEYEVENT_KEYDOWN,
    TCefKeyEventType.KEYEVENT_RAWKEYDOWN]) then
  begin
    if event.windows_key_code = VK_F1 then
    begin
      isKeyboardShortcut := True;
      Result := True;
      frmKeymanDeveloper.HelpTopic(Self);
    end
    else if event.windows_key_code = VK_F12 then
    begin
      cef.ChromiumBrowser.ShowDevTools(Point(Low(Integer), Low(Integer)), nil);
    end
    else if SendMessage(Application.Handle, CM_APPKEYDOWN,
      event.windows_key_code, 0) = 1 then
    begin
      isKeyboardShortcut := True;
      Result := True;
    end;
  end;
end;

procedure TfrmHelp.cefBeforePopup(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
  const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var client: ICefClient; var settings: TCefBrowserSettings;
  var noJavascriptAccess, Result: Boolean);
var
  FormName: string;
begin
  Result := True;
  if Copy(targetUrl, 1, 5) = 'help:' then
  begin
    if FHelpControl is TCustomForm then
      FormName := (FHelpControl as TCustomForm).ClassName
    else if FHelpControl.Owner <> nil then
      FormName := FHelpControl.Owner.ClassName
    else
      FormName := '';
    if FormName = '' then
      frmKeymanDeveloper.HelpTopic('index')
    else
      frmKeymanDeveloper.HelpTopic('context_' + Copy(FormName, 2, MAXINT));
  end
  else
    cef.LoadURL(targetUrl);
end;

procedure TfrmHelp.cefRunContextMenu(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel;
  const callback: ICefRunContextMenuCallback; var aResult: Boolean);
begin
  aResult := GetKeyState(VK_SHIFT) >= 0;
end;

end.
