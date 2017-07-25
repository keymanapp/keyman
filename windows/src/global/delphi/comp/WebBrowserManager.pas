(*
  Name:             WebBrowserManager
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      27 Mar 2008

  Modified Date:    1 May 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          27 Mar 2008 - mcdurdin - Initial version
                    29 Mar 2010 - mcdurdin - I2199 - Shift+click new window support
                    17 Dec 2010 - mcdurdin - I2570 - Use new EmbeddedWB
                    19 Jul 2011 - mcdurdin - I2984 - Floating point in Internet Explorer fix
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
*)
unit WebBrowserManager;  // I3306

interface

uses
  Windows, Messages, Types, Classes, EmbeddedWB, MSHTML_TLB,
  XMLRenderer, keymanapi_TLB, UserMessages, EwbCore,
  TempFileManager;

type
  TWebBrowserManager_FireCommandEvent = procedure(Sender: TObject; const command: WideString;
    params: TStringList) of object;

  TWebBrowserManager_SetTitleEvent = procedure(Sender: TObject; const Title: WideString) of object;

  TWebBrowserManager_ResizeEvent = procedure(Sender: TObject; X, Y: Integer) of object;

  TWebBrowserManager = class(TComponent)
  private
    web: TEmbeddedWB;
    FSafeWndHandle: Cardinal;
    FXMLFileName: TTempFile;   // I4181
    FXMLRenderers: TXMLRenderers;
    FOnFireCommand: TWebBrowserManager_FireCommandEvent;
    FDialogName: WideString;
    FOnResize: TWebBrowserManager_ResizeEvent;
    FOnSetTitle: TWebBrowserManager_SetTitleEvent;
    FOnScriptError: TScriptErrorEvent;
    FRenderTemplate: WideString;

    procedure webShowContextMenu(Sender: TCustomEmbeddedWB;
      const dwID: Cardinal; const ppt: PPoint; const CommandTarget: IInterface;
      const Context: IDispatch; var Result: HRESULT);
    procedure webKeyDown(Sender: TObject; var Key: Word; ScanCode: Word;
      Shift: TShiftState);
    procedure webScriptError(Sender: TObject; ErrorLine, ErrorCharacter,
      ErrorCode, ErrorMessage, ErrorUrl: string; var ScriptErrorAction: TScriptErrorAction);
    function webShowHelpRequest(Sender: TObject; HWND: NativeUInt;
      pszHelpFile: PWideChar; uCommand, dwData: Integer; ptMouse: TPoint;
      var pDispatchObjectHit: IDispatch): HRESULT;
    procedure webDocumentComplete(ASender: TObject; const pDisp: IDispatch;
      var URL: OleVariant);
    procedure webBeforeNavigate2(ASender: TObject; const pDisp: IDispatch;
      var URL, Flags, TargetFrameName, PostData, Headers: OleVariant;
      var Cancel: WordBool);
    procedure webNewWindow3(ASender: TObject; var ppDisp: IDispatch;
      var Cancel: WordBool; dwFlags: Cardinal; const bstrUrlContext,
      bstrUrl: WideString);

    procedure SetWebBrowser(const Value: TEmbeddedWB);
    procedure AddWebHooks;
    procedure RemoveWebHooks;

    procedure SafeWndProc(var Message: TMessage);
    procedure DeleteXMLFiles;
    procedure SetRenderTemplate(const Value: WideString);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure FireCommand(const command: WideString; params: TStringList); virtual;
    property XMLRenderers: TXMLRenderers read FXMLRenderers;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Content_Render(FRefreshKeyman: Boolean = False; const AdditionalData: WideString = '');
    function TemplateExists: Boolean;
    property DialogName: WideString read FDialogName;
  published
    property OnFireCommand: TWebBrowserManager_FireCommandEvent read FOnFireCommand write FOnFireCommand;
    property OnSetTitle: TWebBrowserManager_SetTitleEvent read FOnSetTitle write FOnSetTitle;
    property OnResize: TWebBrowserManager_ResizeEvent read FOnResize write FOnResize;
    property OnScriptError: TScriptErrorEvent read FOnScriptError write FOnScriptError;
    property WebBrowser: TEmbeddedWB read web write SetWebBrowser;
    property RenderTemplate: WideString read FRenderTemplate write SetRenderTemplate;
  end;

implementation

uses
  Forms,
  SHDocVw,
  SysUtils,
  utilhttp,
  utilsystem,
  WebSoundControl;

{ TWebBrowserManager }

constructor TWebBrowserManager.Create(AOwner: TComponent);
begin
  Set8087CW($133F);  // I2984
  inherited Create(AOwner);
  FSafeWndHandle := Classes.AllocateHWnd(SafeWndProc);
  FXMLRenderers := TXMLRenderers.Create;
  FXMLRenderers.RenderTemplate := FRenderTemplate;
end;

destructor TWebBrowserManager.Destroy;
begin
  Classes.DeallocateHWnd(FSafeWndHandle);
  if web <> nil then web.RemoveFreeNotification(Self);
  DeleteXMLFiles;
  FreeAndNil(FXMLRenderers);
  inherited Destroy;
end;

procedure TWebBrowserManager.FireCommand(const command: WideString;
  params: TStringList);
begin
  if Assigned(FOnFireCommand) then
    FOnFireCommand(Self, command, params);
end;

procedure TWebBrowserManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = web) and (Operation = opRemove) then
  begin
    RemoveWebHooks;
    web := nil;
  end;
  inherited;
end;

procedure TWebBrowserManager.SafeWndProc(var Message: TMessage);
var
  command: WideString;
  params: TStringList;
begin
  with Message do
    case Msg of
      WM_USER_FireCommand:
        begin
          params := TStringList(Message.LParam);
          command := params[0];
          params.Delete(0);
          FireCommand(command, params);
          params.Free;
        end;
      WM_USER_ContentRender:
        Content_Render;
    else
      Result := DefWindowProc(FSafeWndHandle, Msg, wParam, lParam);
    end;
end;

procedure TWebBrowserManager.SetRenderTemplate(const Value: WideString);
begin
  Assert(Assigned(FXMLRenderers));
  FRenderTemplate := Value;
  FXMLRenderers.RenderTemplate := FRenderTemplate;
end;

procedure TWebBrowserManager.SetWebBrowser(const Value: TEmbeddedWB);
begin
  if web <> nil then
  begin
    RemoveWebHooks;
    web.RemoveFreeNotification(Self);
  end;
  web := Value;
  if web <> nil then
  begin
    web.FreeNotification(Self);
    AddWebHooks;
  end;
end;

function TWebBrowserManager.TemplateExists: Boolean;
begin
  Result := FXMLRenderers.TemplateExists;
end;

procedure TWebBrowserManager.AddWebHooks;
begin
  if web <> nil then
  begin
    web.OnShowContextMenu := webShowContextMenu;
    web.OnKeyDown := webKeyDown;
    web.OnScriptError := webScriptError;
    web.OnShowHelpRequest := webShowHelpRequest;
    web.OnDocumentComplete := webDocumentComplete;
    web.OnBeforeNavigate2 := webBeforeNavigate2;
    web.OnNewWindow3 := webNewWindow3;
  end;
end;

procedure TWebBrowserManager.RemoveWebHooks;
begin
  if web <> nil then
  begin
    web.OnShowContextMenu := nil;
    web.OnKeyDown := nil;
    web.OnScriptError := nil;
    web.OnShowHelpRequest := nil;
    web.OnDocumentComplete := nil;
    web.OnBeforeNavigate2 := nil;
    web.OnNewWindow3 := nil;
  end;
end;

procedure TWebBrowserManager.webBeforeNavigate2(ASender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
var
  params: TStringList;
begin
  if GetParamsFromURL(URL, params) then
  begin
    PostMessage(FSafeWndHandle, WM_USER_FireCommand, 0, Integer(params));
    Cancel := True;
  end;
end;

procedure TWebBrowserManager.webDocumentComplete(ASender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
var
  doc2: IHTMLDocument2;
  doc3: IHTMLDocument3;
  elem: IHTMLElement;
begin
  try
    if Assigned(web.Document) then
    begin
      doc2 := (web.Document as IHTMLDocument2);
      doc3 := (web.Document as IHTMLDocument3);

      if Assigned(FOnSetTitle) then
        FOnSetTitle(Self, doc2.title);

      if Assigned(FOnResize) then
      begin
        elem := doc3.getElementById('size');
        if Assigned(elem) then
          FOnResize(Self, elem.offsetWidth, elem.offsetHeight);
      end;

{        if True then

        ClientWidth := elem.offsetWidth;
        ClientHeight := elem.offsetHeight;

        Left := (Screen.Width - Width) div 2;
        Top := (Screen.Height - Height) div 2;
      end;}

      elem := doc3.documentElement;
      if Assigned(elem) then
        elem.insertAdjacentHTML('afterBegin', '&#xa0;<SCRIPT For="window" Event="onerror">var noOp = null;</SCRIPT>');
    	// NOTE: The &nbsp, or some other visible HTML, is required. Internet Explorer will not
    	// parse and recognize the script block without some visual HTML to
    	// accompany it.
    end;
  except
    Exit;
  end;
  DeleteXMLFiles;   // I4181
end;

procedure TWebBrowserManager.webNewWindow3(ASender: TObject;
  var ppDisp: IDispatch; var Cancel: WordBool; dwFlags: Cardinal;
  const bstrUrlContext, bstrUrl: WideString);
var
  params: TStringList;
begin
  Cancel := True;
  if GetParamsFromURL(bstrURL, params)
    then PostMessage(FSafeWndHandle, WM_USER_FireCommand, 0, Integer(params))
    else web.Go(bstrURL);
end;

procedure TWebBrowserManager.webKeyDown(Sender: TObject; var Key: Word;
  ScanCode: Word; Shift: TShiftState);
begin
  if (Key = VK_F5) and (ssCtrl in Shift) then
  begin
    Key := 0;
    PostMessage(FSafeWndHandle, WM_USER_ContentRender, 0, 0);
  end;
end;

procedure TWebBrowserManager.webScriptError(Sender: TObject; ErrorLine,
  ErrorCharacter, ErrorCode, ErrorMessage, ErrorUrl: string; var ScriptErrorAction: TScriptErrorAction);
begin
  if Assigned(FOnScriptError) then
    FOnScriptError(Self, ErrorLine, ErrorCharacter, ErrorCode, ErrorMessage, ErrorUrl, ScriptErrorAction);
end;

procedure TWebBrowserManager.webShowContextMenu(Sender: TCustomEmbeddedWB;
  const dwID: Cardinal; const ppt: PPoint; const CommandTarget: IInterface;
  const Context: IDispatch; var Result: HRESULT);
begin
  PostMessage(web.ParentWindow, WM_CONTEXTMENU, web.Handle, MAKELONG(ppt.X, ppt.Y));
  Result := S_OK;
end;

function TWebBrowserManager.webShowHelpRequest(Sender: TObject; HWND: NativeUInt;
  pszHelpFile: PWideChar; uCommand, dwData: Integer; ptMouse: TPoint;
  var pDispatchObjectHit: IDispatch): HRESULT;
begin
  Application.HelpJump('context_'+lowercase(FDialogName));
  Result := S_OK;
end;

procedure TWebBrowserManager.Content_Render(FRefreshKeyman: Boolean; const AdditionalData: WideString);
var
  v: OleVariant;
begin
  DeleteXMLFiles;

  FXMLFileName := FXMLRenderers.RenderToFile(FRefreshKeyman, AdditionalData);

  FDialogName := ChangeFileExt(ExtractFileName(FXMLRenderers.RenderTemplate), '');

  v := navNoHistory or navNoReadFromCache or navNoWriteToCache;
  web.Navigate(FXMLFileName.Name, v);   // I4181
end;

procedure TWebBrowserManager.DeleteXMLFiles;
begin
  FreeAndNil(FXMLFileName);   // I4181
end;

end.
