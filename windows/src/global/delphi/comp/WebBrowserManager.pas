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
                    17 Dec 2010 - mcdurdin - I2570 - Use new E-mbeddedWB
                    19 Jul 2011 - mcdurdin - I2984 - Floating point in Internet Explorer fix
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
*)
unit WebBrowserManager;  // I3306

interface

uses
  Windows, Messages, Types, Classes,
  XMLRenderer, keymanapi_TLB, UserMessages,
  Keyman.UI.UframeCEFHost,
  TempFileManager;

type
  TWebBrowserManager_FireCommandEvent = procedure(Sender: TObject; const command: WideString;
    params: TStringList) of object;

  TWebBrowserManager_SetTitleEvent = procedure(Sender: TObject; const Title: WideString) of object;

  TWebBrowserManager_ResizeEvent = procedure(Sender: TObject; X, Y: Integer) of object;

  TWebBrowserManager_ScriptErrorEvent = TNotifyEvent;

  TWebBrowserManager = class(TComponent)
  private
    cef: TframeCEFHost;
    FSafeWndHandle: Cardinal;
    FXMLFileName: TTempFile;   // I4181
    FXMLRenderers: TXMLRenderers;
    FOnFireCommand: TWebBrowserManager_FireCommandEvent;
    FDialogName: WideString;
    FOnResize: TWebBrowserManager_ResizeEvent;
    FOnSetTitle: TWebBrowserManager_SetTitleEvent;
    FOnScriptError: TWebBrowserManager_ScriptErrorEvent;
    FRenderTemplate: WideString;

    procedure SetWebBrowser(const Value: TframeCEFHost);
    procedure AddWebHooks;
    procedure RemoveWebHooks;

    procedure SafeWndProc(var Message: TMessage);
    procedure DeleteXMLFiles;
    procedure SetRenderTemplate(const Value: WideString);

    procedure cefBeforeBrowse(Sender: TObject; const Url, command: string; params: TStringList; wasHandled: Boolean);
    procedure cefLoadEnd(Sender: TObject);
    procedure cefPreKeySyncEvent(Sender: TObject; e: TCEFHostKeyEventData; out isShortcut, Handled: Boolean);
    procedure cefKeyEvent(Sender: TObject; e: TCEFHostKeyEventData; wasShortcut, wasHandled: Boolean);
    procedure cefTitleChange(Sender: TObject; const title: string);
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
    property OnScriptError: TWebBrowserManager_ScriptErrorEvent read FOnScriptError write FOnScriptError;
    property WebBrowser: TframeCEFHost read cef write SetWebBrowser;
    property RenderTemplate: WideString read FRenderTemplate write SetRenderTemplate;
  end;

implementation

uses
  Forms,
  SysUtils,
  uCEFConstants,
  uCEFTypes,
  utilhttp,
  utilsystem;

{ TWebBrowserManager }

constructor TWebBrowserManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSafeWndHandle := Classes.AllocateHWnd(SafeWndProc);
  FXMLRenderers := TXMLRenderers.Create;
  FXMLRenderers.RenderTemplate := FRenderTemplate;
end;

destructor TWebBrowserManager.Destroy;
begin
  Classes.DeallocateHWnd(FSafeWndHandle);
  if cef <> nil then cef.RemoveFreeNotification(Self);
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
  if (AComponent = cef) and (Operation = opRemove) then
  begin
    RemoveWebHooks;
    cef := nil;
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

procedure TWebBrowserManager.SetWebBrowser(const Value: TframeCEFHost);
begin
  if cef <> nil then
  begin
    RemoveWebHooks;
    cef.RemoveFreeNotification(Self);
  end;
  cef := Value;
  if cef <> nil then
  begin
    cef.FreeNotification(Self);
    AddWebHooks;
  end;
end;

function TWebBrowserManager.TemplateExists: Boolean;
begin
  Result := FXMLRenderers.TemplateExists;
end;

procedure TWebBrowserManager.AddWebHooks;
begin
  if cef <> nil then
  begin
    cef.OnBeforeBrowse := cefBeforeBrowse;
    cef.OnLoadEnd := cefLoadEnd;
    cef.OnKeyEvent := cefKeyEvent;
    cef.OnPreKeySyncEvent := cefPreKeySyncEvent;
    cef.OnTitleChange := cefTitleChange;
  end;
  {$MESSAGE HINT 'TODO: Support OnShowContextMenu, OnScriptError'}
  {if web <> nil then
  begin
    web.OnShowContextMenu := webShowContextMenu;
    web.OnScriptError := webScriptError;
  end;}
end;

procedure TWebBrowserManager.RemoveWebHooks;
begin
  if cef <> nil then
  begin
    cef.OnBeforeBrowse := nil;
    cef.OnLoadEnd := nil;
    cef.OnKeyEvent := nil;
    cef.OnPreKeySyncEvent := nil;
    cef.OnTitleChange := nil;
  end;
  {if web <> nil then
  begin
    web.OnShowContextMenu := nil;
    web.OnScriptError := nil;
  end;}
end;

{procedure TWebBrowserManager.webScriptError(Sender: TObject; ErrorLine,
  ErrorCharacter, ErrorCode, ErrorMessage, ErrorUrl: string; var ScriptErrorAction: TScriptErrorAction);
begin
  if Assigned(FOnScriptError) then
    FOnScriptError(Self, ErrorLine, ErrorCharacter, ErrorCode, ErrorMessage, ErrorUrl, ScriptErrorAction);
end;}


{procedure TWebBrowserManager.webShowContextMenu(Sender: TCustomEmbeddedWB;
  const dwID: Cardinal; const ppt: PPoint; const CommandTarget: IInterface;
  const Context: IDispatch; var Result: HRESULT);
begin
  PostMessage(web.ParentWindow, WM_CONTEXTMENU, web.Handle, MAKELONG(ppt.X, ppt.Y));
  Result := S_OK;
end;}

procedure TWebBrowserManager.cefBeforeBrowse(Sender: TObject; const Url,
  command: string; params: TStringList; wasHandled: Boolean);
begin
  if command <> '' then
  begin
    FireCommand(command, params);
  end;
end;

procedure TWebBrowserManager.cefKeyEvent(Sender: TObject;
  e: TCEFHostKeyEventData; wasShortcut, wasHandled: Boolean);
begin
  // VCL Thread
  if e.event.kind in [KEYEVENT_RAWKEYDOWN, KEYEVENT_KEYDOWN] then
  begin
    if (e.event.windows_key_code = VK_F5) and ((e.event.modifiers and EVENTFLAG_CONTROL_DOWN) = EVENTFLAG_CONTROL_DOWN) then
      PostMessage(FSafeWndHandle, WM_USER_ContentRender, 0, 0)
    else if e.event.windows_key_code = VK_F1 then
      Application.HelpJump('context_'+lowercase(FDialogName));
  end;
end;

procedure TWebBrowserManager.cefPreKeySyncEvent(Sender: TObject;
  e: TCEFHostKeyEventData; out isShortcut, Handled: Boolean);
begin
  // CEF thread
  Handled := False;

  if e.event.kind in [KEYEVENT_RAWKEYDOWN, KEYEVENT_KEYDOWN] then
    if (e.event.windows_key_code = VK_F5) and ((e.event.modifiers and EVENTFLAG_CONTROL_DOWN) = EVENTFLAG_CONTROL_DOWN) then
      Handled := True
    else if e.event.windows_key_code = VK_F1 then
      Handled := True;
end;

procedure TWebBrowserManager.cefTitleChange(Sender: TObject;
  const title: string);
begin
  if Assigned(FOnSetTitle) then
    FOnSetTitle(Self, title);
end;

procedure TWebBrowserManager.cefLoadEnd(Sender: TObject);
begin
{$MESSAGE HINT 'TODO: Support resizing'}
  DeleteXMLFiles;
{
      if Assigned(FOnResize) then
      begin
        elem := doc3.getElementById('size');
        if Assigned(elem) then
          FOnResize(Self, elem.offsetWidth, elem.offsetHeight);
      end;
}
end;

procedure TWebBrowserManager.Content_Render(FRefreshKeyman: Boolean; const AdditionalData: WideString);
begin
  DeleteXMLFiles;

  FXMLFileName := FXMLRenderers.RenderToFile(FRefreshKeyman, AdditionalData);

  FDialogName := ChangeFileExt(ExtractFileName(FXMLRenderers.RenderTemplate), '');

  cef.Navigate(FXMLFileName.Name);   // I4181
end;

procedure TWebBrowserManager.DeleteXMLFiles;
begin
  FreeAndNil(FXMLFileName);   // I4181
end;

end.
