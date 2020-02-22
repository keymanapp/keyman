(*
  Name:             UfrmWebContainer
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      6 Oct 2006

  Modified Date:    15 Sep 2016
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          06 Oct 2006 - mcdurdin - Initial version
                    05 Dec 2006 - mcdurdin - Refactor XMLRenderer into this unit
                    12 Dec 2006 - mcdurdin - Size dialog according to locale.xml <Dialog> entry
                    04 Jan 2007 - mcdurdin - Add help support
                    22 Jan 2007 - mcdurdin - Add DialogName property
                    30 May 2007 - mcdurdin - Support F5 to refresh content
                    13 Jul 2007 - mcdurdin - I939 - Report IE script errors back to website
                    27 Mar 2008 - mcdurdin - Relocated
                    20 Jul 2008 - mcdurdin - I1553 - Report exceptions externally
                    28 Aug 2008 - mcdurdin - Don't crash on XML rendering error?
                    16 Jan 2009 - mcdurdin - I1792 - Fix crash on script error when file is missing
                    30 Jan 2009 - mcdurdin - I1829 - Close after a script error
                    29 Mar 2010 - mcdurdin - I2199 - Shift+click web browser
                    25 May 2010 - mcdurdin - I1694 - Select Keyman UI language rework
                    17 Dec 2010 - mcdurdin - I2570 - Use new E-mbeddedWB
                    18 Feb 2011 - mcdurdin - I2721 - Override Javascript-disabled security for web controls
                    28 Feb 2011 - mcdurdin - I2720 - Prevent Keyman Desktop splash from showing multiple copies (focus management for web browser)
                    18 Mar 2011 - mcdurdin - I2786 - Application title is sometimes incorrect
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    24 Jan 2012 - mcdurdin - I2992 - Script errors no longer automatically crash Keyman
                    06 Feb 2012 - mcdurdin - I2992 - Handle script errors more cleanly
                    04 Nov 2012 - mcdurdin - I3544 - V9.0 - Merge of I2992 - Handle script errors more cleanly
                    01 Jan 2013 - mcdurdin - I3710 - V9.0 - Script errors should encode program and version
                    27 Feb 2014 - mcdurdin - I4088 - V9.0 - Desktop dialogs can crash with stack overflow when F5 pressed
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
                    15 Sep 2016 - mcdurdin - I4989 - Download locale not working correctly
*)
unit UfrmWebContainer;  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UfrmKeymanBase,
  XMLRenderer, keymanapi_TLB, UserMessages, TempFileManager, Keyman.UI.UframeCEFHost;

type
  TfrmWebContainer = class(TfrmKeymanBase)
    procedure TntFormCreate(Sender: TObject);
    procedure TntFormDestroy(Sender: TObject);
  private
    FDialogName: WideString;
    FXMLRenderers: TXMLRenderers;
    FXMLFileName: TTempFile;
//    FNoMoreErrors: Boolean;   // I4181
    procedure WMUser_FormShown(var Message: TMessage); message WM_USER_FormShown;
    procedure WMUser_ContentRender(var Message: TMessage); message WM_USER_ContentRender;
    procedure DownloadUILanguages;
    procedure ContributeUILanguages;
  protected
    cef: TframeCEFHost;

    procedure cefLoadEnd(Sender: TObject); virtual;
    procedure cefBeforeBrowse(Sender: TObject; const Url: string; wasHandled: Boolean); virtual;
    procedure cefBeforeBrowseSync(Sender: TObject; const Url: string;
      out Handled: Boolean); virtual;  // I4989
    procedure cefPreKeySyncEvent(Sender: TObject; e: TCEFHostKeyEventData; out isShortcut, Handled: Boolean); virtual;
    procedure cefKeyEvent(Sender: TObject; e: TCEFHostKeyEventData; wasShortcut, wasHandled: Boolean); virtual;
    procedure cefCommand(Sender: TObject; const command: string;
      params: TStringList); virtual;
    procedure cefResizeFromDocument(Sender: TObject; awidth, aheight: Integer); virtual;
    procedure cefHelpTopic(Sender: TObject); virtual;

    procedure FireCommand(const command: WideString; params: TStringList); virtual;
    function ShouldProcessAllCommands: Boolean; virtual;
    function ShouldSetAppTitle: Boolean; virtual; // I2786
    function ShouldSetCaption: Boolean; virtual;

    procedure OpenLink(params: TStringList);
    procedure UILanguage(params: TStringList);


    procedure Content_Render(FRefreshKeyman: Boolean = False; const AdditionalData: WideString = ''); virtual;
    property XMLRenderers: TXMLRenderers read FXMLRenderers;
    procedure WndProc(var Message: TMessage); override;  // I2720
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetFocus; override;  // I2720
    property DialogName: WideString read FDialogName;


    procedure Do_Content_Render(FRefreshKeyman: Boolean); virtual;
  end;

procedure CreateForm(InstanceClass: TComponentClass; var Reference);

type
  TOnDownloadLocale = function(Owner: TForm): Boolean;

var
  FOnDownloadLocale: TOnDownloadLocale = nil;

implementation

uses
  uCEFConstants,
  uCEFInterfaces,
  uCEFTypes,

  custinterfaces,
  ErrorControlledRegistry,
  ExternalExceptionHandler,
  kmint,
  UILanguages,
  UfrmScriptError,
  Upload_Settings,
  utilexecute,
  utilsystem,
  utilhttp,
  utilxml,
  VersionInfo;

{$R *.dfm}

procedure CreateForm(InstanceClass: TComponentClass; var Reference);
begin
  Application.CreateForm(InstanceClass, Reference);
end;

procedure TfrmWebContainer.Content_Render(FRefreshKeyman: Boolean;
  const AdditionalData: WideString);
var
  FWidth, FHeight: Integer;
begin
  FreeAndNil(FXMLFileName);   // I4181

  try
    FXMLFileName := FXMLRenderers.RenderToFile(FRefreshKeyman, AdditionalData);
  except
    on E:EXMLRenderer do
    begin
      ShowMessage(E.Message);
      Exit;
    end;
  end;

  FDialogName := ChangeFileExt(ExtractFileName(FXMLRenderers.RenderTemplate), '');
  HelpType := htKeyword;
  HelpKeyword := FDialogName;

  FWidth := 0; FHeight := 0;
  //GetDialogParameters(FWidth, FHeight);
  kmint.KeymanCustomisation.CustMessages.GetDialogParameters(FDialogName, FWidth, FHeight);

  if (FWidth <> 0) and (FHeight <> 0) then
  begin
    ClientWidth := FWidth;
    ClientHeight := FHeight;
  end;

  cef.Navigate(FXMLFileName.Name);
end;

procedure TfrmWebContainer.Do_Content_Render(FRefreshKeyman: Boolean);
begin
  Content_Render(FRefreshKeyman);   // I4088
end;

constructor TfrmWebContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TfrmWebContainer.FireCommand(const command: WideString; params: TStringList);
begin
  if command = 'link' then OpenLink(params)
  else if command = 'uilanguage' then UILanguage(params)
  else if command = 'downloaduilanguages' then DownloadUILanguages
  else if command = 'contributeuilanguages' then ContributeUILanguages   // I4989
  else if command = 'resize' then cef.DoResizeByContent
  else ShowMessage(command + '?' + params.Text);
end;

procedure TfrmWebContainer.OpenLink(params: TStringList);
begin
  if not TUtilExecute.URL(params.Values['url']) then  // I3349
    ShowMessage(SysErrorMessage(GetLastError));
end;

procedure TfrmWebContainer.SetFocus;  // I2720
begin
  if Assigned(cef) then
    cef.SetFocus;
end;

function TfrmWebContainer.ShouldProcessAllCommands: Boolean;
begin
  Result := False;
end;

function TfrmWebContainer.ShouldSetAppTitle: Boolean; // I2786
begin
  Result := False;
end;

function TfrmWebContainer.ShouldSetCaption: Boolean;
begin
  Result := TRue;
end;

procedure TfrmWebContainer.TntFormCreate(Sender: TObject);
begin
  inherited;
  FXMLRenderers := TXMLRenderers.Create;

  cef := TframeCEFHost.Create(Self);
  cef.Parent := Self;
  cef.Visible := True;
  cef.ShouldOpenRemoteUrlsInBrowser := True;
  cef.OnBeforeBrowse := cefBeforeBrowse;
  cef.OnBeforeBrowseSync := cefBeforeBrowseSync;
  cef.OnCommand := cefCommand;
  cef.OnLoadEnd := cefLoadEnd;
  cef.OnKeyEvent := cefKeyEvent;
  cef.OnPreKeySyncEvent := cefPreKeySyncEvent;
  cef.OnResizeFromDocument := cefResizeFromDocument;
  cef.OnHelpTopic := cefHelpTopic;
end;

procedure TfrmWebContainer.cefBeforeBrowse(Sender: TObject; const Url: string; wasHandled: Boolean);
var
  aparams: TStringList;
  acommand: string;
begin
  if ShouldProcessAllCommands then
  begin
    GetParamsFromURLEx(URL, aparams);
    acommand := aparams[0];
    aparams.Delete(0);
    FireCommand(acommand, aparams);
    aparams.Free;
  end;
end;

procedure TfrmWebContainer.cefCommand(Sender: TObject; const command: string; params: TStringList);
begin
  FireCommand(command, params);
end;

procedure TfrmWebContainer.cefHelpTopic(Sender: TObject);
begin
  Application.HelpJump('context_'+lowercase(FDialogName));
end;

procedure TfrmWebContainer.cefBeforeBrowseSync(Sender: TObject; const Url: string; out Handled: Boolean);
var
  params: TStringList;
begin
  AssertCefThread;
  if ShouldProcessAllCommands then
  begin
    GetParamsFromURLEx(URL, params);
    Handled := True;
    params.Free;
  end
  else
    Handled := False;
end;

procedure TfrmWebContainer.cefKeyEvent(Sender: TObject; e: TCEFHostKeyEventData;
  wasShortcut, wasHandled: Boolean);
begin
  if e.event.kind in [KEYEVENT_RAWKEYDOWN, KEYEVENT_KEYDOWN] then
  begin
    if (e.event.windows_key_code = VK_F5) and ((e.event.modifiers and EVENTFLAG_CONTROL_DOWN) = EVENTFLAG_CONTROL_DOWN) then
      PostMessage(Handle, WM_USER_ContentRender, 0, 0)
    else if e.event.windows_key_code = VK_F1 then
      Application.HelpJump('context_'+lowercase(FDialogName));
  end;
end;

procedure TfrmWebContainer.TntFormDestroy(Sender: TObject);
begin
  inherited;
  FXMLRenderers.Free;
  FreeAndNil(FXMLFileName);   // I4181
end;

procedure TfrmWebContainer.DownloadUILanguages;
begin
  if Assigned(FOnDownloadLocale) then
    if FOnDownloadLocale(Self) then
      Do_Content_Render(True);
end;

procedure TfrmWebContainer.ContributeUILanguages;   // I4989
begin
  TUtilExecute.URL(MakeKeymanURL(URLPath_CreateTranslation));
end;

procedure TfrmWebContainer.UILanguage(params: TStringList);
var
  UILanguageID: WideString;
begin
  UILanguageID := params.Values['value'];
  kmint.KeymanCustomisation.CustMessages.LanguageCode := UILanguageID;
  Do_Content_Render(True);
end;

procedure TfrmWebContainer.cefLoadEnd(Sender: TObject);
begin
  AssertVclThread;
  cef.DoResizeByContent;
  Screen.Cursor := crDefault;
  if ShouldSetCaption then Self.Caption := cef.cef.Browser.MainFrame.Name;
  if ShouldSetAppTitle then Application.Title := Self.Caption;  // I2786
end;

procedure TfrmWebContainer.cefPreKeySyncEvent(Sender: TObject;
  e: TCEFHostKeyEventData; out isShortcut, Handled: Boolean);
begin
  if e.event.kind in [KEYEVENT_RAWKEYDOWN, KEYEVENT_KEYDOWN] then
    if (e.event.windows_key_code = VK_F5) and ((e.event.modifiers and EVENTFLAG_CONTROL_DOWN) = EVENTFLAG_CONTROL_DOWN) then
      Handled := True
    else if e.event.windows_key_code = VK_F1 then
      Handled := True;
end;

procedure TfrmWebContainer.cefResizeFromDocument(Sender: TObject; awidth,
  aheight: Integer);
begin
  ClientWidth := awidth;
  ClientHeight := aheight;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

function IsLocalURL(URL: WideString): Boolean;
begin
  Result := (Copy(URL, 1, 5) = 'file:') or (Copy(URL, 1, 1) = '/');
end;

procedure TfrmWebContainer.WMUser_ContentRender(var Message: TMessage);
begin
  Do_Content_Render(True);
end;

procedure TfrmWebContainer.WMUser_FormShown(var Message: TMessage);
begin
  if Assigned(cef) then
    cef.SetFocus;
end;

procedure TfrmWebContainer.WndProc(var Message: TMessage);  // I2720
begin
  inherited;

  with Message do
    case Msg of
      WM_SETFOCUS:
        if Assigned(cef) then
          cef.SetFocus;
      WM_ACTIVATE:
        begin
          if (Message.WParam <> 0) and Assigned(cef) then
            cef.SetFocus;
        end;
    end;
end;

end.
