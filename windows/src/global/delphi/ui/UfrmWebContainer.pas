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
                    28 Feb 2011 - mcdurdin - I2720 - Prevent Keyman splash from showing multiple copies (focus management for web browser)
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
  keymanapi_TLB, UserMessages, Keyman.UI.UframeCEFHost;

type
  TfrmWebContainer = class(TfrmKeymanBase)
    procedure TntFormCreate(Sender: TObject);
  private
    FDialogName: WideString;
    procedure WMUser_FormShown(var Message: TMessage); message WM_USER_FormShown;
    procedure WMUser_ContentRender(var Message: TMessage); message WM_USER_ContentRender;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;

    procedure ContributeUILanguages;
  protected
    cef: TframeCEFHost;
    FRenderPage: string;

    procedure cefTitleChange(Sender: TObject; const title: string); virtual;
    procedure cefLoadEnd(Sender: TObject); virtual;
    procedure cefPreKeySyncEvent(Sender: TObject; e: TCEFHostKeyEventData; out isShortcut, Handled: Boolean); virtual;
    procedure cefKeyEvent(Sender: TObject; e: TCEFHostKeyEventData; wasShortcut, wasHandled: Boolean); virtual;
    procedure cefCommand(Sender: TObject; const command: string;
      params: TStringList); virtual;
    procedure cefResizeFromDocument(Sender: TObject; awidth, aheight: Integer); virtual;
    procedure cefHelpTopic(Sender: TObject); virtual;

    procedure FireCommand(const command: WideString; params: TStringList); virtual;
    function ShouldSetAppTitle: Boolean; virtual; // I2786
    function ShouldSetCaption: Boolean; virtual;

    procedure OpenLink(params: TStringList);
    procedure UILanguage(params: TStringList);

    function IsLocalUrl(const url: string): Boolean;

    procedure Content_Render(const Query: string = ''); virtual;
    procedure WndProc(var Message: TMessage); override;  // I2720

    procedure DoOpenHelp;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetFocus; override;  // I2720
    property DialogName: WideString read FDialogName;


    procedure Do_Content_Render; virtual;
  end;

procedure CreateForm(InstanceClass: TComponentClass; var Reference);

implementation

uses
  System.StrUtils,

  uCEFConstants,
  uCEFInterfaces,
  uCEFTypes,

  custinterfaces,
  ErrorControlledRegistry,
  Keyman.Configuration.System.UmodWebHttpServer,
  Keyman.System.KeymanSentryClient,
  kmint,
  UILanguages,
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

procedure TfrmWebContainer.Content_Render(const Query: string);
var
  FWidth, FHeight: Integer;
begin
  FDialogName := FRenderPage;
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

  cef.Navigate(modWebHttpServer.Host + '/page/'+FRenderPage+IfThen(Query='','','?'+Query));
end;

procedure TfrmWebContainer.Do_Content_Render;
begin
  Content_Render;   // I4088
end;

constructor TfrmWebContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TfrmWebContainer.FireCommand(const command: WideString; params: TStringList);
begin
  if command = 'link' then OpenLink(params)
  else if command = 'uilanguage' then UILanguage(params)
  else if command = 'contributeuilanguages' then ContributeUILanguages   // I4989
  else if command = 'resize' then cef.DoResizeByContent
  else ShowMessage(command + '?' + params.Text);
end;

/// <summary>Returns true if url is from the local render server</summary>
function TfrmWebContainer.IsLocalUrl(const url: string): Boolean;
begin
  Result := url.StartsWith(modWebHttpServer.Host, True);
end;

procedure TfrmWebContainer.DoOpenHelp;
begin
  if HelpTopic = ''
    then Application.HelpJump('context/'+lowercase(FDialogName))
    else Application.HelpJump(HelpTopic);
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

function TfrmWebContainer.ShouldSetAppTitle: Boolean; // I2786
begin
  Result := False;
end;

function TfrmWebContainer.ShouldSetCaption: Boolean;
begin
  Result := True;
end;

procedure TfrmWebContainer.TntFormCreate(Sender: TObject);
begin
  inherited;
  cef := TframeCEFHost.Create(Self);
  cef.Parent := Self;
  cef.Visible := True;
  cef.ShouldOpenRemoteUrlsInBrowser := True;
  cef.OnCommand := cefCommand;
  cef.OnLoadEnd := cefLoadEnd;
  cef.OnKeyEvent := cefKeyEvent;
  cef.OnPreKeySyncEvent := cefPreKeySyncEvent;
  cef.OnResizeFromDocument := cefResizeFromDocument;
  cef.OnHelpTopic := cefHelpTopic;
  cef.OnTitleChange := cefTitleChange;
end;

procedure TfrmWebContainer.cefCommand(Sender: TObject; const command: string; params: TStringList);
begin
  TKeymanSentryClient.Breadcrumb('user', 'Clicked '+command+' in '+ClassName, 'click');
  FireCommand(command, params);
end;

procedure TfrmWebContainer.cefHelpTopic(Sender: TObject);
begin
  DoOpenHelp;
end;

procedure TfrmWebContainer.cefKeyEvent(Sender: TObject; e: TCEFHostKeyEventData;
  wasShortcut, wasHandled: Boolean);
begin
  if e.event.kind in [KEYEVENT_RAWKEYDOWN, KEYEVENT_KEYDOWN] then
  begin
    if (e.event.windows_key_code = VK_F5) and ((e.event.modifiers and EVENTFLAG_CONTROL_DOWN) = EVENTFLAG_CONTROL_DOWN) then
      PostMessage(Handle, WM_USER_ContentRender, 0, 0)
    else if e.event.windows_key_code = VK_F1 then
      DoOpenHelp;
  end;
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
  Do_Content_Render;
end;

procedure TfrmWebContainer.cefLoadEnd(Sender: TObject);
begin
  AssertVclThread;
  cef.DoResizeByContent;
  Screen.Cursor := crDefault;
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

procedure TfrmWebContainer.cefTitleChange(Sender: TObject; const title: string);
begin
  if ShouldSetCaption then Self.Caption := title;
  if ShouldSetAppTitle then Application.Title := Self.Caption;  // I2786
end;

function IsLocalURL(URL: WideString): Boolean;
begin
  Result := (Copy(URL, 1, 5) = 'file:') or (Copy(URL, 1, 1) = '/');
end;

procedure TfrmWebContainer.WMSysCommand(var Message: TWMSysCommand);
begin
  with Message do
  begin
    if (CmdType and $FFF0 = SC_CONTEXTHELP)
      then DoOpenHelp
      else inherited;
  end;
end;

procedure TfrmWebContainer.WMUser_ContentRender(var Message: TMessage);
begin
  Do_Content_Render;
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
