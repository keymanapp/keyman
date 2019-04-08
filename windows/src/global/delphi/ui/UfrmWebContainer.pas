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
    procedure cefLoadEnd(Sender: TObject);
    procedure cefBeforeBrowse(Sender: TObject; const Url, command: string;
      params: TStringList; wasHandled: Boolean);
    procedure cefBeforeBrowseSync(Sender: TObject; const Url: string;
      out Handled: Boolean);   // I4989

  protected
    cef: TframeCEFHost;
    procedure DoResizeByContent;

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
  else if command = 'resize' then DoResizeByContent
  else ShowMessage(command + '?' + params.Text);
end;

procedure TfrmWebContainer.OpenLink(params: TStringList);
begin
  if not TUtilExecute.URL(params.Values['url']) then  // I3349
    ShowMessage(SysErrorMessage(GetLastError));
end;

procedure TfrmWebContainer.SetFocus;  // I2720
begin
//  if Assigned(cef) then
//    cef.SetFocus;
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
  cef.OnLoadEnd := cefLoadEnd;
end;

procedure TfrmWebContainer.cefBeforeBrowse(Sender: TObject; const Url, command: string; params: TStringList; wasHandled: Boolean);
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
  end
  else
  begin
    if params.Count > 0 then
      FireCommand(command, params);
  end;
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
  DoResizeByContent;
  Screen.Cursor := crDefault;
  if ShouldSetCaption then Self.Caption := cef.cef.Browser.MainFrame.Name;
  if ShouldSetAppTitle then Application.Title := Self.Caption;  // I2786
end;

{$MESSAGE HINT 'TODO: Support VK_F5 key event'}
{procedure TfrmWebContainer.webKeyDown(Sender: TObject; var Key: Word;
  ScanCode: Word; Shift: TShiftState);
begin
  if (Key = VK_F5) and (ssCtrl in Shift) then
  begin
    Key := 0;
    PostMessage(Handle, WM_USER_ContentRender, 0, 0);
  end;
end;}

procedure TfrmWebContainer.DoResizeByContent;
begin
  try
{$MESSAGE HINT 'TODO: Support window resizing'}
{    if Assigned(web.Document) then
    begin
      doc3 := (web.Document as IHTMLDocument3);

      elem := doc3.getElementById('size');
      if Assigned(elem) then
      begin
        ClientWidth := elem.offsetWidth;
        ClientHeight := elem.offsetHeight;

        Left := (Screen.Width - Width) div 2;
        Top := (Screen.Height - Height) div 2;

        (web.Document as IHTMLDocument2).parentWindow.scrollTo(0,0);
      end;
    end;
    }
  except
    Exit;
  end;
end;

function IsLocalURL(URL: WideString): Boolean;
begin
  Result := (Copy(URL, 1, 5) = 'file:') or (Copy(URL, 1, 1) = '/');
end;

{$MESSAGE HINT 'TODO: Log script errors'}
{procedure TfrmWebContainer.webScriptError2(Sender: TObject; ErrorLine,
  ErrorCharacter, ErrorCode, ErrorMessage, ErrorUrl: string;
  var ScriptErrorAction: TScriptErrorAction);
var
  FLog: string;
  FFileName: WideString;
  FTellKeymanSupport: Boolean;
  FAborting: Boolean;
begin
  FAborting := False;

  if FNoMoreErrors then
  begin
    ScriptErrorAction := eaContinue;
    Exit;
  end;

  case ShowScriptErrorDialog(Self, ErrorMessage, FTellKeymanSupport) of  // I2992   // I3544
    mrYes: ScriptErrorAction := eaContinue;
    mrCancel: begin ScriptErrorAction := eaContinue; FNoMoreErrors := True; end;
    mrNo:   ScriptErrorAction := eaCancel;
    mrAbort: begin ScriptErrorAction := eaCancel; FAborting := True; end;
  end;

  if FTellKeymanSupport then
  begin
    FLog := '';

    try
      with TStringList.Create do
      try
        FFileName := ConvertFileURLToPath(ErrorUrl);

        if FileExists(FFileName) then  // I1792
          LoadFromFile(FFileName);  // Use preamble encoding
        FLog := Text;
      finally
        Free;
      end;
    except
      on E:Exception do
        FLog := 'Exception '+E.Message+' trying to load '+FXMLFileName.Name+' for review';   // I4181
    end;

    LogExceptionToExternalHandler(
      ExtractFileName(ParamStr(0))+'_'+GetVersionString+'_script_'+Self.ClassName+'_'+ErrorCode,   // I3710
      'Error '+ErrorCode+' occurred at line '+ErrorLine+', character '+ErrorCharacter+' in '+ErrorUrl+', '+
      FXMLFileName.Name+#13#10+'Internet Explorer Version: '+GetIEVersionString+#13#10#13#10,   // I4181
      ErrorMessage, FLog);
  end;

  if FAborting then
    PostMessage(Handle, WM_CLOSE, 0, 0); // I1829, dead screen when uninstalling keyboard
  //Release;   // Disabled in I1792
  //PostMessage(Handle,
end;}

{$MESSAGE HINT 'TODO: Support context menu'}
{procedure TfrmWebContainer.webShowContextMenu2(Sender: TCustomEmbeddedWB;
  const dwID: Cardinal; const ppt: PPoint; const CommandTarget: IInterface;
  const Context: IDispatch; var Result: HRESULT);
begin
  Result := S_OK;
end;}

{$MESSAGE HINT 'TODO: Support context help'}
{function TfrmWebContainer.webShowHelpRequest1(Sender: TObject; HWND: NativeUInt;
  pszHelpFile: PWideChar; uCommand, dwData: Integer; ptMouse: TPoint;
  var pDispatchObjectHit: IDispatch): HRESULT;
begin
  Application.HelpJump('context_'+lowercase(FDialogName));
  Result := S_OK;
end;}

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
      WM_ACTIVATE, WM_SETFOCUS, WM_KILLFOCUS:
        begin
          if Assigned(cef) then
            cef.SetFocus;
        end;
    end;
end;

end.
