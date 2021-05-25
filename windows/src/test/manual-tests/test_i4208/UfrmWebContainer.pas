(*
  Name:             UfrmWebContainer
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      6 Oct 2006

  Modified Date:    27 Feb 2014
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
                    13 Jul 2007 - mcdurdin - I939 - Report IE script errors back to Tavultesoft
                    27 Mar 2008 - mcdurdin - Relocated
                    20 Jul 2008 - mcdurdin - I1553 - Report exceptions externally
                    28 Aug 2008 - mcdurdin - Don't crash on XML rendering error?
                    16 Jan 2009 - mcdurdin - I1792 - Fix crash on script error when file is missing
                    30 Jan 2009 - mcdurdin - I1829 - Close after a script error
                    29 Mar 2010 - mcdurdin - I2199 - Shift+click web browser
                    25 May 2010 - mcdurdin - I1694 - Select Keyman UI language rework
                    17 Dec 2010 - mcdurdin - I2570 - Use new EmbeddedWB
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
*)
unit UfrmWebContainer;  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UfrmKeymanBase, OleCtrls, SHDocVw, EmbeddedWB,
  IeConst, SHDocVw_EWB, EwbCore,
  KeymanEmbeddedWB;

type
  TfrmWebContainer = class(TfrmKeymanBase)
    web: TKeymanEmbeddedWB;
    procedure webDocumentComplete(ASender: TObject; const pDisp: IDispatch;
      var URL: OleVariant);
    procedure webNewWindow3(ASender: TObject; var ppDisp: IDispatch;
      var Cancel: WordBool; dwFlags: Cardinal; const bstrUrlContext,
      bstrUrl: WideString);
    procedure webShowContextMenu2(Sender: TCustomEmbeddedWB;
      const dwID: Cardinal; const ppt: PPoint; const CommandTarget: IInterface;
      const Context: IDispatch; var Result: HRESULT);
    function webShowHelpRequest1(Sender: TObject; HWND: NativeUInt;
      pszHelpFile: PWideChar; uCommand, dwData: Integer; ptMouse: TPoint;
      var pDispatchObjectHit: IDispatch): HRESULT;
  private
    FDialogName: WideString;
    procedure WMUser_FormShown(var Message: TMessage); message WM_USER_FormShown;
    function GetIEVersionString: WideString;
    procedure DownloadUILanguages;

  protected
    procedure DoResizeByContent;

    procedure FireCommand(const command: WideString; params: TStringList); virtual;
    function ShouldProcessAllCommands: Boolean; virtual;
    function ShouldSetAppTitle: Boolean; virtual; // I2786

    procedure OpenLink(params: TStringList);
    procedure UILanguage(params: TStringList);


    procedure Content_Render(FRefreshKeyman: Boolean = False; const AdditionalData: WideString = '');
    procedure WndProc(var Message: TMessage); override;  // I2720
  public
    constructor Create(AOwner: TComponent); overload; override;
    procedure SetFocus; override;  // I2720
    property DialogName: WideString read FDialogName;


    procedure Do_Content_Render(FRefreshKeyman: Boolean); virtual;
  end;

type
  TOnDownloadLocale = function(Owner: TForm): Boolean;

var
  FOnDownloadLocale: TOnDownloadLocale = nil;

implementation

uses
  mshtml;

{$R *.dfm}

procedure TfrmWebContainer.Content_Render(FRefreshKeyman: Boolean;
  const AdditionalData: WideString);
var
  FWidth, FHeight: Integer;
  v: OleVariant;
begin
  HelpType := htKeyword;
  HelpKeyword := FDialogName;

  //FWidth := 333; FHeight := 666;
  //GetDialogParameters(FWidth, FHeight);

{  if (FWidth <> 0) and (FHeight <> 0) then
  begin
    ClientWidth := FWidth;
    ClientHeight := FHeight;
  end;}

  v := navNoHistory or navNoReadFromCache or navNoWriteToCache;
  web.Navigate('http://www.google.com/', v);
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
  else if command = 'resize' then DoResizeByContent
  else ShowMessage(command + '?' + params.Text);
end;

procedure TfrmWebContainer.OpenLink(params: TStringList);
begin
  ShowMessage(SysErrorMessage(GetLastError));
end;

procedure TfrmWebContainer.SetFocus;  // I2720
begin
  web.SetFocusToDoc;
end;

function TfrmWebContainer.ShouldProcessAllCommands: Boolean;
begin
  Result := False;
end;

function TfrmWebContainer.ShouldSetAppTitle: Boolean; // I2786
begin
  Result := False;
end;

procedure TfrmWebContainer.DownloadUILanguages;
begin
  if Assigned(FOnDownloadLocale) then
    if FOnDownloadLocale(Self) then
      Do_Content_Render(True);
end;

procedure TfrmWebContainer.UILanguage(params: TStringList);
var
  UILanguageID: WideString;
begin
  UILanguageID := params.Values['value'];
  Do_Content_Render(True);
end;

procedure TfrmWebContainer.webDocumentComplete(ASender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
var
  doc2: IHTMLDocument2;
  doc3: IHTMLDocument3;
  elem: IHTMLElement;
begin
  try
    //if (pDisp as IWebBrowser2).TopLevelContainer then
    //begin
      DoResizeByContent;

      Screen.Cursor := crDefault;

      doc2 := (web.Document as IHTMLDocument2);
      Self.Caption := doc2.title;
      if ShouldSetAppTitle then Application.Title := Self.Caption;  // I2786

      doc3 := (web.Document as IHTMLDocument3);

      elem := doc3.documentElement;
      if Assigned(elem) then  // MS KB 317024
        elem.insertAdjacentHTML('afterBegin', '&#xa0;<SCRIPT For="window" Event="onerror">var noOp = null;</SCRIPT>');
      // NOTE: The &nbsp, or some other visible HTML, is required. Internet Explorer will not
      // parse and recognize the script block without some visual HTML to
      // accompany it.
    //end;
  except
    Exit;
  end;
end;

procedure TfrmWebContainer.DoResizeByContent;
var
  doc3: IHTMLDocument3;
  elem: TSize;
begin
  try
    if Assigned(web.Document) then
    begin
      doc3 := (web.Document as IHTMLDocument3);

      elem.cx := 222;
      elem.cy := 444; // := doc3.getElementById('size');
      if False then
      begin
        ClientWidth := elem.cx;
        ClientHeight := elem.cy;

        Left := (Screen.Width - Width) div 2;
        Top := (Screen.Height - Height) div 2;

        (web.Document as IHTMLDocument2).parentWindow.scrollTo(0,0);
      end;
    end;
  except
    Exit;
  end;
end;

function IsLocalURL(URL: WideString): Boolean;
begin
  Result := (Copy(URL, 1, 5) = 'file:') or (Copy(URL, 1, 1) = '/');
end;

procedure TfrmWebContainer.webNewWindow3(ASender: TObject;
  var ppDisp: IDispatch; var Cancel: WordBool; dwFlags: Cardinal;
  const bstrUrlContext, bstrUrl: WideString);
begin
  Cancel := True;
end;

function TfrmWebContainer.GetIEVersionString: WideString;
begin
  Result := 'unknown';
end;

procedure TfrmWebContainer.webShowContextMenu2(Sender: TCustomEmbeddedWB;
  const dwID: Cardinal; const ppt: PPoint; const CommandTarget: IInterface;
  const Context: IDispatch; var Result: HRESULT);
begin
  Result := S_OK;
end;

function TfrmWebContainer.webShowHelpRequest1(Sender: TObject; HWND: NativeUInt;
  pszHelpFile: PWideChar; uCommand, dwData: Integer; ptMouse: TPoint;
  var pDispatchObjectHit: IDispatch): HRESULT;
begin
  Application.HelpJump('context_'+lowercase(FDialogName));
  Result := S_OK;
end;

procedure TfrmWebContainer.WMUser_FormShown(var Message: TMessage);
begin
  Do_Content_Render(True);

  web.SetFocus;
  web.SetFocusToDoc;
end;

procedure TfrmWebContainer.WndProc(var Message: TMessage);  // I2720
begin
  inherited;
  
  with Message do
    case Msg of
      WM_ACTIVATE, WM_SETFOCUS, WM_KILLFOCUS:
        begin
          Windows.SetFocus(web.Handle);
          web.SetFocusToDoc;
        end;
    end;
end;

end.
