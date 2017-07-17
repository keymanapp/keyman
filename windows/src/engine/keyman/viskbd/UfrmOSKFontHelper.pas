(*
  Name:             UfrmOSKFontHelper
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      27 Mar 2008

  Modified Date:    25 Sep 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          27 Mar 2008 - mcdurdin - Initial version I1374
                    20 Jul 2008 - mcdurdin - I1533 - Show hint for non-Unicode keyboards
                    29 Mar 2010 - mcdurdin - I2199 - Shift+click
                    24 Jun 2010 - mcdurdin - I2421 - Start work on font helper showing additional detail
                    26 Jul 2010 - mcdurdin - Code tidy - remove old commented-out code
                    17 Dec 2010 - mcdurdin - I2570 - Upgrade EmbeddedWB (also I2393)
                    18 Feb 2011 - mcdurdin - I2721 - Override Javascript-disabled security for web controls
                    18 Feb 2011 - mcdurdin - I2712 - SMP support for font helper
                    18 Mar 2011 - mcdurdin - I1698, I2120, I2323, I2565 - Font helper crash when searching
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    08 Jun 2012 - mcdurdin - I3309 - V9.0 - Migrate to Delphi XE2, VS2010, svn 1.7
                    24 Jan 2012 - mcdurdin - I3216 - Crash when F5 pressed in OSK font helper context
                    05 Jul 2012 - mcdurdin - I3390 - Font helper can crash when WM_FONTCHANGE received [CrashID:keyman.exe_8.0.350.0_2C53A3AE_EAccessViolation]
                    03 Nov 2012 - mcdurdin - I3519 - V9.0 - Merge of I3390 - Font helper can crash when WM_FONTCHANGE received
                    03 Nov 2012 - mcdurdin - I3520 - V9.0 - Merge of I3216 - Crash when F5 pressed in OSK font helper context
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
                    25 Sep 2014 - mcdurdin - I4412 - V9.0 - Character Map needs to insert characters using SendInput
*)
unit UfrmOSKFontHelper;  // I3306

interface

uses
  System.Contnrs,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UfrmOSKPlugInBase, OleCtrls, SHDocVw, EmbeddedWB,
  keymanapi_TLB, xmlrenderer, UfrmKeymanBase, utilcheckfonts,
  UserMessages, SHDocVw_EWB, EwbCore, KeymanEmbeddedWB,
  TempFileManager;

type
  TKeyboardProps = record
    KeyboardName: WideString;
    UsageFileName: WideString;
    HasOSK: Boolean;
    HasWelcome: Boolean;
  end;

  TfrmOSKFontHelper = class(TfrmOSKPlugInBase)
    web: TKeymanEmbeddedWB; // I2721
    procedure webShowContextMenu(Sender: TCustomEmbeddedWB;
      const dwID: Cardinal; const ppt: PPoint; const CommandTarget: IInterface;
      const Context: IDispatch; var Result: HRESULT);
    procedure webKeyDown(Sender: TObject; var Key: Word; ScanCode: Word;
      Shift: TShiftState);
    procedure webScriptError(Sender: TObject; ErrorLine, ErrorCharacter,
      ErrorCode, ErrorMessage, ErrorUrl: string; var ScriptErrorAction: TScriptErrorAction);
    procedure webDocumentComplete(ASender: TObject; const pDisp: IDispatch;
      var URL: OleVariant);
    procedure webBeforeNavigate2(ASender: TObject; const pDisp: IDispatch;
      var URL, Flags, TargetFrameName, PostData, Headers: OleVariant;
      var Cancel: WordBool);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure webNewWindow3(ASender: TObject; var ppDisp: IDispatch;
      var Cancel: WordBool; dwFlags: Cardinal; const bstrUrlContext,
      bstrUrl: WideString);
    function webShowHelpRequest1(Sender: TObject; HWND: NativeUInt;
      pszHelpFile: PWideChar; uCommand, dwData: Integer; ptMouse: TPoint;
      var pDispatchObjectHit: IDispatch): HRESULT;
  private
    FXML: string;   // I4181
    FXMLFileName: TTempFile;   // I4181
    FXMLRenderers: TXMLRenderers;
    FDialogName: WideString;
    FCheckFontsThread: TCheckFontsThread;
    FCheckFontKeyboards: TCheckFontKeyboards;
    FLastSelectedKeyboardID: WideString;
    FLastSelectedKeymanID: Integer;
    FLastSelectedKeyboardName: WideString;

    procedure Content_Render;
    procedure WMUser_FireCommand(var Message: TMessage); message WM_USER_FireCommand;
    procedure WMUser_ContentRender(var Message: TMessage); message WM_USER_ContentRender;
    procedure WMUser_FontChange(var Message: TMessage); message WM_USER_FontChange;  // I3390   // I3519

    procedure CMFontChange(var Message: TMessage); message CM_FONTCHANGE;
    procedure FireCommand(const command: WideString; params: TStringList);
    procedure CheckFontsThreadComplete(Sender: TObject);
    procedure DisplayKeyboardFonts;
    procedure StartCheckingFonts(Keyboard: IKeymanKeyboardInstalled);
    procedure Do_Content_Render(const AXML: WideString);
    { Private declarations }
  public
    { Public declarations }
    procedure SelectKeyboard(KeymanID: Integer);
  end;

implementation

uses
  findfonts,
  KLog,
  kmint,
  MSHTML_TLB,
  custinterfaces,
  UfrmKeyman7Main,
  UfrmVisualKeyboard,
  Unicode,
  USendInputString,
  utilexecute,
  utilhttp,
  utilsystem,
  utilxml,
  WideStringClass;

{$R *.dfm}

{ TfrmOSKFontHelper }

procedure TfrmOSKFontHelper.SelectKeyboard(KeymanID: Integer);
var
  i: Integer;
  kbds: IKeymanKeyboardsInstalled;
begin
  if KeymanID <> -1 then
  begin
    kbds := kmcom.Keyboards;
    for i := 0 to kbds.Count - 1 do
      if kbds[i].KeymanID = KeymanID then
      begin
        // Get fonts for the keyboard
        StartCheckingFonts(kbds[i]);
        kbds := nil;
        Exit;
      end;
  end;

  FLastSelectedKeyboardName := '';
  FLastSelectedKeyboardID := '';
  FLastSelectedKeymanID := -1;
  DisplayKeyboardFonts; // Displays default details
end;

procedure TfrmOSKFontHelper.webBeforeNavigate2(ASender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
var
  params: TStringList;
begin
  if GetParamsFromURL(URL, params) then
  begin
    PostMessage(Handle, WM_USER_FireCommand, 0, Integer(params));
    Cancel := True;
  end;
end;

procedure TfrmOSKFontHelper.webDocumentComplete(ASender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
var
  doc3: IHTMLDocument3;
  elem: IHTMLElement;
begin
  try
    if Assigned(web.Document) then
    begin
      doc3 := (web.Document as IHTMLDocument3);

      elem := doc3.documentElement;
      if Assigned(elem) then
        elem.insertAdjacentHTML('afterBegin', '&#xa0;<SCRIPT For="window" Event="onerror">var noOp = null;</SCRIPT>');
    	// NOTE: The &nbsp, or some other visible HTML, is required. Internet Explorer will not
    	// parse and recognize the script block without some visual HTML to
    	// accompany it.
    end;
    FreeAndNil(FXMLFileName);   // I4181
  except
    Exit;
  end;
end;

procedure TfrmOSKFontHelper.webNewWindow3(ASender: TObject;
  var ppDisp: IDispatch; var Cancel: WordBool; dwFlags: Cardinal;
  const bstrUrlContext, bstrUrl: WideString);
var
  params: TStringList;
begin
  Cancel := True;
  if GetParamsFromURL(bstrURL, params)
    then PostMessage(Handle, WM_USER_FireCommand, 0, Integer(params))
    else web.Go(bstrURL);
end;

procedure TfrmOSKFontHelper.webKeyDown(Sender: TObject; var Key: Word;
  ScanCode: Word; Shift: TShiftState);
begin
  if (Key = VK_F5) and (ssCtrl in Shift) then
  begin
    Key := 0;
    PostMessage(Handle, WM_USER_ContentRender, 0, 0);
  end;
end;

procedure TfrmOSKFontHelper.webScriptError(Sender: TObject; ErrorLine,
  ErrorCharacter, ErrorCode, ErrorMessage, ErrorUrl: string; var ScriptErrorAction: TScriptErrorAction);
begin
  ScriptErrorAction := eaCancel;
  //TODO: Log message to event log
end;

procedure TfrmOSKFontHelper.webShowContextMenu(Sender: TCustomEmbeddedWB;
  const dwID: Cardinal; const ppt: PPoint; const CommandTarget: IInterface;
  const Context: IDispatch; var Result: HRESULT);
begin
  PostMessage(Handle, WM_CONTEXTMENU, web.Handle, MAKELONG(ppt.X, ppt.Y));
  Result := S_OK;
//Result := S_FALSE;
end;

function TfrmOSKFontHelper.webShowHelpRequest1(Sender: TObject;
  HWND: NativeUInt; pszHelpFile: PWideChar; uCommand, dwData: Integer;
  ptMouse: TPoint; var pDispatchObjectHit: IDispatch): HRESULT;
begin
  Application.HelpJump('context_'+lowercase(FDialogName));
  Result := S_OK;
end;

procedure TfrmOSKFontHelper.WMUser_ContentRender(var Message: TMessage);
begin
  DisplayKeyboardFonts;  // I3216   // I3520
end;

procedure TfrmOSKFontHelper.WMUser_FireCommand(var Message: TMessage);
var
  command: WideString;
  params: TStringList;
begin
  params := TStringList(Message.LParam);
  command := params[0];
  params.Delete(0);
  FireCommand(command, params);
  params.Free;
end;

procedure TfrmOSKFontHelper.Content_Render;
var
  AdditionalData: WideString;
begin
  AdditionalData := FXML;

  FreeAndNil(FXMLFileName);   // I4181

  if not FileExists(GetXMLTemplatePath(FXMLRenderers.RenderTemplate)+FXMLRenderers.RenderTemplate) then
    Exit;

  FXMLFileName := FXMLRenderers.RenderToFile(False, AdditionalData);

  FDialogName := ChangeFileExt(ExtractFileName(FXMLRenderers.RenderTemplate), '');
  //HelpType := htKeyword;
  //HelpKeyword := FDialogName;
end;

procedure TfrmOSKFontHelper.FireCommand(const command: WideString; params: TStringList);
var
  hwnd: THandle;
  text: string;
  ch: WideString;
  n: Integer;
  s: WideString;
  v: Integer;
begin
  if command = 'link' then TUtilExecute.URL(params.Values['url'])  // I3349
  else if command = 'tutorial' then //(ActiveProduct as IKeymanProduct2).OpenTutorial
  else if command = 'osk' then frmKeyman7Main.frmVisualKeyboard.ActivePage := apKeyboard
  else if command = 'welcome' then frmKeyman7Main.MnuOpenKeyboardHelp(nil)
  else if command = 'help' then frmKeyman7Main. MnuOpenProductHelp(frmKeyman7Main.frmVisualKeyboard)
  else if command = '' then
       
  else if command = 'insertchars' then
  begin
    hwnd := kmcom.Control.LastFocusWindow;

    ch := Trim(params.Values['chars']);

    // I1429 - fix U+xxxx in insertchars URL
    
    n := Pos(' ', ch); if n = 0 then n := Length(ch)+1;

    while ch <> '' do
    begin
      s := Copy(ch, 1, n-1); if s = '' then Break;

      if Copy(s,1,2) = 'U+' then v := StrToIntDef('$'+Copy(s,3,6),0)
      else if s[1] = 'x' then v := StrToIntDef('$'+Copy(s,2,6),0)
      else if s[1] = 'd' then v := StrToIntDef(Copy(s,2,7), 0)
      else v := 0;

      if (v > 32) and (v <= $10FFFF) then
      begin
        if Uni_IsSurrogate(v)
          then text := text + Uni_UTF32ToSurrogate1(v) + Uni_UTF32ToSurrogate2(v)
          else text := text + Char(v);
      end;

      Delete(ch, 1, n); ch := Trim(ch);
      n := Pos(' ', ch);
      if n = 0 then n := Length(ch)+1;
    end;

    if text <> '' then
      SendInputString(hwnd, text);   // I4412
  end;
end;

procedure TfrmOSKFontHelper.FormCreate(Sender: TObject);
begin
  inherited;
  FCheckFontKeyboards := TCheckFontKeyboards.Create;
end;

procedure TfrmOSKFontHelper.FormDestroy(Sender: TObject);
begin
  inherited;
  if Assigned(FCheckFontsThread) then  // I1698, I2120, I2323, I2565
  begin
    FCheckFontsThread.OnTerminate := nil;
    FCheckFontsThread.Terminate;
    FCheckFontsThread := nil;
  end;

  FreeAndNil(FXMLFileName);   // I4181
  FreeAndNil(FCheckFontKeyboards);
end;

procedure TfrmOSKFontHelper.CheckFontsThreadComplete(Sender: TObject);
var
  i: Integer;
begin
  FCheckFontsThread.Keyboards.OwnsObjects := False;
  for i := 0 to FCheckFontsThread.Keyboards.Count - 1 do
    FCheckFontKeyboards.Add(FCheckFontsThread.Keyboards[i]);

  if FLastSelectedKeyboardID = '' then
    DisplayKeyboardFonts
  else if FCheckFontKeyboards.Keyboards[FLastSelectedKeyboardID] = nil then
    SelectKeyboard(FLastSelectedKeymanID)
  else
    DisplayKeyboardFonts;

  FCheckFontsThread := nil;  // I3390 - moved from above to ensure thread doesn't restart until really ready (appears render can be re-entrant)   // I3519
end;

procedure TfrmOSKFontHelper.CMFontChange(var Message: TMessage);
begin
  PostMessage(Handle, WM_USER_FontChange, 0, 0);  // I3390 - This is via a SendMessage which locks the sending process, so we'll accept it and requery in our own time   // I3519
end;

procedure TfrmOSKFontHelper.WMUser_FontChange(var Message: TMessage);  // I3390   // I3519
begin
  FCheckFontKeyboards.Clear;
  SelectKeyboard(FLastSelectedKeymanID);
end;

procedure TfrmOSKFontHelper.StartCheckingFonts(Keyboard: IKeymanKeyboardInstalled);
begin
  FLastSelectedKeyboardID := Keyboard.ID;
  FLastSelectedKeyboardName := Keyboard.Name;
  FLastSelectedKeymanID := Keyboard.KeymanID;

  if FCheckFontKeyboards.Keyboards[FLastSelectedKeyboardID] <> nil then
  begin
    DisplayKeyboardFonts;
    Exit;
  end;

  if Assigned(FCheckFontsThread) then Exit; // Still looking up previous keyboard fonts, it will be checked shortly

  if Keyboard.Encodings = keANSI then
  begin
    // I1533 - hint for non-Unicode keyboard.
    Do_Content_Render('<Keyboard Name="'+XmlEncode(FLastSelectedKeyboardName)+'" />');
  end
  else
  begin
    Do_Content_Render('<Searching /><Keyboard Name="'+XmlEncode(FLastSelectedKeyboardName)+'" />');

    FCheckFontsThread := TCheckFontsThread.Create;
    FCheckFontsThread.FreeOnTerminate := True;
    FCheckFontsThread.OnTerminate := CheckFontsThreadComplete;
    FCheckFontsThread.AddKeyboard(Keyboard.ID, Keyboard.Filename, Keyboard.GetCharsUsed);
    FCheckFontsThread.Start;  // I3309
  end;
end;

procedure TfrmOSKFontHelper.DisplayKeyboardFonts;
var
  FKeyboard: TCheckFontKeyboard;
  J: Integer;

  function CharCode(ch: WideChar): WideString;
  begin
    Result := IntToHex(Ord(ch), 4);
  end;

var
  FFontsData: WideString;
  i: Integer;
  ch,ch2: WideChar;
begin
  FFontsData := '';

  FKeyboard := FCheckFontKeyboards.Keyboards[FLastSelectedKeyboardID];
  if (FLastSelectedKeyboardID <> '') and Assigned(FKeyboard) then
  begin
    FFontsData := '<Chars>';

    I := 1;
    while I <= Length(FKeyboard.Chars) do  // I2712
    begin
      ch := FKeyboard.Chars[I];
      if Uni_IsSurrogate1(ch) and (I < Length(FKeyboard.Chars)) then
      begin
        ch2 := FKeyboard.Chars[I+1];
        FFontsData := FFontsData + '<Ch CharCode="'+IntToHex(Uni_SurrogateToUTF32(ch,ch2),5)+'">'+XmlEncode(ch+ch2)+'</Ch>';
        Inc(I);
      end
      else
        FFontsData := FFontsData + '<Ch CharCode="'+CharCode(ch)+'">'+XmlEncode(ch)+'</Ch>';
      Inc(I);
    end;

    FFontsData := FFontsData + '</Chars>';

    FFontsData := FFontsData + '<Fonts>';

    for I := 0 to FKeyboard.Fonts.Count - 1 do
    begin
      FFontsData := FFontsData + '<Font '+
        'Index="'+IntToStr(I)+'" '+
        'Name="'+XmlEncode(FKeyboard.Fonts[I].FontName)+'" '+
        'Coverage="'+IntToStr(FKeyboard.Fonts[i].Coverage)+'">'; //<IncludedChars>';

      FFontsData := FFontsData + '<ExcludedChars>';
      J := 1;
      while J <= Length(FKeyboard.Fonts[I].ExcludedChars) do  // I2712
      begin
        ch := FKeyboard.Fonts[I].ExcludedChars[J];
        if Uni_IsSurrogate1(ch) and (J < Length(FKeyboard.Fonts[I].ExcludedChars)) then
        begin
          ch2 := FKeyboard.Fonts[I].ExcludedChars[J+1];
          FFontsData := FFontsData + '<Ch CharCode="'+IntToHex(Uni_SurrogateToUTF32(ch,ch2),5)+'" />';
          Inc(J);
        end
        else
          FFontsData := FFontsData + '<Ch CharCode="'+CharCode(ch)+'" />';
        Inc(J);
      end;
      FFontsData := FFontsData + '</ExcludedChars>';
      FFontsData := FFontsData + '</Font>';
    end;

    FFontsData := FFontsData + '</Fonts><Keyboard Name="'+XmlEncode(FLastSelectedKeyboardName)+'" />';
    {finally
      FFonts.Free;
    end;}
  end
  else
  begin
    if kmcom.Keyboards.Count = 0 then
      FFontsData := '<NoKeyboards />';
  end;

  Do_Content_Render(FFontsData);
end;

procedure TfrmOSKFontHelper.Do_Content_Render(const AXML: WideString);
var
  v: OleVariant;
begin
  FXMLRenderers := TXMLRenderers.Create;
  FXMLRenderers.RenderTemplate := 'FontHelper.xsl';

  FXML := AXML;
  Content_Render;
  FreeAndNil(FXMLRenderers);

  v := navNoHistory or navNoReadFromCache or navNoWriteToCache;
  if Assigned(FXMLFileName) and FileExists(FXMLFileName.Name) then   // I4181
    web.Navigate(FXMLFileName.Name, v);   // I4181
end;

end.
