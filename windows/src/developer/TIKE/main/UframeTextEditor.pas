unit UframeTextEditor;  // I3323   // I4797

interface

uses
  System.Types,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ImgList,
  MenuImgList, ExtCtrls,

{$IFDEF USE_PLUSMEMO}
  SyntaxHighlight,
  PlusGutter,
  ExtHilit,
  HtmlHighlight,
  pmprint,
  PMSupport,
{$ENDIF}

  UserMessages,
  Keyman.Developer.UI.UframeCEFHost,
  TextFileFormat, UfrmTike,
  System.ImageList, KeymanDeveloperMemo,
  Vcl.StdCtrls, PlusMemo;

type
  TParColourLineType = (pcltNone, pcltBreakpoint, pcltExecutionPoint, pcltError);

  TframeTextEditor = class(TTIKEForm)
    memo: TKeymanDeveloperMemo;
    lstImages: TMenuImgList;
    dlgFonts: TFontDialog;
    dlgPrintSetup: TPrinterSetupDialog;
    dlgFind: TFindDialog;
    dlgReplace: TReplaceDialog;
    lstImagesDisabled: TImageList;
    tmrUpdateSelectedToken: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure memoBeforeChange(Sender: TObject; var Txt: PWideChar);
    procedure memoEnter(Sender: TObject);
    procedure memoExit(Sender: TObject);
    procedure memoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure memoChange(Sender: TObject);
    procedure memoSelMove(Sender: TObject);
    procedure memoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure memoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuPopupShowCharacterClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure dlgFindFind(Sender: TObject);
    procedure dlgReplaceReplace(Sender: TObject);
    procedure dlgReplaceFind(Sender: TObject);
    procedure tmrUpdateSelectedTokenTimer(Sender: TObject);
    procedure TntFormDestroy(Sender: TObject);
  private
    class var FInitialFilenameIndex: Integer;
  private
{$IFDEF USE_PLUSMEMO}
    gutter: TPlusGutter;
    SyntaxHighlighter: TSyntaxHighlighter;
    pmPrinter: TPlusMemoPrinter;
    highlighter: TExtHighlighter;
    highlighterHTML: THtmlHighlighter;
{$ENDIF}

    FLoading: Boolean;
    FEditorFormat: TEditorFormat;
    FOnChanged: TNotifyEvent;
    FErrorPar: Integer;
    FTextFileFormat: TTextFileFormat;
    FindFound: Boolean;

    cef: TframeCEFHost;
    FFilename: string;

    procedure RefreshOptions;
    function GetText: WideString;
    procedure SetText(Value: WideString);
    procedure SetEditorFormat(const Value: TEditorFormat);
    procedure UpdateFontTags;
    procedure Changed;
    procedure UpdateState;
    procedure ClearError;
    procedure UpdateSelectedToken;
    procedure SetCharFont(const Value: TFont);
    procedure SetCodeFont(const Value: TFont);
    function GetCharFont: TFont;
    function GetCodeFont: TFont;
    procedure SetTextFileFormat(const Value: TTextFileFormat);
    function GetSelectedTokens(var token, prevtoken: WideString; var x, tx: Integer): Boolean;

    procedure cefBeforeBrowse(Sender: TObject; const Url: string; out Result: Boolean);
    procedure cefLoadEnd(Sender: TObject);
    procedure LoadFileInBrowser;
    procedure WMUser_FireCommand(var Message: TMessage); message WM_USER_FireCommand;
    procedure FireCommand(const commands: TStringList);
  protected
    function GetHelpTopic: string; override;

    procedure SaveToStream(AStream: TStream);
  public
    { Public declarations }
    procedure UpdateParColour(par: Integer; LineType: TParColourLineType);
    procedure SetFocus; override;

    procedure FindError(ln: Integer);
    procedure FindErrorByOffset(offset: Integer);   // I4083
    function OffsetToLine(Offset: Integer): Integer;   // I4083

    procedure EditFind;
    procedure EditFindNext;
    procedure EditReplace;

    procedure SyntaxColourChange;

    function PrintFile(Header: WideString = ''): Boolean;
    function PrintPreview(Header: WideString = ''): Boolean;

    procedure LoadFromFile(AFileName: WideString); overload;   // I4034
    procedure LoadFromStream(AStream: TStream); overload;  // I2964
    procedure SaveToFile(AFileName: WideString);
    procedure LoadFromFile(AFileName: WideString; ATextFileFormat: TTextFileFormat); overload;   // I4034
    procedure LoadFromStream(AStream: TStream; ATextFileFormat: TTextFileFormat); overload;  // I2964

    property EditorText: WideString read GetText write SetText;
    property EditorFormat: TEditorFormat read FEditorFormat write SetEditorFormat;
    property OnChanged: TNotifyEvent read FOnChanged write FonChanged;

    property CodeFont: TFont read GetCodeFont write SetCodeFont;
    property CharFont: TFont read GetCharFont write SetCharFont;
    property TextFileFormat: TTextFileFormat read FTextFileFormat write SetTextFileFormat;
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics,

  dmActionsMain,
  dmActionsTextEditor,
  CharacterInfo,
  CharMapDropTool,
  keyboardparser,
  KeymanDeveloperOptions,
  kwhelp,
  ErrorControlledRegistry,
  RegistryKeys,
  UfrmHelp,
  UfrmMain,
  UmodWebHTTPServer,
  Unicode,
  utilhttp,
  utilstr;
  
{$R *.dfm}

{ TframeTextEditor }

procedure TframeTextEditor.RefreshOptions;
begin
  //TODO: memo.TabStops   := FKeymanDeveloperOptions.IndentSize;
end;

procedure TframeTextEditor.cefBeforeBrowse(Sender: TObject; const Url: string; out Result: Boolean);
var
  params: TStringList;
begin
  Result := False;

  if csDestroying in ComponentState then   // I3983
  begin
    Result := True;
    Exit;
  end;

  if GetParamsFromURL(URL, params) then
  begin
    PostMessage(Handle, WM_USER_FireCommand, 0, Integer(params));
    Result := True;
  end;
end;

procedure TframeTextEditor.cefLoadEnd(Sender: TObject);
begin
  //
end;

procedure TframeTextEditor.Changed;
begin
  //memo.Update; // required due to bug in Delphi's TStyleHook which causes invalidated areas to be lost because   // I4962
    // WM_SETREDRAW is set on the window when the caption is changed, due to the {*} character being added
    // to the caption.   I4870   // I4918 undoes memo.Update for more performant painting
  if Assigned(FOnChanged) then FOnChanged(Self);
  UpdateState;
end;

procedure TframeTextEditor.FormCreate(Sender: TObject);
begin
  inherited;

  cef := TframeCEFHost.Create(Self);
  cef.Parent := Self;
  cef.Visible := True;
  cef.OnBeforeBrowse := cefBeforeBrowse;
//  cef.OnLoadEnd := cefLoadEnd;

{$IFDEF USE_PLUSMEMO}
  highlighter := TExtHighlighter.Create(Self);
  highlighterHTML := THtmlHighlighter.Create(Self);
  with highlighterHTML do
  begin
    Bracket.AltFont := False;
    Bracket.Style := [fsBold];
    Bracket.Background := -1;
    Bracket.Foreground := clBlue;
    Bracket.Cursor := crDefault;
    DefaultText.AltFont := False;
    DefaultText.Style := [];
    DefaultText.Background := -1;
    DefaultText.Foreground := clWindowText;
    DefaultText.Cursor := crDefault;
    HtmlKeyword.AltFont := False;
    HtmlKeyword.Style := [fsBold];
    HtmlKeyword.Background := -1;
    HtmlKeyword.Foreground := clNavy;
    HtmlKeyword.Cursor := crDefault;
    HtmlTag.AltFont := False;
    HtmlTag.Style := [fsItalic];
    HtmlTag.Background := -1;
    HtmlTag.Foreground := clGreen;
    HtmlTag.Cursor := crDefault;
    HtmlComment.AltFont := False;
    HtmlComment.Style := [fsItalic];
    HtmlComment.Background := -1;
    HtmlComment.Foreground := clOlive;
    HtmlComment.Cursor := crDefault;
    HtmlAttribute.AltFont := False;
    HtmlAttribute.Style := [];
    HtmlAttribute.Background := -1;
    HtmlAttribute.Foreground := clGreen;
    HtmlAttribute.Cursor := crDefault;
    HtmlAttributeValue.AltFont := False;
    HtmlAttributeValue.Style := [];
    HtmlAttributeValue.Background := -1;
    HtmlAttributeValue.Foreground := clGray;
    HtmlAttributeValue.Cursor := crDefault;
    PerlScript.AltFont := False;
    PerlScript.Style := [];
    PerlScript.Background := -1;
    PerlScript.Foreground := clTeal;
    PerlScript.Cursor := crDefault;
    ScriptTag.AltFont := False;
    ScriptTag.Style := [fsBold];
    ScriptTag.Background := clWindow;
    ScriptTag.Foreground := clPurple;
    ScriptTag.Cursor := crDefault;
    ScriptContent.AltFont := True;
    ScriptContent.Style := [];
    ScriptContent.Background := -1;
    ScriptContent.Foreground := clWindowText;
    ScriptContent.Cursor := crDefault;
    SpecialChars.AltFont := False;
    SpecialChars.Style := [];
    SpecialChars.Background := -1;
    SpecialChars.Foreground := clFuchsia;
    SpecialChars.Cursor := crDefault;
    UnknownTag.AltFont := False;
    UnknownTag.Style := [];
    UnknownTag.Background := -1;
    UnknownTag.Foreground := clRed;
    UnknownTag.Cursor := crDefault;
    XMLSyntax := False;
  end;

  memo.Highlighter := highlighter;

  gutter := TPlusGutter.Create(Self);
  gutter.Align := alLeft;
  gutter.IgnoreLastLineIfEmpty := False;
  gutter.PlusMemo := memo;
  gutter.Parent := Self;

  pmPrinter := TPlusMemoPrinter.Create(Self);
  with pmPrinter do
  begin
    MemoToPrint := memo;
    GutterWidth := 0.500000000000000000;
    MarginLeft := 0.750000000000000000;
    MarginRight := 0.500000000000000000;
    MarginTop := 0.750000000000000000;
    MarginBottom := 0.750000000000000000;
    LineSpacing := -1.200000047683716000;
    Footer := 'Page {p} of {P}';
    HeaderYPos := 0.400000005960464400;
    FooterYPos := 0.400000005960464400;
    HeaderFont.Charset := ANSI_CHARSET;
    HeaderFont.Color := clWindowText;
    HeaderFont.Height := -13;
    HeaderFont.Name := 'Arial';
    HeaderFont.Style := [];
    FooterFont.Charset := ANSI_CHARSET;
    FooterFont.Color := clWindowText;
    FooterFont.Height := -13;
    FooterFont.Name := 'Arial';
    FooterFont.Style := [];
    NumbersFont.Charset := DEFAULT_CHARSET;
    NumbersFont.Color := clWindowText;
    NumbersFont.Height := -11;
    NumbersFont.Name := 'Tahoma';
    NumbersFont.Style := [];
    PreviewTitle := 'Print preview';
    PrintTitle := '{application}';
  end;

  SyntaxHighlighter := TSyntaxHighlighter.Create(Self);
  SyntaxHighlighter.Apply(memo, highlighter);
{$ENDIF}
  UpdateFontTags;
end;

type
  TAccessWinControl = class(TWinControl);

procedure TframeTextEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  function ShiftStateToLParam(Shift: TShiftState): Integer;
  const
    AltMask = $20000000;
  begin
    if ssAlt in Shift then
      Result := AltMask
    else
      Result := 0;
  end;
var
  msg: TWMKey;
begin
  if (Owner is TCustomForm) and (Owner as TCustomForm).KeyPreview then
  begin
    msg.Msg := WM_KEYDOWN;
    msg.CharCode := Key;
    msg.KeyData := ShiftStateToLParam(Shift);
    if TAccessWinControl(Owner).DoKeyDown(msg) then Key := 0;
  end;
end;

function TframeTextEditor.GetCharFont: TFont;
begin
  if FEditorFormat = efHTML
    then Result := memo.Font   // I1426 - script tags should be 'code' font
    else Result := Memo.AltFont;
end;

function TframeTextEditor.GetCodeFont: TFont;
begin
  if FEditorFormat = efHTML
    then Result := Memo.AltFont  // I1426 - script tags should be 'code' font
    else Result := Memo.Font;
end;

function TframeTextEditor.GetText: WideString;
begin
  Result := modWebHttpServer.AppSource.GetSource(FFileName);
end;

procedure TframeTextEditor.LoadFromFile(AFileName: WideString);
var
  fs: TFileStream;
begin
  FLoading := True;
  try
    FFileName := AFileName;
    if FileExists(AFileName) then
    begin
      fs := TFileStream.Create(AFileName, fmOpenRead);
      try
        LoadFromStream(fs);
      finally
        fs.Free;
      end;
    end
    else
    begin
      TextFileFormat := tffUTF8;
      memo.SetTextBuf('');
      LoadFileInBrowser;
      UpdateSelectedToken;
    end;
    memo.Modified := False;
  finally
    FLoading := False;
  end;
end;

procedure TframeTextEditor.LoadFromStream(AStream: TStream; ATextFileFormat: TTextFileFormat);  // I2964
begin
  FLoading := True;
  try
    TextFileFormat := ATextFileFormat;

    memo.Lines.LoadFromStream(AStream, TextFileFormatToEncoding(TextFileFormat));   // I3637
    memo.Modified := False;  // I3082   // I3502

    LoadFileInBrowser;
  finally
    FLoading := False;
  end;
end;

procedure TframeTextEditor.LoadFileInBrowser;
  function GenerateNewFilename: string;
  begin
    Inc(FInitialFilenameIndex);
    Result := '*texteditor*'+IntToStr(FInitialFilenameIndex);
  end;
begin
  if FFilename = '' then
    FFilename := GenerateNewFilename;
  modWebHTTPServer.AppSource.RegisterSource(FFilename, memo.Lines.Text, True);
  cef.Navigate(modWebHttpServer.GetLocalhostURL + '/app/editor/?'+URLEncode(FFilename));   // I4195
end;

procedure TframeTextEditor.LoadFromFile(AFileName: WideString;
  ATextFileFormat: TTextFileFormat);   // I4034
var
  fs: TFileStream;
begin
  FLoading := True;
  try
    FFileName := AFileName;
    if FileExists(AFileName) then
    begin
      try
        fs := TFileStream.Create(AFileName, fmOpenRead);
        try
          LoadFromStream(fs, ATextFileFormat);
        finally
          fs.Free;
        end;
      except
        on E:EFOpenError do   // I4616   // I4721
        begin
          ShowMessage(E.Message);
          Exit;
        end;
      end;
    end
    else
    begin
      TextFileFormat := ATextFileFormat;
      memo.SetTextBuf('');
      LoadFileInBrowser;
    end;
    memo.Modified := False;
  finally
    FLoading := False;
  end;
end;

procedure TframeTextEditor.LoadFromStream(AStream: TStream);
begin
  FLoading := True;
  try
    memo.Lines.LoadFromStream(AStream); // prolog determines encoding  // I3337
    if memo.Encoding = TEncoding.UTF8 then  // I3337   // I3636
      TextFileFormat := tffUTF8
    else if memo.Encoding = TEncoding.Unicode then  // I3337   // I3636
      TextFileFormat := tffUTF16
    else
      TextFileFormat := tffANSI;
    UpdateSelectedToken;
    memo.Modified := False;

    LoadFileInBrowser;
  finally
    FLoading := False;
  end;
end;

procedure TframeTextEditor.SaveToFile(AFileName: WideString);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  with fs do
  try
    Self.SaveToStream(fs);
  finally
    Free;
  end;
  memo.Modified := False;
end;


procedure TframeTextEditor.SaveToStream(AStream: TStream);
var
  FSource: string;
  ss: TStringStream;
begin
  FSource := modWebHttpServer.AppSource.GetSource(FFileName);
  case FTextFileFormat of
    tffANSI:  ss := TStringStream.Create(FSource, TEncoding.Default);
    tffUTF8:  ss := TStringStream.Create(FSource, TEncoding.UTF8);
    tffUTF16: ss := TStringStream.Create(FSource, TEncoding.Unicode);
    else raise EAssertionFailed.Create('Unsupported file format');
  end;
  try
    AStream.CopyFrom(ss, 0);
  finally
    ss.Free;
  end;
  memo.Modified := False;
end;

procedure TframeTextEditor.memoBeforeChange(Sender: TObject; var Txt: PWideChar);
begin
  ClearError;
end;

procedure TframeTextEditor.memoChange(Sender: TObject);
begin
  if not FLoading then
  Changed;
end;
(*
  with Source as TCharacterDragObject do
    s := Text[cmimText];
{    if IsUnicode
      then s := Format('U+%4.4X', [CharCode])
      else s := Format('d%d', [CharCode]);}

  memo.SelText := s;

  Windows.SetFocus(memo.Handle);

  Accept := (Source is TCharacterDragObject) and (X > 16) and (Y > 16);
  if Accept then
  begin
    memo.SetCaretFromMouse(X - 16, Y - 16);
  end;
 *)
procedure TframeTextEditor.memoEnter(Sender: TObject);
begin
  //frmKeymanDeveloper.RegisterToolbarClient(Self);
end;

procedure TframeTextEditor.memoExit(Sender: TObject);
begin
  //frmKeymanDeveloper.UnregisterToolbarClient(Self);
end;

procedure TframeTextEditor.memoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  k: Integer;
  pt: TPoint;
begin
  if (Key=VK_Insert) and (Shift = []) then
  begin
    memo.Overwrite := not memo.Overwrite;
    if memo.Overwrite
      then frmKeymanDeveloper.barStatus.Panels[1].Text := 'Overwrite'
      else frmKeymanDeveloper.barStatus.Panels[1].Text := 'Insert';
    Key := 0;
  end
  else if (Key=VK_ESCAPE) and (Shift = []) then
  begin
    ClearError;
  end
  else if (Key=VK_APPS) and (Shift = []) then
  begin
    pt := memo.ClientToScreen(Point(memo.CaretX, memo.CaretY + memo.LineHeight));
    modActionsTextEditor.mnuTextEditor.Popup(pt.X, pt.Y);
    Key := 0;
  end
  else if (Key=VK_Tab) and (Shift = []) then
  begin
    if FKeymanDeveloperOptions.UseTabChar then Exit;
    k := memo.TabStops-(memo.SelCol mod memo.TabStops);
    if k = 0 then k := memo.TabStops;
    memo.SelText := StringOfChar(' ', k);
    Key := 0;
  end;
end;

procedure TframeTextEditor.memoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  if Button = mbRight then
  begin
//    memo.Perform(WM_LBUTTONDOWN, 0, MAKELONG(X, Y));
//    memo.Perform(WM_LBUTTONUP, 0, MAKELONG(X, Y));
    pt := memo.ClientToScreen(Point(X,Y));
    modActionsTextEditor.mnuTextEditor.Popup(pt.X, pt.Y);
  end;
end;

procedure TframeTextEditor.memoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateState;
end;

procedure TframeTextEditor.memoSelMove(Sender: TObject);
begin
  UpdateState;
end;

procedure TframeTextEditor.mnuPopupShowCharacterClick(Sender: TObject);
begin
  //
end;

function TframeTextEditor.PrintFile(Header: WideString): Boolean;
begin
  Result := False;
{$IFDEF USE_PLUSMEMO}
  pmPrinter.Header := Header;
  pmPrinter.Print;
{$ENDIF}
end;

function TframeTextEditor.PrintPreview(Header: WideString): Boolean;
begin
{$IFDEF USE_PLUSMEMO}
  pmPrinter.Header := Header;
  pmPrinter.Preview;
{$ENDIF}
  Result := True;
end;

procedure TframeTextEditor.SetCharFont(const Value: TFont);
begin
  if FEditorFormat = efHTML
    then memo.Font := Value  // I1426 - script tags should be 'code' font
    else memo.AltFont := Value;
end;

procedure TframeTextEditor.SetCodeFont(const Value: TFont);
begin
  if FEditorFormat = efHTML
    then memo.AltFont := Value  // I1426 - script tags should be 'code' font
    else memo.Font := Value;
end;

procedure TframeTextEditor.SetEditorFormat(const Value: TEditorFormat);
begin
  FEditorFormat := Value;
  UpdateFontTags;
end;

procedure TframeTextEditor.SetFocus;
begin
  memo.SetFocus;
end;

procedure TframeTextEditor.UpdateFontTags;
begin
{$IFDEF USE_PLUSMEMO}
  case FEditorFormat of
    efKMN:  memo.Highlighter := highlighter;
    efHTML: begin highlighterHTML.XMLSyntax := False; memo.Highlighter := highlighterHTML; end;
    efXML:  begin highlighterHTML.XMLSyntax := True;  memo.Highlighter := highlighterHTML; end;
    efText: memo.Highlighter := nil;
  end;
  memo.ApplyStartStopKeys := Assigned(memo.Highlighter);
  memo.ApplyKeywords := Assigned(memo.Highlighter);
  memo.ReApplyKeywords;
{$ENDIF}
end;

procedure TframeTextEditor.SetText(Value: WideString);
var
  SelLine: Integer;
  SelCol: Integer;
begin
  if memo.Text <> Value then   // I4021
  begin
    FLoading := True;
    try
      SelLine := memo.SelLine;
      SelCol := memo.SelCol;
      memo.SetTextBuf(PWideChar(Value));
      memo.Modified := False;
      RefreshOptions;
      if SelLine >= memo.Lines.Count then SelLine := memo.Lines.Count - 1;   // I4499   // I4655
      memo.SelLine := SelLine;
      memo.SelCol := SelCol;
      memo.ScrollInView;
      UpdateSelectedToken;

      LoadFileInBrowser;
    finally
      FLoading := False;
    end;
  end;
end;

procedure TframeTextEditor.SetTextFileFormat(const Value: TTextFileFormat);
begin
  if FTextFileFormat <> Value then
  begin
    FTextFileFormat := Value;
    if FTextFileFormat = tffANSI then
      memo.Lines.Text := memo.Lines.Text;
  end;
end;

procedure TframeTextEditor.SyntaxColourChange;
begin
{$IFDEF USE_PLUSMEMO}
  SyntaxHighlighter.Load;
  SyntaxHighlighter.Apply(memo, highlighter);
  memo.ReApplyKeywords;
{$ENDIF}
end;

procedure TframeTextEditor.UpdateState;
begin
  frmKeymanDeveloper.barStatus.Panels[0].Text := Format('Line %d, Col %d', [memo.SelLine+1, memo.SelCol+1]);

  if memo.Focused then
    UpdateSelectedToken;
  //UpdateToolbarState;
end;

procedure TframeTextEditor.WMUser_FireCommand(var Message: TMessage);
var
  params: TStringList;
begin
  params := TStringList(Message.LParam);
  if (params.Count > 0) and (params[0] = 'command') then
  begin
    params.Delete(0);
    FireCommand(params);
  end;
  params.Free;
end;

{-------------------------------------------------------------------------------
 - Paragraph colour management and errors                                      -
 -------------------------------------------------------------------------------}

 procedure TframeTextEditor.ClearError;
var
  par: Integer;
begin
  if FErrorPar > -1 then
  begin
    par := FErrorPar;
    FErrorPar := -1;
    UpdateParColour(par, pcltNone);
  end;
end;

procedure TframeTextEditor.dlgFindFind(Sender: TObject);
begin
  if not memo.FindTxt(dlgFind.FindText, frDown in dlgFind.Options,
      frMatchCase in dlgFind.Options, frWholeWord in dlgFind.Options, True)
    then ShowMessage(Format('Cannot find "%s"', [dlgFind.FindText]))
    else memo.ScrollInView;
end;

procedure TframeTextEditor.dlgReplaceFind(Sender: TObject);
begin
  if not memo.FindTxt(dlgReplace.FindText, frDown in dlgReplace.Options,
      frMatchCase in dlgReplace.Options, frWholeWord in dlgReplace.Options, True) then
  begin
    ShowMessage(Format('Cannot find "%s"', [dlgReplace.FindText]));
    FindFound := False;
  end
  else
  begin
    memo.SelLength := -Length(dlgReplace.FindText);
    memo.ScrollInView;
    FindFound := True;
  end;
end;

procedure TframeTextEditor.dlgReplaceReplace(Sender: TObject);
var
  i: Integer;
begin
  if frReplaceAll in dlgReplace.Options then
  begin
    memo.SelStart := 0; i := 0;
    while memo.FindTxt(dlgReplace.FindText, frDown in dlgReplace.Options,
        frMatchCase in dlgReplace.Options, frWholeWord in dlgReplace.Options, True) do
    begin
      memo.SelText := dlgReplace.ReplaceText;
      Inc(i);
    end;
    ShowMessage(Format('Replaced %d occurrences of "%s" with "%s"',
      [i, dlgReplace.FindText, dlgReplace.ReplaceText]));
    FindFound := False;
  end
  else if frReplace in dlgReplace.Options then
  begin
    if not FindFound then
    begin
      dlgReplaceFind(dlgReplace);
      Exit;
    end;
    if (memo.SelText = dlgReplace.FindText) then
    begin
      memo.SelText := dlgReplace.ReplaceText;
      dlgReplaceFind(dlgReplace);
      Exit;
    end;
  end;
end;

procedure TframeTextEditor.EditFind;
begin
  if not memo.Focused then Exit;
  dlgFind.Execute;
end;

procedure TframeTextEditor.EditFindNext;
begin
  if not memo.Focused then Exit;
  dlgFindFind(dlgFind);
end;

procedure TframeTextEditor.EditReplace;
begin
  if not memo.Focused then Exit;
  FindFound := False;
  dlgReplace.Execute;
end;

procedure TframeTextEditor.FindError(ln: Integer);
begin
  ClearError;

  if (ln <= 0) or (ln >= memo.LineCount) then Exit;   // I4765

  memo.SelLine := ln;
  memo.SelCol := 0;
  memo.ScrollInView;
  FErrorPar := ln;

  UpdateParColour(FErrorPar, pcltError);
end;

function TframeTextEditor.OffsetToLine(Offset: Integer): Integer;   // I4083
begin
  Result := 0;
  while (Result < memo.ParagraphCount) and (Offset > memo.PargrphOffset[Result]) do
    Inc(Result);
end;

procedure TframeTextEditor.FindErrorByOffset(offset: Integer);   // I4083
begin
  ClearError;

  if offset <= 0 then Exit;

  memo.SelStart := offset;
  memo.SelCol := 0;
  memo.ScrollInView;
  FErrorPar := memo.SelLine;

  UpdateParColour(FErrorPar, pcltError);
end;

procedure TframeTextEditor.FireCommand(const commands: TStringList);
var
  i: Integer;
  command: string;
begin
  i := 0;
  while i < commands.Count do
  begin
    command := commands[i];
    if command = 'modified' then Changed   // I3948
//    else if command = 'undo-disable' then FCanUndo := False
//    else if command = 'undo-enable' then FCanUndo := True
//    else if command = 'redo-disable' then FCanRedo := False
//    else if command = 'redo-enable' then FCanRedo := True
    else if command.StartsWith('selected-char,') then
    begin
      Inc(i);
//      UpdateCharacterMap(command.Substring('selected-char,'.Length));   // I4046
    end
    else ShowMessage('keyman:'+commands.Text);
    Inc(i);
  end;
end;

procedure TframeTextEditor.UpdateParColour(par: Integer; LineType: TParColourLineType);
{$IFDEF USE_PLUSMEMO}
var
  FGColor, BGColor: TColor;
begin
  if par < 0 then Exit;
  case LineType of
    pcltNone: begin FGColor := -1; BGColor := -1; end;
    pcltBreakpoint: begin FGColor := clWhite; BGColor := clRed; end;
    pcltExecutionPoint: begin FGColor := clWhite; BGColor := clBlue; end;
    pcltError: begin FGColor := clWhite; BGColor := clMaroon; end;
    else Exit;
  end;

  if (memo.ParagraphsBackground[par] <> BGColor) or (memo.ParagraphsForeground[par] <> FGColor) then
  begin
    memo.ParagraphsBackground[par] := BGColor;
    memo.ParagraphsForeground[par] := FGColor;
  end;
{$ELSE}
begin
{$ENDIF}
end;


function TframeTextEditor.GetHelpTopic: string;
var
  x, tx: Integer;
  token, prevtoken: WideString;
begin
  if FEditorFormat <> efKMN then
    Exit(SHelpTopic_Context_TextEditor);

  if not GetSelectedTokens(token, prevtoken, x, tx) then
    Exit(SHelpTopic_Context_TextEditor);

  if not IsValidHelpToken(token, False) then
    if IsValidHelpToken(prevtoken, False) then
      token := prevtoken
    else if not IsValidHelpToken(token, True) then
      Exit(SHelpTopic_Context_TextEditor);

  Result := token;
end;

procedure TframeTextEditor.tmrUpdateSelectedTokenTimer(Sender: TObject);
var
  prevtoken, token: WideString;
  tx, x: Integer;
begin
  tmrUpdateSelectedToken.Enabled := False;

  if not GetSelectedTokens(token, prevtoken, x, tx) then Exit;

  if (EditorFormat <> efKMN) and (memo.SelText = '') then
    token := FormatUnicode(token);

  UpdateCharacterMap(False, token, x, tx, memo.SelText <> '');   // I4807

  if EditorFormat = efKMN then
  begin
    if not IsValidHelpToken(token, False) then
      if IsValidHelpToken(prevtoken, False) then
        token := prevtoken
      else if not IsValidHelpToken(token, True) then
        Exit;

    HelpKeyword := token;

    if Assigned(frmHelp) and frmHelp.Showing then
      frmHelp.QueueRefresh;
  end;
end;

function TframeTextEditor.GetSelectedTokens(var token, prevtoken: WideString; var x, tx: Integer): Boolean;
var
  ch: WideString;
begin
  x := memo.SelCol+1;

  if EditorFormat = efKMN then   // I4807
  begin
    if memo.SelLength < 0 then
      Inc(x, memo.SelLength);
    token := GetTokenAtCursor(memo.LinesArray[memo.SelLine], x, tx, prevtoken);
    if (memo.SelText <> '') and (token <> '') then
    begin
      prevtoken := memo.SelText;
      if (x > tx) and CharInSet(token[1], ['"', '''']) then
        prevtoken := token[1] + prevtoken + token[1];

      if not TKeyboardParser_Line.GetXStr(prevtoken, token) then
        token := memo.SelText;

      x := 1;
      tx := 1;
    end
    else
      token := GetTokenAtCursor(memo.LinesArray[memo.SelLine], x, tx, prevtoken);
  end
  else if memo.SelText <> '' then
  begin
    token := memo.SelText;
  end
  else
  begin
    ch := memo.GetTextPart(memo.SelStart-1, memo.SelStart);
    if ch = '' then Exit(False);

    if Uni_IsSurrogate2(ch[1]) then
      ch := memo.GetTextPart(memo.SelStart-2, memo.SelStart)
    else if Uni_IsSurrogate1(ch[1]) then
      ch := memo.GetTextPart(memo.SelStart-1, memo.SelStart+1);

    token := ch; //FormatUnicode(ch);
  end;

  Result := True;
end;

procedure TframeTextEditor.TntFormDestroy(Sender: TObject);
begin
  inherited;
{$IFDEF USE_PLUSMEMO}
  FreeAndNil(SyntaxHighlighter);  // I2794
{$ENDIF}
  if FFileName <> '' then
    modWebHttpServer.AppSource.UnregisterSource(FFileName);
end;

procedure TframeTextEditor.UpdateSelectedToken;
begin
  tmrUpdateSelectedToken.Enabled := False;
  tmrUpdateSelectedToken.Enabled := True;
end;

end.
