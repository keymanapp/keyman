(*
  Name:             UframeTextEditor
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    23 Feb 2016
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Initial version
                    14 Sep 2006 - mcdurdin - Add find and replace dialogs
                    14 Sep 2006 - mcdurdin - Add LoadFromStream and SaveToStream
                    14 Sep 2006 - mcdurdin - Add ConvertCharacter and GetWideCodes functions
                    28 Sep 2006 - mcdurdin - Add context help and character map lookup
                    06 Oct 2006 - mcdurdin - Only update help if form is visible
                    04 Dec 2006 - mcdurdin - Update help only when help form visible
                    12 Dec 2006 - mcdurdin - Add Print, Print Preview
                    04 Jan 2007 - mcdurdin - Add help support
                    25 Jan 2007 - mcdurdin - Delete dlgSave unused component
                    19 Mar 2007 - mcdurdin - I712 - Fix character map should follow selected character
                    30 May 2007 - mcdurdin - I781 - Search and Replace dialogs now support Unicode
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    14 Jun 2008 - mcdurdin - I1426 - Fixup script tag font
                    18 Mar 2011 - mcdurdin - I2794 - Fix memory leak
                    08 Jul 2011 - mcdurdin - I2971 - Branding Pack locale.xml not editing as UTF-8
                    18 May 2012 - mcdurdin - I3323 - V9.0 - Change from Plus-MemoU to Plus-Memo
                    08 Jun 2012 - mcdurdin - I3337 - V9.0 - Review of input/output for Unicode
                    06 Feb 2012 - mcdurdin - I3082 - Reload text file with specific encoding support, not marking as unmodified after reload
                    03 Nov 2012 - mcdurdin - I3502 - V9.0 - Merge of I3082 - Reload text file with specific encoding support
                    13 Dec 2012 - mcdurdin - I3637 - V9.0 - I3502 Fail - Reload as Format button is disabled in some contexts
                    01 Jan 2013 - mcdurdin - I3636 - V9.0 - File format dropdown shows wrong value
                    10 Jan 2014 - mcdurdin - I4021 - V9.0 - Redesign Keyboard Wizard to integrate V9 features
                    07 Feb 2014 - mcdurdin - I4034 - V9.0 - Restructure keyboard wizard for source views and features
                    27 Feb 2014 - mcdurdin - I4083 - V9.0 - When errors encountered in JSON layout file, locate the error in the source view
                    27 May 2015 - mcdurdin - I4616 - Developer crashes when saving or switching views due to locked files [CrashID:tike.exe_9.0.481.0_004587D3_EFOpenError]
                    27 May 2015 - mcdurdin - I4721 - V9.0 - Developer crashes if a file is in use and a reload is attempted
                    27 May 2015 - mcdurdin - I4499 - Developer crashes switching to source tab when line number is too high [CrashID:tike.exe_9.0.466.0_00698269_ERangeError]
                    27 May 2015 - mcdurdin - I4655 - Developer crashes when changing font settings and in code view for touch layout if not on first line [CrashID:tike.exe_9.0.487.0_0069BE51_ERangeError]
                    22 Jun 2015 - mcdurdin - I4765 - Double-click on message does not find source line since build 500
                    24 Jul 2015 - mcdurdin - I4797 - Convert to characters tool is inconsistent
                    03 Aug 2015 - mcdurdin - I4807 - Add Character Identifier to Keyman Developer
                    24 Aug 2015 - mcdurdin - I4870 - Editor does not always refresh immediately with new theming
                    06 Nov 2015 - mcdurdin - I4918 - Text editor is not refreshing correctly with new theme
                    23 Feb 2016 - mcdurdin - I4962 - Redraw not reliably working in text editor
*)
unit UframeTextEditor;  // I3323   // I4797

interface

uses
  System.Types,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ImgList,
  MenuImgList, ExtCtrls,

{$IFDEF USE_PLUSMEMO}
  SyntaxHighlight,
  PlusMemo,
  PlusGutter,
  ExtHilit,
  HtmlHighlight,
  pmprint,
  PMSupport,
{$ENDIF}

  TextFileFormat, UfrmTike,
  System.ImageList, KeymanDeveloperMemo,
  Vcl.StdCtrls;

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
    function UpdateHelp(token: WideString): Boolean;
  public
    { Public declarations }
    procedure GetHelpTopic(var s: string); override;
    procedure UpdateParColour(par: Integer; LineType: TParColourLineType);
    procedure SetFocus; override;

    procedure FindError(ln: Integer);
    procedure FindErrorByOffset(offset: Integer);   // I4083
    function OffsetToLine(Offset: Integer): Integer;   // I4083

    function GetHelpToken: WideString;

    procedure EditFind;
    procedure EditFindNext;
    procedure EditReplace;

    procedure SyntaxColourChange;

    function PrintFile(Header: WideString = ''): Boolean;
    function PrintPreview(Header: WideString = ''): Boolean;

    procedure LoadFromFile(AFileName: WideString); overload;   // I4034
    procedure LoadFromFile(AFileName: WideString; ATextFileFormat: TTextFileFormat); overload;   // I4034
    procedure SaveToFile(AFileName: WideString);
    procedure LoadFromStream(AStream: TStream; ATextFileFormat: TTextFileFormat); overload;  // I2964
    procedure LoadFromStream(AStream: TStream); overload;  // I2964
    procedure SaveToStream(AStream: TStream);

    property EditorText: WideString read GetText write SetText;
    property EditorFormat: TEditorFormat read FEditorFormat write SetEditorFormat;
    property OnChanged: TNotifyEvent read FOnChanged write FonChanged;

    property CodeFont: TFont read GetCodeFont write SetCodeFont;
    property CharFont: TFont read GetCharFont write SetCharFont;
    property TextFileFormat: TTextFileFormat read FTextFileFormat write SetTextFileFormat;
  end;

implementation

uses
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
  Unicode,
  utilstr;
  
{$R *.dfm}

{ TframeTextEditor }

procedure TframeTextEditor.RefreshOptions;
begin
  memo.TabStops   := FKeymanDeveloperOptions.IndentSize;
end;

procedure TframeTextEditor.Changed;
begin
  memo.Update; // required due to bug in Delphi's TStyleHook which causes invalidated areas to be lost because   // I4962
    // WM_SETREDRAW is set on the window when the caption is changed, due to the {*} character being added
    // to the caption.   I4870   // I4918 undoes memo.Update for more performant painting
  if Assigned(FOnChanged) then FOnChanged(Self);
  UpdateState;
end;

procedure TframeTextEditor.FormCreate(Sender: TObject);
begin
  inherited;
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
  Result := memo.Text;
end;

procedure TframeTextEditor.LoadFromFile(AFileName: WideString);
var
  fs: TFileStream;
begin
  FLoading := True;
  try
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
  finally
    FLoading := False;
  end;
end;

procedure TframeTextEditor.LoadFromFile(AFileName: WideString;
  ATextFileFormat: TTextFileFormat);   // I4034
var
  fs: TFileStream;
begin
  FLoading := True;
  try
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
    memo.Modified := False;
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
begin
  case FTextFileFormat of
    tffANSI:  memo.Lines.SaveToStream(AStream, TEncoding.Default);  // I3337
    tffUTF8:  memo.Lines.SaveToStream(AStream, TEncoding.UTF8);
    tffUTF16: memo.Lines.SaveToStream(AStream, TEncoding.Unicode);
  end;
  memo.Modified := False;
end;

procedure TframeTextEditor.memoBeforeChange(Sender: TObject; var Txt: PWideChar);
begin
  ClearError;
end;

procedure TframeTextEditor.memoChange(Sender: TObject);
begin
  if not FLoading then Changed;
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

function TframeTextEditor.GetHelpToken: WideString;
var
  x, tx: Integer;
  prevtoken: WideString;
begin
  x := memo.SelCol+1;

  if EditorFormat = efKMN
    then Result := GetTokenAtCursor(memo.LinesArray[memo.SelLine], x, tx, prevtoken)
    else Result := '';

  if not IsValidHelpToken(Result) then
  begin
    Result := prevtoken;
    if not IsValidHelpToken(Result) then Result := '';
  end;
end;

procedure TframeTextEditor.GetHelpTopic(var s: string);
begin
  if FEditorFormat <> efKMN then Exit;

  s := memo.SelText;
  if s = '' then
  begin
{$IFDEF USE_PLUSMEMO}
    memo.Delimiters := [#9, ' ', '.', ',', ';', ':', '=', '<', '>', '(', ')'];
{$ENDIF}
    s := memo.CurrentWord;
  end;

  s := kwhelp.HelpKeyword(s);
end;

function TframeTextEditor.UpdateHelp(token: WideString): Boolean;
begin
  Result := False;
  if not IsValidHelpToken(token) then Exit;
  if Assigned(frmHelp) and frmHelp.Showing then frmHelp.LoadHelp(token, 'KMN');
  Result := True;
end;

procedure TframeTextEditor.tmrUpdateSelectedTokenTimer(Sender: TObject);
var
  prevtoken, token: WideString;
  tx, x: Integer;
  ch: WideString;
begin
  tmrUpdateSelectedToken.Enabled := False;

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
    if ch = '' then Exit;

    if Uni_IsSurrogate2(ch[1]) then
      ch := memo.GetTextPart(memo.SelStart-2, memo.SelStart)
    else if Uni_IsSurrogate1(ch[1]) then
      ch := memo.GetTextPart(memo.SelStart-1, memo.SelStart+1);

    token := FormatUnicode(ch);
  end;

  UpdateCharacterMap(False, token, x, tx, memo.SelText <> '');   // I4807
  if EditorFormat = efKMN then
    if not UpdateHelp(token) then UpdateHelp(prevtoken);
end;

procedure TframeTextEditor.TntFormDestroy(Sender: TObject);
begin
  inherited;
{$IFDEF USE_PLUSMEMO}
  FreeAndNil(SyntaxHighlighter);  // I2794
{$ENDIF}
end;

procedure TframeTextEditor.UpdateSelectedToken;
begin
  tmrUpdateSelectedToken.Enabled := False;
  tmrUpdateSelectedToken.Enabled := True;
end;

end.
