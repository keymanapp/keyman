(*  r
  Name:             UfrmTextEditor
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      27 Mar 2008

  Modified Date:    1 Sep 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          27 Mar 2008 - mcdurdin - I1248 - Initial version
                    14 Jun 2008 - mcdurdin - I1356 - Refresh fonts when they change
                    14 Jun 2008 - mcdurdin - I1449 - Resize hint bar to size of hint content
                    20 Jul 2008 - mcdurdin - I1534 - Show hint for non-Unicode keyboards
                    20 Jul 2008 - mcdurdin - I1530 - Only show a message on tell users to start Keyman
                    28 Jul 2008 - mcdurdin - I1571 - Help menu
                    16 Jan 2009 - mcdurdin - I1641 - Fix crash when focus is attempted and fails
                    29 Mar 2010 - mcdurdin - I2199 - Shift+Click in web browser opens new window
                    28 Jun 2010 - mcdurdin - I2421 - Return additional information for font coverage
                    05 Nov 2010 - mcdurdin - I2482 - Tutorial interferes when not active (part 1 - when hidden)
                    10 Dec 2010 - mcdurdin - I2558 - LazyWrite performance
                    17 Dec 2010 - mcdurdin - I2570 - Upgrade E-mbeddedWB (also I2393)
                    03 Feb 2011 - mcdurdin - I2260 - "Start Text Editor" should not start in "Tutorial" mode
                    03 Feb 2011 - mcdurdin - I2697 - Fix potential race condition on termination of check fonts thread
                    03 Feb 2011 - mcdurdin - I2698 - Font styling shortcuts for text editor
                    18 Feb 2011 - mcdurdin - I2721 - Override Javascript-disabled security for web controls
                    28 Feb 2011 - mcdurdin - I2720 - Prevent Keyman D_esktop splash from showing multiple copies
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    08 Jun 2012 - mcdurdin - I3309 - V9.0 - Migrate to Delphi XE2, VS2010, svn 1.7
                    02 Feb 2012 - mcdurdin - I3088 - Avoid crash when resizing hint bar and web browser not fully initialsied
                    03 Nov 2012 - mcdurdin - I3499 - V9.0 - Merge of I3088 - Avoid crash when resizing hint bar and web browser not fully initialised
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
                    03 Jul 2014 - mcdurdin - I3674 - V9.0 - Getting Started window gives instructions that are not valid for KM9
                    01 Sep 2014 - mcdurdin - I4397 - V9.0 - Get Started gets impatient and shows nag too quickly on start
                    01 Sep 2014 - mcdurdin - I4393 - V9.0 - Keyman D_esktop Free Edition polish

*)
unit UfrmTextEditor;  // I3306

interface

uses
  System.Contnrs,
  System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ActiveX,
  ToolWin, Menus,
  ActnList, ImgList, Keyman.UI.UframeCEFHost,
  utilcheckfonts, AppEvnts, KeymanTextEditorRichEdit,
  UfrmKeymanBase, UserMessages, System.Actions, System.ImageList;

type
  TfrmTextEditor = class(TfrmKeymanBase)
    editor: TKeymanTextEditorRichEdit;
    TntMainMenu1: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    Help1: TMenuItem;
    StatusBar: TStatusBar;
    StandardToolBar: TToolBar;
    CutButton: TToolButton;
    CopyButton: TToolButton;
    PasteButton: TToolButton;
    UndoButton: TToolButton;
    ToolButton10: TToolButton;
    FontName: TComboBox;
    ToolButton11: TToolButton;
    FontSize: TComboBox;
    ToolButton2: TToolButton;
    BoldButton: TToolButton;
    ItalicButton: TToolButton;
    UnderlineButton: TToolButton;
    ToolButton16: TToolButton;
    LeftAlign: TToolButton;
    CenterAlign: TToolButton;
    RightAlign: TToolButton;
    ToolButton20: TToolButton;
    BulletsButton: TToolButton;
    ToolbarImages: TImageList;
    ActionList1: TActionList;
    FileNewCmd: TAction;
    FileOpenCmd: TAction;
    FileSaveCmd: TAction;
    FilePrintCmd: TAction;
    FileExitCmd: TAction;
    FileSaveAsCmd: TAction;
    ActionList2: TActionList;
    EditUndoCmd: TAction;
    EditCutCmd: TAction;
    EditCopyCmd: TAction;
    EditPasteCmd: TAction;
    New1: TMenuItem;
    panFonts: TPanel;
    panEditor: TPanel;
    splitFonts: TSplitter;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Undo1: TMenuItem;
    N2: TMenuItem;
    Help2: TMenuItem;
    ApplicationEvents1: TApplicationEvents;
    mnuView: TMenuItem;
    mnuViewFontHelper: TMenuItem;
    procedure TntFormCreate(Sender: TObject);
    procedure TntFormDestroy(Sender: TObject);
    procedure TntFormClose(Sender: TObject; var Action: TCloseAction);
    procedure editorKeyPress(Sender: TObject; var Key: Char);
    procedure editorSelectionChange(Sender: TObject);
    procedure TntFormResize(Sender: TObject);
    procedure TntFormPaint(Sender: TObject);
    procedure TntFormShow(Sender: TObject);


    procedure AlignButtonClick(Sender: TObject);
    procedure BoldButtonClick(Sender: TObject);
    procedure BulletsButtonClick(Sender: TObject);
    procedure EditCopy(Sender: TObject);
    procedure EditCut(Sender: TObject);
    procedure EditPaste(Sender: TObject);
    procedure EditUndo(Sender: TObject);
    procedure FileExit(Sender: TObject);
    procedure FontNameChange(Sender: TObject);
    procedure FontSizeChange(Sender: TObject);
    procedure ItalicButtonClick(Sender: TObject);
    procedure ShowHint(Sender: TObject);
    procedure UnderlineButtonClick(Sender: TObject);
    procedure ActionList2Update(Action: TBasicAction; var Handled: Boolean);
    procedure FormShow_Editor;
    procedure RichEditChange(Sender: TObject);
    procedure editorKeymanSelectLang(Sender: TObject);
    procedure editorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure ApplicationEvents1Message(var Msg: tagMSG; var Handled: Boolean);
    procedure Help2Click(Sender: TObject);
    procedure mnuViewFontHelperClick(Sender: TObject);
    procedure editorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    cefFonts: TframeCEFHost;
    wm_keyman_control, wm_keyman_refresh: UINT;
    FUpdating: Boolean;
    FCheckFontsThread: TCheckFontsThread;
    FCheckFontKeyboards: TCheckFontKeyboards;
    FMustCheckFonts: Boolean;

    procedure CMFontChange(var Message: TMessage); message CM_FONTCHANGE;
    procedure WMUserFormShown(var Message: TMessage); message WM_USER_FormShown;
    procedure WMUserCheckFonts(var Message: TMessage); message WM_USER_CheckFonts;
    procedure PostKeymanControlMessage(msg, wParam: UINT; lParam: Cardinal);
    {function SendKeymanControlMessage(msg, wParam: UINT;
      lParam: Cardinal): Cardinal;}
    function CurrText: TTextAttributes;
    procedure GetFontNames;
    procedure SetEditRect;
    procedure FormCreate_Editor;
    procedure FormResize_Editor;
    procedure FormPaint_Editor;

    procedure SetModified(Value: Boolean);
    procedure UpdateCursorPos;

    procedure CheckKeyboardFonts(FSetFont: Boolean);
    procedure CheckFontsThreadComplete(Sender: TObject);
    procedure LoadWebBox(web: TframeCEFHost; const AdditionalData: WideString = '');   // I4181

    procedure HideFontsBox;
    procedure FireCommand(const command: WideString; params: TStringList);
    procedure ResetAutoKeyboard;
    procedure StartCheckFontsThread;
    procedure cefCommand(Sender: TObject; const command: string;
      params: TStringList);
  protected
    class function ShouldRegisterWindow: Boolean; override;  // I2720
  public
  end;

procedure OpenTextEditor(Owner: TComponent = nil);  // I2260

implementation

{$R *.dfm}

uses
  MessageIdentifierConsts,
  MessageIdentifiers,
  findfonts,
  HintConsts,
  inifiles,
  initprog,
  KeymanControlMessages,
  KLog,
  keymanapi_TLB,
  Keyman.Configuration.System.UmodWebHttpServer,
  Keyman.Configuration.System.HttpServer.App.TextEditorFonts,
  kmint,
  KMShellHints,
  ErrorControlledRegistry,
  RegistryKeys,
  RichEdit,
  ShellApi,
  ttInfo,
  UfrmHTML,
  utilhttp,
  utilsystem,
  UtilExecute,
  utilxml,
  UfrmWebContainer;

resourcestring
  sSaveChanges = 'Save changes to %s?';
  sOverWrite = 'OK to overwrite %s';
  sUntitled = 'Untitled';
  sModified = 'Modified';
  sColRowInfo = 'Line: %3d   Col: %3d';

const
  GutterWid = 6;

procedure OpenTextEditor(Owner: TComponent = nil);  // I2260
var
  frmTextEditor: TfrmTextEditor;
begin
  if ApplicationRunning then
  begin
    frmTextEditor := TfrmTextEditor.Create(nil);
    frmTextEditor.Show;
  end
  else
  begin
    UfrmWebContainer.CreateForm(TfrmTextEditor, frmTextEditor);
    frmTextEditor.Visible := True;
  end;
end;

{ TfrmTextEditor }

procedure TfrmTextEditor.TntFormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  cefFonts.Free;
  Action := caFree;  // I2482
end;

procedure TfrmTextEditor.TntFormCreate(Sender: TObject);
begin
  inherited;
  wm_keyman_control := RegisterWindowMessage('WM_KEYMAN_CONTROL');
  wm_keyman_refresh := RegisterWindowMessage('WM_KEYMANREFRESH');

  HelpTopic := 'context/text-editor';
  Caption := MsgFromId(SKTextEditorCaption);

  FCheckFontKeyboards := TCheckFontKeyboards.Create;

  StartCheckFontsThread;
  HideFontsBox;

  cefFonts := TframeCEFHost.Create(Self);
  cefFonts.Parent := panFonts;
  cefFonts.Visible := True;
  cefFonts.ShouldOpenRemoteUrlsInBrowser := True;
  cefFonts.OnCommand := cefCommand;

  FormCreate_Editor;
end;

procedure TfrmTextEditor.StartCheckFontsThread;
var
  i: Integer;
  t: TThread;
begin
  if Assigned(FCheckFontsThread) then
  begin
    t := FCheckFontsThread;  // I2697
    FCheckFontsThread := nil;
    t.OnTerminate := nil;
    t.Terminate;
  end;

  FCheckFontsThread := TCheckFontsThread.Create;
  FCheckFontsThread.FreeOnTerminate := True;
  FCheckFontsThread.OnTerminate := CheckFontsThreadComplete;

  kmcom.Packages.Refresh;
  kmcom.Keyboards.Refresh;

  KL.Log('Checking fonts for keyboards - %d keyboards installed', [kmcom.Keyboards.Count]);

  for i := 0 to kmcom.Keyboards.Count - 1 do
  begin
    if ((kmcom.Keyboards[i].Encodings and keUnicode) = keUnicode) and kmcom.Keyboards[i].Loaded then
    begin
      KL.Log('Checking fonts for '+kmcom.Keyboards[i].ID);
      FCheckFontsThread.AddKeyboard(kmcom.Keyboards[i].ID, kmcom.Keyboards[i].Filename, kmcom.Keyboards[i].GetCharsUsed);
    end;
  end;

  FCheckFontsThread.Start;  // I3309
end;

procedure TfrmTextEditor.CheckFontsThreadComplete(Sender: TObject);
var
  i: Integer;
begin
  if not Assigned(FCheckFontsThread) then Exit;  // I2697
  FCheckFontsThread.Keyboards.OwnsObjects := False;
  FCheckFontKeyboards.Clear;
  for i := 0 to FCheckFontsThread.Keyboards.Count - 1 do
    FCheckFontKeyboards.Add(FCheckFontsThread.Keyboards[i]);
  FCheckFontsThread := nil;

  if FMustCheckFonts and mnuViewFontHelper.Checked then
    CheckKeyboardFonts(True);
end;

procedure TfrmTextEditor.TntFormDestroy(Sender: TObject);
begin
  inherited;
  PostKeymanControlMessage(KMC_NOTIFYWELCOME, NW_NOTIFYHANDLE, 0);
end;

procedure TfrmTextEditor.TntFormPaint(Sender: TObject);
begin
  inherited;
  FormPaint_Editor;
end;

procedure TfrmTextEditor.TntFormResize(Sender: TObject);
begin
  inherited;
  FormResize_Editor;
end;

procedure TfrmTextEditor.TntFormShow(Sender: TObject);
begin
  inherited;
  FormShow_Editor;
end;

procedure TfrmTextEditor.PostKeymanControlMessage(msg: UINT; wParam: UINT; lParam: Cardinal);
var
  hKeymanControl: THandle;
begin
  hKeymanControl := FindWindow('TfrmKeyman7Main', nil);   // I3674
  if hKeymanControl <> 0 then
    PostMessage(hKeymanControl, wm_keyman_control, MAKELONG(msg, wParam), lParam);
end;

{function TfrmTextEditor.SendKeymanControlMessage(msg: UINT; wParam: UINT; lParam: Cardinal): Cardinal;
var
  hKeymanControl: THandle;
begin
  hKeymanControl := FindWindow('TApplication', 'keyman');
  if hKeymanControl <> 0 then
    Result := SendMessage(hKeymanControl, wm_keyman_control, MAKELONG(msg, wParam), lParam)
  else
    Result := 0;
end;}


procedure TfrmTextEditor.LoadWebBox(web: TframeCEFHost; const AdditionalData: WideString = '');   // I4181
var
  Data: ITextEditorFontsSharedData;
  PageTag: Integer;
begin
  Data := TTextEditorFontsSharedData.Create(AdditionalData);
  PageTag := modWebHttpServer.SharedData.Add(Data);
  web.Navigate(modWebHttpServer.Host + '/page/welcome_fonts?tag='+IntToStr(PageTag));
end;

procedure TfrmTextEditor.mnuViewFontHelperClick(Sender: TObject);
begin
  mnuViewFontHelper.Checked := not mnuViewFontHelper.Checked;
  if not mnuViewFontHelper.Checked
    then HideFontsBox
    else CheckKeyboardFonts(False);
end;

procedure TfrmTextEditor.editorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [ssCtrl] then  // 2698
  begin
    case Key of
      Ord('B'): begin BoldButton.Down := not BoldButton.Down; BoldButtonClick(BoldButton); end;
      Ord('I'): begin ItalicButton.Down := not ItalicButton.Down; ItalicButtonClick(ItalicButton); end;
      Ord('U'): begin UnderlineButton.Down := not UnderlineButton.Down; UnderlineButtonClick(UnderlineButton); end;
      Ord('L'): begin LeftAlign.Down := True; AlignButtonClick(LeftAlign); end;
      Ord('E'): begin CenterAlign.Down := True; AlignButtonClick(CenterAlign); end;
      Ord('R'): begin RightAlign.Down := True; AlignButtonClick(RightAlign); end;
      else
      begin
        ResetAutoKeyboard;
        Exit;
      end;
    end;
    Key := 0;
  end
  else if (Key = VK_TAB) and (Shift = []) then  // I2698 - override for Ctrl+I
  begin
    editor.SelText := #9;
    Key := 0;
  end
  else
    ResetAutoKeyboard;
end;

procedure TfrmTextEditor.ResetAutoKeyboard;
var
  n: DWord;
begin
  n := SendMessage(editor.Handle, EM_GETLANGOPTIONS, 0, 0);
  if (n and IMF_AUTOKEYBOARD) <> 0 then
  begin
    SendMessage(editor.Handle, EM_SETLANGOPTIONS, 0, n and not IMF_AUTOKEYBOARD);
  end;
end;

procedure TfrmTextEditor.editorKeymanSelectLang(Sender: TObject);
begin
  PostMessage(Handle, WM_USER_CheckFonts, 0, 0);
end;

procedure TfrmTextEditor.editorKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #9 then  // I2698 - override for Ctrl+I
  begin
    Key := #0;
  end;
end;

procedure TfrmTextEditor.editorKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [ssCtrl] then  // I2698
    case Key of
      Ord('B'),
      Ord('I'),
      Ord('U'),
      Ord('L'),
      Ord('E'),
      Ord('R'): Key := 0;
    end;
end;

procedure TfrmTextEditor.WMUserCheckFonts(var Message: TMessage);
begin
  if mnuViewFontHelper.Checked then
    CheckKeyboardFonts(True);
end;

procedure TfrmTextEditor.WMUserFormShown(var Message: TMessage);
begin
  CheckKeyboardFonts(False);
end;

{------------------------------------------------------------------------------------------------}

procedure TfrmTextEditor.editorSelectionChange(Sender: TObject);
begin
  with Editor.Paragraph do
  try
    FUpdating := True;
    BoldButton.Down := fsBold in Editor.SelAttributes.Style;
    ItalicButton.Down := fsItalic in Editor.SelAttributes.Style;
    UnderlineButton.Down := fsUnderline in Editor.SelAttributes.Style;
    BulletsButton.Down := Boolean(Numbering);
    FontSize.Text := IntToStr(Editor.SelAttributes.Size);
    FontName.Text := Editor.SelAttributes.Name;
    case Ord(Alignment) of
      0: LeftAlign.Down := True;
      1: RightAlign.Down := True;
      2: CenterAlign.Down := True;
    end;
    UpdateCursorPos;
  finally
    FUpdating := False;
  end;
end;


function TfrmTextEditor.CurrText: TTextAttributes;
begin
  //if Editor.SelLength > 0 then
  Result := Editor.SelAttributes;
  //else Result := Editor.DefAttributes;
end;

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
begin
  TStrings(Data).Add(LogFont.lfFaceName);
  Result := 1;
end;

procedure TfrmTextEditor.GetFontNames;
var
  DC: HDC;
begin
  DC := GetDC(0);
  FontName.Clear;
  EnumFonts(DC, nil, @EnumFontsProc, Pointer(FontName.Items));
  ReleaseDC(0, DC);
  FontName.Sorted := True;
end;

procedure TfrmTextEditor.Help2Click(Sender: TObject);
begin
  Application.HelpJump(HelpTopic);
end;

procedure TfrmTextEditor.HideFontsBox;
begin
  panFonts.Visible := False;
  splitFonts.Visible := False;
end;

procedure TfrmTextEditor.CheckKeyboardFonts(FSetFont: Boolean);
var
  FFonts: TFindFontList;
  FKeyboard: TCheckFontKeyboard;
  n: Integer;
  lang: IKeymanLanguage;

  procedure AddFonts(const path, ext: WideString);
  var
    f: TSearchRec;
  begin
    if FindFirst(path + '*'+ ext, 0, f) = 0 then
    begin
      repeat
        with TTTInfo.Create(path + f.Name, [tfNames]) do
        try
          FFonts.Add(FullName, -1, '', '');
        finally
          Free;
        end;
      until FindNext(f) <> 0;
      FindClose(f);
    end;
  end;

var
  FFontsData: WideString;
  i: Integer;
begin
  if Assigned(FCheckFontsThread) then
  begin
    FMustCheckFonts := True;
    Exit;
  end;

  FMustCheckFonts := False;

  panFonts.Visible := True;
  splitFonts.Visible := True;
  splitFonts.Left := panFonts.Left - splitFonts.Width;

  lang := kmcom.Control.ActiveLanguage;
  if (lang = nil) or (lang.KeymanKeyboardLanguage = nil) then
  begin
    LoadWebBox(cefFonts, '<not_keyman />');   // I4181
    //HideFontsBox;
    Exit;
  end;

  FFonts := TFindFontList.Create;
  try
    FKeyboard := FCheckFontKeyboards.Keyboards[lang.KeymanKeyboardLanguage.OwnerKeyboard.ID];
    if not Assigned(FKeyboard) then
    begin
      LoadWebBox(cefFonts); // I1534 - show hint for non-Unicode keyboards   // I4181
      Exit;
    end;

    for I := 0 to FKeyboard.Fonts.Count - 1 do
    begin
      n := FFonts.IndexOfName(FKeyboard.Fonts[i].FontName);
      if n < 0
        then FFonts.Add(FKeyboard.Fonts[i].FontName, FKeyboard.Fonts[i].Coverage, FKeyboard.Fonts[i].IncludedChars, FKeyboard.Fonts[i].ExcludedChars)
        else
        begin
          FFonts[n].Coverage := FKeyboard.Fonts[i].Coverage;
          FFonts[n].IncludedChars := FKeyboard.Fonts[i].IncludedChars;
          FFonts[n].ExcludedChars := FKeyboard.Fonts[i].ExcludedChars;
        end;
    end;

    FFontsData := '<Fonts>';
    for I := 0 to FKeyboard.Fonts.Count - 1 do
      FFontsData := FFontsData + '<Font Name="'+XmlEncode(FKeyboard.Fonts[I].FontName)+'" Coverage="'+IntToStr(FKeyboard.Fonts[i].Coverage)+'" />';
    FFontsData := FFontsData + '</Fonts>';

    LoadWebBox(cefFonts, FFontsData);   // I4181

    if (FKeyboard.Fonts.Count > 0) and FSetFont then
      CurrText.Name := FKeyboard.Fonts[0].FontName;
  finally
    FFonts.Free;
  end;
end;

procedure TfrmTextEditor.CMFontChange(var Message: TMessage);
begin
  GetFontNames;
end;

procedure TfrmTextEditor.SetEditRect;
var
  R: TRect;
begin
  with Editor do
  begin
    R := Rect(GutterWid, 0, ClientWidth-GutterWid, ClientHeight);
    SendMessage(Handle, EM_SETRECT, 0, Longint(@R));
  end;
end;

procedure TfrmTextEditor.FormCreate_Editor;
    procedure GetFontSizes;
    var
      i: Integer;
    const
      FFontSizes: array[0..15] of Integer = (8,9,10,11,12,14,16,18,20,22,24,26,28,36,48,72);
    begin
      for i := 0 to High(FFontSizes) do
        FontSize.Items.Add(IntToStr(FFontSizes[i]));
    end;
begin
  Application.OnHint := ShowHint;
  GetFontNames;
  GetFontSizes;
  EditorSelectionChange(Self);

  CurrText.Name := 'Arial';
  CurrText.Size := 36;
end;

class function TfrmTextEditor.ShouldRegisterWindow: Boolean; // I2720
begin
  Result := True;
end;

procedure TfrmTextEditor.ShowHint(Sender: TObject);
begin
  if Length(Application.Hint) > 0 then
  begin
    StatusBar.SimplePanel := True;
    StatusBar.SimpleText := Application.Hint;
  end
  else StatusBar.SimplePanel := False;
end;

procedure TfrmTextEditor.FileExit(Sender: TObject);
begin
  Close;
end;

procedure TfrmTextEditor.EditUndo(Sender: TObject);
begin
  with Editor do
    if HandleAllocated then SendMessage(Handle, EM_UNDO, 0, 0);
end;

procedure TfrmTextEditor.EditCut(Sender: TObject);
begin
  Editor.CutToClipboard;
end;

procedure TfrmTextEditor.EditCopy(Sender: TObject);
begin
  Editor.CopyToClipboard;
end;

procedure TfrmTextEditor.EditPaste(Sender: TObject);
begin
  Editor.PasteFromClipboard;
end;

procedure TfrmTextEditor.FormResize_Editor;
begin
  SetEditRect;
  EditorSelectionChange(Self);
end;

procedure TfrmTextEditor.FormPaint_Editor;
begin
  SetEditRect;
end;

procedure TfrmTextEditor.BoldButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  if BoldButton.Down then
    CurrText.Style := CurrText.Style + [fsBold]
  else
    CurrText.Style := CurrText.Style - [fsBold];
end;

procedure TfrmTextEditor.ItalicButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  if ItalicButton.Down then
    CurrText.Style := CurrText.Style + [fsItalic]
  else
    CurrText.Style := CurrText.Style - [fsItalic];
end;

procedure TfrmTextEditor.FontSizeChange(Sender: TObject);
begin
  if FUpdating then Exit;
  CurrText.Size := StrToInt(FontSize.Text);
end;

procedure TfrmTextEditor.AlignButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  Editor.Paragraph.Alignment := TAlignment(TControl(Sender).Tag);
end;

procedure TfrmTextEditor.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);
begin
  if (Msg.message = wm_keyman_refresh) and (Msg.wParam = KR_REFRESH) then
  begin
    StartCheckFontsThread;
  end;
  inherited;
end;

procedure TfrmTextEditor.FontNameChange(Sender: TObject);
begin
  if FUpdating then Exit;
  CurrText.Name := FontName.Items[FontName.ItemIndex];
end;

procedure TfrmTextEditor.UnderlineButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  if UnderlineButton.Down then
    CurrText.Style := CurrText.Style + [fsUnderline]
  else
    CurrText.Style := CurrText.Style - [fsUnderline];
end;

procedure TfrmTextEditor.BulletsButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  Editor.Paragraph.Numbering := TNumberingStyle(BulletsButton.Down);
end;

procedure TfrmTextEditor.UpdateCursorPos;
var
  CharPos: TPoint;
begin
  CharPos.Y := SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0,
    Editor.SelStart);
  CharPos.X := (Editor.SelStart -
    SendMessage(Editor.Handle, EM_LINEINDEX, CharPos.Y, 0));
  Inc(CharPos.Y);
  Inc(CharPos.X);
  StatusBar.Panels[0].Text := Format(sColRowInfo, [CharPos.Y, CharPos.X]);
end;

procedure TfrmTextEditor.cefCommand(Sender: TObject; const command: string; params: TStringList);
begin
  FireCommand(command, params);
end;

procedure TfrmTextEditor.FormShow_Editor;
begin
  UpdateCursorPos;
  RichEditChange(nil);
  if editor.CanFocus then // I1641
    Editor.SetFocus;
end;

procedure TfrmTextEditor.RichEditChange(Sender: TObject);
begin
  SetModified(Editor.Modified);
end;

procedure TfrmTextEditor.SetModified(Value: Boolean);
begin
  if Value then StatusBar.Panels[1].Text := sModified
  else StatusBar.Panels[1].Text := '';
end;

procedure TfrmTextEditor.ActionList2Update(Action: TBasicAction;
  var Handled: Boolean);
begin
 { Update the status of the edit commands }
  EditCutCmd.Enabled := Editor.SelLength > 0;
  EditCopyCmd.Enabled := EditCutCmd.Enabled;
  if Editor.HandleAllocated then
  begin
    EditUndoCmd.Enabled := Editor.Perform(EM_CANUNDO, 0, 0) <> 0;
    EditPasteCmd.Enabled := Editor.Perform(EM_CANPASTE, 0, 0) <> 0;
  end;
end;

procedure TfrmTextEditor.FireCommand(const command: WideString; params: TStringList);
var
  kbd: IKeymanKeyboardInstalled;

  pkg: IKeymanPackage;
  lang: IKeymanLanguage;
begin
  if command = 'selectfont' then
  begin
    cefFonts.SetFocus;
    if editor.CanFocus then // I1641
      editor.SetFocus;
    CurrText.Name := params.Values['font'];
  end
  else if command = 'welcome' then
  begin
    lang := kmcom.Control.ActiveLanguage;
    if lang = nil then Exit;
    if lang.KeymanKeyboardLanguage = nil then Exit;
    kbd := lang.KeymanKeyboardLanguage.OwnerKeyboard;
    if kbd = nil then Exit;
    pkg := kbd.OwnerPackage;
    if pkg <> nil
      then DoShowPackageWelcome(pkg, True)
      else ShowMessage(MsgFromIdFormat(SKPackageDoesNotIncludeWelcome, ['']));
  //  kmcom.Control.ActiveKeyboard.
  end
  else if command = 'link' then
    TUtilExecute.URL(params.Values['url']);  // I3349
//    ShowKeyboardWelcome;
end;

end.
