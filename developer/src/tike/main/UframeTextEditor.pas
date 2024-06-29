unit UframeTextEditor;  // I3323   // I4797

interface

uses
  System.Classes,
  System.ImageList,
  System.JSON,
  System.SysUtils,
  System.Types,
  System.Variants,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.ImgList,
  Vcl.Menus,
  Vcl.StdCtrls,
  Winapi.Messages,
  Winapi.Windows,

  uCEFConstants,
  uCEFInterfaces,
  uCEFTypes,

  Keyman.UI.UframeCEFHost,
  KMDActionInterfaces,
  MenuImgList,
  TextFileFormat,
  UfrmTike,
  UserMessages;

type
  TEditorBreakpointClickedEvent = procedure(Sender: TObject; ALine: Integer) of object;

  TframeTextEditor = class(TTIKEForm, IKMDEditActions, IKMDSearchActions, IKMDTextEditorActions)
    lstImages: TMenuImgList;
    dlgFonts: TFontDialog;
    dlgPrintSetup: TPrinterSetupDialog;
    lstImagesDisabled: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TntFormDestroy(Sender: TObject);
  private
    class var FInitialFilenameIndex: Integer;
  private
    FCharFont, FCodeFont: TFont;
    FSelectedRange: TRect;
    FSelectedRangeIsBackwards: Boolean;
    FHasBeenLoaded: Boolean;
    FCanUndo: Boolean;
    FCanRedo: Boolean;
    FHasSelection: Boolean;
    FLoading: Boolean;
    FEditorFormat: TEditorFormat;
    FOnChanged: TNotifyEvent;
    FTextFileFormat: TTextFileFormat;

    cef: TframeCEFHost;
    FFilename: string;
    FOnBreakpointClicked: TEditorBreakpointClickedEvent;
    FErrorLine: Integer;

    procedure RefreshOptions;
    function GetText: WideString;
    procedure SetText(Value: WideString);
    procedure SetEditorFormat(const Value: TEditorFormat);
    procedure Changed;
    procedure UpdateState(ALocation: string);
    procedure ClearError;
    procedure SetCharFont(const Value: TFont);
    procedure SetCodeFont(const Value: TFont);
    function GetCharFont: TFont;
    function GetCodeFont: TFont;
    procedure SetTextFileFormat(const Value: TTextFileFormat);

    procedure cefCommand(Sender: TObject; const command: string; params: TStringList);
    procedure cefBeforeContextMenu(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure cefContextMenuCommand(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: Integer;
      eventFlags: Cardinal; out Result: Boolean);
    procedure cefPreKeySyncEvent(Sender: TObject; e: TCEFHostKeyEventData; out isShortcut, Handled: Boolean);
    procedure cefKeyEvent(Sender: TObject; e: TCEFHostKeyEventData; wasShortcut, wasHandled: Boolean);
    procedure cefLoadEnd(Sender: TObject);
    procedure cefBeforeBrowseSync(Sender: TObject; const Url: string; isPopup: Boolean; out Handled: Boolean);
    procedure cefBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);

    procedure WMUser_TextEditor_Command(var Message: TMessage); message WM_USER_TextEditor_Command;
    procedure WMUser_SyntaxColourChange(var Message: TMessage); message WM_USER_SYNTAXCOLOURCHANGE;
    procedure FireCommand(const commands: TStringList);
    procedure LoadFileInBrowser(const AData: string);
    procedure UpdateInsertState(const AMode: string);
    procedure ExecuteCommand(const command: string; const parameters: TJSONValue = nil);
    procedure ExecuteLineCommand(ALine: Integer; const command: string);
    procedure UpdateToken(command: string);
    procedure SetCursorPosition(AColumn, ARow: Integer);
    procedure DoBreakpointClicked(const line: string);
    procedure UpdateEditorFonts;
    procedure CharMapDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure CharMapDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DelayedFindError;
    procedure DoOpenLinkIfExternal(const url: string; var handled: Boolean);
    procedure cefHelpTopic(Sender: TObject);

  protected
    function GetHelpTopic: string; override;

    procedure SaveToStream(AStream: TStream);

    { IKMDSearchActions }
    procedure EditFind;
    procedure EditFindNext;
    procedure EditReplace;
    function CanEditFind: Boolean;
    function CanEditFindNext: Boolean;

    { IKMDEditActions }
    function IKMDTextEditorActions_GetText: string;
    procedure IKMDTextEditorActions_SetText(const Value: string);
    function IKMDTextEditorActions.GetText = IKMDTextEditorActions_GetText;
    procedure IKMDTextEditorActions.SetText = IKMDTextEditorActions_SetText;
    procedure CutToClipboard;
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    procedure Undo;
    procedure Redo;   // I4032
    procedure SelectAll;
    procedure ClearSelection;
    function CanCut: Boolean;
    function CanCopy: Boolean;
    function CanPaste: Boolean;
    function CanUndo: Boolean;
    function CanRedo: Boolean;   // I4032
    function CanSelectAll: Boolean;
    function CanClearSelection: Boolean;

    { IKMDTextEditorActions }
    function GetEditorFormat: TEditorFormat;
    function GetSelectedRow: Integer;
    function GetSelectedCol: Integer;
    function GetSelectedRange: TRect;
    procedure ReplaceSelection(ARange: TRect; const ANewText: string);

  public
    { Public declarations }
    procedure DebugSetBreakpoint(ALine: Integer);
    procedure DebugClearBreakpoint(ALine: Integer);
    procedure DebugUpdateExecutionPoint(ALine: Integer);
    property OnBreakpointClicked: TEditorBreakpointClickedEvent read FOnBreakpointClicked write FOnBreakpointClicked;

    procedure SetFocus; override;
    procedure SetupCharMapDrop;

    procedure FindError(ln: Integer);
    procedure FindErrorByOffset(offset: Integer);   // I4083
    function OffsetToLine(Offset: Integer): Integer;   // I4083

    procedure SyntaxColourChange;

    function PrintFile(Header: WideString = ''): Boolean;
    function PrintPreview(Header: WideString = ''): Boolean;

    procedure LoadFromFile(AFileName: WideString); overload;   // I4034
    procedure LoadFromStream(AStream: TStream); overload;  // I2964
    procedure SaveToFile(AFileName: WideString);
    procedure LoadFromFile(AFileName: WideString; ATextFileFormat: TTextFileFormat); overload;   // I4034
    procedure LoadFromStream(AStream: TStream; ATextFileFormat: TTextFileFormat); overload;  // I2964

    procedure SetSelectedRow(ARow: Integer);

    property EditorText: WideString read GetText write SetText;
    property EditorFormat: TEditorFormat read GetEditorFormat write SetEditorFormat;
    property OnChanged: TNotifyEvent read FOnChanged write FonChanged;

    property CodeFont: TFont read GetCodeFont write SetCodeFont;
    property CharFont: TFont read GetCharFont write SetCharFont;
    property TextFileFormat: TTextFileFormat read FTextFileFormat write SetTextFileFormat;
  end;

implementation

uses
  System.TypInfo,
  Vcl.Clipbrd,

  Keyman.Developer.System.HelpTopics,
  Keyman.UI.FontUtils,

  dmActionsMain,
  dmActionsTextEditor,
  CharacterDragObject,
  CharacterInfo,
  CharMapDropTool,
  CharMapInsertMode,
  keyboardparser,
  KeymanDeveloperOptions,
  kwhelp,
  ErrorControlledRegistry,
  RegistryKeys,
  UfrmHelp,
  UfrmMain,
  UmodWebHTTPServer,
  Unicode,
  utilexecute,
  utilhttp,
  utilstr;

{$R *.dfm}

{ TframeTextEditor }

procedure TframeTextEditor.RefreshOptions;
begin
  //TODO: memo.TabStops   := FKeymanDeveloperOptions.IndentSize;
end;

procedure TframeTextEditor.ReplaceSelection(ARange: TRect;
  const ANewText: string);
var
  j: TJSONObject;
begin
  j := TJSONObject.Create;
  try
    j.AddPair('top', TJSONNumber.Create(ARange.Top));
    j.AddPair('left', TJSONNumber.Create(ARange.Left));
    j.AddPair('bottom', TJSONNumber.Create(ARange.Bottom));
    j.AddPair('right', TJSONNumber.Create(ARange.Right));
    j.AddPair('newText', ANewText);
    ExecuteCommand('replaceSelection', j);
  finally
    j.Free;
  end;
end;

procedure TframeTextEditor.cefCommand(Sender: TObject; const command: string; params: TStringList);
begin
  AssertVclThread;
  if command = 'command' then
  begin
    FireCommand(params);
  end;
end;

const
  TEXTEDITOR_CONTEXTMENU_SHOWCHARACTER       = MENU_ID_USER_FIRST + 1;  //actTextEditor_ShowCharacter
  TEXTEDITOR_CONTEXTMENU_CONVERTTOCHARACTERS = MENU_ID_USER_FIRST + 2;  //actTextEditor_ConvertToCharacters

procedure TframeTextEditor.cefBeforeContextMenu(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  if not cef.cef.IsSameBrowser(browser) then exit;

  model.AddSeparator;
  model.AddItem(TEXTEDITOR_CONTEXTMENU_SHOWCHARACTER,        'S&how Character');
  model.SetEnabled(TEXTEDITOR_CONTEXTMENU_SHOWCHARACTER, modActionsTextEditor.actTextEditor_ShowCharacter.Enabled);
  model.AddItem(TEXTEDITOR_CONTEXTMENU_CONVERTTOCHARACTERS,  'C&onvert to Characters');
  model.SetAccelerator(TEXTEDITOR_CONTEXTMENU_CONVERTTOCHARACTERS, Ord('U'), True, True, False);
  model.SetEnabled(TEXTEDITOR_CONTEXTMENU_CONVERTTOCHARACTERS, modActionsTextEditor.actTextEditor_ConvertToCharacters.Enabled);
end;

procedure TframeTextEditor.DoOpenLinkIfExternal(const url: string; var handled: Boolean);
begin
  if url.StartsWith('keyman:') then
  begin
    handled := False;
    Exit;
  end;

  handled := not url.StartsWith(modWebHttpServer.GetLocalhostURL + '/app/editor/');
  if handled then
    TUtilExecute.URL(url);
end;

procedure TframeTextEditor.cefBeforeBrowseSync(Sender: TObject;
  const Url: string; isPopup: Boolean; out Handled: Boolean);
begin
  DoOpenLinkIfExternal(Url, Handled);
end;

procedure TframeTextEditor.cefBeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue;
  var noJavascriptAccess, Result: Boolean);
begin
  DoOpenLinkIfExternal(targetUrl, Result);
end;

procedure TframeTextEditor.cefContextMenuCommand(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer; eventFlags: Cardinal;
  out Result: Boolean);
begin
  Result := False;

  if not cef.cef.IsSameBrowser(browser) then exit;

  case commandId of
    TEXTEDITOR_CONTEXTMENU_SHOWCHARACTER,
    TEXTEDITOR_CONTEXTMENU_CONVERTTOCHARACTERS:
      PostMessage(Handle, WM_USER_TextEditor_Command, commandId, 0);
  end;
end;

procedure TframeTextEditor.cefKeyEvent(Sender: TObject; e: TCEFHostKeyEventData;
  wasShortcut, wasHandled: Boolean);
begin
  AssertVclThread;
  if e.event.windows_key_code = VK_ESCAPE then
  begin
    ClearError;
  end;
end;

procedure TframeTextEditor.cefPreKeySyncEvent(Sender: TObject;
  e: TCEFHostKeyEventData; out isShortcut, Handled: Boolean);
begin
  AssertCefThread;
  Handled := False;
  // While FErrorLine is from main thread, this is unlikely to cause trouble so
  // will not worry about synchronisation
  if (e.event.windows_key_code = VK_ESCAPE) and (FErrorLine > 0) then
    Handled := True;
end;

procedure TframeTextEditor.Changed;
begin
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

procedure TframeTextEditor.FormCreate(Sender: TObject);
begin
  inherited;

  FCharFont := TFont.Create;
  FCodeFont := TFont.Create;

  cef := TframeCEFHost.Create(Self);
  cef.ShouldShowContextMenu := True;
  cef.Parent := Self;
  cef.Visible := True;
  cef.OnCommand := cefCommand;
  cef.OnLoadEnd := cefLoadEnd;
  cef.OnBeforeBrowseSync := cefBeforeBrowseSync;
  cef.OnHelpTopic := cefHelpTopic;

  cef.cef.OnBeforeContextMenu := cefBeforeContextMenu;
  cef.cef.OnContextMenuCommand := cefContextMenuCommand;
  cef.cef.OnBeforePopup := cefBeforePopup;

  cef.OnPreKeySyncEvent := cefPreKeySyncEvent;
  cef.OnKeyEvent := cefKeyEvent;
  SetupCharMapDrop;
end;

procedure TframeTextEditor.SetupCharMapDrop;
begin
  GetCharMapDropTool.Handle(cef, cmimDefault, CharMapDragOver, CharMapDragDrop);
end;

procedure TframeTextEditor.cefHelpTopic(Sender: TObject);
begin
  frmKeymanDeveloper.HelpTopic(Self);
end;

procedure TframeTextEditor.cefLoadEnd(Sender: TObject);
begin
  FHasBeenLoaded := True;
  UpdateEditorFonts;
  SetupCharMapDrop;
  DelayedFindError;
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
  Result := FCharFont;
end;

function TframeTextEditor.GetCodeFont: TFont;
begin
  Result := FCodeFont;
end;

function TframeTextEditor.GetEditorFormat: TEditorFormat;
begin
  Result := FEditorFormat;
end;

function TframeTextEditor.GetText: WideString;
begin
  Result := modWebHttpServer.AppSource.GetSource(FFileName);
end;

function TframeTextEditor.IKMDTextEditorActions_GetText: string;
begin
  Result := GetText;
end;

procedure TframeTextEditor.IKMDTextEditorActions_SetText(const Value: string);
begin
  SetText(Value);
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
      LoadFileInBrowser('');
    end;
  finally
    FLoading := False;
  end;
end;

procedure TframeTextEditor.LoadFromStream(AStream: TStream; ATextFileFormat: TTextFileFormat);  // I2964
var
  s: TStringList;
begin
  FLoading := True;
  try
    TextFileFormat := ATextFileFormat;

    s := TStringList.Create;
    try
      s.LoadFromStream(AStream, TextFileFormatToEncoding(TextFileFormat));   // I3637
      LoadFileInBrowser(s.Text);
    finally
      s.Free;
    end;

  finally
    FLoading := False;
  end;
end;

procedure TframeTextEditor.LoadFileInBrowser(const AData: string);
  function GenerateNewFilename: string;
  begin
    Inc(FInitialFilenameIndex);
    Result := '*texteditor*'+IntToStr(FInitialFilenameIndex);
  end;
  function EncodeFont(const prefix: string; f: TFont): string;
  begin
    Result := Format('&%sName=%s&%sSize=%d', [prefix, URLEncode(f.Name), prefix, TFontUtils.FontSizeInPixels(f)]);
  end;
const
  mode: array[TEditorFormat] of string = (
    'keyman', 'xml', 'text', 'html', 'json', 'javascript', 'css', 'wordlisttsv'
  );
begin
  if FFilename = '' then
    FFilename := GenerateNewFilename;
  modWebHTTPServer.AppSource.RegisterSource(FFilename, AData, True);
  cef.Navigate(
    modWebHttpServer.GetLocalhostURL + '/app/editor/'+
    '?mode='+mode[FEditorFormat]+
    '&filename='+URLEncode(FFilename)+
    EncodeFont('charFont', FCharFont)+
    EncodeFont('codeFont', FCodeFont));
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
        on E:EEncodingError do
        begin
          ShowMessage(E.Message);
          Exit;
        end;
      end;
    end
    else
    begin
      TextFileFormat := ATextFileFormat;
      LoadFileInBrowser('');
    end;
  finally
    FLoading := False;
  end;
end;

procedure TframeTextEditor.LoadFromStream(AStream: TStream);
var
  s: TStringList;
  buffer: TBytes;
  encoding: TEncoding;
  p: Int64;
  BufferLength, PreambleLength: Integer;
begin
  FLoading := True;
  try
    { Sniff the buffer preamble }
    p := AStream.Position;
    SetLength(buffer, 16);
    BufferLength := AStream.Read(buffer, 16);
    SetLength(buffer, BufferLength);
    encoding := nil;
    PreambleLength := TEncoding.GetBufferEncoding(buffer, encoding, TEncoding.UTF8);
    AStream.Position := p + PreambleLength;

    s := TStringList.Create;
    try
      p := AStream.Position;
      try
        s.LoadFromStream(AStream, encoding); // prolog determines encoding  // I3337
      except
        on E:EEncodingError do
        begin
          // Reload as ANSI, because not valid UTF-8
          AStream.Position := p;
          s.LoadFromStream(AStream, TEncoding.Default);
        end;
      end;

      if s.Encoding = TEncoding.UTF8 then  // I3337   // I3636
        TextFileFormat := tffUTF8
      else if s.Encoding = TEncoding.Unicode then  // I3337   // I3636
        TextFileFormat := tffUTF16
      else
        TextFileFormat := tffANSI;
      LoadFileInBrowser(s.Text);
    finally
      s.Free;
    end;

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

procedure TframeTextEditor.UpdateInsertState(const AMode: string);
begin
  frmKeymanDeveloper.barStatus.Panels[1].Text := AMode;
end;

function TframeTextEditor.PrintFile(Header: WideString): Boolean;
begin
  ExecuteCommand('print');
  Result := True;
end;

function TframeTextEditor.PrintPreview(Header: WideString): Boolean;
begin
  // TODO: print preview
  Result := True;
end;

procedure TframeTextEditor.SetCharFont(const Value: TFont);
begin
  FCharFont.Assign(Value);
  UpdateEditorFonts;
end;

procedure TframeTextEditor.SetCodeFont(const Value: TFont);
begin
  FCodeFont.Assign(Value);
  UpdateEditorFonts;
end;

procedure TframeTextEditor.UpdateEditorFonts;
  function FontToJSON(f: TFont): TJSONObject;
  begin
    Result := TJSONObject.Create;
    Result.AddPair('name', f.Name);
    Result.AddPair('size', TJSONNumber.Create(TFontUtils.FontSizeInPixels(f)));
  end;
var
  j: TJSONObject;
begin
  if not FHasBeenLoaded then
    Exit;

  j := TJSONObject.Create;
  try
    j.AddPair('codeFont', FontToJSON(FCodeFont));
    j.AddPair('charFont', FontToJSON(FCharFont));
    ExecuteCommand('setFonts', j);
  finally
    j.Free;
  end;
end;

procedure TframeTextEditor.SetEditorFormat(const Value: TEditorFormat);
begin
  FEditorFormat := Value;
end;

procedure TframeTextEditor.SetFocus;
begin
  cef.SetFocus;
end;

procedure TframeTextEditor.SetSelectedRow(ARow: Integer);
begin
  SetCursorPosition(0, ARow);
end;

procedure TframeTextEditor.SetCursorPosition(AColumn, ARow: Integer);
var
  j: TJSONObject;
begin
  j := TJSONObject.Create;
  try
    j.AddPair('row', TJSONNumber.Create(ARow));
    j.AddPair('column', TJSONNumber.Create(AColumn));
    ExecuteCommand('moveCursor', j);
  finally
    j.Free;
  end;
end;

function TframeTextEditor.GetSelectedCol: Integer;
begin
  if FSelectedRangeIsBackwards
    then Result := FSelectedRange.Left
    else Result := FSelectedRange.Right;
end;

function TframeTextEditor.GetSelectedRange: TRect;
begin
  Result := FSelectedRange;
end;

function TframeTextEditor.GetSelectedRow: Integer;
begin
  if FSelectedRangeIsBackwards
    then Result := FSelectedRange.Top
    else Result := FSelectedRange.Bottom;
end;

procedure TframeTextEditor.SetText(Value: WideString);
var
  v: TJSONString;
begin
  RefreshOptions;

  if FHasBeenLoaded then
  begin
    // #5095 Update the backing store so we have the data available immediately
    if modWebHttpServer.AppSource.IsSourceRegistered(FFileName) then
      modWebHttpServer.AppSource.SetSource(FFilename, Value);

    // Then update the text editor as well
    v := TJSONString.Create(Value);
    try
      ExecuteCommand('setText', v);
    finally
      v.Free;
    end;
  end
  else
  begin
    FLoading := True;
    try
      LoadFileInBrowser(Value);
    finally
      FLoading := False;
    end;
  end;
end;

procedure TframeTextEditor.SetTextFileFormat(const Value: TTextFileFormat);
var
  FSource: string;
begin
  if FTextFileFormat <> Value then
  begin
    FTextFileFormat := Value;
    if (FTextFileFormat = tffANSI) and modWebHttpServer.AppSource.IsSourceRegistered(FFileName) then
    begin
      // Recode to ANSI
      FSource := string(AnsiString(modWebHttpServer.AppSource.GetSource(FFileName)));
      SetText(FSource);
    end;
  end;
end;

procedure TframeTextEditor.SyntaxColourChange;
begin
  // TODO: syntax colouring
end;

procedure TframeTextEditor.UpdateState(ALocation: string);
begin
  if cef.HasFocus then
  begin
    if ALocation <> '' then
    begin
      FSelectedRange.Top   := StrToIntDef(StrToken(ALocation, ','),0);
      FSelectedRange.Left    := StrToIntDef(StrToken(ALocation, ','),0);
      FSelectedRange.Bottom  := StrToIntDef(StrToken(ALocation, ','),0);
      FSelectedRange.Right := StrToIntDef(StrToken(ALocation, ','),0);
      FSelectedRangeIsBackwards := StrToIntDef(StrToken(ALocation, ','),0) > 0;
      frmKeymanDeveloper.barStatus.Panels[0].Text := Format('Line %d, Col %d', [FSelectedRange.Top+1,FSelectedRange.Left+1]);
    end;
  end;
end;

procedure TframeTextEditor.WMUser_SyntaxColourChange(var Message: TMessage);
begin
  ExecuteCommand('reloadSettings');
end;

procedure TframeTextEditor.WMUser_TextEditor_Command(var Message: TMessage);
begin
  case Message.wParam of
    TEXTEDITOR_CONTEXTMENU_SHOWCHARACTER:
      modActionsTextEditor.actTextEditor_ShowCharacter.Execute;
    TEXTEDITOR_CONTEXTMENU_CONVERTTOCHARACTERS:
      modActionsTextEditor.actTextEditor_ConvertToCharacters.Execute;
  end;
end;

{-------------------------------------------------------------------------------
 - Paragraph colour management and errors                                      -
 -------------------------------------------------------------------------------}

procedure TframeTextEditor.ClearError;
begin
  ExecuteCommand('highlightError');
end;

{ IKMDEditActions }

function TframeTextEditor.CanClearSelection: Boolean;
begin
  Result := FHasSelection;
end;

function TframeTextEditor.CanCopy: Boolean;
begin
  Result := FHasSelection;
end;

function TframeTextEditor.CanCut: Boolean;
begin
  Result := FHasSelection;
end;

function TframeTextEditor.CanPaste: Boolean;
begin
  Result := Clipboard.HasFormat(CF_TEXT);
end;

function TframeTextEditor.CanRedo: Boolean;
begin
  Result := FCanRedo;
end;

function TframeTextEditor.CanSelectAll: Boolean;
begin
  Result := True; // TODO: Only if we have text?
end;

function TframeTextEditor.CanUndo: Boolean;
begin
  Result := FCanUndo;
end;

procedure TframeTextEditor.Undo;
begin
  ExecuteCommand('editUndo');
end;

procedure TframeTextEditor.Redo;
begin
  ExecuteCommand('editRedo');
end;

procedure TframeTextEditor.SelectAll;
begin
  ExecuteCommand('editSelectAll');
end;

procedure TframeTextEditor.ClearSelection;
begin
  cef.cef.ClipboardDel;
end;

procedure TframeTextEditor.CopyToClipboard;
begin
  cef.cef.ClipboardCopy;
end;

procedure TframeTextEditor.CutToClipboard;
begin
  cef.cef.ClipboardCut;
end;

procedure TframeTextEditor.DebugClearBreakpoint(ALine: Integer);
begin
  ExecuteLineCommand(ALine, 'clearBreakpoint');
end;

procedure TframeTextEditor.DebugSetBreakpoint(ALine: Integer);
begin
  ExecuteLineCommand(ALine, 'setBreakpoint');
end;

procedure TframeTextEditor.DebugUpdateExecutionPoint(ALine: Integer);
begin
  ExecuteLineCommand(ALine, 'updateExecutionPoint');
end;

procedure TframeTextEditor.PasteFromClipboard;
begin
  cef.cef.ClipboardPaste;
end;

{ IKMDSearchActions }

function TframeTextEditor.CanEditFind: Boolean;
begin
  Result := True;
end;

function TframeTextEditor.CanEditFindNext: Boolean;
begin
  Result := True;
end;

procedure TframeTextEditor.EditFind;
begin
  if not cef.HasFocus then Exit;
  ExecuteCommand('searchFind');
end;

procedure TframeTextEditor.EditFindNext;
begin
  if not cef.HasFocus then Exit;
  ExecuteCommand('searchFindNext');
end;

procedure TframeTextEditor.EditReplace;
begin
  if not cef.HasFocus then Exit;
  ExecuteCommand('searchReplace');
end;

procedure TframeTextEditor.ExecuteCommand(const command: string; const parameters: TJSONValue);
begin
  if Assigned(parameters) then
  begin
    cef.cef.ExecuteJavaScript('window.editorGlobalContext.'+command+'('+parameters.ToJSON+')', '');
  end
  else
  begin
    cef.cef.ExecuteJavaScript('window.editorGlobalContext.'+command+'()', '');
  end;
end;

procedure TframeTextEditor.ExecuteLineCommand(ALine: Integer;
  const command: string);
var
  v: TJSONNumber;
begin
  v := TJSONNumber.Create(ALine);
  try
    ExecuteCommand(command, v);
  finally
    v.Free;
  end;
end;

procedure TframeTextEditor.DelayedFindError;
begin
  if FErrorLine > 0 then
    FindError(FErrorLine);
end;

procedure TframeTextEditor.FindError(ln: Integer);
begin
  ClearError;

  FErrorLine := ln;

  if not FHasBeenLoaded then
    Exit;

  if (ln <= 0) then Exit;

  ExecuteLineCommand(ln, 'highlightError');
  SetSelectedRow(ln);
end;

function TframeTextEditor.OffsetToLine(Offset: Integer): Integer;   // I4083
var
  line: Integer;
begin
  with TStringList.Create do
  try
    Text := Self.GetText;
    line := 0;
    while offset > 0 do
    begin
      Dec(offset, Strings[line].Length + 2);
      Inc(line);
    end;
  finally
    Free;
  end;

  Result := line;
end;

procedure TframeTextEditor.FindErrorByOffset(offset: Integer);   // I4083
begin
  ClearError;
  FindError(OffsetToLine(offset));
end;

procedure TframeTextEditor.FireCommand(const commands: TStringList);
var
  i: Integer;
  command0, command: string;
  commandParams: TArray<string>;
begin
  i := 0;
  while i < commands.Count do
  begin
    command0 := commands[i];
    commandParams := command0.Split([',']);
    command := commandParams[0];
    if command = 'modified' then Changed   // I3948
    else if command = 'undo-disable' then FCanUndo := False
    else if command = 'undo-enable' then FCanUndo := True
    else if command = 'redo-disable' then FCanRedo := False
    else if command = 'redo-enable' then FCanRedo := True
    else if command = 'has-selection' then FHasSelection := True
    else if command = 'no-selection' then FHasSelection := False
    else if command = 'breakpoint-clicked' then DoBreakpointClicked(commandParams[1])
    else if command0.StartsWith('insert-mode,') then
    begin
      UpdateInsertState(command0.Substring('insert-mode,'.Length));
    end
    else if command0.StartsWith('location,') then
    begin
      UpdateState(command0.Substring('location,'.Length));
    end
    else if command0.StartsWith('token,') then
    begin
      UpdateToken(command0.Substring('token,'.Length));
    end
    else ShowMessage('keyman:'+commands.Text);
    Inc(i);
  end;
end;

procedure TframeTextEditor.DoBreakpointClicked(const line: string);
begin
  if Assigned(FOnBreakpointClicked) then
    FOnBreakpointClicked(Self, StrToInt(line));
end;

procedure TframeTextEditor.UpdateToken(command: string);
var
  col: Integer;
  line: string;
  x, tx: Integer;
  token, prevtoken: WideString;
  FHelpTopic, FPrevHelpTopic: WideString;
begin
  token := StrToken(command, ',');
  if token = 'null' then
    col := 0
  else if not TryStrToInt(token, col) then
    Exit;

  line := command;

  x := col+1;

  token := GetTokenAtCursor(line, x, tx, prevtoken);
  if (FHasSelection) and (token <> '') then
  begin
    prevtoken := line;
    if (x > tx) and CharInSet(token[1], ['"', '''']) then
      prevtoken := token[1] + prevtoken + token[1];

    if not TKeyboardParser_Line.GetXStr(prevtoken, token) then
      token := line;

    x := 1;
    tx := 1;
  end
  else
    token := GetTokenAtCursor(line, x, tx, prevtoken);

  UpdateCharacterMap(False, token, x, tx, FHasSelection);

  FHelpTopic := token;
  FPrevHelpTopic := prevtoken;

  if EditorFormat = efKMN then
  begin
    if not IsValidHelpToken(FHelpTopic, False) then
    begin
      if not IsValidHelpToken(FPrevHelpTopic, False)
        then FHelpTopic := ''
        else FHelpTopic := FPrevHelpTopic;
    end;

    if FHelpTopic <> '' then
    begin
      HelpKeyword := FHelpTopic;

      if Assigned(frmHelp) and frmHelp.Showing then
        frmHelp.QueueRefresh;
    end;
  end;
end;

function TframeTextEditor.GetHelpTopic: string;
begin
  if FEditorFormat <> efKMN then
  begin
    if Parent is TTIKEForm then
      Exit((Parent as TTIKEForm).HelpTopic);
    Exit(SHelpTopic_Context_TextEditor);
  end;

  Result := HelpKeyword;
end;

procedure TframeTextEditor.TntFormDestroy(Sender: TObject);
begin
  inherited;
  if FFileName <> '' then
    modWebHttpServer.AppSource.UnregisterSource(FFileName);

  FreeAndNil(FCharFont);
  FreeAndNil(FCodeFont);
end;

procedure TframeTextEditor.CharMapDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  j: TJSONObject;
begin
  cef.cefwp.SetFocus;
  j := TJSONObject.Create;
  try
    j.AddPair('x', TJSONNumber.Create(X));
    j.AddPair('y', TJSONNumber.Create(Y));
    j.AddPair('state', GetEnumName(TypeInfo(TDragState), Ord(State)));
    ExecuteCommand('charmapDragOver', j);
  finally
    j.Free;
  end;

  // We cannot test acceptance via event because it is asynchronous
  // So we will just assume we can accept and throw it away if it is outside bounds
  // during drop.
  Accept := True;
end;

procedure TframeTextEditor.CharMapDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  j: TJSONObject;
  cdo: TCharacterDragObject;
begin
  cef.cefwp.SetFocus;
  cdo := Source as TCharacterDragObject;
  j := TJSONObject.Create;
  try
    j.AddPair('x', TJSONNumber.Create(X));
    j.AddPair('y', TJSONNumber.Create(Y));

    // The character map insert format actually only
    // makes sense for the .kmn source editor. For all
    // other contexts, we currently only support cmimCharacter.
    // It would be possible to do \uxxxx for JS/JSON etc but
    // for now that is low priority.
    if FEditorFormat = efKMN
      then j.AddPair('text', cdo.Text[cdo.InsertType])
      else j.AddPair('text', cdo.Text[cmimCharacter]);
    ExecuteCommand('charmapDragDrop', j);
  finally
    j.Free;
  end;
end;

end.
