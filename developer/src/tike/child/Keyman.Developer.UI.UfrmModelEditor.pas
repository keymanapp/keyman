﻿unit Keyman.Developer.UI.UfrmModelEditor;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  System.Variants,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Grids,
  Vcl.StdCtrls,
  Winapi.Messages,
  Winapi.Windows,

  Keyman.Developer.UI.dmActionsModelEditor,
  dmActionsMain,
  LeftTabbedPageControl,
  UfrmMDIChild,
  UfrmMDIEditor,
  UframeTextEditor,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.modeltsProjectFile,
  Keyman.Developer.UI.Project.modeltsProjectFileUI,

  Keyman.Developer.UI.UframeWordlistEditor,
  Keyman.Developer.System.LexicalModelParser,
  Keyman.Developer.System.LexicalModelParserTypes;

type
  TfrmModelEditor = class(TfrmTikeEditor)
    pages: TLeftTabbedPageControl;
    pageDetails: TTabSheet;
    sbDetails: TScrollBox;
    sbCompile: TScrollBox;
    panWordlists: TPanel;
    lblWordlists: TLabel;
    gridWordlists: TStringGrid;
    cmdAddWordlist: TButton;
    cmdRemoveWordlist: TButton;
    panBasicInformation: TPanel;
    lblFormat: TLabel;
    lblBasicInformation: TLabel;
    pageSource: TTabSheet;
    pageCompile: TTabSheet;
    lblCongrats: TLabel;
    panBuildLexicalModel: TPanel;
    cbFormat: TComboBox;
    cbWordBreaker: TComboBox;
    lblWordBreaker: TLabel;
    memoComments: TMemo;
    lblComments: TLabel;
    lblDebugHostCaption: TLabel;
    lblCrossPlatform: TLabel;
    cmdTestLexicalModel: TButton;
    cmdOpenDebugHost: TButton;
    lbDebugHosts: TListBox;
    cmdSendURLsToEmail: TButton;
    Label1: TLabel;
    editTestKeyboard: TEdit;
    cmdBrowseTestKeyboard: TButton;
    Label2: TLabel;
    lblReadOnly: TLabel;
    dlgAddWordlist: TOpenDialog;
    dlgBrowseTestKeyboard: TOpenDialog;
    imgQRCode: TImage;
    lblInsertAfterWord: TLabel;
    cbInsertAfterWord: TComboBox;
    lblQuotationMarks: TLabel;
    cbOpenQuote: TComboBox;
    chkIsRTL: TCheckBox;
    cbCloseQuote: TComboBox;
    lblOpenQuote: TLabel;
    lblCloseQuote: TLabel;
    panOpenInExplorer: TPanel;
    lblOpenInExplorer: TLabel;
    cmdOpenSourceFolder: TButton;
    cmdOpenBuildFolder: TButton;
    cmdOpenProjectFolder: TButton;
    panFileActions: TPanel;
    lblFileActions: TLabel;
    cmdAddToProject: TButton;
    cmdCompile: TButton;
    Label5: TLabel;
    editOutPath: TEdit;
    cmdCopyDebuggerLink: TButton;
    cmdConfigureWebDebugger: TButton;
    chkLanguageUsesCasing: TCheckBox;
    procedure FormDestroy(Sender: TObject);
    procedure cmdAddWordlistClick(Sender: TObject);
    procedure cmdRemoveWordlistClick(Sender: TObject);
    procedure gridWordlistsDblClick(Sender: TObject);
    procedure pagesChanging(Sender: TObject; var AllowChange: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure cbFormatClick(Sender: TObject);
    procedure cbWordBreakerClick(Sender: TObject);
    procedure memoCommentsChange(Sender: TObject);
    procedure cmdOpenDebugHostClick(Sender: TObject);
    procedure cmdSendURLsToEmailClick(Sender: TObject);
    procedure cmdBrowseTestKeyboardClick(Sender: TObject);
    procedure editTestKeyboardChange(Sender: TObject);
    procedure lbDebugHostsClick(Sender: TObject);
    procedure cbInsertAfterWordClick(Sender: TObject);
    procedure cbInsertAfterWordKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbOpenQuoteClick(Sender: TObject);
    procedure cbOpenQuoteKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbCloseQuoteClick(Sender: TObject);
    procedure cbCloseQuoteKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure chkIsRTLClick(Sender: TObject);
    procedure cmdOpenSourceFolderClick(Sender: TObject);
    procedure cmdOpenBuildFolderClick(Sender: TObject);
    procedure cmdOpenProjectFolderClick(Sender: TObject);
    procedure cmdCopyDebuggerLinkClick(Sender: TObject);
    procedure chkLanguageUsesCasingClick(Sender: TObject);
    procedure sbDetailsMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    type
      TWordlist = class
        Frame: TframeWordlistEditor;
        Tab: TTabSheet;
        Filename: string;
        destructor Destroy; override;
      end;
      TWordlists = class(TObjectList<TWordlist>)
      public
        function IndexOfFilename(const Filename: string): Integer;
        function FindByFullPath(const Filename: string): TWordlist;
      end;
  private
    parser: TLexicalModelParser;
    wordlists: TWordlists;
    frameSource: TframeTextEditor;
    FSetup: Integer;
    procedure FillDetails;
    procedure EnableControls;
    procedure UpdateWordlistTabs;
    function AddWordlistTab(
      const WordlistFilename: string): TfrmModelEditor.TWordlist;
    function WordlistFromTab(tab: TTabSheet): TfrmModelEditor.TWordlist;
    function MoveDesignToSource: Boolean;
    function MoveSourceToDesign: Boolean;
    procedure SourceChanged(Sender: TObject);
    function CheckModifiedWordlistsForRemoval(
      newParser: TLexicalModelParser): Boolean;
    procedure UpdateQRCode;
    procedure WordlistChanged(Sender: TObject);
    { Private declarations }
  protected
    function GetHelpTopic: string; override;
    function DoOpenFile: Boolean; override;
    function DoSaveFile: Boolean; override;
    function GetFileNameFilter: string; override;
    function GetDefaultExt: string; override;

    procedure CodeFontChanged; override;
    procedure CharFontChanged; override;

    function GetProjectFile: TProjectFile; override;

  public
    function CanChangeTab(FForward: Boolean): Boolean; override;
    procedure ChangeTab(FForward: Boolean); override;

    function CanChangeView(FView: TCodeDesignView): Boolean; override;
    procedure ChangeView(FView: TCodeDesignView); override;

    function HasSubfilename(const Filename: string): Boolean; override;

    procedure FindError(const Filename: string; s: string; line: Integer); override;   // I4081
    procedure NotifyStartedWebDebug;
  end;

implementation

uses
  System.UITypes,
  Vcl.Clipbrd,

  Keyman.Developer.System.HelpTopics,
  Keyman.Developer.System.Project.modeltsProjectFileAction,
  Keyman.Developer.System.ServerAPI,
  Keyman.System.QRCode,
  TextFileFormat,
  UfrmSendURLsToEmail,
  utilexecute,
  utilsystem;

{$R *.dfm}

function FormatToIndex(format: TLexicalModelFormat): Integer;
begin
  Result := Ord(format) - 1;  // unknown = -1
end;

function FormatFromIndex(index: Integer): TLexicalModelFormat;
begin
  Result := TLexicalModelFormat(index+1);  // unknown = -1
end;

function WordBreakerToIndex(wordBreaker: TLexicalModelWordBreaker): Integer;
begin
  Result := Ord(wordBreaker) - 1;  // unknown = -1
end;

function WordBreakerFromIndex(index: Integer): TLexicalModelWordBreaker;
begin
  Result := TLexicalModelWordBreaker(index+1);  // unknown = -1
end;

type
  TComboStringOption = record
    value, name: string;
    class procedure FillCombo(const opts: array of TComboStringOption;
      combo: TComboBox); static;
    class function GetValue(const opts: array of TComboStringOption;
      combo: TComboBox): string; static;
    class procedure SetValue(const opts: array of TComboStringOption;
      const text: string; combo: TComboBox); static;
  end;

const
  CInsertAfterWordOptions: array[0..4] of TComboStringOption = (
    (value: ' '; name: '(Space U+0020)'),
    (value: ''; name: '(No word break)'),
    (value: Char($200B); name: '(Zero width space U+200B)'),
    (value: Char($0F0B); name: '(Tibetan tsheg U+0F0B)'),
    (value: Char($1361); name: '(Ethiopian wordspace U+1361)')
  );

  COpenQuoteOptions: array[0..11] of TComboStringOption = (
    (value: TLexicalModelParser.CDefaultOpenQuote),
    (value: '«'),
    (value: '„'),
    (value: '»'),
    (value: '"'),
    (value: ''''),
    (value: '‹'),
    (value: '‘'),
    (value: '‚'),
    (value: '›'),
    (value: '「'),
    (value: '『')
  );

  CCloseQuoteOptions: array[0..10] of TComboStringOption = (
    (value: TLexicalModelParser.CDefaultCloseQuote),
    (value: '»'),
    (value: '“'),
    (value: '«'),
    (value: '"'),
    (value: ''''),
    (value: '›'),
    (value: '’'),
    (value: '‹'),
    (value: '」'),
    (value: '』')
  );

class procedure TComboStringOption.SetValue(const opts: array of TComboStringOption; const text: string;
  combo: TComboBox);
var
  index: Integer;
begin
  for index := 0 to High(opts) do
    if opts[index].value = text then
    begin
      combo.ItemIndex := index;
      Exit;
    end;
  combo.Text := text;
end;

class function TComboStringOption.GetValue(const opts: array of TComboStringOption;
  combo: TComboBox): string;
begin
  if (combo.ItemIndex >= 0) and (combo.ItemIndex <= High(opts))
    then Result := opts[combo.ItemIndex].value
    else Result := combo.Text;
end;

class procedure TComboStringOption.FillCombo(const opts: array of TComboStringOption; combo: TComboBox);
var
  opt: TComboStringOption;
begin
  combo.Items.BeginUpdate;
  try
    combo.Items.Clear;
    for opt in opts do
    begin
      if opt.name = ''
        then combo.Items.Add(opt.value)
        else combo.Items.Add(opt.name);
    end;
  finally
    combo.Items.EndUpdate;
  end;
end;

{ TfrmModelEditor }

function TfrmModelEditor.DoOpenFile: Boolean;
var
  model: TStringList;
begin
  model := TStringList.Create;
  try
    model.DefaultEncoding := TEncoding.UTF8;
    try
      model.LoadFromFile(FileName);
    except
      on E:EEncodingError do
      begin
        try
          model.LoadFromFile(FileName, TEncoding.Default);
        except
          on E:EEncodingError do
          begin
            ShowMessage('Could not load file, does not appear to be a valid encoding.');
            Exit(False);
          end;
        end;
      end;
    end;
    parser := TLexicalModelParser.Create(model.Text);
  finally
    model.Free;
  end;

  wordlists := TWordlists.Create;

  Inc(FSetup);
  try
    FillDetails;
    UpdateWordlistTabs;
    MoveDesignToSource;
    dlgAddWordlist.InitialDir := ExtractFilePath(Filename);
  finally
    Dec(FSetup);
  end;

  Result := True;
end;

function TfrmModelEditor.DoSaveFile: Boolean;
var
  wordlist: TWordlist;
  stream: TStringStream;
begin
  if pages.ActivePage = pageSource then
  begin
    if not MoveSourceToDesign then Exit(False);
  end;

  for wordlist in wordlists do
  begin
    wordlist.Frame.SaveToFile(wordlist.Frame.Filename);
  end;

  // We use TStringStream instead of TStringList so we don't get
  // a BOM on the saved file
  stream := TStringStream.Create(parser.Text, TEncoding.UTF8);
  try
    stream.SaveToFile(FileName);
  finally
    stream.Free;
  end;

  editOutPath.Text := (ProjectFile as TmodelTsProjectFile).TargetFilename;   // I4688

  Result := True;
end;

procedure TfrmModelEditor.editTestKeyboardChange(Sender: TObject);
begin
  if FSetup > 0 then
    Exit;
  (ProjectFile as TmodelTsProjectFile).TestKeyboard := editTestKeyboard.Text;
end;

procedure TfrmModelEditor.EnableControls;
var
  e: Boolean;
begin
  e := parser.IsEditable;

  { Details tab }

  lblFormat.Enabled := e;
  cbFormat.Enabled := e;
  lblWordBreaker.Enabled := e;
  cbWordBreaker.Enabled := e;
  lblComments.Enabled := e;

  lblInsertAfterWord.Enabled := e;
  cbInsertAfterWord.Enabled := e;
  lblQuotationMarks.Enabled := e;
  lblOpenQuote.Enabled := e;
  cbOpenQuote.Enabled := e;
  lblCloseQuote.Enabled := e;
  cbCloseQuote.Enabled := e;
  chkIsRTL.Enabled := e;
  chkLanguageUsesCasing.Enabled := e;

  memoComments.Enabled := e;
  cmdAddWordlist.Enabled := e;
  gridWordlists.Enabled := e and (parser.Wordlists.Count > 0);
  cmdRemoveWordlist.Enabled := e and (parser.Wordlists.Count > 0);
  lblReadOnly.Visible := not e;

  { Build tab }
  cmdOpenDebugHost.Enabled := lbDebugHosts.ItemIndex >= 0;
  cmdSendURLsToEmail.Enabled := lbDebugHosts.Items.Count > 0;   // I4506

  // We use FProjectFile because we don't want to accidentally create a standalone
  // project file as GetProjectFile is side-effecty. EnableControls is called early
  // in construction before FProjectFile is assigned. It is called again later so
  // enabled state will be correct.
  cmdOpenProjectFolder.Enabled := Assigned(FProjectFile) and Assigned(FProjectFile.Project);
end;

procedure TfrmModelEditor.NotifyStartedWebDebug;
begin
  lbDebugHosts.Clear;
  TServerDebugAPI.GetServerURLs(lbDebugHosts.Items);
  if lbDebugHosts.Items.Count > 0 then
    lbDebugHosts.ItemIndex := 0;
  UpdateQRCode;
  EnableControls;
end;

function TfrmModelEditor.HasSubfilename(const Filename: string): Boolean;
begin
  Result := wordlists.FindByFullPath(Filename) <> nil;
end;

procedure TfrmModelEditor.FindError(const Filename: string; s: string;
  line: Integer);
var
  wordlist: TWordlist;
begin
  if SameText(Filename, Self.FileName) then
  begin
    // Switch to code view, highlight line
    pages.ActivePage := pageSource;
    frameSource.FindError(line);
    Exit;
  end;

  wordlist := wordlists.FindByFullPath(Filename);
  if Assigned(wordlist) then
  begin
    pages.ActivePage := wordlist.Tab;
    wordlist.Frame.FindError(Filename, s, line);
    Exit;
  end;
end;

procedure TfrmModelEditor.FormCreate(Sender: TObject);
begin
  inherited;
  Inc(FSetup);
  try
    frameSource := TframeTextEditor.Create(Self);
    frameSource.Parent := pageSource;

    frameSource.Align := alClient;
    frameSource.EditorFormat := efJS;
    frameSource.Visible := True;
    frameSource.OnChanged := SourceChanged;
    frameSource.TextFileFormat := tffUTF8;

    TComboStringOption.FillCombo(CInsertAfterWordOptions, cbInsertAfterWord);
    TComboStringOption.FillCombo(COpenQuoteOptions, cbOpenQuote);
    TComboStringOption.FillCombo(CCloseQuoteOptions, cbCloseQuote);

    pages.ActivePage := pageDetails;
  finally
    Dec(FSetup);
  end;
end;

procedure TfrmModelEditor.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(parser);
  FreeAndNil(wordlists);
end;

function TfrmModelEditor.GetDefaultExt: string;
begin
  Result := '.model.ts';
end;

function TfrmModelEditor.GetFileNameFilter: string;
begin
  Result := 'Lexical model files (*.model.ts)|*.model.ts|All files (*.*)|*.*';
end;

function TfrmModelEditor.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_ModelEditor;
end;

function TfrmModelEditor.GetProjectFile: TProjectFile;
begin
  Result := inherited GetProjectFile;
  if not Assigned(Result) then
  begin
    FStandaloneProjectFile := TmodelTsProjectFileAction.Create(nil, FileName, nil);
    Result := FStandaloneProjectFile;
  end;
end;

procedure TfrmModelEditor.gridWordlistsDblClick(Sender: TObject);
begin
  pages.ActivePageIndex := gridWordlists.Row + 1;
end;

procedure TfrmModelEditor.lbDebugHostsClick(Sender: TObject);
begin
  UpdateQRCode;
end;

procedure TfrmModelEditor.memoCommentsChange(Sender: TObject);
begin
  if FSetup > 0 then
    Exit;
  parser.Comment := memoComments.Text;
  Modified := True;
end;

procedure TfrmModelEditor.SourceChanged(Sender: TObject);
begin
  if FSetup > 0 then
    Exit;
  Modified := True;
end;

procedure TfrmModelEditor.ChangeTab(FForward: Boolean);
begin
  pages.SelectNextPage(FForward);
end;

procedure TfrmModelEditor.ChangeView(FView: TCodeDesignView);
var
  wordlist: TWordlist;
begin
  wordlist := WordlistFromTab(pages.ActivePage);
  if not Assigned(wordlist) then Exit;
  case FView of
    cdvDesign: if wordlist.Frame.pages.ActivePage <> wordlist.Frame.pageDesign then wordlist.Frame.pages.SelectNextPage(False);
    cdvCode:   if wordlist.Frame.pages.ActivePage <> wordlist.Frame.pageCode then wordlist.Frame.pages.SelectNextPage(True);
  end;
end;

function TfrmModelEditor.CheckModifiedWordlistsForRemoval(newParser: TLexicalModelParser): Boolean;
var
  wordlist: TWordlist;
begin
  for wordlist in wordlists do
  begin
    if (newParser.Wordlists.IndexOf(wordlist.Filename) < 0) and
      wordlist.Frame.Modified then
    begin
      case MessageDlg(Format('The wordlist %s has been removed but has been modified. Save changes before removal?',
          [ExtractFileName(wordlist.Filename)]), mtConfirmation, mbYesNoCancel, 0) of
        mrYes: wordlist.Frame.SaveToFile(wordlist.Frame.Filename);
        mrNo: ;
        mrCancel: Exit(False);
      end;
    end;
  end;

  Result := True;
end;

function TfrmModelEditor.MoveSourceToDesign: Boolean;
var
  newParser: TLexicalModelParser;
begin
  Inc(FSetup);
  try
    newParser := TLexicalModelParser.Create(frameSource.EditorText);
    if not CheckModifiedWordlistsForRemoval(newParser) then
    begin
      FreeAndNil(newParser);
      Exit(False);
    end;
    FreeAndNil(parser);
    parser := newParser;
    FillDetails;
    UpdateWordlistTabs;
  finally
    Dec(FSetup);
  end;
  Result := True;
end;

function TfrmModelEditor.MoveDesignToSource: Boolean;
begin
  frameSource.EditorText := parser.Text;
  Result := True;
end;

procedure TfrmModelEditor.pagesChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  if FSetup > 0 then Exit;

  if pages.ActivePage = pageSource then
    AllowChange := MoveSourceToDesign
  else
    AllowChange := MoveDesignToSource
end;

procedure TfrmModelEditor.sbDetailsMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  (Sender as TScrollBox).VertScrollBar.Position := (Sender as TScrollBox).VertScrollBar.Position - WheelDelta div 2;
  Handled := True;
end;

{ --------- Details tab --------- }

procedure TfrmModelEditor.FillDetails;
var
  i: Integer;
begin
  cbFormat.ItemIndex := FormatToIndex(parser.Format);
  cbWordBreaker.ItemIndex := WordBreakerToIndex(parser.WordBreaker);
  TComboStringOption.SetValue(CInsertAfterWordOptions, parser.InsertAfterWord, cbInsertAfterWord);
  TComboStringOption.SetValue(COpenQuoteOptions, parser.OpenQuote, cbOpenQuote);
  TComboStringOption.SetValue(CCloseQuoteOptions, parser.CloseQuote, cbCloseQuote);
  chkIsRTL.Checked := parser.IsRTL;
  chkLanguageUsesCasing.Checked := parser.LanguageUsesCasing;

  memoComments.Text := parser.Comment;

  if parser.Wordlists.Count = 0 then
  begin
    gridWordlists.RowCount := 1;
    gridWordlists.Cells[0, 0] := '';
  end
  else
  begin
    gridWordlists.RowCount := parser.Wordlists.Count;
    for i := 0 to parser.Wordlists.Count - 1 do
    begin
      gridWordlists.Cells[0, i] := parser.Wordlists[i];
    end;
  end;

  editOutPath.Text := (ProjectFile as TmodelTsProjectFile).TargetFilename;   // I4688
  editTestKeyboard.Text := (ProjectFile as TmodelTsProjectFile).TestKeyboard;

  EnableControls;
end;

procedure TfrmModelEditor.UpdateWordlistTabs;
var
  i, n: Integer;
  wordlist: TWordlist;
begin
  // Remove tabs that are no longer listed
  for i := wordlists.Count - 1 downto 0 do
  begin
    if parser.Wordlists.IndexOf(wordlists[i].Filename) < 0 then
      wordlists.Delete(i);
  end;

  // Add new tabs
  for i := 0 to parser.Wordlists.Count - 1 do
  begin
    n := wordlists.IndexOfFilename(parser.Wordlists[i]);
    if n < 0 then
    begin
      wordlist := AddWordlistTab(parser.Wordlists[i]);
      wordlist.Tab.PageIndex := i + 1;
    end
    else
      wordlists[n].Tab.PageIndex := i + 1;
  end;
end;

function TfrmModelEditor.AddWordlistTab(const WordlistFilename: string): TfrmModelEditor.TWordlist;
begin
  Result := TWordlist.Create;

  wordlists.Add(Result);
  Result.Filename := WordlistFilename;

  Result.Tab := TTabSheet.Create(Self);
  Result.Tab.TabVisible := True;
  Result.Tab.ImageIndex := 17; // 'spreadsheet'
  Result.Tab.Caption := ExtractFileName(Result.Filename);
  Result.Tab.PageControl := pages;

  Result.Frame := TframeWordlistEditor.Create(Self);
  Result.Frame.OnChanged := WordlistChanged;
  Result.Frame.LoadFromFile(ExtractFilePath(Filename) + WordlistFilename);
  Result.Frame.Align := alClient;

  Result.Frame.Parent := Result.Tab;
  Result.Frame.Visible := True;
end;

function TfrmModelEditor.WordlistFromTab(
  tab: TTabSheet): TfrmModelEditor.TWordlist;
begin
  for Result in wordlists do
    if Result.Tab = tab then
      Exit;

  Result := nil;
end;

procedure TfrmModelEditor.WordlistChanged(Sender: TObject);
begin
  // We only go from clean to dirty, not vice-versa, on this notification
  if (Sender as TframeWordlistEditor).Modified then
    Modified := True;
end;

{ TfrmModelEditor.TWordlists }

function TfrmModelEditor.TWordlists.FindByFullPath(
  const Filename: string): TWordlist;
begin
  for Result in Self do
  begin
    if SameText(Result.Frame.Filename, Filename) then
    begin
      Exit;
    end;
  end;
  Result := nil;
end;

function TfrmModelEditor.TWordlists.IndexOfFilename(
  const Filename: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if SameText(Items[Result].Filename, Filename) then
      Exit;
  Result := -1;
end;

{ Wordlist management }

function TfrmModelEditor.CanChangeTab(FForward: Boolean): Boolean;
begin
  Result := True;
end;

function TfrmModelEditor.CanChangeView(FView: TCodeDesignView): Boolean;
begin
  Result := WordlistFromTab(pages.ActivePage) <> nil;
end;

procedure TfrmModelEditor.cbCloseQuoteClick(Sender: TObject);
begin
  if FSetup > 0 then
    Exit;
  parser.CloseQuote := TComboStringOption.GetValue(CCloseQuoteOptions, cbCloseQuote);
  Modified := True;
end;

procedure TfrmModelEditor.cbCloseQuoteKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  cbCloseQuoteClick(Sender);
end;

procedure TfrmModelEditor.cbFormatClick(Sender: TObject);
begin
  if FSetup > 0 then
    Exit;
  parser.Format := FormatFromIndex(cbFormat.ItemIndex);
  Modified := True;
end;

procedure TfrmModelEditor.cbInsertAfterWordClick(Sender: TObject);
begin
  if FSetup > 0 then
    Exit;
  parser.InsertAfterWord := TComboStringOption.GetValue(CInsertAfterWordOptions, cbInsertAfterWord);
  Modified := True;
end;

procedure TfrmModelEditor.cbInsertAfterWordKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  cbInsertAfterWordClick(Sender);
end;

procedure TfrmModelEditor.cbOpenQuoteClick(Sender: TObject);
begin
  if FSetup > 0 then
    Exit;
  parser.OpenQuote := TComboStringOption.GetValue(COpenQuoteOptions, cbOpenQuote);
  Modified := True;
end;

procedure TfrmModelEditor.cbOpenQuoteKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  cbOpenQuoteClick(cbOpenQuote);
end;

procedure TfrmModelEditor.cbWordBreakerClick(Sender: TObject);
begin
  if FSetup > 0 then
    Exit;
  parser.WordBreaker := WordBreakerFromIndex(cbWordBreaker.ItemIndex);
  Modified := True;
end;

procedure TfrmModelEditor.chkIsRTLClick(Sender: TObject);
begin
  if FSetup > 0 then
    Exit;
  parser.IsRTL := chkIsRTL.Checked;
  Modified := True;
end;

procedure TfrmModelEditor.chkLanguageUsesCasingClick(Sender: TObject);
begin
  if FSetup > 0 then
    Exit;
  parser.LanguageUsesCasing := chkLanguageUsesCasing.Checked;
  Modified := True;
end;

procedure TfrmModelEditor.cmdAddWordlistClick(Sender: TObject);
const
  SMsg_CopyFile =
    'The file "%0:s" is not in the same folder as the model file "%1:s".'#13#10#13#10+
    'Do you want to copy this file into the model file folder (recommended)?';
  SMsg_OverwriteFile =
    'The file "%0:s" already exists.'#13#10#13#10+
    'Do you want to overwrite it with "%1:s"?';
  SMsg_UnableToCopyFile =
    'The file "%1:s" could not be copied to "%0:s": %2:s';
var
  SourceFilename, TargetFilename: string;
  i: Integer;

  function DoCopyFile: Boolean;
  begin
    if FileExists(TargetFilename) and
      (MessageDlg(Format(SMsg_OverwriteFile, [TargetFilename,
      dlgAddWordlist.Filename]), mtWarning, mbOkCancel, 0) = mrCancel) then
    begin
      Exit(False);
    end;

    if not CopyFile(PChar(dlgAddWordlist.FileName), PChar(TargetFilename), False) then
    begin
      ShowMessage(Format(SMsg_UnableToCopyFile, [TargetFilename, dlgAddWordlist.FileName,
        SysErrorMessage(GetLastError)]));
      Exit(False);
    end;

    Result := True;
  end;

  function DoCheckFileLocation: Boolean;
  begin
    if ExtractFilePath(SourceFilename) <> '' then
    begin
      case MessageDlg(Format(SMsg_CopyFile, [dlgAddWordlist.FileName, Filename]), mtConfirmation, mbYesNoCancel, 0) of
        mrCancel: Exit(False);
        mrYes:
          begin
            Result := DoCopyFile;
            SourceFilename := ExtractFileName(TargetFilename);
          end;
        mrNo: Result := True;
        else Exit(False);
      end;
    end
    else
      Result := True;
  end;

begin
  if dlgAddWordlist.Execute then
  begin
    SourceFilename := ExtractRelativePath(Filename, dlgAddWordlist.FileName);
    TargetFilename := ExtractFilePath(Filename) + ExtractFileName(dlgAddWordlist.FileName);
    if not DoCheckFileLocation then
      Exit;

    SourceFilename := SourceFilename.Replace('\','/');
    if parser.Wordlists.IndexOf(SourceFilename) < 0 then
      parser.Wordlists.Add(SourceFilename);
    Inc(FSetup);
    try
      FillDetails;

      // Force a reload of the wordlist tab if it has been overwritten
      for i := 0 to wordlists.Count - 1 do
      begin
        if SameText(wordlists[i].Filename, SourceFilename) then
        begin
          wordlists.Delete(i);
          Break;
        end;
      end;
      UpdateWordlistTabs;
    finally
      Dec(FSetup);
    end;
    Modified := True;
  end;
end;

procedure TfrmModelEditor.cmdBrowseTestKeyboardClick(Sender: TObject);
begin
  if dlgBrowseTestKeyboard.Execute then
    editTestKeyboard.Text := dlgBrowseTestKeyboard.FileName;
end;

procedure TfrmModelEditor.cmdCopyDebuggerLinkClick(Sender: TObject);
begin
  try
    Clipboard.AsText := lbDebugHosts.Items[lbDebugHosts.ItemIndex];
  except
    on E:Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TfrmModelEditor.cmdOpenDebugHostClick(Sender: TObject);
begin
  TUtilExecute.URL(lbDebugHosts.Items[lbDebugHosts.ItemIndex]);
end;

procedure TfrmModelEditor.cmdOpenSourceFolderClick(Sender: TObject);
begin
  OpenContainingFolder(FileName);
end;

procedure TfrmModelEditor.cmdOpenBuildFolderClick(Sender: TObject);
begin
  OpenContainingFolder((ProjectFile as TmodeltsProjectFile).TargetFilename);
end;

procedure TfrmModelEditor.cmdOpenProjectFolderClick(Sender: TObject);
begin
  if Assigned(ProjectFile.Project) then
    OpenContainingFolder(ProjectFile.Project.FileName);
end;

procedure TfrmModelEditor.cmdRemoveWordlistClick(Sender: TObject);
var
  wordlist: TWordlist;
begin
  wordlist := wordlists[gridWordlists.Row];
  if wordlist.Frame.Modified then
  begin
    case MessageDlg(Format('Save changes to wordlist %s before removing it?',
        [ExtractFileName(wordlist.Filename)]),
        mtConfirmation, mbYesNoCancel, 0) of
      mrYes: wordlist.Frame.SaveToFile(wordlist.Frame.Filename);
      mrNo: ;
      mrCancel: Exit;
    end;
  end;

  parser.Wordlists.Delete(gridWordlists.Row);
  Inc(FSetup);
  try
    FillDetails;
    UpdateWordlistTabs;
  finally
    Dec(FSetup);
  end;
  Modified := True;
end;

procedure TfrmModelEditor.cmdSendURLsToEmailClick(Sender: TObject);
begin
  with TfrmSendURLsToEmail.Create(Application.MainForm) do
  try
    //TODO: KeyboardName := Self.FKeyboardParser.GetSystemStoreValue(ssName);
    Hosts.Assign(lbDebugHosts.Items);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmModelEditor.CodeFontChanged;
var
  i: Integer;
begin
  inherited;
  // We always use the CodeFont for the source frame at present because it is
  // a .ts file
  frameSource.CharFont := CodeFont;
  frameSource.CodeFont := CodeFont;
  for i := 0 to wordlists.Count - 1 do
    wordlists[i].Frame.CodeFont := CodeFont;
end;

procedure TfrmModelEditor.CharFontChanged;
var
  i: Integer;
begin
  inherited;
  // We always use the CodeFont for the source frame at present because it is
  // a .ts file
  frameSource.CharFont := CodeFont;
  frameSource.CodeFont := CodeFont;
  for i := 0 to wordlists.Count - 1 do
    wordlists[i].Frame.CharFont := CharFont;
end;

{ TfrmModelEditor.TWordlist }

destructor TfrmModelEditor.TWordlist.Destroy;
var
  pages: TPageControl;
  NewTab: TTabSheet;
begin
  Frame.Free;
  // Workaround for https://quality.embarcadero.com/browse/RSP-32327
  pages := Tab.PageControl;
  NewTab := pages.ActivePage;
  Tab.Free;
  pages.ActivePage := NewTab;
  inherited Destroy;
end;

procedure TfrmModelEditor.UpdateQRCode;
var
  b: Vcl.Graphics.TBitmap;
begin
  imgQRCode.Picture := nil;
  if lbDebugHosts.ItemIndex >= 0 then
  begin
    b := Vcl.Graphics.TBitmap.Create;
    try
      DrawQRCode(lbDebugHosts.Items[lbDebugHosts.ItemIndex], b);
      imgQRCode.Picture.Bitmap := b;
    finally
      b.Free;
    end;
  end;
end;

end.
