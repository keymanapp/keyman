unit Keyman.Developer.UI.UfrmModelEditor;

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

  LeftTabbedPageControl,
  UfrmMDIEditor,
  UframeTextEditor,
  Keyman.Developer.UI.UframeWordlistEditor,
  Keyman.Developer.System.LexicalModelParser,
  Keyman.Developer.System.LexicalModelParserTypes;

type
  TfrmModelEditor = class(TfrmTikeEditor)
    pages: TLeftTabbedPageControl;
    pageDetails: TTabSheet;
    sbDetails: TScrollBox;
    panWordlists: TPanel;
    lblWordlists: TLabel;
    gridWordlists: TStringGrid;
    cmdNewWordlist: TButton;
    cmdRemoveWordlist: TButton;
    panBasicInformation: TPanel;
    lblFormat: TLabel;
    lblBasicInformation: TLabel;
    pageSource: TTabSheet;
    pageCompile: TTabSheet;
    Panel1: TPanel;
    lblCongrats: TLabel;
    cmdCompile: TButton;
    cmdAddToProject: TButton;
    cmdOpenContainingFolder2: TButton;
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
    cmdAddExistingWordlist: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure cmdNewWordlistClick(Sender: TObject);
    procedure cmdAddExistingWordlistClick(Sender: TObject);
    procedure cmdRemoveWordlistClick(Sender: TObject);
    procedure gridWordlistsDblClick(Sender: TObject);
    procedure pagesChanging(Sender: TObject; var AllowChange: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure cbFormatClick(Sender: TObject);
    procedure cbWordBreakerClick(Sender: TObject);
    procedure memoCommentsChange(Sender: TObject);
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
      end;
  private
    model: TStrings;
    parser: TLexicalModelParser;
    wordlists: TWordlists;
    frameSource: TframeTextEditor;
    FSetup: Integer;
    procedure FillDetails;
    procedure EnableControls;
    procedure UpdateWordlistTabs;
    function AddWordlistTab(
      const WordlistFilename: string): TfrmModelEditor.TWordlist;
    function MoveDesignToSource: Boolean;
    function MoveSourceToDesign: Boolean;
    { Private declarations }
  protected
    function GetHelpTopic: string; override;
    function DoOpenFile: Boolean; override;
    function DoSaveFile: Boolean; override;
    function GetFileNameFilter: string; override;
    function GetDefaultExt: string; override;

  public
    procedure FindError(const Filename: string; s: string; line: Integer); override;   // I4081
  end;

implementation

uses
  TextFileFormat;

{$R *.dfm}

function FormatToIndex(format: TLexicalModelFormat): Integer;
begin
  Result := Ord(format) - 1;  // unknown = -1
end;

function WordBreakerToIndex(wordBreaker: TLexicalModelWordBreaker): Integer;
begin
  Result := Ord(wordBreaker) - 1;  // unknown = -1
end;

function FormatFromIndex(format: Integer): TLexicalModelFormat;
begin
  Result := TLexicalModelFormat(format+1);  // unknown = -1
end;

function WordBreakerFromIndex(wordBreaker: Integer): TLexicalModelWordBreaker;
begin
  Result := TLexicalModelWordBreaker(wordBreaker+1);  // unknown = -1
end;

{ TfrmModelEditor }

function TfrmModelEditor.DoOpenFile: Boolean;
begin
  model := TStringList.Create;
  model.LoadFromFile(FileName);
  parser := TLexicalModelParser.Create(model.Text);
  wordlists := TWordlists.Create;

  Inc(FSetup);
  try
    FillDetails;
    UpdateWordlistTabs;
  finally
    Dec(FSetup);
  end;

  Result := True;
end;

function TfrmModelEditor.DoSaveFile: Boolean;
begin
  // TODO
  Result := True;
end;

procedure TfrmModelEditor.EnableControls;
var
  e: Boolean;
begin
  e := parser.IsEditable;
  lblFormat.Enabled := e;
  cbFormat.Enabled := e;
  lblWordBreaker.Enabled := e;
  cbWordBreaker.Enabled := e;
  lblComments.Enabled := e;
  memoComments.Enabled := e;
  cmdAddExistingWordlist.Enabled := e;
  gridWordlists.Enabled := e and (parser.Wordlists.Count > 0);
  cmdRemoveWordlist.Enabled := e and (parser.Wordlists.Count > 0);
  lblReadOnly.Visible := not e;
end;

procedure TfrmModelEditor.FindError(const Filename: string; s: string;
  line: Integer);
begin
  inherited;
  // TODO
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
//    frameSource.OnChanged := SourceChanged;
    frameSource.TextFileFormat := tffUTF8;

    pages.ActivePage := pageDetails;
  finally
    Dec(FSetup);
  end;
end;

procedure TfrmModelEditor.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(parser);
  FreeAndNil(model);
  FreeAndNil(wordlists);
end;

function TfrmModelEditor.GetDefaultExt: string;
begin
  Result := '.model.ts';  // TODO: test if this actually works!
end;

function TfrmModelEditor.GetFileNameFilter: string;
begin
  Result := 'Lexical model files (*.model.ts)|*.model.ts|All files (*.*)|*.*';
end;

function TfrmModelEditor.GetHelpTopic: string;
begin
  Result := ''; //SHelpTopic_Context_WordlistEditor;  // TODO: use a better topic
end;

procedure TfrmModelEditor.gridWordlistsDblClick(Sender: TObject);
begin
  pages.ActivePageIndex := gridWordlists.Row + 1;
end;

procedure TfrmModelEditor.memoCommentsChange(Sender: TObject);
begin
  if FSetup > 0 then
    Exit;
  parser.Comment := memoComments.Text;
  Modified := True;
end;

function TfrmModelEditor.MoveSourceToDesign: Boolean;
begin
  Inc(FSetup);
  try
    FreeAndNil(parser);
    parser := TLexicalModelParser.Create(frameSource.EditorText);
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

{ --------- Details tab --------- }

procedure TfrmModelEditor.FillDetails;
var
  i: Integer;
begin
  cbFormat.ItemIndex := FormatToIndex(parser.Format);
  cbWordBreaker.ItemIndex := WordBreakerToIndex(parser.WordBreaker);
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
  Result.Frame.LoadFromFile(ExtractFilePath(Filename) + WordlistFilename);
  Result.Frame.Align := alClient;

  Result.Frame.Parent := Result.Tab;
  Result.Frame.Visible := True;
end;

{ TfrmModelEditor.TWordlists }

function TfrmModelEditor.TWordlists.IndexOfFilename(
  const Filename: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if SameText(Items[Result].Filename, Filename) then
      Exit;
  Result := -1;
end;

{ Wordlist management }

procedure TfrmModelEditor.cbFormatClick(Sender: TObject);
begin
  if FSetup > 0 then
    Exit;
  parser.Format := FormatFromIndex(cbFormat.ItemIndex);
  Modified := True;
end;

procedure TfrmModelEditor.cbWordBreakerClick(Sender: TObject);
begin
  if FSetup > 0 then
    Exit;
  parser.WordBreaker := WordBreakerFromIndex(cbWordBreaker.ItemIndex);
  Modified := True;
end;

procedure TfrmModelEditor.cmdAddExistingWordlistClick(Sender: TObject);
begin
  if dlgAddWordlist.Execute then
  begin
    parser.Wordlists.Add(ExtractRelativePath(Filename, dlgAddWordlist.FileName));
    Inc(FSetup);
    try
      FillDetails;
      UpdateWordlistTabs;
    finally
      Dec(FSetup);
    end;
    Modified := True;
  end;
end;

procedure TfrmModelEditor.cmdNewWordlistClick(Sender: TObject);
begin
  if dlgAddWordlist.Execute then
  begin
    parser.Wordlists.Add(ExtractRelativePath(Filename, dlgAddWordlist.FileName));
    Inc(FSetup);
    try
      FillDetails;
      UpdateWordlistTabs;
    finally
      Dec(FSetup);
    end;
    Modified := True;
  end;
end;

procedure TfrmModelEditor.cmdRemoveWordlistClick(Sender: TObject);
begin
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

{ TfrmModelEditor.TWordlist }

destructor TfrmModelEditor.TWordlist.Destroy;
begin
  Frame.Free;
  Tab.Free;
  inherited Destroy;
end;

end.
