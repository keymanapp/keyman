unit Keyman.Developer.UI.UframeWordlistEditor;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  Winapi.Messages,
  Winapi.Windows,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Grids,

  StringGridEditControlled,

  Keyman.System.WordlistTsvFile,
  UframeTextEditor,
  UfrmMDIEditor, Vcl.StdCtrls, Vcl.ExtCtrls, UfrmTike;

type
  TframeWordlistEditor = class(TTIKEForm)
    pages: TPageControl;
    pageDesign: TTabSheet;
    pageCode: TTabSheet;
    gridWordlist: TStringGridEditControlled;
    panGridControls: TPanel;
    cmdDeleteRow: TButton;
    cmdSortByFrequency: TButton;
    procedure pagesChanging(Sender: TObject; var AllowChange: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure gridWordlistSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure gridWordlistDrawCell(Sender: TObject; ACol, ARow: Integer;
      ARect: TRect; State: TGridDrawState);
    procedure cmdDeleteRowClick(Sender: TObject);
    procedure gridWordlistClick(Sender: TObject);
    procedure cmdSortByFrequencyClick(Sender: TObject);
  private
    FWordlist: TWordlistTsvFile;
    frameSource: TframeTextEditor;
    FSetup: Integer;
    FModified: Boolean;
    FOnChanged: TNotifyEvent;
    FFilename: string;
    FCodeFont: TFont;
    FCharFont: TFont;
    procedure UpdateData;
    function MoveCodeToWordlist: Boolean;
    procedure FillCode;
    procedure FillGrid;
    function MoveDesignToWordlist: Boolean;
    procedure SourceChanged(Sender: TObject);
    procedure FillGridNewRow;
    procedure EnableControls;
    procedure SetModified(const Value: Boolean);
    procedure SetCharFont(const Value: TFont);
    procedure SetCodeFont(const Value: TFont);
    procedure CharCodeFontChanged(Sender: TObject);
    procedure CharFontChanged;
    procedure CodeFontChanged;
  protected
    function GetHelpTopic: string; override;
    function DoOpenFile: Boolean;
    function DoSaveFile: Boolean;
  public
    procedure FindError(const Filename: string; s: string; line: Integer);   // I4081
    function LoadFromFile(const Filename: string): Boolean;
    function SaveToFile(const Filename: string): Boolean;
    property Filename: string read FFilename;
    property Modified: Boolean read FModified write SetModified;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;

    property CodeFont: TFont read FCodeFont write SetCodeFont;
    property CharFont: TFont read FCharFont write SetCharFont;
  end;

implementation

{$R *.dfm}

uses
  Keyman.Developer.System.HelpTopics,
  TextFileFormat,
  UfrmMain,
  UfrmMessages;

const
  ColWidth_Word = 120;
  ColWidth_Frequency = 64;
  S_AddRowText = 'Add word...';

{ TfrmWordlistEditor }

procedure TframeWordlistEditor.FormCreate(Sender: TObject);
begin
  inherited;
  Inc(FSetup);
  try
    FCharFont := TFont.Create;
    FCharFont.OnChange := CharCodeFontChanged;
    FCodeFont := TFont.Create;
    FCodeFont.OnChange := CharCodeFontChanged;

    FWordlist := TWordlistTsvFile.Create;
    frameSource := TframeTextEditor.Create(Self);
    frameSource.Parent := pageCode;
    frameSource.Align := alClient;
    frameSource.EditorFormat := efWordlistTsv;
    frameSource.Visible := True;
    frameSource.OnChanged := SourceChanged;
    frameSource.TextFileFormat := tffUTF8;
//TODO:    frameSource.UseTabs := True;
    pages.ActivePage := pageDesign;

    gridWordlist.Cells[0, 0] := 'Word Form';
    gridWordlist.Cells[1, 0] := 'Count';
    gridWordlist.Cells[2, 0] := 'Comment';
    gridWordlist.ColWidths[0] := ColWidth_Word;
    gridWordlist.ColWidths[1] := ColWidth_Frequency;
    gridWordlist.ColWidths[2] := gridWordlist.ClientWidth - ColWidth_Word - ColWidth_Frequency - 2;
  finally
    Dec(FSetup);
  end;
end;

procedure TframeWordlistEditor.FormDestroy(Sender: TObject);
begin
  inherited;
  FWordlist.Free;
  FreeAndNil(FCharFont);
  FreeAndNil(FCodeFont);
end;

procedure TframeWordlistEditor.FormResize(Sender: TObject);
begin
  inherited;
  gridWordlist.ColWidths[2] := gridWordlist.ClientWidth - ColWidth_Word - ColWidth_Frequency - 2;
end;

procedure TframeWordlistEditor.CharCodeFontChanged(Sender: TObject);
begin
  if Sender = FCharFont
    then CharFontChanged
    else CodeFontChanged;
end;

procedure TframeWordlistEditor.CharFontChanged;
begin
  // We always use the CharFont for the wordlist tsv
  frameSource.CodeFont := CharFont;
  frameSource.CharFont := CharFont;
  gridWordlist.Font := CharFont;
  gridWordlist.Canvas.Font := CharFont;
  gridWordlist.DefaultRowHeight := gridWordlist.Canvas.TextHeight('A') + 2;
end;

procedure TframeWordlistEditor.cmdDeleteRowClick(Sender: TObject);
begin
  FWordlist.RemoveWord(gridWordlist.Row-1);
  FillGrid;
  EnableControls;
  Modified := True;
end;

procedure TframeWordlistEditor.cmdSortByFrequencyClick(Sender: TObject);
begin
  FWordlist.SortByFrequency;
  FillGrid;
  Modified := True;
end;

procedure TframeWordlistEditor.CodeFontChanged;
begin
  // We always use the CharFont for the wordlist tsv
  frameSource.CodeFont := CharFont;
  frameSource.CharFont := CharFont;
end;

function TframeWordlistEditor.DoOpenFile: Boolean;
begin
  if FileExists(FileName) then
  begin
    try
      FWordlist.LoadFromFile(FileName);
    except
      on E:EEncodingError do
      begin
        ShowMessage('Error loading '+FileName+': '+E.Message);
        Exit(False);
      end;
    end;
    FModified := False;
  end;
  UpdateData;
  Result := True;
end;

function TframeWordlistEditor.DoSaveFile: Boolean;
begin
  MoveCodeToWordlist; // Does nothing if not in code page
  FWordlist.SaveToFile(FileName);
  Result := True;
end;

procedure TframeWordlistEditor.EnableControls;
var
  e: Boolean;
begin
  e := gridWordlist.Row < gridWordlist.RowCount - 1;
  cmdDeleteRow.Enabled := e;
  cmdSortByFrequency.Enabled := e;
end;

procedure TframeWordlistEditor.FindError(const Filename: string; s: string; line: Integer);
begin
  if pages.ActivePage <> pageCode then
  begin
    if MoveDesignToWordlist then
      pages.ActivePage := pageCode;
  end;

  if line > 0 then
    Dec(line);
  frameSource.FindError(line);
end;

function TframeWordlistEditor.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_WordlistEditor;
end;

procedure TframeWordlistEditor.gridWordlistClick(Sender: TObject);
begin
  EnableControls;
end;

procedure TframeWordlistEditor.gridWordlistDrawCell(Sender: TObject; ACol,
  ARow: Integer; ARect: TRect; State: TGridDrawState);
var
  LText: string;
begin
  inherited;
  gridWordlist.Canvas.Font := CharFont;
  if ARow = gridWordlist.RowCount - 1 then
  begin
    // Drawing last row
    gridWordlist.Canvas.Font.Style := [fsItalic];
  end
  else if gridWordlist.Cells[0, ARow] = '#' then
  begin
    // Drawing a comment cell
    gridWordlist.Canvas.Font.Style := [fsBold];
  end
  else
    gridWordlist.Canvas.Font.Style := [];

  LText := gridWordlist.Cells[ACol, ARow];
  gridWordlist.Canvas.TextRect(ARect, ARect.Left+2,
    ARect.Top+((ARect.Height - gridWordlist.Canvas.TextHeight(LText)) div 2), LText);
end;

procedure TframeWordlistEditor.gridWordlistSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  w: TWordlistWord;
  TrimmedValue: string;
  Frequency: Integer;
begin
  if FSetup > 0 then Exit;

  Inc(FSetup);
  try
    if ARow = gridWordlist.RowCount - 1 then
    begin
      if (Value = '') or (Value = S_AddRowText) then
        Exit;

      w.Word := Value;
      w.Frequency := 0;
      w.Comment := '';
      FWordlist.AddWord(w);
      gridWordlist.RowCount := gridWordlist.RowCount + 1;
      FillGridNewRow;
    end
    else
    begin
      TrimmedValue := Value.Trim;
      Frequency := StrToIntDef(TrimmedValue.Replace(',', '', [rfReplaceAll]), 0);
      w := FWordlist.Word[ARow-1];
      case ACol of
        0: if w.Word = TrimmedValue then Exit else w.Word := TrimmedValue;
        1: if w.Frequency = Frequency then Exit else w.Frequency := Frequency;
        2: if w.Comment = TrimmedValue then Exit else w.Comment := TrimmedValue;
      end;
      FWordlist.Word[ARow-1] := w;
    end;

    Modified := True;
  finally
    Dec(FSetup);
  end;
end;

procedure TframeWordlistEditor.pagesChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  if FSetup > 0 then Exit;

  if pages.ActivePage = pageCode then
    AllowChange := MoveCodeToWordlist
  else
    AllowChange := MoveDesignToWordlist
end;

function TframeWordlistEditor.MoveCodeToWordlist: Boolean;
begin
  if pages.ActivePage <> pageCode then
    Exit(False);

  FWordlist.Text := frameSource.EditorText;
  FillGrid;
  Result := True;
end;

function TframeWordlistEditor.MoveDesignToWordlist: Boolean;
begin
  FillCode;
  Result := True;
end;

function TframeWordlistEditor.LoadFromFile(const Filename: string): Boolean;
begin
  FFilename := Filename;
  Result := DoOpenFile;
end;

procedure TframeWordlistEditor.UpdateData;
begin
  if pages.ActivePage = pageCode
    then FillCode
    else FillGrid;
end;

function TframeWordlistEditor.SaveToFile(const Filename: string): Boolean;
begin
  FFilename := Filename;
  Result := DoSaveFile;
end;

procedure TframeWordlistEditor.SetCharFont(const Value: TFont);
begin
  FCharFont.Assign(Value);
end;

procedure TframeWordlistEditor.SetCodeFont(const Value: TFont);
begin
  FCodeFont.Assign(Value);
end;

procedure TframeWordlistEditor.SetModified(const Value: Boolean);
begin
  FModified := Value;
  if Value and Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TframeWordlistEditor.SourceChanged(Sender: TObject);
begin
  if FSetup = 0 then
  begin
    Modified := True;
  end;
end;

procedure TframeWordlistEditor.FillCode;
begin
  frameSource.EditorText := FWordlist.Text;
end;

procedure TframeWordlistEditor.FillGrid;
var
  i: Integer;
  w: TWordlistWord;
begin
  gridWordlist.RowCount := FWordlist.Count + 2;
  for i := 0 to FWordlist.Count - 1 do
  begin
    w := FWordlist.Word[i];
    gridWordlist.Cells[0, i+1] := w.Word;
    if w.Frequency = 0
      then gridWordlist.Cells[1, i+1] := ''
      else gridWordlist.Cells[1, i+1] := IntToStr(w.Frequency);
    gridWordlist.Cells[2, i+1] := w.Comment;
  end;
  FillGridNewRow;
  gridWordlist.FixedRows := 1;
end;

procedure TframeWordlistEditor.FillGridNewRow;
begin
  gridWordlist.Cells[0, gridWordlist.RowCount - 1] := S_AddRowText;
  gridWordlist.Cells[1, gridWordlist.RowCount - 1] := '';
  gridWordlist.Cells[2, gridWordlist.RowCount - 1] := '';
  EnableControls;
end;

end.
