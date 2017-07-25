(*
  Name:             UframeJSONEditor
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      30 Apr 2015

  Modified Date:    24 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          30 Apr 2015 - mcdurdin - I4678 - V9.0 - Fixup Ctrl+PgUp, Ctrl+PgDn, Alt+Left, Alt+Right hotkeys
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
                    24 Aug 2015 - mcdurdin - I4875 - RTL flag is not mapped in the JSON editor
                    24 Aug 2015 - mcdurdin - I4878 - Blank font filename entry in JSON metadata UI editor crashes on save [CrashID:tike.exe_9.0.516.0_009CD793_EAssertionFailed]
*)
unit UframeJSONEditor;   // I4796

interface

uses
  System.Generics.Collections,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Grids, Vcl.StdCtrls,

  JSONKeyboardInfo,
  UfrmMDIChild,
  KeyboardParser,
  UframeTextEditor, Vcl.ComCtrls;

type
  TframeJSONEditor = class(TForm)
    pages: TPageControl;
    pageCode: TTabSheet;
    pageDesign: TTabSheet;
    sbJSON: TScrollBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblLanguages: TLabel;
    lblLanguagesHint: TLabel;
    editKeyboardBaseURI: TEdit;
    editFontBaseURI: TEdit;
    gridFonts: TStringGrid;
    gridOSKFonts: TStringGrid;
    cmdAddFont: TButton;
    cmdRemoveFont: TButton;
    cmdAddOSKFont: TButton;
    cmdRemoveOSKFont: TButton;
    gridLanguages: TStringGrid;
    cmdAddLanguage: TButton;
    cmdRemoveLanguage: TButton;
    procedure FormCreate(Sender: TObject);
    procedure editKeyboardBaseURIChange(Sender: TObject);
    procedure cmdAddFontClick(Sender: TObject);
    procedure cmdAddOSKFontClick(Sender: TObject);
    procedure cmdAddLanguageClick(Sender: TObject);
    procedure cmdRemoveFontClick(Sender: TObject);
    procedure cmdRemoveOSKFontClick(Sender: TObject);
    procedure cmdRemoveLanguageClick(Sender: TObject);
    procedure gridFontsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure pagesChanging(Sender: TObject; var AllowChange: Boolean);
  private
    frameTextEditor: TframeTextEditor;
    FLoading: Boolean;
    FFileName: string;
//    FOwner: TForm;
    procedure CodeModified(Sender: TObject);
    function TransferUIToJSON: Boolean;
    function TransferJSONToUI(ASilent: Boolean): Boolean;
    procedure SetFileName(const Value: string);
    function InKeyboardEditor: Boolean;
    function GetKeyboardParser: TKeyboardParser;
    procedure EnableControls;
    procedure RemoveCurrentGridRow(grid: TStringGrid);
    property KeyboardParser: TKeyboardParser read GetKeyboardParser;
  public
    procedure Save;
    function Validate(ASilent: Boolean): Boolean;
    procedure ChangeView(FView: TCodeDesignView);   // I4678
    property FileName: string read FFileName write SetFileName;
  end;

implementation

uses
  CompileKeymanWeb,
  kmxfile,
  TextFileFormat,
  UfrmKeymanWizard;

{$R *.dfm}

procedure TframeJSONEditor.CodeModified(Sender: TObject);
begin
  if InKeyboardEditor then
    (Owner as TfrmKeymanWizard).Modified := True;
end;

procedure TframeJSONEditor.editKeyboardBaseURIChange(Sender: TObject);
begin
  if InKeyboardEditor then
    (Owner as TfrmKeymanWizard).Modified := True;
end;

procedure TframeJSONEditor.EnableControls;
begin
  if gridFonts.RowCount > 1 then gridFonts.FixedRows := 1;
  if gridOSKFonts.RowCount > 1 then gridOSKFonts.FixedRows := 1;
  if gridLanguages.RowCount > 1 then gridLanguages.FixedRows := 1;
  gridFonts.Enabled := gridFonts.RowCount > 1;
  gridOSKFonts.Enabled := gridOSKFonts.RowCount > 1;
  gridLanguages.Enabled := gridLanguages.RowCount > 1;
  cmdRemoveFont.Enabled := gridFonts.RowCount > 1;
  cmdRemoveOSKFont.Enabled := gridOSKFonts.RowCount > 1;
  cmdRemoveLanguage.Enabled := gridLanguages.RowCount > 1;
end;

procedure TframeJSONEditor.FormCreate(Sender: TObject);
begin
  gridFonts.ColCount := 3;
  gridFonts.Cells[0,0] := 'Face Name';
  gridFonts.Cells[1,0] := 'Size';
  gridFonts.Cells[2,0] := 'Filename';
  gridFonts.ColWidths[0] := 120;
  gridFonts.ColWidths[1] := 50;
  gridFonts.ColWidths[2] := 120;

  gridOSKFonts.ColCount := 3;
  gridOSKFonts.Cells[0,0] := 'Face Name';
  gridOSKFonts.Cells[1,0] := 'Size';
  gridOSKFonts.Cells[2,0] := 'Filename';
  gridOSKFonts.ColWidths[0] := 120;
  gridOSKFonts.ColWidths[1] := 50;
  gridOSKFonts.ColWidths[2] := 120;

  gridLanguages.ColCount := 2;
  gridLanguages.Cells[0, 0] := 'ID';
  gridLanguages.Cells[1, 0] := 'Name';
  gridLanguages.ColWidths[0] := 50;
  gridLanguages.ColWidths[1] := 120;

  frameTextEditor := TframeTextEditor.Create(Self);
  frameTextEditor.TextFileFormat := tffUTF8;
  frameTextEditor.Parent := pageCode;
  frameTextEditor.Align := alClient;
  frameTextEditor.OnChanged := CodeModified;
  frameTextEditor.Visible := True;
end;

function TframeJSONEditor.GetKeyboardParser: TKeyboardParser;
begin
  Result := (Owner as TfrmKeymanWizard).Parser;
end;

procedure TframeJSONEditor.gridFontsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  if InKeyboardEditor then
    (Owner as TfrmKeymanWizard).Modified := True;
end;

function TframeJSONEditor.InKeyboardEditor: Boolean;
begin
  Result := Assigned(Owner) and
    (Owner is TfrmKeymanWizard) and
    ((Owner as TfrmKeymanWizard).Parser <> nil);
end;

procedure TframeJSONEditor.Save;
begin
  if pages.ActivePage = pageDesign then
    TransferUIToJSON;
  frameTextEditor.SaveToFile(FFileName);
end;

procedure TframeJSONEditor.SetFileName(const Value: string);
begin
  FLoading := True;
  try
    FFileName := Value;
    frameTextEditor.LoadFromFile(Value, tffUTF8);
    if not TransferJSONToUI(True) then
      // If we cannot load successfully, just show the code
      // We'll validate at compile time
      pages.ActivePage := pageCode;
  finally
    FLoading := False;
  end;

  EnableControls;
end;

procedure TframeJSONEditor.pagesChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  if FLoading then Exit;

  if pages.ActivePage = pageCode
    then AllowChange := TransferJSONToUI(False)
    else AllowChange := TransferUIToJSON;
end;

function TframeJSONEditor.TransferJSONToUI(ASilent: Boolean): Boolean;
var
  i: Integer;
begin
  with TJSONKeyboardInfo.Create do
  try
    if not Read(frameTextEditor.EditorText) then
    begin
      if not ASilent then
      begin
        ShowMessage(ReadError);
        try
          frameTextEditor.memo.SelStart := ReadErrorOffset;
          frameTextEditor.memo.SelLength := 0;
        except
          ; // if for some reason the editor cannot do this, let's not stress over it
        end;
      end;
      Exit(False);
    end;

    // Map JSON source across to controls
    editKeyboardBaseURI.Text := KeyboardBaseUri;
    editFontBaseURI.Text := FontBaseUri;

    gridFonts.RowCount := Fonts.Count+1;
    for i := 0 to Fonts.Count-1 do
    begin
      gridFonts.Cells[0, i+1] := Fonts[i].Family;
      gridFonts.Cells[1, i+1] := Fonts[i].Size;
      gridFonts.Cells[2, i+1] := Fonts[i].FileName;
    end;

    gridOSKFonts.RowCount := OSKFonts.Count+1;
    for i := 0 to OSKFonts.Count-1 do
    begin
      gridOSKFonts.Cells[0, i+1] := OSKFonts[i].Family;
      gridOSKFonts.Cells[1, i+1] := OSKFonts[i].Size;
      gridOSKFonts.Cells[2, i+1] := OSKFonts[i].FileName;
    end;

    gridLanguages.RowCount := Languages.Count+1;
    for i := 0 to Languages.Count-1 do
    begin
      gridLanguages.Cells[0, i+1] := Languages[i].ID;
      gridLanguages.Cells[1, i+1] := Languages[i].Name;
    end;
  finally
    Free;
  end;

  Result := True;
end;

function TframeJSONEditor.TransferUIToJSON: Boolean;
var
  FFont: TJSONKeyboardFont;
  i: Integer;
  FLanguage: TJSONKeyboardLanguage;
begin
  // Load the JSON from the text, and apply the new fields
  with TJSONKeyboardInfo.Create do
  try
    if not Read(frameTextEditor.EditorText) then
    begin
      // Technically, this should never happen
      Assert(False, 'Should not be transferring to invalid JSON; UI should not permit this to happen.');
      ShowMessage(ReadError);
      Exit(False);
    end;

    // Map control data back
      //Device := 'any';
    KeyboardBaseUri := editKeyboardBaseURI.Text;
    FontBaseUri := editFontBaseURI.Text;

    // Font data
    Fonts.Clear;
    for i := 1 to gridFonts.RowCount - 1 do
    begin
      if (Trim(gridFonts.Cells[0, i]) = '') or (Trim(gridFonts.Cells[2, i]) = '') then   // I4878
        Continue;
      FFont := TJSONKeyboardFont.Create;
      FFont.Family := gridFonts.Cells[0, i];
      FFont.Size := gridFonts.Cells[1, i];
      FFont.FileName := gridFonts.Cells[2, i];
      Fonts.Add(FFont);
    end;

    OSKFonts.Clear;
    for i := 1 to gridOSKFonts.RowCount - 1 do
    begin
      if (Trim(gridOSKFonts.Cells[0, i]) = '') or (Trim(gridOSKFonts.Cells[2, i]) = '') then   // I4878
        Continue;
      FFont := TJSONKeyboardFont.Create;
      FFont.Family := gridOSKFonts.Cells[0, i];
      FFont.Size := gridOSKFonts.Cells[1, i];
      FFont.FileName := gridOSKFonts.Cells[2, i];
      OSKFonts.Add(FFont);
    end;

    Languages.Clear;
    for i := 1 to gridLanguages.RowCount - 1 do
    begin
      FLanguage := TJSONKeyboardLanguage.Create;
      FLanguage.ID := gridLanguages.Cells[0, i];
      FLanguage.Name := gridLanguages.Cells[1, i];
      Languages.Add(FLanguage);
    end;

    // Map keyboard data back, if attached to a keyboard editor
    if InKeyboardEditor then
    begin
      KeyboardID := GetKeymanWebCompiledNameFromFileName(KeyboardParser.FileName);
      KeyboardName := KeyboardParser.GetSystemStoreValue(ssName);
      KeyboardVersion := KeyboardParser.GetSystemStoreValue(ssKeyboardVersion);
      if KeyboardVersion = '' then KeyboardVersion := '1.0';
      KeyboardFilename := GetKeymanWebCompiledFileName(KeyboardID, KeyboardVersion);
      RTL := KeyboardParser.GetSystemStoreValue(ssKMW_RTL) = '1';   // I4875
      LastModified := Now;  // TODO: Need to be careful with this one?
    end;

    // Save back to string
    frameTextEditor.EditorText := Write;
  finally
    Free;
  end;

  Result := True;
end;

function TframeJSONEditor.Validate(ASilent: Boolean): Boolean;
begin
  if pages.ActivePage = pageDesign then
    Result := TransferUIToJSON
  else
  begin
    Result := TransferJSONToUI(ASilent)
  end;
end;

procedure TframeJSONEditor.ChangeView(FView: TCodeDesignView);   // I4678
begin
  case FView of
    cdvDesign: pages.ActivePage := pageDesign;
    cdvCode:   pages.ActivePage := pageCode;
  end;
end;

procedure TframeJSONEditor.cmdAddFontClick(Sender: TObject);
begin
  gridFonts.RowCount := gridFonts.RowCount + 1;
  EnableControls;
end;

procedure TframeJSONEditor.cmdAddLanguageClick(Sender: TObject);
begin
  gridLanguages.RowCount := gridLanguages.RowCount + 1;
  EnableControls;
end;

procedure TframeJSONEditor.cmdAddOSKFontClick(Sender: TObject);
begin
  gridOSKFonts.RowCount := gridOSKFonts.RowCount + 1;
  EnableControls;
end;

procedure TframeJSONEditor.RemoveCurrentGridRow(grid: TStringGrid);
var
  y: Integer;
  x: Integer;
begin
  for y := grid.Row to grid.RowCount - 2 do
  begin
    for x := 0 to grid.ColCount - 1 do
    begin
      grid.Cells[x, y] := grid.Cells[x, y+1];
      grid.Objects[x, y] := grid.Objects[x, y+1];
    end;
  end;
  grid.RowCount := grid.RowCount - 1;
end;

procedure TframeJSONEditor.cmdRemoveFontClick(Sender: TObject);
begin
  RemoveCurrentGridRow(gridFonts);
  EnableControls;
end;

procedure TframeJSONEditor.cmdRemoveLanguageClick(Sender: TObject);
begin
  RemoveCurrentGridRow(gridLanguages);
  EnableControls;
end;

procedure TframeJSONEditor.cmdRemoveOSKFontClick(Sender: TObject);
begin
  RemoveCurrentGridRow(gridOSKFonts);
  EnableControls;
end;

end.
