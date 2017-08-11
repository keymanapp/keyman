(*
  Name:             UfrmLocaleEditor
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2007

  Modified Date:    3 May 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2007 - mcdurdin - Initial version
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
*)
unit UfrmLocaleEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ExtCtrls,
  TntStdCtrls, TntExtCtrls, TntGrids,
  xmldoc, xmlintf, TntDialogs;

type
  TfrmLocaleEditor = class(TForm)
    gridMessages: TTntStringGrid;
    panMessageDetails: TTntPanel;
    lblIdentifier: TTntLabel;
    lblText: TTntLabel;
    lblComment: TTntLabel;
    editIdentifier: TTntEdit;
    memoText: TTntMemo;
    memoComment: TTntMemo;
    cmdExit: TButton;
    cmdSave: TButton;
    Label1: TTntLabel;
    memoTranslation: TTntMemo;
    cmdFont: TTntButton;
    dlgFont: TFontDialog;
    cmdOpen: TButton;
    cmdNew: TButton;
    dlgSave: TTntSaveDialog;
    dlgOpen: TTntOpenDialog;
    procedure cmdFontClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmdNewClick(Sender: TObject);
    procedure cmdExitClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure gridMessagesClick(Sender: TObject);
    procedure memoTranslationChange(Sender: TObject);
    procedure cmdOpenClick(Sender: TObject);
    procedure cmdSaveClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FModified: Boolean;
    FFileName: WideString;
    FEnglishLocaleXML, FTranslatedLocaleXML: IXMLDocument;
    FEnglishNodes, FTranslatedNodes: TStringList;
    FChanging: Boolean;
    function TestModified: Boolean;
    procedure FillGrid;
    function Save: Boolean;
    function GetNodeText(node: IXMLNode): WideString;
    procedure SetTFont(const FontName: string; FontSize: Integer);
    procedure SetNodeText(node: IXMLNode; val: WideString);
    procedure UpdateCaption;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmLocaleEditor: TfrmLocaleEditor;

implementation

uses
  ErrorControlledRegistry;

{$R *.dfm}

procedure TfrmLocaleEditor.cmdExitClick(Sender: TObject);
begin
  if not TestModified then Exit;
  Close;
end;

procedure TfrmLocaleEditor.cmdFontClick(Sender: TObject);
begin
  dlgFont.Font := memoTranslation.Font;
  if dlgFont.Execute then
  begin
    SetTFont(dlgFont.Font.Name, dlgFont.Font.Size);
  end;
end;

function TfrmLocaleEditor.TestModified: Boolean;
begin
  if not FModified then
  begin
    Result := True;
    Exit;
  end;

  case MessageDlg('Do you want to save changes to your locale.xml file?', mtConfirmation, mbYesNoCancel, 0) of
    mrYes: Result := Save;
    mrNo: Result := True;
    mrCancel: Result := False;
  else
    Result := False;
  end;
end;

function TfrmLocaleEditor.GetNodeText(node: IXMLNode): WideString;
  function GetChildNodesText(node: IXMLNode): WideString;
  var
    i: Integer;
  begin
    Result := '';
    for i := 0 to node.ChildNodes.Count - 1 do
      Result := Result + node.ChildNodes[i].XML;
  end;

begin
  if node.IsTextElement then
    Result := node.Text
  else
  begin
    Result := GetChildNodesText(node);
  end;
end;

procedure TfrmLocaleEditor.SetNodeText(node: IXMLNode; val: WideString);
var
  i: Integer;
  n: Integer;
  s: IXMLNode;
begin
  if node.HasChildNodes then
  begin
    for i := node.ChildNodes.Count - 1 downto 0 do
      node.ChildNodes.Delete(i);
  end;

  n := Pos('<', val);
  while n > 0 do
  begin
    if n > 1 then
    begin
      s := FTranslatedLocaleXML.CreateNode(Copy(val, 1, n-1), ntText);
      node.ChildNodes.Add(s);
    end;
    Delete(val, 1, n);

    n := Pos('>', val);
    if n = 0 then Exit;
    s := FTranslatedLocaleXML.CreateNode(Copy(val, 1, n-1));
    node.ChildNodes.Add(s);

    Delete(val, 1, n);
    n := Pos('<', val);
    if n = 0 then Exit;
    s.NodeValue := Copy(val, 1, n-1);
    Delete(val, 1, n);
    n := Pos('>', val);
    if n = 0 then Exit;
    Delete(val, 1, n);
    n := Pos('<', val);
  end;

  if val <> '' then
  begin
    s := FTranslatedLocaleXML.CreateNode(val, ntText);
    node.ChildNodes.Add(s);
  end;
//  node.NodeValue := val;
end;

procedure TfrmLocaleEditor.gridMessagesClick(Sender: TObject);
var
  id: string;
  s: WideString;
  I, n: Integer;
  J: Integer;
begin
  id := gridMessages.Cells[0, gridMessages.Row];
  editIdentifier.Text := id;
  memoText.Text := gridMessages.Cells[1, gridMessages.Row];
  FChanging := True;
  memoTranslation.Text := gridMessages.Cells[2, gridMessages.Row];
  FChanging := False;
  n := StrToInt(FEnglishNodes.Values[id]);
  s := '';
  for I := n-1 downto 0 do
  begin
    if FEnglishLocaleXML.DocumentElement.ChildNodes[I].NodeType = ntElement then
    begin
      for J := I+1 to n-1 do
        s := s + FEnglishLocaleXML.DocumentElement.ChildNodes[J].XML + #13#10;
      Break;
    end;
  end;
  memoComment.Text := s;
end;

procedure TfrmLocaleEditor.memoTranslationChange(Sender: TObject);
var
  node: IXMLNode;
  v: string;
begin
  if FChanging then Exit;

  gridMessages.Cells[2, gridMessages.Row] := memoTranslation.Text;

  v := FTranslatedNodes.Values[gridMessages.Cells[0, gridMessages.Row]];
  if v = '' then
  begin
    node := FTranslatedLocaleXML.DocumentElement.AddChild('String');
    node.Attributes['Id'] := gridMessages.Cells[0, gridMessages.Row];
  end
  else
  begin
    node := FTranslatedLocaleXML.DocumentElement.ChildNodes[StrToInt(v)];
  end;

  SetNodeText(node, memoTranslation.Text);

  if not FModified then
  begin
    FModified := True;
    UpdateCaption;
  end;
end;

procedure TfrmLocaleEditor.FillGrid;
var
  rootE, rootT: IXMLNode;
  i: Integer;
  v: string;
  FFontSize: Integer;
  FFontName: WideString;
begin
  FChanging := True;
  FEnglishNodes.Clear;
  FTranslatedNodes.Clear;

  rootE := FEnglishLocaleXML.DocumentElement; //.ChildNodes['Locale'];
  rootT := FTranslatedLocaleXML.DocumentElement; //.ChildNodes['Locale'];

  for i := 0 to rootE.ChildNodes.Count - 1 do
  begin
//    ShowMessage(rootE.ChildNodes[i].NodeName);
    if rootE.ChildNodes[i].NodeName = 'String' then
      FEnglishNodes.Add(rootE.ChildNodes[i].Attributes['Id']+'='+IntToStr(i));
  end;

  for i := 0 to rootT.ChildNodes.Count - 1 do
    if rootT.ChildNodes[i].NodeName = 'String' then
      FTranslatedNodes.Add(rootT.ChildNodes[i].Attributes['Id']+'='+IntToStr(i));

  gridMessages.RowCount := FEnglishNodes.Count + 1;
  gridMessages.Cells[0,0] := 'ID';
  gridMessages.Cells[1,0] := 'English';
  gridMessages.Cells[2,0] := 'Translation';

  for i := 0 to FEnglishNodes.Count - 1 do
  begin
    gridMessages.Cells[0, i+1] := FEnglishNodes.Names[i];
    gridMessages.Cells[1, i+1] := GetNodeText(rootE.ChildNodes[StrToInt(FEnglishNodes.ValueFromIndex[i])]);
    v := FTranslatedNodes.Values[FEnglishNodes.Names[i]];
    if v <> ''
      then gridMessages.Cells[2, i+1] := GetNodeText(rootT.ChildNodes[StrToInt(v)])
      else gridMessages.Cells[2, i+1] := gridMessages.Cells[1, i+1];
  end;

  v := FTranslatedNodes.Values['SK_UIFontName'];
  if v <> '' then
  begin
    FFontName := GetNodeText(rootT.ChildNodes[StrToInt(v)]);
    v := FTranslatedNodes.Values['SK_UIFontSize'];
    if v <> '' then
    begin
      FFontSize := StrToIntDef(GetNodeText(rootT.ChildNodes[StrToInt(v)]), 8);
      SetTFont(FFontName, FFontSize);
    end;
  end;
  
  FChanging := False;
end;

procedure TfrmLocaleEditor.SetTFont(const FontName: string; FontSize: Integer);
begin
  dlgFont.Font.Name := FontName;
  dlgFont.Font.Size := FontSize;
  memoTranslation.Font := dlgFont.Font;
  gridMessages.Font := dlgFont.Font;
  with Canvas do
  begin
    Font := dlgFont.Font;
    gridMessages.DefaultRowHeight := TextHeight('A') + 2;
  end;
end;

function TfrmLocaleEditor.Save: Boolean;
begin
  Result := False;
  if (FFileName = '') or (GetAsyncKeyState(VK_SHIFT) < 0) then
  begin
    if dlgSave.Execute then FFileName := dlgSave.FileName
    else Exit;
  end;

  FTranslatedLocaleXML.SaveToFile(FFileName);
  FModified := False;
  UpdateCaption;
  Result := True;
end;

procedure TfrmLocaleEditor.UpdateCaption;
var
  s: string;
begin
  s := 'Translate Keyman Desktop User Interface - ';
  if FFileName = '' then
    s := s +'(Untitled)'
  else
    s := s + FFileName;
  if FModified then
    s := s + ' *';
  Caption := s;
end;

procedure TfrmLocaleEditor.cmdNewClick(Sender: TObject);
begin
  if not TestModified then Exit;

  FFileName := '';
  FModified := False;
  UpdateCaption;

  FTranslatedLocaleXML := NewXMLDocument;
  FTranslatedLocaleXML.StandAlone := 'yes';
  FTranslatedLocaleXML.Options := [];
  FTranslatedLocaleXML.XML.Text := FEnglishLocaleXML.XML.Text;
  FTranslatedLocaleXML.Active := True;

  FillGrid;
end;

procedure TfrmLocaleEditor.cmdOpenClick(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    FFileName := dlgOpen.FileName;
    
    FTranslatedLocaleXML.LoadFromFile(FFileName);
    FModified := False;
    UpdateCaption;
    FillGrid;
  end;
end;

procedure TfrmLocaleEditor.cmdSaveClick(Sender: TObject);
begin
  Save;
end;

procedure TfrmLocaleEditor.FormCreate(Sender: TObject);
var
  FRootPath: string;
begin
  FEnglishNodes := TStringList.Create;
  FTranslatedNodes := TStringList.Create;
  FEnglishLocaleXML := NewXMLDocument;
  FEnglishLocaleXML.StandAlone := 'yes';
  FEnglishLocaleXML.Options := [];

  FRootPath := '';
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly('Software\Tavultesoft\Keyman\8.0') and ValueExists('root path') then
      FRootPath := IncludeTrailingPathDelimiter(ReadString('root path'));
  finally
    Free;
  end;

  if not FileExists(FRootPath + 'xml\locale.xml') then
  begin
    ShowMessage('Cannot find the path for the English locale.xml.  Please make sure Keyman Desktop 7.0 is installed and locate the English locale.xml');
    if dlgOpen.Execute then
      FRootPath := dlgOpen.FileName
    else
    begin
      Application.Terminate;
      Application.ShowMainForm := False;
    end;
  end
  else
    FRootPath := FRootPath + 'xml\locale.xml';

  FEnglishLocaleXML.LoadFromFile(FRootPath);
  cmdNewClick(cmdNew);
end;

procedure TfrmLocaleEditor.FormDestroy(Sender: TObject);
begin
  FTranslatedLocaleXML := nil;
  FEnglishLocaleXML := nil;
  FTranslatedNodes.Free;
  FEnglishNodes.Free;
end;

procedure TfrmLocaleEditor.FormResize(Sender: TObject);
var
  x: Integer;
begin
  x := gridMessages.ColWidths[1] + gridMessages.ColWidths[2];
  gridMessages.ColWidths[1] := (gridMessages.ClientWidth - gridMessages.ColWidths[0] - 2) * gridMessages.ColWidths[1] div x;
  gridMessages.ColWidths[2] := (gridMessages.ClientWidth - gridMessages.ColWidths[0] - 2) * gridMessages.ColWidths[2] div x;
end;

end.
