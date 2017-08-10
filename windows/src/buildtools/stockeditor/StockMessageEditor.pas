(*
  Name:             StockMessageEditor
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    04 Dec 2006 - mcdurdin - Add support for manipulating sections
                    05 Dec 2006 - mcdurdin - Save to desktop.pot as well
                    17 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    17 May 2012 - mcdurdin - I3321 - V9.0 - Fixup paths in Delphi source for v9.0
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
*)
unit StockMessageEditor;  // I3306

interface

uses
  System.UITypes,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Grids, StockMessages, StockFileNames,
  StringGridEditControlled;

type
  TfrmStockMessageEditor = class(TForm)
    gridMessages: TStringGridEditControlled;
    panMessageDetails: TPanel;
    lblIdentifier: TLabel;
    lblText: TLabel;
    editIdentifier: TEdit;
    memoText: TMemo;
    lblParams: TLabel;
    cmdExit: TButton;
    cmdOK: TButton;
    lblComment: TLabel;
    cmdAdd: TButton;
    cmdDelete: TButton;
    cmdMoveToSection: TButton;
    memoComment: TMemo;
    gridParams: TStringGrid;
    cmdAddParam: TButton;
    cmdDeleteParam: TButton;
    cmdSave: TButton;
    cmdNewSection: TButton;
    cmdDeleteSection: TButton;
    cmdRenameSection: TButton;
    procedure FormCreate(Sender: TObject);
    procedure gridMessagesClick(Sender: TObject);
    procedure gridMessagesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure memoTextChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure gridMessagesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure cmdOKClick(Sender: TObject);
    procedure gridMessagesCanEditShow(Sender: TStringGridEditControlled;
      ACol, ARow: Integer; var CanShow: Boolean);
    procedure gridMessagesSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure cmdAddClick(Sender: TObject);
    procedure editIdentifierChange(Sender: TObject);
    procedure gridParamsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure cmdAddParamClick(Sender: TObject);
    procedure cmdDeleteParamClick(Sender: TObject);
    procedure cmdDeleteClick(Sender: TObject);
    procedure gridParamsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure FormDestroy(Sender: TObject);
    procedure memoCommentChange(Sender: TObject);
    procedure cmdSaveClick(Sender: TObject);
    procedure cmdExitClick(Sender: TObject);
    procedure cmdMoveToSectionClick(Sender: TObject);
    procedure cmdNewSectionClick(Sender: TObject);
    procedure cmdDeleteSectionClick(Sender: TObject);
    procedure cmdRenameSectionClick(Sender: TObject);
  private
    FChanging: Boolean;
    FStock: TStockMessages;
    procedure FillGrid;
    procedure FillParamsGrid;
    procedure EnableControls;
    procedure SelectObject(ActiveObject: TObject);
    procedure Save;
  end;

var
  frmStockMessageEditor: TfrmStockMessageEditor;

implementation

uses
  SourceRootPath,
  UfrmStockMessageEditorMoveToGroup;

{$R *.DFM}

{ TfrmMessageEditor }

function StockFilePath_Messages: string;
begin
  Result := CSourceRootPath+'\desktop\branding\'+StockFileName_Messages;
end;

function StockFilePath_DesktopPot: string;
begin
  Result := CSourceRootPath+'\desktop\branding\desktop.pot';
end;

function CMessageIdentifierConsts: string;
begin
  Result := CSourceRootPath+'\global\delphi\cust\MessageIdentifierConsts.pas';
end;

function CMessageDefaults: string;
begin
  Result := CSourceRootPath+'\global\delphi\cust\MessageDefaults.pas';
end;

procedure TfrmStockMessageEditor.FormCreate(Sender: TObject);
var
  ms: TMemoryStream;
  fs: TFileStream;
begin
  fs := TFileStream.Create(StockFilePath_Messages, fmOpenRead);  // I3321
  ms := TMemoryStream.Create;
  try
    ObjectTextToBinary(fs, ms);
    ms.Position := 0;
    FStock := TStockMessages.Create(ms);
  finally
    fs.Free;
    ms.Free;
  end;

  gridMessages.Cells[0, 0] := 'ID';
  gridMessages.Cells[1, 0] := 'Default string';
  gridParams.Cells[0, 0] := 'ID';
  gridParams.Cells[1, 0] := 'Description';

  FillGrid;
end;

procedure TfrmStockMessageEditor.FillGrid;
var
  i, j, n: Integer;
  ActiveObject: TObject;
begin
  if not Assigned(FStock) then Exit;

  ActiveObject := gridMessages.Objects[0, gridMessages.Row];

  n := 1;
  for i := 0 to FStock.Sections.Count - 1 do
    n := n + FStock.Sections[i].Strings.Count;

  gridMessages.RowCount := n + FStock.Sections.Count;

  n := 1;
  for i := 0 to FStock.Sections.Count - 1 do
  begin
    gridMessages.Cells[0, n] := FStock.Sections[i].Name;
    gridMessages.Cells[1, n] := '???';
    gridMessages.Objects[0, n] := FStock.Sections[i];
    Inc(n);
    for j := 0 to FStock.Sections[i].Strings.Count - 1 do
    begin
      gridMessages.Cells[0, n] := FStock.Sections[i].Strings[j].Name;
      gridMessages.Objects[0, n] := FStock.Sections[i].Strings[j];
      gridMessages.Cells[1, n] := FStock.Sections[i].Strings[j].DefStr;
      Inc(n);
    end;
  end;

  gridMessages.ColWidths[1] := gridMessages.ClientWidth - gridMessages.ColWidths[0] - 1;
  gridMessages.Col := 1;
  gridMessages.Row := 2;

  SelectObject(ActiveObject);
end;

procedure TfrmStockMessageEditor.SelectObject(ActiveObject: TObject);
var
  I: Integer;
begin
  if ActiveObject = nil then
    gridMessages.Row := 1
  else
    for I := 0 to gridMessages.RowCount - 1 do
      if gridMessages.Objects[0, I] = ActiveObject then
        gridMessages.Row := I;
  gridMessagesClick(gridMessages);
end;

procedure TfrmStockMessageEditor.gridMessagesClick(Sender: TObject);
begin
//
  FChanging := True;
  if gridMessages.Objects[0, gridMessages.Row] is TStockMessageString then
    with gridMessages.Objects[0, gridMessages.Row] as TStockMessageString do
    begin
      editIdentifier.Text := Name;
      memoText.Text := DefStr;
      FillParamsGrid;
      memoComment.Text := Comment;
    end
  else
  begin
    editIdentifier.Text := '';
    memoText.Text := '';
    gridParams.RowCount := 1;
    memoComment.Text := '';
  end;
  EnableControls;
  FChanging := False;
end;

procedure TfrmStockMessageEditor.EnableControls;
var
  e: Boolean;
begin
  e := gridMessages.Objects[0, gridMessages.Row] is TStockMessageString;

  editIdentifier.Enabled := e;
  memoText.Enabled := e;
  cmdAddParam.Enabled := e;
  gridParams.Enabled := e and (gridParams.RowCount > 1);
  cmdDeleteParam.Enabled := e and (gridParams.RowCount > 1);
  memoComment.Enabled := e;
  lblIdentifier.Enabled := e;
  lblParams.Enabled := e;
  lblComment.Enabled := e;
  lblText.Enabled := e;
  cmdMoveToSection.Enabled := e;

  e := gridMessages.Objects[0, gridMessages.Row] is TStockMessageSection;

  cmdDeleteSection.Enabled := e and ((gridMessages.Objects[0, gridMessages.Row] as TStockMessageSection).Strings.Count = 0);
  cmdRenameSection.Enabled := e;
end;

procedure TfrmStockMessageEditor.gridMessagesDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  with gridMessages.Canvas do
  begin
    if gridMessages.Objects[0, ARow] is TStockMessageSection then
    begin
      Font.Style := [fsBold];
      if ACol = 0 then Rect.Right := Rect.Right + 1;

      if gdSelected in State then begin Brush.Color := clHighlight; Font.Color := clHighlightText; end
      else begin Brush.Color := clBtnFace; Font.Color := clBtnText; end;
      TextRect(Rect, gridMessages.CellRect(0, ARow).Left + 1, Rect.Top + 1, gridMessages.Cells[0, ARow]);
    end
    else
    begin
      if (gdFixed in State) then begin Brush.Color := clBtnFace; Font.Color := clBtnText; end
      else if ACol = 0 then begin Brush.Color := clInfoBk; Font.Color := clInfoText; end
      else if gdSelected in State then begin Brush.Color := clHighlight; Font.Color := clHighlightText; end
      else begin Brush.Color := clWindow; Font.Color := clWindowText; end;
      Font.Style := [];
      FillRect(Rect);
      if (ACol = 1) and (gridMessages.Objects[0, ARow] is TStockMessageString) then
        with gridMessages.Objects[0, ARow] as TStockMessageString do
          TextOut(Rect.Left + 1, Rect.Top + 1, DefStr)
      else
        TextOut(Rect.Left + 1, Rect.Top + 1, gridMessages.Cells[ACol, ARow]);
        //StringReplace(gridMessages.Cells[ACol, ARow], #13#10, '<nl>', [rfReplaceAll]))
    end;
  end;
end;

procedure TfrmStockMessageEditor.memoTextChange(Sender: TObject);
begin
  if not (gridMessages.Objects[0, gridMessages.Row] is TStockMessageString) then Exit;
  if FChanging then Exit;
  FChanging := True;
  (gridMessages.Objects[0, gridMessages.Row] as TStockMessageString).DefStr := memoText.Text;
  gridMessages.Repaint;
  FChanging := False;
end;

procedure TfrmStockMessageEditor.FormResize(Sender: TObject);
begin
  gridMessages.ColWidths[1] := gridMessages.ClientWidth - gridMessages.ColWidths[0] - 1;
end;

procedure TfrmStockMessageEditor.gridMessagesSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  CanSelect := (ACol = 1);
end;

procedure TfrmStockMessageEditor.cmdOKClick(Sender: TObject);
begin
  Save;
  ModalResult := mrOk;
end;

procedure TfrmStockMessageEditor.Save;
var
  ms: TMemoryStream;
  fs: TFileStream;
begin
  fs := TFileStream.Create(StockFilePath_Messages, fmCreate);  // I3321
  ms := TMemoryStream.Create;
  try
    FStock.Save(ms);
    ms.Position := 0;
    ObjectBinaryToText(ms, fs);
  finally
    fs.Free;
    ms.Free;
  end;
  FStock.SaveToPas(CMessageIdentifierConsts, CMessageDefaults);  // I3321
  FStock.SaveToPoFile(StockFilePath_DesktopPot);
end;

procedure TfrmStockMessageEditor.cmdSaveClick(Sender: TObject);
begin
  Save;
end;

procedure TfrmStockMessageEditor.cmdRenameSectionClick(Sender: TObject);
var
  FName: string;
  FSection: TStockMessageSection;
begin
  if gridMessages.Objects[0, gridMessages.Row] is TStockMessageSection then
  begin
    FSection := gridMessages.Objects[0, gridMessages.Row] as TStockMessageSection;
    FName := FSection.Name;
    if InputQuery('Rename Section', 'Section Name', FName) then
    begin
      FSection.Name := FName;
      FillGrid;
      SelectObject(FSection);
    end;
  end;
end;

procedure TfrmStockMessageEditor.gridMessagesCanEditShow(Sender: TStringGridEditControlled; ACol, ARow: Integer; var CanShow: Boolean);
begin
  CanShow := (ACol = 1) and (gridMessages.Objects[0, ARow] is TStockMessageString);
end;

procedure TfrmStockMessageEditor.gridMessagesSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);  // I3310
begin
  if not (gridMessages.Objects[0, ARow] is TStockMessageString) then Exit;
  if FChanging then Exit;
  FChanging := True;
  //gridMessages.Cells[ACol, ARow] := Value;
  (gridMessages.Objects[0, ARow] as TStockMessageString).DefStr := Value;
  gridMessagesClick(gridMessages);
  FChanging := False;
end;

procedure TfrmStockMessageEditor.cmdAddClick(Sender: TObject);
var
  mid: string;
  sms: TStockMessageString;
  coll: TStockMessageStrings;
  i: Integer;
begin
  if gridMessages.Objects[0, gridMessages.Row] is TStockMessageString then
    with gridMessages.Objects[0, gridMessages.Row] as TStockMessageString do
      coll := Collection as TStockMessageStrings
  else if gridMessages.Objects[0, gridMessages.Row] is TStockMessageSection then
    with gridMessages.Objects[0, gridMessages.Row] as TStockMessageSection do
    coll := Strings
  else
    Exit;
  if not InputQuery('New Message', 'Message Identifier', mid) then Exit;
  sms := coll.Add;
  sms.Name := mid;
  FillGrid;
  for i := 1 to gridMessages.RowCount - 1 do
    if gridMessages.Objects[0, i] = sms then
    begin
      gridMessages.Col := 1;
      gridMessages.Row := i;
    end;
  memoText.SetFocus;
end;

procedure TfrmStockMessageEditor.editIdentifierChange(Sender: TObject);
begin
  if not (gridMessages.Objects[0, gridMessages.Row] is TStockMessageString) then Exit;
  if FChanging then Exit;
  FChanging := True;
  (gridMessages.Objects[0, gridMessages.Row] as TStockMessageString).Name := editIdentifier.Text;
  gridMessages.Cells[0, gridMessages.Row] := editIdentifier.Text;
  FChanging := False;
end;

procedure TfrmStockMessageEditor.gridParamsSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
begin
  if ACol = 0 then (gridParams.Objects[0, ARow] as TStockMessageParameter).Value := Value
  else (gridParams.Objects[0, ARow] as TStockMessageParameter).Description := Value;
end;

procedure TfrmStockMessageEditor.cmdAddParamClick(Sender: TObject);
var
  pid: string;
  param: TStockMessageParameter;
begin
  if not InputQuery('New Parameter', 'Parameter ID', pid) then Exit;
  with gridMessages.Objects[0, gridMessages.Row] as TStockMessageString do
    param := Parameters.Add;

  param.Value := pid;
  FillParamsGrid;
  gridParams.Row := gridParams.RowCount - 1;
  gridParams.Col := 1;
  EnableControls;
  gridParams.SetFocus;
end;

procedure TfrmStockMessageEditor.cmdMoveToSectionClick(Sender: TObject);
var
  I: Integer;
  ActiveMessage: TStockMessageString;
begin
  with TfrmStockMessageEditorMoveToGroup.Create(Self) do
  try
    ActiveMessage := gridMessages.Objects[0, gridMessages.Row] as TStockMessageString;
    for I := 0 to FStock.Sections.Count - 1 do
      lbSections.Items.AddObject(FStock.Sections[I].Name, FStock.Sections[I]);
    ActiveSection := (ActiveMessage.Collection as TStockMessageStrings).Section;
    if ShowModal = mrOk then
    begin
      ActiveMessage.Collection := ActiveSection.Strings; 
      FillGrid;
    end;
  finally
    Free;
  end;
end;

procedure TfrmStockMessageEditor.cmdNewSectionClick(Sender: TObject);
var
  FName: string;
  FSection: TStockMessageSection;
begin
  if InputQuery('New Section', 'Section Name', FName) then
  begin
    FSection := FStock.Sections.Add;
    FSection.Name := FName;
    FillGrid;
    SelectObject(FSection);
  end;
end;

procedure TfrmStockMessageEditor.FillParamsGrid;
var
  i: Integer;
begin
  if gridMessages.Objects[0, gridMessages.Row] is TStockMessageString then
    with gridMessages.Objects[0, gridMessages.Row] as TStockMessageString do
    begin
      gridParams.RowCount := Parameters.Count + 1;
      for i := 0 to Parameters.Count - 1 do
      begin
        gridParams.Objects[0, i+1] := Parameters[i];
        gridParams.Cells[0, i+1] := Parameters[i].Value;
        gridParams.Cells[1, i+1] := Parameters[i].Description;
      end;
      if gridParams.RowCount > 1 then gridParams.FixedRows := 1;
    end;
end;

procedure TfrmStockMessageEditor.cmdDeleteParamClick(Sender: TObject);
begin
  if gridMessages.Objects[0, gridMessages.Row] is TStockMessageString then
    with gridMessages.Objects[0, gridMessages.Row] as TStockMessageString do
    begin
      gridParams.Objects[0, gridParams.Row].Free;
      FillParamsGrid;
      EnableControls;
    end;
end;

procedure TfrmStockMessageEditor.cmdDeleteSectionClick(Sender: TObject);
begin
  if gridMessages.Objects[0, gridMessages.Row] is TStockMessageSection then
  begin
    gridMessages.Objects[0, gridMessages.Row].Free;
    gridMessages.Objects[0, gridMessages.Row] := nil;
    FillGrid;
  end;
end;

procedure TfrmStockMessageEditor.cmdDeleteClick(Sender: TObject);
begin
  if gridMessages.Objects[0, gridMessages.Row] is TStockMessageString then
    with gridMessages.Objects[0, gridMessages.Row] as TStockMessageString do
      if MessageDlg('Delete stock message '+Name+'?', mtConfirmation, mbOKCancel, 0) = mrOk then
        Free;
  FillGrid;
end;

procedure TfrmStockMessageEditor.gridParamsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  with gridParams.Canvas do
  begin
    if (ARow = 0) then begin Brush.Color := clBtnFace; Font.Color := clBtnText; end
    else if ACol = 0 then begin Brush.Color := clInfoBk; Font.Color := clInfoText; end
    else if gdSelected in State then begin Brush.Color := clHighlight; Font.Color := clHighlightText; end
    else begin Brush.Color := clWindow; Font.Color := clWindowText; end;
    Font.Style := [];
    FillRect(Rect);
    TextOut(Rect.Left + 1, Rect.Top + 1, gridParams.Cells[ACol, ARow]);
  end;
end;

procedure TfrmStockMessageEditor.FormDestroy(Sender: TObject);
begin
  FStock.Free;
end;

procedure TfrmStockMessageEditor.memoCommentChange(Sender: TObject);
begin
  if not (gridMessages.Objects[0, gridMessages.Row] is TStockMessageString) then Exit;
  if FChanging then Exit;
  FChanging := True;
  (gridMessages.Objects[0, gridMessages.Row] as TStockMessageString).Comment := memoComment.Text;
  FChanging := False;
end;

procedure TfrmStockMessageEditor.cmdExitClick(Sender: TObject);
begin
  Close;
end;

end.
