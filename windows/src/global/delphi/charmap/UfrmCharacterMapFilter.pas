(*
  Name:             UfrmCharacterMapFilter
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Initial version
                    14 Sep 2006 - mcdurdin - Moved to new location
                    06 Oct 2006 - mcdurdin - Add Characters in Current Font Only function
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit UfrmCharacterMapFilter;  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmCharacterMapFilter = class(TForm)
    lblRangeTo: TLabel;
    editFilter: TEdit;
    cmdStar: TButton;
    cmdQuestionMark: TButton;
    cmdRange: TButton;
    lblStar: TLabel;
    lblQuestionMark: TLabel;
    lblRange: TLabel;
    cmdSet: TButton;
    lblSet: TLabel;
    cmdOK: TButton;
    cmdCancel: TButton;
    cmdDollar: TButton;
    lblDollar: TLabel;
    cmdBlock: TButton;
    lblBlock: TLabel;
    editRangeStart: TEdit;
    editRangeStop: TEdit;
    rbRange: TRadioButton;
    rbName: TRadioButton;
    lblRangeFrom: TLabel;
    cmdCharactersInCurrentFontOnly: TButton;
    lblCharactersInCurrentFontOnly: TLabel;
    procedure cmdInsertClick(Sender: TObject);
    procedure cmdBlockClick(Sender: TObject);
    procedure rbRangeClick(Sender: TObject);
    procedure rbNameClick(Sender: TObject);
    procedure editRangeStartChange(Sender: TObject);
    procedure editRangeStopChange(Sender: TObject);
    procedure editRangeStopKeyPress(Sender: TObject; var Key: Char);
    procedure cmdCharactersInCurrentFontOnlyClick(Sender: TObject);
    procedure editFilterChange(Sender: TObject);
  private
    FCurrentFontChar: WideString;
    function GetFilter: WideString;
    procedure SetFilter(Value: WideString);
    procedure SetFilterFromRange;
    procedure EnableControls;
    { Private declarations }
  public
    { Public declarations }
    property Filter: WideString read GetFilter write SetFilter;
  end;

implementation

uses
  UnicodeData;

{$R *.dfm}

procedure TfrmCharacterMapFilter.cmdBlockClick(Sender: TObject);
begin
  if Copy(editFilter.Text, 1, Length(FCurrentFontChar)+1) = FCurrentFontChar+'<'
    then editFilter.Text := FCurrentFontChar + Copy(editFilter.Text, Length(FCurrentFontChar)+1, Length(editFilter.Text))
    else editFilter.Text := FCurrentFontChar + '<' + Copy(editFilter.Text, Length(FCurrentFontChar)+1, Length(editFilter.Text));
  if editFilter.Text = '<' then
    editFilter.SelStart := 1;
  editFilter.SetFocus;
end;

procedure TfrmCharacterMapFilter.cmdCharactersInCurrentFontOnlyClick(
  Sender: TObject);
begin
  if Copy(editFilter.Text, 1, 1) = '>'
    then editFilter.Text := Copy(editFilter.Text, 2, Length(editFilter.Text))
    else editFilter.Text := '>' + editFilter.Text;
  if editFilter.Text = '>' then
    editFilter.SelStart := 1;
  if editFilter.Enabled
    then editFilter.SetFocus
    else editRangeStart.SetFocus;
end;

procedure TfrmCharacterMapFilter.cmdInsertClick(Sender: TObject);
begin
  editFilter.SelText := (Sender as TButton).Caption;
  editFilter.SetFocus;
end;

procedure TfrmCharacterMapFilter.editFilterChange(Sender: TObject);
begin
  if Copy(editFilter.Text, 1, 1) = '>'
    then FCurrentFontChar := '>'
    else FCurrentFontChar := '';
end;

procedure TfrmCharacterMapFilter.editRangeStartChange(Sender: TObject);
begin
  SetFilterFromRange;
end;

procedure TfrmCharacterMapFilter.editRangeStopChange(Sender: TObject);
begin
  SetFilterFromRange;
end;

procedure TfrmCharacterMapFilter.editRangeStopKeyPress(Sender: TObject;
  var Key: Char);
begin
  if CharInSet(Key, ['a'..'f']) then
    Key := UpCase(Key)
  else if not CharInSet(Key, ['A'..'F','0'..'9', #8]) then
    Key := #0;
end;

procedure TfrmCharacterMapFilter.EnableControls;
var
  e: Boolean;
begin
  e := rbRange.Checked;
  editRangeStart.Enabled := e;
  editRangeStop.Enabled := e;
  lblRangeTo.Enabled := e;
  lblRangeFrom.Enabled := e;

  editFilter.Enabled := not e;
  cmdBlock.Enabled := not e;
  lblBlock.Enabled := not e;
  cmdStar.Enabled := not e;
  lblStar.Enabled := not e;
  cmdQuestionMark.Enabled := not e;
  lblQuestionMark.Enabled := not e;
  cmdRange.Enabled := not e;
  lblRange.Enabled := not e;
  cmdSet.Enabled := not e;
  lblSet.Enabled := not e;
  cmdDollar.Enabled := not e;
  lblDollar.Enabled := not e;
end;

function TfrmCharacterMapFilter.GetFilter: WideString;
begin
  Result := editFilter.Text;
end;

procedure TfrmCharacterMapFilter.SetFilter(Value: WideString);
var
  RangeStart, RangeStop: Integer;
begin
  if Copy(Value, 1, 1) = '>' then
  begin
    FCurrentFontChar := '>';
    Delete(Value, 1, 1);
  end
  else
    FCurrentFontChar := '';

  if GetUnicodeRangeFromFilter(Value, RangeStart, RangeStop) then
  begin
    rbRange.Checked := True;
    editRangeStart.Text := IntToHex(RangeStart, 4);
    if RangeStop > 0 then
      editRangeStop.Text := IntToHex(RangeStop, 4);
    SetFilterFromRange;
    ActiveControl := editRangeStart;
  end
  else
  begin
    editFilter.Text := FCurrentFontChar+Value;
    rbName.Checked := True;
    ActiveControl := editFilter;
  end;
  EnableControls;
end;

procedure TfrmCharacterMapFilter.SetFilterFromRange;
begin
  if editRangeStart.Text = '' then
    editFilter.Text := FCurrentFontChar
  else
  begin
    editFilter.Text := FCurrentFontChar + 'U+'+editRangeStart.Text;
    if editRangeStop.Text <> '' then
      editFilter.Text := editFilter.Text + ' - U+'+editRangeStop.Text;
  end;
end;

procedure TfrmCharacterMapFilter.rbNameClick(Sender: TObject);
begin
  EnableControls;
  if Visible then editFilter.SetFocus;
end;

procedure TfrmCharacterMapFilter.rbRangeClick(Sender: TObject);
begin
  EnableControls;
  if Visible then editRangeStart.SetFocus;
end;

end.
