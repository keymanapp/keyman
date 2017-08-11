(*
  Name:             UfrmUnderlyingLayout
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    3 May 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
*)
unit UfrmUnderlyingLayout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, ExtCtrls, UfrmKMShell;

type
  TfrmUnderlyingLayout = class(TfrmKMShell)
    cmdOK: TButton;
    gridKeyboards: TStringGrid;
    cmdCancel: TButton;
    lblKeyboardNotInstalled: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure gridKeyboardsKeyPress(Sender: TObject; var Key: Char);
    procedure gridKeyboardsClick(Sender: TObject);
    procedure gridKeyboardsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure gridKeyboardsDblClick(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FLookupText: string;
    tmrResetLookup: TTimer;
    procedure tmrResetLookupTimer(Sender: TObject);
    procedure InvalidateCell(ACol, ARow: Integer);
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  ErrorControlledRegistry, 
  RegistryKeys,
  Utilkmshell;

{$R *.DFM}

procedure TfrmUnderlyingLayout.FormCreate(Sender: TObject);
var
  str: TStringList;
  i, n: Integer;
  FSelected, syspath, s: string;
  buf: array[0..260] of Char;
begin
  tmrResetLookup := TTimer.Create(nil);
  tmrResetLookup.Interval := 500;
  tmrResetLookup.Enabled := False;
  tmrResetLookup.OnTimer := tmrResetLookupTimer;

  if not Assigned(gridKeyboards) then Exit;

  GetSystemDirectory(buf, 260);
  syspath := AppendSlash(buf);
  str := TStringList.Create;
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKeyReadOnly(SRegKey_KeymanEngine) then
    begin
      if ValueExists(SRegValue_UnderlyingLayout) then
        FSelected := ReadString(SRegValue_UnderlyingLayout);
      CloseKey;
    end
    else
      FSelected := '';

    RootKey := HKEY_LOCAL_MACHINE;

    n := 0;

    if OpenKeyReadOnly(SRegKey_KeyboardLayouts) then
    begin
      GetKeyNames(str);

      for i := str.Count - 1 downto 0 do
        if OpenKeyReadOnly('\'+SRegKey_KeyboardLayouts + '\' + str[i]) then
          if ValueExists(SRegValue_KeyboardKeymanInstall) or not ValueExists('Layout Text') then
            str.Delete(i)
          else
          begin
            s := ReadString('Layout File');
            if not FileExists(syspath+s)
              then str[i] := ReadString('Layout Text') + '|'+s+'|' + str[i] + '|0'
              else str[i] := ReadString('Layout Text') + '|'+s+'|' + str[i] + '|1';
          end;

      str.Sort;

      gridKeyboards.RowCount := str.Count + 1;

      gridKeyboards.Cells[0,0] := 'Default layout';
      gridKeyboards.Cells[3,0] := '1';
      for i := 0 to str.Count - 1 do
      begin
        s := str[i];
        gridKeyboards.Cells[0, i+1] := StrToken(s, '|');
        gridKeyboards.Cells[1, i+1] := StrToken(s, '|');
        gridKeyboards.Cells[2, i+1] := StrToken(s, '|');
        gridKeyboards.Cells[3, i+1] := StrToken(s, '|');
        if FSelected = gridKeyboards.Cells[2, i+1] then n := i+1;
      end;
    end;

    gridKeyboards.DefaultRowHeight := 16 * PixelsPerInch div 96;
    gridKeyboards.ColWidths[3] := -1;
    gridKeyboards.ColWidths[1] := -1;
    gridKeyboards.ColWidths[2] := -1;
    gridKeyboards.ColWidths[0] := gridKeyboards.ClientWidth;

    gridKeyboards.Row := n;
  finally
    str.Free;
    Free;
  end;
end;

procedure TfrmUnderlyingLayout.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(tmrResetLookup);
end;

procedure TfrmUnderlyingLayout.gridKeyboardsKeyPress(Sender: TObject; var Key: Char);
var
  i, n: Integer;
begin
  tmrResetLookup.Enabled := False;
  if Key = #8 then
  begin
    if FLookupText <> '' then Delete(FLookupText, 1, 1);
    n := Length(FLookupText);
    for i := 1 to gridKeyboards.RowCount - 1 do
      if Copy(gridKeyboards.Cells[0, i], 1, n) = FLookupText then
      begin
        gridKeyboards.Row := i;
        tmrResetLookup.Enabled := True;
        Exit;
      end;
  end
  else
  begin
    n := Length(FLookupText) + 1;
    for i := 1 to gridKeyboards.RowCount - 1 do
      if UpperCase(Copy(gridKeyboards.Cells[0, i], 1, n)) = FLookupText + UpCase(Key) then
      begin
        FLookupText := FLookupText + UpCase(Key);
        gridKeyboards.Row := i;
        InvalidateCell(0, gridKeyboards.Row);
        gridKeyboards.Update;
        tmrResetLookup.Enabled := True;
        Exit;
      end;
  end;
  tmrResetLookup.Enabled := True;
end;

procedure TfrmUnderlyingLayout.tmrResetLookupTimer(Sender: TObject);
begin
  FLookupText := '';
  tmrResetLookup.Enabled := False;
  if not Assigned(gridKeyboards) then Exit;
  InvalidateCell(0, gridKeyboards.Row);
  gridKeyboards.Update;
end;

procedure TfrmUnderlyingLayout.gridKeyboardsClick(Sender: TObject);
var
  e: Boolean;
begin
  if Assigned(gridKeyboards) then e := gridKeyboards.Cells[3, gridKeyboards.Row] = '1' else e := False;
  if Assigned(cmdOK) then cmdOK.Enabled := e;
  if Assigned(lblKeyboardNotInstalled) then lblKeyboardNotInstalled.Visible := not e;
end;

procedure TfrmUnderlyingLayout.gridKeyboardsDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  w: Integer;
begin
  if not Assigned(gridKeyboards) then Exit;
  with gridKeyboards.Canvas do
  begin
    Font := gridKeyboards.Font;
    if gdFixed in State then
    begin
      Brush.Color := clBtnFace;
      Font.Color := clBtnText;
    end
    else if gdSelected in State then
    begin
      Brush.Color := clHighlight;
      Font.Color := clHighlightText;
    end
    else
    begin
      Brush.Color := clWindow;
      Font.Color := clWindowText;
    end;

    if gridKeyboards.Cells[3, ARow] = '0' then Font.Color := clGrayText;

    Brush.Style := bsSolid;
    FillRect(Rect);
    TextRect(Rect, Rect.Left+2, Rect.Top+2, gridKeyboards.Cells[ACol, ARow]);
    if (FLookupText <> '') and (gdSelected in State) and (ACol = 0) then
    begin
      w := TextWidth(Copy(gridKeyboards.Cells[ACol, ARow], 1, Length(FLookupText)));
      Pen.Color := clBlack;
      Pen.Width := 2;
      MoveTo(w+2, Rect.Top);
      LineTo(w+2, Rect.Bottom-1);
    end;
  end;
end;

procedure TfrmUnderlyingLayout.InvalidateCell(ACol, ARow: Integer);
var
  r: TRect;
begin
  if not Assigned(gridKeyboards) then Exit;
  r := gridKeyboards.CellRect(ACol, ARow);
  InvalidateRect(gridKeyboards.Handle, @r, False);
end;

procedure TfrmUnderlyingLayout.gridKeyboardsDblClick(Sender: TObject);
begin
  if not Assigned(cmdOK) then Exit;
  if cmdOK.Enabled then cmdOKClick(cmdOK);
end;

procedure TfrmUnderlyingLayout.cmdOKClick(Sender: TObject);
begin
  if not Assigned(gridKeyboards) then Exit;
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey(SRegKey_KeymanEngine, True) then
    begin
      if gridKeyboards.Row > 0 then
        WriteString(SRegValue_UnderlyingLayout, gridKeyboards.Cells[2, gridKeyboards.Row])
      else if ValueExists(SRegValue_UnderlyingLayout) then DeleteValue(SRegValue_UnderlyingLayout);
    end;
  finally
    Free;
  end;

  ModalResult := mrOk;
end;

end.

