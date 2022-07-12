(*
  Name:             UfrmSelectSystemKeyboard
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    1 Dec 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Fill a combo box list of system keyboards
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    01 Dec 2012 - mcdurdin - I3613 - V9.0 - System shadow keyboards obsolete, strip out remaining code
*)
unit UfrmSelectSystemKeyboard;  // I3306

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, ExtCtrls, UfrmTike;

type
  TSystemKeyboardItem = class
    LayoutText: string;
    LayoutFile: string;
    KeyboardID: string;
    Installed: Boolean;
  end;

  TfrmSelectSystemKeyboard = class(TTIKEForm)
    cmdOK: TButton;
    gridKeyboards: TStringGrid;
    Label1: TLabel;
    cmdCancel: TButton;
    tmrResetLookup: TTimer;
    lblKeyboardNotInstalled: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure gridKeyboardsKeyPress(Sender: TObject; var Key: Char);
    procedure tmrResetLookupTimer(Sender: TObject);
    procedure gridKeyboardsClick(Sender: TObject);
    procedure gridKeyboardsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure gridKeyboardsDblClick(Sender: TObject);
  private
    FLookupText: string;
    function GetSystemKeyboardName: string;
    procedure SetSystemKeyboardName(Value: string);
    procedure InvalidateCell(ACol, ARow: Integer);
  protected
    function GetHelpTopic: string; override;
  public
    { Public declarations }
    property SystemKeyboardName: string read GetSystemKeyboardName write SetSystemKeyboardName;
  end;

function SelectSystemKeyboard(FOwner: TComponent; var FKeyboardName: string): Boolean;

function LoadSystemKeyboard(var FLoadedSystemKeyboard: Boolean; FKeyboardName: string): HKL;

procedure ClearSystemKeyboardList(keyboardlist: TStrings);
procedure FillSystemKeyboardList(keyboardlist: TStrings);
function GetSystemKeyboardIndex(keyboardlist: TStrings; id: string): Integer;

implementation

uses
  Keyman.Developer.System.HelpTopics,

  ErrorControlledRegistry, 
  RegistryKeys,
  utilstr;
  
{$R *.DFM}

function SelectSystemKeyboard(FOwner: TComponent; var FKeyboardName: string): Boolean;
begin
  Result := False;

  with TfrmSelectSystemKeyboard.Create(FOwner) do
  try
    SystemKeyboardName := FKeyboardName;
    if ShowModal = mrOk then
    begin
      if SystemKeyboardName = FKeyboardName then Exit;

      FKeyboardName := SystemKeyboardName;

      Result := True;
    end;
  finally
    Free;
  end;
end;


procedure TfrmSelectSystemKeyboard.FormCreate(Sender: TObject);
var
  str: TStringList;
  i: Integer;
  syspath, s: string;
  buf: array[0..260] of Char;
begin
  inherited;
  gridKeyboards.DefaultRowHeight := 16 * PixelsPerInch div 96;
  GetSystemDirectory(buf, 260);
  syspath := IncludeTrailingPathDelimiter(buf);
  str := TStringList.Create;
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeyboardLayouts_LM) then
    begin
      GetKeyNames(str);

      for i := str.Count - 1 downto 0 do
        if OpenKeyReadOnly('\'+SRegKey_KeyboardLayouts_LM + '\' + str[i]) then
          if not ValueExists('Layout Text') then   // I3613
            str.Delete(i)
          else
          begin
            s := ReadString('Layout File');
            if not FileExists(syspath+s)
              then str[i] := ReadString('Layout Text') + '|' + s + '|' + str[i] + '|0'
              else str[i] := ReadString('Layout Text') + '|' + s + '|' + str[i] + '|1';
          end;

      str.Sort;
      gridKeyboards.RowCount := str.Count + 1;
      for i := 0 to str.Count - 1 do
      begin
        s := str[i];
        gridKeyboards.Cells[0, i+1] := StrToken(s, '|');
        gridKeyboards.Cells[1, i+1] := StrToken(s, '|');
        gridKeyboards.Cells[2, i+1] := StrToken(s, '|');
        gridKeyboards.Cells[3, i+1] := StrToken(s, '|');
      end;
    end;

    gridKeyboards.Cells[0, 0] := 'Name';
    gridKeyboards.Cells[1, 0] := 'Layout';
    gridKeyboards.Cells[2, 0] := 'ID';
    gridKeyboards.ColWidths[3] := 0;
    gridKeyboards.ColWidths[1] := 100 * PixelsPerInch div 96;
    gridKeyboards.ColWidths[2] := 80 * PixelsPerInch div 96;
    gridKeyboards.ColWidths[0] := gridKeyboards.ClientWidth - (100  * PixelsPerInch div 96) -
      (80 * PixelsPerInch div 96) - 2;
  finally
    str.Free;
    Free;
  end;
end;

function TfrmSelectSystemKeyboard.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_SelectSystemKeyboard;
end;

function TfrmSelectSystemKeyboard.GetSystemKeyboardName: string;
begin
  Result := gridKeyboards.Cells[2, gridKeyboards.Row];
end;

procedure TfrmSelectSystemKeyboard.SetSystemKeyboardName(Value: string);
var
  i: Integer;
begin
  for i := 1 to gridKeyboards.RowCount - 1 do
    if gridKeyboards.Cells[2, i] = Value then
    begin
      gridKeyboards.Row := i;
      Exit;
    end;
end;

procedure TfrmSelectSystemKeyboard.gridKeyboardsKeyPress(Sender: TObject; var Key: Char);
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

procedure TfrmSelectSystemKeyboard.tmrResetLookupTimer(Sender: TObject);
begin
  FLookupText := '';
  tmrResetLookup.Enabled := False;
  InvalidateCell(0, gridKeyboards.Row);
  gridKeyboards.Update;
end;

procedure TfrmSelectSystemKeyboard.gridKeyboardsClick(Sender: TObject);
begin
  cmdOK.Enabled := gridKeyboards.Cells[3, gridKeyboards.Row] = '1';
  lblKeyboardNotInstalled.Visible := not cmdOK.Enabled;
end;

procedure TfrmSelectSystemKeyboard.gridKeyboardsDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  w: Integer;
begin
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

procedure TfrmSelectSystemKeyboard.InvalidateCell(ACol, ARow: Integer);
var
  r: TRect;
begin
  r := gridKeyboards.CellRect(ACol, ARow);
  InvalidateRect(gridKeyboards.Handle, @r, False);
end;

procedure TfrmSelectSystemKeyboard.gridKeyboardsDblClick(Sender: TObject);
begin
  if cmdOK.Enabled then ModalResult := mrOk;
end;

procedure ClearSystemKeyboardList(keyboardlist: TStrings);
var
  i: Integer;
begin
  for i := 0 to keyboardlist.Count - 1 do
    keyboardlist.Objects[i].Free;
  keyboardlist.Clear;
end;

procedure FillSystemKeyboardList(keyboardlist: TStrings);
var
  buf: array[0..260] of char;
  syspath: string;
  str: TStringList;
  i: Integer;
  k: TSystemKeyboardItem;
begin
  keyboardlist.BeginUpdate;
  ClearSystemKeyboardList(keyboardlist);

  GetSystemDirectory(buf, 260);
  syspath := IncludeTrailingPathDelimiter(buf);

  str := TStringList.Create;
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeyboardLayouts_LM) then
    begin
      GetKeyNames(str);

      for i := str.Count - 1 downto 0 do
        if OpenKeyReadOnly('\'+SRegKey_KeyboardLayouts_LM + '\' + str[i]) then
          if not ValueExists('Layout Text') then   // I3613
            str.Delete(i)
          else
          begin
            k := TSystemKeyboardItem.Create;
            k.LayoutText := ReadString('Layout Text');
            k.LayoutFile := ReadString('Layout File');
            k.Installed := FileExists(syspath+k.LayoutFile);
            k.KeyboardID := str[i];
            keyboardlist.AddObject(k.LayoutText + ' ('+k.LayoutFile+')', k);
          end;

      str.Assign(keyboardlist);
      str.Sort;
      keyboardlist.Assign(str);
    end;

  finally
    str.Free;
    Free;
  end;
  keyboardlist.EndUpdate;
end;

function LoadSystemKeyboard(var FLoadedSystemKeyboard: Boolean; FKeyboardName: string): HKL;
type
  PHKL = ^HKL;
var
  h: HKL;
  hq, hp: PHKL;
  i, n: Integer;
begin
  { Load the layout (if not already loaded) and determine if it should be unloaded again }
  n := GetKeyboardLayoutList(0, hp);
  hp := PHKL(AllocMem(n * sizeof(HKL)));
  try
    GetKeyboardLayoutList(n, hp^);
    h := LoadKeyboardLayout(PChar(FKeyboardName), 0);
    hq := hp;
    FLoadedSystemKeyboard := True;
    for i := 0 to n - 1 do
      if hq^ = h
        then begin FLoadedSystemKeyboard := False; Break; end
        else Inc(hq);
  finally
    FreeMem(hp);
  end;
  Result := h;
end;

function GetSystemKeyboardIndex(keyboardlist: TStrings; id: string): Integer;
var
  i: Integer;
begin
  for i := 0 to keyboardlist.Count - 1 do
    if (keyboardlist.Objects[i] is TSystemKeyboardItem) and (TSystemKeyboardItem(keyboardlist.Objects[i]).KeyboardID = id) then
    begin
      Result := i;                
      Exit;
    end;
  Result := -1;
end;

end.

