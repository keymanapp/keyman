unit CheckboxGridHelper;

interface

uses
  System.Types,
  Vcl.Grids;

type
  TCheckboxGridHelper = class
  private
    FGrid: TStringGrid;
    function GetChecked(ACol, ARow: Integer): Boolean;
    procedure SetChecked(ACol, ARow: Integer; const Value: Boolean);
  public
    constructor Create(AGrid: TStringGrid);
    procedure DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
    procedure Click(ACol: Integer = -1; ARow: Integer = -1);
    function IsChecked(ACol: Integer = -1; ARow: Integer = -1): Boolean;
    property Checked[ACol, ARow: Integer]: Boolean read GetChecked write SetChecked;
  end;

implementation

uses
  System.Classes,
  System.Math,
  System.SysUtils,
  Winapi.UxTheme,
  Winapi.Windows,
  VCL.Themes;

procedure TCheckboxGridHelper.Click(ACol, ARow: Integer);
begin
  if ACol = -1 then
    ACol := FGrid.Col;
  if ARow = -1 then
    ARow := FGrid.Row;
  Checked[ACol, ARow] := not IsChecked(ACol, ARow);
end;

constructor TCheckboxGridHelper.Create(AGrid: TStringGrid);
begin
  inherited Create;
  FGrid := AGrid;
end;

type
  TStringGridAccess = class(TStringGrid);

procedure TCheckboxGridHelper.DrawCell(
  ACol, ARow: Integer;
  ARect: TRect; AState: TGridDrawState);
const
  PADDING = 4;
var
  h: HTHEME;
  s: TSize;
  r: TRect;
const
  CCellNormal: array[TGridDrawingStyle] of TThemedGrid =
    (tgClassicCellNormal, tgCellNormal, tgGradientCellNormal);
  CCellSelected: array[TGridDrawingStyle] of TThemedGrid =
    (tgClassicCellSelected, tgCellSelected, tgGradientCellSelected);
  CFixedNormal: array[TGridDrawingStyle] of TThemedGrid =
    (tgClassicFixedCellNormal, tgFixedCellNormal, tgGradientFixedCellNormal);
  CFixedHot: array[TGridDrawingStyle] of TThemedGrid =
    (tgClassicFixedCellHot, tgFixedCellHot, tgGradientFixedCellHot);
  CFixedPressed: array[TGridDrawingStyle] of TThemedGrid =
    (tgClassicFixedCellPressed, tgFixedCellPressed, tgGradientFixedCellPressed);
begin
  if (gdSelected in AState) and
     (not (gdFocused in AState) or
     ([goDrawFocusSelected, goRowSelect] * FGrid.Options <> [])) then
    TStringGridAccess(FGrid).DrawCellHighlight(ARect, AState, ACol, ARow)
  else
    TStringGridAccess(FGrid).DrawCellBackground(ARect, FGrid.Color, AState, ACol, ARow);

  if StyleServices.Enabled then
  begin
    s.cx := GetSystemMetrics(SM_CXMENUCHECK);
    s.cy := GetSystemMetrics(SM_CYMENUCHECK);
    if UseThemes then
    begin
      h := OpenThemeData(FGrid.Handle, 'BUTTON');
      if h <> 0 then
        try
          GetThemePartSize(h,
            FGrid.Canvas.Handle,
            BP_CHECKBOX,
            CBS_CHECKEDNORMAL,
            nil,
            TS_DRAW,
            s);
          r.Top := ARect.Top + (ARect.Bottom - ARect.Top - s.cy) div 2;
          r.Bottom := r.Top + s.cy;
          r.Left := ARect.Left + PADDING;
          r.Right := r.Left + s.cx;
          DrawThemeBackground(h,
            FGrid.Canvas.Handle,
            BP_CHECKBOX,
            IfThen(IsChecked(ACol, ARow), CBS_CHECKEDNORMAL, CBS_UNCHECKEDNORMAL),
            r,
            nil);
        finally
          CloseThemeData(h);
        end;
    end;
  end
  else
  begin
    r.Top := ARect.Top + (ARect.Bottom - ARect.Top - s.cy) div 2;
    r.Bottom := r.Top + s.cy;
    r.Left := ARect.Left + PADDING;
    r.Right := r.Left + s.cx;
    DrawFrameControl(FGrid.Canvas.Handle,
      r,
      DFC_BUTTON,
      IfThen(IsChecked(ACol, ARow), DFCS_CHECKED, DFCS_BUTTONCHECK));
  end;
end;

function TCheckboxGridHelper.GetChecked(ACol, ARow: Integer): Boolean;
begin
  Result := IsChecked(ACol, ARow);
end;

function TCheckboxGridHelper.IsChecked(ACol, ARow: Integer): Boolean;
begin
  if ACol = -1 then
  begin
    ACol := FGrid.Col;
    ARow := FGrid.Row;
  end;

  Result := FGrid.Cells[ACol, ARow] = '1';
end;

procedure TCheckboxGridHelper.SetChecked(ACol, ARow: Integer;
  const Value: Boolean);
begin
  if Value
    then FGrid.Cells[ACol, ARow] := '1'
    else FGrid.Cells[ACol, ARow] := '0';
end;

end.
