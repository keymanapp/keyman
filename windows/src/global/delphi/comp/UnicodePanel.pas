(*
  Name:             UnicodePanel
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      19 Nov 2007

  Modified Date:    19 Nov 2007
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          19 Nov 2007 - mcdurdin - I1157 - const string parameters
*)
unit UnicodePanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TUnicodePanel = class(TPanel)
  private
    FWideCaption: WideString;
    FGlyphIndex: Boolean;
    function GetCaption: string;
    procedure SetCaption(Value: string);
    procedure SetWideCaption(Value: WideString);
    procedure SetGlyphIndex(const Value: Boolean);
  protected
  public
    procedure Paint; override;
    property WideCaption: WideString read FWideCaption write SetWideCaption;
    property GlyphIndex: Boolean read FGlyphIndex write SetGlyphIndex;
  published
    property Caption: string read GetCaption write SetCaption;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Keyman', [TUnicodePanel]);
end;

{ TUnicodePanel }

function TUnicodePanel.GetCaption: string;
begin
  Result := FWideCaption;
end;

procedure TUnicodePanel.Paint;
const
  Alignments: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  Rect: TRect;
  TopColor, BottomColor: TColor;
  FontHeight: Integer;
  Flags: Longint;
  buf: WideChar;
  sz: TSize;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  Rect := GetClientRect;
  if BevelOuter <> bvNone then
  begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  Frame3D(Canvas, Rect, Color, Color, BorderWidth);
  if BevelInner <> bvNone then
  begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  with Canvas do
  begin
    Brush.Color := Color;
    FillRect(Rect);
    Brush.Style := bsClear;
    Font := Self.Font;
    FontHeight := TextHeight('W');
    with Rect do
    begin
      Top := ((Bottom + Top) - FontHeight) div 2;
      Bottom := Top + FontHeight;
    end;

    if FGlyphIndex
      then Flags := ETO_GLYPH_INDEX or ETO_CLIPPED
      else Flags := ETO_CLIPPED;

    buf := ' ';
    GetTextExtentPoint32W(Handle, @buf, 1, sz);
    SetTextAlign(Handle, TA_CENTER or VTA_CENTER);
    ExtTextOutW(Handle, (Rect.Right + Rect.Left) div 2, (Rect.Bottom + Rect.Top - sz.cy) div 2,
      Flags, @Rect, PWideChar(FWideCaption), Length(FWideCaption), nil);
    SetTextAlign(Handle, TA_LEFT or TA_BASELINE);
  end;
end;

procedure TUnicodePanel.SetCaption(Value: string);
begin
  FWideCaption := Value;
  Invalidate;
end;

procedure TUnicodePanel.SetGlyphIndex(const Value: Boolean);
begin
  FGlyphIndex := Value;
  Invalidate;
end;

procedure TUnicodePanel.SetWideCaption(Value: WideString);
begin
  FWideCaption := Value;
  Invalidate;
end;

end.
