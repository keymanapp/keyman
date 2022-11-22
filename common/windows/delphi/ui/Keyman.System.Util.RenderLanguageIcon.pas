unit Keyman.System.Util.RenderLanguageIcon;

interface

uses
  System.Types,
  System.UITypes,
  Vcl.Graphics;

{
  Renders a two or three character language icon into the canvas

  Parameters:
    x, y: top left of icon graphic on canvas
    code: the language code to render
    size: 0=default size (-10), MaxInt=scale to fit, otherwise font.size as given
}
procedure DrawLanguageIcon(ACanvas: TCanvas; x, y: Integer; code: string; size: integer = 0);   // I3933

implementation

procedure DrawLanguageIcon(ACanvas: TCanvas; x, y: Integer; code: string; size: integer);   // I3933
var
  FIconRect: TRect;
  sz: TSize;
  FBrushColor: TColor;
  FFontColor: TColor;
  FFontName: string;
  FFontSize: Integer;
begin
  FIconRect := Rect(x, y, x+16, y+16);

  FBrushColor := ACanvas.Brush.Color;
  FFontColor := ACanvas.Font.Color;
  FFontName := ACanvas.Font.Name;
  FFontSize := ACanvas.Font.Size;

  if Size = 0 then
    Size := -10;

  ACanvas.Brush.Color := $C0C2C6;
  ACanvas.FillRect(Rect(x, y, x+16, y+16));
  ACanvas.Font.Color := $404143;
  ACanvas.Font.Name := 'Tahoma';

  if Size = MaxInt then
  begin
    sz.cx := MaxInt;
    Size := -12;
    while (sz.cx > 16) and (Size < -4) do
    begin
      ACanvas.Font.Size := Size;
      sz := ACanvas.TextExtent(code);
      Inc(Size);
    end;
  end
  else
  begin
    ACanvas.Font.Size := Size;
  end;

  sz := ACanvas.TextExtent(code);

  ACanvas.TextRect(FIconRect, FIconRect.Left + 8 - sz.cx div 2, FIconRect.Top + 8 - sz.cy div 2 - 1, code);

  ACanvas.Brush.Color := FBrushColor;
  ACanvas.Font.Color := FFontColor;
  ACanvas.Font.Name := FFontName;
  ACanvas.Font.Size := FFontSize;
end;

end.
