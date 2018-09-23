unit Keyman.System.Util.RenderLanguageIcon;

interface

uses
  System.Types,
  System.UITypes,
  Vcl.Graphics;

procedure DrawLanguageIcon(ACanvas: TCanvas; x, y: Integer; code: string);   // I3933

implementation

procedure DrawLanguageIcon(ACanvas: TCanvas; x, y: Integer; code: string);   // I3933
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

  ACanvas.Brush.Color := $C0C2C6;
  ACanvas.FillRect(Rect(x, y, x+16, y+16));
  ACanvas.Font.Color := $404143;
  ACanvas.Font.Name := 'Tahoma';
  ACanvas.Font.Size := -10;

  sz := ACanvas.TextExtent(code);

  ACanvas.TextRect(FIconRect, FIconRect.Left + 8 - sz.cx div 2, FIconRect.Top + 8 - sz.cy div 2 - 1, code);

  ACanvas.Brush.Color := FBrushColor;
  ACanvas.Font.Color := FFontColor;
  ACanvas.Font.Name := FFontName;
  ACanvas.Font.Size := FFontSize;
end;

end.
