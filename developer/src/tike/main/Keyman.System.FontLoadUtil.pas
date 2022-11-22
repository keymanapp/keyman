unit Keyman.System.FontLoadUtil;

interface

uses
  System.Classes;

type
  TFontLoadUtil = class sealed
  public
    class function LoadFontData(const AFontName: string; Data: TStream): Boolean; static;
  end;

implementation

uses
  Vcl.Graphics,
  Winapi.Windows;

class function TFontLoadUtil.LoadFontData(const AFontName: string;
  Data: TStream): Boolean;
var
  sz: DWord;
  FHDC: THandle;
  Canvas: TCanvas;
begin
  if AFontName = '' then
    Exit(False);

  Canvas := TCanvas.Create;
  try
    FHDC := GetDC(GetDesktopWindow);
    try
      Canvas.Handle := FHDC;
      try
        Canvas.Font.Name := AFontName;
        Canvas.TextExtent('A'); // This forces a csFontValid   // I4824

        sz := Winapi.Windows.GetFontData(Canvas.Handle, 0, 0, nil, 0);
        if sz = GDI_ERROR then
          Exit(False);

        Data.Size := sz;
        Data.Position := 0;
        if Winapi.Windows.GetFontData(Canvas.Handle, 0, 0, (Data as TMemoryStream).Memory, sz) = GDI_ERROR then
        begin
          Data.Size := 0;
          Exit(False);
        end;
      finally
        Canvas.Handle := 0;
      end;
    finally
      ReleaseDC(GetDesktopWindow, FHDC);
    end;
  finally
    Canvas.Free;
  end;
  Result := True;
end;

end.
