unit Keyman.UI.FontUtils;

interface

uses
  System.SysUtils,
  Vcl.Graphics,
  Winapi.Windows;

type
  TFontUtils = class
    class function FontSizeInPoints(const name: string; size: Integer): Integer; overload;
    class function FontSizeInPoints(f: TFont): Integer; overload;
    class function FontSizeInPixels(f: TFont): Integer;
  end;

implementation

class function TFontUtils.FontSizeInPoints(const name: string; size: Integer): Integer;
var
  tm: TTextMetric;
begin
  // The TFont.Size value may or may not include internal leading
  // so we ask the Windows API for the value including internal leading
  if size < 0 then
  begin
    with Vcl.Graphics.TBitmap.Create do
    try
      Canvas.Font.Name := name;
      Canvas.Font.Size := size;
      Canvas.TextExtent('A'); // Without this, the font may not be selected in.
      GetTextMetrics(Canvas.Handle, tm);
      Result := (Canvas.Font.Height) * 72 div GetDeviceCaps(Canvas.Handle, LOGPIXELSY);
    finally
      Free;
    end;
  end
  else
    Result := size;
end;

class function TFontUtils.FontSizeInPixels(f: TFont): Integer;
var
  tm: TTextMetric;
begin
  // The TFont.Size value may or may not include internal leading
  // so we ask the Windows API for the value including internal leading
  with Vcl.Graphics.TBitmap.Create do
  try
    Canvas.Font.Name := f.Name;
    Canvas.Font.Size := f.Size;
    Canvas.TextExtent('A'); // Without this, the font may not be selected in.
    GetTextMetrics(Canvas.Handle, tm);
    Result := tm.tmHeight;
  finally
    Free;
  end;
end;

class function TFontUtils.FontSizeInPoints(f: TFont): Integer;
begin
  Result := FontSizeInPoints(f.Name, f.Size);
end;

end.
