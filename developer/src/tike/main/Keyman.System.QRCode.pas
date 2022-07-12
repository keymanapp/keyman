unit Keyman.System.QRCode;

interface

uses
  DelphiZXingQRCode,
  Vcl.Graphics;

procedure DrawQRCode(const text: string; b: TBitmap);

implementation

procedure DrawQRCode(const text: string; b: TBitmap);
var
  QRCode: TDelphiZXingQRCode;
  Row, Column: Integer;
begin
  QRCode := TDelphiZXingQRCode.Create;
  try
    QRCode.Data := text;
    b.SetSize(QRCode.Rows, QRCode.Columns);
    for Row := 0 to QRCode.Rows - 1 do
    begin
      for Column := 0 to QRCode.Columns - 1 do
      begin
        if (QRCode.IsBlack[Row, Column]) then
        begin
          b.Canvas.Pixels[Column, Row] := clBlack;
        end else
        begin
          b.Canvas.Pixels[Column, Row] := clWhite;
        end;
      end;
    end;
  finally
    QRCode.Free;
  end;
end;

end.
