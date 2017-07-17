unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, OleServer, keymanapi_TLB;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Image1: TImage;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    procedure ReportErrors(e: Exception; k: ITavultesoftKeyman);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses ActiveX;

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  k: ITavultesoftKeyman;
  i, j: Integer;
begin
  k := CoTavultesoftKeyman.Create;
  try
    k.Keyboards.Install('c:\keyman\5.0\assorted\genkmnp\enhlao.kmx', True);
  except
    on E:Exception do
    begin
      ReportErrors(E, k);
      raise;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  k: ITavultesoftKeyman;
  n, i: Integer;
  p: IPicture;
  p2, pt: TPoint;
  newhdc,oldhdc: HDC;
  bo: OLE_HANDLE;
begin
  k := CoTavultesoftKeyman.Create;
  for i := 0 to k.Keyboards.Count - 1 do
  begin
    if k.Keyboards.Items[i].Loaded then
      memo1.Lines.Add(k.Keyboards.Items[i].Name+',True')
    else
      memo1.Lines.Add(k.Keyboards.Items[i].Name+',False');
  end;

  p := k.Keyboards.Items[1].Bitmap;

  p.get_Width(pt.x);
  p.get_Height(pt.y);

  p2.x := MulDiv(pt.x, GetDeviceCaps(Image1.Picture.Bitmap.Canvas.Handle, LOGPIXELSX), 2540);
  p2.y := MulDiv(pt.y, GetDeviceCaps(Image1.Picture.Bitmap.Canvas.Handle, LOGPIXELSY), 2540);

  Image1.Picture.Bitmap.Width := p2.x;
  Image1.Picture.Bitmap.Height := p2.y;

  p.Render(Image1.Picture.Bitmap.Canvas.Handle, 0,0,p2.x,p2.y,0,pt.y,pt.x,-pt.y, Rect(0,0,0,0));

  k.Keyboards.Items[0].Loaded := False;
  k.Keyboards.Save;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  k: ITavultesoftKeyman;
begin
  k := CoTavultesoftKeyman.Create;
  try
    k.Packages.Install('c:\keyman\5.1\devel\projects\basiclao.kmp', True);
  except
    on E:Exception do
    begin
      ReportErrors(E, k);
      raise;
    end;
  end;
end;

procedure TForm1.ReportErrors(e: Exception; k: ITavultesoftKeyman);
var
  i, j: Integer;
begin
      memo1.Lines.Add(E.Message);
      for i := 0 to k.Errors.Count - 1 do
        with k.Errors.Items[i] do
        begin
          memo1.Lines.Add(IntToHex(ErrorCode, 8) + ' - :::' + Description + '::: - ' + IntToStr(ParameterCount));
          for j := 0 to ParameterCount - 1 do
            memo1.Lines.Add(Parameter[j] + '=' + ParameterValue[j]);
        end;
end;

end.
