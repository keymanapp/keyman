unit test_iconsave_form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    dlgOpen: TFileOpenDialog;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

uses
  System.Win.ComObj,
  Winapi.ActiveX,

  kmxfile,
  utilicon;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  ki: TKeyboardInfo;
begin
  if dlgOpen.Execute then
  begin
    GetKeyboardInfo(dlgOpen.FileName, True, ki, True);
    try
      ConvertKeyboardBitmapToAlphaIcon(ki, dlgOpen.FileName + '.test.ico');
    finally
      ki.Bitmap.Free;
      ki.Icon.Free;
      ki.MemoryDump.Free;
    end;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OleCheck(CoInitializeEx(nil, COINIT_APARTMENTTHREADED));
end;

end.
