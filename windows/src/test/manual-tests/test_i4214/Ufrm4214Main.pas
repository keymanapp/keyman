unit Ufrm4214Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Ufrm4214Dialog,
  Ufrm4214DialogWebBrowser,
  Ufrm4214DialogEWB;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  with Tfrm4214Dialog.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  with Tfrm4214DialogWebBrowser.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  with Tfrm4214DialogEWB.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

end.
