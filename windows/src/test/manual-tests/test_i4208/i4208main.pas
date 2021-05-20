unit i4208main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.OleCtrls, SHDocVw_EWB, EwbCore,
  EmbeddedWB, Vcl.StdCtrls, KeymanEmbeddedWB, UfrmWebContainer, UfrmHelp;

type
  TForm1 = class(TfrmWebcontainer)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  with TfrmHelp.Create(Self) do
  begin
    Show;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FormStyle := fsNormal;
end;

end.
