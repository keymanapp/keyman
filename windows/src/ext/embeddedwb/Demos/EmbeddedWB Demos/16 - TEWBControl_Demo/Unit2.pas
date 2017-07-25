unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  Dialogs, StdCtrls, OleCtrls, SHDocVw_EWB, EwbCore, EmbeddedWB;

type
  TForm2 = class(TForm)
    EmbeddedWB1: TEmbeddedWB;
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  Close;
end;

end.
