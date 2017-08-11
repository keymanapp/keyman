unit Ufrm4214DialogEWB;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.OleCtrls, SHDocVw_EWB, EwbCore,
  EmbeddedWB;

type
  Tfrm4214DialogEWB = class(TForm)
    EmbeddedWB1: TEmbeddedWB;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure Tfrm4214DialogEWB.FormCreate(Sender: TObject);
begin
  EmbeddedWB1.Go('http://www.google.com/');
end;

end.
