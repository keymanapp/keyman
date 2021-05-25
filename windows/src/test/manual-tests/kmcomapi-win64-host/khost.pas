unit khost;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  keymanapi_tlb;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    c: IKeyman;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  c := CoKeyman.Create;
end;

end.
