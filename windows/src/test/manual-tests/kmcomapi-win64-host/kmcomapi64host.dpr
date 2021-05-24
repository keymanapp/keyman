program kmcomapi64host;

uses
  Vcl.Forms,
  khost in 'khost.pas' {Form1},
  keymanapi_TLB in '..\..\engine\kmcomapi\keymanapi_TLB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
