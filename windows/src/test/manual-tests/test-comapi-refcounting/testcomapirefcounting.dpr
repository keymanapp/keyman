program testcomapirefcounting;

uses
  Forms,
  testcomapi in 'testcomapi.pas' {Form1},
  kmcomapi_TLB in '..\..\engine\kmcomapi\kmcomapi_TLB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
