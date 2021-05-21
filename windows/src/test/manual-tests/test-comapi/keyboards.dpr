program keyboards;

uses
  Forms,
  main in 'main.pas' {Form1},
  kmcomapi_TLB in '..\..\engine\kmcomapi\kmcomapi_TLB.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
