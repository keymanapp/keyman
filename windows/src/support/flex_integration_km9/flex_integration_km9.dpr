program flex_integration_km9;

uses
  Vcl.Forms,
  flex_integration_km9_main in 'flex_integration_km9_main.pas' {Form1},
  kmcomapi_TLB in '..\..\engine\kmcomapi\kmcomapi_TLB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
