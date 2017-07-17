program i3721;

uses
  Vcl.Forms,
  test_i3721 in 'test_i3721.pas' {Form1},
  kmcomapi_TLB in '..\..\engine\kmcomapi\kmcomapi_TLB.pas',
  msctf in '..\..\global\delphi\winapi\msctf.pas',
  utiltsf in '..\..\global\delphi\general\utiltsf.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
