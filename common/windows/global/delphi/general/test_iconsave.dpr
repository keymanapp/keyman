program test_iconsave;

uses
  Vcl.Forms,
  test_iconsave_form in '..\..\..\test\test_iconsave\test_iconsave_form.pas' {Form1},
  utilicon in 'utilicon.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
