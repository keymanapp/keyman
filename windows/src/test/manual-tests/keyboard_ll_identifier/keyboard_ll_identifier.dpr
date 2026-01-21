program keyboard_ll_identifier;

uses
  Vcl.Forms,
  keyboard_ll_identifier_unit in 'keyboard_ll_identifier_unit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
