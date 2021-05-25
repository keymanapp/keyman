program wm_unichar;

uses
  Forms,
  wm_unichar_form in 'wm_unichar_form.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
