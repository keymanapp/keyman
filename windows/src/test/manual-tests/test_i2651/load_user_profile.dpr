program load_user_profile;

uses
  Forms,
  test_load_user_profile in 'test_load_user_profile.pas' {Form1},
  ShellUserRegistry in '..\..\global\delphi\general\ShellUserRegistry.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
