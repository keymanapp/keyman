program i3358_media_keys;

uses
  Forms,
  UfrmMediaKeysMain in 'UfrmMediaKeysMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
