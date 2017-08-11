program WinStation;

uses
  Forms,
{$IFDEF FPC}
  Interfaces, // this includes the LCL widgetset
{$ENDIF FPC}
  MainForm in 'MainForm.pas' {Form1},
  JwsclWinStations;



begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
