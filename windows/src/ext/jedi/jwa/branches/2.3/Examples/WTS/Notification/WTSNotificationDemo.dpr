program WTSNotificationDemo;

uses
  Forms,
  MainForm in 'MainForm.pas' {NotificiationForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TNotificiationForm, NotificiationForm);
  Application.Run;
end.
