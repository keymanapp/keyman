unit Sentry.Client.Test.SentryClientVclTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmSentryClientVclTestMain = class(TForm)
    cmdSendEvent: TButton;
    cmdCrashApp: TButton;
    procedure cmdSendEventClick(Sender: TObject);
    procedure cmdCrashAppClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSentryClientVclTestMain: TfrmSentryClientVclTestMain;

implementation

uses
  Sentry.Client,
  Keyman.System.KeymanSentryClient;

{$R *.dfm}

type
  ETestVclError = class(Exception);

procedure TfrmSentryClientVclTestMain.cmdCrashAppClick(Sender: TObject);
begin
  raise ETestVclError.Create('This ia a crash from the vcl test app');
end;

procedure TfrmSentryClientVclTestMain.cmdSendEventClick(Sender: TObject);
begin
  TKeymanSentryClient.Client.MessageEvent(SENTRY_LEVEL_INFO, 'This is a message from the VCL test app');
end;

end.
