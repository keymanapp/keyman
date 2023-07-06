unit Keyman.Configuration.UI.UfrmDiagnosticTests;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  UfrmKeymanBase;

type
  TfrmDiagnosticTests = class(TfrmKeymanBase)
    cmdSendTestException: TButton;
    cmdSendTestCOMAPIException: TButton;
    cmdClose: TButton;
    cmdSendTestEvent: TButton;
    cmdSendTestKeyman32Exception: TButton;
    cmdSendTestKMTipException: TButton;
    procedure cmdSendTestExceptionClick(Sender: TObject);
    procedure cmdSendTestEventClick(Sender: TObject);
    procedure cmdSendTestCOMAPIEventClick(Sender: TObject);
    procedure cmdSendTestKMTipExceptionClick(Sender: TObject);
    procedure cmdSendTestKeyman32ExceptionClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class procedure Run;
  end;

implementation

uses
  Sentry.Client,

  Keyman.System.KeymanSentryClient,
  KeymanEngineControl,
  kmint;

{$R *.dfm}

procedure TfrmDiagnosticTests.cmdSendTestCOMAPIEventClick(Sender: TObject);
begin
  (kmcom.Control as IKeymanEngineControl).DiagnosticTestException;
end;

procedure TfrmDiagnosticTests.cmdSendTestEventClick(Sender: TObject);
begin
  TKeymanSentryClient.Client.MessageEvent(Sentry.Client.SENTRY_LEVEL_INFO, 'Triggered '+TKeymanSentryClient.LOGGER_DESKTOP+'.kmshell');
end;

procedure TfrmDiagnosticTests.cmdSendTestExceptionClick(Sender: TObject);
begin
  TKeymanSentryClient.Validate(True);
end;

type
  TKeyman_Diagnostic = procedure(mode: Integer); stdcall;

procedure TfrmDiagnosticTests.cmdSendTestKeyman32ExceptionClick(
  Sender: TObject);
var
  h: THandle;
  Keyman_Diagnostic: TKeyman_Diagnostic;
begin
  h := GetModuleHandle('keyman32.dll');
  if h = 0 then
  begin
    ShowMessage('keyman32.dll is not loaded. Make sure Keyman is running.');
    Exit;
  end;

  Keyman_Diagnostic := GetProcAddress(h, 'Keyman_Diagnostic');
  if Assigned(Keyman_Diagnostic)
    then Keyman_Diagnostic(0)
    else ShowMessage('Could not find export Keyman_Diagnostic in keyman32.dll');
end;

procedure TfrmDiagnosticTests.cmdSendTestKMTipExceptionClick(Sender: TObject);
var
  h: THandle;
  Keyman_Diagnostic: TKeyman_Diagnostic;
begin
  h := GetModuleHandle('kmtip.dll');
  if h = 0 then
  begin
    ShowMessage('kmtip.dll is not loaded. Make sure Keyman is running and a Keyman keyboard is selected.');
    Exit;
  end;

  Keyman_Diagnostic := GetProcAddress(h, 'Keyman_Diagnostic');
  if Assigned(Keyman_Diagnostic)
    then Keyman_Diagnostic(0)
    else ShowMessage('Could not find export Keyman_Diagnostic in kmtip.dll');
end;

class procedure TfrmDiagnosticTests.Run;
var
  f: TfrmDiagnosticTests;
begin
  f := TfrmDiagnosticTests.Create(nil);
  try
    f.ShowModal;
  finally
    f.Free;
  end;
end;

end.
