program SentryClientVclTest;

uses
  Vcl.Forms,
  Sentry.Client.Test.SentryClientVclTestMain in 'Sentry.Client.Test.SentryClientVclTestMain.pas' {frmSentryClientVclTestMain},
  Sentry.Client in '..\Sentry.Client.pas',
  sentry in '..\sentry.pas',
  Sentry.Client.Vcl in '..\Sentry.Client.Vcl.pas',
  Keyman.System.KeymanSentryClient in '..\..\..\general\Keyman.System.KeymanSentryClient.pas',
  KeymanVersion in '..\..\..\general\KeymanVersion.pas',
  KeymanPaths in '..\..\..\general\KeymanPaths.pas',
  RegistryKeys in '..\..\..\general\RegistryKeys.pas',
  DebugPaths in '..\..\..\general\DebugPaths.pas',
  ErrorControlledRegistry in '..\..\..\vcl\ErrorControlledRegistry.pas',
  utilexecute in '..\..\..\general\utilexecute.pas',
  Unicode in '..\..\..\general\Unicode.pas';

{$R *.res}

begin
  TKeymanSentryClient.Start(TSentryClientVcl, kscpDesktop, '', [kscfCaptureExceptions, kscfShowUI, kscfTerminate]);
  try
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.CreateForm(TfrmSentryClientVclTestMain, frmSentryClientVclTestMain);
  Application.Run;

    // This should be done before shutting down the exception manager
    if Application.MainForm <> nil then
      Application.MainForm.Free;
  finally
    TKeymanSentryClient.Stop;
  end;
end.
