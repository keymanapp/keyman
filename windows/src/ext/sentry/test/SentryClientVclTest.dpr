program SentryClientVclTest;

uses
  Vcl.Forms,
  Sentry.Client.Test.SentryClientVclTestMain in 'Sentry.Client.Test.SentryClientVclTestMain.pas' {frmSentryClientVclTestMain},
  Sentry.Client in '..\Sentry.Client.pas',
  sentry in '..\sentry.pas',
  Sentry.Client.Vcl in '..\Sentry.Client.Vcl.pas',
  Keyman.System.KeymanSentryClient in '..\..\..\global\delphi\general\Keyman.System.KeymanSentryClient.pas',
  KeymanVersion in '..\..\..\global\delphi\general\KeymanVersion.pas',
  KeymanPaths in '..\..\..\global\delphi\general\KeymanPaths.pas',
  RegistryKeys in '..\..\..\global\delphi\general\RegistryKeys.pas',
  DebugPaths in '..\..\..\global\delphi\general\DebugPaths.pas',
  ErrorControlledRegistry in '..\..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  utilexecute in '..\..\..\global\delphi\general\utilexecute.pas',
  Unicode in '..\..\..\global\delphi\general\Unicode.pas';

{$R *.res}

begin
  TKeymanSentryClient.Start(TSentryClientVcl);
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
