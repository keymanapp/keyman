program settings_manager;

uses
  Vcl.Forms,
  Keyman.UI.SettingsManager.UfrmSettingsManagerMain in 'Keyman.UI.SettingsManager.UfrmSettingsManagerMain.pas' {frmSettingsManagerMain},
  Keyman.System.SettingsManager in 'Keyman.System.SettingsManager.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  Keyman.System.Settings in 'Keyman.System.Settings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSettingsManagerMain, frmSettingsManagerMain);
  Application.Run;
end.
