program kmdevlink;

uses
  Forms,
  UfrmMain in 'UfrmMain.pas' {frmMain},
  VersionInfo in '..\..\..\..\common\windows\delphi\general\VersionInfo.pas',
  klog in '..\..\..\..\common\windows\delphi\general\klog.pas',
  KeymanTrayIcon in '..\..\engine\keyman\KeymanTrayIcon.pas',
  GetOsVersion in '..\..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  UfrmOpenCRMRecord in 'UfrmOpenCRMRecord.pas' {frmOpenCRMRecord},
  findfonts in '..\..\..\..\common\windows\delphi\general\findfonts.pas',
  utilcheckfontchars in 'utilcheckfontchars.pas',
  UserMessages in '..\..\..\..\common\windows\delphi\general\UserMessages.pas',
  ErrorControlledRegistry in '..\..\..\..\common\windows\delphi\vcl\ErrorControlledRegistry.pas',
  Unicode in '..\..\..\..\common\windows\delphi\general\Unicode.pas',
  utilexecute in '..\..\..\..\common\windows\delphi\general\utilexecute.pas',
  UfrmCharacterIdentifier in 'UfrmCharacterIdentifier.pas' {frmCharacterIdentifier};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Keyman Dev Link';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
  frmMain.Free;
  frmMain := nil;
  frmCharacterIdentifier.Free;
  frmCharacterIdentifier := nil;
end.

