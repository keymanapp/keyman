program test_iconsave;

uses
  Vcl.Forms,
  test_iconsave_form in 'test_iconsave_form.pas' {Form1},
  utilicon in '..\..\..\..\..\common\windows\delphi\general\utilicon.pas',
  kmxfile in '..\..\..\..\..\common\windows\delphi\keyboards\kmxfile.pas',
  crypt_base in '..\..\global\delphi\productactivation\crypt_base.pas',
  crypt_guid in '..\..\global\delphi\productactivation\crypt_guid.pas',
  productactivation in '..\..\global\delphi\productactivation\productactivation.pas',
  tavultesoft_certificate in '..\..\global\delphi\productactivation\tavultesoft_certificate.pas',
  Unicode in '..\..\..\..\common\windows\delphi\general\Unicode.pas',
  crypt_user in '..\..\global\delphi\productactivation\crypt_user.pas',
  KeyNames in '..\..\..\..\common\windows\delphi\general\KeyNames.pas',
  KeymanVersion in '..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  utildir in '..\..\..\..\common\windows\delphi\general\utildir.pas',
  utilfiletypes in '..\..\..\..\common\windows\delphi\general\utilfiletypes.pas',
  kmcomapi_TLB in '..\..\engine\kmcomapi\kmcomapi_TLB.pas',
  utilolepicture in '..\..\engine\kmcomapi\util\utilolepicture.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
