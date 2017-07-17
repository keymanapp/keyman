program charident;

uses
  Forms,
  UfrmCharacterIdentifier in 'UfrmCharacterIdentifier.pas' {frmCharacterIdentifier},
  PlusmemoU in '..\..\global\delphi\plusmemou\PlusmemoU.pas',
  utilcheckfontchars in '..\..\online\tscrmlink\utilcheckfontchars.pas',
  findfonts in '..\..\global\delphi\general\findfonts.pas',
  KeymanTrayIcon in '..\..\engine\keyman\KeymanTrayIcon.pas',
  UserMessages in '..\..\global\delphi\general\UserMessages.pas',
  GetOsVersion in '..\..\global\delphi\general\GetOsVersion.pas',
  klog in '..\..\global\delphi\general\klog.pas',
  urlutil in '..\..\global\delphi\general\urlutil.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  UfrmRenderingTestCases in 'UfrmRenderingTestCases.pas' {frmRenderingTestCases},
  Unicode in '..\..\global\delphi\general\Unicode.pas',
  pngextra in '..\..\..\cmpsrc\PngDelphi\pngextra.pas',
  pngimage in '..\..\..\cmpsrc\PngDelphi\pngimage.pas',
  pnglang in '..\..\..\cmpsrc\PngDelphi\pnglang.pas',
  zlibpas in '..\..\..\cmpsrc\PngDelphi\zlibpas.pas',
  utilxml in '..\..\global\delphi\general\utilxml.pas',
  ttinfo in '..\..\global\delphi\general\ttinfo.pas';

{$R *.res}
{$R version.res}

begin
  Application.Initialize;
  Application.Title := 'Keyman Character Identifier';
  Application.CreateForm(TfrmCharacterIdentifier, frmCharacterIdentifier);
  Application.CreateForm(TfrmRenderingTestCases, frmRenderingTestCases);
  Application.Run;
end.
