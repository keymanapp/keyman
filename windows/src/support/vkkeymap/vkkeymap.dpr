program vkkeymap;

uses
  Vcl.Forms,
  UfrmVKKeyMap in 'UfrmVKKeyMap.pas' {Form1},
  OnScreenKeyboard in '..\..\..\..\common\windows\delphi\components\OnScreenKeyboard.pas',
  OnScreenKeyboardData in '..\..\..\..\common\windows\delphi\visualkeyboard\OnScreenKeyboardData.pas',
  VisualKeyboardParameters in '..\..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardParameters.pas',
  ExtShiftState in '..\..\..\..\common\windows\delphi\visualkeyboard\ExtShiftState.pas',
  CleartypeDrawCharacter in '..\..\..\..\common\windows\delphi\general\CleartypeDrawCharacter.pas',
  usp10 in '..\..\..\..\common\windows\delphi\general\usp10.pas',
  GetOsVersion in '..\..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  Unicode in '..\..\..\..\common\windows\delphi\general\Unicode.pas',
  Glossary in '..\..\..\..\common\windows\delphi\general\Glossary.pas',
  ScanCodeMap in '..\..\..\..\common\windows\delphi\general\ScanCodeMap.pas',
  ErrorControlledRegistry in '..\..\..\..\common\windows\delphi\vcl\ErrorControlledRegistry.pas',
  RegistryKeys in '..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  VKeys in '..\..\..\..\common\windows\delphi\general\VKeys.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
