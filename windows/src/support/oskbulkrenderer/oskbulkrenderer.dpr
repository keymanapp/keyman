program oskbulkrenderer;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Keyman.System.OSKBulkRenderer in 'Keyman.System.OSKBulkRenderer.pas',
  OnScreenKeyboard in '..\..\global\delphi\comp\OnScreenKeyboard.pas',
  OnScreenKeyboardData in '..\..\global\delphi\visualkeyboard\OnScreenKeyboardData.pas',
  VisualKeyboard in '..\..\global\delphi\visualkeyboard\VisualKeyboard.pas',
  VisualKeyboardParameters in '..\..\global\delphi\visualkeyboard\VisualKeyboardParameters.pas',
  ExtShiftState in '..\..\global\delphi\comp\ExtShiftState.pas',
  CleartypeDrawCharacter in '..\..\global\delphi\general\CleartypeDrawCharacter.pas',
  usp10 in '..\..\global\delphi\general\usp10.pas',
  GetOsVersion in '..\..\global\delphi\general\GetOsVersion.pas',
  Unicode in '..\..\global\delphi\general\Unicode.pas',
  Glossary in '..\..\global\delphi\general\Glossary.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  ScanCodeMap in '..\..\global\delphi\general\ScanCodeMap.pas',
  kmxfileconsts in '..\..\global\delphi\general\kmxfileconsts.pas',
  VKeyChars in '..\..\global\delphi\general\VKeyChars.pas',
  VisualKeyboardLoaderBinary in '..\..\global\delphi\visualkeyboard\VisualKeyboardLoaderBinary.pas',
  VisualKeyboardLoaderXML in '..\..\global\delphi\visualkeyboard\VisualKeyboardLoaderXML.pas',
  VisualKeyboardSaverBinary in '..\..\global\delphi\visualkeyboard\VisualKeyboardSaverBinary.pas',
  VisualKeyboardSaverXML in '..\..\global\delphi\visualkeyboard\VisualKeyboardSaverXML.pas',
  utilstr in '..\..\global\delphi\general\utilstr.pas',
  utilsystem in '..\..\global\delphi\general\utilsystem.pas',
  utilexecute in '..\..\global\delphi\general\utilexecute.pas',
  VKeys in '..\..\global\delphi\general\VKeys.pas',
  VisualKeyboardExportPNG in '..\..\global\delphi\visualkeyboard\VisualKeyboardExportPNG.pas',
  VisualKeyboardExportBMP in '..\..\global\delphi\visualkeyboard\VisualKeyboardExportBMP.pas',
  keybtn in '..\..\global\delphi\comp\keybtn.pas',
  VersionInfo in '..\..\global\delphi\general\VersionInfo.pas';

begin

{$R version.res}

  try
    if not TOSKBulkRenderer.Run
      then ExitCode := 1
      else ExitCode := 0;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
