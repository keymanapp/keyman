program passtructsize;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  compile in '..\..\..\..\global\delphi\general\compile.pas',
  kmxfileconsts in '..\..\..\..\global\delphi\general\kmxfileconsts.pas',
  RegistryKeys in '..\..\..\..\global\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\..\..\global\delphi\general\KeymanVersion.pas',
  RedistFiles in '..\..\..\..\developer\TIKE\main\RedistFiles.pas',
  ErrorControlledRegistry in '..\..\..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  Upload_Settings in '..\..\..\..\global\delphi\general\Upload_Settings.pas',
  DebugPaths in '..\..\..\..\global\delphi\general\DebugPaths.pas',
  VersionInfo in '..\..\..\..\global\delphi\general\VersionInfo.pas';

{
  These size values are used in unit tests to ensure
  that the structure sizes correspond precisely across
  compilers (pas and c++).
}
const sz_FILE_STORE = sizeof(FILE_STORE);
const sz_FILE_KEY = sizeof(FILE_KEY);
const sz_FILE_GROUP = sizeof(FILE_GROUP);
const sz_FILE_DEADKEY = sizeof(FILE_DEADKEY);
const sz_FILE_VKDICTIONARY = sizeof(FILE_VKDICTIONARY);
const sz_FILE_KEYBOARD = sizeof(FILE_KEYBOARD);

begin
  writeln('sz_FILE_STORE = '+IntToStr(sz_FILE_STORE));
  writeln('sz_FILE_KEY = '+IntToStr(sz_FILE_KEY));
  writeln('sz_FILE_GROUP = '+IntToStr(sz_FILE_GROUP));
  writeln('sz_FILE_DEADKEY = '+IntToStr(sz_FILE_DEADKEY));
  writeln('sz_FILE_VKDICTIONARY = '+IntToStr(sz_FILE_VKDICTIONARY));
  writeln('sz_FILE_KEYBOARD = '+IntToStr(sz_FILE_KEYBOARD));
end.
