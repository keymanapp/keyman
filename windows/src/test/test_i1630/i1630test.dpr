program i1630test;

uses
  Forms,
  check_shortcuts in 'check_shortcuts.pas' {Form1},
  HotkeyUtils in '..\..\global\delphi\general\HotkeyUtils.pas',
  utilhotkey in '..\..\global\delphi\general\utilhotkey.pas',
  KeyNames in '..\..\global\delphi\general\KeyNames.pas',
  kmcomapi_TLB in '..\..\engine\kmcomapi\kmcomapi_TLB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
