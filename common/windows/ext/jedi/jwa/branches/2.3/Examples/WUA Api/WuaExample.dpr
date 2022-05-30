program WuaExample;

uses
  Forms,
  Main in 'Main.pas' {wuaMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TwuaMain, wuaMain);
  Application.Run;
end.
