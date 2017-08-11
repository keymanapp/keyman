program UACServiceTest;

uses
  SvcMgr,
  Unit1 in 'Unit1.pas' {UACService: TService};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TUACService, UACService);
  Application.Run;
end.
