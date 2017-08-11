library RunElCOM;

uses
  ComServ,
  RunElCOM_TLB in 'RunElCOM_TLB.pas',
  JwCoRunElevated in 'JwCoRunElevated.pas' {JwRunElevated: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
 