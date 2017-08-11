library prjSearchhookDemo;

uses
  SysUtils,
  Classes,
  _SearchHook in '_SearchHook.pas',
  comserv,
  Searchhook_TLB in 'Searchhook_TLB.pas',
  prjSearchhookDemo_TLB in 'prjSearchhookDemo_TLB.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;


{$R *.TLB}

{$R *.RES}

begin
end.
