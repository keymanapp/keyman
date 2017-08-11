{
VistaElevation shows how to use out of process COM elevation
in a Windows Vista system.
This library is registered and gives information about the actual elevation status.

Version: 0.5
Release: 1.10.2007
Written by Christian Wimmer
}
library VistaElevationDLL;

uses
  ComServ,
  VistaElevationDLL_TLB in 'VistaElevationDLL_TLB.pas',
  VistaElevatedImplementation in 'VistaElevatedImplementation.pas' {ElevationDemoObject: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB} 
{$R *.RES}

begin
end.
