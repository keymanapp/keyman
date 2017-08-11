{
VistaElevation shows how to use out of process COM elevation
in a Windows Vista system.

Version: 0.5
Release: 1.10.2007
Written by Christian Wimmer
}
program VistaElevation;
{$I JwsclUAC.inc}
{.$APPTYPE CONSOLE}

uses
  SysUtils, Activex, ComObj, VistaElevationDLL_TLB,
  JwaWindows, Dialogs,
  JwsclElevation;

var  ElevatedObject: IElevationDemoObject;
begin
  CoInitialize(nil);

  try
    OleCheck(
      JwCoCreateInstanceAsAdmin(
        GetForegroundWindow,
        CLASS_ElevationDemoObject,
        IID_IElevationDemoObject,
        ElevatedObject));
  except
    on E : EOleSysError do
    begin
      MessageDlg(Format('The elevation demo could not be launched properly. Maybe you did not '+
        'install/register the VistaElevationDLL.dll correctly? Message: (0x%x) %s',[E.ErrorCode,E.Message]), mtError, [mbOK], 0);
      halt(1);
    end;
  end;

  ElevatedObject.DoSomething('Hello from JWSCL Vista Elevation Demo');
end.
