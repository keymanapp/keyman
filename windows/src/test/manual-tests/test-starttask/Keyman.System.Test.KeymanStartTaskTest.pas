unit Keyman.System.Test.KeymanStartTaskTest;

interface

procedure Run;

implementation

uses
  Winapi.ActiveX,
  Winapi.Windows,

  Keyman.System.KeymanStartTask;

procedure Run;
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  try
    if ParamStr(1) = '-d' then
      TKeymanStartTask.DeleteTask
    else if ParamStr(1) = '-c' then
      TKeymanStartTask.CreateTask
    else
      TKeymanStartTask.Recreate;
  finally
    CoUninitialize;
  end;
end;

end.
