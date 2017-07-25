program Desktops;

{$APPTYPE CONSOLE}

uses
  JwaWindows,
  JwsclWinStations,
  JwsclTypes,
  JwsclStrings;

var WinSta : TJwSecurityWindowStation;
    Names : TJwTJwStringArray;
    i : Integer;
begin
  WinSta := TJwSecurityWindowStation.Open('winsta0',true, WINSTA_ENUMDESKTOPS);
  try
    Names := WinSta.DesktopNames;

    for i := low(Names) to high(Names) do
      Writeln(Names[i]);

  finally
    WinSta.Free;
  end;

  readln;
end.
