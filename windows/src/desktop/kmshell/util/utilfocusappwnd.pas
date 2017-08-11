unit utilfocusappwnd;

interface

function FocusConfiguration: Boolean;  // I2720
function FocusSplash: Boolean;  // I2720
function FocusTextEditor: Boolean;  // I2720

implementation

uses
  UfrmMain,
  UfrmSplash,
  UfrmTextEditor,
  Winapi.Windows;

function FocusAppWnd(h: HWND): Boolean;
var
  hwndOwner: Cardinal;  // I2720
begin
  if h = 0 then
    Result := False
  else
  begin
    hwndOwner := GetWindow(h, GW_OWNER);
    if hwndOwner <> 0 then
      if IsIconic(hwndOwner) then OpenIcon(hwndOwner);  // I2782
    Result := SetForegroundWindow(h);
  end;
end;

function FocusSplash: Boolean;  // I2720
begin
  Result := FocusAppWnd(TfrmSplash.GetRegisteredHandle);
end;

function FocusConfiguration: Boolean;  // I2720
begin
  Result := FocusAppWnd(TfrmMain.GetRegisteredHandle);
end;

function FocusTextEditor: Boolean;  // I2720
begin
  Result := FocusAppWnd(TfrmTextEditor.GetRegisteredHandle);
end;

end.
