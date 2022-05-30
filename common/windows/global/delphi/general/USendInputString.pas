unit USendInputString;

interface

uses
  Winapi.Windows;

function SendInputString(hwnd: THandle; const text: string): Boolean; overload;
function SendInputString(hwnd: THandle; const UTF32Char: DWord): Boolean; overload;

implementation

uses
  Unicode;

function SendInputString(hwnd: THandle; const UTF32Char: DWord): Boolean;
begin
  if Uni_IsSurrogate(UTF32Char)
    then Result := SendInputString(hwnd, Uni_UTF32ToSurrogate1(UTF32Char) + Uni_UTF32ToSurrogate2(UTF32Char))
    else Result := SendInputString(hwnd, Char(UTF32Char));
end;

function SendInputString(hwnd: THandle; const text: string): Boolean;
var
  i: Integer;
  chr: array of TInput;
begin
  AttachThreadInput(GetCurrentThreadId, GetWindowThreadProcessId(hwnd, nil), TRUE);

  SetForegroundWindow(hwnd);
  SetActiveWindow(hwnd);
  SetFocus(hwnd);

  AttachThreadInput(GetCurrentThreadId, GetwindowThreadProcessId(hwnd, nil), FALSE);

  SetLength(chr, Length(text) * 2);

  for i := 0 to Length(text) - 1 do
  begin
    with chr[i*2] do
    begin
      Itype := INPUT_KEYBOARD;
      ki.wVk := 0;
      ki.wScan := Ord(text[i+1]);
      ki.dwFlags := KEYEVENTF_UNICODE;
      ki.time := 0;
      ki.dwExtraInfo := 0;
    end;
    with chr[i*2+1] do
    begin
      Itype := INPUT_KEYBOARD;
      ki.wVk := 0;
      ki.wScan := Ord(text[i+1]);
      ki.dwFlags := KEYEVENTF_UNICODE or KEYEVENTF_KEYUP;
      ki.time := 0;
      ki.dwExtraInfo := 0;
    end;
  end;

  Result := SendInput(Length(text)*2, chr[0], sizeof(TInput)) = Dword(Length(text) * 2);
end;

end.
