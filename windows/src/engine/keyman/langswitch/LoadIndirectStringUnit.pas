unit LoadIndirectStringUnit;

interface

function LoadIndirectString(const name: WideString): WideString;

implementation

uses
  Windows;

type
  TSHLoadIndirectString = function(const pszSource: PWideChar; pszOutBuf: PWideChar; cchOutBuf: UINT; ppvReserved: Pointer): HRESULT; stdcall;

var
  hSHLWApi: THandle = 0;
  FSHLoadIndirectString: TSHLoadIndirectString = nil;

function LoadIndirectString(const name: WideString): WideString;
var
  buf: array[0..260] of WideChar;
begin
  Result := '';

  if hSHLWApi = 0 then
  begin
    hSHLWApi := LoadLibrary('shlwapi.dll');
    if hSHLWApi = 0 then
      Exit;
    FSHLoadIndirectString := GetProcAddress(hSHLWApi, 'SHLoadIndirectString');
  end;

  if not Assigned(FSHLoadIndirectString) then Exit;
  if FSHLoadIndirectString(PWideChar(name), buf, Length(buf), nil) = S_OK then
    Result := buf;
end;

initialization
finalization
  if hSHLWApi <> 0 then FreeLibrary(hSHLWApi);
  hSHLWApi := 0;
  FSHLoadIndirectString := nil;
end.
