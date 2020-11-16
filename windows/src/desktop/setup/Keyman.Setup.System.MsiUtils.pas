unit Keyman.Setup.System.MsiUtils;

interface

function GetMsiVersion(const Filename: string): string;

implementation

uses
  jwawintype,
  jwawinbase,
  jwawinerror,
  jwamsi,
  jwamsiquery;

function GetMsiVersion(const Filename: string): string;
var
  sz: DWord;
  buf: array[0..64] of WideChar;
  hProduct: MSIHANDLE;
begin
  Result := '';
  if MsiOpenPackageExW(PWideChar(Filename), MSIOPENPACKAGEFLAGS_IGNOREMACHINESTATE, hProduct) = ERROR_SUCCESS then
  begin
    sz := 64;
    if MsiGetProductPropertyW(hProduct, 'ProductVersion', buf, @sz) = ERROR_SUCCESS then
      Result := buf;
    MsiCloseHandle(hProduct);
  end;
end;

end.
