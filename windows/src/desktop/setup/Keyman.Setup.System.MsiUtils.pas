unit Keyman.Setup.System.MsiUtils;

interface

function GetMsiVersion(const Filename: string; var VersionWithTag: string): string;

implementation

uses
  jwawintype,
  jwawinbase,
  jwawinerror,
  jwamsi,
  jwamsiquery;

function GetMsiVersion(const Filename: string; var VersionWithTag: string): string;
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

    sz := 64;
    if MsiGetProductPropertyW(hProduct, 'VersionWithTag', buf, @sz) = ERROR_SUCCESS then
      VersionWithTag := buf;

    if VersionWithTag = '' then
      VersionWithTag := Result;

    MsiCloseHandle(hProduct);
  end;
end;

end.
