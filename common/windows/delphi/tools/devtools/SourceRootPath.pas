unit SourceRootPath;

interface

{$IFDEF VER310}
const DelphiMajorVersion = '18.0'; 
{$ELSE}
{$IFDEF VER320}
const DelphiMajorVersion = '19.0'; 
{$ELSE}
{$IFDEF VER330}
const DelphiMajorVersion = '20.0'; 
{$ELSE}
{$IFDEF VER340}
const DelphiMajorVersion = '21.0';
{$ELSE}
ERROR: must define Delphi version
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

const DelphiBasePath = 'C:\Program Files (x86)\Embarcadero\Studio\' + DelphiMajorVersion + '\';

function CSourcePath: string;
function CSourceRootPath: string; //='C:\Projects\keyman\open\windows\src';
function CSourceBinRootPath: string; //='C:\Projects\keyman\open\windows\bin';
function CSourceRootLibPath: string; //='C:\Projects\keyman\open\windows\lib';

implementation

uses
  System.SysUtils;

function CSourcePath: string;
begin
  // Root path of repo should be found in environment variable KEYMAN_ROOT
  // e.g. C:\projects\keyman\open
  Result := GetEnvironmentVariable('KEYMAN_ROOT');
  if Result = '' then
  begin
    // If not found, look at path of executable. It should be either
    // src\buildtools\<app>\<app.exe> or bin\buildtools\<app.exe>

    Result := ExtractFileDir(ParamStr(0));
    if not SameText(ExtractFileName(Result), 'buildtools') then
      Result := ExtractFileDir(Result);
    if not SameText(ExtractFileName(Result), 'buildtools') then
    begin
      raise Exception.Create('Unable to locate source root path. Set KEYMAN_ROOT environment variable accordingly');
    end;
    Result := ExtractFileDir(Result); //src or bin
    Result := ExtractFileDir(Result); //windows
    Result := ExtractFileDir(Result); //root
  end
  else
    Result := ExcludeTrailingPathDelimiter(Result);
end;

function CSourceRootPath: string; //='C:\Projects\keyman\open\windows\src';
begin
  Result := CSourcePath + '\windows\src';
end;

function CSourceBinRootPath: string; //='C:\Projects\keyman\open\windows\bin';
begin
  Result := CSourcePath + '\windows\bin';
end;

function CSourceRootLibPath: string; //='C:\Projects\keyman\open\windows\lib';
begin
  Result := CSourcePath + '\windows\lib';
end;

end.
