unit Keyman.System.UpdateCheckStorage;

interface

uses
  Keyman.System.UpdateCheckResponse;

type
  TUpdateCheckStorage = class sealed
  private
    class function MetadataFilename: string; static;
  public
    class function HasUpdates: Boolean; static;
    class function LoadUpdateCacheData(var data: TUpdateCheckResponse): Boolean; static;
    class procedure SaveUpdateCacheData(const data: TUpdateCheckResponse); static;
    class function HasKeyboardPackages(const data: TUpdateCheckResponse): Boolean; static;
    class function HasKeymanInstallFile(const data: TUpdateCheckResponse): Boolean; static;
  end;

implementation

uses
  System.SysUtils,
  System.RegularExpressions,

  KeymanPaths,
  KeymanVersion;

{ TUpdateCheckStorage }

class function TUpdateCheckStorage.MetadataFilename: string;
begin
  Result := TKeymanPaths.KeymanUpdateCachePath(TKeymanPaths.S_UpdateCache_Metadata);
end;

class procedure TUpdateCheckStorage.SaveUpdateCacheData(
  const data: TUpdateCheckResponse);
begin
  ForceDirectories(TKeymanPaths.KeymanUpdateCachePath);
  data.SaveToFile(MetadataFilename);
end;

class function TUpdateCheckStorage.HasUpdates: Boolean;
begin
  Result := FileExists(MetadataFilename);
end;

class function TUpdateCheckStorage.LoadUpdateCacheData(var data: TUpdateCheckResponse): Boolean;
begin
  Result :=
    HasUpdates and
    data.LoadFromFile(MetadataFilename, 'bundle', CKeymanVersionInfo.Version);
end;

class function TUpdateCheckStorage.HasKeyboardPackages(const data: TUpdateCheckResponse): Boolean;
var
  i : Integer;
  fileName : string;
  KeyboardRegex: TRegEx;
begin
  Result := False;
  KeyboardRegex := TRegEx.Create('\.k..$');
  for i := 0 to High(data.Packages) do
  begin
    fileName := data.Packages[i].FileName;
    if KeyboardRegex.IsMatch(fileName) then
      Result := True;
  end;
end;

class function TUpdateCheckStorage.HasKeymanInstallFile(const data: TUpdateCheckResponse): Boolean;
var
  fileExtension: string;
begin
  fileExtension := LowerCase(ExtractFileExt(data.FileName));
  if fileExtension = '.exe' then
    Result := True
  else
    Result := False
end;

end.
