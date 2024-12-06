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
  f: TSearchRec;
begin
  Result := False;
  for i := 0 to High(data.Packages) do
  begin
    fileName := data.Packages[i].FileName;
    if FindFirst(fileName + '*.k??', 0, f) = 0 then
      Result := True;
    System.SysUtils.FindClose(f);
  end;
end;

class function TUpdateCheckStorage.HasKeymanInstallFile(const data: TUpdateCheckResponse): Boolean;
var
  i : Integer;
  fileName : string;
  f: TSearchRec;
begin
  Result := False;
  for i := 0 to High(data.Packages) do
  begin
    fileName := data.Packages[i].FileName;
    if FindFirst(fileName + '*.exe', 0, f) = 0 then
      Result := True;
    System.SysUtils.FindClose(f);
  end;
end;

end.
