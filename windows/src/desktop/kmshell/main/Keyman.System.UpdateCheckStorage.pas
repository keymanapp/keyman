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

end.
