unit Keyman.System.UpdateCheckStorage;

interface

uses
  Keyman.System.UpdateCheckResponse;

type
  TUpdateCheckStorage = class sealed
  private
    class function MetadataFilename: string; static;
  public
    class function LoadUpdateCacheData(var data: TUpdateCheckResponse): Boolean; static;
    class procedure SaveUpdateCacheData(const data: TUpdateCheckResponse); static;
    class function HasKeyboardPackages(const data: TUpdateCheckResponse): Boolean; static;

    (** In most cases the function `HasKeymanInstallFileUpdate` will be required.
        @return  True  if there is keyman installer file in the metadata cache file *)
    class function HasKeymanInstallFile(const data: TUpdateCheckResponse): Boolean; static;

    (** @return  True   if there is keyman installer file in the metadata cache file and it is newer
                        then the current installed version. `TUpdateCheckResponse.DoParse`
                        checks this and sets `ucrsUpdateReady` flag.
    *)
    class function HasKeymanInstallFileUpdate(const data: TUpdateCheckResponse): Boolean; static;

    (** @return  True   if there is at least one keyboard package or a newer Keyman installer file in the Metadata
                        cache file.
                        *)
    class function CheckMetaDataForUpdate: Boolean; static;

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

class function TUpdateCheckStorage.CheckMetaDataForUpdate: Boolean;
var
  ucr: TUpdateCheckResponse;
begin
  Result := TUpdateCheckStorage.LoadUpdateCacheData(ucr) and
            (TUpdateCheckStorage.HasKeyboardPackages(ucr) or
             TUpdateCheckStorage.HasKeymanInstallFileUpdate(ucr));
end;

class function TUpdateCheckStorage.LoadUpdateCacheData(var data: TUpdateCheckResponse): Boolean;
begin
  Result :=
    FileExists(MetadataFilename) and
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

class function TUpdateCheckStorage.HasKeymanInstallFileUpdate(const data: TUpdateCheckResponse): Boolean;
begin
  Result := HasKeymanInstallFile(data) and (data.Status = ucrsUpdateReady);
end;

end.
