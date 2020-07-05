unit Keyman.Setup.System.OnlineResourceCheck;

interface

uses
  Keyman.Setup.System.InstallInfo;

type
  TOnlineResourceCheck = class sealed
    class procedure QueryServer(ASilent: Boolean; AInstallInfo: TInstallInfo); static;
  end;

implementation

uses
  System.Net.HttpClient,
  System.Net.URLClient,
  System.SysUtils,

  GlobalProxySettings,
  Keyman.System.UpdateCheckResponse,
  KeymanVersion,
  PackageInfo,
  Upload_Settings,
  versioninfo;

class procedure TOnlineResourceCheck.QueryServer(ASilent: Boolean; AInstallInfo: TInstallInfo);
var
  http: THTTPClient;
  pack: TInstallInfoPackage;
  ucr: TUpdateCheckResponse;
  ucrpack: TUpdateCheckResponsePackage;
  location: TInstallInfoFileLocation;
  packLocation: TInstallInfoPackageFileLocation;
  currentVersion: string;
  lang: TUpdateCheckResponseLanguage;
  iipl: TInstallInfoPackageLanguage;
  url: TURI;
  response: IHTTPResponse;
  u: AnsiString;
begin
  currentVersion := AInstallInfo.MsiLocations.LatestVersion(SKeymanVersion_Min_Evergreen);

  url := TURI.Create(MakeAPIURL(API_Path_UpdateCheck_Windows));
  url.AddParameter('version', currentVersion);
  url.AddParameter('tier', KeymanVersion.CKeymanVersionInfo.Tier);
  for pack in AInstallInfo.Packages do
    url.AddParameter('package_'+pack.ID, pack.Locations.LatestVersion);
  http := THTTPClient.Create;
  try
    response := http.Get(url.ToString);
    if response.StatusCode <> 200 then
    begin
      // TODO: log failed response
      //raise EOnlineUpdateCheck.Create('Error '+IntToStr(Response.StatusCode));
      Exit;
    end;

    u := UTF8Encode(Response.ContentAsString(TEncoding.UTF8));
  finally
    http.Free;
  end;

  if ucr.Parse(u, 'msi', currentVersion) then
  begin
    for ucrpack in ucr.Packages do
    begin
      pack := AInstallInfo.Packages.FindById(ucrpack.ID, False);
      if Assigned(pack) then
      begin
        packLocation := TInstallInfoPackageFileLocation.Create(iilOnline);
        pack.Locations.Add(packLocation);
        packLocation.Name := ucrpack.Name;
        packLocation.Path := ucrpack.FileName;
        packLocation.URL := ucrpack.DownloadURL;
        packLocation.Version := ucrpack.NewVersion;
        packLocation.Size := ucrpack.DownloadSize;

        for lang in ucrpack.Languages do
        begin
          iipl := TInstallInfoPackageLanguage.Create(lang.ID, lang.displayName);
          packLocation.Languages.Add(iipl);
        end;
      end;
    end;

    if ucr.Status = ucrsUpdateReady then
    begin
      location := TInstallInfoFileLocation.Create(iilOnline);
      location.URL := ucr.InstallURL;
      location.Path := ucr.FileName;
      location.Version := ucr.NewVersion;
      location.Size := ucr.InstallSize;
      AInstallInfo.MsiLocations.Add(location);
    end;
  end;
end;

end.
