{**
 * Wrapper for checking api.keyman.com for latest version of resources to be
 * installed. Uses the /windows/14.0/update endpoints.
 *}
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
  System.SysUtils,

  GlobalProxySettings,
  httpuploader,
  Keyman.System.UpdateCheckResponse,
  KeymanVersion,
  PackageInfo,
  Upload_Settings,
  versioninfo;

class procedure TOnlineResourceCheck.QueryServer(ASilent: Boolean; AInstallInfo: TInstallInfo);
var
  http: THTTPUploader;
  pack: TInstallInfoPackage;
  ucr: TUpdateCheckResponse;
  ucrpack: TUpdateCheckResponsePackage;
  location: TInstallInfoFileLocation;
  packLocation: TInstallInfoPackageFileLocation;
  currentVersion: string;
  lang: TUpdateCheckResponseLanguage;
  iipl: TInstallInfoPackageLanguage;
  u: AnsiString;
begin
  currentVersion := AInstallInfo.MsiLocations.LatestVersion(SKeymanVersion_Min_Evergreen);

  http := THttpUploader.Create(nil);
  try
    http.ShowUI := not ASilent;
    http.Request.SetURL(MakeAPIURL(API_Path_UpdateCheck_Windows));
    http.Fields.Add('version', AnsiString(currentVersion));
    http.Fields.Add('tier', AnsiString(AInstallInfo.Tier));
    http.Fields.Add('update', '0'); // This is probably a fresh install of a package, not an update
    for pack in AInstallInfo.Packages do
      http.Fields.Add(AnsiString('package_'+pack.ID), AnsiString(pack.Locations.LatestVersion));

    http.Upload;
    if http.Response.StatusCode <> 200 then
    begin
      // TODO: log failed response
      //raise EOnlineUpdateCheck.Create('Error '+IntToStr(Response.StatusCode));
      Exit;
    end;

    u := http.response.PMessageBody;
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
