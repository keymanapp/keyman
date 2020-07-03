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
begin
  currentVersion := AInstallInfo.MsiLocations.LatestVersion(SKeymanVersion_Min_Evergreen);

  http := THTTPUploader.Create(nil);
  try
    http.ShowUI := not ASilent;
    http.Fields.Add('version', ansistring(currentVersion));
    // TODO: allow override of this tier with a command line parameter or filename rename
    http.Fields.Add('tier', ansistring(KeymanVersion.CKeymanVersionInfo.Tier));
    for pack in AInstallInfo.Packages do
      http.Fields.Add(ansistring('package_'+pack.ID), ansistring(pack.Locations.LatestVersion));

    http.Proxy.Server := GetProxySettings.Server;
    http.Proxy.Port := GetProxySettings.Port;
    http.Proxy.Username := GetProxySettings.Username;
    http.Proxy.Password := GetProxySettings.Password;

    http.Request.HostName := API_Server;
    http.Request.Protocol := API_Protocol;
    http.Request.UrlPath := API_Path_UpdateCheck_Windows;

    http.Upload;
    if http.Response.StatusCode <> 200 then
    begin
      // TODO: log failed response
      //raise EOnlineUpdateCheck.Create('Error '+IntToStr(Response.StatusCode));
      Exit;
    end;

    if ucr.Parse(http.Response.MessageBodyAsString, 'msi', currentVersion) then
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
  finally
    http.Free;
  end;
end;

end.
