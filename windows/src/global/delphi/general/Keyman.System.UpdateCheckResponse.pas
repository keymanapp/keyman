unit Keyman.System.UpdateCheckResponse;

interface

uses
  System.JSON,
  System.SysUtils;

type
  EUpdateCheckResponse = class(Exception);

  TUpdateCheckResponseStatus = (ucrsNoUpdate, ucrsUpdateReady);

  TUpdateCheckResponsePackage = record
    ID: string;
    NewID: string;
    Name: string;
    OldVersion, NewVersion: string;
    DownloadURL: string;
    SavePath: string;
    FileName: string;
    DownloadSize: Integer;
    Install: Boolean;
  end;

  TUpdateCheckResponsePackages = TArray<TUpdateCheckResponsePackage>;

  TUpdateCheckResponse = record
  private
    FInstallSize: Int64;
    FInstallURL: string;
    FNewVersion: string;
    FStatus: TUpdateCheckResponseStatus;
    FErrorMessage: string;
    FCurrentVersion: string;
    FPackages: TUpdateCheckResponsePackages;
    FFileName: string;
    function ParseKeyboards(nodes: TJSONObject): Boolean;
  public
    function Parse(const message: AnsiString; const app, currentVersion: string): Boolean;

    property CurrentVersion: string read FCurrentVersion;
    property NewVersion: string read FNewVersion;
    property FileName: string read FFileName;
    property InstallURL: string read FInstallURL;
    property InstallSize: Int64 read FInstallSize;
    property ErrorMessage: string read FErrorMessage;
    property Status: TUpdateCheckResponseStatus read FStatus;
    property Packages: TUpdateCheckResponsePackages read FPackages;
  end;

implementation

uses
  System.Generics.Collections,
  versioninfo;

{ TUpdateCheckResponse }

function TUpdateCheckResponse.Parse(const message: AnsiString; const app, currentVersion: string): Boolean;
var
  node, doc: TJSONObject;
begin
  FCurrentVersion := currentVersion;
  FStatus := ucrsNoUpdate;

  // TODO: test with UTF8 characters in response
  doc := TJSONObject.ParseJSONValue(UTF8String(message)) as TJSONObject;
  if doc = nil then
  begin
    FErrorMessage := Format('Invalid response:'#13#10'%s', [string(message)]);
    Exit(False);
  end;

  if doc.Values[app] is TJSONObject then
  begin
    node := doc.Values[app] as TJSONObject;
    if CompareVersions(node.Values['version'].Value, FCurrentVersion) > 0 then
    begin
      FNewVersion := node.Values['version'].Value;
      FFileName := node.Values['file'].Value;
      FInstallURL := node.Values['url'].Value;
      FInstallSize := (node.Values['size'] as TJSONNumber).AsInt64;
      FStatus := ucrsUpdateReady;
    end
    else
    begin
      FErrorMessage := 'No updates are currently available.';
      FStatus := ucrsNoUpdate;
    end;
  end
  else if doc.Values['message'] <> nil then
  begin
    FErrorMessage := doc.Values['message'].Value;
    Exit(False);
  end;

  if doc.Values['keyboards'] is TJSONObject
    then Result := ParseKeyboards(doc.Values['keyboards'] as TJSONObject)
    else Result := True;
end;

function TUpdateCheckResponse.ParseKeyboards(nodes: TJSONObject): Boolean;
var
  node: TJSONObject;
  i: Integer;
begin
  SetLength(FPackages,nodes.Count);
  for i := 0 to nodes.Count - 1 do
  begin
    node := nodes.Pairs[i].JsonValue as TJSONObject;
    FPackages[i].NewID := node.Values['id'].Value;
    FPackages[i].ID := nodes.Pairs[i].JsonString.Value;
    FPackages[i].Name := node.Values['name'].Value;
    //FPackages[j].OldVersion := pkg.Version;
    FPackages[i].NewVersion := node.Values['version'].Value;
    FPackages[i].DownloadSize := (node.Values['packageFileSize'] as TJSONNumber).AsInt64;
    FPackages[i].DownloadURL := node.Values['url'].Value;
    FPackages[i].FileName := node.Values['packageFilename'].Value;
  end;

  Result := True;
end;

end.
