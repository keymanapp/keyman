unit Keyman.System.UpdateCheckResponse;

interface

uses
  System.SysUtils;

type
  EUpdateCheckResponse = class(Exception);

  TUpdateCheckResponseStatus = (ucrsNoUpdate, ucrsUpdateReady, ucrsError);

  TUpdateCheckResponse = record
  private
    FInstallSize: Int64;
    FInstallURL: string;
    FNewVersion: string;
    FStatus: TUpdateCheckResponseStatus;
    FErrorMessage: string;
    FCurrentVersion: string;
  public
    function Parse(const message: AnsiString; const app, currentVersion: string): Boolean;

    property CurrentVersion: string read FCurrentVersion;
    property NewVersion: string read FNewVersion;
    property InstallURL: string read FInstallURL;
    property InstallSize: Int64 read FInstallSize;
    property ErrorMessage: string read FErrorMessage;
    property Status: TUpdateCheckResponseStatus read FStatus;
  end;

implementation

uses
  versioninfo,
  System.JSON;

{ TUpdateCheckResponse }

function TUpdateCheckResponse.Parse(const message: AnsiString; const app, currentVersion: string): Boolean;
var
  node, doc: TJSONObject;
begin
  FCurrentVersion := currentVersion;

  // TODO: test with UTF8 characters in response
  doc := TJSONObject.ParseJSONValue(UTF8String(message)) as TJSONObject;
  if doc = nil then
  begin
    FStatus := ucrsError;
    FErrorMessage := Format('Invalid response:'#13#10'%s', [string(message)]);
    Exit(False);
  end;

  if doc.Values[app] is TJSONObject then
  begin
    node := doc.Values[app] as TJSONObject;
    if CompareVersions(node.Values['version'].Value, FCurrentVersion) < 0 then
    begin
      FNewVersion := node.Values['version'].Value;
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
    FStatus := ucrsError;
    FErrorMessage := doc.Values['message'].Value;
  end
  else
    FStatus := ucrsNoUpdate;

  Result := True;
end;

end.
