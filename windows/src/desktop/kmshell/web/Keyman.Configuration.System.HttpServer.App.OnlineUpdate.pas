unit Keyman.Configuration.System.HttpServer.App.OnlineUpdate;

interface

uses
  Keyman.Configuration.System.HttpServer.App,
  OnlineUpdateCheck; // TODO refactor dependency chain so data and code are separate units

type
  TOnlineUpdateHttpResponder = class(TAppHttpResponder)
  public
    procedure ProcessRequest; override;
  end;

implementation

uses
  System.Classes,
  System.Contnrs,
  System.StrUtils,
  System.SysUtils,

  GenericXMLRenderer,
  Keyman.System.LocaleStrings,
  MessageIdentifierConsts,
  utilxml;

{ TOnlineUpdateHttpResponder }

procedure TOnlineUpdateHttpResponder.ProcessRequest;
var
  xml: string;
  i: Integer;
  data: IOnlineUpdateSharedData;
begin
  if not GetTaggedData(IOnlineUpdateSharedData, data) then
  begin
    Respond404(Context, RequestInfo, ResponseInfo);
    Exit;
  end;

  xml := '';

  if (data.Params.Keyman.DownloadURL <> '') then
  begin
    xml := xml +
      '<Update>'+
        '<index>0</index>'+
        IfThen(not XMLRenderers.kmcom.SystemInfo.IsAdministrator, '<RequiresAdmin />')+
        '<Keyman>'+
          '<Text>'+xmlencode(TLocaleStrings.MsgFromIdFormat(XMLRenderers.kmcom, SKUpdate_KeymanText, [data.Params.Keyman.NewVersion]))+'</Text>'+
          '<NewVersion>'+xmlencode(data.Params.Keyman.NewVersion)+'</NewVersion>'+
          '<OldVersion>'+xmlencode(data.Params.Keyman.OldVersion)+'</OldVersion>'+
          '<DownloadSize>'+xmlencode(Format('%d', [data.Params.Keyman.DownloadSize div 1024]))+'KB</DownloadSize>'+
          '<DownloadURL>'+xmlencode(data.Params.Keyman.DownloadURL)+'</DownloadURL>'+
        '</Keyman>'+
      '</Update>';
  end;

  for i := 0 to High(data.Params.Packages) do
  begin
    xml := xml +
      '<Update>'+
        '<index>'+IntToStr(i+1)+'</index>'+
        IfThen(not XMLRenderers.kmcom.SystemInfo.IsAdministrator, '<RequiresAdmin />')+
        '<Package>'+
          '<Text>'+xmlencode(TLocaleStrings.MsgFromIdFormat(XMLRenderers.kmcom, SKUpdate_PackageText, [data.Params.Packages[i].Description, data.Params.Packages[i].NewVersion]))+'</Text>'+
          '<NewVersion>'+xmlencode(data.Params.Packages[i].NewVersion)+'</NewVersion>'+
          '<OldVersion>'+xmlencode(data.Params.Packages[i].OldVersion)+'</OldVersion>'+
          '<DownloadSize>'+xmlencode(Format('%d', [data.Params.Packages[i].DownloadSize div 1024]))+'KB</DownloadSize>'+
          '<DownloadURL>'+xmlencode(data.Params.Packages[i].DownloadURL)+'</DownloadURL>'+
        '</Package>'+
      '</Update>';
  end;

  XMLRenderers.RenderTemplate := 'onlineupdate.xsl';
  XMLRenderers.Clear;
  XMLRenderers.Add(TGenericXMLRenderer.Create(XMLRenderers, xml));
  ProcessXMLPage;
end;

initialization
  TOnlineUpdateHttpResponder.Register('/page/onlineupdate', TOnlineUpdateHttpResponder);
end.
