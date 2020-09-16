unit Keyman.Configuration.System.HttpServer.App.ProxyConfiguration;

interface

uses
  System.Types,

  Keyman.Configuration.System.HttpServer.App;

type
  TProxyConfigurationHttpResponder = class(TAppHttpResponder)
  public
    procedure ProcessRequest; override;
  end;

implementation

uses
  System.Classes,
  System.Contnrs,
  System.SysUtils,

  GenericXMLRenderer,
  GlobalProxySettings,
  utilxml;

const
  Path_Page = '/page/proxyconfiguration';


{ TProxyConfigurationHttpResponder }

procedure TProxyConfigurationHttpResponder.ProcessRequest;
var
  FPassword, xml: string;
  ps: TProxySettings;
begin
  ps := TProxySettings.Create;
  try
    if ps.Password <> ''
      then FPassword := ProxySettingsStandinPassword
      else FPassword := '';

    xml :=
      '<Proxy>'+
        '<Server>'+XMLEncode(ps.Server)+'</Server>'+
        '<Port>'+IntToStr(ps.Port)+'</Port>'+
        '<Username>'+XMLEncode(ps.Username)+'</Username>'+
        '<Password>'+XMLEncode(FPassword)+'</Password>'+
      '</Proxy>';
  finally
    ps.Free;
  end;

  XMLRenderers.RenderTemplate := 'proxyconfiguration.xsl';
  XMLRenderers.Clear;
  XMLRenderers.Add(TGenericXMLRenderer.Create(XMLRenderers, xml));
  ProcessXMLPage;
end;

initialization
  TProxyConfigurationHttpResponder.Register(Path_Page, TProxyConfigurationHttpResponder);
end.
