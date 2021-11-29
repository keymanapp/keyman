unit Keyman.Developer.System.ValidateKpsFile;

interface

uses
  Keyman.Developer.System.Project.ProjectLog;

type
  TValidateKpsFile = class
  private
    FOnLogEvent: TProjectLogObjectEvent;
    FKpsFile: string;
    FKpsXsdPath: string;
    constructor Create(const AKpsFile, AKpsXsdPath: string);
    function Validate: Boolean;
    property OnLogEvent: TProjectLogObjectEvent read FOnLogEvent write FOnLogEvent;
  public
    class function Execute(const KpsFile, KpsXsdPath: string; FCallback: TProjectLogObjectEvent): Boolean;
  end;

implementation

uses
  Winapi.MsXml;

{ TValidateKpsFile }

constructor TValidateKpsFile.Create(const AKpsFile, AKpsXsdPath: string);
begin
  inherited Create;
  FKpsFile := AKpsFile;
  FKpsXsdPath := AKpsXsdPath;
end;

class function TValidateKpsFile.Execute(const KpsFile, KpsXsdPath: string;
  FCallback: TProjectLogObjectEvent): Boolean;
var
  v: TValidateKpsFile;
begin
  v := TValidateKpsFile.Create(KpsFile, KpsXsdPath);
  try
    v.OnLogEvent := FCallback;
    Result := v.Validate;
  finally
    v.Free;
  end;
end;

function TValidateKpsFile.Validate: Boolean;
var
  xml, xsd: IXMLDOMDocument2;
  cache: IXMLDOMSchemaCollection;
begin
  xsd := CoDOMDocument60.Create;
  xsd.Async := False;
  xsd.load(FKpsXsdPath);

  cache := CoXMLSchemaCache60.Create;
  cache.add('', xsd);

  xml := CoDOMDocument60.Create;
  xml.async := False;
  xml.schemas := cache;

  Result := xml.load(FKpsFile);
  if not Result then
    FOnLogEvent(plsError, FKpsFile, 'Validation error: '+xml.parseError.reason, 1, xml.parseError.line);
end;

end.
