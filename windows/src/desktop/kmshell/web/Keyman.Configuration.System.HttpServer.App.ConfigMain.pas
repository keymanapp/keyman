unit Keyman.Configuration.System.HttpServer.App.ConfigMain;

interface

uses
  System.Types,

  IdCustomHttpServer,

  Keyman.Configuration.System.HttpServer.App,
  OnlineUpdateCheck; // TODO refactor dependency chain so data and code are separate units

type
  IConfigMainSharedData = interface
    ['{69D029C0-4537-4CBE-B525-C34B0E809820}']
    function HTML: string;
    function BitmapPath: string;
    function Files: TStringDynArray;
    function GetState: string;
    procedure SetState(const Value: string);
    property State: string read GetState write SetState;
  end;

  TConfigMainSharedData = class(TInterfacedObject, IConfigMainSharedData)
  private
    FBitmapPath: string;
    FHTML: string;
    FFiles: TStringDynArray;
  public
    constructor Create(const ABitmapPath: string);
    procedure SetFiles(const AFiles: TStringDynArray);
    procedure SetHTML(const AHTML: string);

    function GetState: string;
    procedure SetState(const Value: string);

    function HTML: string;
    function BitmapPath: string;
    function Files: TStringDynArray;
  end;

  TConfigMainHttpResponder = class(TAppHttpResponder)
  private
    procedure ProcessBitmap(data: IConfigMainSharedData; const document: string);
    procedure ProcessPage(data: IConfigMainSharedData);
    procedure ProcessState(data: IConfigMainSharedData);
  public
    procedure ProcessRequest; override;
  end;

implementation

uses
  System.Classes,
  System.Contnrs,
  System.StrUtils,
  System.SysUtils,

  ErrorControlledRegistry,
  RegistryKeys;

const
  Path_Page = '/page/keyman';
  Path_Bitmap = '/data/keyman/bitmap/';
  Path_State = '/data/keyman/state';

{ TOnlineUpdateHttpResponder }

procedure TConfigMainHttpResponder.ProcessPage(data: IConfigMainSharedData);
begin
  // TODO: The render is currently done in the main form.

  // PageTag := RequestInfo.Params.Values['tag'];

  ResponseInfo.ContentStream := TStringStream.Create(data.HTML, TEncoding.UTF8);
  ResponseInfo.FreeContentStream := True;
  ResponseInfo.ContentStream.Position := 0;
  ResponseInfo.CharSet := 'UTF-8';
  ResponseInfo.ContentType := 'text/html; charset=UTF-8';
end;

procedure TConfigMainHttpResponder.ProcessBitmap(data: IConfigMainSharedData; const document: string);
var
  name: string;
begin
  if not IncludesParentFolderReference(document) then
  begin
    for name in data.Files do
    begin
      if name = document then
      begin
        RespondFile(data.BitmapPath + document, Context, RequestInfo, ResponseInfo);
        Exit;
      end;
    end;
  end;
  Respond404(Context, RequestInfo, ResponseInfo);
end;

procedure TConfigMainHttpResponder.ProcessState(data: IConfigMainSharedData);
begin
  if RequestInfo.CommandType = hcGET then
  begin
    ResponseInfo.ContentType := 'application/json; charset=utf-8';
    ResponseInfo.ContentText := data.State;
  end
  else if RequestInfo.CommandType = hcPOST then
  begin
    data.State := RequestInfo.Params.Values['state'];
    ResponseInfo.ContentType := 'application/json; charset=utf-8';
    ResponseInfo.ContentText := data.State;
  end
  else
    Respond404(Context, RequestInfo, ResponseInfo);
end;

procedure TConfigMainHttpResponder.ProcessRequest;
var
  data: IConfigMainSharedData;
begin
  if not GetTaggedData(IConfigMainSharedData, data) then
  begin
    Respond404(Context, RequestInfo, ResponseInfo);
    Exit;
  end;

  if RequestInfo.Document.StartsWith(Path_Bitmap) then
    ProcessBitmap(data, RequestInfo.Document.Substring(Path_Bitmap.Length))
  else if RequestInfo.Document = Path_Page then
    ProcessPage(data)
  else if RequestInfo.Document = Path_State then
    ProcessState(data)
  else
    Respond404(Context, RequestInfo, ResponseInfo);
end;

{ TConfigMainSharedData }

function TConfigMainSharedData.BitmapPath: string;
begin
  Result := FBitmapPath;
end;

constructor TConfigMainSharedData.Create(const ABitmapPath: string);
begin
  inherited Create;
  FBitmapPath := ABitmapPath;
end;

function TConfigMainSharedData.Files: TStringDynArray;
begin
  Result := FFiles;
end;

function TConfigMainSharedData.HTML: string;
begin
  Result := FHTML;
end;

procedure TConfigMainSharedData.SetFiles(const AFiles: TStringDynArray);
begin
  FFiles := AFiles;
end;

procedure TConfigMainSharedData.SetHTML(const AHTML: string);
begin
  FHTML := AHTML;
end;

function TConfigMainSharedData.GetState: string;
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKeyReadOnly(SRegKey_KeymanDesktop_CU) and ValueExists(SRegValue_ConfigurationState)
      then Result := ReadString(SRegValue_ConfigurationState)
      else Result := '0';
  finally
    Free;
  end;
end;

procedure TConfigMainSharedData.SetState(const Value: string);
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKey(SRegKey_KeymanDesktop_CU, True) then
      WriteString(SRegValue_ConfigurationState, Value);
  finally
    Free;
  end;
end;

initialization
  TConfigMainHttpResponder.Register(Path_Page, TConfigMainHttpResponder);
  TConfigMainHttpResponder.Register(Path_Bitmap, TConfigMainHttpResponder);
  TConfigMainHttpResponder.Register(Path_State, TConfigMainHttpResponder);
end.
