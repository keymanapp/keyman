unit Keyman.Configuration.System.HttpServer.App.ConfigMain;

interface

uses
  System.Types,

  Keyman.Configuration.System.HttpServer.App,
  OnlineUpdateCheck; // TODO refactor dependency chain so data and code are separate units

type
  IConfigMainSharedData = interface
    ['{69D029C0-4537-4CBE-B525-C34B0E809820}']
    function HTML: string;
    function BitmapPath: string;
    function Files: TStringDynArray;
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
    function HTML: string;
    function BitmapPath: string;
    function Files: TStringDynArray;
  end;

  TConfigMainHttpResponder = class(TAppHttpResponder)
  private
    procedure ProcessBitmap(data: IConfigMainSharedData; const document: string);
    procedure ProcessPage(data: IConfigMainSharedData);
  public
    procedure ProcessRequest; override;
  end;

implementation

uses
  System.Classes,
  System.Contnrs,
  System.StrUtils,
  System.SysUtils;

const
  Path_Page = '/page/keyman';
  Path_Bitmap = '/data/keyman/bitmap/';

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

initialization
  TConfigMainHttpResponder.Register(Path_Page, TConfigMainHttpResponder);
  TConfigMainHttpResponder.Register(Path_Bitmap, TConfigMainHttpResponder);
end.
