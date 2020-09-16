unit Keyman.Configuration.System.HttpServer.App.InstallKeyboard;

interface

uses
  System.Types,

  Keyman.Configuration.System.HttpServer.App;

type
  // To do: consider moving model interface/class into a separate unit
  IInstallKeyboardSharedData = interface
    ['{FA6F464A-2A4D-429F-BC2E-DBFF283D6316}']
    function XML: string;
    function BitmapPath: string;
    function PackagePath: string;
    function Files: TStringDynArray;
  end;

  TInstallKeyboardSharedData = class(TInterfacedObject, IInstallKeyboardSharedData)
  private
    FXML, FBitmapPath, FPackagePath: string;
    FFiles: TStringDynArray;
  public
    constructor Create(const AXML, ABitmapPath, APackagePath: string; const AFiles: TStringDynArray);
    function XML: string;
    function Files: TStringDynArray;
    function BitmapPath: string;
    function PackagePath: string;
  end;

  TInstallKeyboardHttpResponder = class(TAppHttpResponder)
  private
    procedure ProcessBitmap(data: IInstallKeyboardSharedData; const document: string);
    procedure ProcessPackageFile(data: IInstallKeyboardSharedData; const document: string);
    procedure ProcessPage(data: IInstallKeyboardSharedData);
  public
    procedure ProcessRequest; override;
  end;


implementation

uses
  System.Classes,
  System.Contnrs,
  System.SysUtils,

  GenericXMLRenderer;

{ TInstallKeyboardSharedData }

const
  Path_Page = '/page/installkeyboard';
  Path_PackageFile = '/data/installkeyboard/package/';
  Path_Bitmap = '/data/installkeyboard/bitmap/';

function TInstallKeyboardSharedData.BitmapPath: string;
begin
  Result := FBitmapPath;
end;

constructor TInstallKeyboardSharedData.Create(const AXML, ABitmapPath, APackagePath: string;
  const AFiles: TStringDynArray);
begin
  inherited Create;
  FXML := AXML;
  FBitmapPath := ABitmapPath;
  FPackagePath := APackagePath;
  FFiles := AFiles;
end;

function TInstallKeyboardSharedData.Files: TStringDynArray;
begin
  Result := FFiles;
end;

function TInstallKeyboardSharedData.PackagePath: string;
begin
  Result := FPackagePath;
end;

function TInstallKeyboardSharedData.XML: string;
begin
  Result := FXML;
end;

{ TInstallKeyboardHttpResponder }

procedure TInstallKeyboardHttpResponder.ProcessPackageFile(data: IInstallKeyboardSharedData; const document: string);
begin
  if not IncludesParentFolderReference(document) then
  begin
    RespondFile(data.PackagePath + document, Context, RequestInfo, ResponseInfo);
    Exit;
  end;
  Respond404(Context, RequestInfo, ResponseInfo)
end;

procedure TInstallKeyboardHttpResponder.ProcessBitmap(data: IInstallKeyboardSharedData; const document: string);
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

procedure TInstallKeyboardHttpResponder.ProcessPage(data: IInstallKeyboardSharedData);
begin
  XMLRenderers.RenderTemplate := 'installkeyboard.xsl';
  XMLRenderers.Clear;
  XMLRenderers.Add(TGenericXMLRenderer.Create(XMLRenderers, data.XML));
  ProcessXMLPage;
end;

procedure TInstallKeyboardHttpResponder.ProcessRequest;
var
  data: IInstallKeyboardSharedData;
begin
  if not GetTaggedData(IInstallKeyboardSharedData, data) then
  begin
    Respond404(Context, RequestInfo, ResponseInfo);
    Exit;
  end;

  if RequestInfo.Document.StartsWith(Path_PackageFile) then
  begin
    ProcessPackageFile(data, RequestInfo.Document.Substring(Path_PackageFile.Length));
  end
  else if RequestInfo.Document.StartsWith(Path_Bitmap) then
  begin
    ProcessBitmap(data, RequestInfo.Document.Substring(Path_Bitmap.Length));
  end
  else if RequestInfo.Document = Path_Page then
  begin
    ProcessPage(data);
  end
  else
  begin
    Respond404(Context, RequestInfo, ResponseInfo);
    Exit;
  end;
end;

initialization
  TInstallKeyboardHttpResponder.Register(Path_Page, TInstallKeyboardHttpResponder);
  TInstallKeyboardHttpResponder.Register(Path_PackageFile, TInstallKeyboardHttpResponder);
  TInstallKeyboardHttpResponder.Register(Path_Bitmap, TInstallKeyboardHttpResponder);
end.
