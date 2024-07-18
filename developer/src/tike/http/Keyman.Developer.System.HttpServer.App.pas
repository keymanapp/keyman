unit Keyman.Developer.System.HttpServer.App;

interface

uses
  IdContext,
  IdCustomHTTPServer,
  IdHTTPServer,

  Keyman.System.HttpServer.Base,

  utilfiletypes;

// /app

type
  TAppHttpResponder = class(TBaseHttpResponder)
  private
    FAppRoot: string;
    FStandardTemplatePath: string;
    procedure RespondProject(doc: string; AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure RespondHelp(doc: string; AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure RespondSettings(doc: string; AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  public
    constructor Create;
    procedure ProcessRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  end;

implementation

uses
  System.Classes,
  System.JSON,
  System.SysUtils,
  System.Variants,
  Winapi.Windows,
  Xml.XMLDoc,
  Xml.XMLIntf,

  JsonUtil,
  KeymanDeveloperOptions,
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectLoader,
  Keyman.Developer.System.Project.WelcomeRenderer,
  RedistFiles;

{ TAppHttpServer }

procedure TAppHttpResponder.RespondHelp(doc: string; AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  xml, xsl: IXMLDocument;
  s: WideString;
begin

  if FStandardTemplatePath = '' then
  begin
    FStandardTemplatePath := GetXMLTemplatePath + 'help\';
  end;

  if (doc <> 'help/') and (doc <> 'help/index') then
  begin
    Respond404(AContext, ARequestInfo, AResponseInfo);
    Exit;
  end;

  xml := TXMLDocument.Create(nil);
  xml.ParseOptions := [poResolveExternals];
  // I902 - resolve externals when loading XML files so locale.xml parses
  xml.LoadFromFile(FStandardTemplatePath + 'contexthelp.xml');

  xsl := TXMLDocument.Create(nil);
  // xml.ParseOptions := [poResolveExternals];  // I902 - resolve externals when loading XML files so locale.xml parses

  xsl.LoadFromFile(FStandardTemplatePath + 'help.xsl');
  xml.Node.transformNode(xsl.Node, s);

  AResponseInfo.ContentType := 'text/html; charset=UTF-8';
  AResponseInfo.ContentText := s;
end;

procedure TAppHttpResponder.RespondProject(doc: string; AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

  procedure RespondWelcome;
  begin
    AResponseInfo.ContentType := 'text/html; charset=UTF-8';
    AResponseInfo.ContentText := TWelcomeRenderer.Render;
  end;

  procedure RespondProjectFile;
  var
    path: string;
    p: TProject;
  begin
    if ARequestInfo.Params.IndexOfName('path') < 0 then
    begin
      AResponseInfo.ResponseNo := 400;
      AResponseInfo.ResponseText := 'Missing parameter path';
      Exit;
    end;

    path := CrackUTF8ZeroExtendedString(ARequestInfo.CommandType, ARequestInfo.Params.Values['path']);

    if (Path <> '') and (not DirectoryExists(ExtractFileDir(path)) or not SameText(ExtractFileExt(path), Ext_ProjectSource)) then
    begin
      AResponseInfo.ResponseNo := 404;
      AResponseInfo.ResponseText := 'Project file '+path+' does not exist.';
      Exit;
    end;

    // Transform the .kpj
    try
      p := TProject.Create(ptUnknown, path, True);
    except
      on E:EProjectLoader do
      begin
        AResponseInfo.ResponseNo := 400;
        AResponseInfo.ResponseText := 'Invalid project file: '+E.Message;
        Exit;
      end;
    end;
    try
      AResponseInfo.ContentType := 'text/html; charset=UTF-8';
      AResponseInfo.ContentText := p.Render;
    finally
      p.Free;
    end;
  end;

  procedure RespondIco;
  var
    path: string;
  begin
    if ARequestInfo.Params.IndexOfName('path') < 0 then
    begin
      AResponseInfo.ResponseNo := 400;
      AResponseInfo.ResponseText := 'Missing parameter path';
      Exit;
    end;

    path := CrackUTF8ZeroExtendedString(ARequestInfo.CommandType, ARequestInfo.Params.Values['path']);

    if not FileExists(path) or (
      not SameText(ExtractFileExt(path), '.ico') and
      not SameText(ExtractFileExt(path), '.bmp')
      ) then
    begin
      AResponseInfo.ResponseNo := 404;
      AResponseInfo.ResponseText := 'Image file '+path+' does not exist.';
      Exit;
    end;

    RespondFile(path, AContext, ARequestInfo, AResponseInfo);
  end;

  procedure RespondResource;
  begin
    Delete(doc, 1, 4);
    if (Pos('/', doc) > 0) or (Pos('\', doc) > 0) then
      Respond404(AContext, ARequestInfo, AResponseInfo)
    else
      RespondFile(TProject.StandardTemplatePath + '/' + doc, AContext, ARequestInfo, AResponseInfo);
  end;

  procedure RespondState;
  var
    displayState, path: string;
    xmldoc: IXMLDocument;
    FNewDisplayState: PChar;
    FNewPath: PChar;
  begin
    with TStringStream.Create('', TEncoding.UTF8) do
    try
      CopyFrom(ARequestInfo.PostStream, 0);
      displayState := DataString;
    finally
      Free;
    end;

    try
      xmldoc := LoadXMLData(displayState);
    except
      on E:Exception do
      begin
        Respond404(AContext, ARequestInfo, AResponseInfo);
        Exit;
      end;
    end;

    try
      if (xmldoc.DocumentElement.ChildNodes.IndexOf('path') >= 0) and not
        VarIsNull(xmldoc.DocumentElement.ChildValues['path']) then
        path := xmldoc.DocumentElement.ChildValues['path'];
    except
      path := '';
    end;

    // Saving state

    if (Path <> '') and (not DirectoryExists(ExtractFileDir(path)) or not SameText(ExtractFileExt(path), Ext_ProjectSource)) then
    begin
      AResponseInfo.ResponseNo := 404;
      AResponseInfo.ResponseText := 'Project file '+path+' does not exist.';
      Exit;
    end;

    if GlobalProjectStateWndHandle <> 0 then
    begin
      // Potential for a race condition here as the window
      // may be destroyed before we post to it. Can only happen at
      // process destruction time and really not worth worrying about.
      FNewDisplayState := StrNew(PChar(displayState));
      FNewPath := StrNew(PChar(Path));
      PostMessage(GlobalProjectStateWndHandle, WM_USER_ProjectUpdateDisplayState, NativeInt(FNewPath), NativeInt(FNewDisplayState));
    end;
  end;
begin
  //
  // http://localhost:8008/app/project/(index)?path=<fqp-to-.kpj>
  // http://localhost:8008/app/project/<resource>
  //
  Delete(doc, 1, 8);
  if (doc = '') or (doc = 'index') then
    RespondProjectFile
  else if doc = 'welcome' then
    RespondWelcome
  else if doc = 'ico' then
    RespondIco
  else if Copy(doc, 1, 4) = 'res/' then
    RespondResource
  else if doc = 'state' then
    RespondState;
end;

procedure TAppHttpResponder.RespondSettings(doc: string; AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

  procedure RespondEditorSettings;
  var
    root: TJSONObject;
    offset: Integer;
    theme: TJSONObject;
  begin
    // Respond as JSON

    root := TJSONObject.Create;
    try
      // Basic settings - tabs

      // TODO: FKeymanDeveloperOptins is technically not thread safe.
      root.AddPair('useTabChar', TJSONBool.Create(FKeymanDeveloperOptions.UseTabChar));
      root.AddPair('indentSize', TJSONNumber.Create(FKeymanDeveloperOptions.IndentSize));

      // Specify a theme by name or as a JSON object, loaded from a custom theme file

      if FKeymanDeveloperOptions.EditorTheme <> '' then
      begin
        if TKeymanDeveloperOptions.IsDefaultEditorTheme(FKeymanDeveloperOptions.EditorTheme) then
        begin
          root.AddPair('theme', FKeymanDeveloperOptions.EditorTheme);
        end
        else
        begin
          if FileExists(FKeymanDeveloperOptions.EditorTheme) then
          begin
            try
              theme := LoadJSONFromFile(FKeymanDeveloperOptions.EditorTheme, offset);
              if theme <> nil then
                root.AddPair('theme', theme);
            except
              on E:Exception do ; // We will ignore file and json errors and use default theme
            end;
          end;
        end;
      end;

      AResponseInfo.ContentType := 'application/json';
      AResponseInfo.CharSet := 'UTF-8';
      AResponseInfo.ContentText := JSONToString(root);
    finally
      root.Free;
    end;
  end;

begin
  if doc = 'settings/editor' then
  begin
    RespondEditorSettings;
  end
  else
    Respond404(AContext, ARequestInfo, AResponseInfo);
end;

constructor TAppHttpResponder.Create;
begin
  FAppRoot := GetXMLTemplatePath + 'app\';
end;

procedure TAppHttpResponder.ProcessRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  doc: string;
begin
  // We know the request starts with /app/
  doc := ARequestInfo.Document;
  Delete(doc, 1, 5);  // /app/

  // App requests are only accepted from local computer
  // So test that the request comes from localhost
  if (AContext.Binding.PeerIP <> '127.0.0.1') and
    (AContext.Binding.PeerIP <> '0:0:0:0:0:0:0:1') and
    not doc.StartsWith('lib/sentry/') then  // We'll allow sentry for non-local requests
  begin
    AResponseInfo.ResponseNo := 403;
    AResponseInfo.ResponseText := 'Access denied';
    Exit;
  end;

  if Copy(doc, 1, 8) = 'project/' then
  begin
    RespondProject(doc, AContext, ARequestInfo, AResponseInfo);
    Exit;
  end
  else if Copy(doc, 1, 5) = 'help/' then
  begin
    RespondHelp(doc, AContext, ARequestInfo, AResponseInfo);
    Exit;
  end
  else if Copy(doc, 1, 9) = 'settings/' then
  begin
    RespondSettings(doc, AContext, ARequestInfo, AResponseInfo);
    Exit;
  end
  else
  begin
    if IncludesParentFolderReference(doc) then
    begin
      // Block paths that attempt to break out of our 'root'
      Respond404(AContext, ARequestInfo, AResponseInfo);
    end
    else
      RespondFile(FAppRoot + doc, AContext, ARequestInfo, AResponseInfo);
  end;

  Respond404(AContext, ARequestInfo, AResponseInfo);
end;

end.
