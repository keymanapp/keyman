unit Keyman.Developer.System.Project.WelcomeRenderer;

interface

type
  TWelcomeRenderer = class
    class function Render: string;
    class function ProjectMRUFilename: string;
  end;

implementation

uses
  System.SysUtils,
  Winapi.msxml,
  Winapi.Shlobj,
  Xml.Win.msxmldom,

  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.UrlRenderer,
  RegistryKeys,
  VersionInfo,
  utilsystem;

class function TWelcomeRenderer.ProjectMRUFilename: string;
begin
  Result := GetFolderPath(CSIDL_APPDATA) + SFolderKeymanDeveloper + 'project_mru.xml';
end;

class function TWelcomeRenderer.Render: string;
var
  node, root: IXMLDOMElement;
  doc, xsl: IXMLDomDocument;
  FLastDir: string;
begin
  FLastDir := GetCurrentDir;
  SetCurrentDir(TProject.StringsTemplatePath);
  try
    doc := MSXMLDOMDocumentFactory.CreateDOMDocument;
    try
      doc.async := False;
      if FileExists(ProjectMRUFilename) then
      begin
        doc.load(ProjectMRUFilename);
        root := doc.createElement('Data');
        node := doc.documentElement;
        doc.documentElement := root;
        root.appendChild(node);
      end
      else
      begin
        doc.loadXML('<Data><MRU /></Data>');
        root := doc.documentElement;
      end;

      node := doc.createElement('Version');
      node.appendChild(doc.createTextNode(GetVersionString));
      root.appendChild(node);

      TProjectUrlRenderer.AddUrls(root);

      xsl := MSXMLDOMDocumentFactory.CreateDOMDocument;
      try
        xsl.async := False;
        xsl.resolveExternals := True;
        xsl.validateOnParse := False;
        xsl.load(TProject.StringsTemplatePath + 'globalwelcome.xsl');

        Result := doc.transformNode(xsl);
      finally
        xsl := nil;
      end;
    finally
      doc := nil;
    end;
  finally
    SetCurrentDir(FLastDir);
  end;
end;

end.
