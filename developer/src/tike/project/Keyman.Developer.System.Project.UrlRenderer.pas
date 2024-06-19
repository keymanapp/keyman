unit Keyman.Developer.System.Project.UrlRenderer;

interface

uses
  Winapi.msxml;

type
  TProjectUrlRenderer = class
    class procedure AddUrls(root: IXMLDomNode);
    class procedure AddProcessState(root: IXMLDomNode);
  end;

implementation

uses
  KeymanDeveloperOptions,
  Upload_Settings;

{ TProjectUrlRenderer }

class procedure TProjectUrlRenderer.AddProcessState(root: IXMLDomNode);
var
  node: IXMLDomElement;
begin
  node := root.ownerDocument.createElement('DeveloperState');
  node.setAttribute('promptToUpgradeProjects', FKeymanDeveloperOptions.PromptToUpgradeProjects);
  root.appendChild(node);
end;

class procedure TProjectUrlRenderer.AddUrls(root: IXMLDomNode);
var
  node: IXMLDomNode;

  procedure AddUrl(const id, href: string);
  var
    urlNode: IXMLDOMElement;
  begin
    urlNode := root.ownerDocument.createElement('Url');
    urlNode.setAttribute('id', id);
    urlNode.setAttribute('href', MakeKeymanURL(href));
    node.appendChild(urlNode);
  end;
begin
  node := root.ownerDocument.createElement('DeveloperUrls');
  AddUrl('help-keyboards', URLPath_KeymanDeveloper_HelpKeyboards);
  AddUrl('help-packages', URLPath_KeymanDeveloper_HelpPackages);
  AddUrl('help-mobile', URLPath_KeymanDeveloper_HelpMobile);
  AddUrl('keymanweb', URLPath_KeymanDeveloper_KeymanWeb);
  AddUrl('keyman-engine-home', URLPath_KeymanDeveloper_KeymanEngineHome);
  AddUrl('home', URLPath_KeymanDeveloperHome);
  AddUrl('home-presentation', URLPath_KeymanDeveloperHome_Presentation);
  root.appendChild(node);
end;

end.
