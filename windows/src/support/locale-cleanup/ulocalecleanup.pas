unit ulocalecleanup;

interface

procedure Run;

implementation

uses
  System.Generics.Collections,
  System.Classes,
  System.SysUtils,
  System.Types,
  System.Variants,
  Winapi.msxml,

  KeymanVersion;

const
  SDefaultInFile = 'c:\keyman\'+SKeymanVersion+'\src\desktop\kmshell\xml\locale.xml';
  SDefaultTargetPath = 'c:\keyman\'+SKeymanVersion+'\src\desktop\kmshell\locale\';

type
  TNodeList = TList<IXMLDOMNode>;

procedure CheckTargetLocale(targetFilename: string; docMaster: IXMLDOMDocument);
var
  docTarget: IXMLDOMDocument;
  i: Integer;
  ID: string;
  node: IXMLDOMNode;
  nodeMaster: IXMLDOMNode;
  FRemoveNodes: TNodeList;
  FRemoveNodeText: TStringList;
  FMissingNodeText: TStringList;
  nodeTarget: IXMLDOMNode;
begin
  docTarget := CoDOMDocument.Create;
  docTarget.preserveWhiteSpace := True;
  docTarget.async := False;
  docTarget.validateOnParse := False;
  docTarget.load(targetFilename);

  if docTarget.parseError.errorCode <> 0 then
  begin
    writeln(docTarget.parseError.reason);
    writeln(docTarget.parseError.srcText);
    Exit;
  end;

  writeln('Checking '+targetFilename+' for unused strings');

  FRemoveNodeText := TStringList.Create;
  FRemoveNodes := TNodeList.Create;
  try
    // Remove nodes that are no longer in the target locale.xml
    for i := 0 to docTarget.DocumentElement.childNodes.length - 1 do
    begin
      node := docTarget.DocumentElement.ChildNodes[i];
      if node.NodeName = 'String' then
      begin
        if node.Attributes.getNamedItem('Id') <> nil then
        begin
          ID := node.Attributes.getNamedItem('Id').text;
          nodeMaster := docMaster.documentElement.selectSingleNode('/Locale/String[@Id="'+ID+'"]');
          if not Assigned(nodeMaster) then
          begin
            FRemoveNodes.Add(node);
            FRemoveNodeText.Add(Id);
          end;
        end;
      end;
    end;

    writeln('  Removing '+IntToStr(FRemoveNodes.Count)+' entries');
    for i := 0 to FRemoveNodes.Count - 1 do
      FRemoveNodes[i].parentNode.removeChild(FRemoveNodes[i]);

    FRemoveNodeText.SaveToFile(ChangeFileExt(targetFilename,'')+'-removed.txt');
  finally
    FRemoveNodes.Free;
    FRemoveNodeText.Free;
  end;

  FMissingNodeText := TStringList.Create;
  try
    // Document nodes that are missing from target locale.xml
    for i := 0 to docMaster.DocumentElement.childNodes.length - 1 do
    begin
      node := docMaster.DocumentElement.ChildNodes[i];
      if node.NodeName = 'String' then
      begin
        if node.Attributes.getNamedItem('Id') <> nil then
        begin
          ID := node.Attributes.getNamedItem('Id').text;
          nodeTarget := docTarget.documentElement.selectSingleNode('/Locale/String[@Id="'+ID+'"]');
          if not Assigned(nodeTarget) then
          begin
            FMissingNodeText.Add(ID);
          end;
        end;
      end;
    end;

    writeln('  '+IntToStr(FMissingNodeText.Count)+' entries are missing from '+targetFilename);
    FMissingNodeText.SaveToFile(ChangeFileExt(targetFilename,'')+'-missing.txt');

    // Add missing nodes from English locale.xml
    for i := 0 to FMissingNodeText.Count - 1 do
    begin
      nodeTarget := docMaster.documentElement.selectSingleNode('/Locale/String[@Id="'+FMissingNodeText[i]+'"]').cloneNode(True);
      docTarget.documentElement.appendChild(nodeTarget);
      nodeTarget := docTarget.createTextNode(#13#10);
      docTarget.documentElement.appendChild(nodeTarget);
    end;
  finally
    FMissingNodeText.Free;
  end;

  docTarget.save(ChangeFileExt(targetFilename,'')+'-clean.xml');
end;

procedure Run;
var
  docMaster: IXMLDOMDocument;
  InFile: string;
  f: TSearchRec;
begin
  if (Copy(ParamStr(1), 1, 1) = '-') or (Copy(ParamStr(1), 1, 1) = '-') then
  begin
    writeln('Usage: '+ParamStr(0)+' [input-locale.xml [target\locale.xml]]');
    writeln('  If input-locale is not specified, will use '+SDefaultInFile);
    writeln('  If target\locale is not specified, will check locale.xml in every folder in '+SDefaultTargetPath);
    writeln('  For every target\locale.xml, a locale-clean.xml will be written which strips obsolete strings,');
    writeln('  and a locale-removed.txt will be written listing removed strings,');
    writeln('  and a locale-missing.txt will be written listing strings which are missing or untranslated.');
    Exit;
  end;

  if ParamCount < 1
    then InFile := SDefaultInFile
    else InFile := ParamStr(1);

  docMaster := CoDOMDocument.Create;
  docMaster.preserveWhiteSpace := True;
  docMaster.async := False;
  docMaster.validateOnParse := False;
  docMaster.load(InFile);

  if docMaster.parseError.errorCode <> 0 then
  begin
    writeln(docMaster.parseError.reason);
    writeln(docMaster.parseError.srcText);
    Exit;
  end;

  if ParamCount = 2 then
  begin
    CheckTargetLocale(ParamStr(2), docMaster);
  end
  else
  begin
    if FindFirst(SDefaultTargetPath+'*', faDirectory, f) = 0 then
    begin
      repeat
        if ((f.Attr and faDirectory) = faDirectory) and (f.Name <> '.') and (f.Name <> '..') and
            FileExists(SDefaultTargetPath + f.Name + '\locale.xml') then
          CheckTargetLocale(SDefaultTargetPath + f.Name + '\locale.xml', docMaster);
      until FindNext(f) <> 0;
      FindClose(f);
    end;
  end;
end;

end.
