unit DevCheckSvnStatus;

interface

type
  TCheckSvnStatus = class
    class function Run: Boolean;
  end;

implementation

uses
  DevUtils,
  XmlDoc,
  XmlIntf;

{ TCheckSvnStatus }

class function TCheckSvnStatus.Run: Boolean;
var
  doc: IXMLDocument;
  target: IXMLNode;
  ec, i: Integer;
  entry: IXMLNode;
  wcstatus: IXMLNode;
  path: string;
  j: Integer;
  item: string;
begin
  if not DevUtils.CommandExecute('svn st -u --xml', 'c:\keyman\svnst.txt', 'c:\keyman\9.0\src', 0, ec, False, True) or (ec <> 0) then
    Exit(False);
  doc := LoadXmlDocument('c:\keyman\svnst.txt');
  target := doc.DocumentElement.ChildNodes.FindNode('target');

  Result := True;

  for i := 0 to target.ChildNodes.Count-1 do
  begin
    entry := target.ChildNodes[i];
    if entry.NodeName = 'entry' then
    begin
      if Result then
      begin
        writeln;
        writeln('SVN is not up to date.  Please resolve the following files:');
      end;
      Result := False;
      path := entry.Attributes['path'];
      for j := 0 to entry.ChildNodes.Count - 1 do
      begin
        wcstatus := entry.ChildNodes[j];
        if wcstatus.NodeName = 'wc-status' then
        begin
          item := wcstatus.Attributes['item'];
          writeln('  '+path+':    '+item);
        end;
      end;
    end;
  end;
end;

end.
