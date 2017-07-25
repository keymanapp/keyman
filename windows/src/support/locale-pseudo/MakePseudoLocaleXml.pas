unit MakePseudoLocaleXml;

interface

procedure Run;

implementation

uses
  System.SysUtils,
  System.Types,
  System.Variants,
  Winapi.msxml,

  KeymanVersion;

const
  // adapted from https://github.com/bunkat/pseudoloc [MIT]
  pseudolocTable: array[0..51] of string = (
    'ÀÁÂÃÄÅĀĂĄǺȀȂ',
    'àáâãäåāăąǻȁȃ',
    'ßƁɃʙ',
    'ƀƂƃƄƅɓ',
    'ĆĈĊČ',
    'ćĉċč',
    'ĎĐƉƊ',
    'ďđ',
    'ĒĔĖĘĚȄȆ',
    'ēĕėęěȅȇ',
    'Ƒ',
    'ƒ',
    'ĜĞĠĢ',
    'ĝğġģ',
    'ĤĦ',
    'ĥħ',
    'ĨĪĬĮİ',
    'ĩīĭįı',
    'ĵ',
    'Ĵ',
    'ĶƘ',
    'ķĸƙ',
    'ĹĻĽĿŁ',
    'ĺļľŀł',
    'M',
    'm',
    'ŃŅŇŊƝ',
    'ńņňŉŋƞ',
    'ŌŎŐƠ',
    'ōŏőơ',
    'Ƥ',
    'ƥƿ',
    'ǪǬ',
    'ǫǭɋ',
    'ŔŖŘƦ',
    'ŕŗř',
    'ŚŜŞŠ',
    'śŝşš',
    'ŢŤŦ',
    'ţťŧ',
    'ŨŪŬŮŰŲ',
    'ũūŭůűų',
    'V',
    'v',
    'Ŵ',
    'ŵ',
    'X',
    'x',
    'ŶŸƳȲɎ',
    'ŷƴȳɏ',
    'ŹŻŽƵ',
    'źżžƶ'
);

function Pseudo(a: string): string;
var
  i, n: Integer;
  FInToken: Boolean;
begin
  if a = '' then
    Exit('');
  FInToken := False;
  for i := 1 to Length(a) do
  begin
    if FInToken then
    begin
      if CharInSet(a[i], ['a'..'z','A'..'Z']) then
        FInToken := False;
      Continue;
    end;
    if CharInSet(a[i], ['a'..'z']) then
      n := (Ord(a[i]) - Ord('a')) * 2 + 1
    else if CharInSet(a[i], ['A'..'Z']) then
      n := (Ord(a[i]) - Ord('A')) * 2
    else if a[i] = '%' then
    begin
      FInToken := True;
      Continue;
    end
    else
      Continue;
    a[i] := pseudoloctable[n][Random(Length(pseudoloctable[n]))+1];
  end;
  Result := '[!!'+a+'!!]';
end;

procedure Pseudofy(node: IXMLDOMNode);
var
  i: Integer;
begin
  if node.nodeType = NODE_TEXT then
  try
    node.nodeValue := Pseudo(VarToStr(node.text))
  except
    on E:Exception do writeln(E.Message);
  end
  else if node.nodeType = NODE_ENTITY_REFERENCE then
    Exit
  else
    for i := 0 to node.childNodes.length - 1 do
      Pseudofy(node.childNodes[i]);
end;

const
  SDefaultInFile = 'c:\keyman\'+SKeymanVersion+'\src\desktop\kmshell\xml\locale.xml';
  SDefaultOutFile = 'c:\keyman\'+SKeymanVersion+'\src\desktop\kmshell\locale\qqq\locale.xml';
procedure Run;
var
  doc: IXMLDOMDocument;
  node: IXMLDOMNode;
  i: Integer;
  InFile, OutFile: string;
begin
  if (Copy(ParamStr(1), 1, 1) = '-') or (Copy(ParamStr(1), 1, 1) = '-') then
  begin
    writeln('Usage: '+ParamStr(0)+' [input-locale.xml [output-locale.xml]]');
    writeln('  If input is not specified, will use '+SDefaultInFile);
    writeln('  If output is not specified, will use '+SDefaultOutFile);
    Exit;
  end;

  if ParamCount < 1
    then InFile := SDefaultInFile
    else InFile := ParamStr(1);

  doc := CoDOMDocument.Create;
  doc.preserveWhiteSpace := True;
  doc.async := False;
  doc.validateOnParse := False;
  doc.load(InFile);

  if doc.parseError.errorCode <> 0 then
  begin
    writeln(doc.parseError.reason);
    writeln(doc.parseError.srcText);
    Exit;
  end;

  for i := 0 to doc.DocumentElement.childNodes.length - 1 do
  begin
    node := doc.DocumentElement.ChildNodes[i];
    if node.NodeName = 'String' then
    begin
      if (node.Attributes.getNamedItem('Id') <> nil) and
        // Yeah, let's not localize those :)
        ((node.Attributes.getNamedItem('Id').nodeValue = 'SK_UIFontName') or
          (node.Attributes.getNamedItem('Id').nodeValue = 'SK_UIFontSize')) then
        Continue;
      Pseudofy(node);
    end;
  end;

  if ParamCount < 2
    then OutFile := SDefaultOutFile
    else OutFile := ParamStr(2);

  doc.save(OutFile);
end;

end.
