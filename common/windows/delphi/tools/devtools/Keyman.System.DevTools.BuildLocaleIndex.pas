(*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Build index.xml from individual locale.xml files
 *)
unit Keyman.System.DevTools.BuildLocaleIndex;

interface

uses
  System.Classes,
  System.SysUtils,

  Xml.XMLIntf,
  Xml.XMLDoc;

type
  TBuildLocaleIndex = class
  private
    class function ProcessFile(const SourceFile: string; LocalesNode: IXmlNode;
      const BCP47: string): Boolean; static;
  public
    class function Run(SourcePath, Index: string): Boolean; static;
  end;

implementation

uses
  Xml.Win.msxmldom,
  Winapi.ActiveX,
  System.StrUtils,
  System.TypInfo,
  System.Variants;

{ TBuildLocaleIndex }

(**
  * Processes a single `strings.xml` file.
  *
  * Extracts the values of `SKUILanguageName` and
  * `SKUILanguageNameWithEnglish`, and appends a `<locale>` entry to the
  * given XML node.
  *
  * @param SourceFile    Full path to the `strings.xml` file to process.
  * @param LocalesNode   The `<locales>` XML node to append to.
  * @param BCP47         The BCP-47 language tag.
  * @return True if successful; False if required strings were missing.
  *)
class function TBuildLocaleIndex.ProcessFile(const SourceFile: string; LocalesNode: IXmlNode;
  const BCP47: string): Boolean;
var
  doc: IXmlDocument;
  node, locale: IXMLNode;
  uiLanguageName, uiLanguageNameWithEnglish: string;
begin
  doc := TXMLDocument.Create(nil);
  doc.ParseOptions := [poResolveExternals];  // I902 - resolve externals when loading XML files
  doc.LoadFromFile(SourceFile);

  node := doc.DocumentElement.ChildNodes[0];
  while Assigned(node) do
  begin
    if (node.NodeName = 'string') and (node.Attributes['name'] = 'SKUILanguageName') then
      uiLanguageName := node.NodeValue
    else if (node.NodeName = 'string') and (node.Attributes['name'] = 'SKUILanguageNameWithEnglish') then
      uiLanguageNameWithEnglish := node.NodeValue;
    node := node.NextSibling;
  end;

  if (uiLanguageName = '') or (uiLanguageNameWithEnglish = '') then
  begin
    writeln('ERROR: '+SourceFile+' is missing SKUILanguageName or SKUILanguageNameWithEnglish strings');
    Exit(False);
  end;

  locale := LocalesNode.AddChild('locale');
  locale.Attributes['SKLanguageCode'] := BCP47;
  locale.Attributes['SKUILanguageName'] := uiLanguageName;
  locale.Attributes['SKUILanguageNameWithEnglish'] := uiLanguageNameWithEnglish;

  Result := True;
end;

(**
  * Finds all localizations and generates an index.xml of the names and
  * language codes for each localization, for performance purposes.
  *
  * @param SourcePath   Full path to the locale folder with subfolders
                        for each localization.
  * @param Index        Path to the index.xml file to write.
  * @return True if successful; False if required strings were missing.
  *)
class function TBuildLocaleIndex.Run(SourcePath, Index: string): Boolean;
var
  f: TSearchRec;
  SourceFile, BCP47: string;
  OutputDoc: IXmlDocument;
  LocalesNode: IXmlNode;
begin
  SourcePath := IncludeTrailingPathDelimiter(SourcePath);

  // Iterate through SourcePath/*/strings.xml and generate index.xml
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  try
    Xml.Win.msxmldom.MSXMLDOMDocumentFactory.AddDOMProperty('ProhibitDTD', False);

    OutputDoc := NewXMLDocument;
    OutputDoc.Encoding := 'UTF-8';
    OutputDoc.Active := True;
    LocalesNode := OutputDoc.AddChild('locales');

    if FindFirst(SourcePath+'*', faDirectory, f) = 0 then
    begin
      repeat
        BCP47 := f.Name;
        SourceFile := SourcePath+BCP47+'\strings.xml';
        if (BCP47 <> '..') and (BCP47 <> '.') and FileExists(SourcePath+BCP47+'\strings.xml') then
        begin
          if not ProcessFile(SourceFile, LocalesNode, BCP47) then
            Exit(False);
        end;
      until FindNext(f) <> 0;
      FindClose(f);
    end;

    LocalesNode := nil;

    OutputDoc.SaveToFile(Index);
    OutputDoc := nil;

  finally
    CoUninitialize;
  end;

  Result := True;
end;

end.
