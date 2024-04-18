unit Keyman.Developer.System.LdmlKeyboardProjectTemplate;

interface

uses
  System.SysUtils,
  Xml.XmlDoc,
  Xml.XmlIntf,

  kpsfile,
  Keyman.Developer.System.ProjectTemplate,
  UKeymanTargets,
  utilfiletypes;

type
  ELDMLKeyboardProjectTemplate = class(EProjectTemplate);

  TLDMLKeyboardProjectTemplate = class(TProjectTemplate)
  private
    procedure WriteLDML;
    procedure WriteKPS;
    procedure WriteKPJ;
    function GetKeyboardFilename: string;
  protected
    const
      SDataPath_LdmlKeyboard = 'ldml-keyboard\';

    function DataPath: string; override;
  public
    constructor Create(const BasePath, KeyboardID: string);
    procedure Generate; override;
    property KeyboardFilename: string read GetKeyboardFilename;
  end;

implementation

uses
  System.Classes,
  System.StrUtils,

  BCP47Tag,
  Keyman.System.LanguageCodeUtils,
  KeymanVersion,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectFiles,
  PackageInfo,
  utilstr;

{ TLDMLKeyboardProjectTemplate }

constructor TLDMLKeyboardProjectTemplate.Create(const BasePath, KeyboardID: string);
begin
  inherited Create(BasePath, KeyboardID, [ktAny]);
end;

procedure TLDMLKeyboardProjectTemplate.Generate;
begin
  if not ForceDirectories(BasePath + ID + '\' + SFolder_Source) then
    raise ELDMLKeyboardProjectTemplate.Create('Could not create destination path '+BasePath+ID);

  WriteDocumentation;

  WriteLDML;
  WriteKPS;
  WriteKPJ;

  WriteRepositoryMetadata;
end;

function TLDMLKeyboardProjectTemplate.GetKeyboardFilename: string;
begin
  Result := GetFilename(Ext_LDMLKeyboardSource);
end;

const
  XMLNS = 'https://schemas.unicode.org/cldr/45/keyboard3';
  TemplateXML: string =
    '<?xml version="1.0" encoding="UTF-8"?>'#13#10+
    // We won't inject the DOCTYPE because of pathing challenges, CLDR-15505
    //  '<!DOCTYPE keyboard3 SYSTEM "ldmlKeyboard3.dtd">'#13#10+
    '<keyboard3 xmlns="'+XMLNS+'">'#13#10+
    '  <keys>'#13#10+
    '    <import base="cldr" path="45/keys-Zyyy-punctuation.xml"/>'#13#10+
    '    <import base="cldr" path="45/keys-Zyyy-currency.xml"/>  '#13#10+
    '  </keys>'#13#10+
    '  <layers formId="us">'#13#10+
    '    <layer modifiers="none">'#13#10+
    '      <row keys="grave 1 2 3 4 5 6 7 8 9 0 hyphen equal"/>'#13#10+
    '      <row keys="q w e r t y u i o p open-square close-square backslash"/>'#13#10+
    '      <row keys="a s d f g h j k l semi-colon apos"/>'#13#10+
    '      <row keys="z x c v b n m comma period slash"/>'#13#10+
    '      <row keys="space"/>'#13#10+
    '    </layer>'#13#10+
    '    <layer modifiers="shift">'#13#10+
    '      <row keys="tilde bang at hash dollar percent caret amp asterisk open-paren close-paren underscore plus"/>'#13#10+
    '      <row keys="Q W E R T Y U I O P open-curly close-curly pipe"/>'#13#10+
    '      <row keys="A S D F G H J K L colon double-quote"/>'#13#10+
    '      <row keys="Z X C V B N M open-angle close-angle question"/>'#13#10+
    '      <row keys="space"/>'#13#10+
    '    </layer>'#13#10+
    '    <layer modifiers="caps">'#13#10+
    '      <row keys="grave 1 2 3 4 5 6 7 8 9 0 hyphen equal"/>'#13#10+
    '      <row keys="Q W E R T Y U I O P open-square close-square backslash"/>'#13#10+
    '      <row keys="A S D F G H J K L semi-colon apos"/>'#13#10+
    '      <row keys="Z X C V B N M comma period slash"/>'#13#10+
    '      <row keys="space"/>'#13#10+
    '    </layer>'#13#10+
    '    <layer modifiers="shift caps">'#13#10+
    '      <row keys="tilde bang at hash dollar percent caret amp asterisk open-paren close-paren underscore plus"/>'#13#10+
    '      <row keys="q w e r t y u i o p open-curly close-curly pipe"/>'#13#10+
    '      <row keys="a s d f g h j k l colon double-quote"/>'#13#10+
    '      <row keys="z x c v b n m open-angle close-angle question"/>'#13#10+
    '      <row keys="space"/>'#13#10+
    '    </layer>'#13#10+
    '  </layers>'#13#10+
    '</keyboard3>'#13#10;

procedure TLDMLKeyboardProjectTemplate.WriteLDML;
var
  doc: IXmlDocument;
  node, root: IXmlNode;
  xml, tag, tags: string;
  str: TStringStream;
begin
  tags := Trim(BCP47Tags);
  tag := StrToken(tags, ' ');

  doc := TXMLDocument.Create(nil);
  doc.ParseOptions := [];
  doc.LoadFromXML(TemplateXML);

  root := doc.DocumentElement;
  root.Attributes['locale'] := tag;
  root.Attributes['conformsTo'] := '45';

  doc.DocumentElement := root;

  node := doc.CreateElement('info', XMLNS);
  node.Attributes['author'] := Author;
  node.Attributes['name'] := Name;
  root.ChildNodes.Insert(0, node);

  node := doc.CreateElement('version', XMLNS);
  node.Attributes['number'] := Version;
  root.ChildNodes.Insert(1, node);

  if tags <> '' then
  begin
    node := doc.CreateElement('locales', XMLNS);
    while tags <> '' do
    begin
      tag := StrToken(tags, ' ');
      // note: no XMLNS needed here
      node.AddChild('locale').Attributes['id'] := tag;
    end;
    root.ChildNodes.Insert(2, node);
  end;

  // Reformat the document

  doc.SaveToXML(xml);
  xml := FormatXMLData(xml);
  str := TStringStream.Create(xml, TEncoding.UTF8);
  try
    str.SaveToFile(KeyboardFileName);
  finally
    str.Free;
  end;
end;

procedure TLDMLKeyboardProjectTemplate.WriteKPJ;
var
  kpj: TProject;
begin
  kpj := TProject.Create(ptKeyboard, GetProjectFilename, False);
  try
    kpj.Options.Version := pv20;
    kpj.Options.BuildPath := '$PROJECTPATH\' + SFolder_Build;
    kpj.Options.SourcePath := '$PROJECTPATH\' + SFolder_Source;
    kpj.Options.WarnDeprecatedCode := True;
    kpj.Options.CompilerWarningsAsErrors := True;
    kpj.Options.CheckFilenameConventions := True;
    kpj.Options.SkipMetadataFiles := False;
    kpj.Save;
  finally
    kpj.Free;
  end;
end;

procedure TLDMLKeyboardProjectTemplate.WriteKPS;
var
  kps: TKPSFile;
  f: TPackageContentFile;
  pk: TPackageKeyboard;
begin
  kps := TKPSFile.Create;
  try
    // Set kps metadata
    kps.Info.Desc[PackageInfo_Name] := Name;
    kps.Info.Desc[PackageInfo_Copyright] := Copyright;
    kps.Info.Desc[PackageInfo_Author] := Author;
    kps.Info.Desc[PackageInfo_Description] := Description;
    kps.KPSOptions.FollowKeyboardVersion := True;
    kps.FileName := GetPackageFilename;

    // Add .kmx
    f := TPackageContentFile.Create(kps);
    f.FileName := BasePath + ID + '\' + SFolder_Build + '\' + ID + Ext_KeymanFile;
    kps.Files.Add(f);

    // Add .kvk
    f := TPackageContentFile.Create(kps);
    f.FileName := BasePath + ID + '\' + SFolder_Build + '\' + ID + Ext_VisualKeyboard;
    kps.Files.Add(f);

    // Add welcome
    f := TPackageContentFile.Create(kps);
    f.FileName := BasePath + ID + '\' + SFolder_Source + '\' + SFile_WelcomeHTM;
    kps.Files.Add(f);

    // Add readme
    f := TPackageContentFile.Create(kps);
    f.FileName := BasePath + ID + '\' + SFolder_Source + '\' + SFile_ReadmeHTM;
    kps.Files.Add(f);
    kps.Options.ReadmeFile := f;

    // Add license
    f := TPackageContentFile.Create(kps);
    f.FileName := BasePath + ID + '\' + SFile_LicenseMD;
    kps.Files.Add(f);
    kps.Options.LicenseFile := f;

    // Add metadata about the keyboard
    pk := TPackageKeyboard.Create(kps);
    pk.Name := Name;
    pk.ID := ID;
    pk.Version := Version;
    kps.Keyboards.Add(pk);

    SetPackageLanguageMetadata(kps, pk.Languages);

    kps.SaveXML;
  finally
    kps.Free;
  end;
end;

function TLDMLKeyboardProjectTemplate.DataPath: string;
begin
  Result := inherited DataPath + SDataPath_LdmlKeyboard;
end;

end.
