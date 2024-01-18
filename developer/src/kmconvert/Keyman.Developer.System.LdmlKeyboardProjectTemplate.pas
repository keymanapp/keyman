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

procedure TLDMLKeyboardProjectTemplate.WriteLDML;
var
  doc: IXmlDocument;
  node, root: IXmlNode;
  tag, tags: string;
begin
  tags := Trim(BCP47Tags);
  tag := StrToken(tags, ' ');

  doc := NewXMLDocument();
  doc.Options := doc.Options + [doNodeAutoIndent];   // I4704
  doc.Encoding := 'utf-8';

  root := doc.CreateElement('keyboard3', '');
  root.Attributes['locale'] := tag;
  root.Attributes['conformsTo'] := 'techpreview';

  doc.DocumentElement := root;

  node := root.AddChild('info');
  node.Attributes['author'] := Author;
  node.Attributes['name'] := Name;

  root.AddChild('version').Attributes['number'] := Version;

  if tags <> '' then
  begin
    node := root.AddChild('locales');
    while tags <> '' do
    begin
      tag := StrToken(tags, ' ');
      node.AddChild('locale').Attributes['id'] := tag;
    end;
  end;

  root.AddChild('displays');
  root.AddChild('keys');
  root.AddChild('flicks');
  root.AddChild('layers');
  root.AddChild('transforms');
  root.AddChild('variables');

  doc.SaveToFile(KeyboardFileName);
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
