unit Keyman.Developer.System.KeyboardProjectTemplate;

interface

uses
  System.SysUtils,

  kpsfile,
  Keyman.Developer.System.ProjectTemplate,
  UKeymanTargets,
  utilfiletypes;

type
  EKeyboardProjectTemplate = class(EProjectTemplate);

  TKeyboardProjectTemplate = class(TProjectTemplate)
  private
    FIconFilename: string;
    FOSKFilename: string;
    FTouchLayoutFilename: string;
    FIncludeIcon: Boolean;

    function GetIconFilename: string;
    function GetKeyboardFilename: string;
    function GetOSKFilename: string;
    function GetTouchLayoutFilename: string;

    function HasIcon: Boolean;
    function HasKMX: Boolean;
    function HasKVKS: Boolean;
    function HasTouchLayout: Boolean;

    procedure WriteKMN;
    procedure WriteKPS;
    procedure WriteKPJ;
    procedure WriteKVKS;
    procedure WriteTouchLayout;
    procedure WriteIcon;
  protected
    const
      SDataPath_BasicKeyboard = 'basic-keyboard\';

    function DataPath: string; override;
  public
    constructor Create(const BasePath, KeyboardID: string; Targets: TKeymanTargets);
    procedure Generate; override;

    property IncludeIcon: Boolean read FIncludeIcon write FIncludeIcon;

    property KeyboardFilename: string read GetKeyboardFilename;
    property OSKFilename: string read GetOSKFilename;
    property IconFilename: string read GetIconFilename;
    property TouchLayoutFilename: string read GetTouchLayoutFilename;
  end;

implementation

uses
  System.Classes,
  System.StrUtils,

  BCP47Tag,
  Keyman.System.LanguageCodeUtils,
  KeyboardParser,
  KeymanVersion,
  kmxfileconsts,
  Keyman.Developer.System.GenerateKeyboardIcon,
  Keyman.Developer.System.Project.kmnProjectFile,
  Keyman.Developer.System.Project.kpsProjectFile,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectFiles,
  PackageInfo,
  RedistFiles,
  TouchLayout,
  utilstr,
  VisualKeyboard;


{ TKeyboardProjectTemplate }

constructor TKeyboardProjectTemplate.Create(const BasePath, KeyboardID: string; Targets: TKeymanTargets);
begin
  inherited Create(BasePath, KeyboardID, Targets);
end;

procedure TKeyboardProjectTemplate.Generate;
begin
  if not ForceDirectories(BasePath + ID + '\' + SFolder_Source) then
    raise EKeyboardProjectTemplate.Create('Could not create destination path '+BasePath+ID);

  WriteDocumentation;

  WriteKMN;
  WriteKPS;
  if HasKVKS then
    WriteKVKS;
  if HasTouchLayout then
    WriteTouchLayout;
  if HasIcon then
    WriteIcon;

  WriteKPJ;

  WriteRepositoryMetadata;
end;

function TKeyboardProjectTemplate.GetIconFilename: string;
begin
  if FIncludeIcon
    then Result := GetFilename(FIconFilename)
    else Result := '';
end;

function TKeyboardProjectTemplate.GetKeyboardFilename: string;
begin
  Result := GetFilename(Ext_KeymanSource);
end;

function TKeyboardProjectTemplate.GetOSKFilename: string;
begin
  Result := GetFilename(FOSKFilename);
end;

function TKeyboardProjectTemplate.GetTouchLayoutFilename: string;
begin
  Result := GetFilename(FTouchLayoutFilename);
end;

function TKeyboardProjectTemplate.HasIcon: Boolean;
begin
  Result := FIncludeIcon and ((KMXKeymanTargets+[ktAny]) * Targets <> []);
end;

function TKeyboardProjectTemplate.HasKMX: Boolean;
begin
  Result := (KMXKeymanTargets+[ktAny]) * Targets <> [];
end;

function TKeyboardProjectTemplate.HasKVKS: Boolean;
begin
  Result := (KeymanTargetsUsingKVK+[ktAny]) * Targets <> [];
end;

function TKeyboardProjectTemplate.HasTouchLayout: Boolean;
begin
  Result := (TouchKeymanTargets+[ktAny]) * Targets <> [];
end;

procedure TKeyboardProjectTemplate.WriteKMN;
var
  FKeyboardSource: string;
  kp: TKeyboardParser;
  sl: TStringList;
begin
  kp := TKeyboardParser.Create;
  try
    kp.FileName := KeyboardFilename;

    kp.InitialComment := #13#10+
      ID+' generated from template at '+FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)+#13#10+
      'with name "'+Name+'"'#13#10;
    kp.SetSystemStoreValue(ssName, Name);
    if Copyright <> '' then
      kp.SetSystemStoreValue(ssCopyright, Copyright);
    kp.SetSystemStoreValue(ssVersion, SKeymanKeyboardVersion);
    kp.SetSystemStoreValue(ssKeyboardVersion, Version);
    kp.SetSystemStoreValue(ssTargets, KeymanTargetsToString(Targets));

    if HasIcon then
      kp.Features.Add(kfIcon);

    if HasKVKS then
      kp.Features.Add(kfOSK);

    if HasTouchLayout then
      kp.Features.Add(kfTouchLayout);

    kp.AddRequiredLines;

    FKeyboardSource := kp.KeyboardText;

    FIconFilename := kp.GetSystemStoreValue(ssBitmap);
    FOSKFilename := kp.GetSystemStoreValue(ssVisualKeyboard);
    FTouchLayoutFilename := kp.GetSystemStoreValue(ssLayoutFile);

    sl := TStringList.Create;
    try
      sl.Text := FKeyboardSource;
      sl.SaveToFile(KeyboardFilename, TEncoding.UTF8);
    finally
      sl.Free;
    end;
  finally
    kp.Free;
  end;
end;

procedure TKeyboardProjectTemplate.WriteKPJ;
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

procedure TKeyboardProjectTemplate.WriteKPS;
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
    if HasKMX then
    begin
      f := TPackageContentFile.Create(kps);
      f.FileName := BasePath + ID + '\' + SFolder_Build + '\' + ID + Ext_KeymanFile;
      kps.Files.Add(f);
    end;

    // Add .js
    if HasTouchLayout then
    begin
      f := TPackageContentFile.Create(kps);
      f.FileName := BasePath + ID + '\' + SFolder_Build + '\' + ID + Ext_Javascript;
      kps.Files.Add(f);
    end;

    // Add .kvk
    if HasKVKS then
    begin
      f := TPackageContentFile.Create(kps);
      f.FileName := BasePath + ID + '\' + SFolder_Build + '\' + ID + Ext_VisualKeyboard;
      kps.Files.Add(f);
    end;

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

procedure TKeyboardProjectTemplate.WriteKVKS;
var
  vk: TVisualKeyboard;
begin
  Assert(HasKVKS);
  vk := TVisualKeyboard.Create;
  try
    vk.Header.AssociatedKeyboard := ID;
    vk.SaveToFile(OSKFilename, kvksfXML);
  finally
    vk.Free;
  end;
end;

procedure TKeyboardProjectTemplate.WriteTouchLayout;
var
  tl: TTouchLayout;
  JSON: string;
  ss: TStringStream;
  sl: TStringList;
const
  CTouchLayoutBasicFile = 'template-basic'+Ext_KeymanTouchLayout;
begin
  Assert(HasTouchLayout);

  sl := TStringList.Create; // We use a stringlist because may have a BOM
  try
    sl.LoadFromFile(GetLayoutBuilderPath + CTouchLayoutBasicFile, TEncoding.UTF8);
    JSON := sl.Text;
  finally
    sl.Free;
  end;

  tl := TTouchLayout.Create;
  try
    // We'll load and save to ensure we get formatting
    tl.Load(JSON);
    JSON := tl.Save(True);

    // We don't want a BOM on output
    ss := TStringStream.Create(JSON, TEncoding.UTF8);
    try
      ss.SaveToFile(TouchLayoutFilename);
    finally
      ss.Free;
    end;
  finally
    tl.Free;
  end;
end;

procedure TKeyboardProjectTemplate.WriteIcon;
var
  FTags: string;
begin
  Assert(HasIcon);
  // We'll generate an icon, based first on BCP47 tag,
  // then if that is not present, on first two letters
  // of filename. Not going to go silly here!

  if FileExists(IconFilename) then
  begin
    // Some tools may already generate an icon,
    // for example import windows keyboard
    Exit;
  end;

  if BCP47Tags <> ''
    then FTags := BCP47Tags
    else FTags := ChangeFileExt(ExtractFileName(IconFilename), '');

  FTags := Copy(FTags, 1, 3);

  TKeyboardIconGenerator.GenerateIcon(FTags, IconFilename, MaxInt);
end;

function TKeyboardProjectTemplate.DataPath: string;
begin
  Result := inherited DataPath + SDataPath_BasicKeyboard;
end;

end.
