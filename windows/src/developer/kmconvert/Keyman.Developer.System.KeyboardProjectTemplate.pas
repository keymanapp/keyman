unit Keyman.Developer.System.KeyboardProjectTemplate;

interface

uses
  System.SysUtils,

  kpsfile,
  UKeymanTargets;

type
  EKeyboardProjectTemplate = class(Exception);

  TKeyboardProjectTemplate = class
  private
    FName: string;
    FBasePath: string;
    FKeyboardID: string;
    FTargets: TKeymanTargets;
    FIconFilename: string;
    FOSKFilename: string;
    FTouchLayoutFilename: string;
    FAuthor: string;
    FCopyright: string;
    FVersion: string;
    FBCP47Tags: string;
    FIncludeIcon: Boolean;
    function GetFilename(Base: string): string;
    function GetIconFilename: string;
    function GetKeyboardFilename: string;
    function GetOSKFilename: string;
    function GetPackageFilename: string;
    function GetProjectFilename: string;
    function GetTouchLayoutFilename: string;

    function HasIcon: Boolean;
    function HasKMX: Boolean;
    function HasKVKS: Boolean;
    function HasTouchLayout: Boolean;

    procedure WriteKMN;
    procedure WriteKPS;
    procedure WriteKVKS;
    procedure WriteTouchLayout;
    procedure WriteKPJ;
    procedure WriteRepositoryMetadata;
    procedure WriteDocumentation;
    procedure WriteKeyboardInfo;
    procedure Transform(const SourceFile: string; DestFile: string = '');
    function DataPath: string;
    procedure SetKeyboardLanguages(kps: TkpsFile);
  public
    constructor Create(const BasePath, KeyboardID: string; Targets: TKeymanTargets);
    destructor Destroy; override;
    procedure Generate;
    property Name: string read FName write FName;
    property Copyright: string read FCopyright write FCopyright;
    property Author: string read FAuthor write FAuthor;
    property Version: string read FVersion write FVersion;
    property BCP47Tags: string read FBCP47Tags write FBCP47Tags;
    property IncludeIcon: Boolean read FIncludeIcon write FIncludeIcon;

    property KeyboardFilename: string read GetKeyboardFilename;
    property ProjectFilename: string read GetProjectFilename;
    property PackageFilename: string read GetPackageFilename;
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
  Keyman.Developer.System.Project.kmnProjectFile,
  Keyman.Developer.System.Project.kpsProjectFile,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectFiles,
  PackageInfo,
  RedistFiles,
  TouchLayout,
  utilfiletypes,
  utilstr,
  VisualKeyboard;


const
  SFolder_Source = 'source';
  SFolder_Build = 'build';

  SFile_ReadmeMD = 'README.md';     // in root
  SFile_HistoryMD = 'HISTORY.md';   // in root
  SFile_LicenseMD = 'LICENSE.md';   // in root
  SFile_WelcomeHTM = 'welcome.htm'; // in source/
  SFile_ReadmeHTM = 'readme.htm';   // in source/

  SFileTemplate_KeyboardInfo = '%s.keyboard_info'; // in root

{ TKeyboardProjectTemplate }

constructor TKeyboardProjectTemplate.Create(const BasePath, KeyboardID: string; Targets: TKeymanTargets);
begin
  inherited Create;
  FBasePath := IncludeTrailingPathDelimiter(ExpandFileName(BasePath));
  FKeyboardID := LowerCase(KeyboardID);
  FTargets := Targets;
  FVersion := '1.0';
end;

destructor TKeyboardProjectTemplate.Destroy;
begin
  inherited Destroy;
end;

procedure TKeyboardProjectTemplate.Generate;
begin
  if not ForceDirectories(FBasePath + FKeyboardID + '\' + SFolder_Source) then
    raise EKeyboardProjectTemplate.Create('Could not create destination path '+FBasePath+FKeyboardID);

  WriteDocumentation;

  WriteKMN;
  WriteKPS;
  if HasKVKS then
    WriteKVKS;
  if HasTouchLayout then
    WriteTouchLayout;

  WriteKPJ;

  WriteRepositoryMetadata;

  WriteKeyboardInfo;
end;

function TKeyboardProjectTemplate.GetFilename(Base: string): string;
begin
  if Base = '' then
    Result := ''
  else if ExtractFileExt(Base) = Base then
    Result := FBasePath + FKeyboardID + '\' + SFolder_Source + '\' + FKeyboardID + Base
  else
    Result := FBasePath + FKeyboardID + '\' + SFolder_Source + '\' + Base;
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

function TKeyboardProjectTemplate.GetPackageFilename: string;
begin
  Result := GetFilename(Ext_PackageSource);
end;

function TKeyboardProjectTemplate.GetProjectFilename: string;
begin
  // Project file goes in parent folder
  Result := FBasePath + FKeyboardID + '\' + FKeyboardID + Ext_ProjectSource;
end;

function TKeyboardProjectTemplate.GetTouchLayoutFilename: string;
begin
  Result := GetFilename(FTouchLayoutFilename);
end;

function TKeyboardProjectTemplate.HasIcon: Boolean;
begin
  Result := FIncludeIcon and ((KMXKeymanTargets+[ktAny]) * FTargets <> []);
end;

function TKeyboardProjectTemplate.HasKMX: Boolean;
begin
  Result := (KMXKeymanTargets+[ktAny]) * FTargets <> [];
end;

function TKeyboardProjectTemplate.HasKVKS: Boolean;
begin
  Result := (KeymanTargetsUsingKVK+[ktAny]) * FTargets <> [];
end;

function TKeyboardProjectTemplate.HasTouchLayout: Boolean;
begin
  Result := (TouchKeymanTargets+[ktAny]) * FTargets <> [];
end;

procedure TKeyboardProjectTemplate.WriteDocumentation;
begin
  // Write readme.htm and welcome.htm
  Transform(SFolder_Source + '\' + SFile_WelcomeHTM);
  Transform(SFolder_Source + '\' + SFile_ReadmeHTM);
end;

procedure TKeyboardProjectTemplate.WriteKeyboardInfo;
begin
  // Write keyboardid.keyboard_info
  Transform(
    Format(SFileTemplate_KeyboardInfo, ['keyboard']),
    Format(SFileTemplate_KeyboardInfo, [FKeyboardID])
  );
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
      FKeyboardID+' generated from template at '+FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)+#13#10+
      'with name "'+FName+'"'#13#10;
    kp.SetSystemStoreValue(ssName, FName);
    if FCopyright <> '' then
      kp.SetSystemStoreValue(ssCopyright, FCopyright);
    kp.SetSystemStoreValue(ssVersion, SKeymanKeyboardVersion);
    kp.SetSystemStoreValue(ssKeyboardVersion, FVersion);
    kp.SetSystemStoreValue(ssTargets, KeymanTargetsToString(FTargets));

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
  kpj := TProject.Create(GetProjectFilename);
  try
    kpj.Options.BuildPath := '$PROJECTPATH\' + SFolder_Build;
    kpj.Options.WarnDeprecatedCode := True;
    kpj.Options.CompilerWarningsAsErrors := True;
    kpj.Options.CheckFilenameConventions := True;

    // Add keyboard and package to project
    kpj.Files.Add(TkmnProjectFile.Create(kpj, GetKeyboardFilename, nil));
    kpj.Files.Add(TkpsProjectFile.Create(kpj, GetPackageFilename, nil));

    // Add metadata files to project
    kpj.Files.Add(TOpenableProjectFile.Create(kpj, FBasePath + FKeyboardID + '\' + SFile_HistoryMD, nil));
    kpj.Files.Add(TOpenableProjectFile.Create(kpj, FBasePath + FKeyboardID + '\' + SFile_LicenseMD, nil));
    kpj.Files.Add(TOpenableProjectFile.Create(kpj, FBasePath + FKeyboardID + '\' + SFile_ReadmeMD, nil));
    kpj.Files.Add(TOpenableProjectFile.Create(kpj, FBasePath + FKeyboardID + '\' + Format(SFileTemplate_KeyboardInfo, [FKeyboardID]), nil));

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
    kps.Info.Desc[PackageInfo_Name] := FName;
    kps.Info.Desc[PackageInfo_Copyright] := FCopyright;
    kps.Info.Desc[PackageInfo_Author] := FAuthor;
    kps.KPSOptions.FollowKeyboardVersion := True;
    kps.FileName := GetPackageFilename;

    // Add .kmx
    if HasKMX then
    begin
      f := TPackageContentFile.Create(kps);
      f.FileName := FBasePath + FKeyboardID + '\' + SFolder_Build + '\' + FKeyboardID + Ext_KeymanFile;
      kps.Files.Add(f);
    end;

    // Add .js
    if HasTouchLayout then
    begin
      f := TPackageContentFile.Create(kps);
      f.FileName := FBasePath + FKeyboardID + '\' + SFolder_Build + '\' + FKeyboardID + Ext_Javascript;
      kps.Files.Add(f);
    end;

    // Add .kvk
    if HasKVKS then
    begin
      f := TPackageContentFile.Create(kps);
      f.FileName := FBasePath + FKeyboardID + '\' + SFolder_Build + '\' + FKeyboardID + Ext_VisualKeyboard;
      kps.Files.Add(f);
    end;

    // Add welcome
    f := TPackageContentFile.Create(kps);
    f.FileName := FBasePath + FKeyboardID + '\' + SFolder_Source + '\' + SFile_WelcomeHTM;
    kps.Files.Add(f);

    // Add readme
    f := TPackageContentFile.Create(kps);
    f.FileName := FBasePath + FKeyboardID + '\' + SFolder_Source + '\' + SFile_ReadmeHTM;
    kps.Files.Add(f);
    kps.Options.ReadmeFile := f;

    // Add metadata about the keyboard
    pk := TPackageKeyboard.Create(kps);
    pk.Name := FName;
    pk.ID := FKeyboardID;
    pk.Version := FVersion;
    kps.Keyboards.Add(pk);

    SetKeyboardLanguages(kps);

    kps.SaveXML;
  finally
    kps.Free;
  end;
end;

procedure TKeyboardProjectTemplate.SetKeyboardLanguages(kps: TkpsFile);
var
  tag, tags, s: string;
  bcp47tag: TBCP47Tag;
  pkl: TPackageKeyboardLanguage;
begin
  tags := Trim(FBCP47Tags);
  tag := StrToken(tags, ' ');
  while tag <> '' do
  begin
    pkl := TPackageKeyboardLanguage.Create(kps);

    bcp47tag := TBCP47Tag.Create(tag);
    try
      bcp47tag.Canonicalize;
      pkl.ID := bcp47tag.Tag;

      if not TLanguageCodeUtils.BCP47Languages.TryGetValue(bcp47tag.Language, pkl.Name) then
      begin
        pkl.Name := bcp47tag.Language;
      end;

      if bcp47tag.Script <> '' then
      begin
        if TLanguageCodeUtils.BCP47Languages.TryGetValue(bcp47tag.Script, s) then
          pkl.Name := pkl.Name + ' ('+s+')';
      end;
    finally
      bcp47tag.Free;
    end;

    kps.Keyboards[0].Languages.Add(pkl);

    tag := StrToken(tags, ' ');
  end;
end;

procedure TKeyboardProjectTemplate.WriteKVKS;
var
  vk: TVisualKeyboard;
begin
  Assert(HasKVKS);
  vk := TVisualKeyboard.Create;
  try
    vk.Header.AssociatedKeyboard := FKeyboardID;
    vk.SaveToFile(OSKFilename, kvksfXML);
  finally
    vk.Free;
  end;
end;

procedure TKeyboardProjectTemplate.WriteRepositoryMetadata;
begin
  Transform(SFile_HistoryMD);
  Transform(SFile_LicenseMD);
  Transform(SFile_ReadmeMD);
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

function TKeyboardProjectTemplate.DataPath: string;
begin
  Result := GetRedistProjectTemplatePath;
end;

procedure TKeyboardProjectTemplate.Transform(const SourceFile: string; DestFile: string = '');
var
  s: string;
  ss: TStringStream;

  function GetLanguageTagListForKeyboardInfo: string;
  var
    tags, tag: string;
    bcp47tag: TBCP47Tag;
  begin
    // We won't worry about line breaks...
    tags := Trim(FBCP47Tags);
    tag := StrToken(tags, ' ');
    Result := '';
    while tag <> '' do
    begin
      if Result <> '' then
        Result := Result + ', ';

      bcp47tag := TBCP47Tag.Create(tag);
      try
        bcp47tag.Canonicalize;
        Result := Result + '"' + bcp47tag.Tag + '"';
      finally
        bcp47tag.Free;
      end;
      tag := StrToken(tags, ' ');
    end;
  end;

  function GetPlatformsDotListForReadme: string;
  var
    kt: TKeymanTarget;
  begin
    Result := '';
    for kt := Low(TKeymanTarget) to High(TKeymanTarget) do
    begin
      if kt = ktAny then
        Continue;
      Result := Result + ' * ' + SKeymanTargetNames[kt] + #13#10;
    end;
  end;

begin
  if DestFile = '' then
    DestFile := SourceFile;

  ss := TStringStream.Create('', TEncoding.UTF8);
  try
    ss.LoadFromFile(DataPath + SourceFile);
    s := ss.DataString;
  finally
    ss.Free;
  end;

  s := ReplaceStr(s, '$NAME', FName);
  s := ReplaceStr(s, '$VERSION', FVersion);
  s := ReplaceStr(s, '$COPYRIGHT', FCopyright);
  s := ReplaceStr(s, '$AUTHOR', FAuthor);
  s := ReplaceStr(s, '$DATE', FormatDateTime('yyyy-mm-dd', Now));
  if Pos('$LANGUAGES_KEYBOARD_INFO', s) > 0 then
    s := ReplaceStr(s, '$LANGUAGES_KEYBOARD_INFO', GetLanguageTagListForKeyboardInfo);
  if Pos('$PLATFORMS_DOTLIST_README', s) > 0 then
    s := ReplaceStr(s, '$PLATFORMS_DOTLIST_README', GetPlatformsDotListForReadme);

  ss := TStringStream.Create(s, TEncoding.UTF8);
  try
    ss.SaveToFile(FBasePath + FKeyboardID + '\' + DestFile);
  finally
    ss.Free;
  end;
end;

end.
