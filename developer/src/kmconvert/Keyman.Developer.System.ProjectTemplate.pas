unit Keyman.Developer.System.ProjectTemplate;

interface

uses
  System.SysUtils,

  kpsfile,
  PackageInfo,
  UKeymanTargets,
  utilfiletypes;

type
  EProjectTemplate = class(Exception);

  TProjectTemplate = class
  private
    FID: string;
    FName: string;
    FBasePath: string;
    FAuthor: string;
    FCopyright: string;
    FVersion: string;
    FBCP47Tags: string;
    FProjectType: TKeymanProjectType;
    FTargets: TKeymanTargets;
    FFullCopyright: string;
    FDescription: string;

  protected
    const
      SFolder_Source = 'source';
      SFolder_Build = 'build';

      SFile_ReadmeMD = 'README.md';     // in root
      SFile_HistoryMD = 'HISTORY.md';   // in root
      SFile_LicenseMD = 'LICENSE.md';   // in root
      SFile_WelcomeHTM = 'welcome.htm'; // in source/
      SFile_ReadmeHTM = 'readme.htm';   // in source/

    function GetFilename(Base: string): string;

    function DataPath: string; virtual;

    function GetPackageFilename: string; virtual;
    function GetProjectFilename: string; virtual;

    procedure WriteRepositoryMetadata;
    procedure WriteDocumentation;
    procedure Transform(const SourceFile: string; DestFile: string = '');
    procedure SetPackageLanguageMetadata(kps: TKpsFile; languages: TPackageKeyboardLanguageList);

    property BasePath: string read FBasePath;
    property ID: string read FID;
  public
    constructor Create(const BasePath, ID: string; ATargets: TKeymanTargets);

    procedure Generate; virtual; abstract;

    property ProjectType: TKeymanProjectType read FProjectType;

    property Targets: TKeymanTargets read FTargets;

    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Copyright: string read FCopyright write FCopyright;
    property FullCopyright: string read FFullCopyright write FFullCopyright;
    property Author: string read FAuthor write FAuthor;
    property Version: string read FVersion write FVersion;
    property BCP47Tags: string read FBCP47Tags write FBCP47Tags;

    property ProjectFilename: string read GetProjectFilename;
    property PackageFilename: string read GetPackageFilename;
  end;

implementation

uses
  System.Classes,
  System.StrUtils,

  BCP47Tag,
  Keyman.System.CanonicalLanguageCodeUtils,
  Keyman.System.LanguageCodeUtils,
  KeyboardParser,
  KeymanVersion,
  Keyman.Developer.System.Project.kpsProjectFile,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectFiles,
  RedistFiles,
  TouchLayout,
  utilstr,
  VisualKeyboard;

{ TProjectTemplate }

constructor TProjectTemplate.Create(const BasePath, ID: string; ATargets: TKeymanTargets);
begin
  inherited Create;
  FBasePath := IncludeTrailingPathDelimiter(ExpandFileName(BasePath));
  FID := ID;
  FVersion := '1.0';
  FTargets := ATargets;
end;

function TProjectTemplate.GetFilename(Base: string): string;
begin
  if Base = '' then
    Result := ''
  else if ExtractFileExt(Base) = Base then
    Result := FBasePath + FID + '\' + SFolder_Source + '\' + FID + Base
  else
    Result := FBasePath + FID + '\' + SFolder_Source + '\' + Base;
end;

function TProjectTemplate.GetPackageFilename: string;
begin
  Result := GetFilename(Ext_PackageSource);
end;

function TProjectTemplate.GetProjectFilename: string;
begin
  // Project file goes in parent folder
  Result := FBasePath + FID + '\' + FID + Ext_ProjectSource;
end;

procedure TProjectTemplate.WriteDocumentation;
begin
  // Write readme.htm and welcome.htm
  Transform(SFolder_Source + '\' + SFile_WelcomeHTM);
  Transform(SFolder_Source + '\' + SFile_ReadmeHTM);
end;

procedure TProjectTemplate.SetPackageLanguageMetadata(kps: TKpsFile; languages: TPackageKeyboardLanguageList);
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
      //bcp47tag.Canonicalize; #2080
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

    languages.Add(pkl);

    tag := StrToken(tags, ' ');
  end;
end;

procedure TProjectTemplate.WriteRepositoryMetadata;
begin
  Transform(SFile_HistoryMD);
  Transform(SFile_LicenseMD);
  Transform(SFile_ReadmeMD);
end;

function TProjectTemplate.DataPath: string;
begin
  Result := GetRedistProjectTemplatePath;
end;

procedure TProjectTemplate.Transform(const SourceFile: string; DestFile: string = '');
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
        bcp47tag.Tag := TCanonicalLanguageCodeUtils.FindBestTag(bcp47tag.Tag, False, False);
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
      if (kt in FTargets) or (ktAny in FTargets) then
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
  s := ReplaceStr(s, '$ID', FId);
  s := ReplaceStr(s, '$VERSION', FVersion);
  s := ReplaceStr(s, '$COPYRIGHT', FCopyright);
  s := ReplaceStr(s, '$FULLCOPYRIGHT', FFullCopyright);
  s := ReplaceStr(s, '$AUTHOR', FAuthor);
  s := ReplaceStr(s, '$DESCRIPTION', FDescription);
  s := ReplaceStr(s, '$DATE', FormatDateTime('yyyy-mm-dd', Now));
  if Pos('$LANGUAGES_KEYBOARD_INFO', s) > 0 then
    s := ReplaceStr(s, '$LANGUAGES_KEYBOARD_INFO', GetLanguageTagListForKeyboardInfo);
  if Pos('$PLATFORMS_DOTLIST_README', s) > 0 then
    s := ReplaceStr(s, '$PLATFORMS_DOTLIST_README', GetPlatformsDotListForReadme);

  ss := TStringStream.Create(s, TEncoding.UTF8);
  try
    ss.SaveToFile(FBasePath + FID + '\' + DestFile);
  finally
    ss.Free;
  end;
end;

end.
