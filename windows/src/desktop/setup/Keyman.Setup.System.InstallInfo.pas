{**
 * Provides environmental and configuration parameters for the installation.
 * Includes functionality for querying online API for extra metadata such as
 * version and location of .msi to be installed.
 *}
unit Keyman.Setup.System.InstallInfo;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,

  PackageInfo,
  SetupStrings;

type
  EInstallInfo = class(Exception);

  TMSIInfo = record
    Version, ProductCode: string;
  end;

  TInstallInfoLocationType = (iilLocal, iilOnline);

  TInstallInfoPackageLanguage = class
  private
    FName: string;
    FBCP47: string;
  public
    constructor Create(const ABCP47, AName: string);
    property BCP47: string read FBCP47;
    property Name: string read FName;
  end;

  TInstallInfoPackageLanguages = class(TObjectlist<TInstallInfoPackageLanguage>)
  end;

  TInstallInfoFileLocation = class
  private
    FLocationType: TInstallInfoLocationType;
    FUrl: string;
    FSize: Integer;
    FVersion: string;
    FPath: string;
    FProductCode: string;
  public
    constructor Create(ALocationType: TInstallInfoLocationType); virtual;
    procedure UpgradeToLocalPath(const ARootPath: string);
    property LocationType: TInstallInfoLocationType read FLocationType;
    property Size: Integer read FSize write FSize;
    property Path: string read FPath write FPath;
    property Url: string read FUrl write FUrl;
    property ProductCode: string read FProductCode write FProductCode; // used only by msi
    property Version: string read FVersion write FVersion;
  end;

  TInstallInfoFileLocations = class(TObjectList<TInstallInfoFileLocation>)
  public
    function LatestVersion(const default: string = '0'): string;
  end;

  TInstallInfoPackageFileLocation = class(TInstallInfoFileLocation)
  private
    FName: string;
    FLanguages: TInstallInfoPackageLanguages;
    FLocalPackage: TPackage;
  public
    constructor Create(ALocationType: TInstallInfoLocationType); override;
    destructor Destroy; override;
    function GetNameOrID(const id: string): string;
    function GetLanguageNameFromBCP47(const bcp47: string): string;
    property Name: string read FName write FName;
    property Languages: TInstallInfoPackageLanguages read FLanguages;
    property LocalPackage: TPackage read FLocalPackage;
  end;

  TInstallInfoPackageFileLocations = class(TObjectList<TInstallInfoPackageFileLocation>)
  public
    function LatestVersion(const default: string = '0'): string;
  end;

  TInstallInfoPackage = class
  private
    FID: string;
    FBCP47: string;
    FLocations: TInstallInfoPackageFileLocations;
    FShouldInstall: Boolean;
    FInstallLocation: TInstallInfoPackageFileLocation;
    function GetBestLocation: TInstallInfoPackageFileLocation;
  public
    constructor Create(const AID: string; const ABCP47: string = '');
    destructor Destroy; override;
    property ID: string read FID;
    property BCP47: string read FBCP47 write FBCP47;
    property ShouldInstall: Boolean read FShouldInstall write FShouldInstall;
    property Locations: TInstallInfoPackageFileLocations read FLocations;
    property InstallLocation: TInstallInfoPackageFileLocation read FInstallLocation write FInstallLocation;
  end;

  TInstallInfoPackages = class(TObjectList<TInstallInfoPackage>)
    function FindById(const id: string; createIfNotFound: Boolean): TInstallInfoPackage;
  end;

  TInstallInfo = class
  private
    FAppName: WideString;
    FPackages: TInstallInfoPackages;
    FStrings: TStrings;
    FTitleImageFilename: string;
    FStartDisabled: Boolean;
    FStartWithConfiguration: Boolean;
    FMsiLocations: TInstallInfoFileLocations;
    FBestMsi: TInstallInfoFileLocation;
    FInstalledVersion: TMSIInfo;
    FIsInstalled: Boolean;
    FIsNewerAvailable: Boolean;
    FTempPath: string;
    FShouldInstallKeyman: Boolean;
    FTier: string;
    FMsiInstallLocation: TInstallInfoFileLocation;
    function GetBestMsi: TInstallInfoFileLocation;
    function GetPackageMetadata(const KmpFilename: string; p: TPackage): Boolean;
  public
    constructor Create(const ATempPath: string);
    destructor Destroy; override;
    procedure LoadSetupInf(const SetupInfPath: string);

    procedure LocatePackagesAndTierFromFilename(Filename: string);
    procedure LocatePackagesFromParameter(const Param: string);
    procedure LoadLocalPackagesMetadata;

    function CheckMsiUpgradeScenarios: Boolean;
    procedure CheckPackageLocations;

    function Text(const Name: TInstallInfoText): WideString; overload;
    function Text(const Name: TInstallInfoText; const Args: array of const): WideString; overload;

    property TempPath: string read FTempPath;

    property EditionTitle: WideString read FAppName;

    property MsiLocations: TInstallInfoFileLocations read FMsiLocations;
    property MsiInstallLocation: TInstallInfoFileLocation read FMsiInstallLocation write FMsiInstallLocation;

    // Keyman Installer information
    property InstalledVersion: TMSIInfo read FInstalledVersion write FInstalledVersion;
    property IsInstalled: Boolean read FIsInstalled;
    property IsNewerAvailable: Boolean read FIsNewerAvailable;

    property Packages: TInstallInfoPackages read FPackages;
    property TitleImageFilename: string read FTitleImageFilename;
    property StartDisabled: Boolean read FStartDisabled;
    property StartWithConfiguration: Boolean read FStartWithConfiguration;

    property Tier: string read FTier write FTier;

    property ShouldInstallKeyman: Boolean read FShouldInstallKeyman write FShouldInstallKeyman;
  end;

implementation

uses
  System.RegularExpressions,
  System.StrUtils,
  System.TypInfo,
  System.Zip,
  Winapi.Windows,

  Keyman.Setup.System.MsiUtils,
  Keyman.Setup.System.SetupUILanguageManager,
  KeymanVersion,
  kmpinffile,
  utilfiletypes,
  versioninfo;

function FileSizeByName(const AFilename: string): Int64; forward;

{ TInstallInfo }

constructor TInstallInfo.Create(const ATempPath: string);
begin
  inherited Create;
  FTempPath := ATempPath;
  FAppName := SKeymanDesktopName;
  FMsiLocations := TInstallInfoFileLocations.Create;
  FPackages := TInstallInfoPackages.Create;
  FStrings := TStringList.Create;
  FTier := KeymanVersion.CKeymanVersionInfo.Tier;
  FShouldInstallKeyman := True;
end;

destructor TInstallInfo.Destroy;
begin
  FreeAndNil(FPackages);
  FreeAndNil(FStrings);
  FreeAndNil(FMsiLocations);
  inherited Destroy;
end;

function TInstallInfo.GetBestMsi: TInstallInfoFileLocation;
var
  location: TInstallInfoFileLocation;
begin
  if FBestMsi <> nil then
    Exit(FBestMsi);

  if FMsiLocations.Count = 0 then
    Exit(nil);

  Result := FMsiLocations[0];
  for location in FMsiLocations do
    case CompareVersions(Result.Version, location.Version) of
      0: if Result.LocationType = iilOnline then Result := location; // prefer local for same version
      1: Result := location;
    end;

  FBestMsi := Result;
end;

function TInstallInfo.Text(const Name: TInstallInfoText): WideString;
var
  s: WideString;
begin
  s := GetEnumName(TypeInfo(TInstallInfoText), Ord(Name));
  Result := FStrings.Values[s];
  if Result = '' then
    Result := TSetupUILanguageManager.Get(Name);

  Result := ReplaceText(Result, '$VERSION', SKeymanVersion);
  Result := ReplaceText(Result, '$APPNAME', FAppName);
end;

procedure TInstallInfo.LoadSetupInf(const SetupInfPath: string);
var
  i: Integer;
  FInSetup: Boolean;
  FInPackages: Boolean;
  FInStrings: Boolean;
  location: TInstallInfoFileLocation;
  val, nm: WideString;
  pack: TInstallInfoPackage;
  packLocation: TInstallInfoPackageFileLocation;
  FVersion, FMSIFileName: string;
begin
  FInSetup := False;
  FInPackages := False;
  FInStrings := False;

  with TStringList.Create do
  try
    LoadFromFile(SetupInfPath + 'setup.inf');  // We'll just use the preamble for encoding  // I3476

    for i := 0 to Count - 1 do
    begin
      if Trim(Strings[i]) = '' then Continue;

      nm := KeyNames[i];
      val := ValueFromIndex[i];

      if Copy(Strings[i], 1, 1) = '[' then
      begin
        FInSetup := WideSameText(Strings[i], '[Setup]');
        FInPackages := WideSameText(Strings[i], '[Packages]');
        FInStrings := WideSameText(Strings[i], '[Strings]');
      end
      else if FInSetup then
      begin
        if WideSameText(nm, 'AppName') then FAppName := val
        else if WideSameText(nm, 'MSIFileName') then FMSIFileName := SetupInfPath + val
        else if WideSameText(nm, 'TitleImage') then FTitleImageFileName := SetupInfPath + val
        else if WideSameText(nm, 'StartWithConfiguration') then FStartWithConfiguration := StrToBoolDef(val, False)
        else if WideSameText(nm, 'StartDisabled') then FStartDisabled := StrToBoolDef(val, False);
      end
      else if FInPackages then
      begin
        if System.SysUtils.FileExists(SetupInfPath + nm) then
        begin
          pack := FPackages.FindById(ChangeFileExt(nm, ''), True);
          packLocation := TInstallInfoPackageFileLocation.Create(iilLocal);
          packLocation.Path := SetupInfPath + nm;
          pack.Locations.Add(packLocation);
          // Previously, the Name and Version of the package would be read from
          // setup.inf. Now, we load these details from the packages themselves
        end;
      end
      else if FInStrings then
        FStrings.Add(Strings[i]);
    end;

    if System.SysUtils.FileExists(FMSIFileName) then  // I3476
    begin
      FVersion := GetMsiVersion(FMSIFileName);
      if FVersion <> '' then
      begin
        location := TInstallInfoFileLocation.Create(iilLocal);
        location.Path := FMSIFileName;
        location.Version := FVersion;
        FMsiLocations.Add(location);
      end;
    end;

    if not System.SysUtils.FileExists(FTitleImageFileName) then  // I3476
      FTitleImageFileName := '';
  finally
    Free;
  end;
end;

procedure TInstallInfo.LocatePackagesFromParameter(const Param: string);
var
  n: Integer;
  opt, res: TArray<string>;
  id, FBCP47: string;
begin
  res := TRegEx.Split(Param, ',');

  for n := 0 to Length(res) - 1 do
  begin
    opt := TRegEx.Split(res[n], '=');
    id := opt[0].ToLower.Trim;
    if Length(opt) > 1 then FBCP47 := opt[1].ToLower.Trim else FBCP47 := '_';
    FPackages.Add(TInstallInfoPackage.Create(id, FBCP47));
  end;
end;

procedure TInstallInfo.LocatePackagesAndTierFromFilename(Filename: string);
const
  SKeymanSetupPrefix = 'keyman-setup';
  SKeymanSetup_Alpha = SKeymanSetupPrefix+'-'+TIER_ALPHA;
  SKeymanSetup_Beta = SKeymanSetupPrefix+'-'+TIER_BETA;
  SKeymanSetup_Stable = SKeymanSetupPrefix+'-'+TIER_STABLE;
var
  n: Integer;
  res: TArray<string>;
  p, id, FBCP47: string;
  m: TMatch;
begin
  // Get just the base filename
  Filename := ExtractFileName(ChangeFileExt(Filename, ''));

  // Strip " (1)" appended for multiple downloads of same file by most browsers
  m := TRegEx.Match(Filename, '^('+SKeymanSetupPrefix+'.+) \(\d+\)$');
  if m.Success then
    Filename := m.Groups[1].Value;

  // Look for our recognised pattern of keyman-setup.package_id.bcp47...
  res := TRegEx.Split(Filename, '\.');
  if (Length(res) < 1) or not res[0].ToLower.StartsWith(SKeymanSetupPrefix) then
    // No packages embedded in filename, or not a recognised filename pattern
    Exit;

  // Look for an embedded tier in the filename, if not set, use default
  p := res[0].ToLower;
  if p.Equals(SKeymanSetup_Stable) then FTier := TIER_STABLE
  else if p.Equals(SKeymanSetup_Beta) then FTier := TIER_BETA
  else if p.Equals(SKeymanSetup_Alpha) then FTier := TIER_ALPHA;

  n := 1;
  while n < Length(res) do
  begin
    id := res[n].ToLower.Trim;
    Inc(n);
    if n < Length(res) then FBCP47 := res[n].ToLower.Trim else FBCP47 := '_';
    Inc(n);

    FPackages.Add(TInstallInfoPackage.Create(id, FBCP47));
  end;
end;

procedure TInstallInfo.LoadLocalPackagesMetadata;
var
  pack: TInstallInfoPackage;
  packLocation: TInstallInfoPackageFileLocation;
  lang: TPackageKeyboardLanguage;
  iipl: TInstallInfoPackageLanguage;
begin
  for pack in FPackages do
  begin
    for packLocation in pack.Locations do
    begin
      if (packLocation.LocationType = iilLocal) and GetPackageMetadata(packLocation.Path, packLocation.LocalPackage) then
      begin
        packLocation.Size := FileSizeByName(packLocation.Path);
        packLocation.Name := packLocation.LocalPackage.Info.Desc[PackageInfo_Name];
        packLocation.Version := packLocation.LocalPackage.Info.Desc[PackageInfo_Version];

        if packLocation.LocalPackage.Keyboards.Count = 1 then
        begin
          // We only populate the language list if there is a single keyboard
          // in the package. Otherwise, we let Keyman install default lang
          // for each keyboard in the package.
          for lang in packLocation.LocalPackage.Keyboards[0].Languages do
          begin
            iipl := TInstallInfoPackageLanguage.Create(lang.ID, lang.Name);
            packLocation.Languages.Add(iipl);
          end;
        end;
      end;
    end;
  end;
end;

function TInstallInfo.GetPackageMetadata(const KmpFilename: string; p: TPackage): Boolean;
var
  zip: TZipFile;
  ms: TStream;
  h: TZipHeader;
begin
  try
    zip := TZipFile.Create;
    try
      zip.Open(KmpFilename, zmRead);
      if zip.IndexOf('kmp.json') >= 0 then
      begin
        // Read kmp.json
        zip.Read('kmp.json', ms, h);
        try
          ms.Position := 0;
          p.LoadJSONFromStream(ms);
        finally
          ms.Free;
        end;
        Exit(True);
      end
      else if zip.IndexOf('kmp.inf') >= 0 then
      begin
        // Read kmp.inf (phooey)
        zip.Extract('kmp.inf', FTempPath, False);
        p.FileName := FTempPath + 'kmp.inf';
        p.LoadIni;
        Exit(True);
      end
      else
        // Not a valid package
        Exit(False);
    finally
      zip.Free;
    end;
  except
    on E:EZipException do
      // TODO: log this to the logfile, but don't show
      Exit(False);
  end;
end;


function TInstallInfo.Text(const Name: TInstallInfoText;
  const Args: array of const): WideString;
begin
  Result := WideFormat(Text(Name), Args);
end;

function TInstallInfo.CheckMsiUpgradeScenarios: Boolean;
begin
    // Keyman install scenarios
  // 1. Keyman latest version already installed --> nothing to do
  // 2. Keyman outdated version already installed --> optional upgrade
  // 3. Keyman not installed --> install required

  // Keyman source scenarios
  // A. Available locally, latest version (or working offline)
  // B. Available locally, outdated version; online update available
  // C. Not available locally, online update available
  // D. Not available (working offline)

  // Matrix:
  // 1ABCD: Nothing to do
  // 2A: offer to upgrade from local
  // 2B: offer to upgrade from online (don't offer local update even if newer than current install, just for simplicity?)
  // 2C: offer to upgrade from online
  // 2D: Nothing to do
  // 3A: require install
  // 3B: offer to install from online (if update not taken, require install from local)
  // 3C: require download
  // 3D: Fail

  FBestMsi := GetBestMsi;
  FMsiInstallLocation := FBestMsi;

  FIsInstalled := FInstalledVersion.Version <> '';

  if not Assigned(FBestMsi) then
    // Matrix 123D
    FIsNewerAvailable := False
  else if FIsInstalled then
    // Matrix 12ABC
    FIsNewerAvailable := CompareVersions(FInstalledVersion.Version, FBestMsi.Version) > 0
  else
    // Matrix 3ABC
    FIsNewerAvailable := True;

  Result := FIsInstalled or FIsNewerAvailable;
end;

procedure TInstallInfo.CheckPackageLocations;
var
  pack: TInstallInfoPackage;
begin
  for pack in FPackages do
    pack.InstallLocation := pack.GetBestLocation;
end;

{ TInstallInfoPackage }

constructor TInstallInfoPackage.Create(const AID: string; const ABCP47: string = '');
begin
  inherited Create;
  FLocations := TInstallInfoPackageFileLocations.Create;
  FID := AID;
  if ABCP47 <> '_' then
    FBCP47 := ABCP47;
  FShouldInstall := True;
end;

destructor TInstallInfoPackage.Destroy;
begin
  FreeAndNil(FLocations);
  inherited Destroy;
end;

function TInstallInfoPackage.GetBestLocation: TInstallInfoPackageFileLocation;
var
  location: TInstallInfoPackageFileLocation;
begin
  if FLocations.Count = 0 then
    Exit(nil);

  Result := FLocations[0];

  for location in FLocations do
    case CompareVersions(Result.Version, location.Version) of
      0: if Result.LocationType = iilOnline then Result := location; // prefer local for same version
      1: Result := location;
    end;
end;

{ TInstallInfoPackageLanguage }

constructor TInstallInfoPackageLanguage.Create(const ABCP47, AName: string);
begin
  inherited Create;
  FBCP47 := ABCP47;
  FName := AName;
end;

{ TInstallInfoPackageFileLocations }

function TInstallInfoPackageFileLocations.LatestVersion(
  const default: string): string;
var
  location: TInstallInfoPackageFileLocation;
begin
  Result := default;
  for location in Self do
    if CompareVersions(Result, location.Version) > 0 then
      Result := location.Version;
end;

{ TInstallInfoFileLocations }

function TInstallInfoFileLocations.LatestVersion(const default: string): string;
var
  location: TInstallInfoFileLocation;
begin
  Result := default;
  for location in Self do
    if CompareVersions(Result, location.Version) > 0 then
      Result := location.Version;
end;

{ TInstallInfoPackages }

function TInstallInfoPackages.FindById(const id: string; createIfNotFound: Boolean): TInstallInfoPackage;
begin
  for Result in Self do
    if SameText(id, Result.ID) then Exit;

  if createIfNotFound then
  begin
    Result := TInstallInfoPackage.Create(id);
    Add(Result);
  end
  else
    Result := nil;
end;

{ TInstallInfoPackageFileLocation }

constructor TInstallInfoPackageFileLocation.Create(ALocationType: TInstallInfoLocationType);
begin
  inherited Create(ALocationType);
  FLanguages := TInstallInfoPackageLanguages.Create;
  FLocalPackage := TKmpInfFile.Create;
end;

destructor TInstallInfoPackageFileLocation.Destroy;
begin
  FreeAndNil(FLanguages);
  FreeAndNil(FLocalPackage);
  inherited Destroy;
end;

function TInstallInfoPackageFileLocation.GetLanguageNameFromBCP47(
  const bcp47: string): string;
var
  lang: TInstallInfoPackageLanguage;
begin
  Result := '';
  for lang in FLanguages do
    if SameText(lang.BCP47, bcp47) then
      Exit(lang.Name);
end;

function TInstallInfoPackageFileLocation.GetNameOrID(const id: string): string;
begin
  if FName = '' then
    Result := id
  else
    Result := FName;
end;

{ TInstallInfoFileLocation }

constructor TInstallInfoFileLocation.Create(
  ALocationType: TInstallInfoLocationType);
begin
  inherited Create;
  FLocationType := ALocationType;
end;

procedure TInstallInfoFileLocation.UpgradeToLocalPath(const ARootPath: string);
begin
  Assert(FLocationType = iilOnline);
  Assert(FileExists(ARootPath + FPath));
  FPath := ARootPath + FPath;
  FLocationType := iilLocal;
end;



// Pulled from IdGlobalProtocols so we don't have to import it
// OS-independent version
function FileSizeByName(const AFilename: string): Int64;
var
  LHandle : THandle;
  LRec : TWin32FindData;
begin
  Result := -1;

  LHandle := Winapi.Windows.FindFirstFile(PChar(AFileName), LRec);
  if LHandle <> INVALID_HANDLE_VALUE then
  begin
    Winapi.Windows.FindClose(LHandle);
    if (LRec.dwFileAttributes and Winapi.Windows.FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      Result := (Int64(LRec.nFileSizeHigh) shl 32) + LRec.nFileSizeLow;
    end;
  end;
end;

end.
