{  Merges a source .keyboard_info file with data programatically extracted from a .kmp file and a keyboard .js file

  - Command line parameters:
     kmcomp -m <keyboard.kmp> <keyboard.js> [-add-help-link <help path>] <keyboard.keyboard_info>

  Note: if keyboard.kmp or keyboard.js do not exist, merge_compiled_keyboard_info will continue to work, just will
        not attempt to pull data from them

  #
  # There are a number of fields we can fill in programatically.
  #

  # id -- from .keyboard_info name
  # name -- from kmp.inf, js
  # authorName -- from kmp.inf
  # authorEmail -- from kmp.inf
  # lastModifiedDate -- build time: this only gets refreshed when the version num increments so it's close enough then
  # packageFilename -- from $keyboard_info_packageFilename
  # packageFileSize -- get from the size of the file
  # jsFilename -- from $keyboard_info_jsFilename
  # jsFileSize -- get from the size of the file
  # isRTL -- from .js, KRTL\s*=\s*1
  # encodings -- from .kmx (existence of .js implies unicode)
  # packageIncludes -- from kmp.inf?
  # version -- from kmp.inf, js
  # minKeymanVersion -- from kmp.inf, kmx, js
  # platformSupport -- deduce from whether kmp exists, js exists
  # languages -- given the BCP 47 ids, generate the subtag names:
  #              displayName, languageName - (required)
  #              scriptName, regionName    - if not blank
}
unit MergeKeyboardInfo;

interface

uses
  System.Json,
  compile,
  kmpinffile,
  packageinfo,
  TempFileManager,
  kmxfile,
  kmxfileconsts,
  Keyman.Developer.System.Project.ProjectLog,
  UKeymanTargets;

type
  TKeyboardInfoMap = record
    Filename: string;
    Info: TKeyboardInfo;
  end;

  TJSKeyboardInfoMap = record
    Filename: string;
    Data: string;
    Standalone: Boolean;
  end;

  TMergeKeyboardInfo = class
  private
    json: TJSONObject;
    FSilent: Boolean;
    FCallback: TProjectLogObjectEvent;
    FBaseName, FJsFile, FKmpFile, FJsonFile: string;
    FMergingValidateIds: Boolean;
    FKMPInfFile: TKMPInfFile;
    FPackageKMXFileInfos: array of TKeyboardInfoMap;
    FPackageJSFileInfos: array of TKeyboardInfoMap;
    FJSFileInfo: TJSKeyboardInfoMap;
    FVersion: string;
    FSourcePath: string;
    FHelpLink: string;
    function Failed(message: string): Boolean;
    function Execute: Boolean; overload;
    function LoadJsonFile: Boolean;
    function LoadKMPFile: Boolean;
    function LoadJSFile: Boolean;
    constructor Create(ASourcePath, AJsFile, AKmpFile, AJsonFile, AHelpLink: string; AMergingValidateIds, ASilent: Boolean; ACallback: TProjectLogObjectEvent);
    procedure AddAuthor;
    procedure AddAuthorEmail;
    procedure CheckOrAddEncodings;
    procedure CheckOrAddFileSizes;
    procedure CheckOrAddID;
    procedure CheckOrAddJsFilename;
    procedure AddLastModifiedDate;
    procedure CheckOrAddMinKeymanVersion;
    procedure AddName;
    procedure CheckOrAddPackageFilename;
    procedure AddPackageIncludes;
    procedure AddHelpLink;
    procedure AddPlatformSupport;
    procedure CheckOrAddVersion;
    procedure AddSubtagNames(id: String; o: TJSONObject);
    procedure CheckOrMigrateLanguages;
    function SaveJsonFile: Boolean;
    procedure CheckPackageKeyboardFilenames;
    procedure AddIsRTL;
    procedure AddSourcePath;
  public
    destructor Destroy; override;
    class function Execute(ASourcePath, AJsFile, AKmpFile, AJsonFile, AHelpLink: string; AMergingValidateIds, ASilent: Boolean; ACallback: TProjectLogObjectEvent): Boolean; overload;
  end;

implementation

uses
  Soap.XsBuiltIns,

  System.Classes,
  System.Generics.Collections,
  System.RegularExpressions,
  System.SysUtils,
  System.Zip,

  Keyman.System.RegExGroupHelperRSP19902,

  BCP47Tag,
  JsonUtil,
  Keyman.System.KeyboardInfoFile,
  Keyman.System.KeyboardUtils,
  Keyman.System.LanguageCodeUtils,
  utilfiletypes,
  VersionInfo;

type
  EInvalidKeyboardInfo = class(Exception)
  end;

{ TMergeKeyboardInfo }

class function TMergeKeyboardInfo.Execute(ASourcePath, AJsFile, AKmpFile, AJsonFile, AHelpLink: string;
  AMergingValidateIds, ASilent: Boolean; ACallback: TProjectLogObjectEvent): Boolean;
begin
  with TMergeKeyboardInfo.Create(ASourcePath, AJsFile, AKmpFile, AJsonFile, AHelpLink, AMergingValidateIds, ASilent, ACallback) do
  try
    Result := Execute;
  finally
    Free;
  end;
end;

constructor TMergeKeyboardInfo.Create(ASourcePath, AJsFile, AKmpFile, AJsonFile, AHelpLink: string;
  AMergingValidateIds, ASilent: Boolean; ACallback: TProjectLogObjectEvent);
begin
  inherited Create;

  FSourcePath := ASourcePath;
  FMergingValidateIds := AMergingValidateIds;
  FHelpLink := AHelpLink;
  FSilent := ASilent;
  FCallback := ACallback;

  if not SameText(ExtractFileExt(AJsFile), '.js') then
  begin
    FKmpFile := AJsFile;
    FJsFile := AKmpFile;
  end
  else
  begin
    FKmpFile := AKmpFile;
    FJsFile := AJsFile;
  end;

  FJsonFile := AJsonFile;

  FBaseName := ChangeFileExt(ExtractFileName(FJsonFile), '');
end;

destructor TMergeKeyboardInfo.Destroy;
begin
  inherited Destroy;
  json.Free;
  FKMPInfFile.Free;
end;

function TMergeKeyboardInfo.Execute: Boolean;
begin
  try
    if not LoadJsonFile then
      Exit(Failed('Could not parse keyboard_info file '+FJsonFile));

    if not LoadKMPFile then
      Exit(Failed('Could not load KMP file '+FKmpFile));

    if not LoadJSFile then
      Exit(Failed('Could not load JS file '+FJsFile));

    CheckPackageKeyboardFilenames;

    CheckOrAddID;
    CheckOrMigrateLanguages;
    AddName;
    AddIsRTL;
    AddAuthor;
    AddAuthorEmail;
    AddLastModifiedDate;
    AddSourcePath;

    CheckOrAddVersion;  // must be called before CheckOrAddJsFilename

    CheckOrAddPackageFilename;
    CheckOrAddJsFilename;
    CheckOrAddEncodings;
    CheckOrAddFileSizes;
    AddPackageIncludes;
    CheckOrAddMinKeymanVersion;
    AddHelpLink;
    AddPlatformSupport;

    if not SaveJsonFile then
      Exit(Failed('Could not save updated keyboard_info file '+FJsonFile));
  except
    on E:EInvalidKeyboardInfo do
    begin
      Failed('Invalid .keyboard_info file: '+E.Message);
      Exit(False);
    end;
    on E:Exception do
    begin
      Failed('Fatal error '+E.ClassName+': '+E.Message);
      Exit(False);
    end;
  end;

  Result := True;
end;

function TMergeKeyboardInfo.LoadJsonFile: Boolean;
begin
  try
    with TStringStream.Create('', TEncoding.UTF8) do
    try
      LoadFromFile(FJsonFile);
      json := TJSONObject.ParseJsonValue(DataString) as TJSONObject;
    finally
      Free;
    end;
  except
    on E:Exception do
      Exit(Failed(E.Message));
  end;

  Result := Assigned(json);
end;

function TMergeKeyboardInfo.LoadKMPFile: Boolean;
var
  FKMXTempFile, FKMPInfTempFile: TTempFile;
  i, j: Integer;
  LocalHeader: TZipHeader;
  Zip: TZipFile;

  procedure SaveMemberToFile(MemberFilename, OutFilename: string);
  var
    ZipMemberStream, OutFileStream: TStream;
  begin
    OutFileStream := TFileStream.Create(OutFilename, fmCreate);
    try
      Zip.Read(MemberFilename, ZipMemberStream, LocalHeader);
      try
        OutFileStream.CopyFrom(ZipMemberStream, 0);
      finally
        ZipMemberStream.Free;
      end;
    finally
      OutFileStream.Free;
    end;
  end;

begin
  if FKMPFile = '' then
    Exit(True);

  if not SameText(ExtractFileExt(FKMPFile), '.kmp') then
    Exit(Failed('packageFile must be a .kmp file '+FKMPFile));

  try
    Zip := TZipFile.Create;
    try
      Zip.Open(FKMPFile, zmRead);

      FKMPInfFile := TKMPInfFile.Create;

      if Zip.IndexOf('kmp.json') >= 0 then
      begin
        FKMPInfTempFile := TTempFileManager.Get('.json');
        try
          FKMPInfFile.FileName := FKMPInfTempFile.Name;
          SaveMemberToFile('kmp.json', FKMPInfTempFile.Name);
          FKMPInfFile.LoadJson;
        finally
          FKMPInfTempFile.Free;
        end;
      end
      else
      begin
        FKMPInfTempFile := TTempFileManager.Get('.inf');
        try
          FKMPInfFile.FileName := FKMPInfTempFile.Name;
          SaveMemberToFile('kmp.inf', FKMPInfTempFile.Name);
          FKMPInfFile.LoadIni;
        finally
          FKMPInfTempFile.Free;
        end;
      end;

      FKMXTempFile := TTempFileManager.Get('.kmx');
      try
        if FKMPInfFile.Keyboards.Count > 0 then
        begin
          for i := 0 to FKMPInfFile.Keyboards.Count - 1 do
          begin
            for j := 0 to High(Zip.FileNames) do
            begin
              // Add the KMX to FPackageKMXFileinfos
              if SameText(Zip.FileName[j], FKMPInfFile.Keyboards[i].ID + '.kmx') then
              begin
                SetLength(FPackageKMXFileInfos, Length(FPackageKMXFileInfos)+1);
                SaveMemberToFile(Zip.FileNames[j], FKMXTempFile.Name);
                FPackageKMXFileInfos[High(FPackageKMXFileInfos)].Filename := Zip.FileNames[j];
                GetKeyboardInfo(FKMXTempFile.Name, False, FPackageKMXFileInfos[High(FPackageKMXFileInfos)].Info, False);
              end;

              // Add the JS to FPackageJSFileInfos
              if SameText(Zip.FileNames[j], FKMPInfFile.Keyboards[i].ID + '.js') then
              begin
                SetLength(FPackageJSFileInfos, Length(FPackageJSFileInfos)+1);
                FPackageJSFileInfos[High(FPackageJSFileInfos)].Filename := Zip.FileNames[j];

                // Apply JS keyboard only to mobile targets, because web is not supported
                // in a package. If a package does not support mobile, it should not include
                // the .js.
                // Not using GetKeyboardInfo because that only handles kmx files
                FPackageJSFileInfos[High(FPackageJSFileInfos)].Info.Targets := 'mobile';
              end;
            end;
          end;
        end

        // Handle keyboard packages which may not have [Keyboard] sections defined in kmp.inf.
        // Note: They will never include any js keyboards.
        else if FKMPInfFile.Keyboards.Count = 0 then
        begin
          for i := 0 to High(Zip.FileNames) do
          begin
            if IsKeyboardFile(Zip.FileName[i]) then
            begin
              SetLength(FPackageKMXFileInfos, Length(FPackageKMXFileInfos)+1);
              SaveMemberToFile(Zip.FileNames[i], FKMXTempFile.Name);
              FPackageKMXFileInfos[High(FPackageKMXFileInfos)].Filename := Zip.FileNames[i];
              GetKeyboardInfo(FKMXTempFile.Name, False, FPackageKMXFileInfos[High(FPackageKMXFileInfos)].Info, False);
            end;
          end;
        end;

      finally
          FKMXTempFile.Free;
      end;
    finally
      FreeAndNil(Zip);
    end;
  except
    on E:EZipException do
      Exit(Failed(E.Message));
  end;

  Result := True;
end;

function TMergeKeyboardInfo.LoadJSFile: Boolean;
begin
  if FJsFile = '' then
    Exit(True);

  with TStringStream.Create('', TEncoding.UTF8) do
  try
    LoadFromFile(FJsFile);
    FJSFileInfo.Filename := FJsFile;
    FJSFileInfo.Data := DataString;
  finally
    Free;
  end;

  FJSFileInfo.Standalone := True;
  Result := True;
end;

function TMergeKeyboardInfo.SaveJsonFile: Boolean;
var
  str: TStringList;
begin
  str := TStringList.Create;
  try
    PrettyPrintJSON(json, str);
    with TStringStream.Create(str.Text, TEncoding.UTF8) do
    try
      // Use TStringStream so we don't get a default BOM prolog
      SaveToFile(FJsonFile);
    finally
      Free;
    end;
  finally
    str.Free;
  end;
  Result := True;
end;

function TMergeKeyboardInfo.Failed(message: string): Boolean;
begin
  FCallback(plsError, FJsonFile, message, 0, 0);
  Result := False;
end;

//
// id -- from .keyboard_info name
//
procedure TMergeKeyboardInfo.CheckOrAddID;
var
  v: TJSONValue;
  FID: string;
begin
  FID := ExtractFileName(ChangeFileExt(FJsonFile, ''));

  v := json.GetValue('id');
  if v <> nil then
  begin
    if v.Value <> FID then
      raise EInvalidKeyboardInfo.CreateFmt('id field is "%s" but should be "%s"', [v.Value, FID]);
  end
  else
    json.AddPair('id', FID);
end;

procedure TMergeKeyboardInfo.AddSubtagNames(id: String; o: TJSONObject);
var
  displayName, languageName, scriptName, regionName: String;
  v: TJSONValue;
  bcp47Tag: TBCP47Tag;
begin
  if id = '' then
    Exit;
  bcp47Tag := TBCP47Tag.Create(id);
  try
    TLanguageCodeUtils.BCP47Languages.TryGetValue(bcp47Tag.Language, languageName);
    TLanguageCodeUtils.BCP47Scripts.TryGetValue(bcp47Tag.Script, scriptName);
    TLanguageCodeUtils.BCP47Regions.TryGetValue(bcp47Tag.Region, regionName);
  finally
    bcp47Tag.Free;
  end;

  displayName := TLanguageCodeUtils.LanguageName(languageName, scriptName, regionName);

  v := o.Values[TKeyboardInfoFile.SDisplayName];
  if not Assigned(v) then
    o.AddPair(TKeyboardInfoFile.SDisplayName, displayName);

  v := o.Values[TKeyboardInfoFile.SLanguageName];
  if not Assigned(v) then
    o.AddPair(TKeyboardInfoFile.SLanguageName, languageName);

  if scriptName <> '' then
  begin
    v := o.Values[TKeyboardInfoFile.SScriptName];
    if not Assigned(v) then
      o.AddPair(TKeyboardInfoFile.SScriptName, scriptName);
  end;

  if regionName <> '' then
  begin
    v := o.Values[TKeyboardInfoFile.SRegionName];
    if not Assigned(v) then
      o.AddPair(TKeyboardInfoFile.SRegionName, regionName);
  end;
end;

procedure TMergeKeyboardInfo.CheckOrMigrateLanguages;
var
  v: TJSONValue;
  alangs: TJSONArray;
  olangs, o: TJSONObject;
  pair: TJSONPair;
  i: Integer;
  id: string;
begin
  v := json.GetValue(TKeyboardInfoFile.SLanguages);

  if v is TJSONArray then
  begin
    // Migrate languages[] array to Object
    alangs := v as TJSONArray;
    olangs := TJSONObject.Create;
    for i := 0 to alangs.Count - 1 do
    begin
      id := alangs.Items[i].Value;
      if id = '' then
        continue;

      // Populate subtag names
      o := TJSONObject.Create;
      olangs.AddPair(id, o);
      AddSubtagNames(id, o);
    end;

    json.RemovePair(TKeyboardInfoFile.SLanguages);
    json.AddPair(TKeyboardInfoFile.SLanguages, olangs);
  end
  else if v is TJSONObject then
  begin
    olangs := v as TJsonObject;
    for i := 0 to olangs.Count - 1 do
    begin
      pair := olangs.Pairs[i];
      id := pair.JSONString.Value;
      if id = '' then
        continue;

      // Populate subtag names
      o := pair.JsonValue as TJSONObject;
      AddSubtagNames(id, o);
    end;
  end;
end;

//
// name -- from kmp.inf, js
//
procedure TMergeKeyboardInfo.AddName;
var
  FName: string;
begin
  if json.GetValue('name') <> nil then Exit;

  if Assigned(FKMPInfFile) then
    FName := FKMPInfFile.Info.Desc[PackageInfo_Name]
  else if not FJSFileInfo.Data.IsEmpty then
  begin
    with TRegEx.Match(FJSFileInfo.Data, 'this\.KN="([^"]+)"') do
    begin
      if Success
        then FName := TGroupHelperRSP19902.Create(Groups[1], FJSFileInfo.Data).FixedValue
        else Exit;
    end;
  end
  else
    Exit;

  json.AddPair('name', FName);
end;

//
// isRTL -- from js; if more than one, just first one
//
procedure TMergeKeyboardInfo.AddIsRTL;
begin
  if json.GetValue('isRTL') <> nil then Exit;

  if (FJSFileInfo.Data <> '') then
  begin
    with TRegEx.Match(FJSFileInfo.Data, 'this\.KRTL=1') do
    begin
      if not Success then Exit;
    end;
  end
  else
    Exit;

  json.AddPair('isRTL', TJsonTrue.Create);
end;

//
//  authorName -- from kmp.inf
//
procedure TMergeKeyboardInfo.AddAuthor;
var
  FName: string;
begin
  if json.GetValue('authorName') <> nil then Exit;
  if not Assigned(FKMPInfFile) then Exit;

  FName := Trim(FKMPInfFile.Info.Desc[PackageInfo_Author]);
  if FName = '' then Exit;
  json.AddPair('authorName', FName);
end;

//
//  authorEmail -- from kmp.inf
//
procedure TMergeKeyboardInfo.AddAuthorEmail;
var
  FEmail: string;
begin
  if json.GetValue('authorEmail') <> nil then Exit;
  if not Assigned(FKMPInfFile) then Exit;

  FEmail := FKMPInfFile.Info.URL[PackageInfo_Author];
  if Copy(FEmail, 1, 7) <> 'mailto:' then Exit;
  json.AddPair('authorEmail', Copy(FEMail,8,MaxInt));
end;

//
//  lastModifiedDate -- build time? Is this
//  good enough, and if not, how can we solve this?
//
procedure TMergeKeyboardInfo.AddLastModifiedDate;
var
  FDateTime: string;
begin
  if json.GetValue('lastModifiedDate') <> nil then Exit;

  with TXSDateTime.Create do
  try
    AsDateTime := Now;
    AsUTCDateTime := AsUTCDateTime; // Converts to UTC
    FDateTime := NativeToXS;
  finally
    Free;
  end;

  json.AddPair('lastModifiedDate', FDateTime);
end;

//
//  packageFilename -- from $keyboard_info_packageFilename
//
procedure TMergeKeyboardInfo.CheckOrAddPackageFilename;
var
  FFilename: string;
  v: TJSONValue;
begin
  FFilename := ExtractFileName(FKmpFile);
  v := json.GetValue('packageFilename');
  if v <> nil then
  begin
    if FKmpFile = '' then
      raise EInvalidKeyboardInfo.CreateFmt('packageFilename field is "%s" but that package is not present.', [v.Value])
    else if v.Value <> FFilename then
      raise EInvalidKeyboardInfo.CreateFmt('packageFilename field is "%s" but should be "%s"', [v.Value, FFilename]);
  end
  else
  begin
    if FKmpFile = '' then Exit;
    json.AddPair('packageFilename', FFilename);
  end;

  // Check that the id of the keyboard matches the filename; used only for release/ keyboards
  // in the keyboards repository
  if FMergingValidateIds then
  begin
    if ChangeFileExt(FFilename, '') <> FBaseName then
      raise EInvalidKeyboardInfo.CreateFmt('packageFilename field is "%s" but should be "%s.kmp"',
        [FFilename, FBaseName]);
  end;
end;

//
//  jsFilename -- from $keyboard_info_jsFilename
//
procedure TMergeKeyboardInfo.CheckOrAddJsFilename;
var
  FFilename: string;
  v: TJSONValue;
begin
  FFilename := ExtractFileName(FJsFile);
  v := json.GetValue('jsFilename');
  if v <> nil then
  begin
    if FJsFile = '' then
      raise EInvalidKeyboardInfo.CreateFmt('jsFilename field is "%s" but that file is not present.', [v.Value])
    else if v.Value <> FFilename then
      raise EInvalidKeyboardInfo.CreateFmt('jsFilename field is "%s" but should be "%s"', [v.Value, FFilename]);
  end
  else
  begin
    if FJsFile = '' then Exit;
    json.AddPair('jsFilename', FFilename);
  end;

  // Check that the id of the keyboard matches the filename; used only for release/ keyboards
  // in the keyboards repository
  if FMergingValidateIds then
  begin
    if ChangeFileExt(FFilename, '') <> FBaseName then
      raise EInvalidKeyboardInfo.CreateFmt('jsFilename field is "%s" but should be "%s.js"',
        [FFilename, FBaseName]);
  end;
end;

//
//  encodings -- from .kmx (existence of .js implies unicode)
//
procedure TMergeKeyboardInfo.CheckOrAddEncodings;
var
  i: Integer;
  encodingsc, encodings: TKIEncodings;
  v: TJSONArray;
  vc: TJSONArray;
begin
  encodings := [];
  // For each .kmx, get encodings and add to the result
  for i := 0 to High(FPackageKMXFileInfos) do
    encodings := encodings + FPackageKMXFileInfos[i].Info.Encodings;

  if FJsFile <> '' then Include(encodings, keUnicode);


  v := TJSONArray.Create;
  if keANSI in encodings then
    v.Add('ansi');

  if keUnicode in encodings then
    v.Add('unicode');

  vc := json.GetValue('encodings') as TJSONArray;
  if vc <> nil then
  begin
    encodingsc := [];
    for i := 0 to vc.Count - 1 do
      if vc.Items[i].Value = 'ansi' then Include(encodingsc, keANSI)
      else if vc.Items[i].Value = 'unicode' then Include(encodingsc, keUnicode);

    if encodingsc <> encodings then
      raise EInvalidKeyboardInfo.CreateFmt('encodings field is "%s" but should be "%s"', [vc.ToJSON, v.ToJSON]);
  end
  else
    json.AddPair('encodings', v);
end;

//
// packageFileSize, jsFileSize, all from the actual files
//
procedure TMergeKeyboardInfo.CheckOrAddFileSizes;
  procedure DoFileSize(prefix: string);
  var
    vs, v: TJSONValue;
    f: TSearchRec;
  begin
    v := json.GetValue(prefix+'Filename');
    if v <> nil then
    begin
      if FindFirst(ExtractFilePath(FJsonFile)+v.Value, 0, f) <> 0 then
        raise EInvalidKeyboardInfo.CreateFmt('Unable to locate file %s to check its size', [v.Value]);
      FindClose(f);
      vs := json.GetValue(prefix+'FileSize');
      if vs = nil then
        json.AddPair(prefix+'FileSize', TJSONNumber.Create(f.Size))
      else
      begin
        if f.Size <> (vs as TJSONNumber).AsInt64 then
          raise EInvalidKeyboardInfo.CreateFmt('File size for %s is recorded as %d but should be %d.',
            [v.Value, (vs as TJSONNumber).AsInt64, f.Size]);
      end;
    end;
  end;

begin
  DoFileSize('js');
  DoFileSize('package');
end;

//
//  packageIncludes -- from kmp.inf?
//
procedure TMergeKeyboardInfo.AddPackageIncludes;
var
  i: Integer;
  id, ext, name: string;
  v: TJSONArray;
  j: Integer;
  Found: Boolean;
begin
  if json.GetValue('packageIncludes') <> nil then Exit;
  if not Assigned(FKMPInfFile) then Exit;

  //"welcome", "documentation", "fonts", "visualKeyboard"

  v := TJSONArray.Create;
  for i := 0 to FKMPInfFile.Files.Count - 1 do
  begin
    name := FKMPInffile.Files[i].FileName;
    ext := ExtractFileExt(name);
    id := '';

    if SameText(name, 'welcome.htm') then id := 'welcome'
    else if SameText(ext, '.kvk') then id := 'visualKeyboard'
    else if SameText(ext, '.rtf') then id := 'documentation'
    else if SameText(ext, '.html') then id := 'documentation'
    else if SameText(ext, '.htm') then id := 'documentation'
    else if SameText(ext, '.pdf') then id := 'documentation'
    else if SameText(ext, '.ttf') then id := 'fonts'
    else if SameText(ext, '.otf') then id := 'fonts'
    else if SameText(ext, '.ttc') then id := 'fonts';

    if (id <> '') then
    begin
      Found := False;
      for j := 0 to v.Count - 1 do
        if v.Items[j].Value = id then
        begin
          Found := True;
          Break;
        end;
      if not Found then
        v.Add(id);
    end;
  end;

  json.AddPair('packageIncludes', v);
end;

//
//  version -- from kmp.inf, js (if more than 1, then just first)
//
procedure TMergeKeyboardInfo.CheckOrAddVersion;
var
  v: TJSONValue;
begin
  if Assigned(FKMPInfFile) then
  begin
    FVersion := FKMPInfFile.Info.Desc[PackageInfo_Version];
  end
  else if (FJSFileInfo.Data <> '') then
  begin
    with TRegEx.Match(FJSFileInfo.Data, 'this\.KBVER=([''"])([^''"]+)(\1)') do
      if Success then FVersion := TGroupHelperRSP19902.Create(Groups[2], FJSFileInfo.Data).FixedValue;
  end;

  if FVersion = '' then
    FVersion := '1.0';

  v := json.GetValue('version');
  if v <> nil then
  begin
    if v.Value <> FVersion then
      raise EInvalidKeyboardInfo.CreateFmt('version field is "%s" but should be "%s"', [v.Value, FVersion]);
  end
  else
    json.AddPair('version', FVersion);
end;

//
//  minKeymanVersion -- from kmp.inf, kmx, js
//
procedure TMergeKeyboardInfo.CheckOrAddMinKeymanVersion;
var
  i: Integer;
  MinVersion: Cardinal;
  MinVersionString: string;
  FJSMinVersionString: string;
  v: TJSONValue;
  s: string;
begin
  MinVersion := $0500;
  // For each .kmx, get minimum version and add to the result
  for i := 0 to High(FPackageKMXFileInfos) do
    if FPackageKMXFileInfos[i].Info.FileVersion > MinVersion then
      MinVersion := FPackageKMXFileInfos[i].Info.FileVersion;

  FJSMinVersionString := '';
  // See also Keyman.System.KeyboardJSInfo.pas, CompilePackage.pas
  with TRegEx.Match(FJSFileInfo.Data, 'this.KMINVER\s*=\s*([''"])(.*?)\1') do
  begin
    if Success then
    begin
      s := TGroupHelperRSP19902.Create(Groups[2], FJSFileInfo.Data).FixedValue;
      if (FJSMinVersionString = '') or (CompareVersions(FJSMinVersionString, s) > 0) then
        FJSMinVersionString := s;
    end;
  end;

  MinVersionString := Format('%d.%d', [(MinVersion and VERSION_MASK_MAJOR) shr 8, (MinVersion and VERSION_MASK_MINOR)]);
  if FJSMinVersionString <> '' then
    if CompareVersions(MinVersionString, FJSMinVersionString) > 0 then
      MinVersionString := FJSMinVersionString;

  v := json.GetValue('minKeymanVersion');
  if v <> nil then
  begin
    if v.Value <> MinVersionString then
      raise EInvalidKeyboardInfo.CreateFmt('minKeymanVersion field is "%s" but should be "%s"', [v.Value, MinVersionString]);
  end
  else
    json.AddPair('minKeymanVersion', MinVersionString);
end;

//
// Add helpLink, from commandline parameter
//
procedure TMergeKeyboardInfo.AddHelpLink;
var
  v: TJSONValue;
begin
  // Validate pre-existing helpLink
  v := json.GetValue('helpLink');
  if v <> nil then
  begin
    if v.Value.IndexOf('https://help.keyman.com/keyboard/') < 0 then
      raise EInvalidKeyboardInfo.Create('helpLink should be on https://help.keyman.com/keyboard/');
  end
  else
  begin
    if FHelpLink = '' then
      Exit;

    json.AddPair('helpLink', FHelpLink);
  end;
end;

//
//  platformSupport -- deduce from whether kmp exists, js exists
//
procedure TMergeKeyboardInfo.AddPlatformSupport;
var
  keyboardFile: TKeyboardInfoMap;
  target: TKeymanTarget;
  targets: TKeymanTargets;
  p, v: TJSONObject;
  numberKMXFiles, numberJSFiles : Integer;

  procedure AddNewPair(PairName: string; value: string);
  begin
    try
      if (v.GetValue(PairName) = nil) then
        v.AddPair(PairName, value);
    finally
    end;
  end;

  function packageContainsKeyboardJs(const id: string): Boolean;
  var
    kf: TKeyboardInfoMap;
  begin
    for kf in FPackageJSFileInfos do
      if SameText(TKeyboardUtils.KeyboardFileNameToID(kf.Filename), id) then
        Exit(True);

    Result := False;
  end;

begin
  try
    // Validate pre-existing platformSupport
    p := json.GetValue('platformSupport') as TJSONObject;
    if p <> nil then
    begin
      numberKMXFiles := Length(FPackageKMXFileInfos);
      if FJsFile <> '' then
      begin
        numberJSFiles := 1;
      end
      else
      begin
        numberJSFiles := Length(FPackageJSFileInfos);
      end;

      // Validate any target
      if (p.GetValue('any') <> nil) and ((numberKMXFiles = 0) or (numberJSFiles = 0)) then
        raise EInvalidKeyboardInfo.Create(
          '"Any" target should have at least 1 .kmx and 1 .js keyboard file in the package');

      // Validate desktop targets
      if ((p.GetValue('windows') <> nil) or (p.GetValue('macos') <> nil) or
          (p.GetValue('linux') <> nil) or (p.GetValue('desktop') <> nil)) and
          (numberKMXFiles = 0) then
        raise EInvalidKeyboardInfo.Create(
          'Desktop targets should have at least 1 .kmx keyboard file in the package');

      // Validate web/mobile targets. No kmx implies web/mobile targets
      if ((numberKMXFiles = 0) or (p.GetValue('web') <> nil) or
          (p.GetValue('iphone') <> nil) or (p.GetValue('ipad') <> nil) or
          (p.GetValue('androidphone') <> nil) or (p.GetValue('androidtablet') <> nil) or
          (p.GetValue('mobile') <> nil) or (p.GetValue('tablet') <> nil)) and
          (numberJSFiles = 0) then
        raise EInvalidKeyboardInfo.Create(
          'Web/mobile targets should have at least 1 .js keyboard file in the package');

      Exit;
    end;
  finally
  end;

  // Parse targets to populate platformSuppport
  v := TJSONObject.Create;

  for keyboardFile in FPackageKMXFileInfos do
  begin
    targets := StringToKeymanTargets(keyboardFile.Info.Targets);
    if ktAny in targets then
    begin
      AddNewPair('windows', 'full');
      AddNewPair('macos', 'full');
      AddNewPair('linux', 'full');
      if packageContainsKeyboardJs(TKeyboardUtils.KeyboardFileNameToId(keyboardFile.Filename)) then
      begin
        AddNewPair('android', 'full');
        AddNewPair('ios', 'full');
      end;
    end
    else
    begin
      // If targets empty, then include all desktop targets
      if (targets = []) then
      begin
        AddNewPair('windows', 'full');
        AddNewPair('macos', 'full');
        AddNewPair('linux', 'full');
      end;

      for target in targets do
      begin
        if target = ktDesktop then
        begin
          AddNewPair('windows', 'full');
          AddNewPair('macos', 'full');
          AddNewPair('linux', 'full');
        end;
        if target = ktWindows then
          AddNewPair('windows', 'full');
        if target = ktMacosx then
          AddNewPair('macos', 'full');
        if target = ktLinux then
          AddNewPair('linux', 'full');

        if packageContainsKeyboardJs(TKeyboardUtils.KeyboardFileNameToId(keyboardFile.Filename)) then
        begin
          // FPackageKMXFileInfos can contain target information for web/mobile targets.
          // This is a current limitation of FPackageJSFileInfos if there's no kmx files
          if (target = ktMobile) then
          begin
            AddNewPair('android', 'full');
            AddNewPair('ios', 'full');
          end;
          if (target = ktIphone) or (target = ktIpad) then
          begin
            AddNewPair('ios', 'full');
          end;
          if (target = ktAndroidphone) or (target = ktAndroidtablet) then
          begin
            AddNewPair('android', 'full');
          end;
        end;
      end;
    end;
  end;

  for keyboardFile in FPackageJSFileInfos do
  begin
    targets := StringToKeymanTargets(keyboardFile.Info.Targets);
    for target in targets do
    begin
      if (target = ktMobile) then
      begin
        AddNewPair('android', 'full');
        AddNewPair('ios', 'full');
      end;
      if (target = ktIphone) or (target = ktIpad) then
      begin
        AddNewPair('ios', 'full');
      end;
      if (target = ktAndroidphone) or (target = ktAndroidtablet) then
      begin
        AddNewPair('android', 'full');
      end;
    end;
  end;

  // Handle JS file not in kmp. Because it is isolated, we cannot detect
  // whether it supports mobile vs desktop web because that is not included
  // in the .js. So, for now we assume both.
  //
  // We no longer assume that the presence of a .js means support for
  // native mobile apps. These apps now work on the basis of having a
  // .kmp file available
  if (FJsFile <> '') then
  begin
    AddNewPair('desktopWeb', 'full');
    AddNewPair('mobileWeb', 'full');
  end;

  json.AddPair('platformSupport', v);
end;

//
// Add sourcePath field, from commandline parameter
//
procedure TMergeKeyboardInfo.AddSourcePath;
begin
  if FSourcePath = '' then
    Exit;

  if not TRegEx.IsMatch(FSourcePath, '^(release|legacy|experimental)\/.+\/.+$') then
    raise EInvalidKeyboardInfo.CreateFmt('The source path "%s" is an invalid format, '+
      'expecting "(release|legacy|experimental)/n/name".', [FSourcePath]);
  json.AddPair('sourcePath', FSourcePath);
end;

procedure TMergeKeyboardInfo.CheckPackageKeyboardFilenames;
var
  numberKMXFiles, numberJSFiles : Integer;
begin
  // Check that the id of the kmx/js files matches the base filename; used only for release/ keyboards
  // in the keyboards repository. This implies there should be only 1 kmx and/or 1 js in release/ keyboard
  // packages; this check would not be done in the packages folder.
  if FMergingValidateIds and Assigned(FKMPInfFile) then
  begin
    numberKMXFiles := Length(FPackageKMXFileInfos);
    numberJSFiles := Length(FPackageJSFileInfos);
    if (numberKMXFiles = 0) and (numberJSFiles = 0) then
      raise EInvalidKeyboardInfo.Create('There should be at least 1 .kmx or .js keyboard file in the package.');

    if numberKMXFiles > 1 then
    begin
      raise EInvalidKeyboardInfo.Create('There should be at most 1 .kmx keyboard file in the packge.');
    end
    else if (numberKMXFiles = 1) and (ChangeFileExt(FPackageKMXFileInfos[0].Filename, '') <> FBaseName) then
    begin
      raise EInvalidKeyboardInfo.CreateFmt('The file "%s" file in the package has the wrong filename. It should be "%s.kmx"',
        [FPackageKMXFileInfos[0].Filename, FBaseName]);
    end;

    if numberJSFiles > 1 then
    begin
      raise EInvalidKeyboardInfo.Create('There should be at most 1 .js keyboard file in the package.');
    end
    else if (numberJSFiles = 1) and (ChangeFileExt(FPackageJSFileInfos[0].Filename, '') <> FBaseName) then
    begin
      raise EInvalidKeyboardInfo.CreateFmt('The file "%s" file in the package has the wrong filename. It should be "%s.js"',
        [FPackageJSFileInfos[0].Filename, FBaseName]);
    end;

  end;
end;


end.
