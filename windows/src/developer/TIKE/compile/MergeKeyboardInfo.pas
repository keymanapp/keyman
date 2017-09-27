{  Merges a source .keyboard_info file with data programatically extracted from a .kmp file and a keyboard .js file

  - Command line parameters:
     kmcomp -m <keyboard.kmp> <keyboard.js> <keyboard.keyboard_info>

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
  # jsFilename -- from $keyboard_info_jsFilename
  # encodings -- from .kmx (existence of .js implies unicode)
  # packageIncludes -- from kmp.inf?
  # version -- from kmp.inf, js
  # minKeymanDesktopVersion -- from kmp.inf, kmx
  # platformSupport -- deduce from whether kmp exists, js exists
}
unit MergeKeyboardInfo;

interface

uses
  System.Json,
  compile,
  kmpinffile,
  packageinfo,
  TempFileManager,
  kmxfile;

type
  TKeyboardInfoMap = record
    Filename: string;
    Info: TKeyboardInfo;
  end;

  TMergeKeyboardInfo = class
  private
    json: TJSONObject;
    FSilent: Boolean;
    FCallback: TCompilerCallback;
    FBaseName, FJsFile, FKmpFile, FJsonFile: string;
    FMergingValidateIds: Boolean;
    FKMPInfFile: TKMPInfFile;
    FKMXFiles: array of TKeyboardInfoMap;
    FJSFileData: string;
    FVersion: string;
    function Failed(message: string): Boolean;
    function Execute: Boolean; overload;
    function LoadJsonFile: Boolean;
    function LoadKMPFile: Boolean;
    function LoadJSFile: Boolean;
    constructor Create(AJsFile, AKmpFile, AJsonFile: string; AMergingValidateIds, ASilent: Boolean; ACallback: TCompilerCallback);
    procedure AddAuthor;
    procedure AddAuthorEmail;
    procedure CheckOrAddEncodings;
    procedure CheckOrAddID;
    procedure CheckOrAddJsFilename;
    procedure AddLastModifiedDate;
    procedure CheckOrAddMinKeymanDesktopVersion;
    procedure AddName;
    procedure CheckOrAddPackageFilename;
    procedure AddPackageIncludes;
    procedure AddPlatformSupport;
    procedure CheckOrAddVersion;
    function SaveJsonFile: Boolean;
    function LoadKMXFile: Boolean;
    procedure CheckKMXFilenames;
  public
    destructor Destroy; override;
    class function Execute(AJsFile, AKmpFile, AJsonFile: string; AMergingValidateIds, ASilent: Boolean; ACallback: TCompilerCallback): Boolean; overload;
  end;

implementation

uses
  Soap.XsBuiltIns,

  System.Classes,
  System.RegularExpressions,
  System.SysUtils,
  System.Zip,

  JsonUtil,
  utilfiletypes;

type
  EInvalidKeyboardInfo = class(Exception)
  end;

{ TMergeKeyboardInfo }

class function TMergeKeyboardInfo.Execute(AJsFile, AKmpFile, AJsonFile: string;
  AMergingValidateIds, ASilent: Boolean; ACallback: TCompilerCallback): Boolean;
begin
  with TMergeKeyboardInfo.Create(AJsFile, AKmpFile, AJsonFile, AMergingValidateIds, ASilent, ACallback) do
  try
    Result := Execute;
  finally
    Free;
  end;
end;

constructor TMergeKeyboardInfo.Create(AJsFile, AKmpFile, AJsonFile: string;
  AMergingValidateIds, ASilent: Boolean; ACallback: TCompilerCallback);
begin
  inherited Create;

  FMergingValidateIds := AMergingValidateIds;
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

    CheckKMXFileNames;

    CheckOrAddID;
    AddName;
    AddAuthor;
    AddAuthorEmail;
    AddLastModifiedDate;

    CheckOrAddVersion;  // must be called before CheckOrAddJsFilename

    CheckOrAddPackageFilename;
    CheckOrAddJsFilename;
    CheckOrAddEncodings;
    AddPackageIncludes;
    CheckOrAddMinKeymanDesktopVersion;
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

function TMergeKeyboardInfo.LoadKMXFile: Boolean;
begin
  SetLength(FKMXFiles, 1);
  FKMXFiles[0].Filename := FKMPFile;
  GetKeyboardInfo(FKMPFile, False, FKMXFiles[0].Info, False);
  Result := True;
end;

function TMergeKeyboardInfo.LoadKMPFile: Boolean;
var
  FKMXTempFile, FKMPInfTempFile: TTempFile;
  i: Integer;
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

  if SameText(ExtractFileExt(FKMPFile), '.kmx') then
    Exit(LoadKMXFile);

  try
    Zip := TZipFile.Create;
    try
      Zip.Open(FKMPFile, zmRead);
      FKMPInfTempFile := TTempFileManager.Get('.inf');
      try
        SaveMemberToFile('kmp.inf', FKMPInfTempFile.Name);
        FKMPInfFile := TKMPInfFile.Create;
        FKMPInfFile.FileName := FKMPInfTempFile.Name;
        FKMPInfFile.LoadIni;
      finally
        FKMPInfTempFile.Free;
      end;

      FKMXTempFile := TTempFileManager.Get('.kmx');
      try
        for i := 0 to High(Zip.FileNames) do
        begin
          if SameText(ExtractFileExt(Zip.FileNames[i]), '.kmx') then
          begin
            SetLength(FKMXFiles, Length(FKMXFiles)+1);

            SaveMemberToFile(Zip.FileNames[i], FKMXTempFile.Name);

            FKMXFiles[High(FKMXFiles)].Filename := Zip.FileNames[i];
            GetKeyboardInfo(FKMXTempFile.Name, False, FKMXFiles[High(FKMXFiles)].Info, False);
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
    FJSFileData := DataString;
  finally
    Free;
  end;

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
  FCallback(0, 0, PAnsiChar(AnsiString(Message)));
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
  else if FJSFileData <> '' then
  begin
    with TRegEx.Match(FJsFileData, 'this\.KN="([^"]+)"') do
    begin
      if Success
        then FName := Groups[1].Value
        else Exit;
    end;
  end
  else
    Exit;

  json.AddPair('id', FName);
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
    if ChangeFileExt(FFilename, '') <> FBaseName+'-'+FVersion then
      raise EInvalidKeyboardInfo.CreateFmt('jsFilename field is "%s" but should be "%s-%s.js"',
        [FFilename, FVersion, FBaseName]);
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
  for i := 0 to High(FKMXFiles) do
    encodings := encodings + FKMXFiles[i].Info.Encodings;

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
//  version -- from kmp.inf, js
//
procedure TMergeKeyboardInfo.CheckOrAddVersion;
var
  v: TJSONValue;
begin
  if Assigned(FKMPInfFile) then
  begin
    FVersion := FKMPInfFile.Info.Desc[PackageInfo_Version];
  end
  else if FJsFileData <> '' then
  begin
    with TRegEx.Match(FJsFileData, 'this\.KBVER=([''"])([^''"]+)(\1)') do
      if Success then FVersion := Groups[2].Value;
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
//  minKeymanDesktopVersion -- from kmp.inf, kmx
//
procedure TMergeKeyboardInfo.CheckOrAddMinKeymanDesktopVersion;
var
  i: Integer;
  MinVersion: Cardinal;
  MinVersionString: string;
  v: TJSONValue;
begin
  MinVersion := $0500;
  // For each .kmx, get encodings and add to the result
  for i := 0 to High(FKMXFiles) do
    if FKMXFiles[i].Info.FileVersion > MinVersion then
      MinVersion := FKMXFiles[i].Info.FileVersion;

  MinVersionString := Format('%d.0', [MinVersion shr 8]);

  v := json.GetValue('minKeymanDesktopVersion');
  if v <> nil then
  begin
    if Length(FKMXFiles) = 0 then
      raise EInvalidKeyboardInfo.Create('minKeymanDesktopVersion field should not be present');

    if v.Value <> MinVersionString then
      raise EInvalidKeyboardInfo.CreateFmt('minKeymanDesktopVersion field is "%s" but should be "%s"', [v.Value, MinVersionString]);
  end
  else
  begin
    if Length(FKMXFiles) = 0 then Exit;
    json.AddPair('minKeymanDesktopVersion', MinVersionString);
  end;
end;

//
//  platformSupport -- deduce from whether kmp exists, js exists
//
procedure TMergeKeyboardInfo.AddPlatformSupport;
var
  v: TJSONArray;
begin
  if json.GetValue('platformSupport') <> nil then Exit;

  v := TJSONArray.Create;

  if Assigned(FKMPInfFile) then
  begin
    v.Add('windows');
    v.Add('macos');
  end
  else if Length(FKMXFiles) > 0 then
    v.Add('windows');

  if FJsFile <> '' then
  begin
    v.Add('desktopWeb');
    v.Add('mobileWeb');
    v.Add('android');
    v.Add('ios');
  end;

  json.AddPair('platformSupport', v);
end;

procedure TMergeKeyboardInfo.CheckKMXFilenames;
begin
  // Check that the id of the kmx files matches the filename; used only for release/ keyboards
  // in the keyboards repository. This implies there should be only 1 kmx in release/ keyboard
  // packages; this check would not be done in the packages folder.
  if FMergingValidateIds and Assigned(FKMPInfFile) then
  begin
    if Length(FKMXFiles) <> 1 then
      raise EInvalidKeyboardInfo.Create('There should be exactly 1 .kmx file in the package.');

    if ChangeFileExt(FKMXFiles[0].Filename, '') <> FBaseName then
      raise EInvalidKeyboardInfo.CreateFmt('The file "%s" file in the package has the wrong filename. It should be "%s.kmx"',
        [FKMXFiles[0].Filename, FBaseName]);
  end;
end;


end.
