(*
  Name:             Main
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      28 Sep 2006

  Modified Date:    26 Jun 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          28 Sep 2006 - mcdurdin - Added $GUID and fixed $DATE
                    11 Dec 2009 - mcdurdin - I1807 - Manifest files need version info automatically updated in build
                    04 May 2012 - mcdurdin - I3308 - V9.0 - Start to move towards Delphi namespaces
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
*)
unit Main;  // I3308

interface

uses
  WinApi.Windows,
  System.Classes,
  System.SysUtils,
  TagFunctions;

procedure Run;

implementation

uses
  WinApi.ActiveX,
  System.RegularExpressions,
  System.Win.ComObj,
  Xml.XMLIntf,
  Xml.XMLDoc,

  Keyman.System.KeymanVersionInfo;

procedure WriteHelp; forward;

type
  TMKVerMode = (mmUnknown, mmWriteVersionedFile, mmWriteManifestFile, mmWriteVersionRc);

const
  TAG_FILEVERSION       = 1;
  TAG_PRODUCTVERSION    = 2;
  TAG_FILEFLAGS         = 3;
  TAG_VALUE             = 4;

  TAG2_COMPANYNAME      = 1;
  TAG2_FILEDESCRIPTION  = 2;
  TAG2_FILEVERSION      = 3;
  TAG2_INTERNALNAME     = 4;
  TAG2_LEGALCOPYRIGHT   = 5;
  TAG2_LEGALTRADEMARKS  = 6;
  TAG2_ORIGINALFILENAME = 7;
  TAG2_PRODUCTNAME      = 8;
  TAG2_PRODUCTVERSION   = 9;
  TAG2_COMMENTS         = 10;

var
  PredefTags: array[1..4] of string =
    ('FILEVERSION', 'PRODUCTVERSION', 'FILEFLAGS', 'VALUE');

  StringTags: array[1..10] of string =
    ('"CompanyName",', '"FileDescription",', '"FileVersion",', '"InternalName",', '"LegalCopyright",',
     '"LegalTradmarks",', '"OriginalFilename",', '"ProductName",', '"ProductVersion",', '"Comments",');

function ConstructEnvironmentTag(const tier: string): string;
var
  TEAMCITY_VERSION, TEAMCITY_PR_NUMBER: string;
begin
  TEAMCITY_VERSION := GetEnvironmentVariable('TEAMCITY_VERSION');
  if TEAMCITY_VERSION = '' then
  begin
    // Local dev machine, not TeamCity
    Result := 'local';
  end
  else
  begin
    // On TeamCity; are we running a pull request build or a master/beta/stable build?
    TEAMCITY_PR_NUMBER := GetEnvironmentVariable('TEAMCITY_PR_NUMBER');
    if TEAMCITY_PR_NUMBER <> ''
      then Result := 'test'
      else Result := tier;
  end;
end;

function ConstructVersionTag(const tier: string): string;
var
  TEAMCITY_VERSION, TEAMCITY_PR_NUMBER: string;
begin
  // This matches the algorithm found in /resources/build/build-utils.sh

  if (tier = 'alpha') or (tier = 'beta') then
    Result := '-'+tier
  else
    Result := '';

  TEAMCITY_VERSION := GetEnvironmentVariable('TEAMCITY_VERSION');
  if TEAMCITY_VERSION = '' then
  begin
    // Local dev machine, not TeamCity
    Result := Result + '-local';
  end
  else
  begin
    // On TeamCity; are we running a pull request build or a master/beta/stable build?
    TEAMCITY_PR_NUMBER := GetEnvironmentVariable('TEAMCITY_PR_NUMBER');
    if TEAMCITY_PR_NUMBER <> '' then
    begin
      // Note TEAMCITY_PR_NUMBER can also be 'master', 'beta', or 'stable-x.y'
      // This indicates we are running a Test build.
      if TRegEx.IsMatch(TEAMCITY_PR_NUMBER, '^(master|beta|stable(-[0-9]+\.[0-9]+)?)$') then
      begin
        Result := Result + '-test';
      end
      else
        Result := Result + '-test-'+TEAMCITY_PR_NUMBER;
    end;
  end;
end;

function Init(var FMode: TMKVerMode; var TemplateFileName: string; UpdateFiles: TStringList; var VersionInfo: TKeymanVersionInfo): Boolean;
var
  s: string;
  n: Integer;
  versionFile, tierFile, version, tier, tag: string;
  environment: string;
begin
  FMode := mmUnknown;

  n := 1;
  s := ParamStr(n);
  while s <> '' do
  begin
    if s = '-u' then
    begin
      FMode := mmWriteVersionedFile;
      UpdateFiles.AddPair(ParamStr(n+1), ParamStr(n+2));
      Inc(n,3);
    end
    else if s = '-v' then
    begin
      FMode := mmWriteVersionRc;
      TemplateFileName := ParamStr(n+1);
      UpdateFiles.AddPair(ParamStr(n+2), ParamStr(n+3));
      Inc(n,4);
    end
    else if s = '-m' then
    begin
      FMode := mmWriteManifestFile;
      UpdateFiles.AddPair(ParamStr(n+1), ParamStr(n+2));
      Inc(n,3);
    end
    else if s = '-version' then
    begin
      versionFile := ParamStr(n+1);
      Inc(n,2);
    end
    else if s = '-tier' then
    begin
      tierFile := ParamStr(n+1);
      Inc(n,2);
    end
    else
      // Unrecognised parameter
      Exit(False);

    s := ParamStr(n);
  end;

  if FMode = mmUnknown then
    Exit(False);

  with TStringList.Create do
  try
    LoadFromFile(versionFile);
    version := Trim(Text);
  finally
    Free;
  end;

  with TStringList.Create do
  try
    LoadFromFile(tierFile);
    tier := Trim(Text);
  finally
    Free;
  end;

  tag := ConstructVersionTag(tier);
  environment := ConstructEnvironmentTag(tier);

  VersionInfo := BuildKeymanVersionInfo(version, tier, tag, environment);

  Result := True;
end;

procedure UpdateResource(const template, fin, fout: string; VersionInfo: TKeymanVersionInfo);
var
  i: Integer;
  StringCompanyName: string;
  StringLegalCopyright, StringLegalTrademarks: string;
  StringProductName: string;
begin
  writeln('Updating file version');

  with TStringList.Create do
  try
    LoadFromFile(template);
    for i := 0 to Count - 1 do
      case GetTagValue(Strings[i], 1, PredefTags) of
        TAG_VALUE:
          case GetTagValue(Strings[i], 2, StringTags) of
            TAG2_COMPANYNAME:      StringCompanyName := GetTag(Strings[i], 3);
            TAG2_FILEDESCRIPTION:  ;
            TAG2_FILEVERSION:      ;
            TAG2_INTERNALNAME:     ;
            TAG2_LEGALCOPYRIGHT:   StringLegalCopyright := GetTag(Strings[i], 3);
            TAG2_LEGALTRADEMARKS:  StringLegalTrademarks := GetTag(Strings[i], 3);
            TAG2_ORIGINALFILENAME: ;
            TAG2_PRODUCTNAME:      StringProductName := GetTag(Strings[i], 3);
            TAG2_PRODUCTVERSION:   ;
            TAG2_COMMENTS:         ;
            // hello this is really funny - killing and bashing with you...
          end;
      end;
  finally
    Free;
  end;

  // I'm free!!!  I'm free!!!  I'm finally free!!  Thank God almighty I'm free at last! (end).

  with TStringList.Create do
  try
    LoadFromFile(fin);

    for i := 0 to Count - 1 do
      case GetTagValue(Strings[i], 1, PredefTags) of
        TAG_FILEVERSION:    Strings[i] := UpdateTag(Strings[i], 2, VersionInfo.VersionRc);
        TAG_PRODUCTVERSION: Strings[i] := UpdateTag(Strings[i], 2, VersionInfo.VersionRc);
        TAG_VALUE:
          case GetTagValue(Strings[i], 2, StringTags) of
            TAG2_COMPANYNAME:      Strings[i] := UpdateTag(Strings[i], 3, StringCompanyName);
            TAG2_FILEDESCRIPTION:  ;
            TAG2_FILEVERSION:      Strings[i] := UpdateTag(Strings[i], 3, '"'+VersionInfo.VersionWin+'\0"');
            TAG2_INTERNALNAME:     ;
            TAG2_LEGALCOPYRIGHT:   Strings[i] := UpdateTag(Strings[i], 3, StringLegalCopyright);
            TAG2_LEGALTRADEMARKS:  Strings[i] := UpdateTag(Strings[i], 3, StringLegalTrademarks);
            TAG2_ORIGINALFILENAME: ;
            TAG2_PRODUCTNAME:      Strings[i] := UpdateTag(Strings[i], 3, StringProductName);
            TAG2_PRODUCTVERSION:   Strings[i] := UpdateTag(Strings[i], 3, '"'+VersionInfo.VersionWin+'\0"');
            TAG2_COMMENTS:         ;
          end;
      end;
    SaveToFile(fout);
  finally
    Free;
  end;
end;

procedure UpdateManifest(const fin, fout: string; VersionInfo: TKeymanVersionInfo);
var
  xml: IXMLDocument;
begin
  xml := LoadXMLDocument(fin);
  xml.DocumentElement.ChildNodes['assemblyIdentity'].Attributes['version'] := VersionInfo.VersionWin;
  xml.SaveToFile(fout);
  xml := nil;
end;

procedure UpdateFile(const fin, fout: string; VersionInfo: TKeymanVersionInfo);
var
  tfi, tfo: TextFile;
  i, n: Integer;
  v: Integer;
  s: string;
  FGUID: array[0..9] of string;
  g: TGUID;
begin
  for i := 0 to 9 do
  begin
    CreateGUID(g);
    FGUID[i] := GUIDToString(g);
  end;
  writeln('Updating file version for '+fin);

  AssignFile(tfi, fin);
  AssignFile(tfo, fout);
  Reset(tfi);
  Rewrite(tfo);

  while not EOF(tfi) do
  begin
    readln(tfi, s);

    // Current replacements
    s := StringReplace(s, '$VersionWin',     VersionInfo.VersionWin,     [rfReplaceAll]);
    s := StringReplace(s, '$VersionRelease', VersionInfo.VersionRelease, [rfReplaceAll]);
    s := StringReplace(s, '$VersionMajor',   IntToStr(VersionInfo.VersionMajor), [rfReplaceAll]);
    s := StringReplace(s, '$VersionMinor',   IntToStr(VersionInfo.VersionMinor), [rfReplaceAll]);
    s := StringReplace(s, '$VersionPatch',   IntToStr(VersionInfo.VersionPatch), [rfReplaceAll]);
    s := StringReplace(s, '$Tier',           VersionInfo.Tier,           [rfReplaceAll]);
    s := StringReplace(s, '$Tag',            VersionInfo.Tag,            [rfReplaceAll]);
    s := StringReplace(s, '$VersionWithTag', VersionInfo.VersionWithTag, [rfReplaceAll]);
    s := StringReplace(s, '$VersionRc',      VersionInfo.VersionRc,      [rfReplaceAll]);
    s := StringReplace(s, '$Environment',    VersionInfo.Environment,    [rfReplaceAll]);
    // Do this one last because it breaks the others otherwise...
    s := StringReplace(s, '$Version',        VersionInfo.Version,        [rfReplaceAll]);

    // Legacy replacements
    // TODO(lowpri): replace these with above and eliminate
    s := StringReplace(s, '$VERSIONNUM', VersionInfo.VersionRc, [rfReplaceAll]);
    s := StringReplace(s, '$VERSION', VersionInfo.VersionWin, [rfReplaceAll]);

    s := StringReplace(s, '$RELEASE_MAJOR', IntToStr(VersionInfo.VersionMajor), [rfReplaceAll]);
    s := StringReplace(s, '$RELEASE_MINOR', IntToStr(VersionInfo.VersionMinor), [rfReplaceAll]);
    s := StringReplace(s, '$RELEASE', VersionInfo.VersionRelease, [rfReplaceAll]);


    n := Pos('$GUID', s);
    while n > 0 do
    begin
      Delete(s, n, 5);
      v := StrToIntDef(Copy(s, n, 1), -1);
      if v = -1 then
        v := 0
      else
        Delete(s,n,1);
      Insert(FGUID[v], s, n);
      n := Pos('$GUID', s);
    end;

    s := StringReplace(s, '$DATE', FormatDateTime('d mmmm yyyy', Now), [rfReplaceAll]);

    writeln(tfo, s);
  end;
  CloseFile(tfi);
  CloseFile(tfo);
end;

procedure Run;
var
  FMode: TMKVerMode;
  TemplateFileName: string;
  UpdateFiles: TStringList;
  i: Integer;
  VersionInfo: TKeymanVersionInfo;
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

  writeln('KMVer: Keyman project version information increment');

  FMode := mmUnknown;
  UpdateFiles := TStringList.Create;
  try
    if not Init(FMode, TemplateFileName, UpdateFiles, VersionInfo) then
    begin
      WriteHelp;
      ExitCode := 2;
      Exit;
    end;

    case FMode of
      mmWriteVersionedFile:
        for i := 0 to UpdateFiles.Count - 1 do
          UpdateFile(UpdateFiles.Names[i], UpdateFiles.ValueFromIndex[i], VersionInfo);

      mmWriteManifestFile:
        UpdateManifest(UpdateFiles.Names[0], UpdateFiles.ValueFromIndex[0], VersionInfo);
      mmWriteVersionRc:
        UpdateResource(TemplateFileName, UpdateFiles.Names[0], UpdateFiles.ValueFromIndex[0], VersionInfo);
    end;


  finally
    UpdateFiles.Free;
  end;
  CoUninitialize;
end;

procedure WriteHelp;
begin
  writeln;
  writeln('Usage: kmver -u f.in f.out [-u f.in f.out...] <common_parameters>');
  writeln(' or    kmver -v <project-version.in> <app-version.in> <version.rc> <common_parameters>');
  writeln(' or    kmver -m <manifest.xml> <common_parameters>');
  writeln('   -v:         Update the version.rc with the template information');
  writeln('   -m:         Update manifest.xml with the template information');
  writeln('   -u:         Update file f.in to f.out, replacing (multiple entries okay):');
  writeln;
  writeln('                   Legacy tags (case sensitive):');
  writeln('                          $VERSION       14.0.2.0');
  writeln('                          $VERSIONNUM    14,0,2,0');
  writeln('                          $RELEASE_MAJOR 14');
  writeln('                          $RELEASE_MINOR 0');
  writeln('                          $RELEASE       14.0');
  writeln('                          $GUID#         GUID 0-9');
  writeln('                          $DATE          1 March 2018');
  writeln;
  writeln('                   Current tags (case sensitive):');
  writeln('                          $Version         14.0.2');
  writeln('                          $VersionWin      14.0.2.0');
  writeln('                          $VersionRelease  14.0');
  writeln('                          $VersionMajor    14');
  writeln('                          $VersionMinor    0');
  writeln('                          $VersionPatch    2');
  writeln('                          $Tier            alpha');
  writeln('                          $Tag             -alpha-test-1234');
  writeln('                          $VersionWithTag  14.0.2-alpha-test-1234');
  writeln('                          $VersionRc       14,0,2,0');
  writeln;
  writeln('   project-version.in: The project version template, usually \keyman\windows\src\<dir>\version.in');
  writeln('   app-version.in: The app version template, usually \keyman\windows\src\<dir>\<app>\version.in');
  writeln('   version.rc: version.rc file to write');
  writeln;
  writeln('Common Parameters:');
  writeln('   -version <VERSION.md>  Path to VERSION.md (in root of repo)');
  writeln('   -tier <TIER>           Current tier (alpha, beta or stable)');
  writeln;
  writeln('Note: tag is constructed from TEAMCITY_VERSION and TEAMCITY_PR_NUMBER environment variables');
end;

end.

