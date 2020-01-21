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

uses WinApi.Windows, System.Classes, System.SysUtils, TagFunctions;

procedure Run;

implementation

uses
  WinApi.ActiveX,
  System.Win.ComObj,
  Xml.XMLIntf,
  Xml.XMLDoc;

// This is the default build number used when doing a build outside of CI, and is
// appended to the release version found in resources/version.md
const
  S_DebugBuildVersion = '9999';

type
  TMKVerMode = (mmUnknown, mmSetRootVersionFromBuild, mmSetRootVersionFromVersionMd, mmWriteVersionedFile, mmWriteManifestFile);
var
  FMode: TMKVerMode = mmUnknown;
  BuildVersion, ResourceMdFilename: string;
  RootTemplateFileName, TemplateFileName, ResourceFileName: string;
  UpdateFiles: TStringList;

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

function Init: Boolean;
var
  s: string;
  n: Integer;
begin
  Result := False;

  FMode := mmUnknown;

  s := LowerCase(ParamStr(1));

  if s = '-c' then
  begin
    RootTemplateFileName := ParamStr(2);
    if SameText(ParamStr(3), '-b') then
    begin
      FMode := mmSetRootVersionFromBuild;
      BuildVersion := ParamStr(4);
    end
    else if SameText(ParamStr(3), '-r') then
    begin
      FMode := mmSetRootVersionFromVersionMd;
      ResourceMdFilename := ParamStr(4);
    end
    else
      Exit;
  end
  else if s = '-v' then
  begin
    FMode := mmWriteVersionedFile;
    n := 2;
    while ParamStr(n) = '-u' do begin Inc(n); UpdateFiles.Add(ParamStr(n)+'='+ParamStr(n+1)); Inc(n,2); end;
    TemplateFileName := ParamStr(n);
    if TemplateFileName = '' then Exit;
    ResourceFileName := ParamStr(n+1);
    if ResourceFileName = '' then ResourceFileName := 'version.rc';
  end
  else if s = '-m' then
  begin
    n := 2;
    FMode := mmWriteManifestFile;
    TemplateFileName := ParamStr(n);
    if TemplateFileName = '' then Exit;
    ResourceFileName := 'manifest.xml';
  end
  else
    Exit;

  Result := True;
end;

function GetCommaValue(var t: string): Integer;
var
  n: Integer;
begin
  if t = '' then raise Exception.Create('Cannot find valid PRODUCTVERSION.');
  n := Pos(',', t); if n = 0 then n := Length(t) + 1;
  Result := StrToInt(Copy(t, 1, n-1));
  Delete(t, 1, n);
end;

// Version number format:
//
//   5.1.22.0
//     5  is major
//     1  is minor
//     22 is build
//     0  is reserved for later use
//

procedure UpdateResource;
var
  i: Integer;
  ProductVersion, FileFlags, StringCompanyName: string;
  StringLegalCopyright, StringLegalTrademarks: string;
  StringProductName, StringProductVersion: string;
  xml: IXMLDocument;
begin
  writeln('Updating file version');

  with TStringList.Create do
  try
    LoadFromFile(TemplateFileName);
    for i := 0 to Count - 1 do
      case GetTagValue(Strings[i], 1, PredefTags) of
        TAG_PRODUCTVERSION: ProductVersion := GetTag(Strings[i], 2);
        TAG_FILEFLAGS:      FileFlags      := GetTag(Strings[i], 2);
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

  StringProductVersion := ProductVersion;
  for i := 1 to Length(StringProductVersion) do
    if StringProductVersion[i] = ',' then StringProductVersion[i] := '.';

  if ResourceFileName = 'manifest.xml' then
  begin
    xml := LoadXMLDocument(ChangeFileExt(ResourceFileName, '.in'));
    xml.DocumentElement.ChildNodes['assemblyIdentity'].Attributes['version'] := StringProductVersion;
    xml.SaveToFile(ResourceFileName);
    xml := nil;
  end
  else
  begin
    StringProductVersion := '"' + StringProductVersion + '\0"';
    with TStringList.Create do
    try
      LoadFromFile(ChangeFileExt(ResourceFileName, '.in'));

      for i := 0 to Count - 1 do
        case GetTagValue(Strings[i], 1, PredefTags) of
          TAG_FILEVERSION:    Strings[i] := UpdateTag(Strings[i], 2, ProductVersion);
          TAG_PRODUCTVERSION: Strings[i] := UpdateTag(Strings[i], 2, ProductVersion);
          TAG_FILEFLAGS:      Strings[i] := UpdateTag(Strings[i], 2, FileFlags);
          TAG_VALUE:
            case GetTagValue(Strings[i], 2, StringTags) of
              TAG2_COMPANYNAME:      Strings[i] := UpdateTag(Strings[i], 3, StringCompanyName);
              TAG2_FILEDESCRIPTION:  ;
              TAG2_FILEVERSION:      Strings[i] := UpdateTag(Strings[i], 3, StringProductVersion);
              TAG2_INTERNALNAME:     ;
              TAG2_LEGALCOPYRIGHT:   Strings[i] := UpdateTag(Strings[i], 3, StringLegalCopyright);
              TAG2_LEGALTRADEMARKS:  Strings[i] := UpdateTag(Strings[i], 3, StringLegalTrademarks);
              TAG2_ORIGINALFILENAME: ;
              TAG2_PRODUCTNAME:      Strings[i] := UpdateTag(Strings[i], 3, StringProductName);
              TAG2_PRODUCTVERSION:   Strings[i] := UpdateTag(Strings[i], 3, StringProductVersion);
              TAG2_COMMENTS:         ;
            end;
        end;
      SaveToFile(ResourceFileName);
    finally
      Free;
    end;
  end;
end;

procedure UpdateFile(const fin, fout: string);
var
  tfi, tfo: TextFile;
  ProductVersion, s: string;
  ProductVersionNum, ProductVersionCvs: string;
  i, n: Integer;
  v: Integer;
  FGUID: array[0..9] of string;
  g: TGUID;
  ProductReleaseMajor, ProductReleaseMinor, ProductRelease: string;
begin
  for i := 0 to 9 do
  begin
    CreateGUID(g);
    FGUID[i] := GUIDToString(g);
  end;
  ProductVersion := '';
  writeln('Updating file version for '+fin);
  with TStringList.Create do
  try
    LoadFromFile(TemplateFileName);
    for i := 0 to Count - 1 do
      case GetTagValue(Strings[i], 1, PredefTags) of
        TAG_PRODUCTVERSION: ProductVersion := GetTag(Strings[i], 2);
      end;
  finally
    Free;
  end;

  if ProductVersion = '' then Exit;
  ProductVersionNum := ProductVersion;
  ProductVersionCvs := ProductVersion;
  for i := 1 to Length(ProductVersion) do
  begin
    if ProductVersion[i] = ',' then ProductVersion[i] := '.';
    if ProductVersionCvs[i] = ',' then ProductVersionCvs[i] := '-';
  end;

  ProductRelease := ProductVersion;
  i := Pos('.', ProductRelease, Pos('.', ProductRelease)+1);
  if i > 0 then
    Delete(ProductRelease, i, MaxInt);

  ProductReleaseMajor := Copy(ProductRelease, 1, Pos('.', ProductRelease)-1);
  ProductReleaseMinor := Copy(ProductRelease, Pos('.', ProductRelease)+1, MAXINT);

  AssignFile(tfi, fin);
  AssignFile(tfo, fout);
  Reset(tfi);
  Rewrite(tfo);

  while not EOF(tfi) do
  begin
    readln(tfi, s);

    s := StringReplace(s, '$VERSIONNUM', ProductVersionNum, [rfReplaceAll]);
    s := StringReplace(s, '$VERSIONCVS', ProductVersionCvs, [rfReplaceAll]);
    s := StringReplace(s, '$VERSION', ProductVersion, [rfReplaceAll]);

    s := StringReplace(s, '$RELEASE_MAJOR', ProductReleaseMajor, [rfReplaceAll]);
    s := StringReplace(s, '$RELEASE_MINOR', ProductReleaseMinor, [rfReplaceAll]);
    s := StringReplace(s, '$RELEASE', ProductRelease, [rfReplaceAll]);

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

procedure WriteRootVersionTemplateFromBuild;
begin
  // Transform the string from 1.2.3.4 to 1,2,3,4 for version.txt/version.rc format
  BuildVersion := StringReplace(BuildVersion, '.', ',', [rfReplaceAll]);

  with TStringList.Create do
  try
    Add('PRODUCTVERSION '+BuildVersion);
    SaveToFile(RootTemplateFileName);
  finally
    Free;
  end;
end;

procedure WriteRootVersionTemplateFromVersionMd;
begin
  with TStringList.Create do
  try
    LoadFromFile(ResourceMdFilename);
    BuildVersion := Strings[0];
  finally
    Free;
  end;

  // Transform the string from 1.2 to 1,2 to match version.txt/version.rc format
  BuildVersion := StringReplace(BuildVersion, '.', ',', [rfReplaceAll]);

  // And add the static 9999 version
  BuildVersion := BuildVersion + ',' + S_DebugBuildVersion+',0';

  with TStringList.Create do
  try
    Add('PRODUCTVERSION '+BuildVersion);
    SaveToFile(RootTemplateFileName);
  finally
    Free;
  end;
end;

procedure Run;
var
  i: Integer;
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

  writeln('KMVer: Keyman project version information increment');
  UpdateFiles := TStringList.Create;
  if not Init then
  begin
    UpdateFiles.Free;
    writeln(#13#10+'Usage: kmver -c <root-version.txt> (-b <version> | -r <VERSION.md>)');
    writeln(' or    kmver -v [-u f.in f.out]* <template> [version.rc]');
    writeln('   -c:         Sets the root template version');
    writeln('       -r:     Use major.minor version data from VERSION.md and appends .9999.0 to indicate custom built version');
    writeln('       -b:     For CI, use the specific version number specified');
    writeln('   -v:         Update the version.rc with the template information');
    writeln('   -m:         Update manifest.xml with the template information');
    writeln('   -u:         Update file f.in to f.out, replacing (multiple entries okay):');
    writeln('                          $VERSION     1.2.3.4');
    writeln('                          $VERSIONNUM  9,0,700,0');
    writeln('                          $VERSIONCVS  9-0-700-0');
    writeln('                          $RELEASE     1.2');
    writeln('                          $GUID#       GUID 0-9');
    writeln('                          $DATE        1 March 2018');
    writeln('   template:   The source template, usually \keyman\windows\src\<dir>\version.txt');
    writeln('   root:       The source template, usually \keyman\windows\src\version.txt');
    writeln('   version.rc: version.rc file to update');
    ExitCode := 2;
    Exit;
  end;

  if FMode = mmSetRootVersionFromBuild then
    WriteRootVersionTemplateFromBuild
  else if FMode = mmSetRootVersionFromVersionMd then
    WriteRootVersionTemplateFromVersionMd
  else
  begin
    if UpdateFiles.Count = 0 then
      UpdateResource;

    for i := 0 to UpdateFiles.Count - 1 do
      UpdateFile(UpdateFiles.Names[i], UpdateFiles.Values[UpdateFiles.Names[i]]);
  end;

  UpdateFiles.Free;
  CoUninitialize;
end;

end.

