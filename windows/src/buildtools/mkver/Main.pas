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
  Xml.XMLDoc,

  KeymanVersion;

var
  IncrementVersion, DebugVersion: Boolean;
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

  s := LowerCase(ParamStr(1));

  if s = '-i' then
  begin
    IncrementVersion := True;
    n := 2;
    if ParamStr(n) = '-d' then begin Inc(n); DebugVersion := True; end;
    TemplateFileName := ParamStr(n);
    RootTemplateFileName := ParamStr(n+1);
    if (TemplateFileName = '') or (RootTemplateFileName = '') then Exit;
  end
  else if s = '-v' then
  begin
    n := 2;
    while ParamStr(n) = '-u' do begin Inc(n); UpdateFiles.Add(ParamStr(n)+'='+ParamStr(n+1)); Inc(n,2); end;
    IncrementVersion := False;
    TemplateFileName := ParamStr(n);
    if TemplateFileName = '' then Exit;
    ResourceFileName := ParamStr(n+1);
    if ResourceFileName = '' then ResourceFileName := 'version.rc';
  end
  else if s = '-m' then
  begin
    n := 2;
    IncrementVersion := False;
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

procedure IncrementTemplate;
var
  i, n, j: Integer;
  pv: string;
begin
  writeln('Incrementing product version');
  pv := '';
  with TStringList.Create do
  try
    LoadFromFile(RootTemplateFileName);
    for i := 0 to Count - 1 do
      case GetTagValue(Strings[i], 1, PredefTags) of
        TAG_PRODUCTVERSION: // Update the major, minor, and build numbers (final num stays untouched for later use)
          pv := GetTag(Strings[i], 2);
      end;
  finally
    Free;
  end;

  if DebugVersion and (TemplateFileName = RootTemplateFileName) then
  begin
    n := 0;
    for i := 1 to Length(pv) do
    begin
      if pv[i] = ',' then inc(n);
      if n = 3 then
      begin
        j := StrToInt(Copy(pv, i+1, 100));
        pv := Copy(pv, 1, i) + IntToStr(j+1);
        Break;
      end;
    end;
  end;

  if pv = '' then raise Exception.Create('Could not retrieve product version');

  with TStringList.Create do
  try
    LoadFromFile(TemplateFileName);
    for i := 0 to Count - 1 do
      case GetTagValue(Strings[i], 1, PredefTags) of
        TAG_PRODUCTVERSION: // Update the major, minor, and build numbers (final num stays untouched for later use)
          begin
            Strings[i] := UpdateTag(Strings[i], 2, pv);
            writeln('New version is ' + pv);
          end;
        TAG_FILEFLAGS:  // Update VS_FF_DEBUG, VS_FF_PRERELEASE
          if DebugVersion
            then Strings[i] := UpdateTag(Strings[i], 2, '0x1L')
            else Strings[i] := UpdateTag(Strings[i], 2, '0x0L');
      end;
    SaveToFile(TemplateFileName);
  finally
    Free;
  end;
end;

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
    xml := LoadXMLDocument(ResourceFileName);
    xml.DocumentElement.ChildNodes['assemblyIdentity'].Attributes['version'] := StringProductVersion;
    xml.SaveToFile(ResourceFileName);
    xml := nil;
  end
  else
  begin
    StringProductVersion := '"' + StringProductVersion + '\0"';
    with TStringList.Create do
    try
      LoadFromFile(ResourceFileName);

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

  AssignFile(tfi, fin);
  AssignFile(tfo, fout);
  Reset(tfi);
  Rewrite(tfo);

  while not EOF(tfi) do
  begin
    readln(tfi, s);
    n := Pos('$VERSION', s);
    while n > 0 do
    begin
      if Copy(s, n, 11) = '$VERSIONNUM' then
      begin
        Delete(s, n, 11);
        Insert(ProductVersionNum, s, n);
      end
      else if Copy(s, n, 11) = '$VERSIONCVS' then
      begin
        Delete(s, n, 11);
        Insert(ProductVersionCvs, s, n);
      end
      else
      begin
        Delete(s, n, 8);
        Insert(ProductVersion, s, n);
      end;
      n := Pos('$VERSION', s);
    end;

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

    n := Pos('$DATE', s);
    while n > 0 do
    begin
      Delete(s, n, 5);
      Insert(FormatDateTime('d mmmm yyyy', Now), s, n);
      n := Pos('$DATE', s);
    end;

    writeln(tfo, s);
  end;
  CloseFile(tfi);
  CloseFile(tfo);
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
    writeln(#13#10+'Usage: kmver -i '{[-m|-n] }+'[-d] <template> <root>');
    writeln(' or    kmver -v [-u f.in f.out]* <template> [version.rc]');
    writeln('   -i:         Updates the template build version from src\version.txt');
    //writeln('   -m:         Also increments major version (resets minor version number)');
    //writeln('   -n:         Also increments minor version');
    writeln('   -d:         Debug version ');
    writeln('   -v:         Update the version.rc with the template information');
    writeln('   -m:         Update manifest.xml with the template information');
    writeln('   -u:         Update file f.in to f.out, replacing (multiple entries okay):');
    writeln('                          $VERSION     5.0.50.0');
    writeln('                          $VERSIONNUM  5,0,50,0');
    writeln('                          $VERSIONCVS  5-0-50-0');
    writeln('   template:   The source template, usually \keyman\'+SKeymanVersion+'\src\<dir>\version.txt');
    writeln('   root:       The source template, usually \keyman\'+SKeymanVersion+'\src\version.txt');
    writeln('   version.rc: version.rc file to update');
    ExitCode := 2;
    Exit;
  end;

  if IncrementVersion
    then IncrementTemplate
    else if UpdateFiles.Count = 0 then UpdateResource;

  for i := 0 to UpdateFiles.Count - 1 do
    UpdateFile(UpdateFiles.Names[i], UpdateFiles.Values[UpdateFiles.Names[i]]);
  UpdateFiles.Free;

  CoUninitialize;
end;

end.

