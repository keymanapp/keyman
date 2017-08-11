(*
  Name:             utilfiletypes
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    4 Dec 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Refactor util functions into multiple units
                    04 Dec 2006 - mcdurdin - Add XML and HTML filetypes
*)
unit utilfiletypes;

interface

type
  TKMFileType = (ftKeymanSource, ftPackageSource, ftKeymanFile, ftPackageFile,
    ftFont, ftReadme, ftTextFile,
    ftKeymanWizard, ftOther, ftBitmap,
    ftVisualKeyboard, ftXMLFile, ftHTMLFile);
const
  {FileTypes: array[TKMFileType] of string = ('ftKeymanSource', 'ftPackageSource',
    'ftKeymanFile', 'ftPackageFile', 'ftFont', 'ftReadme', 'ftTextFile',
    'ftKeymanWizard', 'ftOther', 'ftBitmap',
    'ftVirtualKeyboard', 'ftAddinFile', 'ftInstallerSource', 'ftBrandingSource',
    'ftEncryptedKeymanFile', 'ftEncryptedBranding');}

  FileTypeNames: array[TKMFileType] of string = ('Keyman Source File', 'Package Source File',
    'Keyman Keyboard File', 'Keyman Package File', 'Font', 'Readme File', 'Text File',
    'Keyman Wizard File', 'Other File', 'Bitmap File',
    'Keyman Visual Keyboard File', 'XML File', 'HTML File');


type
  TKMFileTypeInfo = record
    Ext: string;
    FileType: TKMFileType;
  end;

const
  Ext_KeymanFile = '.kmx';
  Ext_PackageFile = '.kmp';

const ExtFileTypes: array[0..9] of TKMFileTypeInfo = (
  (Ext: '.kmn'; FileType: ftKeymanSource),
  (Ext: '.kps'; FileType: ftPackageSource),
  (Ext: '.kmx'; FileType: ftKeymanFile),
  (Ext: '.kmp'; FileType: ftPackageFile),
  (Ext: '.ttf'; FileType: ftFont),
  (Ext: '.otf'; FileType: ftFont),
  (Ext: '.fon'; FileType: ftFont),
  (Ext: '.ttc'; FileType: ftFont),
  (Ext: '.kvk'; FileType: ftVisualKeyboard),
  (Ext: ''; FileType: ftOther));

function GetFileTypeFromFileName(const FileName: string): TKMFileType;
//function UtilFileTypeToTLBFileType(ft: TKMFileType): keymanapi_TLB.KeymanFileType;
function GetFileTypeFilter(ft: TKMFileType; var DefaultExt: string): string;

function IsKeyboardFile(const FileName: string): Boolean;
function RemoveFileExtension(Filename, Extension: string): string;

type
  TKeymanFileTypeInfo = class
  public
    class function IsPackageWelcomeFile(const Filename: string): Boolean; static;
    class function IsPackageUsageFile(const Filename: string): Boolean; static;
    class function IsPackageOptionsFile(const Filename: string): Boolean; static;
  end;

implementation

uses
  StockFileNames,
  System.StrUtils,
  System.SysUtils;

function GetFileTypeFromFileName(const FileName: string): TKMFileType;
var
  ext: string;
  i: Integer;
begin
  ext := LowerCase(ExtractFileExt(FileName));
  for i := 0 to High(ExtFileTypes) do
    if ext = ExtFileTypes[i].Ext then
    begin
      Result := ExtFileTypes[i].FileType;
      Exit;
    end;

  Result := ftOther;
end;

//function UtilFileTypeToTLBFileType(ft: TKMFileType): keymanapi_TLB.KeymanFileType;
//begin
//  Result := KeymanFileType(ft);
//end;

function GetFileTypeFilter(ft: TKMFileType; var DefaultExt: string): string;
var
  s1, s2: string;
  i: Integer;
begin
  DefaultExt := '';
  s1 := '';
  s2 := '';

  for i := 0 to High(ExtFileTypes) do
  begin
    if ExtFileTypes[i].FileType = ft then
    begin
      if DefaultExt = '' then DefaultExt := ExtFileTypes[i].Ext;
      if s1 <> '' then s1 := s1 + ',';
      if s2 <> '' then s2 := s2 + ';';
      s1 := s1 + '*'+ExtFileTypes[i].Ext;
      s2 := s2 + '*'+ExtFileTypes[i].Ext;
    end;
  end;

  if (s1 <> '*') and (s1 <> '')
    then Result := FileTypeNames[ft] + ' ('+s1+')|'+s2
    else Result := '';
end;

function IsKeyboardFile(const FileName: string): Boolean;
begin
    if Length(FileName) < 5 then Result := False
    else Result := (LowerCase(Copy(FileName, Length(FileName)-3, 4)) = '.kmx') or
        (LowerCase(Copy(FileName, Length(FileName)-3, 4)) = '.kxx');
end;

function RemoveFileExtension(Filename, Extension: string): string;
var
  LE, LF: Integer;
begin
  LF := Length(Filename);
  LE := Length(Extension);
  if SameText(Copy(Filename, LF-LE+1, LE), Extension)
    then Result := Copy(Filename, 1, LF-LE)
    else Result := Filename;
end;

{ TKeymanFileTypeInfo }

class function TKeymanFileTypeInfo.IsPackageOptionsFile(const Filename: string): Boolean;
begin
  Result := SameFileName(PackageFile_Options, FileName) or
    (StartsText(PackageFile_Options_Prefix, Filename) and
    SameText(ExtractFileExt(Filename), ExtractFileExt(PackageFile_Options)));
end;

class function TKeymanFileTypeInfo.IsPackageUsageFile(
  const Filename: string): Boolean;
begin
  Result := SameFileName(PackageFile_Usage, FileName) or
    (StartsText(PackageFile_Usage_Prefix, Filename) and
    SameText(ExtractFileExt(Filename), ExtractFileExt(PackageFile_Usage)));
end;

class function TKeymanFileTypeInfo.IsPackageWelcomeFile(
  const Filename: string): Boolean;
begin
  Result := SameFileName(PackageFile_Welcome, FileName) or
    (StartsText(PackageFile_Welcome_Prefix, Filename) and
    SameText(ExtractFileExt(Filename), ExtractFileExt(PackageFile_Welcome)));
end;

end.
