(*
  Name:             keymanpackageinstalled
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    3 May 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Add Serialize
                    04 Dec 2006 - mcdurdin - Add font information in serialization
                    19 Mar 2007 - mcdurdin - I701 - fix leaked COM objects
                    27 Mar 2008 - mcdurdin - Add RefreshKeyboards
                    16 Jan 2009 - mcdurdin - I1696 - Add support for Author, AuthorEmail, Copyright, Version, Website in IKeymanPackage2
                    30 Jan 2009 - mcdurdin - I1826 - Improve load performance of packages
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
                    12 Mar 2010 - mcdurdin - I2216 - Show fonts installed for a package
                    31 Jan 2011 - mcdurdin - I1427 - Show OSK, Usage and documentation information in Config
                    18 Mar 2011 - mcdurdin - I2827 - Disable options link if no keyboard options are available
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
*)
unit keymanpackageinstalled;

interface

uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.Classes,
  System.Win.ComObj,
  System.Win.StdVcl,
  keymanautoobject,
  KeymanContext,
  keymanpackagecontentkeyboards,
  keymanapi_TLB,
  internalinterfaces,
  kmpinffile,
  keymanpackagecontentfonts;

type
  TKeymanPackageInstalled = class(TKeymanAutoObject, IKeymanPackage, IKeymanPackageInstalled, IIntKeymanPackageInstalled)
  private
    FKMPInf: TKMPInfFile;
    FName: WideString;
    FFileName: WideString;
    FDescription: WideString;
    FKeyboards: IKeymanPackageContentKeyboards;
    FFonts: IKeymanPackageContentFonts;
    FFiles: IKeymanPackageContentFiles;
    procedure LoadKMPInfFile;
    function GetContentFile(const Filename: string): IKeymanPackageContentFile;
  protected
    function Serialize(Flags: TOleEnum; const ImagePath: WideString;
      References: TStrings): WideString; override;

    { IKeymanPackage }
    function Get_Author: WideString; safecall;
    function Get_AuthorEmail: WideString; safecall;
    function Get_Copyright: WideString; safecall;
    function Get_Filename: WideString; safecall;
    function Get_Files: IKeymanPackageContentFiles; safecall;
    function Get_Fonts: IKeymanPackageContentFonts; safecall;
    function Get_Graphic: IPicture; safecall;
    function Get_GraphicFile: IKeymanPackageContentFile; safecall;
    function Get_ID: WideString; safecall;
    function Get_Keyboards: IKeymanPackageContentKeyboards; safecall;
    function Get_KeyboardOptionsFile: IKeymanPackageContentFile; safecall;
    function Get_Name: WideString; safecall;
    function Get_ReadmeFile: IKeymanPackageContentFile; safecall;
    function Get_UsageFile: IKeymanPackageContentFile; safecall;
    function Get_Version: WideString; safecall;
    function Get_Website: WideString; safecall;
    function Get_WelcomeFile: IKeymanPackageContentFile; safecall;

    { IKeymanPackageInstalled }
    procedure Uninstall(RemoveFonts: WordBool); safecall;

    { IIntKeymanPackageInstalled }
    procedure RefreshKeyboards;
    function GetKeyboardLanguageList(const KeyboardID: string): TObject;
  public
    constructor Create(AContext: TKeymanContext; const AName: WideString);
    destructor Destroy; override;
  end;

implementation

uses
  Vcl.Graphics,
  ComServ,
  inifiles,
  keymankeyboardinstalled,
  keymanpackagecontentfiles,
  kpuninstallpackage,
  PackageInfo,
  ErrorControlledRegistry, 
  RegistryKeys,
  StockFileNames,
  SysUtils,
  ttinfo,
  utilfiletypes,
  utilxml;

constructor TKeymanPackageInstalled.Create(AContext: TKeymanContext; const AName: WideString);
begin
  inherited Create(AContext, IKeymanPackageInstalled);
  FName := AName;
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;

    if OpenKeyReadOnly(SRegKey_InstalledPackages_LM + '\' + FName) then
    begin
      FDescription := ReadString(SRegValue_PackageDescription);
      FFileName := ReadString(SRegValue_PackageFile);
    end
    else
    begin
      FDescription := FName;
      FFileName := '';
    end;
  finally
    Free;
  end;
  FKeyboards := TKeymanPackageContentKeyboardsInstalled.Create(AContext, FName);
end;

destructor TKeymanPackageInstalled.Destroy;
begin
  FreeAndNil(FKMPInf);
  FKeyboards := nil;
  FFonts := nil;
  inherited Destroy;
end;

function TKeymanPackageInstalled.Get_Author: WideString;
begin
  LoadKMPInfFile;
  Result := FKMPInf.Info.Desc[PackageInfo_Author];
end;

function TKeymanPackageInstalled.Get_AuthorEmail: WideString;
begin
  LoadKMPInfFile;
  Result := FKMPInf.Info.URL[PackageInfo_Author];
end;

function TKeymanPackageInstalled.Get_Copyright: WideString;
begin
  LoadKMPInfFile;
  Result := FKMPInf.Info.Desc[PackageInfo_Copyright];
end;

function TKeymanPackageInstalled.Get_Name: WideString;
begin
  Result := FDescription;
end;

function TKeymanPackageInstalled.Get_KeyboardOptionsFile: IKeymanPackageContentFile;
begin
  Result := GetContentFile(PackageFile_Options_Prefix + Context.Control.CurrentUILanguage + ExtractFileExt(PackageFile_Options));
  if not Assigned(Result) then
    Result := GetContentFile(PackageFile_Options);
end;

function TKeymanPackageInstalled.Get_Keyboards: IKeymanPackageContentKeyboards;
begin
  Result := FKeyboards;
end;

function TKeymanPackageInstalled.Get_ID: WideString;
begin
  Result := FName;
end;

function TKeymanPackageInstalled.GetContentFile(const Filename: string): IKeymanPackageContentFile;
var
  n: Integer;
begin
  LoadKMPInfFile;
  n := Get_Files.IndexOf(FileName);
  if n < 0 then
    Exit(nil);
  Result := Get_Files[n];
end;

function TKeymanPackageInstalled.GetKeyboardLanguageList(
  const KeyboardID: string): TObject;
var
  k: TPackageKeyboard;
begin
  LoadKMPInfFile;
  for k in FKMPInf.Keyboards do
    if SameText(k.ID, KeyboardID) then
      Exit(k.Languages);

  Result := nil;
end;

function TKeymanPackageInstalled.Get_ReadmeFile: IKeymanPackageContentFile;
begin
  LoadKMPInfFile;
  if not Assigned(FKMPInf.Options.ReadmeFile) then
    Exit(nil);
  Result := GetContentFile(FKMPInf.Options.ReadmeFile.FileName);
end;

function TKeymanPackageInstalled.Get_UsageFile: IKeymanPackageContentFile;
begin
  Result := GetContentFile(PackageFile_Usage_Prefix + Context.Control.CurrentUILanguage + ExtractFileExt(PackageFile_Usage));
  if not Assigned(Result) then
    Result := GetContentFile(PackageFile_Usage);
end;

function TKeymanPackageInstalled.Get_Version: WideString;
begin
  LoadKMPInfFile;
  Result := FKMPInf.Info.Desc[PackageInfo_Version];
end;

function TKeymanPackageInstalled.Get_Website: WideString;
begin
  LoadKMPInfFile;
  Result := FKMPInf.Info.URL[PackageInfo_Website];
end;

function TKeymanPackageInstalled.Get_WelcomeFile: IKeymanPackageContentFile;
begin
  Result := GetContentFile(PackageFile_Welcome_Prefix + Context.Control.CurrentUILanguage + ExtractFileExt(PackageFile_Welcome));
  if not Assigned(Result) then
    Result := GetContentFile(PackageFile_Welcome);
end;

procedure TKeymanPackageInstalled.RefreshKeyboards;
begin
  if Assigned(FKeyboards) then
    (FKeyboards as IIntKeymanKeyboardsPackageInstalled).RefreshSource;
end;

function TKeymanPackageInstalled.Serialize(Flags: TOleEnum;
  const ImagePath: WideString; References: TStrings): WideString;
var
  FHasFonts: Boolean;
  i: Integer;
  FSourcePath: string;
  FFontName: string;
  ffontsinf: TIniFile;

    function AddInfo(const tagname, infname: string): string;
    begin
      if fkmpinf.Info.Desc[infname] <> '' then
      begin
        Result := Result + '<detail name="'+tagname+'"';
        if fkmpinf.Info.URL[infname] <> '' then
          Result := Result+' url="'+XMLEncode(fkmpinf.Info.URL[infname])+'"';
        Result := Result + '>'+XMLEncode(fkmpinf.Info.Desc[infname])+'</detail>';
      end
      else
        Result := '';
    end;

begin
  Result := XMLFormat([
    'name', Get_Name,
    'id', Get_ID,
    'filename', Get_Filename]);

  Result := Result + (FKeyboards as IIntKeymanInterface).DoSerialize(Flags, ImagePath, References);

  { Read the kmp.inf and retrieve other details }

  LoadKMPInfFile;

  FSourcePath := ExtractFilePath(fkmpinf.FileName);
  FHasFonts := False;

  ffontsinf := TIniFile.Create(FSourcePath+PackageFile_FontsInf);
  try
    for i := 0 to fkmpinf.Files.Count - 1 do
    begin  // I1427
      if TKeymanFileTypeInfo.IsPackageWelcomeFile(fkmpinf.Files[i].FileName) then
        Result := Result + '<welcome />'
      else if TKeymanFileTypeInfo.IsPackageUsageFile(fkmpinf.Files[i].FileName) then
        Result := Result + '<usage />'
      else if TKeymanFileTypeInfo.IsPackageOptionsFile(fkmpinf.Files[i].FileName) then
        Result := Result + '<options />';
    end;
    for i := 0 to fkmpinf.Files.Count - 1 do
      if fkmpinf.Files[i].FileType = ftFont then
      begin
        if LowerCase(ffontsinf.ReadString('UninstallFonts', fkmpinf.Files[i].FileName, 'true')) = 'true' then
        begin
          if not FHasFonts then
            Result := Result + '<Fonts>';
          FHasFonts := True;
          try
            with TTTInfo.Create(FSourcePath + fkmpinf.Files[i].FileName, [tfNames]) do
            try
              FFontName := FullName;
            finally
              Free;
            end;
          except
            FFontName := ExtractFileName(fkmpinf.Files[i].FileName);
          end;
          Result := Result + '<Font><filename>'+XMLEncode(ExtractFileName(fkmpinf.Files[i].FileName))+'</filename><name>'+
            XMLEncode(FFontName)+'</name></Font>';
        end;
      end;
    if FHasFonts then Result := Result + '</Fonts>';
  finally
    ffontsinf.Free;
  end;

  Result := Result + AddInfo('version', PackageInfo_Version);
  Result := Result + AddInfo('copyright', PackageInfo_Copyright);
  Result := Result + AddInfo('author', PackageInfo_Author);
  Result := Result + AddInfo('website', PackageInfo_Website);
end;

procedure TKeymanPackageInstalled.Uninstall(RemoveFonts: WordBool);
begin
  with TKPUninstallPackage.Create(Context) do
  try
    Execute(FName, RemoveFonts);
  finally
    Free;
  end;
end;

function TKeymanPackageInstalled.Get_Filename: WideString;
begin
  Result := FFileName;
end;

function TKeymanPackageInstalled.Get_Files: IKeymanPackageContentFiles;
begin
  if not Assigned(FFiles) then
  begin
    LoadKMPInfFile;
    FFiles := TKeymanPackageContentFiles.Create(Context, ExtractFilePath(Get_Filename), FKMPInf);
  end;
  Result := FFiles;
end;

function TKeymanPackageInstalled.Get_Fonts: IKeymanPackageContentFonts;
begin
  if not Assigned(FFonts) then
    FFonts := TKeymanPackageContentFonts.Create(Context, Self);
  Result := FFonts;
end;

function TKeymanPackageInstalled.Get_Graphic: IPicture;
var
  b: TBitmap;
  pd: TPictDesc;
begin
  LoadKMPInfFile;
  if Assigned(FKMPInf.Options.GraphicFile) then
  begin
    b := TBitmap.Create;
    b.LoadFromFile(ExtractFilePath(FFileName) + FKMPInf.Options.GraphicFile.FileName);

    pd.cbSizeofstruct := SizeOf(TPictDesc);
    pd.picType := PICTYPE_BITMAP;
    pd.hbitmap := b.Handle;
    pd.hpal := 0;
    OleCreatePictureIndirect(pd, IPicture, True, Result);
  end
  else
    Result := nil;
end;

function TKeymanPackageInstalled.Get_GraphicFile: IKeymanPackageContentFile;
begin
  LoadKMPInfFile;
  if not Assigned(FKMPInf.Options.GraphicFile) then
    Exit(nil);
  Result := GetContentFile(FKMPInf.Options.GraphicFile.FileName);
end;

procedure TKeymanPackageInstalled.LoadKMPInfFile;
begin
  if Assigned(fkmpinf) then Exit;
  fkmpinf := TKMPInfFile.Create;
  fkmpinf.LoadLegacy := False;
  fkmpinf.FileName := Get_Filename;
  fkmpinf.LoadIni;
end;

end.
