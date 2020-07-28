(*
  Name:             keymanpackagefile
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    29 Mar 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Avoid processmessages in unzip
                    04 Dec 2006 - mcdurdin - Add Serialize function, support ShortcutRootPath in installation
                    04 Jan 2007 - mcdurdin - Test if package file exists when installing
                    25 Jan 2007 - mcdurdin - Fix _AddRef and _Release to detach from Keyman object
                    19 Mar 2007 - mcdurdin - Removed forms.pas dependency
                    08 Jun 2007 - mcdurdin - I880 - Fix crash when handling a corrupt package
                    05 Nov 2007 - mcdurdin - I1109 - Add selected keyboard for desktop light
                    14 Jun 2008 - mcdurdin - I1464 - Avoid deletion of wrong files if sourcepath setup fails
                    16 Jan 2009 - mcdurdin - I1696 - Add support for Author, AuthorEmail, Copyright, Version, Website in IKeymanPackage2
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
                    23 Mar 2010 - mcdurdin - I2239 - Reference counting - Functionality loss - New keyboard not in keyboard layouts list immediately on install
                    23 Mar 2010 - mcdurdin - I2238 - Reference counting - Welcome not showing
                    29 Mar 2010 - mcdurdin - I2299 - Internal crash causes keyboard install to not refresh
*)
unit keymanpackagefile;

interface

uses
  Windows, SySUtils, keymanautoobject, ActiveX, ComObj, keymanapi_TLB, keymanpackagecontentkeyboards,
  keymanpackagecontentfiles, StdVcl, kmpinffile, KeymanContext, Graphics, Classes, internalinterfaces;

type
  TKeymanPackageFile = class(TKeymanAutoObject, IKeymanPackage, IKeymanPackageFile, IKeymanPackageFile2)
  private
    FSourcePath: string;
    FSubFiles: IKeymanPackageContentFiles;
    FKeyboards: IKeymanPackageContentKeyboards;
    FFonts: IKeymanPackageContentFonts;
    FKMPInf: TKMPInfFile;
    FFileName: WideString;
    procedure LoadPackage;
    procedure FreePackage;
    function GetContentFile(const Filename: string): IKeymanPackageContentFile;
  protected
    function Serialize(Flags: TOleEnum; const ImagePath: WideString;
      References: TStrings): WideString; override;

    { IKeymanPackage }
    function Get_Author: WideString; safecall;
    function Get_AuthorEmail: WideString; safecall;
    function Get_Copyright: WideString; safecall;
    function Get_Filename: Widestring; safecall;
    function Get_Files: IKeymanPackageContentFiles; safecall;
    function Get_Fonts: IKeymanPackageContentFonts; safecall;
    function Get_Graphic: IPicture; safecall;
    function Get_GraphicFile: IKeymanPackageContentFile; safecall;
    function Get_ID: WideString; safecall;
    function Get_KeyboardOptionsFile: IKeymanPackageContentFile; safecall;
    function Get_Keyboards: IKeymanPackageContentKeyboards; safecall;
    function Get_Name: WideString; safecall;
    function Get_ReadMeFile: IKeymanPackageContentFile; safecall;
    function Get_UsageFile: IKeymanPackageContentFile; safecall;
    function Get_Version: WideString; safecall;
    function Get_Website: WideString; safecall;
    function Get_WelcomeFile: IKeymanPackageContentFile; safecall;

    { IKeymanPackageFile }
    procedure Install(Force: WordBool); safecall;
    function Install2(Force, InstallDefaultLanguage: WordBool): IKeymanPackageInstalled; safecall;
  public
    constructor Create(AContext: TKeymanContext; const Filename: Widestring);
    destructor Destroy; override;
  end;

implementation

uses
  System.Zip,
  custinterfaces,
  keymanpackagecontentfonts,
  packageinfo,
  kpinstallpackage,
  StockFileNames,
  utilkeyman,
  utilxml,
  utilfiletypes,
  utildir,
  ttinfo;

{ TKeymanPackageFile }

constructor TKeymanPackageFile.Create(AContext: TKeymanContext;
  const Filename: Widestring);
begin
  inherited Create(AContext, IKeymanPackageFile);
  FFileName := FileName;
  LoadPackage;
  FKeyboards := TKeymanPackageContentKeyboardsFile.Create(Context, FSourcePath, FKMPInf);
  FSubFiles := TKeymanPackageContentFiles.Create(Context, FSourcePath, FKMPInf);
end;

destructor TKeymanPackageFile.Destroy;
begin
  FreePackage;
  FKeyboards := nil;
  FSubFiles := nil;
  inherited Destroy;
end;

function TKeymanPackageFile.GetContentFile(const Filename: string): IKeymanPackageContentFile;
var
  n: Integer;
begin
  n := Get_Files.IndexOf(FileName);
  if n < 0 then
    Exit(nil);
  Result := Get_Files[n];
end;

function TKeymanPackageFile.Get_Author: WideString;
begin
  Result := FKMPInf.Info.Desc[PackageInfo_Author];
end;

function TKeymanPackageFile.Get_AuthorEmail: WideString;
begin
  Result := FKMPInf.Info.URL[PackageInfo_Author];
end;

function TKeymanPackageFile.Get_Copyright: WideString;
begin
  Result := FKMPInf.Info.Desc[PackageInfo_Copyright];
end;

function TKeymanPackageFile.Get_Name: WideString;
begin
  Result := FKMPInf.Info.Desc[PackageInfo_Name];
end;

function TKeymanPackageFile.Get_Filename: Widestring;
begin
  Result := FFileName;
end;

function TKeymanPackageFile.Get_KeyboardOptionsFile: IKeymanPackageContentFile;
begin
  Result := GetContentFile(PackageFile_Options_Prefix + Context.Control.CurrentUILanguage + ExtractFileExt(PackageFile_Options));
  if not Assigned(Result) then
    Result := GetContentFile(PackageFile_Options);
end;

function TKeymanPackageFile.Get_Keyboards: IKeymanPackageContentKeyboards;
begin
  Result := FKeyboards;
end;

function TKeymanPackageFile.Get_ID: WideString;
begin
  Result := GetShortPackageName(FFileName);
end;

procedure TKeymanPackageFile.Install(Force: WordBool);
begin
  with TKPInstallPackage.Create(Context) do
  try
    Execute(FFileName, Force, True);
  finally
    Free;
  end;
end;

function TKeymanPackageFile.Install2(Force, InstallDefaultLanguage: WordBool): IKeymanPackageInstalled;
var
  kpi: IKeymanPackagesInstalled;
begin
  with TKPInstallPackage.Create(Context) do
  try
    Execute(FFileName, Force, InstallDefaultLanguage);
  finally
    Free;
  end;

  kpi := Context.Packages as IKeymanPackagesInstalled;
  kpi.Refresh;
  Result := kpi.Items[FFileName];
end;

procedure TKeymanPackageFile.LoadPackage;
var
  FZip: TZipFile;
  FTempOutPath: string;
  i: Integer;
  buf: array[0..260] of Char;
  JsonFile, InfFile: string;
begin
  if not FileExists(FFileName) then
    raise Exception.Create('File '+FFileName+' does not exist.');
     
  if GetTempPath(260, buf) = 0 then
    raise Exception.Create('Unable to get temporary path: ' + IntToHex(GetLastError, 8) + ' ' + SysErrorMessage(GetLastError));
  FTempOutPath := buf;
  if GetTempFileName(PChar(FTempOutPath), 'kmp', 0, buf) = 0 then
    raise Exception.Create('Unable to create temp file name: ' + IntToHex(GetLastError, 8) + ' ' + SysErrorMessage(GetLastError));
  if not DeleteFile(buf) then
    raise Exception.Create('Unable to prepare temp file name: ' + IntToHex(GetLastError, 8) + ' ' + SysErrorMessage(GetLastError));
  if not CreateDir(buf) then
    raise Exception.Create('Unable to get create temporary directory: ' + IntToHex(GetLastError, 8) + ' ' + SysErrorMessage(GetLastError));

  FTempOutPath := buf; FTempOutPath := FTempOutPath + '\';

  FSourcePath := FTempOutPath;

  FKMPInf := nil;
  FZip := TZipFile.Create;
  try
    FZip.Open(FFileName, TZipMode.zmRead);

    InfFile := '';

    for i := 0 to FZip.FileCount - 1 do
    begin
      FZip.Extract(i, buf, False);
      if LowerCase(FZip.Filename[i]) = PackageFile_KMPInf then
      begin
        InfFile := FZip.Filename[i];
      end
      else if LowerCase(FZip.Filename[i]) = PackageFile_KMPJSON then
      begin
        JsonFile := FZip.Filename[i];
      end;
    end;

    if (InfFile = '') and (JsonFile = '') then
      raise Exception.Create('Neither kmp.inf nor kmp.json was found in this archive.');
  finally
    FZip.Free;
  end;

  FKMPInf := TKMPInfFile.Create;

  if JsonFile <> '' then
  begin
    FKMPInf.FileName := FTempOutPath + JsonFile;
    FKMPInf.LoadJson;
  end
  else
  begin
    FKMPInf.FileName := FTempOutPath + InfFile;
    FKMPInf.LoadIni;
  end;

  FKMPInf.CheckFiles(FTempOutPath);
end;

function TKeymanPackageFile.Serialize(Flags: TOleEnum;
  const ImagePath: WideString; References: TStrings): WideString;
var
  i: Integer;
  FFonts: Boolean;
  FFontName: string;
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

  FFonts := False;
  for i := 0 to fkmpinf.Files.Count - 1 do
    if fkmpinf.Files[i].FileType = ftFont then
    begin
      if not FFonts then
        Result := Result + '<Fonts>';
      FFonts := True;
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
  if FFonts then Result := Result + '</Fonts>';

  Result := Result + AddInfo('version', PackageInfo_Version);
  Result := Result + AddInfo('copyright', PackageInfo_Copyright);
  Result := Result + AddInfo('author', PackageInfo_Author);
  Result := Result + AddInfo('website', PackageInfo_Website);

  if (Flags and ksfExportImages) = ksfExportImages then
  begin
    if FKMPInf.Options.GraphicFile <> nil then
      Result := Result + '<graphic>'+XMLEncode(FSourcePath + FKMPInf.Options.GraphicFile.FileName)+'</graphic>';
  end;

  if FKMPInf.Options.ReadmeFile <> nil then
    Result := Result + '<readme>'+XMLEncode(FSourcePath + FKMPInf.Options.ReadmeFile.FileName)+'</readme>';

  Result := Result + (FKeyboards as IIntKeymanInterface).DoSerialize(Flags, ImagePath, References);
end;

procedure TKeymanPackageFile.FreePackage;
begin
  FKMPInf.Free;
  if (FSourcePath <> '') then   // I1464 - fix deletion of wrong files if sourcepath setup fails
    RecursiveDelete(FSourcePath);
end;

function TKeymanPackageFile.Get_Graphic: IPicture;
var
  b: TBitmap;
  pd: TPictDesc;
begin
  if Assigned(FKMPInf.Options.GraphicFile) then
  begin
    b := TBitmap.Create;
    b.LoadFromFile(FSourcePath + FKMPInf.Options.GraphicFile.FileName);

    pd.cbSizeofstruct := SizeOf(TPictDesc);
    pd.picType := PICTYPE_BITMAP;
    pd.hbitmap := b.Handle;
    pd.hpal := 0;
    OleCreatePictureIndirect(pd, IPicture, True, Result);
  end
  else
    Result := nil;
end;

function TKeymanPackageFile.Get_GraphicFile: IKeymanPackageContentFile;
begin
  if Assigned(FKMPInf.Options.GraphicFile) then
    Result := GetContentFile(FKMPInf.Options.GraphicFile.FileName);
end;

function TKeymanPackageFile.Get_ReadMeFile: IKeymanPackageContentFile;
begin
  if not Assigned(FKMPInf.Options.ReadmeFile) then
    Exit(nil);
  Result := GetContentFile(FKMPInf.Options.ReadmeFile.FileName);
end;

function TKeymanPackageFile.Get_UsageFile: IKeymanPackageContentFile;
begin
  Result := GetContentFile(PackageFile_Usage_Prefix + Context.Control.CurrentUILanguage + ExtractFileExt(PackageFile_Usage));
  if not Assigned(Result) then
    Result := GetContentFile(PackageFile_Usage);
end;

function TKeymanPackageFile.Get_Files: IKeymanPackageContentFiles;
begin
  Result := FSubFiles;
end;

function TKeymanPackageFile.Get_Fonts: IKeymanPackageContentFonts;
begin
  if not Assigned(FFonts) then
    FFonts := TKeymanPackageContentFonts.Create(Context, Self);

  Result := FFonts;
end;

function TKeymanPackageFile.Get_Version: WideString;
begin
  Result := FKMPInf.Info.Desc[PackageInfo_Version];
end;

function TKeymanPackageFile.Get_Website: WideString;
begin
  Result := FKMPInf.Info.URL[PackageInfo_Website];
end;

function TKeymanPackageFile.Get_WelcomeFile: IKeymanPackageContentFile;
begin
  Result := GetContentFile(PackageFile_Welcome_Prefix + Context.Control.CurrentUILanguage + ExtractFileExt(PackageFile_Welcome));
  if not Assigned(Result) then
    Result := GetContentFile(PackageFile_Welcome);
end;

end.
