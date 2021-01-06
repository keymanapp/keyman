(*
  Name:             keymanpackagecontentfonts
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      18 May 2012

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit keymanpackagecontentfonts;  // I3306

interface

uses
  Windows, ActiveX, ComObj, keymanapi_TLB, keymancontext, keymanautoobject, keymanpackagecontentfont, StdVcl, IniFiles;

type
  TPackageFontList = TAutoObjectList;

  TKeymanPackageContentFonts = class(TKeymanAutoCollectionObject, IKeymanPackageContentFonts)
  private
    FFonts: TPackageFontList;
    FPackageFilename: Widestring;
    FIsPackageInstalled: Boolean;
  protected
    procedure DoRefresh; override;

    { IKeymanPackageContentFonts }
    function Get_Items(Index: OleVariant): IKeymanPackageContentFont; safecall;
    function IndexOf(const Filename: WideString): Integer; safecall;
  public
    constructor Create(AContext: TKeymanContext; APackage: IKeymanPackage);
    destructor Destroy; override;
  end;

implementation

uses
  KeymanErrorCodes,
  KMPInfFile,
  SysUtils,
  TTInfo,
  utilfiletypes,
  Variants;

{ TKeymanPackageContentFonts }

constructor TKeymanPackageContentFonts.Create(AContext: TKeymanContext; APackage: IKeymanPackage);
begin
  FFonts := TPackageFontList.Create;
  FPackageFilename := APackage.Filename;

  FIsPackageInstalled := Supports(APackage, IKeymanPackageInstalled);

  inherited Create(AContext, IKeymanPackageContentFonts, FFonts);
  Refresh;
end;

destructor TKeymanPackageContentFonts.Destroy;
begin
  FreeAndNil(FFonts);
  inherited Destroy;
end;

function TKeymanPackageContentFonts.Get_Items(Index: OleVariant): IKeymanPackageContentFont;
var
  i: Integer;
begin
  if VarType(Index) = varOleStr
    then i := IndexOf(Index)
    else i := Index;

  if (i < Get_Count) and (i >= 0)
    then Result := FFonts[i] as IKeymanPackageContentFont
    else ErrorFmt(KMN_E_Collection_InvalidIndex, VarArrayOf([VarToStr(Index)]));
end;

function TKeymanPackageContentFonts.IndexOf(const Filename: WideString): Integer;
var
  i: Integer;
begin
  for i := 0 to FFonts.Count - 1 do
    if AnsiCompareText((FFonts[i] as IKeymanPackageContentFont).Filename, Filename) = 0 then
      Exit(i);
  Result := -1;
end;

procedure TKeymanPackageContentFonts.DoRefresh;
var
  ffontsinf: TIniFile;
  I: Integer;
  FFont: TKeymanPackageContentFont;
  fkmpinf: TKMPInfFile;
  FFontName, FSourcePath: WideString;
    function IsFontInstalled(s: string): Boolean;
    begin
      Result := not FIsPackageInstalled or (LowerCase(ffontsinf.ReadString('UninstallFonts', s, 'true')) = 'true');
    end;
begin
  fkmpinf := TKMPInfFile.Create;
  fkmpinf.FileName := FPackageFilename;
  fkmpinf.LoadIni;

  FSourcePath := ExtractFilePath(FPackageFilename);
  ffontsinf := TIniFile.Create(FSourcePath+'fonts.inf');
  try

    for i := 0 to fkmpinf.Files.Count - 1 do
      if fkmpinf.Files[i].FileType = ftFont then
      begin
        if not IsFontInstalled(fkmpinf.Files[i].FileName) then Continue;

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

        FFont := TKeymanPackageContentFont.Create(Context, FSourcePath + fkmpinf.Files[i].FileName, FFontName);
        FFonts.Add(FFont);
      end;
  finally
    ffontsinf.Free;
    fkmpinf.Free;
  end;
end;

end.
