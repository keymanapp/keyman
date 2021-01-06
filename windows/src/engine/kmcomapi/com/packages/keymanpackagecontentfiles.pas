(*
  Name:             keymanpackagecontentfiles
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      12 Mar 2010

  Modified Date:    12 Mar 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
*)
unit keymanpackagecontentfiles;

interface

uses
  Windows, SySUtils, keymanautoobject, keymancontext, ActiveX, ComObj, keymanapi_TLB, StdVcl, kmpinffile, keymanpackagecontentfile;

type
  TKeymanPackageContentFiles = class(TKeymanAutoCollectionObject, IKeymanPackageContentFiles)
  private
  type
    TContentFiles = TAutoObjectList;
  var
    FSourcePath: string;
    FContentFiles: TContentFiles;

    { IKeymanPackageContentFiles }
    function Get_Items(Index: OleVariant): IKeymanPackageContentFile; safecall;
    function IndexOf(const Filename: WideString): Integer; safecall;
  public
    constructor Create(AContext: TKeymanContext; const ASourcePath: string; const AKMPInf: TKMPInfFile);
  end;

implementation

uses
  keymanerrorcodes, Variants;

constructor TKeymanPackageContentFiles.Create(AContext: TKeymanContext; const ASourcePath: string; const AKMPInf: TKMPInfFile);
var
  i: Integer;
begin
  FContentFiles := TContentFiles.Create;
  inherited Create(AContext, IKeymanPackageContentFiles, FContentFiles);
  FSourcePath := ASourcePath;
  for i := 0 to AKMPInf.Files.Count - 1 do
    FContentFiles.Add(TKeymanPackageContentFile.Create(Context, FSourcePath, AKMPInf.Files[i]));
end;

function TKeymanPackageContentFiles.Get_Items(Index: OleVariant): IKeymanPackageContentFile;
var
  i: Integer;
begin
  Result := nil;

  if VarType(Index) = varOleStr
    then i := IndexOf(Index)
    else i := Index;

  if (i < Get_Count) and (i >= 0)
    then Result := FContentFiles[i] as IKeymanPackageContentFile
    else ErrorFmt(KMN_E_Collection_InvalidIndex, VarArrayOf([VarToStr(Index)]));
end;

function TKeymanPackageContentFiles.IndexOf(const Filename: WideString): Integer;
var
  i: Integer;
  FilenameExt: string;
begin
  FilenameExt := ExtractFileName(Filename);
  for i := 0 to Get_Count - 1 do
    if SameFileName(FilenameExt, (FContentFiles[i] as IKeymanPackageContentFile).Filename) then
      Exit(i);
  Result := -1;
end;

end.
