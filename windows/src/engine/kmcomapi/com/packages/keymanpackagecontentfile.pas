(*
  Name:             keymanpackagecontentfile
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      12 Mar 2010

  Modified Date:    29 Mar 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
                    29 Mar 2010 - mcdurdin - I2299 - Internal crash causes keyboard install to not refresh
*)
unit keymanpackagecontentfile;

interface

uses
  Windows, SySUtils, keymanautoobject, keymancontext, ActiveX, ComObj, keymanapi_TLB, StdVcl, packageinfo, utilfiletypes;

type
  TKeymanPackageContentFile = class(TKeymanAutoObject, IKeymanPackageContentFile)
  private
    FSourcePath: string;

    FContentFileFilename: WideString;
    FContentFileCopyLocation: TPackageFileCopyLocation;
    FContentFileDescription: WideString;
    FContentFileFileType: TKMFileType;
  protected
    { IKeymanPackageContentFile }
    function Get_Description: WideString; safecall;
    function Get_Filename: WideString; safecall;
    function Get_FullFilename: WideString; safecall;
    function Get_Stream: IUnknown; safecall;
  public
    constructor Create(AContext: TKeymanContext; const ASourcePath: string; const AContentFile: TPackageContentFile);
  end;

implementation

uses
  Classes;

{ TKeymanPackageContentFile }

constructor TKeymanPackageContentFile.Create(AContext: TKeymanContext; const ASourcePath: string; const AContentFile: TPackageContentFile);
begin
  inherited Create(AContext, IKeymanPackageContentFile);
  FSourcePath := ASourcePath;
  FContentFileFilename := ExtractFileName(AContentFile.Filename);
  FContentFileCopyLocation := AContentFile.CopyLocation;
  FContentFileDescription := AContentFile.Description;
  FContentFileFileType := AContentFile.FileType;
end;

function TKeymanPackageContentFile.Get_Stream: IUnknown;
begin
  Result := TStreamAdapter.Create(TFileStream.Create(Get_FullFilename, fmOpenRead), soOwned);
end;

function TKeymanPackageContentFile.Get_Description: WideString;
begin
  Result := FContentFileDescription;
end;

function TKeymanPackageContentFile.Get_Filename: WideString;
begin
  Result := FContentFileFilename;
end;

function TKeymanPackageContentFile.Get_FullFilename: WideString;
begin
  Result := FSourcePath + FContentFileFilename;
end;

end.
