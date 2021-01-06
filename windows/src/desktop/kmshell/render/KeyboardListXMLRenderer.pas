(*
  Name:             KeyboardListXMLRenderer
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    1 May 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
                    04 Jan 2007 - mcdurdin - Encode entities in XML render
                    25 Mar 2011 - mcdurdin - I2678 - Uninitialized variant can crash Keyman Configuration when reading package data
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
*)
unit KeyboardListXMLRenderer;  // I3306

interface

uses
  Classes,
  XMLRenderer,
  Winapi.Windows;

type
  TKeyboardListXMLRenderer = class(TXMLRenderer)
  private
    FFileReferences: TStrings;
    procedure DeleteFileReferences;
  protected
    function XMLData: WideString; override;
  public
    constructor Create(AOwner: TXMLRenderers);
    destructor Destroy; override;
    property FileReferences: TStrings read FFileReferences;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,

  keymanapi_TLB,
  utilsystem,
  utildir,
  utilxml;

{ TKeyboardListXMLRenderer }

constructor TKeyboardListXMLRenderer.Create(AOwner: TXMLRenderers);
begin
  inherited Create(AOwner);
  FFileReferences := TStringList.Create;
end;

procedure TKeyboardListXMLRenderer.DeleteFileReferences;
var
  i: Integer;
begin
  for i := 0 to FFileReferences.Count - 1 do
    DeleteFile(Owner.TempPath + FFileReferences[i]);   // I4181
  FFileReferences.Clear;
end;

destructor TKeyboardListXMLRenderer.Destroy;
begin
  DeleteFileReferences;
  FreeAndNil(FFileReferences);
  inherited Destroy;
end;

function TKeyboardListXMLRenderer.XMLData: WideString;
    function AddKeyboard(kbd: IKeymanKeyboardInstalled): WideString;
    var
      References: OleVariant;
      I: Integer;
    begin
      References := Null; // I2678
      Result := kbd.SerializeXML(ksfExportImages, Owner.TempPath, References);
      if not VarIsNull(References) then // I2678
        for I := VarArrayLowBound(References, 1) to VarArrayHighBound(References, 1) do
          FFileReferences.Add(References[I]);
    end;

    function AddPackage(pkg: IKeymanPackageInstalled): WideString;
    var
      References: OleVariant;
      I: Integer;
    begin
      References := Null; // I2678
      Result := pkg.SerializeXML(ksfExportImages, Owner.TempPath, References);
      if not VarIsNull(References) then // I2678
        for I := VarArrayLowBound(References, 1) to VarArrayHighBound(References, 1) do
          FFileReferences.Add(References[I]);
    end;

var
  I: Integer;
begin
  DeleteFileReferences;

  { Add unpackaged keyboards }
  for I := 0 to kmcom.Keyboards.Count - 1 do
  begin
    if kmcom.Keyboards[i].OwnerPackage = nil then
      Result := Result + AddKeyboard(kmcom.Keyboards[i]);
  end;

  { Add packaged keyboards }
  for I := 0 to kmcom.Packages.Count - 1 do
  begin
    if kmcom.Packages[i].Keyboards.Count > 0 then
      Result := Result + AddPackage(kmcom.Packages[i]);
  end;
end;

end.
