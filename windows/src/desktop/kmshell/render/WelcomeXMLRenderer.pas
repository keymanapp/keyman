(*
  Name:             WelcomeXMLRenderer
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Jun 2007

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Jun 2007 - mcdurdin - Initial version
                    25 Mar 2011 - mcdurdin - I2678 - Uninitialized variant can crash Keyman Configuration when reading package data
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit WelcomeXMLRenderer;  // I3306

interface

uses
  XMLRenderer,
  Windows;

type
  TWelcomeXMLRenderer = class(TXMLRenderer)
  protected
    function XMLData: WideString; override;
  end;

implementation

uses
  Dialogs,
  kmint,
  SysUtils,
  Variants,
  keymanapi_TLB;

{ TWelcomeXMLRenderer }

function TWelcomeXMLRenderer.XMLData: WideString;

    function AddPackage(pkg: IKeymanPackageInstalled): WideString;
    var
      References: OleVariant;
    begin
      References := Null;   // I2678
      Result := pkg.SerializeXML(0, '', References);
    end;

    function AddKeyboard(kbd: IKeymanKeyboardInstalled): WideString;
    var
      References: OleVariant;
    begin
      References := Null;   // I2678
      Result := kbd.SerializeXML(0, '', References);
    end;

var
  I: Integer;
begin
  { Add packaged keyboards }
  for I := 0 to kmcom.Packages.Count - 1 do
  begin
    if kmcom.Packages[i].Keyboards.Count > 0 then
      Result := Result + AddPackage(kmcom.Packages[i]);
  end;

  { Add keyboards }
  for I := 0 to kmcom.Keyboards.Count - 1 do
  begin
    if kmcom.Keyboards[i].OwnerPackage = nil then
      Result := Result + AddKeyboard(kmcom.Keyboards[i]);
  end;
end;

end.
