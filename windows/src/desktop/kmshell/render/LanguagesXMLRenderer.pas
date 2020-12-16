(*
  Name:             LanguagesXMLRenderer
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    25 Mar 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
                    25 Mar 2011 - mcdurdin - I2678 - Uninitialized variant can crash Keyman Configuration when reading package data
*)
unit LanguagesXMLRenderer;

interface

uses
  XMLRenderer,
  Windows;

type
  TLanguagesXMLRenderer = class(TXMLRenderer)
  protected
    function XMLData: WideString; override;
  end;

implementation

uses
  kmint,
  Variants;

{ TLanguagesXMLRenderer }

function TLanguagesXMLRenderer.XMLData: WideString;
var
  References: OleVariant;
begin
  References := Null;   // I2678
  Result := kmcom.Languages.SerializeXML(0, '', References);
end;

end.

