(*
  Name:             GenericXMLRenderer
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      5 Dec 2006

  Modified Date:    5 Dec 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          05 Dec 2006 - mcdurdin - Initial version
*)
unit GenericXMLRenderer;

interface

uses
  XMLRenderer,
  Windows;

type
  TGenericXMLRenderer = class(TXMLRenderer)
  private
    FXML: WideString;
  protected
    function XMLData(FRefreshKeyman: Boolean): WideString; override;
  public
    constructor Create(const AXML: WideString = '');
  end;

implementation

uses
  kmint;

{ TGenericXMLRenderer }

constructor TGenericXMLRenderer.Create(const AXML: WideString);
begin
  inherited Create;
  FXML := AXML;
end;

function TGenericXMLRenderer.XMLData(FRefreshKeyman: Boolean): WideString;
begin
  Result := FXML;
end;

end.
