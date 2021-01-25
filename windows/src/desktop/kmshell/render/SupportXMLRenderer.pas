(*
  Name:             SupportXMLRenderer
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    28 Aug 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
                    04 Dec 2006 - mcdurdin - Add product name to support information
                    04 Jan 2007 - mcdurdin - Encode entities in XML render
                    15 Jan 2007 - mcdurdin - Add ProductID to support xml
                    28 Aug 2014 - mcdurdin - I4390 - V9.0 - Free vs Pro
*)
unit SupportXMLRenderer;

interface

uses
  XMLRenderer,
  Windows;

type
  TSupportXMLRenderer = class(TXMLRenderer)
  protected
    function XMLData: WideString; override;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  VersionInfo,
  kmint,
  keymanapi_TLB,
  MessageIdentifierConsts,
  MessageIdentifiers,
  utilxml;

{ TSupportXMLRenderer }

function TSupportXMLRenderer.XMLData: WideString;
begin
  //kmcom.SystemInfo.EngineVersion
  Result := '<support>'+
    '<productname>'+XMLEncode(MsgFromId(SKApplicationTitle))+'</productname>'+
    '<version>'+XMLEncode(GetVersionString)+'</version>'+
    '<engineversion>'+XMLEncode(kmcom.SystemInfo.EngineVersion)+'</engineversion>'+
    '<engineinstallpath>'+XMLEncode(kmcom.SystemInfo.EngineInstallPath) +'</engineinstallpath>'+
    '</support>';
end;

end.

