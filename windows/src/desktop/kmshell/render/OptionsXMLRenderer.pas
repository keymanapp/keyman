(*
  Name:             OptionsXMLRenderer
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    1 Sep 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
                    04 Dec 2006 - mcdurdin - Read option descriptions from locale
                    27 Mar 2008 - mcdurdin - I1367 - Read option names from locale.xml directly
                    27 Mar 2008 - mcdurdin - I1368 - Sort option groups and names
                    25 Mar 2011 - mcdurdin - I2678 - Uninitialized variant can crash Keyman Configuration when reading package data
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    01 Sep 2014 - mcdurdin - I4393 - V9.0 - Keyman Desktop Free Edition polish
*)
unit OptionsXMLRenderer;  // I3306

interface

uses
  Classes,
  XMLRenderer,
  Windows;

type
  TOptionsXMLRenderer = class(TXMLRenderer)
  protected
    function XMLData(FRefreshKeyman: Boolean): WideString; override;
  public
    constructor Create(AOwner: TXMLRenderers);
  end;

implementation

uses
  custinterfaces,
  kmint,
  MessageIdentifiers,
  utilxml,
  Variants;

{ TOptionsXMLRenderer }

constructor TOptionsXMLRenderer.Create(AOwner: TXMLRenderers);
begin
  inherited Create(AOwner);
end;

function TOptionsXMLRenderer.XMLData(FRefreshKeyman: Boolean): WideString;
var
  References: OleVariant;
  i: Integer;
  FGroups: TStringList;

    function GroupSort(groupname: WideString): Integer;
    begin
      if groupname = 'kogGeneral' then Result := 0
      else if groupname = 'kogStartup' then Result := 1
      else if groupname = 'kogOSK' then Result := 2
      else Result := 3;
    end;
begin
  References := Null;  // I2678
  if FRefreshKeyman then
    kmcom.Options.Refresh;
  Result := kmcom.Options.SerializeXML(0, '', References);
  FGroups := TStringList.Create;
  try
    Result := Result + '<OptionGroups>';
    for i := 0 to kmcom.Options.Count - 1 do
    begin
      if FGroups.IndexOf(kmcom.Options[i].Group) < 0 then
        FGroups.Add(kmcom.Options[i].Group);
    end;

    for i := 0 to FGroups.Count - 1 do
        Result := Result +
          '<OptionGroup>' +
          XMLFormat([
            'sort', GroupSort(FGroups[i]),
            'name', FGroups[i]]) +
          '</OptionGroup>';
  finally
    FGroups.Free;
  end;
  Result := Result + '</OptionGroups>';
end;

end.

