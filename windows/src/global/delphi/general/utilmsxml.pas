(*
  Name:             utilmsxml
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      9 Mar 2009

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          09 Mar 2009 - mcdurdin - I1892 - Activation Server
                    18 May 2012 - mcdurdin - I3309 - V9.0 - Migrate to Delphi XE2, VS2010, svn 1.7
                    08 Jun 2012 - mcdurdin - I3309 - V9.0 - Migrate to Delphi XE2, VS2010, svn 1.7
*)
unit utilmsxml;  // I3309

interface

uses
  System.SysUtils,
  Winapi.Windows,
  Winapi.msxml;

function IsXMLDocument(const s: WideString): Boolean;

function GetXMLAttrStr(node: IXMLDOMNode; const name: WideString; const default: string = ''): WideString;
function GetXMLAttrDateTime(node: IXMLDOMNode; const name: WideString): TDateTime;
function GetXMLAttrInt(node: IXMLDOMNode; const name: WideString; const default: Integer = 0): Integer;
function GetXMLAttrBool(node: IXMLDOMNode; const name: WideString; const default: Boolean = False): Boolean;

function GetXMLNode(node: IXMLDOMNode; const name: WideString): WideString;

procedure SetXMLAttrStr(node: IXMLDOMNode; const name, value: WideString);
procedure SetXMLAttrDateTime(node: IXMLDOMNode; const name: WideString; value: TDateTime);
procedure SetXMLAttrInt(node: IXMLDOMNode; const name: WideString; value: Integer);
procedure SetXMLAttrBool(node: IXMLDOMNode; const name: WideString; value: Boolean);

procedure SetXMLNode(node: IXMLDOMNode; const name, value: WideString); overload;

implementation

var
  FormatSettings: TFormatSettings;

function GetXMLAttrStr(node: IXMLDOMNode; const name: WideString; const default: string = ''): WideString;
var
  attr: IXMLDOMNode;
begin
  attr := node.attributes.getNamedItem(name);
  if Assigned(attr)
    then Result := attr.nodeValue
    else Result := default;
end;

function GetXMLAttrDateTime(node: IXMLDOMNode; const name: WideString): TDateTime;
var
  s: WideString;
begin
  s := GetXMLAttrStr(node, name, '');
  Result := StrToDateTimeDef(s, 0, FormatSettings);
end;

function GetXMLAttrInt(node: IXMLDOMNode; const name: WideString; const default: Integer = 0): Integer;
var
  s: WideString;
begin
  s := GetXMLAttrStr(node, name, '*');
  Result := StrToIntDef(s, default);
end;

function GetXMLAttrBool(node: IXMLDOMNode; const name: WideString; const default: Boolean = False): Boolean; 
var
  s: WideString;
begin
  s := GetXMLAttrStr(node, name, '*');
  Result := StrToBoolDef(s, default);
end;

function GetXMLNode(node: IXMLDOMNode; const name: WideString): WideString;
var
  child: IXMLDOMNode;
begin
  child := node.selectSingleNode(name);
  if Assigned(child)
    then Result := child.text
    else Result := '';
end;

procedure SetXMLAttrStr(node: IXMLDOMNode; const name, value: WideString);
var
  attr: IXMLDOMNode;
begin
  attr := node.ownerDocument.createAttribute(name);
  attr.nodeValue := value;
  node.attributes.setNamedItem(attr);
end;

procedure SetXMLAttrDateTime(node: IXMLDOMNode; const name: WideString; value: TDateTime);
begin
  if value <> 0 then
    SetXMLAttrStr(node, name, DateTimeToStr(value, FormatSettings));
end;

procedure SetXMLAttrInt(node: IXMLDOMNode; const name: WideString; value: Integer);
begin
  SetXMLAttrStr(node, name, IntToStr(value));
end;

procedure SetXMLAttrBool(node: IXMLDOMNode; const name: WideString; value: Boolean); 
begin
  SetXMLAttrStr(node, name, BoolToStr(value));
end;

procedure SetXMLNode(node: IXMLDOMNode; const name, value: WideString);
var
  child: IXMLDOMElement;
begin
  if value <> '' then
  begin
    child := node.ownerDocument.createElement(name);
    child.text := value;
    node.appendChild(child);
  end;
end;

function IsXMLDocument(const s: WideString): Boolean;
begin
  Result := Copy(Trim(s),1,1) = '<';
end;

initialization
  FormatSettings := TFormatSettings.Create('en-AU');  // I3309
//  GetLocaleFormatSettings(GetThreadLocale, FormatSettings);
  FormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  FormatSettings.LongTimeFormat := 'h:mm';
  FormatSettings.DateSeparator := '-';
end.
