(*
  Name:             sysinfo_util
  Copyright:        Copyright (C) SIL International.
  Documentation:    km4.1
  Description:      Utility functions for system information
  Create Date:      13 May 2005

  Modified Date:    19 Jul 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          13 May 2005 - mcdurdin - Integrated into kmshell from tsysinfo
                    09 Jun 2005 - mcdurdin - Use MSXML_TLB not MSXML2_TLB
                    20 Jul 2008 - mcdurdin - I1523 - Report additional items in tsysinfo
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    19 Jul 2011 - mcdurdin - I3000 - Tweak display of diagnostics using .xslt files
*)
unit sysinfo_util;

interface

uses Windows, Classes, Sysutils, Winapi.msxml;

procedure AddFileVersion(node: IXMLDOMNode; const afilename: string);
function TempFileName(ext: string = ''): string;
function XMLTextEncode(s: WideString): WideString;

function xmlAddChild(node: IXMLDOMNode; const name: WideString): IXMLDOMNode;
procedure xmlSetAttribute(node: IXMLDOMNode; const name, value: WideString);
function xmlGetAttribute(node: IXMLDOMNode; const name: WideString): WideString;
function xmlChildValue(node: IXMLDOMNode; const name: WideString): Variant;
function xmlChild(node: IXMLDOMNode; const name: WideString): IXMLDOMNode;

function StrToBoolDef(const str: string; res: Boolean): Boolean;
function VarToStrDef(v: Variant; def: string): string;

implementation

uses
  FileVersionInfo,
  System.Variants;

const
  CSIDL_PROGRAM_FILES = 1;

function XMLTextEncode(s: WideString): WideString;
begin
  Result := s; //UTF16ToUtf8(s);
end;


function xmlAddChild(node: IXMLDOMNode; const name: WideString): IXMLDOMNode;
begin
  Result := node.ownerDocument.createNode(1, name, '');
  node.appendChild(Result);
end;

procedure xmlSetAttribute(node: IXMLDOMNode; const name, value: WideString);
var
  nattr: IXMLDOMNode;
begin
  nattr := node.ownerDocument.createAttribute(name);
  nattr.nodeValue := value;
  node.attributes.setNamedItem(nattr);
end;

function xmlGetAttribute(node: IXMLDOMNode; const name: WideString): WideString;
var
  nattr: IXMLDOMNode;
begin
  nattr := node.attributes.getNamedItem(name);
  if Assigned(nattr)
    then Result := nattr.nodeValue
    else Result := '';
end;

function xmlChild(node: IXMLDOMNode; const name: WideString): IXMLDOMNode;
var
  i: Integer;
begin
  for i := 0 to node.childNodes.Length - 1 do
    if node.childNodes.item[i].nodeName = name then
    begin
      Result := node.childNodes.item[i];
      Exit;
    end;
  Result := nil;
end;

function xmlChildValue(node: IXMLDOMNode; const name: WideString): Variant;
var
  i: Integer;
begin
  for i := 0 to node.childNodes.Length - 1 do
    if node.childNodes.item[i].nodeName = name then
    begin
      Result := node.childNodes.item[i].text;
      Exit;
    end;
  Result := Null;
end;

procedure AddFileVersion(node: IXMLDOMNode; const afilename: string);
begin
  if FileExists( afilename) then
  begin
    with TFileVersionInfo.Create(afilename) do
    try
      node := xmlAddChild(node, 'VersionInformation');
      xmlAddChild(node, 'FileName').Text := XMLTextEncode(FileName);
      xmlAddChild(node, 'VersionCompanyName').Text := XMLTextEncode(StringValue('CompanyName'));
      xmlAddChild(node, 'VersionProductName').Text := XMLTextEncode(StringValue('ProductName'));
      xmlAddChild(node, 'VersionProductVersion').Text := XMLTextEncode(StringValue('ProductVersion'));
      xmlAddChild(node, 'VersionFileDescription').Text := XMLTextEncode(StringValue('FileDescription'));
    finally
      Free;
    end;
  end;
end;

function TempFileName(ext: string = ''): string;
var
  path, filename: array[0..260] of char;
begin
  GetTempPath(260, path);
  GetTempFileName(path, 'tkd', 0, filename);
  if ext <> '' then
  begin
    DeleteFile(filename);
    Result := ChangeFileExt(filename, ext);
  end
  else
    Result := filename;
end;

function StrToBoolDef(const str: string; res: Boolean): Boolean;
begin
  if UpperCase(str) = 'TRUE' then Result := True
  else if StrToIntDef(str, 0) <> 0 then Result := True
  else if (str = 'y') or (str = 'Y') then Result := True
  else if UpperCase(str) = 'FALSE' then Result := False
  else if StrToIntDef(str,-1) = 0 then Result := False
  else if (str = 'n') or (str = 'N') then Result := False
  else Result := res;
end;

function VarToStrDef(v: Variant; def: string): string;
begin
  try
    Result := VarToStr(v);
  except
    Result := def;
  end;
end;

procedure AddFilePEInfo(node: IXMLDOMNode; const FileName: string);
begin
  try
    with TFileVersionInfo.Create(FileName) do
    try
      xmlSetAttribute(node, 'VersionCompanyName', XMLTextEncode(StringValue('CompanyName')));
      xmlSetAttribute(node, 'VersionProductName', XMLTextEncode(StringValue('ProductName')));
      xmlSetAttribute(node, 'VersionProductVersion', XMLTextEncode(StringValue('ProductVersion')));
      xmlSetAttribute(node, 'VersionFileDescription', XMLTextEncode(StringValue('FileDescription')));
    finally
      Free;
    end;
  except
    on E:Exception do
    begin
      try
        xmlSetAttribute(node, 'Error', E.ClassName+': '+E.Message);
      except
        ;
      end;
    end;
  end;
end;

end.
