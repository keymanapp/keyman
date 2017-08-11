(*
  Name:             JsonUtil
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      10 Oct 2014

  Modified Date:    30 May 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          10 Oct 2014 - mcdurdin - I4440 - V9.0 - If more than one language listed for a keyboard, the JSON file becomes invalid
                    30 May 2015 - mcdurdin - I4727 - Changing font while touch layout editor in Code mode results in broken \ rules
*)
unit JsonUtil;

interface

uses
  System.JSON,
  System.Classes;

function JSONToString(obj: TJSONAncestor; ReplaceSlashes: Boolean = False): string;

// Formats JSONValue to an indented structure and adds it to OutputStrings
procedure PrettyPrintJSON(JSONValue: TJSONValue; OutputStrings: TStrings; indent: integer = 0);
function ParseJSONValue(const Data: string; var Offset: Integer): TJSONObject;   // I4035   // I4260
function JSONDateToDateTime(const Value: string; var DateTime: TDateTime): Boolean;
function DateTimeToJSONDate(ADateTime: TDateTime): string;

implementation

uses
  Soap.XsBuiltIns,

  System.StrUtils,
  System.SysUtils;

function JSONToString(obj: TJSONAncestor; ReplaceSlashes: Boolean = False): string;
var
  bytes: TBytes;
  len: Integer;
begin
  SetLength(bytes, obj.EstimatedByteSize);
  len := obj.ToBytes(bytes, 0);
  Result := TEncoding.UTF8.GetString(bytes, 0, len);
  if ReplaceSlashes then
    Result := ReplaceStr(Result, '\/', '/');
end;

const INDENT_SIZE = 2;

procedure PrettyPrintPair(JSONValue: TJSONPair; OutputStrings: TStrings; last: boolean; indent: integer);
const TEMPLATE = '%s : %s';
var
  line: string;
  newList: TStringList;
begin
  newList := TStringList.Create;
  try
    PrettyPrintJSON(JSONValue.JsonValue, newList, indent);
    line := format(TEMPLATE, [JSONValue.JsonString.ToString, Trim(newList.Text)]);
  finally
    newList.Free;
  end;

  line := StringOfChar(' ', indent * INDENT_SIZE) + line;
  if not last then
    line := line + ',';
  OutputStrings.add(line);
end;

procedure PrettyPrintArray(JSONValue: TJSONArray; OutputStrings: TStrings; last: boolean; indent: integer);
var i: integer;
begin
   OutputStrings.add(StringOfChar(' ', indent * INDENT_SIZE) + '[');
   for i := 0 to JSONValue.Count - 1 do
   begin
      PrettyPrintJSON(JSONValue.Items[i], OutputStrings, indent);
      if i < JSONValue.Count - 1 then
        OutputStrings[OutputStrings.Count-1] := OutputStrings[OutputStrings.Count-1] + ',';   // I4440
   end;
   OutputStrings.add(StringOfChar(' ', indent * INDENT_SIZE) + ']');
end;

procedure PrettyPrintJSON(JSONValue: TJSONValue; OutputStrings: TStrings; indent: integer = 0);
var
  i: integer;
begin
  if JSONValue is TJSONObject then
  begin
    OutputStrings.add(StringOfChar(' ', indent * INDENT_SIZE) + '{');
    for i := 0 to TJSONObject(JSONValue).Count - 1 do
      PrettyPrintPair(TJSONObject(JSONValue).Pairs[i], OutputStrings, i = TJSONObject(JSONValue).Count - 1, indent + 1);
    OutputStrings.add(StringOfChar(' ', indent * INDENT_SIZE) + '}');
  end
  else if JSONValue is TJSONArray then
    PrettyPrintArray(TJSONArray(JSONValue), OutputStrings, True, indent + 1)
  else
    OutputStrings.add(StringOfChar(' ', indent * INDENT_SIZE) + JSONToString(JSONValue, True));   // I4727
end;

function RemoveWhites(const str1: string): string;
var
  ch: char;
  inQuotes: Boolean;
begin
  Result := '';
  inQuotes := False;
  for ch in str1 do
  begin
    if ch = '"' then
      inQuotes := not inQuotes;
    if InQuotes or not CharInSet(ch, [' ',#9,#10,#13]) then
      Result := Result + ch;
  end;
end;

function ParseJSONValue(const Data: string; var Offset: Integer): TJSONObject;   // I4035   // I4260
var
  UTF8Data: TBytes;
begin
  UTF8Data := TEncoding.UTF8.GetBytes(RemoveWhites(Data));
  Result := TJSONObject.ParseJSONValue(UTF8Data, Offset, True) as TJSONObject;   // I4083
end;

function JSONDateToDateTime(const Value: string; var DateTime: TDateTime): Boolean;
begin
  try
    with TXSDateTime.Create do
    try
      XSToNative(Value);
      DateTime := AsDateTime;
      Result := True;
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

function DateTimeToJSONDate(ADateTime: TDateTime): string;
begin
  try
    with TXSDateTime.Create do
    try
      AsDateTime := ADateTime;
      Result := NativeToXS;
    finally
      Free;
    end;
  except
    Result := '';
  end;
end;

end.

