(*
  Name:             VisualKeyboardImportXML
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      19 Nov 2007

  Modified Date:    13 Mar 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    16 Jan 2009 - mcdurdin - Widestring support
                    07 Oct 2011 - mcdurdin - I3099 - Fix crashes importing OSK XML format
                    18 May 2012 - mcdurdin - I3324 - V9.0 - Replace XDOM with MSDOM
                    08 Jun 2012 - mcdurdin - I3324 - V9.0 - Replace XDOM with MSDOM
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    13 Mar 2015 - mcdurdin - I4619 - V9.0 - Keyman Developer fails to import kvk xml file
*)
unit VisualKeyboardImportXML;  // I3324  // I3324

interface

uses SysUtils, Classes, VisualKeyboard;

type
  EVisualKeyboardImportXML = class(Exception);

  TVisualKeyboardImportXML = class(TVisualKeyboardImport)
  private
    function ShiftStateStringToVKShift(s: string): Integer;
  public
    procedure ImportFromFile(FileName: WideString); override;
  end;

implementation

uses
  System.Variants,
  Dialogs,
  Graphics,
  KeymanVersion,
  xmlintf,
  xmldoc,
  Unicode,
  utilstr,
  utilsystem,
  VKeys;

{ TVisualKeyboardImportXML }

procedure TVisualKeyboardImportXML.ImportFromFile(FileName: WideString);   // I4619
var
  doc: IXMLDocument;
  header, node: IXMLNode;
  k: TVisualKeyboardKey;
  n: Word;
  b: TBitmap;
  keys: IXMLNode;

      function FindNode(NodeName: string; parent: IXMLNode = nil): IXMLNode;
      begin
        if parent = nil then parent := doc.DocumentElement;
        Result := parent.ChildNodes.FindNode(NodeName);
      end;
begin
  doc := LoadXMLDocument(FileName);

  header := FindNode('header');
  if not Assigned(header) then
    raise EVisualKeyboardImportXML.Create('Invalid xml file: header is missing');

  { version }
  node := FindNode('version', header);
  if not Assigned(node) or ((node.NodeValue <> SKeymanVersion60) and (node.NodeValue <> SKeymanVersion70)) then
    raise EVisualKeyboardImportXML.Create('Invalid xml file: version value invalid (must be '+SKeymanVersion60+' or '+SKeymanVersion70+')');

  { kbdname }
  node := FindNode('kbdname', header);
  if Assigned(node) then
    FKbd.Header.AssociatedKeyboard := node.NodeValue;

  { flags }
  FKbd.Header.Flags := [];
  node := FindNode('flags', header);
  if Assigned(node) then
  begin
    if Assigned(FindNode('key102', node))            then FKbd.Header.Flags := FKbd.Header.Flags + [kvkh102];
    if Assigned(FindNode('displayunderlying', node)) then FKbd.Header.Flags := FKbd.Header.Flags + [kvkhDisplayUnderlying];
    if Assigned(FindNode('useunderlying', node))     then FKbd.Header.Flags := FKbd.Header.Flags + [kvkhUseUnderlying];
    if Assigned(FindNode('usealtgr', node))          then FKbd.Header.Flags := FKbd.Header.Flags + [kvkhAltGr];
  end;

  { fonts }
  node := FindNode('ansifont', header);
  if Assigned(node) then
  begin
    FKbd.Header.ANSIFont.Name := FindNode('font', node).Attributes['name'];
    FKbd.Header.ANSIFont.Size := StrToIntDef(FindNode('font', node).Attributes['size'], 12);
  end;

  node := FindNode('unicodefont', header);
  if Assigned(node) then
  begin
    FKbd.Header.UnicodeFont.Name := FindNode('font', node).Attributes['name'];
    FKbd.Header.UnicodeFont.Size := StrToIntDef(FindNode('font', node).Attributes['size'], 12);
  end;

  { layout }
  node := FindNode('layout', header);
  if Assigned(node) then
    FKbd.Header.UnderlyingLayout := VarToStr(node.NodeValue);

  { Keys }

  keys := FindNode('keys');
  FKbd.Keys.Clear;
  node := FindNode('key', keys);
  while Assigned(node) do
  begin
    n := FindVKeyName(node.Attributes['vkey']);
    if n = $FFFF then
      raise EVisualKeyboardImportXML.Create('Invalid virtual key code: ' +
        node.Attributes['vkey']);

    k := TVisualKeyboardKey.Create;
    k.VKey := n;
    if node.Attributes['shift'] <> '' then
      k.Shift := ShiftStateStringToVKShift(VarToStr(node.Attributes['shift']));

    if node.HasAttribute('text') then
      k.Text := VarToStr(node.Attributes['text'])
    else
    begin
      b := TBitmap.Create;
      b.LoadFromFile(ExpandFileNameEx(FileName, VarToStr(node.Attributes['bitmap'])));
      k.Bitmap := b;
      k.Flags := [kvkkBitmap];
    end;

    if VarToStr(node.Attributes['unicode']) = '1' then
      k.Flags := k.Flags + [kvkkUnicode];

    FKbd.Keys.Add(k);

    node := node.NextSibling;
    while Assigned(node) and (node.NodeName <> 'key') do
      node := node.NextSibling;
  end;
end;

const
  nl = #13#10;
  FDTD: WideString =
    '  <!ELEMENT visualkeyboard (header, keys)>'+nl+
    '  <!ELEMENT header (version, kbdname?, flags?, ansifont?, unicodefont?, keybitmap?, layout?, filename?)>'+nl+  // I3099
    '  <!ELEMENT version (#PCDATA)>'+nl+
    '  <!ELEMENT kbdname (#PCDATA)>'+nl+
    '  <!ELEMENT filename (#PCDATA)>'+nl+  // I3099
    '  <!ELEMENT flags (key102?, displayunderlying?, useunderlying?, usealtgr?)>'+nl+
    '  <!ELEMENT key102 EMPTY>'+nl+
    '  <!ELEMENT displayunderlying EMPTY>'+nl+
    '  <!ELEMENT useunderlying EMPTY>'+nl+
    '  <!ELEMENT usealtgr EMPTY>'+nl+
    '  <!ELEMENT ansifont (font)>'+nl+
    '  <!ELEMENT unicodefont (font)>'+nl+
    '  <!ELEMENT font EMPTY>'+nl+
    '  <!ATTLIST font'+nl+
    '    name CDATA #REQUIRED'+nl+
    '    size CDATA #REQUIRED>'+nl+
    '  <!ELEMENT keybitmap (#PCDATA)>'+nl+
    '  <!ELEMENT layout (#PCDATA)>'+nl+
    '  <!ELEMENT keys (key+)>'+nl+
    '  <!ELEMENT key EMPTY>'+nl+
    '  <!ATTLIST key'+nl+
    '    vkey CDATA #REQUIRED'+nl+
    '    shift CDATA #IMPLIED'+nl+
    '    unicode CDATA #IMPLIED'+nl+
    '    bitmap CDATA #IMPLIED'+nl+
    '    text CDATA #IMPLIED>'+nl;

function TVisualKeyboardImportXML.ShiftStateStringToVKShift(s: string): Integer;
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to High(VKLegalShiftStates) do  // I3099
    if SameText(VKLegalShiftStates[i].Name, s) then
    begin
      Result := VKLegalShiftStates[i].Shift;
      Exit;
    end;
end;

end.
