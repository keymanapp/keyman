(*
  Name:             VisualKeyboardExportXML
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Dec 2006

  Modified Date:    26 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Dec 2006 - mcdurdin - Export to version 7, UTF-8, fix text encoding for key caps
                    22 Jan 2007 - mcdurdin - Export XML files to filename_xml_files subfolder
                    19 Mar 2007 - mcdurdin - I699 - Fix crash when exporting OSK to HTML/XML
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    16 Jan 2009 - mcdurdin - Widestring filenames
                    16 Jan 2009 - mcdurdin - WideString fields
                    04 Jun 2009 - mcdurdin - I2003 - UTF8Encode replacement
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
*)
unit VisualKeyboardSaverXML;  // I3306

interface

uses
  System.Classes,
  VisualKeyboard;

type
  TVisualKeyboardSaverXML = class(TVisualKeyboardSaver)
  public
    procedure SaveToStream(Stream: TStream); override;
  end;

implementation

uses
  System.NetEncoding,
  System.StrUtils,
  System.SysUtils,
  Xml.XmlDoc,
  Xml.XmlIntf,

  KeymanVersion,
  Unicode,
  VKeys;

{ TVisualKeyboardSaverXML }

procedure TVisualKeyboardSaverXML.SaveToStream(Stream: TStream);
var
  FUnicode, i: Integer;
  FShiftIndex: Integer;
  streamIn: TMemoryStream;
  streamOut: TStringStream;
  flags, node, root, header: IXMLNode;
  encodings: array[0..1] of IXMLNode;
  layers: array[0..1, 0..MaxLegalShiftStates-1] of IXMLNode;
  doc: IXMLDocument;
  j: Integer;
begin
  doc := NewXMLDocument;
  doc.Options := [doNodeAutoCreate, doNodeAutoIndent];

  for i := 0 to 1 do
  begin
    encodings[i] := nil;
    for j := 0 to MaxLegalShiftStates-1 do
      layers[i, j] := nil;
  end;

  root := doc.AddChild('visualkeyboard');
  header := root.AddChild('header');
  header.AddChild('version').NodeValue := SKeymanVersion100;
  header.AddChild('kbdname').NodeValue := FKbd.Header.AssociatedKeyboard;
  flags := header.AddChild('flags');
  if kvkh102 in FKbd.Header.Flags then flags.AddChild('key102');
  if kvkhDisplayUnderlying in FKbd.Header.Flags then flags.AddChild('displayunderlying');
  if kvkhUseUnderlying in FKbd.Header.Flags then flags.AddChild('useunderlying');
  if kvkhAltGr in FKbd.Header.Flags then flags.AddChild('usealtgr');
  if FKbd.Header.UnderlyingLayout <> '' then header.AddChild('layout').NodeValue := FKbd.Header.UnderlyingLayout;

  for i := 0 to FKbd.Keys.Count - 1 do
  begin
    if kvkkUnicode in FKbd.Keys[i].Flags
      then FUnicode := 1
      else FUnicode := 0;

    FShiftIndex := GetVKLegalShiftStateIndex(FKbd.Keys[i].Shift);
    if FShiftIndex >= 0 then
    begin
      if layers[FUnicode, FShiftIndex] = nil then
      begin
        if encodings[FUnicode] = nil then
        begin
          encodings[FUnicode] := root.AddChild('encoding');
          encodings[FUnicode].Attributes['name'] := IfThen(FUnicode=1, 'unicode', 'ansi');

          if FUnicode = 0 then
          begin
            encodings[FUnicode].Attributes['fontname'] := FKbd.Header.ANSIFont.Name;
            encodings[FUnicode].Attributes['fontsize'] := FKbd.Header.ANSIFont.Size;
          end
          else
          begin
            encodings[FUnicode].Attributes['fontname'] := FKbd.Header.UnicodeFont.Name;
            encodings[FUnicode].Attributes['fontsize'] := FKbd.Header.UnicodeFont.Size;
          end;
        end;
        layers[FUnicode, FShiftIndex] := encodings[FUnicode].AddChild('layer');
        layers[FUnicode, FShiftIndex].Attributes['shift'] := VKLegalShiftStates[FShiftIndex].Name;
      end;

      node := layers[FUnicode, FShiftIndex].AddChild('key');
      node.Attributes['vkey'] := VKeyNames[FKbd.Keys[i].VKey];

      if (kvkkBitmap in FKbd.Keys[i].Flags) and Assigned(FKbd.Keys[i].Bitmap) then
      begin
        streamIn := TMemoryStream.Create;
        streamOut := TStringStream.Create('', TEncoding.ANSI);
        try
          FKbd.Keys[i].Bitmap.SaveToStream(streamIn);
          streamIn.Position := 0;
          TNetEncoding.Base64.Encode(streamIn, streamOut);
          node.AddChild('bitmap').NodeValue := streamOut.DataString;
        finally
          streamIn.Free;
          streamOut.Free;
        end;
      end
      else
        node.NodeValue := FKbd.Keys[i].Text;
    end;
  end;

  doc.Encoding := 'utf-8';
  doc.SaveToStream(Stream);
end;

end.
