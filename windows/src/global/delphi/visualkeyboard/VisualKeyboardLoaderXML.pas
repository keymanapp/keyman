unit VisualKeyboardLoaderXML;

interface

uses
  System.SysUtils,
  System.Classes,
  Xml.XMLIntf,

  VisualKeyboard;

//
// This originated with the TVisualKeyboardImportXML code but has now diverged for
// UI-clean single-file round-trippable consistency.
//

type
  EVisualKeyboardLoaderXML = class(EVisualKeyboardLoader);

  TVisualKeyboardLoaderXML = class(TVisualKeyboardLoader)
  private
    function ShiftStateStringToVKShift(s: string): Integer;
    procedure LoadEncoding(node: IXMLNode);
    procedure LoadLayer(unicode: Boolean; layer: IXMLNode);
  public
    procedure LoadFromStream(Stream: TStream); override;
  end;

implementation


uses
  System.NetEncoding,
  System.Variants,
  Vcl.Graphics,
  Xml.XMLDoc,

  KeymanVersion,
  Unicode,
  utilstr,
  utilsystem,
  VKeys;

{ TVisualKeyboardLoaderXML }

procedure TVisualKeyboardLoaderXML.LoadEncoding(node: IXMLNode);
var
  i: Integer;
  FUnicode: Boolean;
begin
  if node.HasAttribute('name') then
  begin
    if node.Attributes['name'] = 'unicode' then FUnicode := True
    else if node.Attributes['name'] = 'ansi'  then FUnicode := False
    else raise EVisualKeyboardLoaderXML.Create('Invalid encoding name '+node.Attributes['name']);
  end
  else
    FUnicode := True;

  { fonts }

  if node.HasAttribute('fontname') then
    if FUnicode
      then FKbd.Header.UnicodeFont.Name := node.Attributes['fontname']
      else FKbd.Header.ANSIFont.Name := node.Attributes['fontname'];

  try
    if node.HasAttribute('fontsize') then
      if FUnicode
        then FKbd.Header.UnicodeFont.Size := StrToInt(node.Attributes['fontsize'])
        else FKbd.Header.ANSIFont.Size := StrToInt(node.Attributes['fontsize']);
  except
    on E:EConvertError do
      raise EVisualKeyboardLoaderXML.Create('Invalid font size '+node.Attributes['fontsize']);
  end;

  for i := 0 to node.ChildNodes.Count - 1 do
  begin
    if node.ChildNodes[i].NodeName = 'layer' then
      LoadLayer(FUnicode, node.ChildNodes[i]);
  end;
end;

procedure TVisualKeyboardLoaderXML.LoadLayer(unicode: Boolean; layer: IXMLNode);
var
  shift: Integer;
  k: TVisualKeyboardKey;
  n: Word;
  b: TBitmap;
  ssBitmapIn: TStringStream;
  msBitmapOut: TMemoryStream;
  node, bitmap: IXMLNode;
begin
  if layer.HasAttribute('shift')
    then shift := ShiftStateStringToVKShift(VarToStr(layer.Attributes['shift']))
    else shift := KVKS_NORMAL;

  node := layer.ChildNodes.FindNode('key');
  while Assigned(node) do
  begin
    n := FindVKeyName(node.Attributes['vkey']);
    if n = $FFFF then
      raise EVisualKeyboardLoaderXML.Create('Invalid virtual key code: ' +
        node.Attributes['vkey']);

    k := TVisualKeyboardKey.Create;
    k.VKey := n;
    k.Shift := shift;
    if unicode then k.Flags := k.Flags + [kvkkUnicode];

    bitmap := node.ChildNodes.FindNode('bitmap');
    if Assigned(bitmap) then
    begin
      b := TBitmap.Create;
      ssBitmapIn := TStringStream.Create(VarToStr(bitmap.NodeValue));
      msBitmapOut := TMemoryStream.Create;
      try
        TNetEncoding.Base64.Decode(ssBitmapIn, msBitmapOut);
        msBitmapOut.Position := 0;
        b.LoadFromStream(msBitmapOut);
      finally
        ssBitmapIn.Free;
        msBitmapOut.Free;
      end;
      k.Bitmap := b;
      k.Flags := k.Flags + [kvkkBitmap];
    end
    else
      k.Text := VarToStr(node.NodeValue);

    FKbd.Keys.Add(k);

    node := node.NextSibling;
    while Assigned(node) and (node.NodeName <> 'key') do
      node := node.NextSibling;
  end;
end;

procedure TVisualKeyboardLoaderXML.LoadFromStream(Stream: TStream);    // I4619
var
  doc: IXMLDocument;
  header, node: IXMLNode;
  i: Integer;
  root: IXMLNode;

      function FindNode(NodeName: string; parent: IXMLNode = nil): IXMLNode;
      begin
        if parent = nil then parent := doc.DocumentElement;
        Result := parent.ChildNodes.FindNode(NodeName);
      end;
begin
  FKbd.Clear;

  doc := TXMLDocument.Create(nil);
  doc.LoadFromStream(Stream, xetUTF_8);

  header := FindNode('header');
  if not Assigned(header) then
    raise EVisualKeyboardLoaderXML.Create('Invalid kvk xml: header is missing');

  { version }
  node := FindNode('version', header);
  if not Assigned(node) or (node.NodeValue <> SKeymanVersion100) then
    raise EVisualKeyboardLoaderXML.Create('Invalid kvk xml: version value invalid (must be '+SKeymanVersion100+')');

  { kbdname }
  node := FindNode('kbdname', header);
  if Assigned(node) then
    FKbd.Header.AssociatedKeyboard := VarToStr(node.NodeValue);

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

  { layout }
  node := FindNode('layout', header);
  if Assigned(node) then
    FKbd.Header.UnderlyingLayout := VarToStr(node.NodeValue);

  { keys }

  root := doc.DocumentElement;
  for i := 0 to root.ChildNodes.Count - 1 do
    if root.ChildNodes[i].NodeName = 'encoding' then
      LoadEncoding(root.ChildNodes[i]);
end;

function TVisualKeyboardLoaderXML.ShiftStateStringToVKShift(s: string): Integer;
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
