(*
  Name:             XString
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Jun 2008

  Modified Date:    13 Dec 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Jun 2008 - mcdurdin - I1365 - Add CODE_NOTANY support
                    13 Dec 2012 - mcdurdin - I3661 - V9.0 - XString unit needs updating to support 8 and 9 stores
*)
unit XString;

interface

uses
  Classes,
  contnrs, kmxfile, kmxfileconsts, debugkeyboard;


type
  TXStringFormatOptions = set of (xfoHexadecimal, xfoQuoteChar);

  TXStringElement = class
    Name: WideString;
    StoreIndex: Integer;
    Store: WideString;
    Pos, Tag: Integer;
  end;

  TXStringElementList = class(TObjectList)
  protected
    function Get(Index: Integer): TXStringElement;
    procedure Put(Index: Integer; Item: TXStringElement);
  public
    property Items[Index: Integer]: TXStringElement read Get write Put; default;
    function Add(Item: TXStringElement): Integer;
  end;

  TXString = class
  private
    Fkbd: TDebugKeyboard;
    FString: WideString;
    function XCharLength(pos: Integer): Integer;
    function IntFormatChar(Index: Integer; Options: TXStringFormatOptions; var storeindex: Integer): WideString;
    function IsSentinel(pos: Integer): Boolean;
  public
    constructor Create(AString: WideString; Akbd: TDebugKeyboard);
    function XLength: Integer;
    procedure FormatElement(pos: Integer; Element: TXStringElement; Options: TXStringFormatOptions);
    procedure FormatElements(Elements: TXStringElementList; Options: TXStringFormatOptions);
    function FormatChar(Index: Integer; Options: TXStringFormatOptions): WideString;
    function FormatString(Options: TXStringFormatOptions): WideString;
    function SubString(StartIndex, Len: Integer): WideString;
    property XString: WideString read FString write FString;
  end;

function XStringCodeLength(code: Integer): Integer;

implementation

uses
  StrUtils,
  SysUtils;
{
function XStringToWideString(var p: WideString; OutputLength: Integer): WideString;
var
  n: Integer;
  InQuotes: Boolean;
  s: WideString;
begin
  s := '';
  InQuotes := False;

  while n1 > 0 do
  begin
    if p = '' then Break;
    if Ord(p[1]) = UC_SENTINEL then
    begin
      if InQuotes then s := s + '''';
      InQuotes := False;
      if Length(p) < 2 then Break;
      n := GetXStringCodeLength(Ord(p[2]));
      case Ord(p[2]) of
        CODE_NUL:      s := s + ' nul';
        CODE_BEEP:     s := s + ' beep';
        CODE_DEADKEY:  s := s + ' deadkey';
        CODE_EXTENDED: s := s + ' ['+VKeyNames[Ord(p[4])]+']';
      else
        s := s + ' (illegal)';
      end;

      Delete(p, 1, n+2);
    end
    else
    begin
      if not FShowHexadecimal then
      begin
        if not InQuotes then s := s + ' ''';
        InQuotes := True;
        s := s + p[1];
      end
      else
        s := s + Format(' U+%4.4X', [Ord(p[1])]);

      Delete(p, 1, 1);
    end;
    Dec(n1);
  end;

  if InQuotes then s := s + '''';
  if (s <> '') and (s[1] = ' ') then Delete(s, 1, 1);
  if s <> '' then Result := s + ' ' else Result := '';
end;
}

{ TXString }

constructor TXString.Create(AString: WideString; Akbd: TDebugKeyboard);
begin
  inherited Create;
  FString := AString;
  Fkbd := Akbd;
end;

function TXString.IsSentinel(pos: Integer): Boolean;
begin
  Result := Ord(FString[pos]) = UC_SENTINEL;
end;

function TXString.XCharLength(pos: Integer): Integer;
begin
  if IsSentinel(pos)
    then Result := XStringCodeLength(Ord(FString[pos+1])) + 2
    else Result := 1;
end;

function XStringCodeLength(code: Integer): Integer;
begin
  case code of
		CODE_ANY:			     Result := 1;
		CODE_INDEX:		     Result := 2;
		CODE_USE:			     Result := 1;
		CODE_DEADKEY:		   Result := 1;
		CODE_EXTENDED:		 Result := 3;
		CODE_CLEARCONTEXT: Result := 1;
		CODE_CALL:			   Result := 1;
		CODE_CONTEXTEX:	   Result := 1;
    CODE_NOTANY:       Result := 1;

    CODE_IFOPT:         Result := 3;  // I3661
    CODE_IFSYSTEMSTORE: Result := 3;  // I3661
    CODE_SETOPT:        Result := 2;  // I3661
    CODE_SETSYSTEMSTORE:Result := 2;  // I3661
    CODE_RESETOPT:      Result := 1;  // I3661
    CODE_SAVEOPT:       Result := 1;  // I3661

	else                 Result := 0;
	end;
end;

function TXString.FormatChar(Index: Integer; Options: TXStringFormatOptions): WideString;
var
  storeindex: Integer;
begin
  Result := IntFormatChar(Index, Options, storeindex);
end;

function FormatVirtualKey(n1, n2: Integer): WideString;
begin
  Result := '[K_x]';
end;

function TXString.IntFormatChar(Index: Integer; Options: TXStringFormatOptions; var storeindex: Integer): WideString;
var
  systemstoreindex: TSystemStore;
  storeindex2: Integer;
begin
  storeindex := -1;
  if IsSentinel(Index) then
  begin
    case Ord(FString[Index+1]) of
      CODE_ANY:           begin storeindex := Ord(FString[Index+2])-1; Result := Format('any(%s)', [Fkbd.Stores[storeindex].Name]); end;
      CODE_INDEX:         begin storeindex := Ord(FString[Index+2])-1; Result := Format('index(%s, %d)', [Fkbd.Stores[storeindex].Name, Ord(FString[Index+3])]); end;
      CODE_CONTEXT:       Result := 'context';
      CODE_NUL:           Result := 'nul';
      CODE_USE:           Result := Format('use(%s)', [FKbd.Groups[Ord(FString[Index+2])-1].Name]);
      CODE_RETURN:        Result := 'return';
      CODE_BEEP:          Result := 'beep';
      CODE_DEADKEY:       Result := Format('dk(%s)', [FKbd.Deadkeys[Ord(FString[Index+2])-1].Name]);
      CODE_EXTENDED:      Result := FormatVirtualKey(Ord(FString[Index+2]), Ord(FString[Index+3]));
      CODE_SWITCH:        Result := '??switch';
      CODE_KEY:           Result := '??key';
      CODE_CLEARCONTEXT:  Result := 'clearcontext';
      CODE_CALL:          Result := 'call(func)';
      CODE_CONTEXTEX:     Result := Format('context(%d)', [Ord(FString[Index+2])]);
      CODE_NOTANY:        begin storeindex := Ord(FString[Index+2])-1; Result := Format('notany(%s)', [Fkbd.Stores[storeindex].Name]); end;

      CODE_IFOPT:
        begin
          storeindex := Ord(FString[Index+2])-1;
          storeindex2 := Ord(FString[Index+3])-1;
          Result := Format('if(%s %s "%s")',
           [Fkbd.Stores[storeindex].Name,
            IfThen(Ord(FString[Index+4]) = 1, '!=', '='),
            Fkbd.Stores[storeindex2].AString]);   // I3661
        end;
      CODE_IFSYSTEMSTORE:
        begin
          systemstoreindex := TSystemStore(Ord(FString[Index+2])-1);
          storeindex2 := Ord(FString[Index+3])-1;
          Result := Format('if(&%s %s "%s")',
           [SystemStoreNames[systemstoreindex],
            IfThen(Ord(FString[Index+4]) = 1, '!=', '='),
            Fkbd.Stores[storeindex2].AString]);   // I3661
        end;
      CODE_SETOPT:
        begin
          storeindex := Ord(FString[Index+2])-1;
          storeindex2 := Ord(FString[Index+3])-1;
          Result := Format('set(%s = "%s")',
           [Fkbd.Stores[storeindex].Name,
            Fkbd.Stores[storeindex2].AString]);   // I3661
        end;
      CODE_SETSYSTEMSTORE:
        begin
          systemstoreindex := TSystemStore(Ord(FString[Index+2])-1);
          storeindex2 := Ord(FString[Index+3])-1;
          Result := Format('set(%s = "%s")',
           [SystemStoreNames[systemstoreindex],
            Fkbd.Stores[storeindex2].AString]);   // I3661
        end;
      CODE_RESETOPT:
        begin
          storeindex := Ord(FString[Index+2])-1;
          Result := Format('reset(%s)', [Fkbd.Stores[storeindex].Name]);   // I3661
        end;
      CODE_SAVEOPT:
        begin
          storeindex := Ord(FString[Index+2])-1;
          Result := Format('save(%s)', [Fkbd.Stores[storeindex].Name]);   // I3661
        end;
    else                  Result := Format('Unknown(%d)', [Ord(FString[Index+1])]);
    end;
  end
  else if xfoHexadecimal in Options then
    Result := Format('U+%4.4X', [Ord(FString[Index])])
  else
  begin
    if xfoQuoteChar in Options then
    begin
      Result := '''';
      Result := Result + FString[Index];
      Result := Result + '''';
    end
    else Result := FString[Index];
  end;
end;

function TXString.FormatString(Options: TXStringFormatOptions): WideString;
var
  i: Integer;
  res, ch: WideString;
  FInQuotes: Boolean;
begin
  res := '';
  FInQuotes := False;
  i := 1;
  while i <= Length(FString) do
  begin
    ch := FormatChar(i, Options);
    if Length(ch) = 1 then
    begin
      if not FInQuotes then res := Trim(res + ' ''');
      res := res + ch;
      FInQuotes := True;
    end
    else
    begin
      if FInQuotes then res := res + '''';
      res := Trim(res + ' ' + ch);
      FInQuotes := False;
    end;
    i := i + XCharLength(i);
  end;
  if FInQuotes then res := res + '''';
  Result := res;
end;


procedure TXString.FormatElement(pos: Integer; Element: TXStringElement; Options: TXStringFormatOptions);
var
  storeindex: Integer;
begin
  Element.Name := IntFormatChar(pos, Options + [xfoQuoteChar], storeindex);
  
  Element.StoreIndex := storeindex;
  if storeindex > -1
    then Element.Store := Fkbd.Stores[storeindex].AString
    else Element.Store := '';
  Element.Pos := pos;
end;

procedure TXString.FormatElements(Elements: TXStringElementList; Options: TXStringFormatOptions);
var
  el: TXStringElement;
  i: Integer;
begin
  i := 1;
  while i <= Length(FString) do
  begin
    el := TXStringElement.Create;
    FormatElement(i, el, Options);
    Elements.Add(el);
    i := i + XCharLength(i);
  end;
end;

function TXString.XLength: Integer;
var
  i, len: Integer;
begin
  i := 1; len := 0;
  while i <= Length(FString) do
  begin
    i := i + XCharLength(i);
    Inc(len);
  end;
  Result := len;
end;

function TXString.SubString(StartIndex, Len: Integer): WideString;
var
  i, p, n1, n2: Integer;
begin
  n2 := -1; n1 := 0;                    
  i := 1; p := 1;
  while (i <= Length(FString)) and (n2 < 0) do
  begin
    if p = StartIndex then n1 := i;
    if p = StartIndex+Len then n2 := i;
    i := i + XCharLength(i);
    Inc(p);
  end;
  if n2 = -1 then n2 := Length(FString)+1;
  Result := Copy(FString, n1, n2-n1);
end;

{ TXStringElementList }

function TXStringElementList.Add(Item: TXStringElement): Integer;
begin
  Result := inherited Add(Item);
end;

function TXStringElementList.Get(Index: Integer): TXStringElement;
begin
  Result := TXStringElement(inherited Get(Index));
end;

procedure TXStringElementList.Put(Index: Integer; Item: TXStringElement);
begin
  inherited Put(Index, Item);
end;

end.
