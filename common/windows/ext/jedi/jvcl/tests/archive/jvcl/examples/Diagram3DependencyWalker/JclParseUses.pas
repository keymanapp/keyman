{$I JVCL.INC}
unit JclParseUses;

interface

uses
  Classes, SysUtils;

type
  EUsesListError = class(Exception);

  TUsesList = class
  private
    FText: string;
    function GetCount: Integer;
    function GetItems(Index: Integer): string;
  public
    constructor Create(const AText: PChar);
    function Add(const UnitName: string): Integer;
    function IndexOf(const UnitName: string): Integer;
    procedure Insert(Index: Integer; const UnitName: string);
    procedure Remove(Index: Integer);
    property Text: string read FText;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: string read GetItems; default;
  end;

  TCustomGoal = class
  public
    constructor Create(Text: PChar); virtual; abstract;
  end;

  TProgramGoal = class(TCustomGoal)
  private
    FTextAfterUses: string;
    FTextBeforeUses: string;
    FUsesList: TUsesList;
  public
    constructor Create(Text: PChar); override;
    destructor Destroy; override;
    property TextAfterUses: string read FTextAfterUses;
    property TextBeforeUses: string read FTextBeforeUses;
    property UsesList: TUsesList read FUsesList;
  end;

  TLibraryGoal = class(TCustomGoal)
  private
    FTextAfterUses: string;
    FTextBeforeUses: string;
    FUsesList: TUsesList;
  public
    constructor Create(Text: PChar); override;
    destructor Destroy; override;
    property TextAfterUses: string read FTextAfterUses;
    property TextBeforeUses: string read FTextBeforeUses;
    property UsesList: TUsesList read FUsesList;
  end;

  TUnitGoal = class(TCustomGoal)
  private
    FTextAfterImpl: string;
    FTextAfterIntf: string;
    FTextBeforeIntf: string;
    FUsesImpl: TUsesList;
    FUsesIntf: TUsesList;
  public
    constructor Create(Text: PChar); override;
    destructor Destroy; override;
    property TextAfterImpl: string read FTextAfterImpl;
    property TextAfterIntf: string read FTextAfterIntf;
    property TextBeforeIntf: string read FTextBeforeIntf;
    property UsesImpl: TUsesList read FUsesImpl;
    property UsesIntf: TUsesList read FUsesIntf;
  end;

function CreateGoal(Text: PChar): TCustomGoal;

implementation

uses
{$IFDEF COMPILER6_UP}
  RtlConsts;
{$ELSE}
  Consts;
{$ENDIF}

const
  Blanks: TSysCharSet = [#9, #10, #13, ' '];
  SLibrary = 'library';
  SProgram = 'program';
  SUnit = 'unit';
  SUses = 'uses';

resourcestring
  SDuplicateUnit = 'Duplicate unit ''%s''';
  SInvalidLibrary = 'Invalid library';
  SInvalidProgram = 'Invalid program';
  SInvalidUnit = 'Invalid unit';
  SInvalidUses = 'Invalid uses clause';

function PeekIdentifier(var P:PChar):boolean;forward;

function PeekKeyword(var P: PChar; Keyword: PChar): Boolean; forward;
function ReadIdentifier(var P: PChar): string; forward;
procedure SkipCommentsAndBlanks(var P: PChar); forward;

//----------------------------------------------------------------------------

function CheckIdentifier(var P: PChar): Boolean;
begin
  Result := P^ in ['A'..'Z', '_', 'a'..'z'];
  if Result then
  begin
    Inc(P);
    while P^ in ['0'..'9', 'A'..'Z', '_', 'a'..'z'] do
      Inc(P);
  end;
end;

//----------------------------------------------------------------------------

function CheckKeyword(var P: PChar; Keyword: PChar): Boolean;
var
  KeywordLen: Integer;
begin
  KeywordLen := StrLen(Keyword);
  Result := StrLIComp(P, Keyword, KeywordLen) = 0;
  if Result then
    Inc(P, KeywordLen);
end;

//----------------------------------------------------------------------------

function CreateGoal(Text: PChar): TCustomGoal;
var
  P: PChar;
begin
  Result := nil;
  P := Text;

  SkipCommentsAndBlanks(P);
  if PeekKeyword(P, SProgram) then
    Result := TProgramGoal.Create(Text)
  else
  if PeekKeyword(P, SLibrary) then
    Result := TLibraryGoal.Create(Text)
  else
  if PeekKeyword(P, SUnit) then
    Result := TUnitGoal.Create(Text);
end;

//----------------------------------------------------------------------------

function PeekKeyword(var P: PChar; Keyword: PChar): Boolean;
var
  KeywordLen: Integer;
begin
  KeywordLen := StrLen(Keyword);
  Result := StrLIComp(P, KeyWord, KeywordLen) = 0;
end;

//----------------------------------------------------------------------------

function PeekIdentifier(var P: PChar):boolean;
var Q:PChar;
begin
  Q := P;
  Result := CheckIdentifier(P);
  P := Q;
end;


function ReadIdentifier(var P: PChar): string;
var
  PStart: PChar;
begin
  Result := '';

  if P^ in ['A'..'Z', '_', 'a'..'z'] then
  begin
    PStart := P;
    
    Inc(P);
    while P^ in ['0'..'9', 'A'..'Z', '_', 'a'..'z'] do
      Inc(P);

    SetString(Result, PStart, P - PStart);
  end;
end;

//----------------------------------------------------------------------------

procedure SkipChars(var P: PChar; Chars: TSysCharSet);
begin
  while P^ in Chars do
    Inc(P);
end;

//----------------------------------------------------------------------------

procedure SkipComments(var P: PChar);
var
  Test: PChar;
begin
  if P^ = '{' then
  begin
    Test := StrScan(P, '}');
    if Test <> nil then
      P := Test + 1;
  end
  else
  if StrLComp(P, '(*', 2) = 0 then
  begin
    Test := StrPos(P, '*)');
    if Test <> nil then
      P := Test + 2;
  end
  else
  if StrLComp(P, '//', 2) = 0 then
  begin
    Test := StrPos(P, #13#10);
    if Test <> nil then
      P := Test + 2;
  end;
end;

//----------------------------------------------------------------------------

procedure SkipCommentsAndBlanks(var P: PChar);
var
  Test: PChar;
begin
  repeat
    Test := P;
    SkipChars(P, Blanks);
    SkipComments(P);
  until Test = P;
end;

//----------------------------------------------------------------------------
{ TUsesList private }
//----------------------------------------------------------------------------

function TUsesList.GetCount: Integer;
var
  P: PChar;
begin
  Result := 0;

  if FText = '' then
    Exit;

  P := PChar(FText);
  // an empty uses clause consisting of only blanks and comments
  // (resulting from removal of the last unit) is valid too
  SkipCommentsAndBlanks(P);
  if P^ = #0 then
    Exit;

  if not CheckKeyword(P, SUses) then
    raise EUsesListError.Create(SInvalidUses);

  while P^ <> #0 do
  begin
    SkipCommentsAndBlanks(P);
    if not CheckIdentifier(P) then
      raise EUsesListError.Create(SInvalidUses);
    Inc(Result);
    SkipCommentsAndBlanks(P);

    if PeekKeyword(P, 'in') then
    begin
      Inc(P, 2);
      SkipCommentsAndBlanks(P);
      if P^ <> '''' then
        raise EUsesListError.Create(SInvalidUses);
      Inc(P);
        
      while not (P^ in [#0, '''']) do
        Inc(P);
      if P^ <> '''' then
        raise EUsesListError.Create(SInvalidUses);
      Inc(P);
      SkipCommentsAndBlanks(P);
    end;

    case P^ of
      ',':
        Inc(P);
      ';':
        Break;
      else
        if not PeekIdentifier(P) then
          raise EUsesListError.Create(SInvalidUses);
    end;
  end;
end;

//----------------------------------------------------------------------------

function TUsesList.GetItems(Index: Integer): string;
var
  P, PIdentifier: PChar;
  I: Integer;
begin
  Result := '';

  if (Index < 0) or (Index > Count - 1) then
    raise EUsesListError.CreateFmt(SListIndexError, [Index]);

  P := PChar(FText);
  if not CheckKeyword(P, SUses) then
    raise EUsesListError.Create(SInvalidUses);
  I := -1;
  while P^ <> #0 do
  begin
    SkipCommentsAndBlanks(P);
    PIdentifier := P;
    if not CheckIdentifier(P) then
      raise EUsesListError.Create(SInvalidUses);

    Inc(I);
    if I = Index then
    begin
      while PIdentifier^ in ['0'..'9', 'A'..'Z', '_', 'a'..'z'] do
      begin
        Result := Result + PIdentifier^;
        Inc(PIdentifier);
      end;
      Exit;
    end;
    SkipCommentsAndBlanks(P);

    if PeekKeyword(P, 'in') then
    begin
      Inc(P, 2);
      SkipCommentsAndBlanks(P);
      if P^ <> '''' then
        raise EUsesListError.Create(SInvalidUses);
      Inc(P);

      while not (P^ in [#0, '''']) do
        Inc(P);
      if P^ <> '''' then
        raise EUsesListError.Create(SInvalidUses);
      Inc(P);
      SkipCommentsAndBlanks(P);
    end;

    case P^ of
      ',':
        Inc(P);
      ';':
        Break;
      else
        if not PeekIdentifier(P) then
          raise EUsesListError.Create(SInvalidUses);
    end;
  end;
end;

//----------------------------------------------------------------------------
{ TUsesList public }
//----------------------------------------------------------------------------

constructor TUsesList.Create(const AText: PChar);
var
  P, PStart: PChar;
begin
  inherited Create;
  FText := '';
  if AText = nil then
    Exit;
    
  PStart := PChar(AText);
  P := PStart;
  if CheckKeyword(P, SUses) then
  begin
    while P^ <> #0 do
    begin
      SkipCommentsAndBlanks(P);
      if not CheckIdentifier(P) then
        raise EUsesListError.Create(SInvalidUses);
      SkipCommentsAndBlanks(P);


      if PeekKeyword(P, 'in') then
      begin
        Inc(P, 2);
        SkipCommentsAndBlanks(P);
        if P^ <> '''' then
          raise EUsesListError.Create(SInvalidUses);
        Inc(P);

        while not (P^ in [#0, '''']) do
          Inc(P);
        if P^ <> '''' then
          raise EUsesListError.Create(SInvalidUses);
        Inc(P);
        SkipCommentsAndBlanks(P);
      end;

      case P^ of
        ',':
          Inc(P);
        ';':
          begin
            Inc(P);
            Break;
          end;
        else
          if not PeekIdentifier(P) then
            raise EUsesListError.Create(SInvalidUses)
      end;
    end;
    SetString(FText, PStart, P - PStart);
  end;
end;

//----------------------------------------------------------------------------

function TUsesList.Add(const UnitName: string): Integer;
var
  I: Integer;
  P: PChar;
begin
  Result := -1;

  I := IndexOf(UnitName);
  if I <> -1 then
    raise EUsesListError.CreateFmt(SDuplicateUnit, [UnitName]);

  if FText = '' then
  begin
    FText := Format('%s'#13#10'  %s;'#13#10#13#10, [SUses, UnitName]);
    try
      Result := IndexOf(UnitName);
    except
      FText := '';
      raise;
    end;
  end
  else
  begin
    P := PChar(FText);
    if not CheckKeyword(P, SUses) then
      raise EUsesListError.Create(SInvalidUses);

    while P^ <> #0 do
    begin
      SkipCommentsAndBlanks(P);
      if not CheckIdentifier(P) then
        raise EUsesListError.Create(SInvalidUses);

      SkipCommentsAndBlanks(P);

      if PeekKeyword(P, 'in') then
      begin
        Inc(P, 2);
        SkipCommentsAndBlanks(P);
        if P^ <> '''' then
          raise EUsesListError.Create(SInvalidUses);
        Inc(P);

        while not (P^ in [#0, '''']) do
          Inc(P);
        if P^ <> '''' then
          raise EUsesListError.Create(SInvalidUses);
        Inc(P);
        SkipCommentsAndBlanks(P);
      end;

      case P^ of
        ',':
          Inc(P);
        ';':
          begin
            System.Insert(Format(', %s', [UnitName]), FText, P - PChar(FText) + 1);
            Result := IndexOf(UnitName);
            Break;
          end;
        else
          raise EUsesListError.Create(SInvalidUses);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------

function TUsesList.IndexOf(const UnitName: string): Integer;
var
  P, PIdentifier: PChar;
  Identifier: string;
  I: Integer;
begin
  Result := -1;

  if FText = '' then
    Exit;

  P := PChar(FText);
  if not CheckKeyword(P, SUses) then
    raise EUsesListError.Create(SInvalidUses);

  I := -1;
  while P^ <> #0 do
  begin
    SkipCommentsAndBlanks(P);
    PIdentifier := P;
    if not CheckIdentifier(P) then
      raise EUsesListError.Create(SInvalidUses);
    SetString(Identifier, PIdentifier, P - PIdentifier);

    Inc(I);
    if AnsiCompareText(UnitName, Identifier) = 0 then
    begin
      Result := I;
      Exit;
    end;
    SkipCommentsAndBlanks(P);

    if PeekKeyword(P, 'in') then
    begin
      Inc(P, 2);
      SkipCommentsAndBlanks(P);
      if P^ <> '''' then
        raise EUsesListError.Create(SInvalidUses);
      Inc(P);

      while not (P^ in [#0, '''']) do
        Inc(P);
      if P^ <> '''' then
        raise EUsesListError.Create(SInvalidUses);
      Inc(P);
      SkipCommentsAndBlanks(P);
    end;

    case P^ of
      ',':
        Inc(P);
      ';':
        Break;
      else
        raise EUsesListError.Create(SInvalidUses);
    end;
  end;
end;

//----------------------------------------------------------------------------

procedure TUsesList.Insert(Index: Integer; const UnitName: string);
var
  I: Integer;
  P: PChar;
begin
  if (Index < 0) or (Index > Count - 1) then
    raise EUsesListError.CreateFmt(SListIndexError, [Index]);
  I := IndexOf(UnitName);
  if I <> -1 then
    raise EUsesListError.CreateFmt(SDuplicateUnit, [UnitName]);

  if FText = '' then
  begin
    FText := Format('%s'#13#10'  %s;'#13#10#13#10, [SUses, UnitName]);
    try
      if Index <> IndexOf(UnitName) then
        Exit;
    except
      FText := '';
      raise;
    end;
  end
  else
  begin
    P := PChar(FText);
    if not CheckKeyword(P, SUses) then
      raise EUsesListError.Create(SInvalidUses);

    I := -1;
    while P^ <> #0 do
    begin
      SkipCommentsAndBlanks(P);
      Inc(I);
      if I = Index then
      begin
        System.Insert(Format('%s, ', [UnitName]), FText, P - PChar(FText) + 1);
        Exit;
      end;

      if not CheckIdentifier(P) then
        raise EUsesListError.Create(SInvalidUses);
      SkipCommentsAndBlanks(P);

      if PeekKeyword(P, 'in') then
      begin
        Inc(P, 2);
        SkipCommentsAndBlanks(P);
        if P^ <> '''' then
          raise EUsesListError.Create(SInvalidUses);
        Inc(P);

        while not (P^ in [#0, '''']) do
          Inc(P);
        if P^ <> '''' then
          raise EUsesListError.Create(SInvalidUses);
        Inc(P);
        SkipCommentsAndBlanks(P);
      end;

      case P^ of
        ',':
          Inc(P);
        else
          raise EUsesListError.Create(SInvalidUses);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------

procedure TUsesList.Remove(Index: Integer);
var
  Count, I, DelPos: Integer;
  P, PIdentifier: PChar;
begin
  Count := GetCount;
  if (Index < 0) or (Index > Count - 1) then
    raise EUsesListError.CreateFmt(SListIndexError, [Index]);

  P := PChar(FText);
  if not CheckKeyword(P, SUses) then
    raise EUsesListError.Create(SInvalidUses);

  if (Count = 1) and (Index = 0) then
  begin
    Delete(FText, 1, Length(SUses));
    P := PChar(FText);
  end;

  I := -1;
  while P^ <> #0 do
  begin
    SkipCommentsAndBlanks(P);
    Inc(I);

    if I = Index then
    begin
      // remove unit
      PIdentifier := P;
      if not CheckIdentifier(P) then
        raise EUsesListError.Create(SInvalidUses);
      DelPos := PIdentifier - PChar(FText) + 1;
      Delete(FText, DelPos, P - PIdentifier);
      // skip comments and blanks
      P := PChar(FText) + DelPos - 1;
      PIdentifier := P;
      SkipCommentsAndBlanks(P);
      // check <unitname> in <filename> syntax
      if PeekKeyword(P, 'in') then
      begin
        Inc(P, 2);
        SkipCommentsAndBlanks(P);
        if P^ <> '''' then
          raise EUsesListError.Create(SInvalidUses);
        Inc(P);

        while not (P^ in [#0, '''']) do
          Inc(P);
        if P^ <> '''' then
          raise EUsesListError.Create(SInvalidUses);
        Inc(P);
        SkipCommentsAndBlanks(P);
        DelPos := PIdentifier - PChar(FText) + 1;
        Delete(FText, DelPos, P - PIdentifier);
        P := PChar(FText) + DelPos - 1;
      end;

      // remove separator
      case P^ of
        ',', ';':
          begin
            DelPos := P - PChar(FText) + 1;
            Delete(FText, DelPos, 1);
          end;
        else
          raise EUsesListError.Create(SInvalidUses);
      end;
      // remove trailing spaces, if any
      PIdentifier := PChar(FText) + DelPos - 1;
      P := PIdentifier;
      SkipChars(P, Blanks);
      DelPos := PIdentifier - PChar(FText) + 1;
      Delete(FText, DelPos, P - PIdentifier);
      // skip further comments and blanks
      P := PChar(FText) + DelPos - 1;
      SkipCommentsAndBlanks(P);
      Exit;
    end;
    if not CheckIdentifier(P) then
      raise EUsesListError.Create(SInvalidUses);

    SkipCommentsAndBlanks(P);
    if PeekKeyword(P, 'in') then
    begin
      Inc(P, 2);
      SkipCommentsAndBlanks(P);
      if P^ <> '''' then
        raise EUsesListError.Create(SInvalidUses);
      Inc(P);

      while not (P^ in [#0, '''']) do
        Inc(P);
      if P^ <> '''' then
        raise EUsesListError.Create(SInvalidUses);
      Inc(P);
      SkipCommentsAndBlanks(P);
    end;

    case P^ of
      ',', ';':
        begin
          // make sure semicolon is the last separator in case the last unit is going to be removed
          if (Index = Count - 1) and (I = Index - 1) then
            P^ := ';';
          Inc(P);
        end;
      else
        raise EUsesListError.Create(SInvalidUses);
    end;
  end;
end;

//----------------------------------------------------------------------------
{ TProgramGoal public }
//----------------------------------------------------------------------------

constructor TProgramGoal.Create(Text: PChar);
var
  P, PStart: PChar;
begin
  FTextBeforeUses := '';
  FTextAfterUses := '';

  PStart := Text;
  P := PStart;
  
  // check 'program' label
  SkipCommentsAndBlanks(P);
  if not CheckKeyword(P, SProgram) then
    raise EUsesListError.Create(SInvalidProgram);
  SkipCommentsAndBlanks(P);
  if not CheckIdentifier(P) then
    raise EUsesListError.Create(SInvalidProgram);
  SkipCommentsAndBlanks(P);
  if P^ <> ';' then
    raise EUsesListError.Create(SInvalidProgram);
  Inc(P);
  SkipCommentsAndBlanks(P);

  // remember text before uses
  SetString(FTextBeforeUses, PStart, P - PStart);

  if PeekKeyword(P, SUses) then
  begin
    FUsesList := TUsesList.Create(P);
    PStart := P + Length(FUsesList.Text);
  end
  else // empty uses list
  begin
    FUsesList := TUsesList.Create(nil);
    PStart := P;
  end;
  // remember text after uses
  P := StrEnd(PStart);
  SetString(FTextAfterUses, PStart, P - PStart);
end;

//----------------------------------------------------------------------------

destructor TProgramGoal.Destroy;
begin
  FUsesList.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------
{ TLibraryGoal public }
//----------------------------------------------------------------------------

constructor TLibraryGoal.Create(Text: PChar);
var
  P, PStart: PChar;
begin
  FTextBeforeUses := '';
  FTextAfterUses := '';

  PStart := Text;
  P := PStart;

  // check 'library' label
  SkipCommentsAndBlanks(P);
  if not CheckKeyword(P, SLibrary) then
    raise EUsesListError.Create(SInvalidLibrary);
  SkipCommentsAndBlanks(P);
  if not CheckIdentifier(P) then
    raise EUsesListError.Create(SInvalidLibrary);
  SkipCommentsAndBlanks(P);
  if P^ <> ';' then
    raise EUsesListError.Create(SInvalidLibrary);
  Inc(P);
  SkipCommentsAndBlanks(P);

  // remember text before uses
  SetString(FTextBeforeUses, PStart, P - PStart);

  if PeekKeyword(P, SUses) then
  begin
    FUsesList := TUsesList.Create(P);
    PStart := P + Length(FUsesList.Text);
  end
  else // empty uses list
  begin
    FUsesList := TUsesList.Create(nil);
    PStart := P;
  end;
  // remember text after uses
  P := StrEnd(PStart);
  SetString(FTextAfterUses, PStart, P - PStart);
end;

//----------------------------------------------------------------------------

destructor TLibraryGoal.Destroy;
begin
  FUsesList.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------
{ TUnitGoal public }
//----------------------------------------------------------------------------

constructor TUnitGoal.Create(Text: PChar);
var
  P, PStart: PChar;
begin
  FTextBeforeIntf := '';
  FTextAfterIntf := '';
  FTextAfterImpl := '';

  PStart := Text;
  P := PStart;

  // check 'unit' label
  SkipCommentsAndBlanks(P);
  while (P^ <> #0) and not PeekKeyword(P, 'unit') do
  begin
    SkipChars(P, [#1..#255] - Blanks);
    SkipCommentsAndBlanks(P);
  end;
  if not CheckKeyword(P, SUnit) then
    raise EUsesListError.Create(SInvalidUnit);
  SkipCommentsAndBlanks(P);
  if not CheckIdentifier(P) then
    raise EUsesListError.Create(SInvalidUnit);
  SkipCommentsAndBlanks(P);
  if P^ <> ';' then
    raise EUsesListError.Create(SInvalidUnit);
  Inc(P);
  // check 'interface' label
//  SkipCommentsAndBlanks(P);
  while (P^ <> #0) and not PeekKeyword(P, 'interface') do
  begin
    SkipChars(P, [#1..#255] - Blanks);
    SkipCommentsAndBlanks(P);
  end;
  if not CheckKeyword(P, 'interface') then
    raise EUsesListError.Create(SInvalidUnit);
  SkipCommentsAndBlanks(P);

  // remember text before interface uses
  SetString(FTextBeforeIntf, PStart, P - PStart);
  if PeekKeyword(P, SUses) then
  begin
    FUsesIntf := TUsesList.Create(P);
    PStart := P + Length(FUsesIntf.Text);
  end
  else
  begin
    FUsesIntf := TUsesList.Create(nil);
    PStart := P;
  end;
  // locate implementation
  while (P^ <> #0) and not PeekKeyword(P, 'implementation') do
  begin
    SkipChars(P, [#1..#255] - Blanks);
    SkipCommentsAndBlanks(P);
  end;
  if not CheckKeyword(P, 'implementation') then
    raise EUsesListError.Create(SInvalidUnit);
  SkipCommentsAndBlanks(P);

  // remember text after interface uses
  SetString(FTextAfterIntf, PStart, P - PStart);
  if PeekKeyword(P, SUses) then
  begin
    FUsesImpl := TUsesList.Create(P);
    PStart := P + Length(FUsesImpl.Text);
  end
  else
  begin
    FUsesImpl := TUsesList.Create(nil);
    PStart := P;
  end;
  // remember text after implementation uses
  P := StrEnd(PStart);
  SetString(FTextAfterImpl, PStart, P - PStart);
end;

//----------------------------------------------------------------------------

destructor TUnitGoal.Destroy;
begin
  FUsesIntf.Free;
  FUsesImpl.Free;
  inherited Destroy;
end;

end.
