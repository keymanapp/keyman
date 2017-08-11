(*
  Name:             SyntaxHighlight
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Jan 2007

  Modified Date:    8 Oct 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:
  Notes:
  History:          04 Jan 2007 - mcdurdin - Add notany keyword
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    19 Jul 2011 - mcdurdin - I2899 - New keyboard options keywords not bold
                    18 May 2012 - mcdurdin - I3323 - V9.0 - Change from Plus-MemoU to Plus-Memo
                    17 Aug 2012 - mcdurdin - I3430 - Add support for platform() and baselayout()
                    08 Oct 2012 - mcdurdin - I3464 - Add syntax highlighting for layer keyword
*)
unit SyntaxHighlight;  // I3323

interface

uses
  Windows, Messages, SysUtils, Graphics, Forms, Controls,
  KeymanDeveloperMemo, PlusMemo, PMSupport, ExtHilit;

type
  TSyntaxType = (stPlainText, stHeader, stQuotedText, stStatement, stSystemRule, stCharacter,
                 stSystemStore, stComment, stVirtualKey, stOther);

  TSyntaxTypeInfo = record
    Name: string;
    Style: TFontStyles;
    BGColor, FGColor: TColor;
    PlainBG, PlainFG: Boolean;
  end;

  PSyntaxTypeInfo = ^TSyntaxTypeInfo;

  TSyntaxHighlighter = class
  private
    FUseSyntaxHighlighting: Boolean;
    FSyntaxTypeInfo: array[TSyntaxType] of TSyntaxTypeInfo;
    FOwner: TForm;
    function GetSyntaxTypeInfo(Index: TSyntaxType): PSyntaxTypeInfo;
    procedure SetSyntaxTypeInfo(Index: TSyntaxType; const Value: PSyntaxTypeInfo);
    procedure SyntaxTypeInfoFromString(i: TSyntaxType; str: string);
    function SyntaxTypeInfoToString(i: TSyntaxType): string;
  public
    constructor Create(AOwner: TForm);
    destructor Destroy; override;
    procedure Apply(Memo: TKeymanDeveloperMemo; Ext: TExtHighlighter);
    procedure Load;
    procedure Save;
    procedure ResetColour(n: TSyntaxType);
    procedure ResetAll;
    property SyntaxTypeInfo[Index: TSyntaxType]: PSyntaxTypeInfo read GetSyntaxTypeInfo write SetSyntaxTypeInfo;
    property UseSyntaxHighlighting: Boolean read FUseSyntaxHighlighting write FUseSyntaxHighlighting;
  end;

implementation

uses ErrorControlledRegistry, RegistryKeys;

type
  TSyntaxKeyword = record
    Name: string;
    Options: TWordOptions;
    Scope: Integer;
    tp: TSyntaxType;
  end;

  TSyntaxStartStopKey = record
    Start: string;
    Stop: string;
    Context, Scope: Integer;
    Options: TWordOptions;
    tp: TSyntaxType;
    AltFont: Boolean;
  end;

const
  DefaultSyntaxTypeInfo: array[TSyntaxType] of TSyntaxTypeInfo = (
    (Name: 'Plain text';   Style: [];                 BGColor: clWhite; FGColor: clBlack; PlainBG: True; PlainFG: True),
    (Name: 'Header';       Style: [];                 BGColor: clWhite; FGColor: clNavy;  PlainBG: True; PlainFG: False),
    (Name: 'Quoted text';  Style: [];                 BGColor: clWhite; FGColor: clBlue;  PlainBG: True; PlainFG: False),
    (Name: 'Statement';    Style: [fsBold];           BGColor: clWhite; FGColor: clBlack; PlainBG: True; PlainFG: True),
    (Name: 'System rule';  Style: [fsBold];           BGColor: clWhite; FGColor: clBlack; PlainBG: True; PlainFG: True),
    (Name: 'Character';    Style: [];                 BGColor: clWhite; FGColor: clBlue;  PlainBG: True; PlainFG: False),
    (Name: 'System store'; Style: [fsBold, fsItalic]; BGColor: clWhite; FGColor: clBlack; PlainBG: True; PlainFG: True),
    (Name: 'Comment';      Style: [fsItalic];         BGColor: clWhite; FGColor: clGreen; PlainBG: True; PlainFG: False),
    (Name: 'Virtual Key';  Style: [];                 BGColor: clWhite; FGColor: clMaroon;PlainBG: True; PlainFG: False),
    (Name: ''));

const
  SyntaxKeywords: array[0..71] of TSyntaxKeyword = (
    (Name: 'begin';            tp: stStatement),
    (Name: 'ANSI';             tp: stStatement),
    (Name: 'Unicode';          tp: stStatement),
    (Name: 'use';              tp: stStatement),
    (Name: 'ANSI';             tp: stStatement),
    (Name: 'group';            tp: stStatement),
    (Name: 'store';            tp: stStatement),
    (Name: 'index';            tp: stStatement),
    (Name: 'any';              tp: stStatement),
    (Name: 'using keys';       tp: stStatement),
    (Name: 'context';          tp: stStatement),
    (Name: 'outs';             tp: stStatement),
    (Name: 'beep';             tp: stStatement),
    (Name: 'nul';              tp: stStatement),
    (Name: 'return';           tp: stStatement),
    (Name: 'clearcontext';     tp: stStatement),
    (Name: 'dk';               tp: stStatement),
    (Name: 'deadkey';          tp: stStatement),
    (Name: 'notany';           tp: stStatement),  // I2899
    (Name: 'set';              tp: stStatement),  // I2899
    (Name: 'reset';            tp: stStatement),  // I2899
    (Name: 'if';               tp: stStatement),  // I2899
    (Name: 'save';             tp: stStatement),  // I2899 //23

    (Name: 'platform';         tp: stStatement),  // I3430
    (Name: 'baselayout';       tp: stStatement),  // I3430 //25

    (Name: 'layer';            tp: stStatement),  // I3464

    (Name: 'match';            tp: stSystemRule),
    (Name: 'nomatch';          tp: stSystemRule), //2

    // TODO: name all the system stores; there aren't all that many
    (Name: '&@';               Options: [woWholeWordsOnly, woRegExp]; Scope: 6; tp: stSystemStore), //1
    (Name: '&@@';              Options: [woWholeWordsOnly, woRegExp]; Scope: 6; tp: stSystemStore), //2
    (Name: '&@@@';             Options: [woWholeWordsOnly, woRegExp]; Scope: 6; tp: stSystemStore), //3
    (Name: '&@@@@';            Options: [woWholeWordsOnly, woRegExp]; Scope: 6; tp: stSystemStore), //4
    (Name: '&@@@@@';           Options: [woWholeWordsOnly, woRegExp]; Scope: 6; tp: stSystemStore), //5
    (Name: '&@@@@@@';          Options: [woWholeWordsOnly, woRegExp]; Scope: 6; tp: stSystemStore), //6
    (Name: '&@@@@@@@';         Options: [woWholeWordsOnly, woRegExp]; Scope: 6; tp: stSystemStore), //7
    (Name: '&@@@@@@@@';        Options: [woWholeWordsOnly, woRegExp]; Scope: 6; tp: stSystemStore), //8
    (Name: '&@@@@@@@@@';       Options: [woWholeWordsOnly, woRegExp]; Scope: 6; tp: stSystemStore), //9
    (Name: '&@@@@@@@@@@';      Options: [woWholeWordsOnly, woRegExp]; Scope: 6; tp: stSystemStore), //10
    (Name: '&@@@@@@@@@@@';     Options: [woWholeWordsOnly, woRegExp]; Scope: 6; tp: stSystemStore), //11
    (Name: '&@@@@@@@@@@@@';    Options: [woWholeWordsOnly, woRegExp]; Scope: 6; tp: stSystemStore), //12
    (Name: '&@@@@@@@@@@@@@';   Options: [woWholeWordsOnly, woRegExp]; Scope: 6; tp: stSystemStore), //13
    (Name: '&@@@@@@@@@@@@@@';  Options: [woWholeWordsOnly, woRegExp]; Scope: 6; tp: stSystemStore), //14
    (Name: '&@@@@@@@@@@@@@@@'; Options: [woWholeWordsOnly, woRegExp]; Scope: 6; tp: stSystemStore), //15

    (Name: 'd#';               Options: [woWholeWordsOnly, woRegExp]; tp: stCharacter),
    (Name: 'd##';              Options: [woWholeWordsOnly, woRegExp]; tp: stCharacter),
    (Name: 'd###';             Options: [woWholeWordsOnly, woRegExp]; tp: stCharacter),
    (Name: 'd####';            Options: [woWholeWordsOnly, woRegExp]; tp: stCharacter),
    (Name: 'd#####';           Options: [woWholeWordsOnly, woRegExp]; tp: stCharacter),
    (Name: 'd######';          Options: [woWholeWordsOnly, woRegExp]; tp: stCharacter),
    (Name: 'd#######';         Options: [woWholeWordsOnly, woRegExp]; tp: stCharacter),
    (Name: 'd########';        Options: [woWholeWordsOnly, woRegExp]; tp: stCharacter),  //8
    (Name: 'U+$$$$';           Options: [woWholeWordsOnly, woRegExp]; tp: stCharacter),
    (Name: 'U+$$$$$';          Options: [woWholeWordsOnly, woRegExp]; tp: stCharacter),
    (Name: 'U+$$$$$$';         Options: [woWholeWordsOnly, woRegExp]; tp: stCharacter),  //3
    (Name: 'x$';               Options: [woWholeWordsOnly, woRegExp]; tp: stCharacter),
    (Name: 'x$$';              Options: [woWholeWordsOnly, woRegExp]; tp: stCharacter),
    (Name: 'x$$$';             Options: [woWholeWordsOnly, woRegExp]; tp: stCharacter),
    (Name: 'x$$$$';            Options: [woWholeWordsOnly, woRegExp]; tp: stCharacter),
    (Name: 'x$$$$$';           Options: [woWholeWordsOnly, woRegExp]; tp: stCharacter),
    (Name: 'x$$$$$$';          Options: [woWholeWordsOnly, woRegExp]; tp: stCharacter),  //6

    (Name: 'VERSION';          tp: stHeader),
    (Name: 'COPYRIGHT';        tp: stHeader),
    (Name: 'MESSAGE';          tp: stHeader),
    (Name: 'BITMAP';           tp: stHeader),
    (Name: 'BITMAPS';          tp: stHeader),
    (Name: 'LANGUAGE';         tp: stHeader),
    (Name: 'NAME';             tp: stHeader),
    (Name: 'HOTKEY';           tp: stHeader),
    (Name: 'LAYOUT';           tp: stHeader),
    (Name: 'CAPS ON ONLY';     tp: stHeader),
    (Name: 'CAPS ALWAYS OFF';  tp: stHeader),
    (Name: 'SHIFT FREES CAPS'; tp: stHeader) ); //12

const
  SyntaxStartStopKeys: array[0..6] of TSyntaxStartStopKey = (
    (Start: 'C';  Stop: '';   Context: 2; Options: [woWholeWordsOnly]; tp: stComment; AltFont: True),
    (Start: ''''; Stop: ''''; Context: 3;                              tp: stQuotedText; AltFont: True),
    (Start: '"';  Stop: '"';  Context: 4;                              tp: stQuotedText; AltFont: True),
    (Start: '[';  Stop: ']';  Context: 5;                              tp: stVirtualKey),
    (Start: '(';  Stop: ')';  Context: 6;                              tp: stPlainText),
    (Start: ''''; Stop: ''''; Context: 3; Scope: 5;                    tp: stQuotedText; AltFont: True),
    (Start: '"';  Stop: '"';  Context: 4; Scope: 5;                    tp: stQuotedText; AltFont: True));

{-------------------------------------------------------------------------------
 - Manage highlighting in editor                                               -
 -------------------------------------------------------------------------------}

constructor TSyntaxHighlighter.Create(AOwner: TForm);
begin
  inherited Create;
  FOwner := AOwner;
  Load;
end;

procedure TSyntaxHighlighter.Load;
var
  i: TSyntaxType;
begin
  ResetAll;

  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKeyReadOnly(SRegKey_IDEColours) then
    begin
      for i := Low(TSyntaxType) to High(TSyntaxType) do
      begin
        if ValueExists(FSyntaxTypeInfo[i].Name) then
          SyntaxTypeInfoFromString(i, ReadString(FSyntaxTypeInfo[i].Name));
      end;
      CloseKey;
    end;
    if OpenKeyReadOnly(SRegKey_IDEOptions) then
    begin
      if ValueExists(SRegValue_IDEOptUseSyntaxHighlighting)
        then FUseSyntaxHighlighting := ReadString(SRegValue_IDEOptUseSyntaxHighlighting) = '1'
        else FUseSyntaxHighlighting := True;
    end;
  finally
    Free;
  end;
end;

procedure TSyntaxHighlighter.Save;
var
  i: TSyntaxType;
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey(SRegKey_IDEColours, True) then
    begin
      for i := Low(TSyntaxType) to High(TSyntaxType) do
        WriteString(FSyntaxTypeInfo[i].Name, SyntaxTypeInfoToString(i));
      CloseKey;
    end;
    if OpenKey(SRegKey_IDEOptions, True) then
    begin
      if FUseSyntaxHighlighting
        then WriteString(SRegValue_IDEOptUseSyntaxHighlighting, '1')
        else WriteString(SRegValue_IDEOptUseSyntaxHighlighting, '0');
    end;
  finally
    Free;
  end;
end;

procedure TSyntaxHighlighter.SyntaxTypeInfoFromString(i: TSyntaxType; str: string);
var
  s: string;
  FBG: Boolean;
  n: Integer;
begin
  FSyntaxTypeInfo[i].Style := [];
  FSyntaxTypeInfo[i].PlainFG := False;
  FSyntaxTypeInfo[i].PlainBG := False;

  FBG := True;
  str := Trim(str);
  while str <> '' do
  begin
    n := Pos(' ', str);
    if n = 0 then begin s := str; str := ''; end
    else begin s := Copy(str, 1, n-1); Delete(str, 1, n); end;
    str := Trim(str); s := Trim(s);

    if LowerCase(s) = 'bold'      then Include(FSyntaxTypeInfo[i].Style, fsBold)
    else if LowerCase(s) = 'italic'    then Include(FSyntaxTypeInfo[i].Style, fsItalic)
    else if LowerCase(s) = 'underline' then Include(FSyntaxTypeInfo[i].Style, fsUnderline)
    else if LowerCase(s) = 'plainfg' then FSyntaxTypeInfo[i].PlainFG := True
    else if LowerCase(s) = 'plainbg' then FSyntaxTypeInfo[i].PlainBG := True
    else if FBG then begin FSyntaxTypeInfo[i].BGColor := StringToColor(s); FBG := False; end
    else FSyntaxTypeInfo[i].FGColor := StringToColor(s);
  end;
end;

function TSyntaxHighlighter.SyntaxTypeInfoToString(i: TSyntaxType): string;
begin
  Result := '';
  if fsBold in FSyntaxTypeInfo[i].Style      then Result := Result + 'Bold ';
  if fsItalic in FSyntaxTypeInfo[i].Style    then Result := Result + 'Italic ';
  if fsUnderline in FSyntaxTypeInfo[i].Style then Result := Result + 'Underline ';
  if FSyntaxTypeInfo[i].PlainFG then Result := Result + 'PlainFG ';
  if FSyntaxTypeInfo[i].PlainBG then Result := Result + 'PlainBG ';
  Result := Result + ColorToString(FSyntaxTypeInfo[i].BGColor) + ' ' + ColorToString(FSyntaxTypeInfo[i].FGColor);
end;

destructor TSyntaxHighlighter.Destroy;
begin
  inherited Destroy;
end;

function TSyntaxHighlighter.GetSyntaxTypeInfo(Index: TSyntaxType): PSyntaxTypeInfo;
begin
  Result := @FSyntaxTypeInfo[Index];
end;

procedure TSyntaxHighlighter.SetSyntaxTypeInfo(Index: TSyntaxType; const Value: PSyntaxTypeInfo);
begin
  FSyntaxTypeInfo[Index] := Value^;
end;

procedure TSyntaxHighlighter.Apply(Memo: TKeymanDeveloperMemo; Ext: TExtHighlighter);
var
  i: Integer;
  sti: TSyntaxTypeInfo;
  FGColor, BGColor: TColor;
begin
  Ext.Keywords.Clear;
  Ext.StartStopKeys.Clear;
  Ext.Separators := ' (),';

  if not FUseSyntaxHighlighting then
  begin
    Memo.Font.Style := [];
    Memo.Font.Color := clWindowText;
    Memo.Color := clWindow;
    Exit;
  end;

  Memo.Font.Style := FSyntaxTypeInfo[stPlainText].Style;
  Memo.Font.Color := FSyntaxTypeInfo[stPlainText].FGColor;
  Memo.Color := FSyntaxTypeInfo[stPlainText].BGColor;

  for i := 0 to High(SyntaxKeywords) do
  begin
    sti := FSyntaxTypeInfo[SyntaxKeywords[i].tp];
    if sti.PlainBG then BGColor := FSyntaxTypeInfo[stPlainText].BGColor else BGColor := sti.BGColor;
    if sti.PlainFG then FGColor := FSyntaxTypeInfo[stPlainText].FGColor else FGColor := sti.FGColor;
    Ext.Keywords.AddExtKeyWord(SyntaxKeywords[i].Name, [woWholeWordsOnly] + SyntaxKeywords[i].Options, sti.Style,
      1, SyntaxKeywords[i].Scope, 0, crDefault, BGColor, FGColor);
  end;

  for i := 0 to High(SyntaxStartStopKeys) do
  begin
    sti := FSyntaxTypeInfo[SyntaxStartStopKeys[i].tp];
    if sti.PlainBG then BGColor := FSyntaxTypeInfo[stPlainText].BGColor else BGColor := sti.BGColor;
    if sti.PlainFG then FGColor := FSyntaxTypeInfo[stPlainText].FGColor else FGColor := sti.FGColor;
    if SyntaxStartStopKeys[i].AltFont
      then Include(TPlusFontStyles(sti.Style), TPlusFontStyle(fsAltFont))
      else Exclude(TPlusFontStyles(sti.Style), TPlusFontStyle(fsAltFont));
    Ext.StartStopKeys.AddExStartStopKey(SyntaxStartStopKeys[i].Start, SyntaxStartStopKeys[i].Stop,
      SyntaxStartStopKeys[i].Options, sti.Style, SyntaxStartStopKeys[i].Context, SyntaxStartStopKeys[i].Scope, 0, crDefault, BGColor, FGColor, [ssoParStop]);
  end;
end;

procedure TSyntaxHighlighter.ResetAll;
var
  i: TSyntaxType;
begin
  for i := Low(TSyntaxType) to High(TSyntaxType) do ResetColour(i);
end;

procedure TSyntaxHighlighter.ResetColour(n: TSyntaxType);
begin
  FSyntaxTypeInfo[n] := DefaultSyntaxTypeInfo[n];
end;

end.

