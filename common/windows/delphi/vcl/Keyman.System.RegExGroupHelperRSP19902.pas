unit Keyman.System.RegExGroupHelperRSP19902;

interface

//
// Ref: https://quality.embarcadero.com/browse/RSP-19902
//
// TRegEx Groups return the wrong value when using supplementary plane (SMP)
// characters in an input string. The position is off by 1 for each SMP
// character prior to the match in the input string, it seems because the
// regex engine is (correctly) matching UTF-16 surrogate pairs as a single
// character, whereas the grouping code is treating them as two separate
// characters.
//
// This workaround resolves the problem. However if the problem is fixed in
// the library, then this workaround will fail. See the corresponding unit test
// to make sure that we pick up when this happens.
//
// Fixed in VER330 (Delphi 10.3 Rio, 20.0)

uses
  System.Character,
  System.Types,
  System.SysUtils,
  System.RegularExpressions;

type
  TGroupHelperRSP19902 = record
  private
{$IFDEF VER320}
    FInput: string;
    FSourceGroup: TGroup;
{$ENDIF}
    FFixedIndex, FFixedLength: Integer;
    FFixedValue: string;
  public
    constructor Create(SourceGroup: TGroup; const Input: string);
    property FixedIndex: Integer read FFixedIndex;
    property FixedLength: Integer read FFixedLength;
    property FixedValue: string read FFixedValue;
  end;

implementation

{ TGroupHelperRSP19902 }

constructor TGroupHelperRSP19902.Create(SourceGroup: TGroup; const Input: string);
{$IFDEF VER320}
var
  i: Integer;
begin
  FSourceGroup := SourceGroup;
  FInput := Input;

  FFixedIndex := 1; //Self.Index;
  for i := 1 to SourceGroup.Index - 1 do
  begin
    if input[FFixedIndex].IsHighSurrogate and (FFixedIndex < input.Length) and input[FFixedIndex+1].IsLowSurrogate then
      Inc(FFixedIndex);
    Inc(FFixedIndex);
  end;

  FFixedLength := 0;
  for i := 1 to SourceGroup.Length do
  begin
    if input[FFixedLength+FFixedIndex].IsHighSurrogate and (FFixedLength+FFixedIndex < input.Length) and input[FFixedLength+FFixedIndex+1].IsLowSurrogate then
      Inc(FFixedLength);
    Inc(FFixedLength);
  end;

  FFixedValue := Copy(input, FFixedIndex, FFixedLength);
{$ELSE}
begin
  // This was fixed in VER320 so
  FFixedIndex := SourceGroup.Index;
  FFixedLength := SourceGroup.Length;
  FFixedValue := SourceGroup.Value;
{$ENDIF}
end;

end.
