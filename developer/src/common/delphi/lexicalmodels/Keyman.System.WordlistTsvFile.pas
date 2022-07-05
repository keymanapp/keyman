unit Keyman.System.WordlistTsvFile;

interface

uses
  System.Classes,
  System.Generics.Collections;

type
  TWordlistWord = record
    Word: string;
    Frequency: Integer;
    Comment: string;
    function ToString: string;
    procedure Parse(s: string);
    function IsComment: Boolean; inline;
  end;

  TWordlistTsvFile = class
  private
    FWords: TStringList;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetWord(Index: Integer): TWordlistWord;
    procedure SetWord(Index: Integer; const Value: TWordlistWord);
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadFromFile(Filename: string): Boolean;
    procedure SaveToFile(const Filename: string);
    function AddWord(Value: TWordlistWord): Integer;
    procedure RemoveWord(Index: Integer);
    procedure SortByFrequency;
    property Word[Index: Integer]: TWordlistWord read GetWord write SetWord;
    property Text: string read GetText write SetText;
    property Count: Integer read GetCount;
  end;

implementation

uses
  System.SysUtils,

  utilstr;

{ TWordlistTsvFile }

function TWordlistTsvFile.AddWord(Value: TWordlistWord): Integer;
begin
  Result := FWords.Add(Value.ToString);
end;

constructor TWordlistTsvFile.Create;
begin
  inherited Create;
  FWords := TStringList.Create;
  FWords.DefaultEncoding := TEncoding.UTF8;
end;

destructor TWordlistTsvFile.Destroy;
begin
  FWords.Free;
  inherited Destroy;
end;

function TWordlistTsvFile.GetCount: Integer;
begin
  Result := FWords.Count;
end;

function TWordlistTsvFile.GetText: string;
begin
  Result := FWords.Text;
end;

function TWordlistTsvFile.GetWord(Index: Integer): TWordlistWord;
begin
  Result.Parse(FWords[Index]); // Will throw if out of range
end;

function TWordlistTsvFile.LoadFromFile(Filename: string): Boolean;
begin
  FWords.LoadFromFile(Filename);
  Result := True;
end;

procedure TWordlistTsvFile.RemoveWord(Index: Integer);
begin
  FWords.Delete(Index);
end;

procedure TWordlistTsvFile.SaveToFile(const Filename: string);
begin
  FWords.SaveToFile(Filename);
end;

procedure TWordlistTsvFile.SetText(const Value: string);
begin
  FWords.Text := Value;
end;

procedure TWordlistTsvFile.SetWord(Index: Integer; const Value: TWordlistWord);
begin
  FWords[Index] := Value.ToString; // Will throw if out of range
end;

function FrequencySort(List: TStringList; Index1, Index2: Integer): Integer;
var
  w1, w2: TWordlistWord;
begin
  w1.Parse(List[Index1]);
  w2.Parse(List[Index2]);
  if w1.IsComment and w2.IsComment then
    Result := Index1 - Index2
  else if w1.IsComment then
    Result := -1
  else if w2.IsComment then
    Result := 1
  else
    Result := w2.Frequency - w1.Frequency;
end;

procedure TWordlistTsvFile.SortByFrequency;
begin
  // Puts all comments at top, same order
  // Sorts remaining words by frequency
  FWords.CustomSort(FrequencySort);
end;

{ TWordlistWord }

function TWordlistWord.IsComment: Boolean;
begin
  Result := Word = '#';
end;

procedure TWordlistWord.Parse(s: string);
begin
  // Parse
  s := s.Trim;
  if s.StartsWith('#') then
  begin
    Word := '#';
    Frequency := 0;
    Comment := s.Substring(2);
  end
  else
  begin
    Word := StrToken(s, #9);
    Frequency := StrToIntDef(StrToken(s, #9).Replace(',','', [rfReplaceAll]), 0);
    Comment := s;
  end;
end;

function TWordlistWord.ToString: string;
var
  s: string;
begin
  if (Word = '#') or (Word = '') then
  begin
    s := '# ';
  end
  else
  begin
    s := Word + #9;
    if Frequency = 0
      then s := s + #9
      else s := s + Frequency.ToString + #9;
  end;

  Result := (s + Comment).Trim;
end;

end.
