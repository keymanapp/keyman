(*
 * Keyman is copyright (C) SIL Global. MIT License.
 *)
unit utilcheckfontchars;

interface

uses findfonts, classes, sysutils, widestrings, contnrs;

type
  TCheckFontResult = class
  private
    FChars: WideString;
    FFonts: TFindFontList;
  public
    constructor Create(const AChars: WideString);
    Destructor Destroy; override;
    property Chars: WideString read FChars;
    property Fonts: TFindFontList read FFonts;
  end;

  TCheckFontResults = class(TObjectList)
  private
    function GetItem(Index: Integer): TCheckFontResult;
    procedure SetItem(Index: Integer; const Value: TCheckFontResult);
  public
    property Items[Index: Integer]: TCheckFontResult read GetItem write SetItem; default;
  end;

  TCheckFontsThread = class(TThread)
  private
    FResults: TCheckFontResults;
  protected
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure AddChars(const Chars: WideString);
    property Results: TCheckFontResults read FResults;
  end;


implementation

{ TCheckFontsThread }

procedure TCheckFontsThread.AddChars(const Chars: WideString);
begin
  FResults.Add(TCheckFontResult.Create(Chars));
end;

constructor TCheckFontsThread.Create;
begin
  inherited Create(True);
  FResults := TCheckFontResults.Create;
end;

destructor TCheckFontsThread.Destroy;
begin
  FreeAndNil(FResults);
  inherited Destroy;
end;

procedure TCheckFontsThread.Execute;
var
  i: Integer;
begin
  for i := 0 to FResults.Count - 1 do
  begin
    if Terminated then Break;
    FindFontsForChars(FResults[i].Chars, 50, FResults[i].Fonts);
  end;
end;

{ TCheckFontKeyboards }

function TCheckFontResults.GetItem(Index: Integer): TCheckFontResult;
begin
  Result := inherited GetItem(Index) as TCheckFontResult;
end;

procedure TCheckFontResults.SetItem(Index: Integer;
  const Value: TCheckFontResult);
begin
  inherited SetItem(Index, Value);
end;


{ TCheckFontKeyboard }

constructor TCheckFontResult.Create(const AChars: WideString);
begin
  inherited Create;
  FFonts := TFindFontList.Create;
  FChars := AChars;
end;

destructor TCheckFontResult.Destroy;
begin
  FreeAndNil(FFonts);
  inherited Destroy;
end;

end.
