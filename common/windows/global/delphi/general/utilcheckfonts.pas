(*
  Name:             utilcheckfonts
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      27 Mar 2008

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          27 Mar 2008 - mcdurdin - I1374 - Initial version
                    08 Jun 2012 - mcdurdin - I3311 - V9.0 - Change 'published' to 'public' on classes that don't need RTTI
*)
unit utilcheckfonts;

interface

uses findfonts, classes, sysutils, widestrings, contnrs;

type
  TCheckFontKeyboard = class
  private
    FName: WideString;
    FChars: WideString;
    FFonts: TFindFontList;
    FFileName: WideString;
  public
    constructor Create(const AName, AFileName, AChars: WideString);
    Destructor Destroy; override;
    property Name: WideString read FName;
    property FileName: WideString read FFileName;
    property Chars: WideString read FChars;
    property Fonts: TFindFontList read FFonts;
  end;

  TCheckFontKeyboards = class(TObjectList)  // I3311
  private
    function GetItem(Index: Integer): TCheckFontKeyboard;
    function GetKeyboard(const Name: WideString): TCheckFontKeyboard;
    procedure SetItem(Index: Integer; const Value: TCheckFontKeyboard);
  public
    property Items[Index: Integer]: TCheckFontKeyboard read GetItem write SetItem; default;
    property Keyboards[const Name: WideString]: TCheckFontKeyboard read GetKeyboard;
  end;

  TCheckFontsThread = class(TThread)
  private
    FKeyboards: TCheckFontKeyboards;
  protected
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure AddKeyboard(const KeyboardName, KeyboardFileName, Chars: WideString);
    property Keyboards: TCheckFontKeyboards read FKeyboards;
  end;


implementation

{ TCheckFontsThread }

procedure TCheckFontsThread.AddKeyboard(const KeyboardName, KeyboardFileName, Chars: WideString);
begin
  FKeyboards.Add(TCheckFontKeyboard.Create(KeyboardName, KeyboardFileName, Chars));
end;

constructor TCheckFontsThread.Create;
begin
  inherited Create(True);
  FKeyboards := TCheckFontKeyboards.Create;
end;

destructor TCheckFontsThread.Destroy;
begin
  FreeAndNil(FKeyboards);
  inherited Destroy;
end;

procedure TCheckFontsThread.Execute;
var
  i: Integer;
begin
  for i := 0 to FKeyboards.Count - 1 do
  begin
    if Terminated then Break;
    ////if SameText(FKeyboards[i].Name, 'galaxiegreekkm6') then
    FindFontsForChars(FKeyboards[i].Chars, 50, FKeyboards[i].Fonts);
  end;
end;

{ TCheckFontKeyboards }

function TCheckFontKeyboards.GetItem(Index: Integer): TCheckFontKeyboard;
begin
  Result := inherited GetItem(Index) as TCheckFontKeyboard;
end;

function TCheckFontKeyboards.GetKeyboard(
  const Name: WideString): TCheckFontKeyboard;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if WideSameText(Items[i].Name, Name) then
    begin
      Result := Items[i];
      Exit;
    end;
  Result := nil;
end;

procedure TCheckFontKeyboards.SetItem(Index: Integer;
  const Value: TCheckFontKeyboard);
begin
  inherited SetItem(Index, Value);
end;                                     


{ TCheckFontKeyboard }

constructor TCheckFontKeyboard.Create(const AName, AFileName, AChars: WideString);
begin
  inherited Create;
  FFonts := TFindFontList.Create;
  FName := AName;
  FFileName := AFileName;
  FChars := AChars;
end;

destructor TCheckFontKeyboard.Destroy;
begin
  FreeAndNil(FFonts);
  inherited Destroy;
end;

end.
