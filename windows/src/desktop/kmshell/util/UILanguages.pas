(*
  Name:             UILanguages
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      25 May 2010

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          25 May 2010 - mcdurdin - I1694 - Select Keyman UI language rework
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit UILanguages;  // I3306

interface

uses
  keymanapi_TLB, Classes, CustInterfaces;

type
  TUILanguages = class
  private
    FLanguages: TStringList;
    function GetCount: Integer;
    function GetLanguageCode(Index: Integer): WideString;
    function GetLanguageName(Index: Integer): WideString;
    function GetLanguageNameByCode(Index: WideString): WideString;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Refresh;
    property Count: Integer read GetCount;
    property LanguageCode[Index: Integer]: WideString read GetLanguageCode;
    property LanguageName[Index: Integer]: WideString read GetLanguageName;
    property LanguageNameByCode[Index: WideString]: WideString read GetLanguageNameByCode;
  end;

procedure CreateUILanguages;
function UILanguageList: TUILanguages;

implementation

uses
  kmint, MessageIdentifiers, MessageIdentifierConsts, SysUtils, WideStrings;

var
  FUILanguages: TUILanguages = nil;

procedure CreateUILanguages;
begin
  FreeAndNil(FUILanguages);
  FUILanguages := TUILanguages.Create;
end;

function UILanguageList: TUILanguages;
begin
  if not Assigned(FUILanguages) then
  begin
    CreateUILanguages;
  end;
  Result := FUILanguages;
end;

{ TUILanguages }

constructor TUILanguages.Create;
begin
  FLanguages := TStringList.Create;
  Refresh;
end;

destructor TUILanguages.Destroy;
begin
  FLanguages.Free;
  inherited Destroy;
end;

function TUILanguages.GetCount: Integer;
begin
  Result := FLanguages.Count;
end;

function TUILanguages.GetLanguageCode(Index: Integer): WideString;
begin
  Result := FLanguages.Names[Index];
end;

function TUILanguages.GetLanguageName(Index: Integer): WideString;
begin
  Result := FLanguages.ValueFromIndex[Index];
end;

function TUILanguages.GetLanguageNameByCode(Index: WideString): WideString;
begin
  Result := FLanguages.Values[Index];
end;

procedure TUILanguages.Refresh;
var
  FLanguagesAvailableString: WideString;
  i: Integer;
begin
  with kmint.KeymanCustomisation.CustMessages do
  begin
    FLanguagesAvailableString := MsgFromId(SKDefaultLanguageCode)+#13#10+GetAvailableLanguages;
    FLanguages.Text := FLanguagesAvailableString;

    for i := 0 to FLanguages.Count - 1 do
      FLanguages[i] := FLanguages[i] + '=' + MessageFromID(StringFromMsgId(SKUILanguageNameWithEnglish), FLanguages[i]);
  end;
end;

initialization
finalization
  FreeAndNil(FUILanguages);
end.
