(*
  Name:             CharacterMapSettings
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    14 Sep 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Initial version
*)
unit CharacterMapSettings;

interface

type
  TCharMapSettings = class
  private
    function GetIsUnicode: Boolean;
    procedure SetIsUnicode(const Value: Boolean);
  public
    SelectedCharacterSet, SelectedSubRange: Integer;
    FontName: string;
    UseQuotedTextFont: Boolean;
    constructor Create;
    property IsUnicode: Boolean read GetIsUnicode write SetIsUnicode;
  end;

implementation

uses CharacterRanges;

{ TCharMapSettings }

constructor TCharMapSettings.Create;
begin
  inherited Create;
  SelectedCharacterSet := CR_UnicodeRange;
  SelectedSubRange := -1;
  FontName := 'Arial';
end;

function TCharMapSettings.GetIsUnicode: Boolean;
begin
  Result := not CharacterRange[SelectedCharacterSet].IsANSI;
end;

procedure TCharMapSettings.SetIsUnicode(const Value: Boolean);
begin
  if Value <> IsUnicode then
  begin
    if Value
      then SelectedCharacterSet := CR_UnicodeRange
      else SelectedCharacterSet := CR_ANSIRange;
    SelectedSubRange := -1;
  end;
end;

end.
