(*
  Name:             TouchLayoutUtils
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      21 Feb 2014

  Modified Date:    24 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          21 Feb 2014 - mcdurdin - I4057 - V9.0 - Keyman Developer Keyboard Font dialog helpful to reduce font confusion
                    27 May 2015 - mcdurdin - I4655 - Developer crashes when changing font settings and in code view for touch layout if not on first line [CrashID:tike.exe_9.0.487.0_0069BE51_ERangeError]
                    24 Aug 2015 - mcdurdin - I4872 - OSK font and Touch Layout font should be the same in Developer
*)
unit TouchLayoutUtils;

interface

uses
  KeyboardFonts;

function IsValidTouchLayoutFile(const FileName: string; var ErrMsg: string): Boolean;
function IsValidTouchLayout(const JSON: string; var ErrMsg: string): Boolean;
//function IsValidLayoutTemplateFile(const FileName: string; var ErrMsg: string): Boolean;
function UpdateTouchLayoutFont(var JSON: string; Index: TKeyboardFont; FontName, FontSize: string): Boolean;   // I4057

implementation

uses
  Classes,
  SysUtils,
  TouchLayout,

  System.JSON;

function UpdateTouchLayoutFont(var JSON: string; Index: TKeyboardFont; FontName, FontSize: string): Boolean;   // I4057
var
  FIndexName: string;
  FPlatform: TJSONObject;
  FPair: TJSONPair;
  v: Extended;
begin
  with TTouchLayout.Create do
  try
    if not Load(JSON) then
      Exit(False);

    case Index of
      kfontTouchLayoutPhone: FIndexName := 'phone';
      kfontTouchLayoutTablet: FIndexName := 'tablet';
      kfontTouchLayoutDesktop: FIndexName := 'desktop';
      else Exit(False);
    end;

    if not HasPlatform(FIndexName) then
      Exit(True);

    FPlatform := (LayoutPlatform[FIndexName].JsonValue as TJSONObject);
    FPair := FPlatform.Get('font');
    if Assigned(FPair)
      then FPair.JsonValue := TJSONString.Create(FontName)
      else FPlatform.AddPair('font', FontName);

    if TryStrToFloat(Trim(FontSize), v) then   // I4872
      FontSize := Trim(FontSize) + 'em';
    FPair := FPlatform.Get('fontsize');
    if Assigned(FPair)
      then FPair.JsonValue := TJSONString.Create(FontSize)
      else FPlatform.AddPair('fontsize', FontSize);

    JSON := Save(True);   // I4655
  finally
    Free;
  end;

  Result := True;
end;

function IsValidTouchLayout(const JSON: string; var ErrMsg: string): Boolean;
var
  v: TJSONObject;
begin
  Result := False;
  try
    v := TJSONObject.ParseJSONValue(TrimRight(JSON)) as TJSONObject;
    try
      // TODO: Validate that the JSON is valid?
      Result := v <> nil;
    finally
      v.Free;
    end;
  except
    on E:EJSONException do
    begin
      ErrMsg := E.Message;
    end;
  end;
end;

function IsValidTouchLayoutFile(const FileName: string; var ErrMsg: string): Boolean;
begin
  try
    with TStringStream.Create('', TEncoding.UTF8) do
    try
      LoadFromFile(FileName);
      Result := IsValidTouchLayout(DataString, ErrMsg);
    finally
      Free;
    end;
  except
    // Files we can't read, we ignore
    Result := False;
  end;
end;

end.
