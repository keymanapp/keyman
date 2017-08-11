{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is JvValidateEdit, released on 20 February 2003,
  by Christopher Latta
Portions created by Christopher Latta are Copyright (C) 2003 Christopher Latta.
All Rights Reserved.

Contributor(s): Peter Th�rnqvist

Last Modified: 2003-02-20

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
TJvValidateFormat uses the SysUtils.Format function to format numeric values.
While this uses the Windows regional settings for the currency symbol, decimal
separator and thousands separator, it does not format using the negative symbol,
negative number format, negative currency format and positive currency format.
This could be rectified by a custom-written formatting routine.

-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvValidateEdit;

interface

uses
  { delphi } Classes, Messages, Controls, Graphics,
  { local } JvEdit, JVCLVer;

type
  TJvValidateEditDisplayFormat = (dfAlphabetic, dfAlphaNumeric, dfBinary,
    dfCheckChars, dfCurrency, dfCustom, dfFloat, dfHex, dfInteger,
    dfNonCheckChars, dfNone, dfOctal, dfPercent, dfScientific, dfYear);

  TJvValidateEditCriticalPointsCheck = (cpNone, cpMaxValue, cpBoth);

  TJvValidateEditCriticalPoints = class(TPersistent)
  private
    FCheckPoints: TJvValidateEditCriticalPointsCheck;
    FColorAbove: TColor;
    FColorBelow: TColor;
    FMaxValue: Double;
    FMinValue: Double;
    FOnChange: TNotifyEvent;
    procedure DoChanged;
    procedure SetMinValue(NewValue: Double);
    procedure SetMaxValue(NewValue: Double);
    procedure SetColorAbove(NewValue: TColor);
    procedure SetColorBelow(NewValue: TColor);
    procedure SetCheckPoints(NewValue: TJvValidateEditCriticalPointsCheck);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create;
  published
    property CheckPoints: TJvValidateEditCriticalPointsCheck read FCheckPoints
        write SetCheckPoints;
    property ColorAbove: TColor read FColorAbove write SetColorAbove;
    property ColorBelow: TColor read FColorBelow write SetColorBelow;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvCustomTextValidateEvent = procedure(Sender: TObject; Key: Char;
    const AText: string; const Pos: Integer; var IsValid: boolean) of object;

  TJvCustomValidateEdit = class(TJvCustomEdit)
  private
    bSelfChange: Boolean;
    FAboutJVCL: TJVCLAboutInfo;
    FCheckChars: string;
    FDecimalPlaces: Cardinal;
    FDisplayFormat: TJvValidateEditDisplayFormat;
    FEditText: string;
    FHasMaxValue: Boolean;
    FHasMinValue: Boolean;
    FMaxValue: Double;
    FMinValue: Double;
    FOnCustomValidate: TJvCustomTextValidateEvent;
    FOnValueChanged: TNotifyEvent;
    FZeroEmpty: Boolean;
    EnterText: string;
    FDisplayPrefix: string;
    FDisplaySuffix: string;
    FCriticalPoints: TJvValidateEditCriticalPoints;
    StandardFontColor: TColor;
    FAutoAlignment: Boolean;
    procedure SetText(NewValue: TCaption);
    function GetText: TCaption;
    procedure DisplayText;
    function IsValidChar(const S: string; Key: Char; Posn: Integer): boolean; virtual;
    function MakeValid(ParseString: string): string;
    function ScientificStrToFloat(SciString: string): Double;
    procedure SetHasMaxValue(NewValue: Boolean);
    procedure SetHasMinValue(NewValue: Boolean);
    procedure SetMaxValue(NewValue: Double);
    procedure SetMinValue(NewValue: Double);
    procedure SetDecimalPlaces(NewValue: Cardinal);
    procedure SetDisplayFormat(NewValue: TJvValidateEditDisplayFormat);
    procedure SetZeroEmpty(NewValue: Boolean);
    function GetAsInteger: Integer;
    procedure SetAsInteger(NewValue: Integer);
    function GetAsCurrency: Currency;
    procedure SetAsCurrency(NewValue: Currency);
    function GetAsFloat: Double;
    procedure SetAsFloat(NewValue: Double);
    function GetValue: Variant;
    procedure SetValue(NewValue: Variant);
    procedure SetCheckChars(const NewValue: string);
    procedure CMChanged(var Message: TMessage); message CM_CHANGED;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    function CurrRangeValue(CheckValue: Currency): Currency; overload;
    function FloatRangeValue(CheckValue: Double): Double; overload;
    function IntRangeValue(CheckValue: Integer): Integer; overload;
    function GetEditText: string;
    procedure SetEditText(const NewValue: string);
    procedure ChangeText(NewValue: string);
    function BaseToInt(BaseValue: string; Base: Byte): Integer;
    function IntToBase(NewValue, Base: Byte): string;
    procedure DoValueChanged;
    procedure SetDisplayPrefix(const NewValue: string);
    procedure SetDisplaySuffix(const NewValue: string);
    procedure CriticalPointsChange(Sender: TObject);
    procedure SetFontColor;
    procedure FontChange(Sender: TObject);
    procedure EnforceMaxValue;
    procedure EnforceMinValue;
  protected
    property CheckChars: string read FCheckChars write SetCheckChars;
    property DecimalPlaces: Cardinal read FDecimalPlaces write SetDecimalPlaces;
    property DisplayFormat: TJvValidateEditDisplayFormat read FDisplayFormat write
        SetDisplayFormat;
    property EditText: string read GetEditText write SetEditText;
    property HasMaxValue: Boolean read FHasMaxValue write SetHasMaxValue;
    property HasMinValue: Boolean read FHasMinValue write SetHasMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property OnCustomValidate: TJvCustomTextValidateEvent
      read FOnCustomValidate write FOnCustomValidate;
    property OnValueChanged: TNotifyEvent read FOnValueChanged write
        FOnValueChanged;
    property Text: TCaption read GetText write SetText;
    property Value: Variant read GetValue write SetValue;
    property ZeroEmpty: Boolean read FZeroEmpty write SetZeroEmpty;
    property DisplayPrefix: string read FDisplayPrefix write SetDisplayPrefix;
    property DisplaySuffix: string read FDisplaySuffix write SetDisplaySuffix;
    property CriticalPoints: TJvValidateEditCriticalPoints read FCriticalPoints
        write FCriticalPoints;
    property AutoAlignment: Boolean read FAutoAlignment write FAutoAlignment;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function DoValidate(const Key: Char; const AText: string;
      const Posn: Integer): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvValidateEdit = class(TJvCustomValidateEdit)
  published
    property Alignment default taRightJustify;
    property Anchors;
    property AutoAlignment default True;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property CheckChars;
    property CharCase;
    property Color;
    property Constraints;
    property CriticalPoints;
    property Ctl3D;
    property DecimalPlaces default 0;
    property DisplayFormat default dfInteger;
    property DisplayPrefix;
    property DisplaySuffix;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditText;
    property Enabled;
    property Font;
    property HasMaxValue default False;
    property HasMinValue default False;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property MaxValue;
    property MinValue;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Value;
    property Visible;
    property ZeroEmpty default False;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnCustomValidate;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnValueChanged;
  end;

implementation

uses
  { Delphi } Windows, SysUtils, Math,
  { local } JvFunctions;


constructor TJvCustomValidateEdit.Create(AOwner: TComponent);
begin
  inherited;

  bSelfChange := False;
  FAutoAlignment := True;
  FCriticalPoints := TJvValidateEditCriticalPoints.Create;
  FCriticalPoints.OnChange := CriticalPointsChange;
  FDisplayFormat := dfInteger;
  FCheckChars := '01234567890';
  Alignment := taRightJustify;
  FEditText := '';
  Text      := '';
  AutoSize  := True;
  FMinValue    := 0;
  FMaxValue    := 0;
  FHasMinValue := False;
  FHasMaxValue := False;
  FZeroEmpty := False;
  StandardFontColor := Font.Color;
  Font.OnChange := FontChange;
end;

procedure TJvCustomValidateEdit.Assign(Source: TPersistent);
var
  lcSource: TJvCustomValidateEdit;
begin
  inherited;

  if Source is TJvCustomValidateEdit then
  begin
    lcSource    := Source as TJvCustomValidateEdit;
    CriticalPoints.Assign(lcSource.CriticalPoints);
    DisplayFormat := lcSource.DisplayFormat;
    DecimalPlaces := lcSource.DecimalPlaces;
    MinValue    := lcSource.MinValue;
    MaxValue    := lcSource.MaxValue;
    HasMinValue := lcSource.HasMinValue;
    HasMaxValue := lcSource.HasMaxValue;
    ZeroEmpty := lcSource.ZeroEmpty;
  end;
end;

procedure TJvCustomValidateEdit.SetHasMaxValue(NewValue: Boolean);
begin
  if FHasMaxValue <> NewValue then
  begin
    FHasMaxValue := NewValue;
    EnforceMaxValue;
  end;
end;

procedure TJvCustomValidateEdit.SetHasMinValue(NewValue: Boolean);
begin
  if FHasMinValue <> NewValue then
  begin
    FHasMinValue := NewValue;
    EnforceMinValue;
  end;
end;

procedure TJvCustomValidateEdit.SetMaxValue(NewValue: Double);
begin
  if FMaxValue <> NewValue then
  begin
    FMaxValue := NewValue;
    { make MinValue consistent }
    if FMinValue > FMaxValue then
      FMinValue := FMaxValue;
    EnforceMaxValue;
  end;
end;

procedure TJvCustomValidateEdit.SetMinValue(NewValue: Double);
begin
  if FMinValue <> NewValue then
  begin
    FMinValue := NewValue;
    { make MaxValue consistent }
    if FMaxValue < FMinValue then
      FMaxValue := FMinValue;
    EnforceMinValue;
  end;
end;

procedure TJvCustomValidateEdit.SetDecimalPlaces(NewValue: Cardinal);
begin
  if ControlState = [csReadingState] then
    FDecimalPlaces := NewValue
  else
    if (FDisplayFormat in [dfCurrency, dfFloat, dfScientific, dfPercent]) then
      FDecimalPlaces := NewValue;
  EditText := FEditText;
end;

procedure TJvCustomValidateEdit.SetDisplayFormat(NewValue: 
    TJvValidateEditDisplayFormat);
const
  ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
  NUMBERS = '0123456789';
var
  OldFormat: TJvValidateEditDisplayFormat;
begin
  if FDisplayFormat <> NewValue then
  begin
    EnterText := FEditText;
    OldFormat := FDisplayFormat;
    FDisplayFormat := NewValue;
    case FDisplayFormat of
      dfAlphabetic: begin
        FCheckChars := ALPHABET;
        if FAutoAlignment then
          Alignment := taLeftJustify;
      end;
      dfAlphaNumeric: begin
        FCheckChars := ALPHABET + NUMBERS;
        if FAutoAlignment then
          Alignment := taLeftJustify;
      end;
      dfBinary: begin
        FCheckChars := '01';
        if FAutoAlignment then
          Alignment := taRightJustify;
      end;
      dfCheckChars, dfNonCheckChars: Alignment := taLeftJustify;
      dfCustom, dfNone: begin
        FCheckChars := '';
        if FAutoAlignment then
          Alignment := taLeftJustify;
      end;
      dfCurrency: begin
        FCheckChars := NUMBERS + DecimalSeparator;
        if FAutoAlignment then
          Alignment := taRightJustify;
        if FDecimalPlaces = 0 then
          FDecimalPlaces := CurrencyDecimals;
      end;
      dfFloat, dfPercent: begin
        FCheckChars := NUMBERS + DecimalSeparator;
        if FAutoAlignment then
          Alignment := taRightJustify;
      end;
      dfHex: begin
        FCheckChars := NUMBERS + 'ABCDEFabcdef';
        if FAutoAlignment then
          Alignment := taRightJustify;
      end;
      dfInteger: begin
        FCheckChars := NUMBERS;
        if FAutoAlignment then
          Alignment := taRightJustify;
      end;
      dfOctal: begin
        FCheckChars := '01234567';
        if FAutoAlignment then
          Alignment := taRightJustify;
      end;
      dfScientific: begin
        FCheckChars := NUMBERS + 'Ee' + DecimalSeparator;
        if FAutoAlignment then
          Alignment := taRightJustify;
      end;
      dfYear: begin
        FCheckChars := NUMBERS;
        if FAutoAlignment then
          Alignment := taRightJustify;
        MaxLength := 4;
      end;
    end;

    if OldFormat = dfYear then
      MaxLength := 0;

    // Convert non-base 10 numbers to base 10 and base-10 numbers to non-base 10
    if (OldFormat = dfBinary) and (NewValue in [dfCurrency, dfFloat, dfHex, dfInteger, dfOctal, dfPercent, dfScientific, dfYear]) then
      SetAsInteger(BaseToInt(FEditText, 2))
    else if (OldFormat in [dfCurrency, dfFloat, dfPercent]) and (NewValue in [dfBinary, dfHex, dfOctal]) then
      SetAsFloat(StrToFloatDef(FEditText, 0))
    else if (OldFormat = dfHex) and (NewValue in [dfBinary, dfCurrency, dfFloat, dfInteger, dfOctal, dfPercent, dfScientific, dfYear]) then
      SetAsInteger(BaseToInt(FEditText, 16))
    else if (OldFormat in [dfInteger, dfYear]) and (NewValue in [dfBinary, dfHex, dfOctal]) then
      SetAsInteger(StrToIntDef(FEditText, 0))
    else if (OldFormat = dfOctal) and (NewValue in [dfBinary, dfCurrency, dfFloat, dfHex, dfInteger, dfPercent, dfScientific, dfYear]) then
      SetAsInteger(BaseToInt(FEditText, 8))
    else
    begin
      // ...or just display the value
      EditText := FEditText;
      DoValueChanged;
    end;
  end;
end;

procedure TJvCustomValidateEdit.SetZeroEmpty(NewValue: Boolean);
begin
  if FZeroEmpty <> NewValue then
  begin
    FZeroEmpty := NewValue;
    EditText := FEditText;
  end;
end;

function TJvCustomValidateEdit.GetAsInteger: Integer;
begin
  case FDisplayFormat of
    dfBinary: Result := BaseToInt(FEditText, 2);
    dfHex: Result := BaseToInt(FEditText, 16);
    dfOctal: Result := BaseToInt(FEditText, 8);
    else
      Result := StrToIntDef(FEditText, 0);
  end;
end;

procedure TJvCustomValidateEdit.SetAsInteger(NewValue: Integer);
begin
  EnterText := FEditText;
  case FDisplayFormat of
    dfAlphabetic, dfAlphaNumeric, dfCheckChars, dfCustom,
      dfNonCheckChars, dfNone: EditText := IntToStr(NewValue);
    dfBinary: EditText := IntToBase(NewValue, 2);
    dfHex: EditText := IntToBase(NewValue, 16);
    dfOctal: EditText := IntToBase(NewValue, 8);
    dfCurrency, dfFloat, dfInteger, dfPercent, dfScientific, dfYear:
      EditText := IntToStr(IntRangeValue(NewValue));
  end;
  DoValueChanged;
end;

function TJvCustomValidateEdit.GetAsCurrency: Currency;
begin
  case FDisplayFormat of
    dfBinary: Result := BaseToInt(FEditText, 2);
    dfHex: Result := BaseToInt(FEditText, 16);
    dfOctal: Result := BaseToInt(FEditText, 8);
    else
      Result := StrToCurrDef(FEditText, 0);
  end;
end;

procedure TJvCustomValidateEdit.SetAsCurrency(NewValue: Currency);
begin
  EnterText := FEditText;
  case FDisplayFormat of
    dfAlphabetic, dfAlphaNumeric, dfCheckChars, dfCustom,
      dfNonCheckChars, dfNone: EditText := CurrToStr(NewValue);
    dfBinary: EditText := IntToBase(Trunc(NewValue), 2);
    dfHex: EditText := IntToBase(Trunc(NewValue), 16);
    dfOctal: EditText := IntToBase(Trunc(NewValue), 8);
    dfCurrency, dfFloat, dfInteger, dfPercent, dfScientific, dfYear:
      EditText := CurrToStr(CurrRangeValue(NewValue));
  end;
  DoValueChanged;
end;

function TJvCustomValidateEdit.GetAsFloat: Double;
begin
  case FDisplayFormat of
    dfBinary: Result := BaseToInt(FEditText, 2);
    dfHex: Result := BaseToInt(FEditText, 16);
    dfOctal: Result := BaseToInt(FEditText, 8);
    dfScientific: Result := ScientificStrToFloat(FEditText);
    else
      Result := StrToFloatDef(FEditText, 0);
  end;
end;

procedure TJvCustomValidateEdit.SetAsFloat(NewValue: Double);
begin
  EnterText := FEditText;
  case FDisplayFormat of
    dfAlphabetic, dfAlphaNumeric, dfCheckChars, dfCustom,
      dfNonCheckChars, dfNone: EditText := FloatToStr(NewValue);
    dfBinary: EditText := IntToBase(Trunc(NewValue), 2);
    dfHex: EditText := IntToBase(Trunc(NewValue), 16);
    dfOctal: EditText := IntToBase(Trunc(NewValue), 8);
    dfInteger, dfYear: EditText := IntToStr(IntRangeValue(Trunc(NewValue)));
    dfCurrency, dfFloat, dfPercent:
      EditText := FloatToStr(FloatRangeValue(NewValue));
    dfScientific: EditText := Format('%e', [FloatRangeValue(NewValue)]);
  end;
  DoValueChanged;
end;

function TJvCustomValidateEdit.GetValue: Variant;
begin
  case FDisplayFormat of
    dfCurrency:
      Result := StrToCurrDef(FEditText,0);
    dfFloat, dfPercent, dfScientific:
      Result := StrToFloatDef(FEditText,0);
    dfInteger, dfYear:
      Result := StrToIntDef(Text,0);
    else
      Result := inherited Text;
  end;
end;

procedure TJvCustomValidateEdit.SetValue(NewValue: Variant);
begin
  case FDisplayFormat of
    dfAlphabetic, dfAlphaNumeric, dfCheckChars, dfNonCheckChars, dfNone, dfCustom:
      EditText := NewValue;
    dfBinary, dfHex, dfInteger, dfOctal, dfYear: SetAsInteger(NewValue);
    dfCurrency, dfFloat, dfPercent, dfScientific: SetAsFloat(NewValue);
  end;
end;

procedure TJvCustomValidateEdit.SetCheckChars(const NewValue: string);
begin
  if (FDisplayFormat in [dfNone, dfCheckChars, dfNonCheckChars]) and
    (FCheckChars <> NewValue) then
  begin
    FCheckChars := NewValue;
    EditText := MakeValid(FEditText);
  end;
end;

procedure TJvCustomValidateEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Text, Key, SelStart) and (Key >= #32) then
    Key := #0;
  inherited;
end;

procedure TJvCustomValidateEdit.WMPaste(var Message: TWMPaste);
begin
  EnterText := FEditText;
  inherited;
  EditText := MakeValid(inherited Text);
end;

function TJvCustomValidateEdit.MakeValid(ParseString: string): string;
var
  S: string;
  i: integer;
begin
  S := '';
  for i := 1 to Length(ParseString) do
    if IsValidChar(Copy(ParseString, 1, i - 1), ParseString[i], i) then
      S := S + ParseString[i];
  Result := S;
end;

function TJvCustomValidateEdit.IsValidChar(const S: string;
  Key: Char; Posn: Integer): boolean;
var
  iPosE: Integer;
begin
  case FDisplayFormat of
    dfBinary, dfCheckChars, dfHex, dfOctal, dfYear:
      Result := Pos(Key, FCheckChars) > 0;
    dfAlphabetic: Result := IsCharAlpha(Key);
    dfAlphaNumeric: Result := IsCharAlphaNumeric(Key);
    dfCustom: Result := DoValidate(Key, S, Posn);
    dfInteger: Result := (Pos(Key, FCheckChars) > 0)
      or ((Key = '+') and (Pos('+', S) = 0))
      or ((Key = '-') and (Pos('-', S) = 0));
    dfFloat, dfCurrency, dfPercent: Result := (Pos(Key, FCheckChars) > 0)
      or ((Key = DecimalSeparator) and (Pos(DecimalSeparator, S) = 0))
      or ((Key = '+') and (Pos('+', S) = 0))
      or ((Key = '-') and (Pos('-', S) = 0));
    dfNonCheckChars: Result := Pos(Key, FCheckChars) = 0;
    dfNone: Result := True;
    dfScientific: begin
      Result := (Pos(Key, FCheckChars) > 0) or (Key in ['+', '-']);
      if Result then
      begin
        iPosE := Pos('e', LowerCase(S));
        if Key = DecimalSeparator then
        begin
          if iPosE = 0 then
            Result := (Pos(DecimalSeparator, S) = 0)
          else
            Result := ((Posn < iPosE) and (Pos(DecimalSeparator, Copy(S, 1, iPosE-1)) = 0))
              or ((Posn > iPosE) and (Pos(DecimalSeparator, Copy(S, iPosE+1, 99)) = 0));
        end
        else if Key in ['E', 'e'] then
          Result := (iPosE = 0) and (Posn > 1)
        else if Key = '+' then
          Result := (Posn = 0) or (Posn = iPosE)
        else if Key = '-' then
          Result := (Posn = 0) or (Posn = iPosE);
      end;
    end;
    else
      Result := False;
  end;
end;

function TJvCustomValidateEdit.DoValidate(const Key: Char;
  const AText: string; const Posn: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCustomValidate) then
    FOnCustomValidate(Self, Key, AText, Posn, Result);
end;


procedure TJvCustomValidateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_DELETE then
    EditText := MakeValid(inherited Text);
end;

function TJvCustomValidateEdit.CurrRangeValue(CheckValue: Currency): Currency;
begin
  Result := CheckValue;
  if FHasMaxValue and (CheckValue > FMaxValue) then
    Result := FMaxValue
  else if FHasMinValue and (CheckValue < FMinValue) then
    Result := FMinValue;
end;

function TJvCustomValidateEdit.FloatRangeValue(CheckValue: Double): Double;
begin
  Result := CheckValue;
  if FHasMaxValue and (CheckValue > FMaxValue) then
    Result := FMaxValue
  else if FHasMinValue and (CheckValue < FMinValue) then
    Result := FMinValue;
end;

function TJvCustomValidateEdit.IntRangeValue(CheckValue: Integer): Integer;
begin
  Result := CheckValue;
  if FHasMaxValue and (CheckValue > FMaxValue) then
    Result := Trunc(FMaxValue)
  else if FHasMinValue and (CheckValue < FMinValue) then
    Result := Trunc(FMinValue);
end;


function TJvCustomValidateEdit.GetEditText: string;
begin
  Result := FEditText;
end;

procedure TJvCustomValidateEdit.SetEditText(const NewValue: string);
begin
  FEditText := MakeValid(NewValue);
  if (FDisplayFormat = dfYear) and ((not FHasMaxValue) or
    (FHasMaxValue and (FMaxValue > 2000+TwoDigitYearCenturyWindow)))
    and ((MaxLength = 0) or (MaxLength > 3)) then
      FEditText := IntToStr(MakeYear4Digit(StrToIntDef(FEditText, 0), TwoDigitYearCenturyWindow));
  if FDisplayFormat in [dfBinary, dfCurrency, dfFloat, dfHex, dfInteger,
    dfOctal, dfPercent, dfScientific, dfYear] then
  begin
    EnForceMaxValue;
    EnforceMinValue;
  end;
  ChangeText(FEditText);
  DisplayText;
end;


procedure TJvCustomValidateEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  EnterText := FEditText;
  DisplayText;
  inherited;
end;

procedure TJvCustomValidateEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  EditText := inherited Text;
  inherited;
end;

procedure TJvCustomValidateEdit.ChangeText(NewValue: string);
begin
  bSelfChange := True;
  inherited Text := FDisplayPrefix + NewValue + FDisplaySuffix;
  bSelfChange := False;
end;

procedure TJvCustomValidateEdit.DisplayText;
begin
  // The number types need to be formatted
  if (FDisplayFormat in [dfBinary, dfCurrency, dfFloat, dfInteger,
    dfOctal, dfPercent, dfScientific, dfYear]) and (AsFloat = 0) and FZeroEmpty then
      ChangeText('')
  else
  begin
    if (FCriticalPoints.CheckPoints <> cpNone) and (FDisplayFormat in
      [dfBinary, dfCurrency, dfFloat, dfHex, dfInteger, dfOctal, dfPercent,
      dfScientific, dfYear]) then
        SetFontColor;
    case FDisplayFormat of
      dfCurrency:
        ChangeText(Format('%.*m',[FDecimalPlaces,AsCurrency]));
      dfInteger:
        ChangeText(Format('%d', [AsInteger]));
      dfFloat:
        ChangeText(Format('%.*n',[FDecimalPlaces, AsFloat]));
      dfScientific:
        ChangeText(Format('%.*e',[FDecimalPlaces,AsFloat]));
      dfPercent:
        ChangeText(Format('%.*n%',[FDecimalPlaces, AsFloat]));
      else
        ChangeText(FEditText);
    end;
  end;
end;

function TJvCustomValidateEdit.ScientificStrToFloat(SciString: string): Double;
var
  i: Cardinal;
  sMantissa, sExponent: string;
  bInExp: boolean;
begin
  if Pos('E', Uppercase(SciString)) = 0 then
    Result := StrToFloatDef(SciString, 0)
  else
  begin
    sMantissa := '';
    sExponent := '';
    bInExp := False;
    for i := 1 to Length(SciString) do
    begin
      if UpperCase(SciString[i]) = 'E' then
        bInExp := True
      else
      begin
        if bInExp then
          sExponent := sExponent + SciString[i]
        else
          sMantissa := sMantissa + SciString[i];
      end;
    end;
    Result := StrToFloatDef(sMantissa, 0) * Power(10, StrToFloatDef(sExponent, 0));
  end;
end;

function TJvCustomValidateEdit.BaseToInt(BaseValue: string; Base: Byte): Integer;
var
  i: integer;

  function BaseCharToInt(BaseChar: Char): integer;
  begin
    case Ord(BaseChar) of
      Ord('0')..Ord('9'): Result := Ord(BaseChar) - Ord('0');
      else
        Result := Ord(BaseChar) - Ord('A') + 10;
    end;
  end;

begin
  Assert(Base <= 36, 'BaseToInt: Base > 36 not supported');
  Assert(Base > 1, 'BaseToInt: Base must be greater than 1');

  Result := 0;
  for i := 1 to Length(BaseValue) do
    Inc(Result,Trunc(BaseCharToInt(BaseValue[i]) * Power(Base, Length(BaseValue)-i)));
end;

function TJvCustomValidateEdit.IntToBase(NewValue, Base: Byte):
    string;
var
  iDivisor, iRemainder, i: Cardinal;
  iBaseIterations: Integer;

  function IntToBaseChar(IntValue: Integer): Char;
  begin
    case IntValue of
      0..9: Result := Chr(Ord('0') + IntValue);
      else
        Result := Chr(Ord('A') + IntValue - 10);
    end;
  end;

begin
  Assert(Base <= 36, 'IntToBase: Base > 36 not supported');
  Assert(Base > 1, 'IntToBase: Base must be greater than 1');

  Result := '';
  iRemainder := NewValue;
  if NewValue >= Base then
  begin
    iDivisor := 1;
    iBaseIterations := -1;
    while (NewValue div iDivisor) > 0 do
    begin
      iDivisor := iDivisor * Base;
      Inc(iBaseIterations);
    end;
    iDivisor := iDivisor div Base;
    for i := 1 to iBaseIterations do
    begin
      Result := Result + IntToBaseChar(iRemainder div iDivisor);
      iRemainder := iRemainder mod iDivisor;
      iDivisor := iDivisor div Base;
    end;
  end;
  Result := Result + IntToBaseChar(iRemainder);
end;

procedure TJvCustomValidateEdit.DoValueChanged;
begin
  if Assigned(FOnValueChanged) and (EnterText <> FEditText) then
    FOnValueChanged(Self);
end;

procedure TJvCustomValidateEdit.CMChanged(var Message: TMessage);
begin
  // Update FEditText for User changes, so that the AsInteger, etc,
  // functions work while editing
  if not bSelfChange then
    FEditText := inherited Text;
  if Assigned(OnChange) then
    OnChange(Self);
  inherited;
end;

function TJvCustomValidateEdit.GetText: TCaption;
begin
  Result := inherited Text;
end;

procedure TJvCustomValidateEdit.SetText(NewValue: TCaption);
begin
  EnterText := FEditText;
  EditText := NewValue;
  DoValueChanged;
end;

procedure TJvCustomValidateEdit.SetDisplayPrefix(const NewValue: string);
begin
  FDisplayPrefix := NewValue;
  DisplayText;
end;

procedure TJvCustomValidateEdit.SetDisplaySuffix(const NewValue: string);
begin
  FDisplaySuffix := NewValue;
  DisplayText;
end;

procedure TJvCustomValidateEdit.CriticalPointsChange(Sender: TObject);
begin
  SetFontColor;
  Invalidate;
end;

procedure TJvCustomValidateEdit.SetFontColor;
begin
  Font.OnChange := nil;
  case FCriticalPoints.CheckPoints of
    cpNone: Font.Color := StandardFontColor;
    cpMaxValue:
      if AsFloat > FCriticalPoints.MaxValue then
        Font.Color := FCriticalPoints.ColorAbove
      else
        Font.Color := StandardFontColor;
    cpBoth:
      if AsFloat > FCriticalPoints.MaxValue then
        Font.Color := FCriticalPoints.ColorAbove
      else if AsFloat < FCriticalPoints.MinValue then
        Font.Color := FCriticalPoints.ColorBelow
      else
        Font.Color := StandardFontColor;
  end;
  Font.OnChange := FontChange;
  Invalidate;
end;

procedure TJvCustomValidateEdit.FontChange(Sender: TObject);
begin
  StandardFontColor := Font.Color;
end;

procedure TJvCustomValidateEdit.EnforceMaxValue;
begin
  { Check the Value is within this range }
  if FHasMaxValue and (FDisplayFormat in [dfBinary, dfCurrency, dfFloat,
    dfHex, dfInteger, dfOctal, dfPercent, dfScientific, dfYear]) and
    (AsFloat > FMaxValue) then
    SetAsFloat(FMaxValue);
end;

procedure TJvCustomValidateEdit.EnforceMinValue;
begin
  { Check the Value is within this range }
  if FHasMinValue and (FDisplayFormat in [dfBinary, dfCurrency, dfFloat,
    dfHex, dfInteger, dfOctal, dfPercent, dfScientific, dfYear]) and
    (AsFloat < FMinValue) then
    SetAsFloat(FMinValue);
end;

{ TJvValidateEditCriticalPoints }

constructor TJvValidateEditCriticalPoints.Create;
begin
  inherited;

  FCheckPoints := cpNone;
  FColorAbove := clBlue;
  FColorBelow := clRed;
end;

procedure TJvValidateEditCriticalPoints.SetCheckPoints(NewValue:
    TJvValidateEditCriticalPointsCheck);
begin
  if FCheckPoints <> NewValue then
  begin
    FCheckPoints := NewValue;
    DoChanged;
  end;
end;

procedure TJvValidateEditCriticalPoints.SetColorAbove(NewValue: TColor);
begin
  if FColorAbove <> NewValue then
  begin
    FColorAbove := NewValue;
    DoChanged;
  end;
end;

procedure TJvValidateEditCriticalPoints.SetColorBelow(NewValue: TColor);
begin
  if FColorBelow <> NewValue then
  begin
    FColorBelow := NewValue;
    DoChanged;
  end;
end;

procedure TJvValidateEditCriticalPoints.SetMaxValue(NewValue: Double);
begin
  if FMaxValue <> NewValue then
  begin
    FMaxValue := NewValue;
    DoChanged;
  end;
end;

procedure TJvValidateEditCriticalPoints.SetMinValue(NewValue: Double);
begin
  if FMinValue <> NewValue then
  begin
    FMinValue := NewValue;
    DoChanged;
  end;
end;

procedure TJvValidateEditCriticalPoints.DoChanged;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvValidateEditCriticalPoints.Assign(Source: TPersistent);
var
  lcSource: TJvValidateEditCriticalPoints;
begin
  inherited;

  if Source is TJvValidateEditCriticalPoints then
  begin
    lcSource := Source as TJvValidateEditCriticalPoints;
    CheckPoints := lcSource.CheckPoints;
    ColorAbove := lcSource.ColorAbove;
    ColorBelow := lcSource.ColorBelow;
    MaxValue := lcSource.MaxValue;
    MinValue := lcSource.MinValue;
  end;
end;

end.

