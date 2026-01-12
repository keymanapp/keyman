(*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2025-09-01
 *
 * Utility class to simplify form validation
 *)
unit Keyman.Developer.UI.FormValidation;

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.StdCtrls;

type
  TValidationMethod = record
    Method: TFunc<Boolean>;
    Message: string;
  end;

  TValidationField = record
    CaptionLabel: TLabel;
    ValidationLabel: TLabel;
    Control: TControl;
    Methods: TArray<TValidationMethod>;
    ControlTouched: Boolean;
  end;

  TValidationFields = TArray<TValidationField>;

  TFormValidation = class
  private
    Fields: TValidationFields;
  public
    procedure Add(CaptionLabel, ValidationLabel: TLabel; Control: TControl);
    procedure AddMethod(control: TControl; method: TFunc<Boolean>;
      message: string);
    function Find(control: TControl): Integer;
    procedure TouchField(ChangingControl: TControl); public
    procedure TouchAllFields;
    function Update: Boolean;
  end;

implementation

procedure TFormValidation.TouchAllFields;
var
  i: Integer;
begin
  for i := 0 to High(Fields) do
    Fields[i].ControlTouched := True;
  Update;
end;

function TFormValidation.Update: Boolean;
var
  f: TValidationField;
  m: TValidationMethod;
  n: Integer;
begin
  Result := True;

  for f in Fields do
  begin
    f.ValidationLabel.Caption := '';
    f.CaptionLabel.ParentFont := True;

    for m in f.Methods do
    begin
      if not m.Method() then
      begin
        if f.ControlTouched then
        begin
          f.ValidationLabel.Caption := m.Message;
          f.CaptionLabel.Font.Color := clRed;
        end;

        Result := False;
        Break;  // Still report on following fields
      end;
    end;
  end;
end;

function TFormValidation.Find(control: TControl): Integer;
begin
  for Result := 0 to High(Fields) do
    if Fields[Result].Control = control then
      Exit;
  Result := -1;
end;

procedure TFormValidation.Add(CaptionLabel, ValidationLabel: TLabel; Control: TControl);
var
  n: Integer;
begin
  n := Length(Fields);
  SetLength(Fields, n+1);
  Fields[n].CaptionLabel    := CaptionLabel;
  Fields[n].ValidationLabel := ValidationLabel;
  Fields[n].Control         := Control;
  SetLength(Fields[n].Methods, 0);
end;

procedure TFormValidation.AddMethod(control: TControl; method: TFunc<Boolean>; message: string);
var
  m, n: Integer;
begin
  n := Find(control);
  if n < 0 then
    raise Exception.Create('Could not find control '+control.Name);
  m := Length(Fields[n].Methods);
  SetLength(Fields[n].Methods, m + 1);
  Fields[n].Methods[m].Method := method;
  Fields[n].Methods[m].Message := message;
end;

procedure TFormValidation.TouchField(ChangingControl: TControl);
var
  n: Integer;
begin
  n := Find(ChangingControl);
  if n < 0 then
    raise Exception.Create('Could not find control '+ChangingControl.Name);
  Fields[n].ControlTouched := True;
end;

end.
