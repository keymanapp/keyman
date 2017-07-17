{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudickar att oratool dott de]
All Rights Reserved.

Contributor(s):
Jens Fudickar [jens dott fudickar att oratool dott de]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDynControlEngine;

{$I jvcl.inc}

interface

uses
  Classes, Controls, Forms, StdCtrls, Graphics, Buttons,
  JvDynControlEngineIntf;

type
  TJvDynControlType =
    (jctLabel, jctStaticText, jctPanel, jctScrollBox,
    jctEdit, jctCheckBox, jctComboBox, jctGroupBox, jctImage, jctRadioGroup,
    jctMemo, jctListBox, jctCheckListBox, jctDateTimeEdit, jctDateEdit, jctTimeEdit,
    jctCalculateEdit, jctSpinEdit, jctDirectoryEdit, jctFileNameEdit,
    jctButton, jctButtonEdit, jctForm,
    jctDBEdit, jctDBText, jctDBListBox, jctDBCheckBox, jctDBComboBox, jctDBImage, jctDBRadioGroup,
    jctDBMemo, jctDBDateTimeEdit, jctDBDateEdit, jctDBTimeEdit,
    jctDBCalculateEdit, jctDBSpinEdit, jctDBDirectoryEdit, jctDBFileNameEdit, jctDBGrid,
    jctDBButtonEdit, jctDBNavigator);

  TControlClass = class of TControl;

  TJvAfterCreateControl = procedure(AControl: TControl) of object;

  TJvCustomDynControlEngine = class(TPersistent)
  private
    FRegisteredControlTypes: array [TJvDynControlType] of TControlClass;
    FRegisterControlsExecuted: Boolean;
    FAfterCreateControl: TJvAfterCreateControl;
    function GetPropName(Instance: TPersistent; Index: Integer): string;
    function GetPropCount(Instance: TPersistent): Integer;
  protected
    procedure SetPropertyValue(const APersistent: TPersistent; const APropertyName: string; const AValue: Variant);
    function GetPropertyValue(const APersistent: TPersistent; const APropertyName: string): Variant;
    procedure AfterCreateControl(AControl: TControl); virtual;
    procedure NeedRegisterControls;
    procedure RegisterControls; virtual;
  public
    constructor create; virtual;
    function CreateControl(AControlType: TJvDynControlType; AOwner: TComponent;
      AParentControl: TWinControl; AControlName: string): TControl; virtual;
    function CreateControlClass(AControlClass: TControlClass; AOwner: TComponent;
      AParentControl: TWinControl; AControlName: string): TControl; virtual;

    function IsControlTypeRegistered(const ADynControlType: TJvDynControlType): Boolean;

    function IsControlTypeValid (const ADynControlType: TJvDynControlType;
      AControlClass: TControlClass) : Boolean; virtual;
    procedure RegisterControlType(const ADynControlType: TJvDynControlType;
      AControlClass: TControlClass); virtual;

    procedure SetControlCaption(AControl: IJvDynControl; const Value: string);
    procedure SetControlTabOrder(AControl: IJvDynControl; Value: Integer);

    procedure SetControlOnEnter(AControl: IJvDynControl; Value: TNotifyEvent);
    procedure SetControlOnExit(AControl: IJvDynControl; Value: TNotifyEvent);
    procedure SetControlOnClick(AControl: IJvDynControl; Value: TNotifyEvent);
  published
    property OnAfterCreateControl: TJvAfterCreateControl read FAfterCreateControl write FAfterCreateControl;
  end;

  TJvDynControlEngine = class(TJvCustomDynControlEngine)
  private
  protected
  public
    function CreateLabelControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName, ACaption: string; AFocusControl: TWinControl): TControl; virtual;
    function CreateStaticTextControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName, ACaption: string): TWinControl; virtual;
    function CreatePanelControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName, ACaption: string; AAlign: TAlign): TWinControl; virtual;
    function CreateScrollBoxControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateEditControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateCheckboxControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName, ACaption: string): TWinControl; virtual;
    function CreateComboBoxControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; AItems: TStrings): TWinControl; virtual;
    function CreateGroupBoxControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName, ACaption: string): TWinControl; virtual;
    function CreateImageControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateRadioGroupControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName, ACaption: string; AItems: TStrings;
      AItemIndex: Integer = 0): TWinControl; virtual;
    function CreateMemoControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateListBoxControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; AItems: TStrings): TWinControl; virtual;
    function CreateCheckListBoxControl(AOwner: TComponent;
      AParentControl: TWinControl; const AControlName: string; AItems: TStrings): TWinControl;
    function CreateDateTimeControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateDateControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateTimeControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateCalculateControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateSpinControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateDirectoryControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateFileNameControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateButton(AOwner: TComponent; AParentControl: TWinControl;
      const AButtonName, ACaption, AHint: string;
      AOnClick: TNotifyEvent; ADefault: Boolean = False;
      ACancel: Boolean = False): TButton; virtual;
    function CreateButtonEditControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; AOnButtonClick: TNotifyEvent): TWinControl; virtual;
    function CreateForm(const ACaption, AHint: string): TCustomForm; virtual;

    function CreateLabelControlPanel (AOwner: TComponent; AParentControl: TWinControl;
      const AControlName, ACaption: string; AFocusControl: TWinControl;
      ALabelOnTop : Boolean = True; ALabelDefaultWidth : Integer = 0): TWinControl; virtual;

  published
  end;


{$IFNDEF COMPILER6_UP}
function Supports(Instance: TObject; const Intf: TGUID): Boolean; overload;
function Supports(AClass: TClass; const Intf: TGUID): Boolean; overload;
{$ENDIF COMPILER6_UP}

function IntfCast(Instance: TObject; const Intf: TGUID): IUnknown;

procedure SetDefaultDynControlEngine(AEngine: TJvDynControlEngine);
function DefaultDynControlEngine: TJvDynControlEngine;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  SysUtils, TypInfo,
  JvResources, JvTypes, JvDynControlEngineVCL;

var
  GlobalDefaultDynControlEngine: TJvDynControlEngine = nil;

{$IFNDEF COMPILER6_UP}

function Supports(Instance: TObject; const Intf: TGUID): Boolean;
begin
  Result := Instance.GetInterfaceEntry(Intf) <> nil;
end;

function Supports(AClass: TClass; const Intf: TGUID): Boolean;
begin
  Result := AClass.GetInterfaceEntry(Intf) <> nil;
end;

{$ENDIF !COMPILER6_UP}

function IntfCast(Instance: TObject; const Intf: TGUID): IUnknown;
begin
  if not Supports(Instance, Intf, Result) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
end;

constructor TJvCustomDynControlEngine.create;
begin
  inherited create;
end;


function TJvCustomDynControlEngine.IsControlTypeRegistered(const ADynControlType: TJvDynControlType): Boolean;
begin
  NeedRegisterControls;
  Result := Assigned(FRegisteredControlTypes[ADynControlType]);
end;



function TJvCustomDynControlEngine.IsControlTypeValid (const ADynControlType: TJvDynControlType;
      AControlClass: TControlClass) : Boolean;
var
  Valid: Boolean;
begin
  Valid := Supports(AControlClass, IJvDynControl);
  case ADynControlType of
    jctButton:
      Valid := Valid and Supports(AControlClass, IJvDynControlButton);
    jctButtonEdit:
      Valid := Valid and Supports(AControlClass, IJvDynControlButton)
                     and Supports(AControlClass, IJvDynControlData);
    jctPanel:
      Valid := Valid and Supports(AControlClass, IJvDynControlPanel);
    jctLabel:
      Valid := Valid and Supports(AControlClass, IJvDynControlLabel);
    jctMemo:
      Valid := Valid and
        Supports(AControlClass, IJvDynControlItems) and
        Supports(AControlClass, IJvDynControlData) and
        Supports(AControlClass, IJvDynControlMemo);
    jctRadioGroup, jctComboBox:
      Valid := Valid and
        Supports(AControlClass, IJvDynControlItems) and
        Supports(AControlClass, IJvDynControlData);
    jctEdit, jctCalculateEdit, jctSpinEdit, jctFileNameEdit, jctDirectoryEdit,
      jctCheckBox, jctDateTimeEdit, jctDateEdit, jctTimeEdit:
      Valid := Valid and Supports(AControlClass, IJvDynControlData);
  end;
  Result := Valid;
end;

procedure TJvCustomDynControlEngine.RegisterControlType(const ADynControlType: TJvDynControlType;
  AControlClass: TControlClass);
begin
  NeedRegisterControls;
  FRegisteredControlTypes[ADynControlType] := nil;
  if IsControlTypeValid (ADynControlType, AControlClass) then
    FRegisteredControlTypes[ADynControlType] := AControlClass
  else
    raise EJVCLException.CreateRes(@RsEUnsupportedControlClass);
end;

function TJvCustomDynControlEngine.GetPropCount(Instance: TPersistent): Integer;
var
  Data: PTypeData;
begin
  Data := GetTypeData(Instance.ClassInfo);
  Result := Data^.PropCount;
end;

function TJvCustomDynControlEngine.GetPropName(Instance: TPersistent; Index: Integer): string;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  Data: PTypeData;
begin
  Result := '';
  Data := GetTypeData(Instance.ClassInfo);
  GetMem(PropList, Data^.PropCount * SizeOf(PPropInfo));
  try
    GetPropInfos(Instance.ClassInfo, PropList);
    PropInfo := PropList^[Index];
    Result := PropInfo^.Name;
  finally
    FreeMem(PropList, Data^.PropCount * SizeOf(PPropInfo));
  end;
end;

procedure TJvCustomDynControlEngine.SetPropertyValue(const APersistent: TPersistent;
  const APropertyName: string; const AValue: Variant);
var
  Index: Integer;
  PropName: string;
  SubObj: TObject;
  P: Integer;
  SearchName: string;
  LastName: string;
begin
  SearchName := Trim(APropertyName);
  P := Pos('.', SearchName);
  if P > 0 then
  begin
    LastName := Trim(Copy(SearchName, P + 1, Length(SearchName) - P));
    SearchName := Trim(Copy(SearchName, 1, P - 1));
  end
  else
    LastName := '';
  for Index := 0 to GetPropCount(APersistent) - 1 do
  begin
    PropName := UpperCase(GetPropName(APersistent, Index));
    if UpperCase(SearchName) = PropName then
      case PropType(APersistent, PropName) of
        tkLString, tkWString, tkString:
          SetStrProp(APersistent, PropName, VarToStr(AValue));
        tkEnumeration, tkSet, tkChar, tkInteger:
          SetOrdProp(APersistent, PropName, AValue);
//        tkInt64:
//          SetInt64Prop(APersistent, PropName, AValue);
        tkFloat:
          SetFloatProp(APersistent, PropName, AValue);
        tkClass:
          begin
            SubObj := GetObjectProp(APersistent, PropName);
            if SubObj is TStrings then
              TStrings(SubObj).Text := AValue
            else
            if (SubObj is TPersistent) and (LastName <> '') then
              SetPropertyValue(TPersistent(SubObj), LastName, AValue);
          end;
      end;
  end;
end;

function TJvCustomDynControlEngine.GetPropertyValue(const APersistent: TPersistent;
  const APropertyName: string): Variant;
var
  Index: Integer;
  PropName: string;
  SubObj: TObject;
  P: Integer;
  SearchName: string;
  LastName: string;
begin
  SearchName := Trim(APropertyName);
  P := Pos('.', SearchName);
  if P > 0 then
  begin
    LastName := Trim(Copy(SearchName, P + 1, Length(SearchName) - P));
    SearchName := Trim(Copy(SearchName, 1, P - 1));
  end
  else
    LastName := '';
  for Index := 0 to GetPropCount(APersistent) - 1 do
  begin
    PropName := UpperCase(GetPropName(APersistent, Index));
    if UpperCase(SearchName) = PropName then
      case PropType(APersistent, PropName) of
        tkLString, tkWString, tkString:
          Result := GetStrProp(APersistent, PropName);
        tkEnumeration, tkSet, tkChar, tkInteger:
          Result := GetOrdProp(APersistent, PropName);
        tkInt64:
          {$IFDEF COMPILER6_UP}
          Result := GetInt64Prop(APersistent, PropName);
          {$ELSE}
          Result := Null;
          {$ENDIF COMPILER6_UP}
        tkFloat:
          Result := GetFloatProp(APersistent, PropName);
        tkClass:
          begin
            SubObj := GetObjectProp(APersistent, PropName);
            if SubObj is TStrings then
              Result := TStrings(SubObj).Text
            else
            if (SubObj is TPersistent) and (LastName <> '') then
              Result := GetPropertyValue(TPersistent(SubObj), LastName);
          end;
      end;
  end;
end;

procedure TJvCustomDynControlEngine.AfterCreateControl(AControl: TControl);
begin
  if Assigned(FAfterCreateControl) then
    FAfterCreateControl(AControl);
end;

function TJvCustomDynControlEngine.CreateControl(AControlType: TJvDynControlType;
  AOwner: TComponent; AParentControl: TWinControl; AControlName: string): TControl;
begin
  NeedRegisterControls;
  if Assigned(FRegisteredControlTypes[AControlType]) then
    Result := CreateControlClass(FRegisteredControlTypes[AControlType], AOwner,
      AParentControl, AControlName)
  else
  if AControlType = jctForm then
  begin
    Result := TControl(TForm.Create(AOwner));
    if AControlName <> '' then
      Result.Name := AControlName;
  end
  else
    Result := nil;
  if Result = nil then
    raise EJVCLException.CreateRes(@RsENoRegisteredControlClass);
  AfterCreateControl(Result);
end;

function TJvCustomDynControlEngine.CreateControlClass(AControlClass: TControlClass;
  AOwner: TComponent; AParentControl: TWinControl; AControlName: string): TControl;
var
  DynCtrl: IJvDynControl;
begin
  Result := TControl(AControlClass.Create(AOwner));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrl.ControlSetDefaultProperties;
  if Assigned(AParentControl) then
    Result.Parent := AParentControl;
  if AControlName <> '' then
    Result.Name := AControlName;
end;

procedure TJvCustomDynControlEngine.SetControlCaption(AControl: IJvDynControl; const Value: string);
begin
end;

procedure TJvCustomDynControlEngine.SetControlTabOrder(AControl: IJvDynControl; Value: Integer);
begin
end;

procedure TJvCustomDynControlEngine.SetControlOnEnter(AControl: IJvDynControl; Value: TNotifyEvent);
begin
end;

procedure TJvCustomDynControlEngine.SetControlOnExit(AControl: IJvDynControl; Value: TNotifyEvent);
begin
end;

procedure TJvCustomDynControlEngine.SetControlOnClick(AControl: IJvDynControl; Value: TNotifyEvent);
begin
end;

procedure TJvCustomDynControlEngine.NeedRegisterControls;
begin
  if not FRegisterControlsExecuted then
  begin
    FRegisterControlsExecuted := True;
    RegisterControls;
  end;
end;

procedure TJvCustomDynControlEngine.RegisterControls;
begin
  // no registration
end;



function TJvDynControlEngine.CreateLabelControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName, ACaption: string;
  AFocusControl: TWinControl): TControl;
var
  DynCtrl: IJvDynControl;
  DynCtrlLabel: IJvDynControlLabel;
begin
  Result := CreateControl(jctLabel, AOwner, AParentControl, AControlName);
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrl.ControlSetCaption(ACaption);
  if not Supports(Result, IJvDynControlLabel, DynCtrlLabel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrlLabel.ControlSetFocusControl(AFocusControl);
end;

function TJvDynControlEngine.CreateStaticTextControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName, ACaption: string): TWinControl;
var
  DynCtrl: IJvDynControl;
begin
  Result := TWinControl(CreateControl(jctStaticText, AOwner, AParentControl, AControlName));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrl.ControlSetCaption(ACaption);
end;

function TJvDynControlEngine.CreatePanelControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName, ACaption: string;
  AAlign: TAlign): TWinControl;
var
  DynCtrl: IJvDynControl;
begin
  Result := TWinControl(CreateControl(jctPanel, AOwner, AParentControl, AControlName));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrl.ControlSetCaption(ACaption);
  Result.Align := AAlign;
end;

function TJvDynControlEngine.CreateScrollBoxControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctScrollBox, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateEditControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
var
  DynCtrlEdit: IJvDynControlEdit;
begin
  Result := TWinControl(CreateControl(jctEdit, AOwner, AParentControl, AControlName));
  if not Supports(Result, IJvDynControlEdit, DynCtrlEdit) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
end;

function TJvDynControlEngine.CreateCheckboxControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName, ACaption: string): TWinControl;
var
  DynCtrl: IJvDynControl;
begin
  Result := TWinControl(CreateControl(jctCheckBox, AOwner, AParentControl, AControlName));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrl.ControlSetCaption(ACaption);
end;

function TJvDynControlEngine.CreateComboBoxControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; AItems: TStrings): TWinControl;
var
  DynCtrlItems: IJvDynControlItems;
begin
  Result := TWinControl(CreateControl(jctComboBox, AOwner, AParentControl, AControlName));
  if not Supports(Result, IJvDynControlItems, DynCtrlItems) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrlItems.ControlSetItems(AItems);
end;

function TJvDynControlEngine.CreateGroupBoxControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName, ACaption: string): TWinControl;
var
  DynCtrl: IJvDynControl;
begin
  Result := TWinControl(CreateControl(jctGroupBox, AOwner, AParentControl, AControlName));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrl.ControlSetCaption(ACaption);
end;

function TJvDynControlEngine.CreateImageControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctImage, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateRadioGroupControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName, ACaption: string;
  AItems: TStrings; AItemIndex: Integer = 0): TWinControl;
var
  DynCtrl: IJvDynControl;
  DynCtrlItems: IJvDynControlItems;
  DynCtrlData: IJvDynControlData;
begin
  Result := TWinControl(CreateControl(jctRadioGroup, AOwner, AParentControl, AControlName));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrl.ControlSetCaption(ACaption);
  if not Supports(Result, IJvDynControlItems, DynCtrlItems) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrlItems.ControlSetItems(AItems);
  if not Supports(Result, IJvDynControlData, DynCtrlData) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrlData.ControlValue := AItemIndex;
end;

function TJvDynControlEngine.CreateMemoControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctMemo, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateListBoxControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; AItems: TStrings): TWinControl;
var
  DynCtrlItems: IJvDynControlItems;
begin
  Result := TWinControl(CreateControl(jctListBox, AOwner, AParentControl, AControlName));
  if not Supports(Result, IJvDynControlItems, DynCtrlItems) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrlItems.ControlSetItems(AItems);
end;

function TJvDynControlEngine.CreateCheckListBoxControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; AItems: TStrings): TWinControl;
var
  DynCtrlItems: IJvDynControlItems;
begin
  Result := TWinControl(CreateControl(jctCheckListBox, AOwner, AParentControl, AControlName));
  if not Supports(Result, IJvDynControlItems, DynCtrlItems) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrlItems.ControlSetItems(AItems);
end;

function TJvDynControlEngine.CreateDateTimeControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctDateTimeEdit, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateDateControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctDateEdit, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateTimeControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctTimeEdit, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateCalculateControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctCalculateEdit, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateSpinControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctSpinEdit, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateDirectoryControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctDirectoryEdit, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateFileNameControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctFileNameEdit, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateButton(AOwner: TComponent;
  AParentControl: TWinControl; const AButtonName, ACaption, AHint: string;
  AOnClick: TNotifyEvent; ADefault: Boolean = False; ACancel: Boolean = False): TButton;
begin
  Result := TButton(CreateControl(jctButton, AOwner, AParentControl, AButtonName));
  Result.Hint := AHint;
  Result.Caption := ACaption;
  Result.Default := ADefault;
  Result.Cancel := ACancel;
  Result.OnClick := AOnClick;
end;

function TJvDynControlEngine.CreateButtonEditControl(AOwner: TComponent; AParentControl: TWinControl;
  const AControlName: string; AOnButtonClick: TNotifyEvent): TWinControl;
var
  DynCtrlButtonEdit: IJvDynControlButtonEdit;
begin
  Result := TWinControl(CreateControl(jctButtonEdit, AOwner, AParentControl, AControlName));
  if not Supports(Result, IJvDynControlButtonEdit, DynCtrlButtonEdit) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrlButtonEdit.ControlSetOnButtonClick(AOnButtonClick);
end;

function TJvDynControlEngine.CreateForm(const ACaption, AHint: string): TCustomForm;
begin
  Result := TCustomForm(CreateControl(jctForm, Application, nil, ''));
  Result.Caption := ACaption;
  Result.Hint := AHint;
end;

function TJvDynControlEngine.CreateLabelControlPanel (AOwner: TComponent; AParentControl: TWinControl;
      const AControlName, ACaption: string; AFocusControl: TWinControl; ALabelOnTop : Boolean = True; ALabelDefaultWidth : Integer = 0): TWinControl;
Var
  Panel : TWinControl;
  LabelControl : TControl;
begin
  if not Assigned(AFocusControl) then
    raise Exception.Create('TJvDynControlEngine.CreateLabelControlPanel : AFocusControl must be assigned');
  Panel := CreatePanelControl (AOwner, AParentControl, '', '', alNone);
  LabelControl := CreateLabelControl (AOwner, Panel, '', ACaption, AFocusControl);
//  LabelControl.Width := panel.Canvas.
  AFocusControl.Parent := Panel;
  LabelControl.Top := 1;
  LabelControl.Left := 1;
  if ALabelOnTop then
  begin
    AFocusControl.Top := LabelControl.Height+1;
    AFocusControl.Left := 1;
    if LabelControl.Width > AFocusControl.Width then
      Panel.Width := LabelControl.Width
    else
      Panel.Width := AFocusControl.Width;
    Panel.Height := AFocusControl.Height+LabelControl.Height;
  end
  else
  begin
    if ALabelDefaultWidth > 0 then
     LabelControl.Width := ALabelDefaultWidth;
    AFocusControl.Left := LabelControl.Width+1;
    AFocusControl.Top := 1;
    if LabelControl.Height > AFocusControl.Height then
      Panel.Height := LabelControl.Height
    else
      Panel.Height := AFocusControl.Height;
    Panel.Width := AFocusControl.Width+LabelControl.Width;
  end;
  Panel.Width := Panel.Width + 1;
  Panel.Height := Panel.Height + 1;
  Result := Panel;
end;


procedure SetDefaultDynControlEngine(AEngine: TJvDynControlEngine);
begin
  if AEngine is TJvDynControlEngine then
    GlobalDefaultDynControlEngine := AEngine;
end;

function DefaultDynControlEngine: TJvDynControlEngine;
begin
  Result := GlobalDefaultDynControlEngine;
end;



{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

