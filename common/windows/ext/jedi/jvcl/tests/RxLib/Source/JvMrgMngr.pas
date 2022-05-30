{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMrgMngr.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvMrgMngr;

interface

uses Classes, Controls, Forms, JvVCLUtils{, JvComponent};

type
  TFormRequestEvent = procedure(Sender: TObject; CurrentForm: TCustomForm;
    var NewForm: TCustomForm) of object;
  TFormReorderEvent = procedure(Sender: TObject;
    Activated, Deactivated: TCustomForm) of object;
  TJvFormHistory = class;
  TFormHistoryCommand = (hcNone, hcAdd, hcBack, hcForward, hcGoto);

{ TJvMergeManager }

  TJvMergeManager = class(TComponent)
  private
    FMergeFrame: TWinControl;
    FFormHistory: TJvFormHistory;
    FHistoryCommand: TFormHistoryCommand;
    FOnGetBackForm: TFormRequestEvent;
    FOnGetForwardForm: TFormRequestEvent;
    FOnChange: TNotifyEvent;
    FOnReorder: TFormReorderEvent;
    function IsForm: Boolean;
    function NotIsForm: Boolean;
    procedure ReadForm(Reader: TReader);
    procedure WriteForm(Writer: TWriter);
    procedure SetMergeFrame(Value: TWinControl);
    function GetActiveForm: TCustomForm;
    procedure SetActiveForm(Value: TCustomForm);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetBackForm: TCustomForm; virtual;
    function GetForwardForm: TCustomForm; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure DoChange; dynamic;
    procedure DoReorder(Deactivated: TCustomForm); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Merge(AForm: TCustomForm; Show: Boolean);
    function GotoForm(AForm: TCustomForm): Boolean;
    function GotoFormClass(AFormClass: TFormClass): Boolean;
    procedure GoBack;
    procedure GoForward;
    procedure GotoHistoryIndex(HistoryIndex: Integer);
    property FormHistory: TJvFormHistory read FFormHistory;
    property ActiveForm: TCustomForm read GetActiveForm write SetActiveForm;
    property HistoryCommand: TFormHistoryCommand read FHistoryCommand
      write FHistoryCommand;
  published
    property MergeFrame: TWinControl read FMergeFrame write SetMergeFrame
      stored NotIsForm;
    property OnGetBackForm: TFormRequestEvent read FOnGetBackForm
      write FOnGetBackForm;
    property OnGetForwardForm: TFormRequestEvent read FOnGetForwardForm
      write FOnGetForwardForm;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnReorder: TFormReorderEvent read FOnReorder write FOnReorder;
  end;

{ TJvFormHistory }

  TJvFormHistory = class(TList)
  private
    FCurrent: Integer;
    FHistoryCapacity: Integer;
    procedure SetCurrent(Value: Integer);
    procedure SetHistoryCapacity(Value: Integer);
    function GetForm(Index: Integer): TCustomForm;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddForm(AForm: TCustomForm);
    procedure DeleteHistoryItem(Index: Integer);
    function RemoveItem(Item: TComponent): Boolean;
    procedure ResetHistory;
    property Current: Integer read FCurrent write SetCurrent;
    property HistoryCapacity: Integer read FHistoryCapacity
      write SetHistoryCapacity;
    property Forms[Index: Integer]: TCustomForm read GetForm;
  end;

implementation

{ TJvMergeManager }

constructor TJvMergeManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFormHistory := TJvFormHistory.Create;
  FHistoryCommand := hcAdd;
end;

destructor TJvMergeManager.Destroy;
begin
  FFormHistory.Free;
  inherited Destroy;
end;

function TJvMergeManager.NotIsForm: Boolean;
begin
  Result := (MergeFrame <> nil) and not (MergeFrame is TCustomForm);
end;

function TJvMergeManager.IsForm: Boolean;
begin
  Result := (MergeFrame <> nil) and ((MergeFrame = Owner) and
    (Owner is TCustomForm));
end;

procedure TJvMergeManager.ReadForm(Reader: TReader);
begin
  if Reader.ReadBoolean then
    if Owner is TCustomForm then MergeFrame := TWinControl(Owner);
end;

procedure TJvMergeManager.WriteForm(Writer: TWriter);
begin
  Writer.WriteBoolean(IsForm);
end;

procedure TJvMergeManager.DefineProperties(Filer: TFiler);
{$IFDEF WIN32}
  function DoWrite: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := IsForm <> TJvMergeManager(Filer.Ancestor).IsForm
    else Result := IsForm;
  end;
{$ENDIF}
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('IsForm', ReadForm, WriteForm,
    {$IFDEF WIN32} DoWrite {$ELSE} IsForm {$ENDIF});
end;

procedure TJvMergeManager.SetMergeFrame(Value: TWinControl);
begin
  if FMergeFrame <> Value then begin
    FMergeFrame := Value;
{$IFDEF WIN32}
    if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
    FFormHistory.ResetHistory;
  end;
end;

function TJvMergeManager.GetActiveForm: TCustomForm;
var
  I: Integer;
begin
  if (MergeFrame <> nil) and (MergeFrame.ControlCount > 0) then begin
    for I := MergeFrame.ControlCount - 1 downto 0 do begin
      if MergeFrame.Controls[I] is TCustomForm then begin
        Result := TCustomForm(MergeFrame.Controls[I]);
        Exit;
      end;
    end;
  end;
  Result := nil;
end;

procedure TJvMergeManager.SetActiveForm(Value: TCustomForm);
begin
  GotoForm(Value);
end;

function TJvMergeManager.GetBackForm: TCustomForm;
begin
  if FormHistory.Current < 1 then
    Result := nil
  else
    Result := FormHistory.Forms[FormHistory.Current - 1];
  if Assigned(FOnGetBackForm) then FOnGetBackForm(Self, ActiveForm, Result);
end;

function TJvMergeManager.GetForwardForm: TCustomForm;
begin
  if FormHistory.Current >= FormHistory.Count - 1 then
    Result := nil
  else
    Result := FormHistory.Forms[FormHistory.Current + 1];
  if Assigned(FOnGetForwardForm) then FOnGetForwardForm(Self, ActiveForm, Result);
end;

procedure TJvMergeManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then begin
    if AComponent = MergeFrame then MergeFrame := nil;
    if FormHistory.RemoveItem(AComponent) then DoChange;
  end;
end;

procedure TJvMergeManager.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TJvMergeManager.DoReorder(Deactivated: TCustomForm);
begin
  if Assigned(FOnReorder) then FOnReorder(Self, ActiveForm, Deactivated);
end;

procedure TJvMergeManager.Merge(AForm: TCustomForm; Show: Boolean);
begin
  MergeForm(MergeFrame, TForm(AForm), alClient, Show);
  GotoForm(AForm);
end;

function TJvMergeManager.GotoForm(AForm: TCustomForm): Boolean;
var
  I: Integer;
  OldActiveForm: TCustomForm;
begin
  Result := False;
  OldActiveForm := ActiveForm;
  if MergeFrame = nil then Exit;
  for I := 0 to MergeFrame.ControlCount - 1 do begin
    if MergeFrame.Controls[I] = AForm then begin
      AForm.BringToFront;
      case HistoryCommand of
        hcNone: ;
        hcAdd: FormHistory.AddForm(AForm);
        hcBack: FormHistory.Current := FormHistory.Current - 1;
        hcForward: FormHistory.Current := FormHistory.Current + 1;
        hcGoto: ;
      end;
      HistoryCommand := hcAdd;
      DoReorder(OldActiveForm);
      DoChange;
      Result := True;
      Exit;
    end;
  end;
end;

function TJvMergeManager.GotoFormClass(AFormClass: TFormClass): Boolean;
var
  I: Integer;
begin
  Result := False;
  if MergeFrame = nil then Exit;
  for I := 0 to MergeFrame.ControlCount - 1 do begin
    if MergeFrame.Controls[I] is AFormClass then begin
      Result := GotoForm(MergeFrame.Controls[I] as TCustomForm);     
      Exit;
    end;
  end;
end;

procedure TJvMergeManager.GoBack;
begin
  HistoryCommand := hcBack;
  GotoForm(GetBackForm);
end;

procedure TJvMergeManager.GoForward;
begin
  HistoryCommand := hcForward;
  GotoForm(GetForwardForm);
end;

procedure TJvMergeManager.GotoHistoryIndex(HistoryIndex: Integer);
var
  SaveCurrent: Integer;
begin
  SaveCurrent := FormHistory.Current;
  FormHistory.Current := HistoryIndex;
  try
    HistoryCommand := hcGoto;
    GotoForm(FormHistory.Forms[HistoryIndex]);
  finally
    if ActiveForm <> FormHistory.Forms[HistoryIndex] then
      FormHistory.Current := SaveCurrent;
  end;
end;

{ TJvFormHistory }

constructor TJvFormHistory.Create;
begin
  inherited Create;
  FCurrent := -1;
  FHistoryCapacity := 10;
end;

destructor TJvFormHistory.Destroy;
begin
  inherited Destroy;
end;

procedure TJvFormHistory.SetCurrent(Value: Integer);
begin
  if Value < 0 then Value := -1;
  if Value > Count - 1 then Value := Count - 1;
  if FCurrent <> Value then begin
    FCurrent := Value;
  end;
end;

procedure TJvFormHistory.SetHistoryCapacity(Value: Integer);
var
  I: Integer;
begin
  if Value < FHistoryCapacity then begin
    for I := 0 to Count - Value do begin
      RemoveItem(Forms[0]);
    end;
  end;
  FHistoryCapacity := Value;
end;

function TJvFormHistory.GetForm(Index: Integer): TCustomForm;
begin
  Result := TCustomForm(Items[Index]);
end;

procedure TJvFormHistory.AddForm(AForm: TCustomForm);
var
  I: Integer;
begin
  for I := Count - 1 downto Current + 1 do begin
    DeleteHistoryItem(I);
  end;
  for I := 0 to Count - HistoryCapacity do begin
    DeleteHistoryItem(0);
  end;
  if Count < HistoryCapacity then begin
    Add(AForm);
  end;
  Current := Count - 1;
end;

procedure TJvFormHistory.DeleteHistoryItem(Index: Integer);
begin
  if (Index >= 0) and (Index < Count) then begin
    Delete(Index);
    if Current > Count - 1 then Current := Count - 1;
  end;
end;

function TJvFormHistory.RemoveItem(Item: TComponent): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Count - 1 downto 0 do begin
    if Items[I] = Item then begin
      DeleteHistoryItem(I);
      Result := True;
    end;
  end;
end;

procedure TJvFormHistory.ResetHistory;
begin
  Clear;
  Current := -1;
end;

end.
