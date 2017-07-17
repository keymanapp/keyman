{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBDateTimePicker.PAS, released May 8, 2000

The Initial Developer of the Original Code is Eko Subagio (ekosbg@bigfoot.com)
Portions created by Eko Subagio are Copyright (C) 2000 Eko Subagio.
Portions created by Microsoft are Copyright (C) 1998, 1999 Microsoft Corp.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: September 9, 2000
Last Modified: October 18, 2000 Add Handler Event OnCloseUp and OnDropDown
               by Eko Subagio
Current Version: 1.00

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  (rom) commnts should be ripped by the help writer
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDBDateTimePicker;

/////////////////////////////////////////////////////////////////////////
// TJvDBDateTimePicker
// Copyright(c)2000 Eko Subagio
// TJvDBDateTimePicker is derived from TDateTimePicker from Delphi 5
// TDateTimePicker Copyright(c) 2000 Borland/Inprise.
// Extending and add capability to integrate with database
// www.geocities.com/ekosbg
/////////////////////////////////////////////////////////////////////////

interface

uses
  Windows, SysUtils, Classes, Controls,
  ComCtrls, DB, DBCtrls,
  JVCLVer, JvDateTimePicker;

type
  TJvDBDateTimePicker = class(TJvDateTimePicker)
  private
    FDataLink: TFieldDataLink;
    FAboutJVCL: TJVCLAboutInfo;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(Value: string);
    procedure SetDataSource(Value: TDataSource);
    // Adding capability to edit
    procedure CMExit(var Msg: TCMExit); message CM_EXIT;
  protected
    procedure DataChange(Sender: TObject);
    // Adding capability to edit
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Change; override;
    procedure UpdateData(Sender: TObject);
    // On Close Up & Drop Down
    procedure CalendarOnCloseUp(Sender: TObject);
    procedure CalendarOnDropDown(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

implementation

{$IFDEF COMPILER6_UP} 
uses 
  Variants; 
{$ENDIF} 

///////////////////////////////////////////////////////////////////////////
//constructor TJvDBDateTimePicker.Create
//Parameter   : AOwner as TComponent
//Description : As Constructor the procedure had have responsibility to
//              handle new instance for initial new value.
//Revision    : August 30, 2000
//Author      : -ekosbg-
///////////////////////////////////////////////////////////////////////////

constructor TJvDBDateTimePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  OnCloseUp := CalendarOnCloseUp;
  OnDropDown := CalendarOnDropDown;
end;

///////////////////////////////////////////////////////////////////////////
//Destructor TJvDBDateTimePicker.Destroy
//Parameter   : None
//Description : Destructor had have responsibility to destroy all garbage
//              that had been used in Constructor, free anything in here
//              after anything is initialized in Constructor
//Revision    : August 30, 2000
//Author      : -ekosbg-
///////////////////////////////////////////////////////////////////////////

destructor TJvDBDateTimePicker.Destroy;
begin
  OnCloseUp := nil;
  OnDropDown := nil;
  FDataLink.OnDataChange := nil;
  FDataLink.OnUpdateData := nil;
  FDataLink.Free;
  inherited Destroy;
end;

///////////////////////////////////////////////////////////////////////////
//procedure TJvDBDateTimePicker.DataChange
//Parameter   : Sender as TObject
//Description : DataChange had have responsibility to make data in control
//              always up to date with the current value in database
//              This is event handler for TFieldDataLink event property
//              OnDataChange
//Revision    : August 30, 2000
//Author      : -ekosbg-
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if Kind = dtkDate then
      Date := Trunc(FDataLink.Field.AsDateTime)
    else
      Time := Frac(FDataLink.Field.AsDateTime);
  end
  else
  if csDesigning in ComponentState then
  begin
    Date := Date;
    Time := Now;
  end;
  CheckNullValue;
end;

///////////////////////////////////////////////////////////////////////////
//function TJvDBDateTimePicker.GetDataField
//Return Value : String
//Description  : The function retrieve for fieldname from specified
//               datasource
//Revision     : August 30, 2000
//Author       : -ekosbg-
///////////////////////////////////////////////////////////////////////////

function TJvDBDateTimePicker.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

///////////////////////////////////////////////////////////////////////////
//function TJvDBDateTimePicker.GetDataSource
//Return Value : TDataSource
//Description  : The function retrieve DataSource from specified Table
//               To make connection with database
//Revision     : August 30, 2000
//Author       : -ekosbg-
///////////////////////////////////////////////////////////////////////////

function TJvDBDateTimePicker.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

///////////////////////////////////////////////////////////////////////////
//procedure TJvDBDateTimePicker.SetDataField
//Parameter    : Value as String
//Description  : The procedure is handling the capability to set the
//               DataField property
//Revision     : August 30, 2000
//Author       : -ekosbg-
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.SetDataField(Value: string);
begin
  FDataLink.FieldName := Value;
end;

///////////////////////////////////////////////////////////////////////////
//procedure TJvDBDateTimePicker.SetDataSource
//Parameter    : Value as TDataSource
//Description  : The procedure is handling the capability to set the
//               DataSource property
//Revision     : August 30, 2000
//Author       : -ekosbg-
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

///////////////////////////////////////////////////////////////////////////
//procedure TJvDBDateTimePicker.KeyDown
//Parameter   : Key as Word by references,
//              ShiftState as TShiftState, this is enumeration type
//Description : Handling user action what should to do ? The control should
//              tell to datalink that they should change mode to edit doing
//              an action such as delete, insert or...you guess it
//Revision    : August 30, 2000
//Author      : -ekosbg-
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.KeyDown(var Key: Word; Shift: TShiftState);
begin
  // we still parent code
  inherited KeyDown(Key, Shift);
  // Is it Delete key, insert key or shiftstate ...
  case Key of
    VK_DELETE:
      begin
        FDataLink.Edit;
        if Kind = dtkDate then
          Date := NullDate
        else
          Time := Frac(NullDate);
        CheckNullValue;
        UpdateData(self);
      end;
    VK_INSERT:
      if ssShift in Shift then
        FDataLink.Edit;
  end;
end;

///////////////////////////////////////////////////////////////////////////
//procedure TJvDBDateTimePicker.KeyPress
//Parameter   : Key as Char by references when the key changes it will
//              reflect to the sender parameter variable.
//Description : Handling user action what should to do ?
//              Hmmm... ok, first of all the character that user typed
//              should be checked, if it is invalid ignored the character.
//              Otherwise, tell to datalink that the mode should change
//              to edit.
//Revision    : August 30, 2000
//Author      : -ekosbg-
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and ((FDataLink.Field <> nil) and
    not (FDataLink.Field.IsValidChar(Key))) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  case Key of
    #32..#255:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SetFocus;
      end;
  end;
end;

///////////////////////////////////////////////////////////////////////////
//procedure TJvDBDateTimePicker.CMExit
//Description : User action , She/He leave the control.......
//              We should tell to database that is leave and database
//             should be updated using datalink value
//Revision    : August 30, 2000
//Author      : -ekosbg-
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.CMExit(var Msg: TCMExit);
begin
  // trapping in exception
  try
    // Changes should Reflect database
    FDataLink.UpdateRecord;
  except
    // Only got an error the focus will not leave the control
    SetFocus;
  end;
  // We needs the method behavior from parents of CMExit;
  inherited;
end;

///////////////////////////////////////////////////////////////////////////
//procedure TJvDBDateTimePicker.Change;
//Description : We should maintain the changes in TJvDBDateTimePicker to
//              datalink, in order to notify datalink that it was changed.
//Revision    : August 30, 2000
//Author      : -ekosbg-
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.Change;
begin
  // call method modified
  FDataLink.Edit;
//  FDataLink.Modified;
  // we still need parent code
  inherited Change;
  UpdateData(Self);
end;

///////////////////////////////////////////////////////////////////////////
//procedure TJvDBDateTimePicker.UpdateDate
//Parameter   :
//Description : We should change the value in datalink, and this is the
//              procedure to handle that event. It will assign with
//              event property Datalink, that is OnUpdateData
//Revision    : August 30, 2000
//Author      : -ekosbg-
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.UpdateData(Sender: TObject);
begin
  // update value in datalink with date value in control, not from system
  if not FDataLink.Editing then
    Exit;
  if Kind = dtkDate then
  begin
    if Trunc(NullDate) = Trunc(Date) then
      FDataLink.Field.Value := NULL
    else
      FDataLink.Field.AsDateTime := Trunc(Date);
  end
  else
  begin
    if Frac(NullDate) = Frac(Time) then
      FDataLink.Field.Value := NULL
    else
      FDataLink.Field.AsDateTime := Frac(Time);
  end;
end;

///////////////////////////////////////////////////////////////////////////
//procedure    : TJvDBDateTimePicker.CalendarOnCloseUp
//Parameter    : Sender as TObject
//Descriptions : To set the dataset into edit mode, when the user
//               closing up the Calendar.
//Revision     : October 18, 2000 ekosbg@bigfoot.com
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.CalendarOnCloseUp(Sender: TObject);
begin
  FDataLink.Edit;
end;

///////////////////////////////////////////////////////////////////////////
//procedure    : TJvDBDateTimePicker.CalendarOnDropDown
//Parameter    : Sender as TObject
//Descriptions : To set the dataset into edit mode, when the user
//               dropping down the Calendar.
//Revision     : October 18, 2000 ekosbg@bigfoot.com
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.CalendarOnDropDown(Sender: TObject);
begin
  FDataLink.Edit;
end;

end.

