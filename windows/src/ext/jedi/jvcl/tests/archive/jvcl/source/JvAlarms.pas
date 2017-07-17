{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlarms.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse@buypin.com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Peter Th�rnqvist [peter3@peter3.com]

Last Modified:
  2002-06-15 - (p3) changed TJvAlarm record to TCollection so you can easily manipulate it at design-time

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvAlarms;

interface

uses
  Windows, SysUtils, Classes, Dialogs, Controls, ExtCtrls,
  JvComponent,
// Bianconi - To include TJvTriggerKind type
  JvTypes;
// End of Bianconi

type
  TJvAlarmItem = class(TCollectionItem)
  private
    FName: string;
    FTime: TDateTime;
    FKind: TJvTriggerKind;
  public                       
    procedure Assign(Source: TPersistent); override;
  published
    property Name: string read FName write FName;
    property Time: TDateTime read FTime write FTime;
    property Kind: TJvTriggerKind read FKind write FKind;
  end;

  TJvAlarmEvent = procedure(Sender: TObject;
    const Alarm: TJvAlarmItem; const TriggerTime: TDateTime) of object;

  TJvAlarmItems = class(TOwnedCollection)
  private
    function GetItems(Index: Integer): TJVAlarmItem;
    procedure SetItems(Index: Integer; const Value: TJVAlarmItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TJvAlarmItem;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TJVAlarmItem read GetItems write SetItems; default;
  end;

  TJvAlarms = class(TJvComponent)
  private
    FActive: Boolean;
    FLast: TTimeStamp;
    FOnAlarm: TJvAlarmEvent;
    FRunning: Boolean;
    FTimer: TTimer;
    FAlarms: TJvAlarmItems;
    FBusy: Boolean;
    procedure OnTimer(Sender: TObject);
    procedure SetActive(const Value: Boolean);
    procedure SetAlarms(const Value: TJvAlarmItems);
  protected
    procedure DoAlarm(const Alarm: TJvAlarmItem; const TriggerTime: TDateTime);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(const AName: string; const ATime: TDateTime; const AKind: TJvTriggerKind = tkOneShot);
    procedure Delete(const Idx: Cardinal);

    //    property Alarms[Idx: Cardinal]: TJvAlarm read GetAlarm;
    property Running: Boolean read FRunning;
  published
    property Alarms: TJvAlarmItems read FAlarms write SetAlarms;
    property Active: Boolean read FActive write SetActive default False;
    property OnAlarm: TJvAlarmEvent read FOnAlarm write FOnAlarm;
  end;

implementation

resourcestring
  RC_Alarm = 'Unable to get alarm number %d. Index out of bounds';

//=== TJvAlarms ==============================================================

constructor TJvAlarms.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlarms := TJvAlarmItems.Create(Self);
  FActive := False;
  FRunning := False;
  FOnAlarm := nil;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 500;
  FTimer.OnTimer := OnTimer;
  FTimer.Enabled := False;
  FLast := DateTimeToTimeStamp(Now);
end;

destructor TJvAlarms.Destroy;
begin
  FAlarms.Free;
  FTimer.Free;
  inherited Destroy;
end;

procedure TJvAlarms.Add(const AName: string; const ATime: TDateTime; const AKind: TJvTriggerKind);
begin
  // hs (Oneshot-) timed out ? then we ignore this alarm !
  // works only by calling this funtion directly !
  if (aTime >= Now) or (AKind <> tkOneShot) then
  begin
    with FAlarms.Add do
    begin
      Name := AName;
      Time := ATime;
      Kind := AKind;
    end;
    FRunning := Active;
    FTimer.Enabled := Running;
  end;
end;

procedure TJvAlarms.Delete(const Idx: Cardinal);
begin
  FAlarms.Delete(Idx);
  // (p3)
  FRunning := Active and (FAlarms.Count > 0);
  FTimer.Enabled := Running;
end;

procedure TJvAlarms.DoAlarm(const Alarm: TJvAlarmItem; const TriggerTime: TDateTime);
begin
  if Assigned(FOnAlarm) then
    FOnAlarm(Self, Alarm, TriggerTime);
end;

procedure TJvAlarms.OnTimer(Sender: TObject);
var
  I: Cardinal;
  Current: TDateTime;
  Stamp: TTimeStamp;
  Year, Month, Day: Word;
  Alarm: TJvAlarmItem;
  // hs reentry flag added
  // may be necessary if a userfunction in DoAlarm does not
  // return (ex.: modal dialogbox ) before the same alarm is activated next time.
  // it's just a workaround - may be done better :-)
begin
  if not FBusy then
  begin
    FBusy := True;
    try
      if FAlarms.Count >= 0 then
      begin
        Current := Now;
        Stamp := DateTimeToTimeStamp(Now);
        // sort out delayed Timer events which may arrive in bunches
        if (Stamp.Time - FLast.Time) >= 1000 then
        begin
          FLast := Stamp;
          for I := FAlarms.Count - 1 downto 0 do
          begin
            Alarm := Alarms[I];
            if Current >= Alarm.Time then
            begin
              // OnAlarm aufrufen - hier am besten keine Funktionen
              // > 500msec aufrufen, da diese Routine sonst hier kreiselt
              // und keine weiteren Alarme durchkommen !
              DoAlarm(Alarm, Current);
              Stamp := DateTimeToTimeStamp(Alarm.Time);
              case Alarm.Kind of
                tkOneShot: ;
                //hs Delete(i) removed - later on was a reference to 'Alarm.Kind'
                //  which failed caused by an invalid Alarm
                tkEachSecond:
                  Inc(Stamp.Time, 1000);
                tkEachMinute:
                  Inc(Stamp.Time, 60 * 1000);
                tkEachHour:
                  Inc(Stamp.Time, 60 * 60 * 1000);
                tkEachDay:
                  Inc(Stamp.Date);
                tkEachMonth:
                  Stamp := DateTimeToTimeStamp(IncMonth(Alarm.Time, 1));
                tkEachYear:
                  begin
                    DecodeDate(Current, Year, Month, Day);
                    if IsLeapYear(Year) then
                      Inc(Stamp.Date, 366)
                    else
                      Inc(Stamp.Date, 365);
                  end;
              end;
              if Stamp.Time > 24 * 60 * 60 * 1000 then
              begin
                Inc(Stamp.Date);
                Dec(Stamp.Time, 24 * 60 * 60 * 1000);
              end;
              if Alarm.Kind <> tkOneShot then
                Alarm.Time := TimeStampToDateTime(Stamp)
                // hs a better place for 'Delete(i)'
              else
                Delete(I);
            end;
          end;
        end;
      end;
    finally
      FBusy := False;
    end;
  end;
end;

procedure TJvAlarms.SetActive(const Value: Boolean);
begin
  FActive := Value;
  FRunning := FActive and (FAlarms.Count > 0);
  FTimer.Enabled := Running;
end;

procedure TJvAlarms.SetAlarms(const Value: TJvAlarmItems);
begin
  FAlarms.Assign(Value);
end;

//=== TJvAlarmItems ==========================================================

constructor TJvAlarmItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvAlarmItem);
end;

function TJvAlarmItems.Add: TJvAlarmItem;
begin
  Result := TJvAlarmItem(inherited Add);
end;

procedure TJvAlarmItems.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvAlarmItems then
  begin
    Clear;
    for I := 1 to TJvAlarmItems(Source).Count do
      Add.Assign(TJvAlarmItems(Source).Items[I - 1]);
  end
  else
    inherited Assign(Source);
end;

function TJvAlarmItems.GetItems(Index: Integer): TJVAlarmItem;
begin
  Result := TJVAlarmItem(inherited Items[Index]);
end;

procedure TJvAlarmItems.SetItems(Index: Integer; const Value: TJVAlarmItem);
begin
  inherited Items[Index] := Value;
end;

//=== TJvAlarmItem ===========================================================

procedure TJvAlarmItem.Assign(Source: TPersistent);
begin
  if Source is TJvAlarmItem then
  begin
    Name := TJvAlarmItem(Source).Name;
    Time := TJvAlarmItem(Source).Time;
    Kind := TJvAlarmItem(Source).Kind;
  end;
  inherited Assign(Source);
end;

end.

