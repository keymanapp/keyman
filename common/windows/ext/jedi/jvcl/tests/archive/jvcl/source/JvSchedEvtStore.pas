{-----------------------------------------------------------------------------

 Project JEDI Visible Component Library (J-VCL)

 The contents of this file are subject to the Mozilla Public License Version
 1.1 (the "License"); you may not use this file except in compliance with the
 License. You may obtain a copy of the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS IS" basis,
 WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 the specific language governing rights and limitations under the License.

 The Initial Developer of the Original Code is Marcel Bestebroer
  <marcelb@zeelandnet.nl>.
 Portions created by Marcel Bestebroer are Copyright (C) 2000 - 2002 mbeSoft.
 All Rights Reserved.

 ******************************************************************************

 Persistency layer for JvScheduledEvents

 You may retrieve the latest version of this file at the Project JEDI home
 page, located at http://www.delphi-jedi.org
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvSchedEvtStore;

interface

uses
  Classes,
  JvScheduledEvents;

type
  TSchedEvtStoreAttribute = (sesaStructured, sesaIdentifiers);
  TSchedEvtStoreAttributes = set of TSchedEvtStoreAttribute;

  TSchedEvtStructKind =
    (seskState, seskEvent, seskSchedule, seskScheduleRecurInfo,
     seskScheduleEndInfo, seskScheduleDayFreq, seskScheduleDaily,
     seskScheduleWeekly, seskScheduleMonthly, seskScheduleMonthlyIndex,
     seskScheduleYearly, seskScheduleYearlyIndex);

  TSchedEvtItemKind =
    (seikUnknown, seikScheduleStart, seikScheduleRecurType,
    seikScheduleEndType, seikScheduleEndCount, seikScheduleEndDate,
    seikFreqStart, seikFreqEnd, seikFreqInterval, seikScheduleDailyWeekdays,
    seikScheduleDailyInterval, seikScheduleWeeklyDays,
    seikScheduleWeeklyInterval, seikScheduleMonthlyDay,
    seikScheduleMonthlyIndexType, seikScheduleMonthlyIndex,
    seikScheduleMonthlyInterval, seikScheduleYearlyDay,
    seikScheduleYearlyMonth, seikScheduleYearlyIndexType,
    seikScheduleYearlyIndex, seikScheduleYearlyInterval);

  IJvScheduledEventsStore = interface
    ['{FD6437D8-B951-4C72-AA5F-B96911D51B65}']
    procedure LoadState(const Event: TJvEventCollectionItem);
    procedure SaveState(const Event: TJvEventCollectionItem);

    procedure LoadSchedule(const Event: TJvEventCollectionItem);
    procedure SaveSchedule(const Event: TJvEventCollectionItem);

    procedure LoadEventSettings(const Event: TJvEventCollectionItem);
    procedure SaveEventSettings(const Event: TJvEventCollectionItem);
  end;

  TJvSchedEvtStore = class(TInterfacedObject, IJvScheduledEventsStore)
  private
    FEvent: TJvEventCollectionItem;
    FStructStack: array of TSchedEvtStructKind;
  protected
    // Structure stack managment: low level
    procedure PushStruct(const StructType: TSchedEvtStructKind);
    function PeekStruct: TSchedEvtStructKind;
    function PopStruct: TSchedEvtStructKind;
    // property access methods
    function GetEvent: TJvEventCollectionItem;
    // Retrieving items: Schedule
    procedure CheckSignature; virtual; abstract;
    procedure CheckVersion; virtual; abstract;
    function NextItemKind: TSchedEvtItemKind; virtual;
    procedure RestoreScheduleStart; virtual; abstract;
    procedure RestoreScheduleRecurType; virtual; abstract;
    procedure RestoreScheduleEndType; virtual; abstract;
    procedure RestoreScheduleEndCount; virtual; abstract;
    procedure RestoreScheduleEndDate; virtual; abstract;
    procedure RestoreFreqStart; virtual; abstract;
    procedure RestoreFreqEnd; virtual; abstract;
    procedure RestoreFreqInterval; virtual; abstract;
    procedure RestoreScheduleDailyWeekdays; virtual; abstract;
    procedure RestoreScheduleDailyInterval; virtual; abstract;
    procedure RestoreScheduleWeeklyDays; virtual; abstract;
    procedure RestoreScheduleWeeklyInterval; virtual; abstract;
    procedure RestoreScheduleMonthlyDay; virtual; abstract;
    procedure RestoreScheduleMonthlyIndexType; virtual; abstract;
    procedure RestoreScheduleMonthlyIndex; virtual; abstract;
    procedure RestoreScheduleMonthlyInterval; virtual; abstract;
    procedure RestoreScheduleYearlyDay; virtual; abstract;
    procedure RestoreScheduleYearlyMonth; virtual; abstract;
    procedure RestoreScheduleYearlyIndexType; virtual; abstract;
    procedure RestoreScheduleYearlyIndex; virtual; abstract;
    procedure RestoreScheduleYearlyInterval; virtual; abstract;
    // Storing items: signature (only for unstructured storages) and versioning
    procedure StoreSignature; virtual;
    procedure StoreVersion; virtual; abstract;
    // Storing items: Schedule
    procedure StoreScheduleStart; virtual; abstract;
    procedure StoreScheduleRecurType; virtual; abstract;
    procedure StoreScheduleEndType; virtual; abstract;
    procedure StoreScheduleEndCount; virtual; abstract;
    procedure StoreScheduleEndDate; virtual; abstract;
    procedure StoreFreqStart; virtual; abstract;
    procedure StoreFreqEnd; virtual; abstract;
    procedure StoreFreqInterval; virtual; abstract;
    procedure StoreScheduleDailyWeekdays; virtual; abstract;
    procedure StoreScheduleDailyInterval; virtual; abstract;
    procedure StoreScheduleWeeklyDays; virtual; abstract;
    procedure StoreScheduleWeeklyInterval; virtual; abstract;
    procedure StoreScheduleMonthlyDay; virtual; abstract;
    procedure StoreScheduleMonthlyIndexType; virtual; abstract;
    procedure StoreScheduleMonthlyIndex; virtual; abstract;
    procedure StoreScheduleMonthlyInterval; virtual; abstract;
    procedure StoreScheduleYearlyDay; virtual; abstract;
    procedure StoreScheduleYearlyMonth; virtual; abstract;
    procedure StoreScheduleYearlyIndexType; virtual; abstract;
    procedure StoreScheduleYearlyIndex; virtual; abstract;
    procedure StoreScheduleYearlyInterval; virtual; abstract;
    // Structure stack managment: high level
    procedure BeginStruct(const StructType: TSchedEvtStructKind); virtual;
    procedure EndStruct; virtual;
    procedure CheckBeginStruct(const StructType: TSchedEvtStructKind); virtual;
    procedure CheckEndStruct; virtual;
    property Event: TJvEventCollectionItem read GetEvent;
  public
    function IsStructured: Boolean;
    function UsesIdentifiers: Boolean;
    function GetAttributes: TSchedEvtStoreAttributes; virtual;
    procedure LoadState(const Event: TJvEventCollectionItem);
    procedure SaveState(const Event: TJvEventCollectionItem);
    procedure LoadSchedule(const Event: TJvEventCollectionItem);
    procedure SaveSchedule(const Event: TJvEventCollectionItem);
    procedure LoadEventSettings(const Event: TJvEventCollectionItem);
    procedure SaveEventSettings(const Event: TJvEventCollectionItem);
  end;

function ScheduledEventStore_Stream(const Stream: TStream; const Binary: Boolean = False;
  const OwnsStream: Boolean = True): IJvScheduledEventsStore;

implementation

uses
  SysUtils, TypInfo,
  JclRTTI, JclSchedule,
  JvTypes;

//=== TJvSchedEvtStore =======================================================

procedure TJvSchedEvtStore.PushStruct(const StructType: TSchedEvtStructKind);
begin
  SetLength(FStructStack, Length(FStructStack) + 1);
  FStructStack[High(FStructStack)] := StructType;
end;

function TJvSchedEvtStore.PeekStruct: TSchedEvtStructKind;
begin
  if Length(FStructStack) = 0 then
    raise EJVCLException.Create('Structure stack is empty.');
  Result := FStructStack[High(FStructStack)];
end;

function TJvSchedEvtStore.PopStruct: TSchedEvtStructKind;
begin
  Result := PeekStruct;
  SetLength(FStructStack, High(FStructStack));
end;

function TJvSchedEvtStore.GetEvent: TJvEventCollectionItem;
begin
  Result := FEvent;
end;

function TJvSchedEvtStore.NextItemKind: TSchedEvtItemKind;
begin
  Result := seikUnknown;
end;

procedure TJvSchedEvtStore.StoreSignature;
begin
  // override for non-structured storages to store an identification of the stream if needed.
  TProcedure(AbstractErrorProc);
end;

procedure TJvSchedEvtStore.BeginStruct(const StructType: TSchedEvtStructKind);
begin
  { override to take additional steps when a new structure is to be written. Either call inherited
    or use PushStruct to store the current structure information on the stack. }
  PushStruct(StructType);
end;

procedure TJvSchedEvtStore.EndStruct;
begin
  { override to take additional steps when a structure is to be terminated. Either call inherited
    or use PopStruct to retrieve the structure information from the stack. }
  PopStruct;
end;

procedure TJvSchedEvtStore.CheckBeginStruct(const StructType: TSchedEvtStructKind);
begin
  { override to check if the next structure is the one specified and raise an exception if it isn't.
    Either call inherited or use PushStruct to store the current structure information on the
    stack. }
  PushStruct(StructType);
end;

procedure TJvSchedEvtStore.CheckEndStruct;
begin
  { override to check if the next item marks the end of the structure as specified on the current
    stack (use PopStruct to retrieve the structure information from the stack). Raise an exception
    if the next item does not mark the end of the currently active structure. }
  PopStruct;
end;

function TJvSchedEvtStore.IsStructured: Boolean;
begin
  Result := sesaStructured in GetAttributes;
end;

function TJvSchedEvtStore.UsesIdentifiers: Boolean;
begin
  Result := sesaIdentifiers in GetAttributes;
end;

function TJvSchedEvtStore.GetAttributes: TSchedEvtStoreAttributes;
begin
  Result := [];
end;

procedure TJvSchedEvtStore.LoadState(const Event: TJvEventCollectionItem);
begin
  raise EJVCLException.Create('not implemented');
end;

procedure TJvSchedEvtStore.SaveState(const Event: TJvEventCollectionItem);
begin
  raise EJVCLException.Create('not implemented');
end;

procedure TJvSchedEvtStore.LoadSchedule(const Event: TJvEventCollectionItem);
var
  OrgSchedule: IJclSchedule;
begin
  // Clear the structure stack
  SetLength(FStructStack, 0);
  FEvent := Event;
  with Event do
  begin
    if not (State in [sesNotInitialized, sesEnded]) then
      raise EJVCLException.Create('Schedule is active. Reading a new schedule can only be done on inactive schedules.');
    OrgSchedule := Schedule;
    try
      Schedule := CreateSchedule;
      // Begin of actual reading
      if not IsStructured then
        CheckSignature;
      CheckBeginStruct(seskSchedule);
      CheckVersion;
        // Generic schedule info
      RestoreScheduleStart;
      CheckBeginStruct(seskScheduleRecurInfo);
      RestoreScheduleRecurType;
      if Schedule.RecurringType <> srkOneShot then
      begin
        CheckBeginStruct(seskScheduleEndInfo);
        RestoreScheduleEndType;
        if Schedule.EndType = sekDate then
          RestoreScheduleEndDate
        else
        if Schedule.EndType in [sekTriggerCount, sekDayCount] then
          RestoreScheduleEndCount;
        CheckEndStruct; {seskScheduleEndInfo}

        CheckBeginStruct(seskScheduleDayFreq);
        RestoreFreqStart;
        if not UsesIdentifiers or (NextItemKind = seikFreqEnd) then
        begin
          RestoreFreqEnd;
          RestoreFreqInterval;
        end;
        CheckEndStruct; {seskScheduleDayFreq}

        case Schedule.RecurringType of
          srkDaily:
            begin
              CheckBeginStruct(seskScheduleDaily);
              if not UsesIdentifiers or (NextItemKind = seikScheduleDailyWeekdays) then
                RestoreScheduleDailyWeekdays;
              if not UsesIdentifiers or (NextItemKind = seikScheduleDailyInterval) then
                RestoreScheduleDailyInterval;
              CheckEndStruct; {seskScheduleDaily}
            end;
          srkWeekly:
            begin
              CheckBeginStruct(seskScheduleWeekly);
              RestoreScheduleWeeklyDays;
              RestoreScheduleWeeklyInterval;
              CheckEndStruct; {seskScheduleWeekly}
            end;
          srkMonthly:
            begin
              CheckBeginStruct(seskScheduleMonthly);
              CheckBeginStruct(seskScheduleMonthlyIndex);
              RestoreScheduleMonthlyIndexType;
              if (Schedule as IJclMonthlySchedule).IndexKind <> sikNone then
                RestoreScheduleMonthlyIndex;
              CheckEndStruct; {seskScheduleMonthlyIndex}
              if (Schedule as IJclMonthlySchedule).IndexKind = sikNone then
                RestoreScheduleMonthlyDay;
              RestoreScheduleMonthlyInterval;
              CheckEndStruct; {seskScheduleMonthly}
            end;
          srkYearly:
            begin
              CheckBeginStruct(seskScheduleYearly);
              CheckBeginStruct(seskScheduleYearlyIndex);
              RestoreScheduleYearlyIndexType;
              if (Schedule as IJclYearlySchedule).IndexKind <> sikNone then
                RestoreScheduleYearlyIndex;
              CheckEndStruct; {seskScheduleYearlyIndex}
              if (Schedule as IJclYearlySchedule).IndexKind = sikNone then
                RestoreScheduleYearlyDay;
              RestoreScheduleYearlyMonth;
              RestoreScheduleYearlyInterval;
              CheckEndStruct; {seskScheduleYearly}
            end;
        end;
      end;
      CheckEndStruct; {seskScheduleRecurInfo}
      CheckEndStruct; {seskSchedule}
      // we succeeded in reading in the schedule.
    except
      { uh-oh! reading of the schedule failed. Better restore the original
        schedule so the end user won't miss it ;) }
      Schedule := OrgSchedule;
      raise;
    end;
  end;
end;

procedure TJvSchedEvtStore.SaveSchedule(const Event: TJvEventCollectionItem);
begin
  // Clear the structure stack
  SetLength(FStructStack, 0);
  FEvent := Event;
  with Event do
  begin
    if not (State in [sesNotInitialized, sesEnded, sesPaused]) then
      raise EJVCLException.Create('Schedule is active. Storing of a schedule can only be done on inactive schedules.');
    if not IsStructured then
      StoreSignature;
    BeginStruct(seskSchedule);
    StoreVersion;
    // Generic schedule info
    StoreScheduleStart;
    BeginStruct(seskScheduleRecurInfo);
    StoreScheduleRecurType;
    if Schedule.RecurringType <> srkOneShot then
    begin
      BeginStruct(seskScheduleEndInfo);
      StoreScheduleEndType;
      if Schedule.EndType = sekDate then
        StoreScheduleEndDate
      else
      if Schedule.EndType in [sekTriggerCount, sekDayCount] then
        StoreScheduleEndCount;
      EndStruct; {seskScheduleEndInfo}

      BeginStruct(seskScheduleDayFreq);
      StoreFreqStart;
      if not UsesIdentifiers or ((Schedule as IJclScheduleDayFrequency).Interval <> 0) then
      begin
        StoreFreqEnd;
        StoreFreqInterval;
      end;
      EndStruct; {seskScheduleDayFreq}

      case Schedule.RecurringType of
        srkDaily:
          begin
            BeginStruct(seskScheduleDaily);
            if not UsesIdentifiers or (Schedule as IJclDailySchedule).EveryWeekDay then
              StoreScheduleDailyWeekdays;
            if not UsesIdentifiers or not (Schedule as IJclDailySchedule).EveryWeekDay then
              StoreScheduleDailyInterval;
            EndStruct; {seskScheduleDaily}
          end;
        srkWeekly:
          begin
            BeginStruct(seskScheduleWeekly);
            StoreScheduleWeeklyDays;
            StoreScheduleWeeklyInterval;
            EndStruct; {seskScheduleWeekly}
          end;
        srkMonthly:
          begin
            BeginStruct(seskScheduleMonthly);
            BeginStruct(seskScheduleMonthlyIndex);
            StoreScheduleMonthlyIndexType;
            if (Schedule as IJclMonthlySchedule).IndexKind <> sikNone then
              StoreScheduleMonthlyIndex;
            EndStruct; {seskScheduleMonthlyIndex}
            if (Schedule as IJclMonthlySchedule).IndexKind = sikNone then
              StoreScheduleMonthlyDay;
            StoreScheduleMonthlyInterval;
            EndStruct; {seskScheduleMonthly}
          end;
        srkYearly:
          begin
            BeginStruct(seskScheduleYearly);
            BeginStruct(seskScheduleYearlyIndex);
            StoreScheduleYearlyIndexType;
            if (Schedule as IJclYearlySchedule).IndexKind <> sikNone then
              StoreScheduleYearlyIndex;
            EndStruct; {seskScheduleYearlyIndex}
            if (Schedule as IJclYearlySchedule).IndexKind = sikNone then
              StoreScheduleYearlyDay;
            StoreScheduleYearlyMonth;
            StoreScheduleYearlyInterval;
            EndStruct; {seskScheduleYearly}
          end;
      end;
    end;
    EndStruct; {seskScheduleRecurInfo}
    EndStruct; {seskSchedule}
  end;
end;

procedure TJvSchedEvtStore.LoadEventSettings(const Event: TJvEventCollectionItem);
begin
  raise EJVCLException.Create('not implemented.');
end;

procedure TJvSchedEvtStore.SaveEventSettings(const Event: TJvEventCollectionItem);
begin
  raise EJVCLException.Create('not implemented.');
end;

const
  BinStreamID = 'JVSE';
  BinStreamVer = Word($0001);

//=== TBinStore ==============================================================

type
  TBinStore = class(TJvSchedEvtStore)
  private
    Stream: TStream;
    OwnsStream: Boolean;
    StreamVersion: Word; // Only used for reading
  protected
    // Retrieving items: Schedule
    procedure CheckSignature; override;
    procedure CheckVersion; override;
    procedure RestoreScheduleStart; override;
    procedure RestoreScheduleRecurType; override;
    procedure RestoreScheduleEndType; override;
    procedure RestoreScheduleEndCount; override;
    procedure RestoreScheduleEndDate; override;
    procedure RestoreFreqStart; override;
    procedure RestoreFreqEnd; override;
    procedure RestoreFreqInterval; override;
    procedure RestoreScheduleDailyWeekdays; override;
    procedure RestoreScheduleDailyInterval; override;
    procedure RestoreScheduleWeeklyDays; override;
    procedure RestoreScheduleWeeklyInterval; override;
    procedure RestoreScheduleMonthlyDay; override;
    procedure RestoreScheduleMonthlyIndexType; override;
    procedure RestoreScheduleMonthlyIndex; override;
    procedure RestoreScheduleMonthlyInterval; override;
    procedure RestoreScheduleYearlyDay; override;
    procedure RestoreScheduleYearlyMonth; override;
    procedure RestoreScheduleYearlyIndexType; override;
    procedure RestoreScheduleYearlyIndex; override;
    procedure RestoreScheduleYearlyInterval; override;
    // Storing items: signature (only for unstructured storages) and versioning
    procedure StoreSignature; override;
    procedure StoreVersion; override;
    // Storing items: Schedule
    procedure StoreScheduleStart; override;
    procedure StoreScheduleRecurType; override;
    procedure StoreScheduleEndType; override;
    procedure StoreScheduleEndCount; override;
    procedure StoreScheduleEndDate; override;
    procedure StoreFreqStart; override;
    procedure StoreFreqEnd; override;
    procedure StoreFreqInterval; override;
    procedure StoreScheduleDailyWeekdays; override;
    procedure StoreScheduleDailyInterval; override;
    procedure StoreScheduleWeeklyDays; override;
    procedure StoreScheduleWeeklyInterval; override;
    procedure StoreScheduleMonthlyDay; override;
    procedure StoreScheduleMonthlyIndexType; override;
    procedure StoreScheduleMonthlyIndex; override;
    procedure StoreScheduleMonthlyInterval; override;
    procedure StoreScheduleYearlyDay; override;
    procedure StoreScheduleYearlyMonth; override;
    procedure StoreScheduleYearlyIndexType; override;
    procedure StoreScheduleYearlyIndex; override;
    procedure StoreScheduleYearlyInterval; override;
  public
    constructor Create(const AStream: TStream; const AOwnsStream: Boolean = True);
    destructor Destroy; override;
  end;

constructor TBinStore.Create(const AStream: TStream; const AOwnsStream: Boolean);
begin
  inherited Create;
  Stream := AStream;
  OwnsStream := AOwnsStream;
end;

destructor TBinStore.Destroy;
begin
  if OwnsStream then
    FreeAndNil(Stream);
  inherited Destroy;
end;

procedure TBinStore.CheckSignature;
var
  S: string;
begin
  SetLength(S, Length(BinStreamID));
  Stream.ReadBuffer(S[1], Length(BinStreamID));
  if S <> BinStreamID then
    raise EJVCLException.Create('Not a schedule.');
end;

procedure TBinStore.CheckVersion;
begin
  Stream.ReadBuffer(StreamVersion, SizeOf(StreamVersion));
  if StreamVersion > BinStreamVer then
    raise EJVCLException.CreateFmt('Unknown schedule version ($%s)', [IntToHex(StreamVersion, 4)]);
end;

procedure TBinStore.RestoreScheduleStart;
var
  I: TTimeStamp;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  Event.Schedule.StartDate := I;
end;

procedure TBinStore.RestoreScheduleRecurType;
var
  I: TScheduleRecurringKind;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  Event.Schedule.RecurringType := I;
end;

procedure TBinStore.RestoreScheduleEndType;
var
  I: TScheduleEndKind;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  Event.Schedule.EndType := I;
end;

procedure TBinStore.RestoreScheduleEndCount;
var
  I: Cardinal;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  Event.Schedule.EndCount := I;
end;

procedure TBinStore.RestoreScheduleEndDate;
var
  I: TTimeStamp;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  Event.Schedule.EndDate := I;
end;

procedure TBinStore.RestoreFreqStart;
var
  I: Cardinal;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  (Event.Schedule as IJclScheduleDayFrequency).StartTime := I;
end;

procedure TBinStore.RestoreFreqEnd;
var
  I: Cardinal;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  (Event.Schedule as IJclScheduleDayFrequency).EndTime := I;
end;

procedure TBinStore.RestoreFreqInterval;
var
  I: Cardinal;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  (Event.Schedule as IJclScheduleDayFrequency).Interval := I;
end;

procedure TBinStore.RestoreScheduleDailyWeekdays;
var
  I: Boolean;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  (Event.Schedule as IJclDailySchedule).EveryWeekDay := I;
end;

procedure TBinStore.RestoreScheduleDailyInterval;
var
  I: Cardinal;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  (Event.Schedule as IJclDailySchedule).Interval := I;
end;

procedure TBinStore.RestoreScheduleWeeklyDays;
var
  I: TScheduleWeekDays;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  (Event.Schedule as IJclWeeklySchedule).DaysOfWeek := I;
end;

procedure TBinStore.RestoreScheduleWeeklyInterval;
var
  I: Cardinal;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  (Event.Schedule as IJclWeeklySchedule).Interval := I;
end;

procedure TBinStore.RestoreScheduleMonthlyDay;
var
  I: Cardinal;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  (Event.Schedule as IJclMonthlySchedule).Day := I;
end;

procedure TBinStore.RestoreScheduleMonthlyIndexType;
var
  I: TScheduleIndexKind;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  (Event.Schedule as IJclMonthlySchedule).IndexKind := I;
end;

procedure TBinStore.RestoreScheduleMonthlyIndex;
var
  I: Integer;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  (Event.Schedule as IJclMonthlySchedule).IndexValue := I;
end;

procedure TBinStore.RestoreScheduleMonthlyInterval;
var
  I: Cardinal;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  (Event.Schedule as IJclMonthlySchedule).Interval := I;
end;

procedure TBinStore.RestoreScheduleYearlyDay;
var
  I: Cardinal;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  (Event.Schedule as IJclYearlySchedule).Day := I;
end;

procedure TBinStore.RestoreScheduleYearlyMonth;
var
  I: Cardinal;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  (Event.Schedule as IJclYearlySchedule).Month := I;
end;

procedure TBinStore.RestoreScheduleYearlyIndexType;
var
  I: TScheduleIndexKind;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  (Event.Schedule as IJclYearlySchedule).IndexKind := I;
end;

procedure TBinStore.RestoreScheduleYearlyIndex;
var
  I: Integer;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  (Event.Schedule as IJclYearlySchedule).IndexValue := I;
end;

procedure TBinStore.RestoreScheduleYearlyInterval;
var
  I: Cardinal;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  (Event.Schedule as IJclYearlySchedule).Interval := I;
end;

procedure TBinStore.StoreSignature;
var
  S: string;
begin
  S := BinStreamID;
  Stream.WriteBuffer(S[1], Length(S));
end;

procedure TBinStore.StoreVersion;
var
  W: Word;
begin
  W := BinStreamVer;
  Stream.writeBuffer(W, SizeOf(W));
end;

procedure TBinStore.StoreScheduleStart;
var
  Stamp: TTimeStamp;
begin
  Stamp := Event.Schedule.StartDate;
  Stream.WriteBuffer(Stamp, SizeOf(Stamp));
end;

procedure TBinStore.StoreScheduleRecurType;
var
  RT: TScheduleRecurringKind;
begin
  RT := Event.Schedule.RecurringType;
  Stream.WriteBuffer(RT, SizeOf(RT));
end;

procedure TBinStore.StoreScheduleEndType;
var
  ET: TScheduleEndKind;
begin
  ET := Event.Schedule.EndType;
  Stream.WriteBuffer(ET, SizeOf(ET));
end;

procedure TBinStore.StoreScheduleEndCount;
var
  I: Cardinal;
begin
  I := Event.Schedule.EndCount;
  Stream.WriteBuffer(I, SizeOf(I));
end;

procedure TBinStore.StoreScheduleEndDate;
var
  Stamp: TTimeStamp;
begin
  Stamp := Event.Schedule.EndDate;
  Stream.WriteBuffer(Stamp, SizeOf(Stamp));
end;

procedure TBinStore.StoreFreqStart;
var
  I: Cardinal;
begin
  I := (Event.Schedule as IJclScheduleDayFrequency).StartTime;
  Stream.WriteBuffer(I, SizeOf(I));
end;

procedure TBinStore.StoreFreqEnd;
var
  I: Cardinal;
begin
  I := (Event.Schedule as IJclScheduleDayFrequency).EndTime;
  Stream.WriteBuffer(I, SizeOf(I));
end;

procedure TBinStore.StoreFreqInterval;
var
  I: Cardinal;
begin
  I := (Event.Schedule as IJclScheduleDayFrequency).Interval;
  Stream.WriteBuffer(I, SizeOf(I));
end;

procedure TBinStore.StoreScheduleDailyWeekdays;
var
  EWD: Boolean;
begin
  EWD := (Event.Schedule as IJclDailySchedule).EveryWeekDay;
  Stream.WriteBuffer(EWD, SizeOf(EWD));
end;

procedure TBinStore.StoreScheduleDailyInterval;
var
  I: Cardinal;
begin
  I := (Event.Schedule as IJclDailySchedule).Interval;
  Stream.WriteBuffer(I, SizeOf(I));
end;

procedure TBinStore.StoreScheduleWeeklyDays;
var
  WD: TScheduleWeekDays;
begin
  WD := (Event.Schedule as IJclWeeklySchedule).DaysOfWeek;
  Stream.WriteBuffer(WD, SizeOf(WD));
end;

procedure TBinStore.StoreScheduleWeeklyInterval;
var
  I: Cardinal;
begin
  I := (Event.Schedule as IJclWeeklySchedule).Interval;
  Stream.WriteBuffer(I, SizeOf(I));
end;

procedure TBinStore.StoreScheduleMonthlyDay;
var
  I: Cardinal;
begin
  I := (Event.Schedule as IJclMonthlySchedule).Day;
  Stream.WriteBuffer(I, SizeOf(I));
end;

procedure TBinStore.StoreScheduleMonthlyIndexType;
var
  I: TScheduleIndexKind;
begin
  I := (Event.Schedule as IJclMonthlySchedule).IndexKind;
  Stream.WriteBuffer(I, SizeOf(I));
end;

procedure TBinStore.StoreScheduleMonthlyIndex;
var
  I: Integer;
begin
  I := (Event.Schedule as IJclMonthlySchedule).IndexValue;
  Stream.WriteBuffer(I, SizeOf(I));
end;

procedure TBinStore.StoreScheduleMonthlyInterval;
var
  I: Cardinal;
begin
  I := (Event.Schedule as IJclMonthlySchedule).Interval;
  Stream.WriteBuffer(I, SizeOf(I));
end;

procedure TBinStore.StoreScheduleYearlyDay;
var
  I: Cardinal;
begin
  I := (Event.Schedule as IJclYearlySchedule).Day;
  Stream.WriteBuffer(I, SizeOf(I));
end;

procedure TBinStore.StoreScheduleYearlyMonth;
var
  I: Cardinal;
begin
  I := (Event.Schedule as IJclYearlySchedule).Month;
  Stream.WriteBuffer(I, SizeOf(I));
end;

procedure TBinStore.StoreScheduleYearlyIndexType;
var
  I: TScheduleIndexKind;
begin
  I := (Event.Schedule as IJclYearlySchedule).IndexKind;
  Stream.WriteBuffer(I, SizeOf(I));
end;

procedure TBinStore.StoreScheduleYearlyIndex;
var
  I: Integer;
begin
  I := (Event.Schedule as IJclYearlySchedule).IndexValue;
  Stream.WriteBuffer(I, SizeOf(I));
end;

procedure TBinStore.StoreScheduleYearlyInterval;
var
  I: Integer;
begin
  I := (Event.Schedule as IJclYearlySchedule).Interval;
  Stream.WriteBuffer(I, SizeOf(I));
end;

const
  TxtIdentifiers: array [TSchedEvtItemKind] of PChar =
    ('', {seikUnknown}
     'Start', {seikScheduleStart}
     'Recur type', {seikScheduleRecurType}
     'End type', {seikScheduleEndType}
     'End count', {seikScheduleEndCount}
     'End', {seikScheduleEndDate}
     'Frequency start', {seikFreqStart}
     'Frequency end', {seikFreqEnd}
     'Frequency interval', {seikFreqInterval}
     'Daily every weekday', {seikScheduleDailyWeekdays}
     'Daily interval', {seikScheduleDailyInterval}
     'Weekly days', {seikScheduleWeeklyDays}
     'Weekly interval', {seikScheduleWeeklyInterval}
     'Monthly day', {seikScheduleMonthlyDay}
     'Monthly index type', {seikScheduleMonthlyIndexType}
     'Monthly index', {seikScheduleMonthlyIndex}
     'Monthly interval', {seikScheduleMonthlyInterval}
     'Yearly day', {seikScheduleYearlyDay}
     'Yearly month', {seikScheduleYearlyMonth}
     'Yearly index type', {seikScheduleYearlyIndexType}
     'Yearly index', {seikScheduleYearlyIndex}
     'Yearly interval'); {seikScheduleYearlyInterval}

  sTXTID_SchedGeneric = '# Schedule: Generic';
  sTXTID_SchedRecur = '# Schedule: Recurring info';
  sTXTID_SchedEnd = '# Schedule: End info';
  sTXTID_SchedFreq = '# Schedule: Day frequency';
  sTXTID_SchedDaily = '# Schedule: Daily info';
  sTXTID_SchedWeekly = '# Schedule: Weekly info';
  sTXTID_SchedMonthly = '# Schedule: Monthly info';
  sTXTID_SchedYearly = '# Schedule: Yearly info';

//=== TTxtStore ==============================================================

type
  TTxtStore = class(TJvSchedEvtStore)
  private
    Stream: TStream;
    OwnsStream: Boolean;
  protected
    // Retrieving items: Schedule
    procedure CheckSignature; override;
    procedure CheckVersion; override;
    function NextItemKind: TSchedEvtItemKind; override;
    procedure RestoreScheduleStart; override;
    procedure RestoreScheduleRecurType; override;
    procedure RestoreScheduleEndType; override;
    procedure RestoreScheduleEndCount; override;
    procedure RestoreScheduleEndDate; override;
    procedure RestoreFreqStart; override;
    procedure RestoreFreqEnd; override;
    procedure RestoreFreqInterval; override;
    procedure RestoreScheduleDailyWeekdays; override;
    procedure RestoreScheduleDailyInterval; override;
    procedure RestoreScheduleWeeklyDays; override;
    procedure RestoreScheduleWeeklyInterval; override;
    procedure RestoreScheduleMonthlyDay; override;
    procedure RestoreScheduleMonthlyIndexType; override;
    procedure RestoreScheduleMonthlyIndex; override;
    procedure RestoreScheduleMonthlyInterval; override;
    procedure RestoreScheduleYearlyDay; override;
    procedure RestoreScheduleYearlyMonth; override;
    procedure RestoreScheduleYearlyIndexType; override;
    procedure RestoreScheduleYearlyIndex; override;
    procedure RestoreScheduleYearlyInterval; override;
    // Storing items: signature (only for unstructured storages) and versioning
    procedure StoreSignature; override;
    procedure StoreVersion; override;
    // Storing items: Schedule
    procedure StoreScheduleStart; override;
    procedure StoreScheduleRecurType; override;
    procedure StoreScheduleEndType; override;
    procedure StoreScheduleEndCount; override;
    procedure StoreScheduleEndDate; override;
    procedure StoreFreqStart; override;
    procedure StoreFreqEnd; override;
    procedure StoreFreqInterval; override;
    procedure StoreScheduleDailyWeekdays; override;
    procedure StoreScheduleDailyInterval; override;
    procedure StoreScheduleWeeklyDays; override;
    procedure StoreScheduleWeeklyInterval; override;
    procedure StoreScheduleMonthlyDay; override;
    procedure StoreScheduleMonthlyIndexType; override;
    procedure StoreScheduleMonthlyIndex; override;
    procedure StoreScheduleMonthlyInterval; override;
    procedure StoreScheduleYearlyDay; override;
    procedure StoreScheduleYearlyMonth; override;
    procedure StoreScheduleYearlyIndexType; override;
    procedure StoreScheduleYearlyIndex; override;
    procedure StoreScheduleYearlyInterval; override;
    procedure BeginStruct(const StructType: TSchedEvtStructKind); override;
    procedure EndStruct; override;
    procedure CheckBeginStruct(const StructType: TSchedEvtStructKind); override;
    procedure CheckEndStruct; override;
    function ReadLn: string;
    function ReadNextLine: string;
    function ReadItem(out AName: string): string;
    procedure WriteLn(const S: string);
    function ReadEnum(const AName: string; const TypeInfo: PTypeInfo): Integer;
    function ReadInt(const AName: string): Int64;
    procedure ReadSet(const AName: string; out Value; const TypeInfo: PTypeInfo);
    function ReadStamp(const AName: string): TTimeStamp;
    function ReadStampDate(const AName: string): Integer;
    function ReadStampTime(const AName: string): Integer;
    procedure WriteEnum(const AName: string; const Ordinal: Integer; const TypeInfo: PTypeInfo);
    procedure WriteInt(const AName: string; const Value: Int64);
    procedure WriteSet(const AName: string; const Value; const TypeInfo: PTypeInfo);
    procedure WriteStamp(const AName: string; const Stamp: TTimeStamp);
    procedure WriteStampDate(const AName: string; const Date: Integer);
    procedure WriteStampTime(const AName: string; const Time: Integer);
  public
    function GetAttributes: TSchedEvtStoreAttributes; override;
    constructor Create(const AStream: TStream; const AOwnsStream: Boolean = True);
    destructor Destroy; override;
  end;

constructor TTxtStore.Create(const AStream: TStream; const AOwnsStream: Boolean);
begin
  inherited Create;
  Stream := AStream;
  OwnsStream := AOwnsStream;
end;

destructor TTxtStore.Destroy;
begin
  if OwnsStream then
    FreeAndNil(Stream);
  inherited Destroy;
end;

procedure TTxtStore.CheckSignature;
begin
end;

procedure TTxtStore.CheckVersion;
begin
end;

function TTxtStore.NextItemKind: TSchedEvtItemKind;
var
  SPos: Integer;
  ItemName: string;
  I: Integer;
begin
  SPos := Stream.Position;
  try
    ReadItem(ItemName);
    I := Pos('.', ItemName);
    if I > 0 then
      ItemName := Copy(ItemName, 1, I - 1);
    Result := High(TSchedEvtItemKind);
    while (Result > Low(TSchedEvtItemKind)) and
      not AnsiSameText(ItemName, TxtIdentifiers[Result]) do
      Dec(Result);
  finally
    Stream.Position := SPos;
  end;
end;

procedure TTxtStore.RestoreScheduleStart;
begin
  Event.Schedule.StartDate := ReadStamp(TxtIdentifiers[seikScheduleStart]);
end;

procedure TTxtStore.RestoreScheduleRecurType;
begin
  Event.Schedule.RecurringType := TScheduleRecurringKind(ReadEnum(
    TxtIdentifiers[seikScheduleRecurType], TypeInfo(TScheduleRecurringKind)));
end;

procedure TTxtStore.RestoreScheduleEndType;
begin
  Event.Schedule.EndType := TScheduleEndKind(ReadEnum(
    TxtIdentifiers[seikScheduleEndType], TypeInfo(TScheduleEndKind)));
end;

procedure TTxtStore.RestoreScheduleEndCount;
begin
  Event.Schedule.EndCount := ReadInt(TxtIdentifiers[seikScheduleEndCount]);
end;

procedure TTxtStore.RestoreScheduleEndDate;
begin
  Event.Schedule.EndDate := ReadStamp(TxtIdentifiers[seikScheduleEndDate]);
end;

procedure TTxtStore.RestoreFreqStart;
begin
  (Event.Schedule as IJclScheduleDayFrequency).StartTime := ReadStampTime(TxtIdentifiers[seikFreqStart]);
end;

procedure TTxtStore.RestoreFreqEnd;
begin
  (Event.Schedule as IJclScheduleDayFrequency).EndTime := ReadStampTime(TxtIdentifiers[seikFreqEnd]);
end;

procedure TTxtStore.RestoreFreqInterval;
begin
  (Event.Schedule as IJclScheduleDayFrequency).Interval := ReadStampTime(TxtIdentifiers[seikFreqInterval]);
end;

procedure TTxtStore.RestoreScheduleDailyWeekdays;
begin
  (Event.Schedule as IJclDailySchedule).EveryWeekDay := Boolean(ReadEnum(TxtIdentifiers[seikScheduleDailyWeekdays],
    TypeInfo(Boolean)));
end;

procedure TTxtStore.RestoreScheduleDailyInterval;
begin
  (Event.Schedule as IJclDailySchedule).Interval := ReadInt(TxtIdentifiers[seikScheduleDailyInterval]);
end;

procedure TTxtStore.RestoreScheduleWeeklyDays;
var
  I: TScheduleWeekDays;
begin
  ReadSet(TxtIdentifiers[seikScheduleWeeklyDays], I, TypeInfo(TScheduleWeekDays));
  (Event.Schedule as IJclWeeklySchedule).DaysOfWeek := I;
end;

procedure TTxtStore.RestoreScheduleWeeklyInterval;
begin
  (Event.Schedule as IJclWeeklySchedule).Interval := ReadInt(TxtIdentifiers[seikScheduleWeeklyInterval]);
end;

procedure TTxtStore.RestoreScheduleMonthlyDay;
begin
  (Event.Schedule as IJclMonthlySchedule).Day := ReadInt(TxtIdentifiers[seikScheduleMonthlyDay]);
end;

procedure TTxtStore.RestoreScheduleMonthlyIndexType;
begin
  (Event.Schedule as IJclMonthlySchedule).IndexKind :=
    TScheduleIndexKind(ReadEnum(TxtIdentifiers[seikScheduleMonthlyIndexType], TypeInfo(TScheduleIndexKind)));
end;

procedure TTxtStore.RestoreScheduleMonthlyIndex;
begin
  (Event.Schedule as IJclMonthlySchedule).IndexValue := ReadInt(TxtIdentifiers[seikScheduleMonthlyIndex]);
end;

procedure TTxtStore.RestoreScheduleMonthlyInterval;
begin
  (Event.Schedule as IJclMonthlySchedule).Interval := ReadInt(TxtIdentifiers[seikScheduleMonthlyInterval]);
end;

procedure TTxtStore.RestoreScheduleYearlyDay;
begin
  (Event.Schedule as IJclYearlySchedule).Day := ReadInt(TxtIdentifiers[seikScheduleYearlyDay]);
end;

procedure TTxtStore.RestoreScheduleYearlyMonth;
begin
  (Event.Schedule as IJclYearlySchedule).Month := ReadInt(TxtIdentifiers[seikScheduleYearlyMonth]);
end;

procedure TTxtStore.RestoreScheduleYearlyIndexType;
begin
  (Event.Schedule as IJclYearlySchedule).IndexKind :=
    TScheduleIndexKind(ReadEnum(TxtIdentifiers[seikScheduleYearlyIndexType], TypeInfo(TScheduleIndexKind)));
end;

procedure TTxtStore.RestoreScheduleYearlyIndex;
begin
  (Event.Schedule as IJclYearlySchedule).IndexValue := ReadInt(TxtIdentifiers[seikScheduleYearlyIndex]);
end;

procedure TTxtStore.RestoreScheduleYearlyInterval;
begin
  (Event.Schedule as IJclYearlySchedule).Interval := ReadInt(TxtIdentifiers[seikScheduleYearlyInterval]);
end;

procedure TTxtStore.StoreSignature;
begin
end;

procedure TTxtStore.StoreVersion;
begin
end;

procedure TTxtStore.StoreScheduleStart;
begin
  WriteStamp(TxtIdentifiers[seikScheduleStart], Event.Schedule.StartDate);
end;

procedure TTxtStore.StoreScheduleRecurType;
begin
  WriteEnum(TxtIdentifiers[seikScheduleRecurType], Ord(Event.Schedule.RecurringType), TypeInfo(TScheduleRecurringKind));
end;

procedure TTxtStore.StoreScheduleEndType;
begin
  WriteEnum(TxtIdentifiers[seikScheduleEndType], Ord(Event.Schedule.EndType), TypeInfo(TScheduleEndKind));
end;

procedure TTxtStore.StoreScheduleEndCount;
begin
  WriteInt(TxtIdentifiers[seikScheduleEndCount], Event.Schedule.EndCount);
end;

procedure TTxtStore.StoreScheduleEndDate;
begin
  WriteStamp(TxtIdentifiers[seikScheduleEndDate], Event.Schedule.EndDate);
end;

procedure TTxtStore.StoreFreqStart;
begin
  WriteStampTime(TxtIdentifiers[seikFreqStart], (Event.Schedule as IJclScheduleDayFrequency).StartTime);
end;

procedure TTxtStore.StoreFreqEnd;
begin
  WriteStampTime(TxtIdentifiers[seikFreqEnd], (Event.Schedule as IJclScheduleDayFrequency).EndTime);
end;

procedure TTxtStore.StoreFreqInterval;
begin
  WriteStampTime(TxtIdentifiers[seikFreqInterval], (Event.Schedule as IJclScheduleDayFrequency).Interval);
end;

procedure TTxtStore.StoreScheduleDailyWeekdays;
begin
  WriteEnum(TxtIdentifiers[seikScheduleDailyWeekdays], Ord((Event.Schedule as IJclDailySchedule).EveryWeekDay),
    TypeInfo(Boolean));
end;

procedure TTxtStore.StoreScheduleDailyInterval;
begin
  WriteInt(TxtIdentifiers[seikScheduleDailyInterval], (Event.Schedule as IJclDailySchedule).Interval);
end;

procedure TTxtStore.StoreScheduleWeeklyDays;
var
  WD: TScheduleWeekDays;
begin
  WD := (Event.Schedule as IJclWeeklySchedule).DaysOfWeek;
  WriteSet(TxtIdentifiers[seikScheduleWeeklyDays], WD, TypeInfo(TScheduleWeekDays));
end;

procedure TTxtStore.StoreScheduleWeeklyInterval;
begin
  WriteInt(TxtIdentifiers[seikScheduleWeeklyInterval], (Event.Schedule as IJclWeeklySchedule).Interval);
end;

procedure TTxtStore.StoreScheduleMonthlyDay;
begin
  WriteInt(TxtIdentifiers[seikScheduleMonthlyDay], (Event.Schedule as IJclMonthlySchedule).Day);
end;

procedure TTxtStore.StoreScheduleMonthlyIndexType;
begin
  WriteEnum(TxtIdentifiers[seikScheduleMonthlyIndexType], Ord((Event.Schedule as IJclMonthlySchedule).IndexKind),
    TypeInfo(TScheduleIndexKind));
end;

procedure TTxtStore.StoreScheduleMonthlyIndex;
begin
  WriteInt(TxtIdentifiers[seikScheduleMonthlyIndex], (Event.Schedule as IJclMonthlySchedule).IndexValue);
end;

procedure TTxtStore.StoreScheduleMonthlyInterval;
begin
  WriteInt(TxtIdentifiers[seikScheduleMonthlyInterval], (Event.Schedule as IJclMonthlySchedule).Interval);
end;

procedure TTxtStore.StoreScheduleYearlyDay;
begin
  WriteInt(TxtIdentifiers[seikScheduleYearlyDay], (Event.Schedule as IJclYearlySchedule).Day);
end;

procedure TTxtStore.StoreScheduleYearlyMonth;
begin
  WriteInt(TxtIdentifiers[seikScheduleYearlyMonth], (Event.Schedule as IJclYearlySchedule).Month);
end;

procedure TTxtStore.StoreScheduleYearlyIndexType;
begin
  WriteEnum(TxtIdentifiers[seikScheduleYearlyIndexType], Ord((Event.Schedule as IJclYearlySchedule).IndexKind),
    TypeInfo(TScheduleIndexKind));
end;

procedure TTxtStore.StoreScheduleYearlyIndex;
begin
  WriteInt(TxtIdentifiers[seikScheduleYearlyIndex], (Event.Schedule as IJclYearlySchedule).IndexValue);
end;

procedure TTxtStore.StoreScheduleYearlyInterval;
begin
  WriteInt(TxtIdentifiers[seikScheduleYearlyInterval], (Event.Schedule as IJclYearlySchedule).Interval);
end;

procedure TTxtStore.BeginStruct(const StructType: TSchedEvtStructKind);
begin
  PushStruct(StructType);
  case StructType of
    seskSchedule:
      WriteLn(sTXTID_SchedGeneric);
    seskScheduleRecurInfo:
      WriteLn(sTXTID_SchedRecur);
    seskScheduleEndInfo:
      WriteLn(sTXTID_SchedEnd);
    seskScheduleDayFreq:
      WriteLn(sTXTID_SchedFreq);
    seskScheduleDaily:
      WriteLn(sTXTID_SchedDaily);
    seskScheduleWeekly:
      WriteLn(sTXTID_SchedWeekly);
    seskScheduleMonthly:
      WriteLn(sTXTID_SchedMonthly);
    seskScheduleYearly:
      WriteLn(sTXTID_SchedYearly);
  else
    raise EJVCLException.Create('Unexpected structure.');
  end;
end;

procedure TTxtStore.EndStruct;
begin
  PopStruct;
end;

procedure TTxtStore.CheckBeginStruct(const StructType: TSchedEvtStructKind);
const
  cIncorrectStructure = 'Incorrect structure found.';
var
  S: string;
begin
  PushStruct(StructType);
  S := ReadNextLine;
  case StructType of
    seskSchedule:
      if not AnsiSameText(S, sTXTID_SchedGeneric) then
        raise EJVCLException.Create(cIncorrectStructure);
    seskScheduleRecurInfo:
      if not AnsiSameText(S, sTXTID_SchedRecur) then
        raise EJVCLException.Create(cIncorrectStructure);
    seskScheduleEndInfo:
      if not AnsiSameText(S, sTXTID_SchedEnd) then
        raise EJVCLException.Create(cIncorrectStructure);
    seskScheduleDayFreq:
      if not AnsiSameText(S, sTXTID_SchedFreq) then
        raise EJVCLException.Create(cIncorrectStructure);
    seskScheduleDaily:
      if not AnsiSameText(S, sTXTID_SchedDaily) then
        raise EJVCLException.Create(cIncorrectStructure);
    seskScheduleWeekly:
      if not AnsiSameText(S, sTXTID_SchedWeekly) then
        raise EJVCLException.Create(cIncorrectStructure);
    seskScheduleMonthly:
      if not AnsiSameText(S, sTXTID_SchedMonthly) then
        raise EJVCLException.Create(cIncorrectStructure);
    seskScheduleYearly:
      if not AnsiSameText(S, sTXTID_SchedYearly) then
        raise EJVCLException.Create(cIncorrectStructure);
  else
    raise EJVCLException.Create('Unexpected structure.');
  end;
end;

procedure TTxtStore.CheckEndStruct;
begin
  PopStruct;
end;

function TTxtStore.ReadLn: string;
var
  OrgPos: Integer;
  SIdx: Integer;
  Done: Boolean;
begin
  OrgPos := Stream.Position;
  Result := '';
  SIdx := 0;
  repeat
    Inc(SIdx);
    SetLength(Result, Length(Result) + 255);
    SetLength(Result, SIdx + Stream.Read(Result[SIdx], 255));
    Done := SIdx = Length(Result);
    if not Done then
    begin
      while (SIdx < Length(Result)) and (Copy(Result, SIdx, 2) <> CrLf) do
        Inc(SIdx);
      Done := Copy(Result, SIdx, 2) = CrLf;
      if Done then
        SetLength(Result, SIdx + 1);
    end;
  until Done;
  Stream.Position := OrgPos + Length(Result);
  if Copy(Result, Length(Result) - 1, 2) = CrLf then
    SetLength(Result, Length(Result) - 2);
end;

function TTxtStore.ReadNextLine: string;
begin
  repeat
    Result := ReadLn;
  until (Trim(Result) <> '') or (Stream.Position = Stream.Size);
  Result := Trim(Result);
end;

function TTxtStore.ReadItem(out AName: string): string;
var
  I: Integer;
begin
  AName := '';
  Result := ReadNextLine;
  if Result <> '' then
  begin
    I := Pos('=', Result);
    if I > 0 then
    begin
      AName := Trim(Copy(Result, 1, I - 1));
      Result := Trim(Copy(Result, I + 1, Length(Result) - I));
    end;
  end;
end;

procedure TTxtStore.WriteLn(const S: string);
var
  S2: string;
begin
  S2 := S + CrLf;
  Stream.WriteBuffer(S2[1], Length(S2));
end;

function TTxtStore.ReadEnum(const AName: string; const TypeInfo: PTypeInfo): Integer;
var
  ItemName: string;
  Value: string;
begin
  Value := ReadItem(ItemName);
  if not AnsiSameText(AName, ItemName) then
    raise EJVCLException.Create('Incorrect identifier found.');
  Result := GetEnumValue(TypeInfo, Value);
end;

function TTxtStore.ReadInt(const AName: string): Int64;
var
  ItemName: string;
  Value: string;
begin
  Value := ReadItem(ItemName);
  if not AnsiSameText(AName, ItemName) then
    raise EJVCLException.Create('Incorrect identifier found.');
  Result := StrToInt64(Value);
end;

procedure TTxtStore.ReadSet(const AName: string; out Value; const TypeInfo: PTypeInfo);
var
  ItemName: string;
  StrValue: string;
begin
  StrValue := ReadItem(ItemName);
  if not AnsiSameText(AName, ItemName) then
    raise EJVCLException.Create('Incorrect identifier found.');
  JclStrToSet(TypeInfo, Value, StrValue);
end;

function TTxtStore.ReadStamp(const AName: string): TTimeStamp;
begin
  Result.Date := ReadStampDate(AName + '.Date');
  Result.Time := ReadStampTime(AName + '.Time');
end;

function TTxtStore.ReadStampDate(const AName: string): Integer;
var
  ItemName: string;
  Value: string;
  Y: Word;
  M: Word;
  D: Word;
begin
  Value := ReadItem(ItemName);
  if not AnsiSameText(AName, ItemName) then
    raise EJVCLException.Create('Incorrect identifier found.');
  Y := StrToInt(Copy(Value, 1, 4));
  M := StrToInt(Copy(Value, 6, 2));
  D := StrToInt(Copy(Value, 9, 2));
  Result := DateTimeToTimeStamp(EncodeDate(Y, M, D)).Date;
end;

function TTxtStore.ReadStampTime(const AName: string): Integer;
var
  ItemName: string;
  Value: string;
  H: Word;
  Min: Word;
  MSecs: Integer;
begin
  Value := ReadItem(ItemName);
  if not AnsiSameText(AName, ItemName) then
    raise EJVCLException.Create('Incorrect identifier found.');
  if (Length(Value) < 3) or (Value[3] in ['0'..'9']) then
    Result := StrToInt(Value)
  else
  begin
    H := StrToInt(Copy(Value, 1, 2));
    Min := StrToInt(Copy(Value, 4, 2));
    MSecs := StrToInt(Copy(Value, 7, 2)) * 1000 + StrToInt(Copy(Value, 10, 3));
    Result := H * 3600000 + MIn * 60000 + MSecs;
  end;
end;

procedure TTxtStore.WriteEnum(const AName: string; const Ordinal: Integer; const TypeInfo: PTypeInfo);
begin
  WriteLn(AName + ' = ' + GetEnumName(TypeInfo, Ordinal));
end;

procedure TTxtStore.WriteInt(const AName: string; const Value: Int64);
begin
  WriteLn(AName + ' = ' + IntToStr(Value));
end;

procedure TTxtStore.WriteSet(const AName: string; const Value; const TypeInfo: PTypeInfo);
begin
  WriteLn(AName + ' = ' + JclSetToStr(TypeInfo, Value));
end;

procedure TTxtStore.WriteStamp(const AName: string; const Stamp: TTimeStamp);
begin
  WriteStampDate(AName + '.Date', Stamp.Date);
  WriteStampTime(AName + '.Time', Stamp.Time);
end;

procedure TTxtStore.WriteStampDate(const AName: string; const Date: Integer);
var
  TmpStamp: TTimeStamp;
  TmpDate: TDateTime;
  Y: Word;
  M: Word;
  D: Word;
begin
  TmpStamp.Date := Date;
  TmpStamp.Time := 0;
  TmpDate := TimeStampToDateTime(TmpStamp);
  DecodeDate(TmpDate, Y, M, D);
  WriteLn(AName + ' = ' + Format('%.4d/%.2d/%.2d', [Y, M, D]));
end;

procedure TTxtStore.WriteStampTime(const AName: string; const Time: Integer);
begin
  WriteLn(AName + ' = ' + Format(
    '%.2d:%.2d:%.2d.%.3d',
    [(Time div 3600000) mod 24,
    (Time div 60000) mod 60,
      (Time div 1000) mod 60,
      Time mod 1000]));
end;

function TTxtStore.GetAttributes: TSchedEvtStoreAttributes;
begin
  Result := [sesaStructured, sesaIdentifiers];
end;

{ schedule persistency factories }

function ScheduledEventStore_Stream(const Stream: TStream;
  const Binary, OwnsStream: Boolean): IJvScheduledEventsStore;
begin
  if Binary then
    Result := TBinStore.Create(Stream, OwnsStream)
  else
    Result := TTxtStore.Create(Stream, OwnsStream);
end;

end.

