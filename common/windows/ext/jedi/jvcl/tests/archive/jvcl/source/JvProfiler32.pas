{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvProfiler32.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Certified Software Corp. [certsoft@quest-net.com]
Portions created by Peter Th�rnqvist are Copyright (C) 1996 Certified Software Corp.
All Rights Reserved.

Contributor(s): Peter Th�rnqvist [peter3@peter3.com]

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
 Use QueryPerformanceCounter / Frequency instead of GetTickCount (the high resolution timer)
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvProfiler32;

interface

uses
  Windows, Dialogs, ComCtrls, StdCtrls, Controls, Classes, ExtCtrls, Forms,
  JvComponent;

const
  MaxProfEntries = 1024; { maximum number of "blocks" to profile }
  MaxStackSize = 1024;   { maximum nesting of blocks at any one time }

var
  OddClick: Boolean = True;

type
  TJvProfileInfo = array [0..MaxProfEntries - 1] of record
    InOutTime: Longint;
    TimeSpent: Longint;
    Calls: Longint;
    StringID: string;
  end;

  TProcStack = array [1..MaxStackSize] of record
    CallerID: Integer;
    EntryTime: Longint;
  end;

  TProfReport = class(TForm)
    Panel1: TPanel;
    SaveBtn: TButton;
    lvReport: TListView;
    Panel2: TPanel;
    OKBtn: TButton;
    TrimBtn: TButton;
    procedure FormShow(Sender: TObject);
    procedure lvReportColumnClick(Sender: TObject; Column: TListColumn);
    procedure SaveBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure TrimBtnClick(Sender: TObject);
  public
    StartTime: Integer;
    EndTime: Integer;
    LastProc: Integer;
    ProfileInfo: TJvProfileInfo;
  end;

  TJvProfiler = class(TJvComponent)
  private
    FProfileInfo: TJvProfileInfo;
    FNames: TStrings;
    FStack: TProcStack;
    FStartTime: Longint;
    FEndTime: Longint;
    FLastProc: Integer;
    FStackSize: Integer;
    FEnabled: Boolean;
    FStarted: Boolean;
    FSorted: Boolean;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    procedure SetNames(Value: TStrings);
    procedure SetEnabled(Value: Boolean);
    procedure SetSorted(Value: Boolean);
  protected
    procedure DoStart; virtual;
    procedure DoStop; virtual;
    procedure Initialize; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure EnterID(ID: Integer);
    procedure EnterName(const Name: string);
    procedure ExitName(const Name: string);
    procedure ExitID(ID: Integer);
    procedure Stop;
    procedure ShowReport;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Names: TStrings read FNames write SetNames;
    property Sorted: Boolean read FSorted write SetSorted default False;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
  end;

implementation

uses
  SysUtils, CommCtrl,
  JvTypes;

const
  DefCaption = 'Profiler 32 Report';
  EmptyLine = '0.00';
  DefHeader = 'Profiler 32 run %s by "%s" (machine %s).';
  DefHeader2 =
    'Profiler 32 - (C) 1996 Certified Software Corp, portions Copyright (C) 1997 by Peter Th�rnqvist; all rights reserved.';

{$R *.DFM}

type
  PProfType = ^TProfType;
  TProfType = record
    InOutTime: Integer;
    TimeSpent: Integer;
    Calls: Integer;
    StringID: string;
  end;

  PStackType = ^TStackType;
  TStackType = record
    CallerID: Integer;
    EntryTime: Integer;
  end;

function GetUserNamePas: string;
var
  Buff: array [0..255] of Char;
  I: Cardinal;
begin
  I := 255;
  GetUserName(Buff, I);
  Result := Buff;
end;

function GetComputerNamePas: string;
var
  Buff: array [0..255] of Char;
  I: Cardinal;
begin
  I := 255;
  GetComputerName(Buff, I);
  Result := Buff;
end;

//=== TJvProfiler ============================================================

constructor TJvProfiler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNames := TStringList.Create;
end;

destructor TJvProfiler.Destroy;
begin
  Stop;
  FNames.Free;
  inherited Destroy;
end;

procedure TJvProfiler.Initialize;
var
  I: Integer;
begin
  FEnabled := False;
  FStarted := False;
  FStartTime := 0;
  FEndTime := 0;
  FStackSize := 0;
  FLastProc := -1;
  { build ID list }
  for I := 0 to FNames.Count - 1 do
  begin
    if Length(Trim(FNames[I])) < 1 then
      Continue;                         { skip empty ID's }
    if FLastProc > MaxProfEntries then
      raise EJVCLException.CreateFmt('Max number of ID''s exceeded (%d)',
        [MaxProfEntries - 1]);
    Inc(FLastProc);
    with FProfileInfo[FLastProc] do
    begin
      TimeSpent := 0;
      Calls := 0;
      StringID := FNames[I];
    end;
  end;
end;

procedure TJvProfiler.EnterID(ID: Integer);
var
  Snap: Integer;
begin
  if FEnabled then
  begin
    Snap := GetTickCount;
    if FStackSize > MaxStackSize then
      raise EJVCLException.CreateFmt('Max stack size exceeded (%d)',
        [MaxStackSize]);
    Inc(FStackSize);

    with FStack[FStackSize] do
    begin
      EntryTime := Snap;
      CallerID := ID
    end;

    with FProfileInfo[ID] do
    begin
      Inc(Calls);
      InOutTime := Snap;
    end;
  end;
end;

procedure TJvProfiler.EnterName(const Name: string);
begin
  EnterID(TStringLIst(FNames).IndexOf(Name));
end;

procedure TJvProfiler.ExitName(const Name: string);
begin
  ExitID(TStringList(FNames).IndexOf(Name));
end;

procedure TJvProfiler.ExitID(ID: Integer);
var
  Snap, Elapsed: Integer;
begin
  if Enabled then
  begin
    Snap := GetTickCount;
    with FProfileInfo[ID] do
    begin
      Elapsed := Snap - InOutTime;
      TimeSpent := TimeSpent + Elapsed;
    end;
    if FStackSize > 0 then
      Dec(FStackSize);
    if FStackSize > 0 then
      with FProfileInfo[FStack[FStackSize].CallerID] do
        TimeSpent := TimeSpent - Elapsed;
  end;
end;

procedure TJvProfiler.DoStart;
begin
  if Assigned(FOnStart) then
    FOnStart(self);
end;

procedure TJvProfiler.DoStop;
begin
  if Assigned(FOnStop) then
    FOnStop(self);
end;

procedure TJvProfiler.Start;
begin
  if FEnabled and not FStarted then
  begin
    //    Initialize;
    DoStart;
    FStartTime := GetTickCount;
    FStarted := True;
  end;
end;

procedure TJvProfiler.Stop;
begin
  if FEnabled and FStarted then
  begin
    FEndTime := GetTickCount;
    DoStop;
    FStarted := False;
  end;
end;

procedure TJvProfiler.SetNames(Value: TStrings);
begin
  FNames.Assign(Value);
  Initialize;
end;

procedure TJvProfiler.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
    FEnabled := Value;
end;

procedure TJvProfiler.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    TStringList(FNames).Sorted := FSorted;
    Initialize;
  end;
end;

procedure TJvProfiler.ShowReport;
begin
  if FEnabled then
  begin
    if FStarted then
      Stop;
    with TProfReport.Create(nil) do
    begin
      EndTime := FEndTime;
      StartTime := FStartTime;
      LastProc := FLastProc;
      ProfileInfo := FProfileInfo;
      ShowModal;
      Free;
    end;
  end;
end;

//=== TProfReport ============================================================

procedure TProfReport.FormShow(Sender: TObject);
var
  ThisProc: Integer;
  TotalSum: Integer;
  LItem: TListItem;
begin
  OddClick := True;
  TotalSum := (EndTime - StartTime);
  if TotalSum = 0 then
    Exit;
  lvReport.Items.BeginUpdate;
  lvReport.Items.Clear;
  for ThisProc := 0 to LastProc do
    with ProfileInfo[ThisProc] do
    begin
      LItem := lvReport.Items.Add;
      LItem.Caption := StringID;        { function ID }
      if Calls <> 0 then
      begin
        LItem.SubItems.Add(Format('%4.2f', [TimeSpent * 1.0]));  { Total time spent here }
        LItem.Subitems.Add(IntToStr(Calls)); { Total number of calls }
        LItem.SubItems.Add(Format('%4.2f', [TimeSpent / Calls]));  { average time }
        LItem.SubItems.Add(Format('%4.2f', [TimeSpent / TotalSum * 100]));  { percentage }
      end
      else
      begin
        LItem.SubItems.Add(EmptyLine);
        LItem.SubItems.Add('0');
        LItem.SubItems.Add(EmptyLine);
        LItem.SubItems.Add(EmptyLine);
      end;
    end;
  Caption := Format('%s -  total elapsed time: %d (ms)', [DefCaption, TotalSum]);
  lvReport.Items.EndUpdate;
end;

function IsFloat(S: string): Boolean;
begin
  Result := True;
  try
    StrToFloat(S);
  except
    Result := False;
  end;
end;

function DefSort(lParam1, lParam2: TListItem; lParamSort: Integer): Integer; stdcall;
var
  l1, l2: Extended;
begin
  if lParamSort = 0 then
    Result := AnsiCompareText(lParam1.Caption, lParam2.Caption)
  else
  begin
    if not IsFloat(lParam1.SubItems[lParamSort - 1]) then
      l1 := -1
    else
      l1 := StrToFloat(lParam1.SubItems[lParamSort - 1]);
    if not IsFloat(lParam2.SubItems[lParamSort - 1]) then
      l2 := -1
    else
      l2 := StrToFloat(lParam2.SubItems[lParamSort - 1]);
    Result := round((l1 * 1000) - (l2 * 1000));
  end;
  if OddClick then
    Result := -Result;
end;

procedure TProfReport.lvReportColumnClick(Sender: TObject; Column: TListColumn);
begin
  //  lvReport.Items.BeginUpdate;
  lvReport.CustomSort(TLVCompare(@DefSort), Column.Index);
  OddClick := not OddClick;
  //  lvReport.Items.EndUpdate;
end;

procedure TProfReport.SaveBtnClick(Sender: TObject);
var
  OutList: TStringList;
  S: string;
  I, j: Integer;
begin
  with TSaveDialog.Create(nil) do
  begin
    Filter := 'Text formats|*.asc;*.txt;*.inf;*.doc|All files|*.*';
    if Execute then
    begin
      OutList := TStringList.Create;
      OutList.Add(Format(DefHeader, [DateToStr(Now), GetUserNamePas,
        GetComputerNamePas]));
      OutList.Add(DefHeader2);
      S := '';
      for I := 0 to lvReport.Columns.Count - 1 do
        S := S + lvReport.Columns[I].Caption + #9;
      OutList.Add(S);
      S := '';
      with lvReport do
        for I := 0 to Items.Count - 1 do
        begin
          with Items[I] do
          begin
            S := S + Caption + #9;
            for j := 0 to SubItems.Count - 1 do
              S := S + SubItems[j] + #9;
            OutList.Add(S);
          end;
          S := '';
        end;
      OutList.SaveToFile(Filename);
      OutList.Free;
    end;
    Free;
  end;
end;

procedure TProfReport.OKBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TProfReport.TrimBtnClick(Sender: TObject);
var
  I: Integer;
begin
  lvReport.Items.BeginUpdate;
  for I := lvReport.Items.Count - 1 downto 0 do
    { no calls = not used }
    if AnsiCompareText(lvReport.Items[I].SubItems[1], '0') = 0 then
      lvReport.Items.Delete(I);
  lvReport.Items.EndUpdate;
end;

end.

