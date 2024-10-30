{
  Keyman is copyright (C) SIL Global. MIT License.

  This module provides functionality to track the execution state of the Keyman
  engine. It uses a global atom to record whether Keyman has started during the
  current session and checks if it has previously run.
}
unit Keyman.System.ExecutionHistory;


interface

const
  AtomName = 'KeymanSessionFlag';

function RecordKeymanStarted : Boolean;
function HasKeymanRun : Boolean;

implementation

uses
  System.SysUtils,
  Winapi.Windows,
  KLog;

function RecordKeymanStarted : Boolean;
var
  atom: WORD;
begin
  atom := GlobalAddAtom(AtomName);
  if atom = 0 then
  begin
    // TODO-WINDOWS-UPDATES: #10210 log to sentry
    Result := False;
  end
  else
    Result := True;
end;

function HasKeymanRun : Boolean;
begin
  Result := GlobalFindAtom(AtomName) <> 0;
  if not Result then
  begin
    if GetLastError <> ERROR_FILE_NOT_FOUND then
    begin
      // TODO-WINDOWS-UPDATES: log to Sentry
    end;
  end;
end;

end.
