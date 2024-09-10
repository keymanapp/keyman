unit Keyman.System.ExecutionHistory;

{
  Copyright:    © SIL Global.

  This module provides functionality to track the execution state of the Keyman
  engine. It uses a global atom to record whether Keyman has started during the
  current session and checks if it has previously run.
}

interface

const
  AtomName = 'KeymanSessionFlag';

function RecordKeymanStarted : Boolean;
function HasKeymanRun : Boolean;

implementation
uses
  System.SysUtils,KLog,
  Winapi.Windows;

function RecordKeymanStarted : Boolean;
var
  atom: WORD;
begin
 Result := False;
  try
    atom := GlobalFindAtom(AtomName);
    if atom = 0 then
    begin
      if GetLastError <> ERROR_FILE_NOT_FOUND then
        RaiseLastOSError;
      atom := GlobalAddAtom(AtomName);
      Result := True;
      if atom = 0 then
        RaiseLastOSError;
    end;
  except
    on E: Exception do
      KL.Log(E.ClassName + ': ' + E.Message);
  end;
end;

function HasKeymanRun : Boolean;
var
  atom: WORD;
begin
  Result := False;
  try
    atom := GlobalFindAtom(AtomName);
    if atom <> 0 then
    begin
      if GetLastError <> ERROR_SUCCESS then
        RaiseLastOSError;
      Result := True;
    end
    else
    begin
      Result := False;
    end;

  except
    on E: Exception do
      KL.log(E.ClassName + ': ' + E.Message);
  end;

end;

end.
