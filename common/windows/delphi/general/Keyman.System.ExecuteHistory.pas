unit Keyman.System.ExecuteHistory;

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
      if GetLastError <> ERROR_SUCCESS then
        RaiseLastOSError;
      atom := GlobalAddAtom(AtomName);
      KL.Log('RecordKeymanStarted: True');
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

       KL.Log('HasKeymanRun: Keyman Has Run');
      Result := True;
    end
    else
      Result := False;
  except
    on E: Exception do
      KL.log(E.ClassName + ': ' + E.Message);
  end;

end;

end.
