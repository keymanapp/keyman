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
      //writeln('The Sample Keyman Session Flag has not been set, so the process have never been started in this session.');
      atom := GlobalAddAtom(AtomName);
      Result := True;
      if atom = 0 then
        RaiseLastOSError;
    end;
    //else
      //writeln('The process has been started previously because the Sample Keyman Session Flag has been set.');

    //writeln;
    //writeln('* The Sample Keyman Session Flag atom is: '+IntToStr(atom));
    //writeln;
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
      Result := False;
  except
    on E: Exception do
      KL.log(E.ClassName + ': ' + E.Message);
  end;

end;

end.
