(*
  Name:             Keyman.Developer.System.Project.ProjectLog
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      11 May 2015

  Modified Date:    11 May 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          11 May 2015 - mcdurdin - I4706 - V9.0 - Update compile logging for silent and warning-as-error cleanness
                    
*)
unit Keyman.Developer.System.Project.ProjectLog;   // I4706

interface

type
  TProjectLogState = (
    plsInfo,     // informational only, process-oriented
    plsHint,     // code-level hint
    plsWarning,  // code-level warning
    plsError,    // code-level error
    plsFatal,    // code-level or system-level fatal error, unexpected
    plsSuccess,  // Overall success of build
    plsFailure); // Overall failure of build

type
  TCompilePackageMessageEvent = procedure(Sender: TObject; msg: string; State: TProjectLogState) of object;

  TProjectLogEvent = procedure(state: TProjectLogState; filename, msg: string; msgcode, line: Integer);
  TProjectLogObjectEvent = procedure(state: TProjectLogState; filename, msg: string; msgcode, line: Integer) of object;

const
  ProjectLogStateTitle: array[TProjectLogState] of string = (
    '',
    'Hint',
    'Warning',
    'Error',
    'Fatal Error',
    'Success',
    'Failure'
  );

type
  TProjectLog = class sealed
    class function FormatMessage(state: TProjectLogState; filename, msg: string; code, line: Integer): string;
  end;

implementation

uses
  System.SysUtils;

{ TProjectLog }

class function TProjectLog.FormatMessage(state: TProjectLogState; filename,
  msg: string; code, line: Integer): string;
var
  Prefix: string;
begin
  Prefix := ProjectLogStateTitle[state];
  if Prefix <> '' then Prefix := Prefix + ': ';

  msg := StringReplace(msg, #13#10, '   ', [rfReplaceAll]);

  if line = 0
    then filename := ExtractFileName(filename)
    else filename := Format('%s (%d)', [ExtractFileName(filename), line]);

  if code = 0
    then Result := Format('%s: %s%s', [filename, Prefix, msg])
    else Result := Format('%s: %s%4.4x %s', [filename, Prefix, code, msg]);
end;

end.
