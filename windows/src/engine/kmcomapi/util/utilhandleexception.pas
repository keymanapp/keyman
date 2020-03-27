(*
  Name:             utilhandleexception
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      19 Mar 2007

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          19 Mar 2007 - mcdurdin - Remove forms.pas dependency
                    28 Jul 2008 - mcdurdin - I1574 - Use new global reporting of exceptions
                    18 Mar 2011 - mcdurdin - I2824 - Consolidate logging in Diag folder
                    08 Jun 2012 - mcdurdin - I3337 - V9.0 - Review of input/output for Unicode
*)
unit utilhandleexception;

interface

uses
  System.SysUtils;

procedure LogException(const SourceClassName: string; E: Exception; ExceptAddr: Pointer); overload;
procedure LogException(E: Exception); overload;

implementation

uses
  System.Classes,
  System.JSON,
  Winapi.Windows,

  KeymanPaths,
  JsonUtil,
  KLog,
  utildir,
  VersionInfo;

function RtlCaptureStackBackTrace(FramesToSkip, FramesToCapture: DWORD; BackTrace: Pointer; BackTraceHash: PDWORD): WORD; stdcall; external 'ntdll.dll';

//
// Capture a stack trace and include the offending crash address at the top of
// the trace. Apart from skipping frames and the inclusion of TopAddr, this is
// very similar to sentry_event_value_add_stacktrace.
//
function CaptureStackTrace(TopAddr: Pointer; FramesToSkip: DWORD): TJSONArray;
var
  walked_backtrace: array[0..255] of Pointer;
  frameCount: Word;
  i: Integer;
begin
  Result := TJSONArray.Create;

  frameCount := RtlCaptureStackBackTrace(FramesToSkip, 256, @walked_backtrace[0], nil);
  if frameCount = 0 then
    Exit;

  for i := Integer(frameCount) - 1 downto 0 do
    Result.Add(Format('0x%x', [NativeUInt(walked_backtrace[i])]));

  // Insert the except address at the top of the stack
  if TopAddr <> nil then
    Result.Add(Format('0x%x', [NativeUInt(TopAddr)]));
end;

procedure LogException(const SourceClassName: string; E: Exception; ExceptAddr: Pointer);
const
  Size = 1024;
var
  errlog: TStringList;
  errlogfile: string;
  stack: TJSONArray;
  o: TJSONObject;
  Buffer: array[0..Size-1] of Char;
begin
  stack := CaptureStackTrace(ExceptAddr, 0);

  errlog := TStringList.Create;
  try
    errlogfile := TKeymanPaths.ErrorLogPath('kmcomapi');  // I2824

    o := TJSONObject.Create;
    o.AddPair('sourceClassName', SourceClassName);
    o.AddPair('exception', E.ClassName);

    if ExceptionErrorMessage(E, ExceptAddr, Buffer, Size) > 0
      then o.AddPair('message', Buffer)
      else o.AddPair('message', E.Message);
    o.AddPair('stack', stack);
    PrettyPrintJSON(o, errlog, 2);

    with TStringStream.Create(errlog.Text, TEncoding.UTF8) do
    try
      // Use TStringStream to avoid BOM from TStringList
      SaveToFile(errlogfile);
    finally
      Free;
    end;
  finally
    errlog.Free;
  end;
end;

procedure LogException(E: Exception);
begin
  LogException('', E, ExceptAddr);
end;

end.

