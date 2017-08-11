//*************************************************************
//                          Ewb_DDE                           *
//                                                            *
//                      Freeware Unit                         *
//                     For Delphi 5 to XE                     *
//                            by                              *
//          Eran Bodankin (bsalsa) -(bsalsa@gmail.com)        *
//          Mathias Walter (mich@matze.tv)                    *
//                                                            *
//       Documentation and updated versions:                  *
//                                                            *
//               http://www.bsalsa.com                        *
//*************************************************************
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use/ change/ modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit  for the benefit
   of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

unit EwbDDE;

interface

uses
  Windows, Classes, ShellAPI, EWBAcc, Registry, EwbTools, ShlObj, EwbIEConst,
  sysUtils, ActiveX, ComObj;

type
  TEwb_DDE = class(TThread)
  end;

procedure GetDDEVariables;
function GetCommandTypeFromDDEString(szCommand: string): UINT;
function GetPathFromDDEString(szCommand: string; var szFolder: string): Boolean;
function GetPidlFromDDEString(const szCommand: string): PItemIDList;
function GetShowCmdFromDDEString(szCommand: string): Integer;
function ParseDDECommand(const szCommand: string; var szFolder: string;
  var pidl: PItemIDList; var show: Integer): UINT;
procedure DisposePIDL(ID: PItemIDList);

implementation

uses
  EwbCoreTools;

var
  FindFolder, OpenFolder, ExploreFolder, HtmlFileApp, HtmlFileTopic: string;
    //All DDE variables
  FoldersApp, FoldersTopic: string;

procedure DisposePIDL(ID: PItemIDList);
var
  Malloc: IMalloc;
begin
  if ID <> nil then
  begin
    OLECheck(SHGetMalloc(Malloc));
    Malloc.Free(ID);
  end;
end;

procedure GetDDEVariables;
var
  tmpStr: string;
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  with Reg do
  try
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey('htmlfile\shell\open\ddeexec\application', False);
    HtmlFileApp := Readstring('');
    CloseKey;
    OpenKey('htmlfile\shell\open\ddeexec\topic', False);
    HtmlFileTopic := ReadString('');
    CloseKey;
    OpenKey('Folder\shell\open\ddeexec\application', False);
    FoldersApp := Readstring('');
    CloseKey;
    OpenKey('Folder\shell\open\ddeexec\topic', False);
    FoldersTopic := ReadString('');
    CloseKey;
    OpenKey('Folder\shell\open\ddeexec', False);
    tmpStr := readString('');
    CloseKey;
    tmpStr := Copy(tmpStr, Pos('[', tmpStr) + 1, length(tmpStr));
    OpenFolder := Copy(tmpStr, 1, Pos('(', tmpStr) - 1);
    OpenKey('Folder\shell\explore\ddeexec', False);
    tmpStr := readString('');
    CloseKey;
    tmpStr := Copy(tmpStr, Pos('[', tmpStr) + 1, length(tmpStr));
    ExploreFolder := Copy(tmpStr, 1, Pos('(', tmpStr) - 1);
    OpenKey('Directory\shell\find\ddeexec', False);
    tmpStr := readString('');
    CloseKey;
    tmpStr := Copy(tmpStr, Pos('[', tmpStr) + 1, length(tmpStr));
    FindFolder := Copy(tmpStr, 1, Pos('(', tmpStr) - 1);
  finally
    Free;
  end;
end;

function GetCommandTypeFromDDEString(szCommand: string): UINT;
begin
  szCommand := Copy(szCommand, Pos('[', szCommand) + 1, length(szCommand));
  szCommand := Copy(szCommand, 1, Pos('(', szCommand) - 1);
  if szCommand = OpenFolder then
    Result := VIEW_COMMAND
  else if szCommand = ExploreFolder then
    Result := EXPLORE_COMMAND
  else if szCommand = FindFolder then
    Result := FIND_COMMAND
  else
    Result := NO_COMMAND;
end;

function GetPathFromDDEString(szCommand: string; var szFolder: string): Boolean;
begin
  szCommand := Copy(szCommand, Pos('"', szCommand) + 1, length(szCommand));
  szFolder := Copy(szCommand, 1, Pos('"', szCommand) - 1);
  Result := (szFolder <> '');
end;

function GetPidlFromDDEString(const szCommand: string): PItemIDList;
var
  PidlShared, PidlGlobal: PItemIDList;
  dwProcessId: Integer;
  hShared: THandle;
  St: string;
  ProcessID: string;
  i: Integer;
begin
  St := Copy(szCommand, Pos(',', szCommand) + 1, length(szCommand));
  i := 1;
  while not (CharInSet(St[i], IsDigit) and (i <= Length(St))) do
    Inc(i);
  ProcessID := Copy(St, i, Length(St));
  St := Copy(St, i, length(St) - 1);
  i := 1;
  while CharInSet(St[i], IsDigit) and (i <= Length(St)) do
    Inc(i);
  St := Copy(St, 1, i - 1);

  while not ((ProcessID[i] = ':') or (ProcessID[i] = ',')) and (i <=
    Length(ProcessID)) do
    Inc(i);
  if ProcessID[i] = ':' then
  begin
    ProcessID := Copy(ProcessID, i, Length(ProcessID));
    i := 1;
    while not (CharInSet(ProcessID[i], IsDigit) and (i <= Length(ProcessID)))
      do
      Inc(i);
    ProcessID := Copy(ProcessID, i, Length(ProcessID));
    i := 1;
    while (CharInSet(ProcessID[i], IsDigit)) and (i <= Length(ProcessID)) do
      Inc(i);
    if not (CharInSet(ProcessID[i], IsDigit)) then
      ProcessID := Copy(ProcessID, 1, i - 1);
  end
  else
    ProcessID := '0';
  dwProcessId := StrToInt(ProcessID);
  if dwProcessId <> 0 then
  begin
    hShared := StrToInt(St);
    PidlShared := ShLockShared(hShared, dwProcessId);
    if PidlShared <> nil then
    begin
      Result := CopyPidl(PidlShared);
      ShUnlockShared(PidlShared);
    end
    else
      Result := nil;
    ShFreeShared(hShared, dwProcessId);
  end
  else
  begin
    PidlGlobal := PItemIDList(StrToInt(St));
    Result := CopyPidl(PidlGlobal);
    _Free(PidlGlobal);
  end;
end;

function GetShowCmdFromDDEString(szCommand: string): Integer;
var
  tmpInt: Integer;
begin
  tmpInt := 1;
  while szCommand[tmpInt] <> ',' do
    Inc(tmpInt);
  Inc(tmpInt);
  while szCommand[tmpInt] <> ',' do
    Inc(tmpInt);
  szCommand := Copy(szCommand, tmpInt, Length(szCommand));
  tmpInt := 0;
  repeat
    inc(tmpInt)
  until (tmpInt > Length(szCommand)) or CharInSet(szCommand[tmpInt], IsDigit);
  if tmpInt <= length(szCommand) then
    Result := StrtoInt(szCommand[tmpInt])
  else
    Result := 1;
end;

function ParseDDECommand(const szCommand: string; var szFolder: string;
  var pidl: PItemIDList; var show: Integer): UINT;
begin
  Result := GetCommandTypeFromDDEString(szCommand);
  if Result <> NO_COMMAND then
  begin
    GetPathFromDDEString(szCommand, szFolder);
    pidl := GetPidlFromDDEString(szCommand);
    show := GetShowCmdFromDDEString(szCommand);
  end;
end;

end.
