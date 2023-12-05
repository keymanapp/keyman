(*
  Name:             utildir
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    3 Feb 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Refactor util functions into multiple units
                    06 Oct 2006 - mcdurdin - Fix KGetTempFileName not returning correct extension
                    20 Jul 2008 - mcdurdin - Raise excpetion if GetTempFileName fails to avoid possible deletion of wrong files
                    16 Jan 2009 - mcdurdin - Widestring directory functions
                    16 Jan 2009 - mcdurdin - Fix crash when two copies of Keyman start simultaneously and try and grab the same temp folder name
                    04 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
                    03 Feb 2015 - mcdurdin - I4574 - V9.0 - If any files are read-only, they need the read-only flag removed on install
*)
unit utildir;  // I3306

interface

function DirectoryEmpty(dir: WideString): Boolean;
function DirectoryExists(const Name: string): Boolean;
function ForceDirectories(Dir: string): Boolean;
procedure UnforceDirectories(Dir: string);
function EmptyDirectory(Dir: WideString): Boolean;
function RecursiveDelete(Dir: WideString): Boolean;   // I4181

function CopyFileCleanAttr(const src, dst: string; failIfExists: Boolean): Boolean;   // I4574
function DeleteFileCleanAttr(const filename: string): Boolean;   // I4574

function CreateTempPath: string;
procedure DeleteTempPath(const Path: string);

function KGetTempFileName(const ext: string = ''): string; // deprecated 'has race condition, use TTempFileManager';--> only used by debug functionality now   // I4181
function KGetTempPath: string;

function GetLongFileName(const fname: string): string;

function DosSlashes(const filename: string): string;

implementation

uses
  System.StrUtils,
  System.SysUtils,
  Winapi.Windows;

function DosSlashes(const filename: string): string;
begin
  Result := ReplaceStr(filename, '/', '\');
end;

function DirectoryEmpty(dir: WideString): Boolean;
var
    f: TSearchRec;
    n: Integer;
begin
    Result := False;
    if dir = '' then Exit;
    if dir[Length(dir)] <> '\' then dir := dir + '\';
    n := FindFirst(dir+'*.*', faDirectory, f);
    try
        while n = 0 do
        begin
            if (f.Name <> '.') and (f.Name <> '..') then Exit;
            n := FindNext(f);
        end;
    finally
        System.SysUtils.FindClose(f);
    end;
    Result := True;
end;

function EmptyDirectory(Dir: WideString): Boolean;
var
  f: TSearchRec;
  n: Integer;
begin
  Result := True;
  if dir = '' then Exit;
  if dir[Length(dir)] <> '\' then dir := dir + '\';
  n := FindFirst(dir+'*.*', faDirectory, f);
  try
    while n = 0 do
    begin
      if (f.Name <> '.') and (f.Name <> '..') then
      begin
        if (f.Attr and faDirectory) = faDirectory
          then Result := Result and EmptyDirectory(Dir + f.Name)
          else Result := Result and DeleteFileCleanAttr(Dir + f.Name);   // I4574
      end;
      n := FindNext(f);
    end;
  finally
    System.SysUtils.FindClose(f);
  end;
end;


function DirectoryExists(const Name: string): Boolean;
var
  Code: Dword;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> $FFFFFFFF) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function ForceDirectories(Dir: string): Boolean;
begin
  Result := False;
  if Length(Dir) = 0 then
    raise Exception.Create('Cannot create directory ' + Dir);
  if (AnsiLastChar(Dir) <> nil) and (AnsiLastChar(Dir)^ = '\') then
    Delete(Dir, Length(Dir), 1);
  if (Length(Dir) < 3) or DirectoryExists(Dir)
    or (ExtractFilePath(Dir) = Dir) then
    begin
      Result := True;
      Exit; // avoid 'xyz:\' problem.
    end;
  if not ForceDirectories(ExtractFilePath(Dir)) then Exit;
  Result := CreateDir(Dir);
end;

procedure UnforceDirectories(Dir: string);
{var
    f: TSearchRec;
    n: Integer;
    Found: Boolean;}
begin
    if (AnsiLastChar(Dir) <> nil) and (AnsiLastChar(Dir)^ = '\') then
        Delete(Dir, Length(Dir), 1);
    {n := FindFirst(Dir + '\*.*', faDirectory, f);
    try
        while n = 0 do
        begin
            if (f.Name <> '.') and (f.Name <> '..') then Exit;
            n := FindNext(f);
        end;
    finally
        FindClose(f);
    end;}
    if RemoveDirectory(PChar(Dir)) then UnforceDirectories(ExtractFilePath(Dir));
end;

function RecursiveDelete(Dir: WideString): Boolean;   // I4181
var
  f: TSearchRec;
  n: Integer;
begin
  if (Dir <> '') and (Dir[Length(Dir)] = '\') then
    Delete(Dir, Length(Dir), 1);

  n := FindFirst(Dir + '\*.*', faDirectory, f);
  if n = 0 then
  try
    while n = 0 do
    begin
      if (f.Name <> '.') and (f.Name <> '..') then
      begin
        if (f.Attr and faDirectory) = faDirectory then RecursiveDelete(Dir + '\' + f.Name)
        else DeleteFileCleanAttr(Dir + '\' + f.Name);   // I4574
      end;
      n := FindNext(f);
    end;
  finally
    System.SysUtils.FindClose(f);
  end;

  Result := RemoveDir(Dir);   // I4181
end;

function KGetTempPath: string;
var
  buf: array[0..260] of char;
begin
  GetTempPath(260, buf);
  Result := IncludeTrailingPathDelimiter(buf);
end;

function KGetTempFileName(const ext: string = ''): string;
var
  buf: array[0..260] of char;
begin
// This has a race condition -- with the file extension change...
  Result := ExcludeTrailingPathDelimiter(KGetTempPath);

  if GetTempFileName(PChar(Result), 'kmn', 0, buf) = 0 then
    RaiseLastOSError;

  if (ext <> '') then
  begin
    if FileExists(buf) then DeleteFileCleanAttr(buf);   // I4574
    Result := ChangeFileExt(buf, ext);
  end
  else
    Result := buf;
end;

function CreateTempPath: string;
var
  buf: array[0..260] of char;
  tempfile: string;
begin
  tempfile := '';

  repeat
    Result := ExcludeTrailingPathDelimiter(KGetTempPath);
    if (tempfile <> '') and FileExists(tempfile) then DeleteFileCleanAttr(tempfile);   // I4574

    if GetTempFileName(PChar(Result), 'kmn', 0, buf) = 0 then RaiseLastOSError;
    tempfile := buf;

    Result := ChangeFileExt(tempfile, '.dir');

    if DirectoryExists(Result) then Continue;

    if not CreateDir(Result) then
      raise Exception.Create('Unable to create temporary folder: '+SysErrorMessage(GetLastError));
  until True;

  if FileExists(tempfile) then DeleteFileCleanAttr(tempfile);   // I4574
end;

procedure DeleteTempPath(const Path: string);
begin
  RecursiveDelete(Path);
end;

function GetLongFileName(const fname: string): string;
var
  buf: array[0..260] of char;
  p: PChar;
begin
  buf[0] := #0;
  if GetFullPathName(PChar(fname), 260, buf, p) = 0
    then Result := fname
    else Result := buf;
end;

function CopyFileCleanAttr(const src, dst: string; failIfExists: Boolean): Boolean;   // I4574
begin
  // Remove read-only on destination file before overwrite
  if FileExists(dst) then
    SetFileAttributes(PChar(dst), FILE_ATTRIBUTE_NORMAL);

  Result := CopyFile(PChar(src), PChar(dst), failIfExists);
  if Result then
    // Remove read-only on destination file after copy
    SetFileAttributes(PChar(dst), FILE_ATTRIBUTE_NORMAL);
end;

function DeleteFileCleanAttr(const filename: string): Boolean;   // I4574
begin
  if FileExists(filename) then
    SetFileAttributes(PChar(filename), FILE_ATTRIBUTE_NORMAL);
  Result := System.SysUtils.DeleteFile(filename);
end;

end.

