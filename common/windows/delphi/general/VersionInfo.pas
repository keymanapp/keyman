(*
  Name:             VersionInfo
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Dec 2006

  Modified Date:    4 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Jan 2007 - mcdurdin - Remove CompareVersions function
                    05 Jun 2007 - mcdurdin - I817 - Fix version lookup for 9x DLLs
                    20 Jun 2007 - mcdurdin - Widestring
                    27 Mar 2008 - mcdurdin - Add CompareVersions function
                    04 May 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
*)
unit VersionInfo;

interface

uses
  Winapi.Windows;

function GetVersionString: WideString;
function GetMajorMinorVersionString: string;
procedure GetVersionBuild(var vMajor, vMinor: Integer);
function GetVersionCopyright: WideString;
function GetFileVersionString(const FileName: WideString): WideString;
function GetFileVersionBuild(const FileName: WideString; var VersionMajor, VersionMinor: Integer): Boolean;

function CompareVersions(Version1, Version2: string): Integer; // -1 = Version 1 newer, 1 = Version2 newer

implementation

uses
  System.SysUtils,
  System.StrUtils;

var
  VersionMajor, VersionMinor: Integer;

const
  SVersionCopyright = 'Copyright © SIL International.  All Rights Reserved.';

function GetVersionString: WideString;
begin
  Result := IntToStr(HiWord(VersionMajor)) + '.' +  // I3310
            IntToStr(LoWord(VersionMajor)) + '.' +
            IntToStr(HiWord(VersionMinor)) + '.' +
            IntToStr(LoWord(VersionMinor));
end;

function GetMajorMinorVersionString: string;
begin
  Result := IntToStr(HiWord(VersionMajor)) + '.' +  // I3310
            IntToStr(LoWord(VersionMajor));
end;

procedure GetVersionBuild(var vMajor, vMinor: Integer);
begin
  vMajor := VersionMajor;
  vMinor := VersionMinor;
end;

function GetVersionCopyright: WideString;
begin
  Result := SVersionCopyright;
end;

procedure InitVersion;
var
  buf:PByte;
  hres: HRsrc;
  hmem: HGlobal;
{  p: Pointer;
  len: Cardinal;
  fileinfo: PVSFixedFileInfo;
}
begin
  hres := FindResource{TNT-ALLOW FindResource}(0, MAKEINTRESOURCE(1), RT_VERSION);
  if hres <> 0 then
  begin
    hmem := LoadResource(0, hres);
    buf := PByte(LockResource(hmem));  // I3310

    {if not VerQueryValue(buf, '\', p, len) then Exit;
    fileinfo := p;
    VersionMajor := fileinfo.dwProductVersionMS;
    VersionMinor := fileinfo.dwProductVersionLS;}

    VersionMajor := PLongInt(@buf[$30])^;
    VersionMinor := PLongInt(@buf[$34])^;
  end;
end;

function GetFileVersionString(const FileName: WideString): WideString;
var
  VersionMajor, VersionMinor: Integer;
begin
  if GetFileVersionBuild(FileName, VersionMajor, VersionMinor) then
  begin
    Result := IntToStr(HiWord(VersionMajor)) + '.' +  // I3310
              IntToStr(LoWord(VersionMajor)) + '.' +
              IntToStr(HiWord(VersionMinor)) + '.' +
              IntToStr(LoWord(VersionMinor));
  end
  else
    Result := '';
end;

function GetFileVersionBuild(const FileName: WideString; var VersionMajor, VersionMinor: Integer): Boolean;
var
  sz, dw: DWord;
  buf: PByte; 
  fileinfo: PVSFixedFileInfo;
  p: Pointer;
  len: Cardinal;
begin
  Result := False;
  VersionMajor := 0;
  VersionMinor := 0;

  sz := GetFileVersionInfoSize(PChar(FileName), dw);  // I3310
  if sz = 0 then Exit;
  GetMem(buf, sz);
  if buf = nil then Exit;
  if not GetFileVersionInfo(PChar(FileName), dw, sz, buf) then Exit;  // I3310

  if not VerQueryValue(buf, '\', p, len) then Exit;  // I3310
  fileinfo := p;
  VersionMajor := fileinfo.dwProductVersionMS;
  VersionMinor := fileinfo.dwProductVersionLS;

  FreeMem(buf);
  Result := True;
end;

/// <summary>
/// -1 = Version 1 newer, 1 = Version2 newer
/// </summary>
function CompareVersions(Version1, Version2: string): Integer;
var
  n1, n2: Integer;
  v1: Integer;
  v2: Integer;
begin
  Result := 0;

  if Version1 = Version2 then
    Exit; // Identical - save effort

  repeat
    n1 := Pos('.', Version1); if n1 = 0 then n1 := Length(Version1)+1;
    n2 := Pos('.', Version2); if n2 = 0 then n2 := Length(Version2)+1;

    if not TryStrToInt(Copy(Version1, 1, n1-1), v1) then
    begin
      Result := 1;
      Exit; // 1.2.3.x comparing to 1.2.3.4 - Version2 is newer
    end;
    if not TryStrToInt(Copy(Version2, 1, n2-1), v2) then
    begin
      Result := -1;
      Exit; // 1.2.3.4 comparing to 1.2.3.x - Version1 is newer
    end;

    if v1 > v2 then
    begin
      Result := -1;
      Exit; // 1.2.3 vs 1.2.2
    end;

    if v1 < v2 then
    begin
      Result := 1; //IsNewer := False;
      Exit; // 1.2.2 vs 1.2.3
    end;

    Delete(Version1, 1, n1);
    Delete(Version2, 1, n2);
  until (Version1='') or (Version2='');

  if Version1='' then
  begin
    if Version2='' then
      Exit; // 1.02.3 vs 1.2.3 - same version, just formatting difference
    // 1.2 vs 1.2.3 - Version2 is newer
    Result := 1;
  end
  else
    // 1.2.3 vs 1.2 - Version1 is newer
    Result := -1;
end;

initialization
  InitVersion;
end.

