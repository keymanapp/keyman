(*
  Name:             si_overview
  Copyright:        Copyright (C) SIL International.
  Documentation:    km4.1
  Description:      get basic system details
  Create Date:      13 May 2005

  Modified Date:    23 Dec 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          13 May 2005 - mcdurdin - Integrated into kmshell from tsysinfo
                    09 Jun 2005 - mcdurdin - Use MSXML_TLB not MSXML2_TLB
                    19 Jul 2011 - mcdurdin - I3000 - Tweak display of diagnostics using .xslt files
                    03 Oct 2011 - mcdurdin - I2919 - Unicode support, x64 reporting and xslts
                    23 Dec 2011 - mcdurdin - I3180 - x64 process details for tsysinfo
*)
unit si_overview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, si_base, StdCtrls;

type
  TSI_Overview = class(TSI_Base)
  protected
    function DoCollect: Boolean; override;
    class function GetCaption: String; override;
  end;

implementation

uses msxml, comobj, winver, sysinfo_util;

{ TSI_Overview }

class function TSI_Overview.GetCaption: String;
begin
  Result := 'Overview';
end;

type
  TIsWow64Process2 = function(hProcess: THandle; out pProcessMachine: USHORT; out pNativeMachine: USHORT): BOOL; stdcall;

function TSI_Overview.DoCollect: Boolean;
var
  n, nn, node: IXMLDOMNode;
  s: string;
  i: Integer;
  p: Pchar;
  vi: TOSVersionInfo;
  buf: array[0..1023] of char;
  freespace, totalspace, totalfree: Int64;
  stat: TMemoryStatus;
  processMachine: USHORT;
  nativeMachine: USHORT;
  IsWow64Process2: TIsWow64Process2;
begin
{
SYSTEM INFO:
  Windows version
  Service pack
  Free disk space on hard drives
  Memory
  MS Office version
}
  node := xmlAddChild(rootnode,'Overview');

  { Windows Version and Service Pack }

  FillChar(vi, sizeof(TOSVersionInfo), 0);
  vi.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(vi);

  s := vi.szCSDVersion;
  n := xmlAddChild(node, 'WindowsVersion');
  n.Text := XMLTextEncode(GetWindowsVersionInfo);
  xmlSetAttribute(n,'MajorVersion', IntToStr(vi.dwMajorVersion));
  xmlSetAttribute(n,'MinorVersion', IntToStr(vi.dwMinorVersion));
  xmlSetAttribute(n,'BuildNumber', IntToStr(vi.dwBuildNumber));
  xmlSetAttribute(n,'PlatformID', IntToStr(vi.dwPlatformId));
  xmlSetAttribute(n,'CSDVersion', XMLTextEncode(s));

  if Wow64Process // I2919
    then xmlSetAttribute(n,'x64', 'true')
    else xmlSetAttribute(n,'x64', 'false');

  IsWow64Process2 := GetProcAddress(GetModuleHandle(kernel32), 'IsWow64Process2');
  if Assigned(IsWow64Process2) then
  begin
    if IsWow64Process2(GetCurrentProcess, processMachine, nativeMachine) then
    begin
      // https://learn.microsoft.com/en-us/windows/win32/sysinfo/image-file-machine-constants
      xmlSetAttribute(n, 'ProcessMachine', IntToHex(processMachine, 4));
      xmlSetAttribute(n, 'NativeMachine', IntToHex(nativeMachine, 4));
    end;
  end;

  { Free disk space }

  n := xmlAddChild(node,'DiskSpace');
  i := GetLogicalDriveStrings(1024, buf);
  if (i > 0) and (i < 1024) then
  begin
    p := buf;
    while p^ <> #0 do
    begin
      if (GetDriveType(p) = DRIVE_FIXED) and GetDiskFreeSpaceEx(p, freespace, totalspace, @totalfree) then
      begin
        nn := xmlAddChild(n,'HardDrive');
        s := p;
        xmlSetAttribute(nn,'Drive', XMLTextEncode(s));
        xmlSetAttribute(nn,'QuotaFreeSpace', IntToStr(freespace div 1024 div 1024)+'MB');
        xmlSetAttribute(nn,'TotalSpace', IntToStr(totalspace div 1024 div 1024)+'MB');
        xmlSetAttribute(nn,'TotalFreeSpace', IntToStr(totalfree div 1024 div 1024)+'MB');
      end;
      p := StrScan(p, #0); Inc(p);
    end;
  end;

  { Memory }

  n := xmlAddChild(node,'Memory');

  GlobalMemoryStatus(stat);

  xmlAddChild(n,'MemoryLoad').Text := IntToStr(stat.dwMemoryLoad);
  xmlAddChild(n,'TotalPhysical').Text := IntToStr(stat.dwTotalPhys div 1024 div 1024)+'MB';
  xmlAddChild(n,'FreePhysical').Text := IntToStr(stat.dwAvailPhys div 1024 div 1024)+'MB';
  xmlAddChild(n,'TotalPageFile').Text := IntToStr(stat.dwTotalPageFile div 1024 div 1024)+'MB';
  xmlAddChild(n,'FreePageFile').Text := IntToStr(stat.dwAvailPageFile div 1024 div 1024)+'MB';
  xmlAddChild(n,'TotalVirtual').Text := IntToStr(stat.dwTotalVirtual div 1024 div 1024)+'MB';
  xmlAddChild(n,'FreeVirtual').Text := IntToStr(stat.dwAvailVirtual div 1024 div 1024)+'MB';

  { TODO: MS Office version(s) }

  Result := True;
end;

initialization
  TSI_Overview.Register;
end.
