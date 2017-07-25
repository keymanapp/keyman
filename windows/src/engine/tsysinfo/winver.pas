(*
  Name:             winver
  Copyright:        Copyright (C) SIL International.
  Documentation:    km4.1
  Description:      get details Windows version information
  Create Date:      13 May 2005

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          13 May 2005 - mcdurdin - Integrated into kmshell from tsysinfo
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    08 Jun 2012 - mcdurdin - I3309 - V9.0 - Migrate to Delphi XE2, VS2010, svn 1.7
*)
unit winver;  // I3309

interface

function GetWindowsVersionInfo: string;

implementation

uses Windows, SysUtils, ErrorControlledRegistry;

//const
//  BUFSIZE = 80;

{type
  OSVERSIONINFOEX = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of char;
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved: BYTE;
  end;
  TOsVersionInfoEx = OSVERSIONINFOEX;
  POsVersionInfoEx = ^TOsVersionInfoEx;

function GetVersionEx(lpVersionInformation: POSVersionInfo): BOOL; stdcall; external kernel32 name 'GetVersionExA';

const
  VER_NT_WORKSTATION = 1;
  VER_NT_SERVER = 3;

  VER_SUITE_SMALLBUSINESS            =$00000001;
  VER_SUITE_ENTERPRISE               =$00000002;
  VER_SUITE_BACKOFFICE               =$00000004;
  VER_SUITE_COMMUNICATIONS           =$00000008;
  VER_SUITE_TERMINAL                 =$00000010;
  VER_SUITE_SMALLBUSINESS_RESTRICTED =$00000020;
  VER_SUITE_EMBEDDEDNT               =$00000040;
  VER_SUITE_DATACENTER               =$00000080;
  VER_SUITE_SINGLEUSERTS             =$00000100;
  VER_SUITE_PERSONAL                 =$00000200;
  VER_SUITE_BLADE                    =$00000400;
  VER_SUITE_EMBEDDED_RESTRICTED      =$00000800;
  VER_SUITE_SECURITY_APPLIANCE       =$00001000;
}

function GetWindowsVersionInfo: string;
var
  osvi: TOsVersionInfoEx;
  bOsVersionInfoEx: Boolean;
  s: string;
begin
  Result := '';

  // Try calling GetVersionEx using the OSVERSIONINFOEX structure.
  // If that fails, try using the OSVERSIONINFO structure.

  ZeroMemory(@osvi, sizeof(OSVERSIONINFOEX));
  osvi.dwOSVersionInfoSize := sizeof(OSVERSIONINFOEX);

  bOsVersionInfoEx := GetVersionEx (osvi);  // I3309
  if not bOsVersionInfoEx then
  begin
    osvi.dwOSVersionInfoSize := sizeof(TOsVersionInfo);
    if not GetVersionEx(osvi) then Exit;  // I3309
  end;

  case osvi.dwPlatformId of
    // Test for the Windows NT product family.
    VER_PLATFORM_WIN32_NT:
      begin
        // Test for the specific product family.
        if (osvi.dwMajorVersion = 5) and (osvi.dwMinorVersion = 2) then
          Result := Result + 'Microsoft Windows Server 2003 family, ';
        if (osvi.dwMajorVersion = 5) and (osvi.dwMinorVersion = 1) then
          Result := Result + 'Microsoft Windows XP ';
        if (osvi.dwMajorVersion = 5) and (osvi.dwMinorVersion = 0) then
          Result := Result + 'Microsoft Windows 2000 ';

        if osvi.dwMajorVersion <= 4 then
          Result := Result + 'Microsoft Windows NT ';

        // Test for specific product on Windows NT 4.0 SP6 and later.
        if bOsVersionInfoEx then
        begin
          // Test for the workstation type.
          if osvi.wProductType = VER_NT_WORKSTATION then
          begin
            if osvi.dwMajorVersion = 4 then
              Result := Result + 'Workstation 4.0 '
            else if (osvi.wSuiteMask and VER_SUITE_PERSONAL) = VER_SUITE_PERSONAL then
              Result := Result + 'Home Edition '
            else
              Result := Result + 'Professional ';
          end
          // Test for the server type.
          else if osvi.wProductType = VER_NT_SERVER then
          begin
            if (osvi.dwMajorVersion = 5) and (osvi.dwMinorVersion = 2) then
            begin
              if (osvi.wSuiteMask and VER_SUITE_DATACENTER) = VER_SUITE_DATACENTER then
                Result := Result + 'Datacenter Edition '
              else if (osvi.wSuiteMask and VER_SUITE_ENTERPRISE) = VER_SUITE_ENTERPRISE then
                Result := Result + 'Enterprise Edition '
              else if (osvi.wSuiteMask = VER_SUITE_BLADE) then
                Result := Result + 'Web Edition '
              else
                Result := Result + 'Standard Edition ';
            end
            else if (osvi.dwMajorVersion = 5) and (osvi.dwMinorVersion = 0) then
            begin
              if (osvi.wSuiteMask and VER_SUITE_DATACENTER) = VER_SUITE_DATACENTER then
                Result := Result + 'Datacenter Server '
              else if (osvi.wSuiteMask and VER_SUITE_ENTERPRISE) = VER_SUITE_ENTERPRISE then
                Result := Result + 'Advanced Server '
              else
                Result := Result + 'Server ';
            end
            else  // Windows NT 4.0
            begin
              if (osvi.wSuiteMask and VER_SUITE_ENTERPRISE) = VER_SUITE_ENTERPRISE then
                Result := Result + 'Server 4.0, Enterprise Edition '
              else
                Result := Result + 'Server 4.0 ';
            end;
          end;
        end
        else  // Test for specific product on Windows NT 4.0 SP5 and earlier
        begin
          with TRegistryErrorControlled.Create do  // I2890
          try
            RootKey := HKEY_LOCAL_MACHINE;
            if OpenKeyReadOnly('System\CurrentControlSet\Control\ProductOptions') then
            begin
              if not ValueExists('ProductType') then Result := Result + 'Unknown '
              else
              begin
                s := LowerCase(ReadString('ProductType'));
                if s = 'winnt' then Result := Result + 'Workstation '
                else if s = 'lanmannt' then Result := Result + 'Server '
                else if s = 'servernt' then Result := Result + 'Advanced Server ';
              end;
            end
            else
              Result := Result + 'Unknown ';
          finally
            Free;
          end;

          Result := Result + Format('%d.%d ', [osvi.dwMajorVersion, osvi.dwMinorVersion]);
        end;

        // Display service pack (if any) and build number.

        if (osvi.dwMajorVersion = 4) and (AnsiCompareText(osvi.szCSDVersion, 'Service Pack 6') = 0) then
        begin
          with TRegistryErrorControlled.Create do  // I2890
          try
            RootKey := HKEY_LOCAL_MACHINE;
            if OpenKeyReadOnly('SOFTWARE\Microsoft\Windows NT\CurrentVersion\Hotfix\Q246009') then
              Result := Result + Format('Service Pack 6a (Build %d)', [osvi.dwBuildNumber and $FFFF])
            else
              Result := Result + osvi.szCSDVersion + Format(' (Build %d)', [osvi.dwBuildNumber and $FFFF]);
          finally
            Free;
          end;
        end
        else // Windows NT 3.51 and earlier or Windows 2000 and later
          Result := Result + osvi.szCSDVersion + Format(' (Build %d)', [osvi.dwBuildNumber and $FFFF]);
      end;

    // Test for the Windows 95 product family.
    VER_PLATFORM_WIN32_WINDOWS:
      begin
        if (osvi.dwMajorVersion = 4) and (osvi.dwMinorVersion = 0) then
        begin
          Result := Result + 'Microsoft Windows 95 ';
          if (osvi.szCSDVersion[1] = 'C') or (osvi.szCSDVersion[1] = 'B') then
            Result := Result + 'OSR2 ';
        end;

        if (osvi.dwMajorVersion = 4) and (osvi.dwMinorVersion = 10) then
        begin
          Result := Result + 'Microsoft Windows 98 ';
          if osvi.szCSDVersion[1] = 'A' then
            Result := Result + 'SE ';
        end;

        if (osvi.dwMajorVersion = 4) and (osvi.dwMinorVersion = 90) then
        begin
          Result := Result + 'Microsoft Windows Millennium Edition';
        end;
      end;

    VER_PLATFORM_WIN32s:
      Result := Result + 'Microsoft Win32s';
  end;
end;
    
end.
