{
Description
Project JEDI Windows Security Code Library (JWSCL)

Contains types that are used by the units of JWSCL 

Author
Christian Wimmer

License

The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the  
GNU Lesser General Public License (the  "LGPL License"), in which case the   
provisions of the LGPL License are applicable instead of those above.        
If you wish to allow use of your version of this file only under the terms   
of the LGPL License and not to allow others to use your version of this file 
under the MPL, indicate your decision by deleting  the provisions above and  
replace  them with the notice and other provisions required by the LGPL      
License.  If you do not delete the provisions above, a recipient may use     
your version of this file under either the MPL or the LGPL License.          
                                                                             
For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html 

Note

The Original Code is JwsclVersion.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclVersion;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $

interface

uses SysUtils, 
  JwsclUtils, JwsclResource,
  jwaWindows, JwsclConstants, JwsclExceptions, JwsclTypes,
  JwsclStrings;

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type


  TJwFileVersion = class(TObject)
  private
  protected
  public
    {<B>GetFileInfo</B> retrieves a TFileVersionInfo structure for a given Filename.
    }
    class function GetFileInfo(const Filename: TJwString;
      var FileVersionInfo: TJwFileVersionInfo): Boolean;
  end;


  { <b>TJwServerInfo</b> retrieves Windows Version information on a remote (or
    local) client.                                                             }
  TJwServerInfo = class(TObject)
  private
  protected
    {@exclude}
    FServer: TJwString;
    {@exclude}
    FMajorVersion: Integer;
    {@exclude}
    FMinorVersion: Integer;
    {@exclude}
    FIsServer: Boolean;
    {@exclude}
    FIsTerminalServer: Boolean;
    {@exclude}
    FWindowsType: Integer;
  public
    {The <B>Create</B> constructor creates the TJwServerInfo object and reserves memory
     for it.
     @param Server the servername for which you would like to retreive
     information. If Servername is empty string the local machine will be
     queried. 
     
     Example:
     <code lang="Delphi">
     var
       ServerInfo: TJwServerInfo;

     begin
       // Create TJwServerInfo object and reserve memory for it
       ServerInfo := TJwServerInfo.Create('REMOTESERVER');

       // Are we running on a Server?
       if ServerInfo.IsServer then
       begin
         Memo1.Lines.Add(Format('Server %s runs a Server OS', [ServerInfo.Server]));
       end;

       // Vista or higher code
       if ServerInfo.IsWindowsVista(True) then
       begin
         // Do some Vista specific stuff
       end;

       // Free Memory!
       ServerInfo.Free;
     end;
     </code>
    }
    constructor Create(const Server: TJwString);

    {<B>GetWindowsType</B> returns a constant that defines the windows version the process is running.
     @return The return value can be one of these constants defined in JwsclConstants
     Currently these items are supported 
     
       #  Spacing(Compact)
       #  cOsUnknown = The system is unknown
       #  cOsWinNT   = running on Windows NT
       #  cOsWin2000 = running on Windows 2000
       #  cOsXP      = running on Windows XP
       #  cOS2003    = running on Windows 2003 or Windows 2003 Release 2
       #  cOSXP64    = running on Windows XP 64 Edition (not supported at the moment)
       #  cOsVista   = running on Windows Vista
       #  cOsWin2008 = running on Windows 2008 (tested on rc)
     )
    }
    function GetWindowsType: Integer;

    {<B>IsServer</B> checks if the system is a server version. 
     <B>IsServer</B> returns <B>true</B> if the system is a Server; otherwise <B>false</B> (Workstation).
    }

    property IsServer: Boolean read FIsServer;

    {<B>IsTerminalServer</B> checks if the system is a Terminal Server. A server is considered to
     be a Terminal Server if it meets one of the following conditions:
     
     # The Terminal Server is in application mode 
     # The Terminal Server is advertising itsself on the network 
     
     
     <B>IsTerminalServer</B> returns <B>true</B> if the system is a Terminal Server in application mode
     ; otherwise <B>false</B>.
     See Also
     * TJwTerminalServer.EnumerateServers
    }
    property IsTerminalServer: Boolean read FIsTerminalServer;

    {<B>IsWindows2000</B> checks if the system has the version given in the function name.
     @param bOrHigher defines if the return value should also be <B>true</B> if the system
     is better/higher than the requested system version. 
     @returns <B>IsWindows2000</B> returns <B>true</B> if the system is the requested version (or higher if bOrHigher is true);
     otherwise <B>false</B>.
     If bOrHigher is <B>true</B> the return value is the result of
     <B>true</B> if (bOrHigher and (GetWindowsType > iVer)) is true;
     <B>false</B> if GetWindowsType < (requested version)
      
    }
    function IsWindows2000(bOrHigher: boolean = False): boolean;

    {<B>IsWindows2003</B> checks if the system has the version given in the function name.
     @param bOrHigher defines if the return value should also be <B>true</B> if the system
     is better/higher than the requested system version. 
     @return <B>IsWindows2003</B> returns <B>true</B> if the system is the requested version (or higher if bOrHigher is true);
     otherwise <B>false</B>.
     If bOrHigher is <B>true</B> the return value is the result of
     <B>true</B> if (bOrHigher and (GetWindowsType > iVer)) is true;
     <B>false</B> if GetWindowsType < (requested version)
      
    }
    function IsWindows2003(bOrHigher: boolean = False): boolean;

    {<B>IsWindowsXP</B> checks if the system has the version given in the function name.
     @param bOrHigher defines if the return value should also be <B>true</B> if the system
     is better/higher than the requested system version. 
     @return <B>IsWindowsXP</B> returns <B>true</B> if the system is the requested version (or higher if bOrHigher is true);
     otherwise <B>false</B>.
     If bOrHigher is <B>true</B> the return value is the result of
     <B>true</B> if (bOrHigher and (GetWindowsType > iVer)) is true;
     <B>false</B> if GetWindowsType < (requested version)

    }
    function IsWindowsXP(bOrHigher: boolean = False): boolean;

    {<B>IsWindows2008</B> checks if the system has the version given in the function name.
     @param bOrHigher defines if the return value should also be <B>true</B> if the system
     is better/higher than the requested system version. 
     @return <B>IsWindows2008</B> returns <B>true</B> if the system is the requested version (or higher if bOrHigher is true);
     otherwise <B>false</B>.
     If bOrHigher is <B>true</B> the return value is the result of
     <B>true</B> if (bOrHigher and (GetWindowsType > iVer)) is true;
     <B>false</B> if GetWindowsType < (requested version)
      
    }
    function IsWindows2008(bOrHigher: boolean = False): boolean;


    {<B>IsWindowsVista</B> checks if the system has the version given in the function name.
     @param bOrHigher defines if the return value should also be <B>true</B> if the system
     is better/higher than the requested system version. 
     @return <B>IsWindowsVista</B> returns <B>true</B> if the system is the requested version (or higher if bOrHigher is true);
     otherwise <B>false</B>.
     If bOrHigher is <B>true</B> the return value is the result of
     <B>true</B> if (bOrHigher and (GetWindowsType > iVer)) is true;
     <B>false</B> if GetWindowsType < (requested version)
      
    }
    function IsWindowsVista(bOrHigher: boolean = False): boolean;

    {<B>IsWindowsBeta</B> checks if the system has the version given in the function name.
     @param bOrHigher defines if the return value should also be <B>true</B> if the system
     is better/higher than the requested system version.
     @return <B>IsWindowsVista</B> returns <B>true</B> if the system is the requested version (or higher if bOrHigher is true);
     otherwise <B>false</B>.
     If bOrHigher is <B>true</B> the return value is the result of
     <B>true</B> if (bOrHigher and (GetWindowsType > iVer)) is true;
     <B>false</B> if GetWindowsType < (requested version)

    }
    function IsWindows7(bOrHigher: boolean = False): boolean;


    {<B>Server</B> is the servername as returned by Windows Api}
    property Server: TJwString read FServer;
  end;


    {<B>TJwWindowsVersion</B> provides methods to detect the windows version and product type.
     All methods are class methods so there is no need for an instance of <B>TJwWindowsVersion</B>.
     }
  TJwWindowsVersion = class(TObject)
  private
  protected
  public
      {<B>GetWindowsType</B> returns a constant that defines the windows version the process is running.
       @return The return value can be one of these constants defined in JwsclConstants
        Currently these items are supported
        
         #  cOsUnknown = The system is unknown
         #  cOsWin95   = running on Windows 95
         #  cOsWin98   = running on Windows 98
         #  cOsWin98SE = running on Windows 98 Second Edition
         #  cOsWinME   = running on Windows ME
         #  cOsWinNT   = running on Windows NT
         #  cOsWin2000 = running on Windows 2000
         #  cOsXP      = running on Windows XP
         #  cOS2003    = running on Windows 2003
         #  cOS2003R2  = running on Windows 2003 Release 2
         #  cOSXP64    = running on Windows XP 64 Edition (not supported at the moment)
         #  cOsVista   = running on Windows Vista
         #  cOsWin2008 = running on Windows 2008 (tested on rc)
          
       }

    class function GetWindowsType: integer; virtual;

    class function GetCachedWindowsType: integer;
    class function SetCachedWindowsType(const WindowsType : Integer; Server : Boolean = False) : Integer; virtual;
    class procedure ResetCachedWindowsType; virtual;
      {<B>IsWindows95</B> checks if the system has the version given in the function name.
       @param bOrHigher defines if the return value should also be <B>true</B> if the system
              is better/higher than the requested system version. 
       @return <B>IsWindows95</B> returns <B>true</B> if the system is the requested version (or higher if bOrHigher is true);
               otherwise <B>false</B>.
               If bOrHigher is <B>true</B> the return value is the result of
                <B>true</B> if (bOrHigher and (GetWindowsType > iVer)) is true;
                <B>false</B> if GetWindowsType < (requested version)
                
       }
    class function IsWindows95(bOrHigher: boolean = False): boolean;
      virtual;
      {<B>IsWindows98</B> checks if the system has the version given in the function name.
       @param bOrHigher defines if the return value should also be <B>true</B> if the system
              is better/higher than the requested system version. 
       @return <B>IsWindows98</B> returns <B>true</B> if the system is the requested version (or higher if bOrHigher is true);
               otherwise <B>false</B>.
               If bOrHigher is <B>true</B> the return value is the result of
                <B>true</B> if (bOrHigher and (GetWindowsType > iVer)) is true;
                <B>false</B> if GetWindowsType < (requested version)

       }

    class function IsWindows98(bOrHigher: boolean = False): boolean;
      virtual;

      {<B>IsWindowsME</B> checks if the system has the version given in the function name.
       @param bOrHigher defines if the return value should also be <B>true</B> if the system
              is better/higher than the requested system version. 
       @return <B>IsWindowsME</B> returns <B>true</B> if the system is the requested version (or higher if bOrHigher is true);
               otherwise <B>false</B>.
               If bOrHigher is <B>true</B> the return value is the result of
                <B>true</B> if (bOrHigher and (GetWindowsType > iVer)) is true;
                <B>false</B> if GetWindowsType < (requested version)
                
       }

    class function IsWindowsME(bOrHigher: boolean = False): boolean;
      virtual;

            {<B>IsWindows2000</B> checks if the system has the version given in the function name.
       @param bOrHigher defines if the return value should also be <B>true</B> if the system
              is better/higher than the requested system version. 
       @return <B>IsWindows2000</B> returns <B>true</B> if the system is the requested version (or higher if bOrHigher is true);
               otherwise <B>false</B>.
               If bOrHigher is <B>true</B> the return value is the result of
                <B>true</B> if (bOrHigher and (GetWindowsType > iVer)) is true;
                <B>false</B> if GetWindowsType < (requested version)
                
       }
    class function IsWindows2000(bOrHigher: boolean = False): boolean;
      virtual;


      {<B>IsWindows2003</B> checks if the system has the version given in the function name.
       @param bOrHigher defines if the return value should also be <B>true</B> if the system
              is better/higher than the requested system version. 
       @return <B>IsWindows2003</B> returns <B>true</B> if the system is the requested version (or higher if bOrHigher is true);
               otherwise <B>false</B>.
               If bOrHigher is <B>true</B> the return value is the result of
                <B>true</B> if (bOrHigher and (GetWindowsType > iVer)) is true;
                <B>false</B> if GetWindowsType < (requested version)
                
       }
    class function IsWindows2003(bOrHigher: boolean = False): boolean;
      virtual;


      {<B>IsWindows2003R2</B> checks if the system has the version given in the function name.
       @param bOrHigher defines if the return value should also be <B>true</B> if the system
              is better/higher than the requested system version. 
       @return <B>IsWindows2003R2</B> returns <B>true</B> if the system is the requested version (or higher if bOrHigher is true);
               otherwise <B>false</B>.
               If bOrHigher is <B>true</B> the return value is the result of
                <B>true</B> if (bOrHigher and (GetWindowsType > iVer)) is true;
                <B>false</B> if GetWindowsType < (requested version)
                
       }
    class function IsWindows2003R2(bOrHigher: boolean = False): boolean;
      virtual;


      {<B>IsWindowsXP</B> checks if the system has the version given in the function name.
       @param bOrHigher defines if the return value should also be <B>true</B> if the system
              is better/higher than the requested system version.
       @return <B>IsWindowsXP</B> returns <B>true</B> if the system is the requested version (or higher if bOrHigher is true);
               otherwise <B>false</B>.
               If bOrHigher is <B>true</B> the return value is the result of
                <B>true</B> if (bOrHigher and (GetWindowsType > iVer)) is true;
                <B>false</B> if GetWindowsType < (requested version)
                
       }
    class function IsWindowsXP(bOrHigher: boolean = False): boolean;
      virtual;


      {<B>IsWindowsVista</B> checks if the system has the version given in the function name.

       Currenty the parameter bOrHigher has no meaning in this function!

       @param bOrHigher defines if the return value should also be <B>true</B> if the system
              is better/higher than the requested system version.
       @return <B>IsWindowsVista</B> returns <B>true</B> if the system is the requested version (or higher if bOrHigher is true);
               otherwise <B>false</B>.
               If bOrHigher is <B>true</B> the return value is the result of
                <B>true</B> if (bOrHigher and (GetWindowsType > iVer)) is true;
                <B>false</B> if GetWindowsType < (requested version)
                
       }
    class function IsWindowsVista(bOrHigher: boolean = False): boolean;
      virtual;

      {<B>IsWindows2008</B> checks if the system has the version given in the function name.

       Currently the parameter bOrHigher has no meaning in this function!

       @param bOrHigher defines if the return value should also be <B>true</B> if the system
              is better/higher than the requested system version. 
       @return <B>IsWindows2008</B> returns <B>true</B> if the system is the requested version (or higher if bOrHigher is true);
               otherwise <B>false</B>.
               If bOrHigher is <B>true</B> the return value is the result of
                <B>true</B> if (bOrHigher and (GetWindowsType > iVer)) is true;
                <B>false</B> if GetWindowsType < (requested version)
                
       }
    class function IsWindows2008(bOrHigher: boolean = False): boolean;
      virtual;

    class function IsWindows7(bOrHigher: Boolean = False): Boolean; virtual;


      {<B>IsServer</B> checks if the system is a server version
       @return Returns true if the system is a server; otherwise false (Workstation). }
    class function IsServer: boolean; virtual;


      {<B>CheckWindowsVersion</B> raises an EJwsclUnsupportedWindowsVersionException exception if
       the current windows version does not correspond to the required one in the parameters.

       @param iWinVer contains a cOsXXXXX constant that is defined in JwsclConstants.
              If iWinVer is not between the bounds of sOSVerString the windows version will be checked though, but on
			  exception the supplied cOsXXXX constant will be presented as "Unknown System".
       @param bOrHigher If true the exception will only be raised if the current system version
               is smaller than the given on in iWinVer; otherwise the system version must be exactly the given one in iWinVer 
       @param SourceProc contains the caller method name to be displayed in the exception message 
       @param SourceClass contains the caller class name to be displayed in the exception message 
       @param SourceFile contains the caller file name to be displayed in the exception message 
       @param SourcePos contains the caller source position to be displayed in the exception message 

       raises
 EJwsclUnsupportedWindowsVersionException:  will be raised if the following expression is false :
                ((fWindowsType = iWinVer) or
                (bOrHigher and (fWindowsType > iWinVer)))  
       }
    class procedure CheckWindowsVersion(const iWinVer: integer;
      bOrHigher: boolean; SourceProc, SourceClass, SourceFile: TJwString;
      SourcePos: Cardinal); virtual;

    {<B>IsTerminalServiceRunning</B> checks the status of the terminal service.

     Return
        Returns true if the terminal service is running; otherwise false.
     Remarks
        On Windows 7 (Workstation) the function returns in most cases false because
        of a changed Windows TS architecture.
        Even if TS is not running you can safely call any WTS function. So
        you just need to check for Windows 7 and ignore the result of IsTerminalServiceRunning.
    }
    class function IsTerminalServiceRunning : Boolean;

    // <B>GetNativeProcessorArchitecture</B> returns processor architecture of the current Windows version
    class function GetNativeProcessorArchitecture : Cardinal;

    // <B>IsWindowsX64</B> returns true if the process is running on a Windows x64 version
    class function IsWindowsX64 : boolean;

    // <B>IsWindowsIA64</B> returns true if the process is running on a Windows IA64 version
    class function IsWindowsIA64 : boolean;

    // <B>IsWindows64</B> returns true if the process is running on any 64 bit Windows version
    class function IsWindows64 : boolean;

    class function IsUACEnabled : Boolean;



    {<B>IsProcess64</B> checks if a process is 64 bit.
	 param ProcessHandle Defines the process to be checked for 64 bit. If this parameter is zero
	   the current process is used instead.
	 return Returns true if the given process is a 64bit process.
	 
	 raises
	   EJwsclWinCallFailedException This exception will be raised if the process handle has not the following rights
	       XP/2003 : ROCESS_QUERY_INFORMATION
	       Vista: ROCESS_QUERY_INFORMATION and PROCESS_QUERY_LIMITED_INFORMATION 
	}
    class function IsProcess64(ProcessHandle : DWORD = 0) : boolean;
  end;



{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

uses SysConst, Registry;


{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}




//pseudo class variable
var
  fWindowsType: integer;
  fIsServer: boolean;


class function TJwFileVersion.GetFileInfo(const Filename: TJwString;
  var FileVersionInfo: TJwFileVersionInfo): Boolean;
var VerInfoSize: DWORD;
 DummyVar: DWORD;
 VersionInfo: Pointer;
 Translation: Pointer;
 VersionValue: TJwString;

function VerInfoQuery(VerInfo: Pointer; VerValue: TJwString): String;
var VerInfoSize: DWORD;
  VerInfoPtr: Pointer;
begin
  Result := '';

  VerInfoSize := 0;
{$IFDEF UNICODE}
  if VerQueryValueW(VerInfo, PWideChar(VerValue), VerInfoPtr, VerInfoSize) then
  begin
    Result := Trim(PWideChar(VerInfoPtr));
  end;
{$ELSE}
  if VerQueryValueA(VerInfo, PAnsiChar(VerValue), VerInfoPtr, VerInfoSize) then
  begin
    Result := Trim(PAnsiChar(VerInfoPtr));
  end;
{$ENDIF}

  VerInfoPtr := nil; // Memory is freed when freeing VersionInfo Pointer
end;
begin
  Result := False;
  ZeroMemory(@FileVersionInfo, SizeOf(FileVersionInfo));

  VerInfoSize :=
{$IFDEF UNICODE}
  GetFileVersionInfoSizeW(PWideChar(Filename), DummyVar);
{$ELSE}
  GetFileVersionInfoSizeA(PAnsiChar(Filename), DummyVar);
{$ENDIF}
  if VerInfoSize > 0 then begin

    // GetFileVersionInfoSize returns bytes and not char's so should be okay
    // for unicode and ansi
    GetMem(VersionInfo, VerInfoSize);

    try
      Result :=
{$IFDEF UNICODE}
      GetFileVersionInfoW(PWideChar(Filename), 0, VerInfoSize, VersionInfo);
{$ELSE}
      GetFileVersionInfoA(PAnsiChar(Filename), 0, VerInfoSize, VersionInfo);
{$ENDIF}

      // Exit on failure
      if not Result then Exit;

      Result :=
{$IFDEF UNICODE}
      VerQueryValueW(VersionInfo, '\VarFileInfo\Translation',
        Translation, VerInfoSize);
{$ELSE}
      VerQueryValueA(VersionInfo, '\VarFileInfo\Translation',
        Translation, VerInfoSize);
{$ENDIF}

      // Exit on failure
      if not Result then Exit;

      VersionValue := Format('\StringFileInfo\%.4x%.4x\',
        [LoWord(Integer(Translation^)), HiWord(Integer(Translation^))]);

      FileVersionInfo.CompanyName := VerInfoQuery(VersionInfo,
        VersionValue + 'CompanyName');
      FileVersionInfo.FileDescription := VerInfoQuery(VersionInfo,
        VersionValue + 'FileDescription');
      FileVersionInfo.FileVersion := VerInfoQuery(VersionInfo,
        VersionValue + 'FileVersion');
      FileVersionInfo.InternalName := VerInfoQuery(VersionInfo,
        VersionValue + 'InternalName');
      FileVersionInfo.LegalCopyright := VerInfoQuery(VersionInfo,
        VersionValue + 'LegalCopyright');
      FileVersionInfo.LegalTradeMarks := VerInfoQuery(VersionInfo,
        VersionValue + 'LegalTrademarks');
      FileVersionInfo.OriginalFilename := VerInfoQuery(VersionInfo,
        VersionValue + 'OriginalFilename');
      FileVersionInfo.ProductName := VerInfoQuery(VersionInfo,
        VersionValue + 'ProductName');
      FileVersionInfo.ProductVersion := VerInfoQuery(VersionInfo,
        VersionValue + 'ProductVersion');
      FileVersionInfo.Comments := VerInfoQuery(VersionInfo,
        VersionValue + 'Comments');

    finally
      FreeMem(VersionInfo);
    end;
  end;
end;


constructor TJwServerInfo.Create(const Server: TJwString);
var
  nStatus: NET_API_STATUS;
  pwServer: PWideChar;
  ServerInfoPtr: PServerInfo101;
begin
  pwServer := PWideChar(WideString(Server));

  nStatus := NetServerGetInfo(pwServer, 101, PByte(ServerInfoPtr));

  if nStatus = NERR_Success then
  begin
    FServer := ServerInfoPtr^.sv101_name;
    {Specifies, in the least significant 4 bits of the byte, the major release
     version number of the operating system. The most significant 4 bits of the
     byte specifies the server type. The mask MAJOR_VERSION_MASK should be used
     to ensure correct results.}
    FMajorVersion  := ServerInfoPtr^.sv101_version_major and MAJOR_VERSION_MASK;
    FMinorVersion  := ServerInfoPtr^.sv101_version_minor;
    FIsServer :=
      (JwCheckBitMask(ServerInfoPtr^.sv101_type, SV_TYPE_DOMAIN_CTRL)) or
      (JwCheckBitMask(ServerInfoPtr^.sv101_type, SV_TYPE_DOMAIN_BAKCTRL)) or
      (JwCheckBitMask(ServerInfoPtr^.sv101_type, SV_TYPE_SERVER_NT));
    FIsTerminalServer :=
      (JwCheckBitMask(ServerInfoPtr^.sv101_type, SV_TYPE_TERMINALSERVER));
    FWindowsType := GetWindowsType;

    // Free Memory
    NetApiBufferFree(ServerInfoPtr);
  end
  else begin
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailedWithNTStatus+#13#10+'Server: %2:s', 'DisableAllPrivileges', ClassName,
      RsUNVersion, 0, True, ['NetServerGetInfo', nStatus,FServer]);
  end;
end;

function TJwServerInfo.GetWindowsType: Integer;
begin
  if FMajorVersion <= 4 then
  begin
    Result := cOsWinNT;
  end
  else if (FMajorVersion = 5) and (FMinorVersion = 0) then
  begin
    Result := cOsWin2000;
  end
  else if (FMajorVersion = 5) and (FMinorVersion = 1) then
  begin
    Result := cOsXP;
  end
  else if (FMajorVersion = 5) and (FMinorVersion = 2) then
  begin
    // Is there a way to determine 2003 R2 remotely?
    Result := cOS2003;
  end
  else if (FMajorVersion = 6) and (FMinorVersion = 0) and (not fIsServer) then
  begin
    Result := cOsVista;
  end
  else if (FMajorVersion = 6) and (FMinorVersion = 0) and (fIsServer) then
  begin
    Result := cOsWin2008;
  end
  else if (FMajorVersion = 6) and (FMinorVersion = 0) then
  begin
    Result := cOsVista;
  end
  else if (FMajorVersion = 6) and (FMinorVersion = 1) then
  begin
    Result := cOsWin7;
  end
  else
  begin
    Result := cOsUnknown;
  end;
end;

function TJwServerInfo.IsWindows2000(bOrHigher: Boolean = False): Boolean;
const
  iVer = cOsWin2000;
begin
  Result := (FWindowsType = iVer) or (bOrHigher and (FWindowsType > iVer));
end;

function TJwServerInfo.IsWindows2003(bOrHigher: Boolean = False): Boolean;
const
  iVer = cOs2003;
begin
  Result := (FWindowsType = iVer) or (bOrHigher and (FWindowsType > iVer));
end;

function TJwServerInfo.IsWindowsXP(bOrHigher: Boolean = False): Boolean;
const
  iVer = cOsXP;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and (fWindowsType > iVer));
end;

function TJwServerInfo.IsWindowsVista(bOrHigher: Boolean = False): Boolean;
const
  iVer = cOsVista;
begin
  Result := (FWindowsType = iVer) or (bOrHigher and (FWindowsType > iVer));
end;

function TJwServerInfo.IsWindows7(bOrHigher: Boolean = False): Boolean;
const
  iVer = cOsWin7;
begin
  Result := (FWindowsType = iVer) or (bOrHigher and (FWindowsType > iVer));
end;


function TJwServerInfo.IsWindows2008(bOrHigher: Boolean = False): Boolean;
const
  iVer = cOsWin2008;
begin
  Result := (FWindowsType = iVer) or (bOrHigher and (FWindowsType > iVer));
end;



class procedure TJwWindowsVersion.CheckWindowsVersion(const iWinVer: integer;
  bOrHigher: boolean; SourceProc, SourceClass, SourceFile: TJwString;
  SourcePos: Cardinal);
var
  sOrHigher, sActWinVer, sWinVer: TJwString;

begin
  if (iWinVer < low(sOSVerString)) or (iWinVer > high(sOSVerString)) then
    sWinVer := RsUnknownSuppliedOS
  else
    sWinVer := sOSVerString[iWinVer];

  try
    sActWinVer := sOSVerString[fWindowsType];
  except
    sActWinVer := sOSVerString[-1];
  end;

  sOrHigher := '';
  if bOrHigher then
    sOrHigher := RsVersionOrHigher;

  if not ((fWindowsType = iWinVer) or (bOrHigher and
    (fWindowsType > iWinVer))) then
    raise EJwsclUnsupportedWindowsVersionException.CreateFmtEx(
      RsVersionUnsupportedVersion, SourceProc, SourceClass,
      SourceFile, SourcePos, False,
      [sActWinVer,
      sWinVer, sOrHigher]);
end;


class function TJwWindowsVersion.IsWindows95(bOrHigher: boolean = False)
: boolean;
const
  iVer = cOsWin95;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and
    (fWindowsType > iVer));
end;

class function TJwWindowsVersion.IsWindows98(bOrHigher: boolean = False)
: boolean;
const
  iVer = cOsWin95;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and
    (fWindowsType > iVer));
end;

class function TJwWindowsVersion.IsWindowsME(bOrHigher: boolean = False)
: boolean;
const
  iVer = cOsWinME;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and
    (fWindowsType > iVer));
end;

class function TJwWindowsVersion.IsWindows2000(bOrHigher: boolean = False)
: boolean;
const
  iVer = cOsWin2000;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and
    (fWindowsType > iVer));
end;

class function TJwWindowsVersion.IsWindows2003(bOrHigher: boolean = False)
: boolean;
const
  iVer = cOs2003;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and
    (fWindowsType > iVer));
end;

class function TJwWindowsVersion.IsWindows2003R2(bOrHigher: boolean = False)
: boolean;
const
  iVer = cOs2003R2;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and
    (fWindowsType > iVer));
end;

class function TJwWindowsVersion.IsWindowsXP(bOrHigher: boolean = False)
: boolean;
const
  iVer = cOsXP;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and
    (fWindowsType > iVer));
end;

class function TJwWindowsVersion.IsWindowsVista(bOrHigher: boolean = False)
: boolean;
const
  iVer = cOsVista;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and
    (fWindowsType > iVer));
end;

class function TJwWindowsVersion.IsWindows2008(bOrHigher: boolean = False)
: boolean;
const
  iVer = cOsWin2008;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and
    (fWindowsType > iVer));
end;

class function TJwWindowsVersion.IsServer: boolean;
begin
  Result := fIsServer;
end;

class function TJwWindowsVersion.GetWindowsType: integer;
var
  osVerInfo:
{$IFDEF UNICODE}TOSVersionInfoExW{$ELSE}
  TOSVersionInfoExA
{$ENDIF}
  ;
  majorVer, minorVer: integer;
begin
  //  Result := cOsUnknown;

  osVerInfo.dwOSVersionInfoSize := SizeOf(osVerInfo);

  if
{$IFDEF UNICODE}GetVersionExW{$ELSE}
  GetVersionExA
{$ENDIF}
    (@osVerInfo) then
  begin
    majorVer  := osVerInfo.dwMajorVersion;
    minorVer  := osVerInfo.dwMinorVersion;
    fIsServer := osVerInfo.wProductType <> VER_NT_WORKSTATION;

    case osVerInfo.dwPlatformId of
      VER_PLATFORM_WIN32_NT: { Windows NT/2000 }
      begin
        if majorVer <= 4 then
          Result := cOsWinNT
        else if (majorVer = 5) and (minorVer = 0) then
          Result := cOsWin2000
        else if (majorVer = 5) and (minorVer = 1) then
          Result := cOsXP
        else if (majorVer = 5) and (minorVer = 2) then
        begin
          if boolean(GetSystemMetrics(SM_SERVERR2)) then
            Result := cOS2003R2
          else
            Result := cOS2003;
        end
        else if (majorVer = 6) and (minorVer = 0) and (not fIsServer) then
          Result := cOsVista
        else if (majorVer = 6) and (minorVer = 0) and (fIsServer) then
          Result := cOsWin2008
        else if (majorVer = 6) and (minorVer = 1) then
          Result := cOsWin7
        else if (majorVer = 6) and (minorVer = 1) then
          Result := cOsWin7
        else
          Result := cOsUnknown;
      end;
      VER_PLATFORM_WIN32_WINDOWS:  { Windows 9x/ME }
      begin
        if (majorVer = 4) and (minorVer = 0) then
          Result := cOsWin95
        else if (majorVer = 4) and (minorVer = 10) then
        begin
          if osVerInfo.szCSDVersion[1] = 'A' then
            Result := cOsWin98SE
          else
            Result := cOsWin98;
        end
        else if (majorVer = 4) and (minorVer = 90) then
          Result := cOsWinME
        else
          Result := cOsUnknown;
      end;
      else
        Result := cOsUnknown;
    end;
  end
  else
    Result := cOsUnknown;
end;

class function TJwWindowsVersion.IsTerminalServiceRunning: Boolean;
begin
  result := JwaWindows.IsTerminalServiceRunning;
end;



class function TJwWindowsVersion.IsUACEnabled: Boolean;
var R : TRegistry;
begin
  R := TRegistry.Create;
  R.RootKey := HKEY_LOCAL_MACHINE;
  if R.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System') then
  begin
    result := R.ReadInteger('EnableLUA') = 1;
  end
  else
   raise EJwsclWinCallFailedException.CreateFmtWinCall(
        RsWinCallFailed,
        'IsUACEnabled',                                //sSourceProc
        ClassName,                                //sSourceClass
        RSUnVersion,                          //sSourceFile
        0,                                           //iSourceLine
        True,                                  //bShowLastError
        'OpenKeyReadOnly',                   //sWinCall
        ['OpenKeyReadOnly']);                                  //const Args: array of const
end;

class function TJwWindowsVersion.GetNativeProcessorArchitecture : Cardinal;
var
  SystemInfo : SYSTEM_INFO;
  // only available on Windows >= 5.1 so we have to link it dynamically
  GetNativeSystemInfo : procedure (lpSystemInfo: LPSYSTEM_INFO); stdcall;
begin
  GetNativeSystemInfo := GetProcAddress(GetModuleHandle('kernel32.dll'), 'GetNativeSystemInfo');
  if @GetNativeSystemInfo <> nil
    then
      begin
        GetNativeSystemInfo(@SystemInfo);
        result := SystemInfo.wProcessorArchitecture;
      end
    else
      result := PROCESSOR_ARCHITECTURE_INTEL;
end;


class function TJwWindowsVersion.IsWindowsX64 : boolean;
begin
  result := GetNativeProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64;
end;


class function TJwWindowsVersion.IsWindowsIA64 : boolean;
begin
  result := GetNativeProcessorArchitecture = PROCESSOR_ARCHITECTURE_IA64;
end;


class function TJwWindowsVersion.IsWindows64 : boolean;
begin
  result := IsWindowsX64 or IsWindowsIA64;
end;


class function TJwWindowsVersion.IsProcess64(ProcessHandle : DWORD = 0) : boolean;
var
  RunningInsideWOW64 : BOOL;
begin
  if ProcessHandle = 0 then
    ProcessHandle := GetCurrentProcess();
	
  // If we are on a 64 bit Windows but NOT inside WOW64 we are running natively
  if IsWindows64 then
  begin
    if IsWow64Process(ProcessHandle, RunningInsideWOW64) then
      result := not RunningInsideWOW64
	else
	   raise EJwsclWinCallFailedException.CreateFmtWinCall(
        RsWinCallFailed,
        'IsProcess64',                                //sSourceProc
        ClassName,                                //sSourceClass
        RSUnVersion,                          //sSourceFile
        0,                                           //iSourceLine
        True,                                  //bShowLastError
        'IsWow64Process',                   //sWinCall
        ['IsWow64Process']);                                  //const Args: array of const
  end
  else
  begin
    result := false;
  end;
end;





class function TJwWindowsVersion.GetCachedWindowsType: integer;
begin
  result := fWindowsType;
end;

class procedure TJwWindowsVersion.ResetCachedWindowsType;
var Srv : TJwServerInfo;
begin
  fWindowsType := GetWindowsType;

  try
    Srv := TJwServerInfo.Create('');
    try
      fIsServer := Srv.IsServer;
    finally
      Srv.Free;
    end;
  except
    fIsServer := False;  
  end;
end;

class function TJwWindowsVersion.SetCachedWindowsType(
  const WindowsType: Integer; Server: Boolean): Integer;
begin
  result := fWindowsType; //returns previous value
  fWindowsType := WindowsType;
  fIsServer  := Server;
end;

{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}


class function TJwWindowsVersion.IsWindows7(bOrHigher: Boolean): Boolean;
const
  iVer = cOsWin7;
begin
  Result := (FWindowsType = iVer) or (bOrHigher and (FWindowsType > iVer));
end;

initialization
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INITIALIZATION_SECTION}
  TJwWindowsVersion.ResetCachedWindowsType;

{$ENDIF SL_INITIALIZATION_SECTION}


{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}
