{
Description
Project JEDI Windows Security Code Library (JWSCL)

Contains structures to support vista elevation.

Author
Christian Wimmer

Author
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

The Original Code is JwsclElevation.pas.

Bugs
Not compilable by FreePascal/Lazarus. Missing TTypedComObjectFactory.
}
{$IFDEF FPC}
{$ERROR Not compilable by FreePascal/Lazarus. Missing TTypedComObjectFactory.}
{$ENDIF FPC}

{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclElevation;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}

interface
uses ComObj, JwaWindows, JwsclVersion, JwsclTypes,JwsclStrings;

type
  {<B>TJwElevationClassFactory</B> provides a registration for a typed com object.
   It also creates the necessary registry entries.}
  TJwElevationClassFactory = class(TTypedComObjectFactory)
  private
    fResourceId: AnsiString;
    fDisableProcessIsolation : Boolean;
  public
    {<B>Create</B> registers a com object.
     Every com object that must be elevated must be registered by this constructor.

    @param ResourceId defines a resource id as a string 
    @param DisableProcessIsolation defines whether the elevated com process
     should be isolated (true) or not. }
    constructor Create(
      const ResourceId: AnsiString;
      const DisableProcessIsolation : Boolean;
      const ComServer: TComServerObject;
      const TypedComClass: TTypedComClass;
      const ClassId: TGUID;
      const Instancing: TClassInstancing;
      const ThreadingModel: TThreadingModel = tmApartment
    ); overload;

    {<B>Create</B> registers a com object.
    Every com object that must be elevated must be registered by this constructor.

    @param ResourceId defines a delphi resource id.
      This id must be created by "resourcestring". Use <i>ResourcestringName</i> as parameter input 
    @param DisableProcessIsolation defines whether the elevated com process
     should be isolated (true) or not. }
    constructor Create(
      const ResourceId: PResStringRec;
      const DisableProcessIsolation : Boolean;
      const ComServer: TComServerObject;
      const TypedComClass: TTypedComClass;
      const ClassId: TGUID;
      const Instancing: TClassInstancing;
      const ThreadingModel: TThreadingModel = tmApartment
    ); overload;

    {<B>UpdateRegistry</B> registers or unregisters a com library.
     Use <code>"regsvr32.exe <libname>"</code> to register and <code>"regsvr32.exe /u <lib>"</code> to
     unregister a com library.}
{$IFDEF UNIT_TEST}
    class procedure UpdateRegistry(RegisterFactory: Boolean);
{$ELSE}
    procedure UpdateRegistry(RegisterFactory: Boolean); override;
{$ENDIF UNIT_TEST}
  end;

{<B>JwCoCreateInstanceAsEx</B> creates an out of process COM object and returns it in ObjectInterface.
If the current is already elevated, the function just creates an COM object
and returns it; otherwise the elevation dialog will be shown.

The COM class must be registered in a COM type library.
The executable which call this function must contain a manifest that defines
"asInvoker" as the requested execution level.
<code><requestedExecutionLevel level="asInvoker"/></code>

This function needs CoInitialize to be called.
This function only works on Windows Vista and newer OS versions.

@param MonikerSequence defines a string that contains information how to use the moniker 
@param ParentWindowHandle defines the window handle that is used to display the elevation dialog.
 If this parameter is 0 or the window is has not the input the elevation dialog
 will not be shown but a new task is displayed it the taskbar. Otherwise the elevation dialog
 will be shown direclty. 
@param ClassId defines a guid that describes a registered com object 
@param IID defines the requested com object to be returned
@param ObjectInterface returns the requested and elevated com object 
@return Returns a COM result code. If the call was successfull the return value is S_OK 
}
function JwCoCreateInstanceAsEx(
  const MonikerSequence : WideString;
  const ParentWindowHandle: HWND;
  const ClassId: TGUID;
  const IID: TGUID;
  out ObjectInterface) : HRESULT;


{<B>JwCoCreateInstanceAsAdmin</B> creates an out of process COM object with administrator rights
and returns it in ObjectInterface.
If the current is already elevated, the function just creates an COM object
and returns it; otherwise the elevation dialog will be shown.

The COM class must be registered in a COM type library.
The executable which call this function must contain a manifest that defines
"asInvoker" as the requested execution level.
<code><requestedExecutionLevel level="asInvoker"/></code>

This function needs CoInitialize to be called.
This function only works on Windows Vista and newer OS versions.

@param ParentWindowHandle defines the window handle that is used to display the elevation dialog.
 If this parameter is 0 or the window is has not the input the elevation dialog
 will not be shown but a new task is displayed it the taskbar. Otherwise the elevation dialog
 will be shown direclty. 
@param ClassId defines a guid that describes a registered com object 
@param IID defines the requested com object to be returned 
@param ObjectInterface returns the requested and elevated com object
@return Returns a COM result code. If the call was successfull the return value is S_OK 
}
function JwCoCreateInstanceAsAdmin(
  const ParentWindowHandle: HWND;
  const ClassId: TGUID;
  const IID: TGUID;
  out ObjectInterface
) : HRESULT;


{<B>JwCoCreateInstanceAsHighest</B> creates an out of process COM object with highest possible rights
and returns it in ObjectInterface.
If the current is already elevated, the function just creates an COM object
and returns it; otherwise the elevation dialog will be shown.

The COM class must be registered in a COM type library.
The executable which call this function must contain a manifest that defines
"asInvoker" as the requested execution level.
<code><requestedExecutionLevel level="asInvoker"/></code>


This function needs CoInitialize to be called.
This function only works on Windows Vista and newer OS versions.

@param ParentWindowHandle defines the window handle that is used to display the elevation dialog.
 If this parameter is 0 or the window is has not the input the elevation dialog
 will not be shown but a new task is displayed it the taskbar. Otherwise the elevation dialog
 will be shown direclty. 
@param ClassId defines a guid that describes a registered com object 
@param IID defines the requested com object to be returned 
@param ObjectInterface returns the requested and elevated com object 
@return Returns a COM result code. If the call was successfull the return value is S_OK 
}
function JwCoCreateInstanceAsHighest(
  const ParentWindowHandle: HWND;
  const ClassId: TGUID;
  const IID: TGUID;
  out ObjectInterface
) : HRESULT;

{<B>JwCoGetClassFactoyAsAdmin</B> creates an out of process COM class factory object with administrator rights
and returns it in ObjectInterface.
If the current is already elevated, the function just creates an COM object
and returns it; otherwise the elevation dialog will be shown.

The COM class must be registered in a COM type library.
The executable which call this function must contain a manifest that defines
"asInvoker" as the requested execution level.

<code><requestedExecutionLevel level="asInvoker"/></code>

This function needs CoInitialize to be called.

This function only works on Windows Vista and newer OS versions.

@param ParentWindowHandle defines the window handle that is used to display the elevation dialog.
 If this parameter is 0 or the window is has not the input the elevation dialog
 will not be shown but a new task is displayed it the taskbar. Otherwise the elevation dialog
 will be shown direclty. 
@param ClassId defines a guid that describes a registered com object 
@param IID defines the requested com object to be returned
@param ObjectInterface returns the requested and elevated com object 
@return Returns a COM result code. If the call was successfull the return value is S_OK 
}
function JwCoGetClassFactoyAsAdmin(
  const ParentWindowHandle: HWND;
  const ClassId: TGUID;
  const IID: TGUID;
  out ObjectInterface) : HRESULT;


{<B>JwShellExecute</B> runs a process with elevated privileges in Windows Vista.
If the current is already elevated the function simply opens the given
filename. The verb of shellexecute cannot be changed.

This function only works on Windows Vista and newer OS versions.

@return The return value contains the instance of the newly created app.
The function returns before the new application has started therfore the app
can fail. If ShellExecute determines an error the return value is 0 and
an exception is raised.
 
raises
 EJwsclWinCallFailedException:  will be raised if a call to ShellExecuteEx failed
 EJwsclUnsupportedWindowsVersionException will be raised if the flag
     sefIgnoreElevationIfNotAvailable is set and UAC is not available
}
function JwShellExecute(const hWnd: HWND; FileName, Parameters,
  Directory: TJwString; ShowCmd: Integer; Flags : TJwShellExecuteFlags = [sefNoClosehProcess]): HANDLE;


{ JwElevateProcess is much like JwShellExecute but also may work on Windows 2000
  and XP without UAC. Instead of UAC it uses SuRun if installed. On Vista/2008 and
  newer it tries to use UAC. If UAC and SuRun is not available it falls back to
  ShellExecute with the RunAs verb. In this case a dialog pops up the receives a
  username and password to use (usually Administrator). If no UI is allowed the
  function can use another method that receives the username and password from a
  generated even method (OnElevationGetCredentials). In this case the process is
  started using Windows Secondary Logon Service.
  
  The return value is a process handle, in case of SuRun zero, or zero if it
  closed automatically.
  Parameters
  FileName :                   This parameter receives the application to be
                               elevated.
  Parameters\ :                This parameter receives the parameter to be applied
                               to the new process.
  Directory :                  This parameter receives the target directory of the
                               new process.
  hWindow :                    If any UI is display this window handle is used as a
                               parent. Can be 0 to use no parent at all (not
                               recommended). This handle is used by
                               * UAC prompt
                               * Windows default credentials prompt
                               * SuRun
  ElevationProcessFlags :      Receives a set of flags that controls the behavior
                               of the function. See <link TJwElevationProcessFlags>
                               for more information.
  OnElevationGetCredentials :  This event is used only if
                               * SuRun is not available or ignored (due to missing
                                 epfAllowSuRun)
                               * UAC is not supported by OS
                               * epfNoUi is set in parameter ElevationProcessFlags
                               See <link TJwOnElevationGetCredentials>
  Returns
  The return value is a processID of the newly created process.
  
  If the process is started by SuRun the returned value can be 0 although the
  process was elevated successfully. SuRun 1.2.0.5 and older don't support PIDs.
  In this case zero (0) is returned. If there was an error getting the PID the
  exception EJwsclPIDException will be raised.
  Exceptions
  EJwsclAbortException :           The elevation was aborted by the user.
  EjwsclCryptApiException :        This exception is raised if the encrypted
                                   password received through the
                                   OnElevationGetCredentials event could not be
                                   decrypted.
  EJwsclElevateProcessException :  Super class of
                                   * EJwsclAbortException
                                   * EJwsclElevationException
                                   * EJwsclJwShellExecuteException
                                   * EJwsclShellExecuteException
                                   * EJwsclSuRunErrorException
                                   Actually this exception is not raised.
  EJwsclElevationException :       Currently not used.
  EJwsclJwShellExecuteException :  A call to JwShellExecute failed. This happens
                                   only on system with UAC available. See property
                                   LastError for more information.
  EJwsclShellExecuteException :    This exception is raised if SuRun is not setup
                                   properly.See property LastError for more
                                   information.
  EJwsclSuRunErrorException :      This exception will be raised if SuRun fails to
                                   elevate the new process. The property LastError
                                   of this exception contains more information
                                   about the error. See remarks section for more
                                   information.
  EJwsclWinCallFailedException :   This error only happens when a call to the
                                   Secondary Logon Process failed.See property
                                   LastError for more information.
  EJwsclPIDException :             This error occurs when the PID could not be
                                   returned. You can ignore this error if you don't
                                   use the return value. The exception won't be
                                   thrown if SuRun does not support PIDs.
  Remarks
  In case of the EJwsclSuRunErrorException the LastError property contains more
  information. SuRun returns some status error code information that can be used.
  <table 20c%>
  Status value                  \Description
  ----------------------------  -----------------------------------------------------
  \-1<p />(WAIT_FAILED)         Not a SuRun status code. It happens when a wait call
                                 for the SuRun process failed because the process
                                 handle is invalid.
  258<p />(WAIT_TIMEOUT)        Not a SuRun status code. A time out occurred while
                                 waiting for the SuRun process to finish. This
                                 usually happens when SuRun stucks or Surun's
                                 credential prompt time out is greater than
                                 60seconds. (constant timeout)
  0<p />(RETVAL_OK)             SuRun returned successfully. The designated
                                 application is run elevated.
  1<p />(RETVAL_ACCESSDENIED)   SuRun: TBD
  3<p />(RETVAL_RESTRICT)       SuRun: The current user is not allowed to run
                                 applications elevated or this application cannot be
                                 run elevated by the current user.
  4<p />(RETVAL_CANCELLED)      SuRun: The user canceled the elevation process.
  </table>
  Conditions
  The function acts the following way:
    1. If it detects SuRun and parameter ElevationProcessFlags contains
       epfAllowSuRun it uses SuRun to elevate the application.
    2. If Windows Vista, 2008, 7 is detected
       1. and UAC is available, it uses UAC to elevate the process.
       2. and UAC is not available, it uses Secondary Logon Process and thus event
          parameter OnElevationGetCredentials is called
    3. In all other cases it uses Secondary Logon Process and thus event parameter
       OnElevationGetCredentials is called
  
  JwElevateProcess does not fall back to Secondary Logon Process if UAC is enabled
  but fails.                                                                          }



function JwElevateProcess(const FileName : TJwString;
                Parameters : TJwString;
                Directory : TJwString;
                hWindow : HWND;
                ElevationProcessFlags : TJwElevationProcessFlags;
                const OnElevationGetCredentials : TJwOnElevationGetCredentials) : TJwProcessId;


{JwCheckSuRunStatus checks whether SuRun is availab.e
<extlink http://kay-bruns.de/wp/software/surun/>SuRun</extlink> is a UAC like
clone that runs in Win2000, XP and newer.

StatusData : This out parameter returns status information of SuRun.

Returns
JwCheckSuRunStatus returns true if SuRun is running; otherwise false.
}
function JwCheckSuRunStatus(out StatusData : TJwSuRunStatus) : Boolean;

{$ENDIF SL_IMPLEMENTATION_SECTION}



{$IFNDEF SL_OMIT_SECTIONS}
implementation
uses Registry, SysUtils, ActiveX,
     JwsclExceptions, JwsclSid,     JwsclAcl,
     JwsclConstants,  JwsclUtils, JwsclEncryption,
     JwsclToken, JwaVista,
     JwsclDescriptor, JwsclKnownSid, JwsclMapping, JwsclResource;
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}



function JwCheckSuRunStatus(out StatusData : TJwSuRunStatus) : Boolean;
var
  R : TRegistry;
  pWindowsPath : array[0..MAX_PATH+1] of WideChar;
  WindowsPath : TJwString;
  hPipe : THandle;
  i : Integer;
  FileVer : TJwFileVersionInfo;
  VersionStr : TJwString;
begin
  ZeroMemory(@StatusData, sizeof(StatusData));

  //First get some information that can be there if even SuRun does not run
  R := TRegistry.Create;
  try
    try
      R.RootKey := HKEY_CLASSES_ROOT;
      result := R.OpenKeyReadOnly('exefile\shell\SuRun\command');
      if result then
      begin
        try
          StatusData.ExeFileShellCommand := R.ReadString('');
        except
        end;
      end;
      R.CloseKey;

      R.RootKey := HKEY_LOCAL_MACHINE;
      result := R.OpenKeyReadOnly('Software\SuRun');
      if result then
      begin
        try
          StatusData.CancelTimeOut := R.ReadInteger('CancelTimeOut');
        except
        end;
        try
          StatusData.UseCancelTimeOut := R.ReadBool('UseCancelTimeOut');
        except
        end;
      end;
    finally
      R.Free;
    end;
  except
    //continue though
  end;

  //try to connect to SuRun service pipe. If it fails SuRun does not run
  // at the moment or is not installed
  i := 4;
  repeat
    SetLastError(0);
    hPipe := CreateFile('\\.\Pipe\SuperUserRun',GENERIC_WRITE,0,nil,OPEN_EXISTING,0,0);
    result := (hPipe <> INVALID_HANDLE_VALUE);
    if result then
      CloseHandle(hPipe);
    Sleep(250);
    Dec(i);
  until result or
        (i = 0) or
        (GetLastError() = ERROR_FILE_NOT_FOUND) or
        (GetLastError() = ERROR_ACCESS_DENIED);



  StatusData.ServerStatusCode := GetLastError();


  GetWindowsDirectoryW(pWindowsPath, sizeof(pWindowsPath));
  WindowsPath := TJwString(pWindowsPath);  //W -> A is possible

  //Get path to SuRun installation. It is saved in Windows folder
  if Length(WindowsPath) > 0 then
  begin
    if WindowsPath[Length(WindowsPath)-1] <> '\' then
      WindowsPath := WindowsPath + '\';

{$IFNDEF TESTSURUN}
    StatusData.LocationPath := WindowsPath + 'SuRun.exe';
{$ELSE}
    //only for testing
    StatusData.LocationPath := 'E:\temp\surunsrc\SuRun.exe';
{$ENDIF}

    //Get more information about the SuRun.exe
    TJwFileVersion.GetFileInfo(TJwString(StatusData.LocationPath), FileVer);

    //if it fails it just returns an empty FileVer record. Doesn't matter

    StatusData.FileVersionInfo := FileVer;

    //parse the FileVersion string into the given Version array members.
    VersionStr := TJwString(FileVer.FileVersion);
    i := low(StatusData.Version);

    while (i <= High(StatusData.Version)) and
        (Length(VersionStr) > 0) do
    begin
{$IFNDEF DELPHI2009_UP}
      if VersionStr[1] in ['0'..'9'] then
{$ELSE}
      if CharInSet(VersionStr[1], ['0'..'9']) then
{$ENDIF DELPHI2009_UP}
      begin
        StatusData.Version[i] := 10 * StatusData.Version[i] + Ord(VersionStr[1])-48;
      end
      else
      if (VersionStr[1] = ',') then //comma separated
      begin
        Inc(I); //get to next Version
      end;
      System.Delete(VersionStr, 1, 1);
    end;

    //PID return value is only supported in version 1.2.0.6 and newer
    StatusData.PIDSupport := 1000 * StatusData.Version[0] +
            100 * StatusData.Version[1] +
            10 * StatusData.Version[2] +
            1 * StatusData.Version[3]
            >= 1206;
  end;


end;



function JwElevateProcess(const FileName : TJwString;
                Parameters : TJwString;
                Directory : TJwString;
                hWindow : HWND;
                ElevationProcessFlags : TJwElevationProcessFlags;
                const OnElevationGetCredentials : TJwOnElevationGetCredentials) : TJwProcessId;

var
  UACFlags : TJwShellExecuteFlags;

  SuRunAvail : Boolean; //IsSuRun available at all?
  SuRunStatus : TJwSuRunStatus; //Current SuRun status information. Is it active?

  {GetProcessID exists since Windows XP1
  To support all windows version we use the old fashion style 
  }
  function GetProcessID(const Handle : THandle) : TJwProcessId;
  var P : TProcessBasicInformation;
  begin
    result := NtQueryInformationProcess(
      Handle,//__in       HANDLE ProcessHandle,
      ProcessBasicInformation,//__in       PROCESSINFOCLASS ProcessInformationClass,
      @P,//__out      PVOID ProcessInformation,
      sizeof(P),//__in       ULONG ProcessInformationLength,
      nil//__out_opt  PULONG ReturnLength
    );

    if result = 0 then
      result := P.UniqueProcessId
    else
    begin
      RtlSetLastWin32ErrorAndNtStatusFromNtStatus(result);
      raise EJwsclPIDException.CreateFmtWinCall(
          RsWinCallFailedWithNTStatus,'JwShellExecute','',RsUNElevation,0,
                      true,'NtQueryInformationProcess',['NtQueryInformationProcess',result]);
    end;
  end;


  function RunUAC : THandle;
  var
    P : EJwsclJwShellExecuteException;
    Handle : THandle;
  begin
    try
      Handle := JwShellExecute(hWindow, FileName, Parameters, Directory, SW_NORMAL, UACFlags);
      Result := {localcall}GetProcessId(Handle);
      CloseHandle(Handle);
    except
      on E : EJwsclWinCallFailedException do
      begin
        if E.LastError = E_USER_CANCELED_OPERATIONint then
          raise EJwsclAbortException.CreateFmtWinCall(RsElevationAbort,'JwShellExecute','',RsUNElevation,0,
                      true,'JwShellExecute',[])
        else
        begin
          //encapsulate the failed win call
          P := EJwsclJwShellExecuteException.CreateFmtWinCall(E.Message,'JwShellExecute','',RsUNElevation,0,
                      true,'JwShellExecute',[]);
          P.LastError := E.LastError;
          raise P;
        end;
      end;

    end;
  end;

  function RunSuRun : THandle;
  var
    SuRunCode : Cardinal;
    Shell : {$IFDEF UNICODE}TShellExecuteInfoW;{$ELSE}TShellExecuteInfoA;{$ENDIF}
    IsSuRunError : Boolean;
  begin
    //SuRun does not support PID return value on older versions than 1.2.0.6
    result := 0; //don't use -1 it is what GetcurrentProcess returns. Could be mixed up.

    {This section uses the surun verb to execute the process
    as an administrator.
    Get SuRun from http://kay-bruns.de/wp/software/surun/
    }
    ZeroMemory(@Shell, sizeof(Shell));
    Shell.cbSize := sizeof(Shell);
    Shell.lpVerb := 'SuRun';
    Shell.lpFile := TJwPChar(TJwString(ParamStr(0)));
    Shell.lpParameters := TJwPChar(Parameters);
    Shell.lpDirectory := TJwPChar(Directory);
    Shell.fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_NO_UI;
    Shell.hwnd := hWindow;

    if not  {$IFDEF UNICODE}ShellExecuteExW{$ELSE}ShellExecuteExA{$ENDIF}(Shell) then
    begin
      raise EJwsclShellExecuteException.CreateFmtWinCall(RsSuRunShellExecute,'JwElevateProcess::RunUAC','',RsUNElevation,0,
        true,'ShellExecuteEx',[]);
    end;


    //60sec timeout period
    case WaitForSingleObject(Shell.hProcess, 60*1000) of
      WAIT_TIMEOUT :
        begin
          SetLastError(WAIT_TIMEOUT);
          raise EJwsclSuRunErrorException.CreateFmtWinCall(RsSuRunShellExecute,'JwElevateProcess::RunSuRun','',RsUNElevation,0,
                      true,'SuRun',[]);
        end;
      WAIT_FAILED :
        begin
          SetLastError(WAIT_FAILED);
          raise EJwsclSuRunErrorException.CreateFmtWinCall(RsSuRunShellExecute,'JwElevateProcess::RunSuRun','',RsUNElevation,0,
                      true,'SuRun',[]);
        end;
      WAIT_OBJECT_0 :
        begin
          if GetExitCodeProcess(Shell.hProcess,SuRunCode) then
          begin
            {
            #define RETVAL_OK           0
            #define RETVAL_ACCESSDENIED 1
            #define RETVAL_RESTRICT     3
            #define RETVAL_CANCELLED    4
            }

            if SuRunStatus.PIDSupport then
            begin
              //If SuRun supports PID return value
              //and Code is smaller 100 and not 4 (cancel elevation)
              //there is a real error
              IsSuRunError := (SuRunCode < 100) and (SuRunCode <> 4);
            end
            else
            begin
              IsSuRunError := (SuRunCode <> 0) and (SuRunCode <> 4);
            end;

            if IsSuRunError then
            begin
              SetLastError(SuRunCode);
              raise EJwsclSuRunErrorException.CreateFmtWinCall(RsSunRunFailed,'JwElevateProcess::RunSuRun','',RsUNElevation,0,
                      true,'SuRun',[SuRunCode]);
            end
            else
              result := SuRunCode; //return PID on success (otherwise it's 0)

            if SuRunCode = 4 then
            begin
              SetLastError(E_USER_CANCELED_OPERATIONint);
              raise EJwsclAbortException.CreateFmtWinCall(RsElevationAbort,'JwElevateProcess::RunSuRun','',RsUNElevation,0,
                      true,'SuRun',[]);
            end;
          end;
        end;                        
    end;
  end;

  function RunCreateProcess : THandle;
  var
    lpStartupInfo: STARTUPINFOW;
    lpProcessInformation: PROCESS_INFORMATION;
    UserName, Password : TJwString;
    Abort : Boolean;
    CurDir,
    CmdLine : PWideChar;
    Environment : Pointer;
    Flags : DWORD;

    DecryptedPassword : TJwString;
    IsEncryptedPassword : Boolean;
    Entropy : PDataBlob;
    EncryptionPrompt : Boolean;
  begin
    UserName := 'Administrator';
    Password := '';
    Abort := false;

    Environment := nil;


    ZeroMemory(@lpStartupInfo, sizeof(lpStartupInfo));

    IsEncryptedPassword := false;
    Entropy := nil;
    EncryptionPrompt := false;
    {get
      abort status
      username + password
      environment for createprocess
      startupinfoW for createprocess 
    }
    if Assigned(OnElevationGetCredentials) then
      OnElevationGetCredentials(Abort, UserName, Password,
        IsEncryptedPassword, Entropy, EncryptionPrompt,
        Environment, lpStartupInfo);

    if Abort then
    begin
      SetLastError(E_USER_CANCELED_OPERATIONint);
      raise EJwsclAbortException.CreateFmtWinCall(RsElevationAbort,'JwElevateProcess::RunSuRun','',RsUNElevation,0,
                      true,'OnElevationGetCredentials',[]);
    end;

    //setup commandline
    if Length(Parameters) > 0 then
      CmdLine := PWideChar(WideString(Parameters))
    else
      CmdLine := nil;

    //setup directory  
    if Length(Directory) > 0 then
      CurDir := PWideChar(WideString(Directory))
    else
      CurDir := nil;

    //prevent size change from event  
    lpStartupInfo.cb := sizeof(lpStartupInfo);

    Flags := CREATE_NEW_CONSOLE;
    if Environment <> nil then
      Flags := Flags or CREATE_UNICODE_ENVIRONMENT;

    if IsEncryptedPassword then
    begin
      DecryptedPassword := JwDecryptString(Password, EncryptionPrompt, Entropy);
    end;

    try
      if not CreateProcessWithLogonW(
        PWideChar(WideString(UserName)),//lpUsername,
        '',//lpDomain,
        PWideChar(WideString(DecryptedPassword)),//lpPassword: LPCWSTR;
        LOGON_WITH_PROFILE,//dwLogonFlags: DWORD;
        PWideChar(WideString(FileName)),//lpApplicationName: LPCWSTR;
        CmdLine,//lpCommandLine: LPWSTR;
        Flags,//dwCreationFlags: DWORD;
        Environment,//lpEnvironment: LPVOID;
        CurDir,//lpCurrentDirectory: LPCWSTR;
        lpStartupInfo,//const lpStartupInfo: STARTUPINFOW;
        lpProcessInformation//var lpProcessInformation: PROCESS_INFORMATION
      ) then
      raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,'JwElevateProcess::RunSuRun','',
         RsUNElevation,0, true,'CreateProcessWithLogonW',['CreateProcessWithLogonW']);
    finally
      JwZeroPassword(Password);
      JwZeroPassword(DecryptedPassword);
      if Environment <> nil then
        DestroyEnvironmentBlock(Environment);
    end;

    CloseHandle(lpProcessInformation.hThread);
    try
      result := {localcall}GetProcessId(lpProcessInformation.hProcess);
    finally
      CloseHandle(lpProcessInformation.hProcess);
    end;
  end;




begin
  //is SuRun allowed?
  SuRunAvail := (epfAllowSuRun in ElevationProcessFlags) and JwCheckSuRunStatus(SuRunStatus);

  //standard flags for JwShellExecute
  UACFlags := [sefNoUi, sefNoClosehProcess];

  //remove flags from UAC if neccessary
  if (epfNoUi in ElevationProcessFlags) then
    Exclude(UACFlags, sefNoUi);

  //use SuRun first and if enabled
  //regardeless of OS
  if SuRunAvail then
  begin
    result := RunSuRun;
  end
  else
  //if UAC is supported by OS
  if TJwWindowsVersion.IsWindowsVista(true) or
     TJwWindowsVersion.IsWindows2008(true) then
  begin
    //this flags raises an exception if UAC is not available
    Exclude(UACFlags, sefIgnoreElevationIfNotAvailable);
    try
      result := RunUAC;
    except
      on E : EJwsclUnsupportedWindowsVersionException do
      begin
        //if UAC is not available though, use CreateProcess...
        result := RunCreateProcess;
      end;
      //otherwise we get EJwsclWinCallFailedException or EJwsclAbortException
    end;
  end
  else
  begin
    //if UI is allowed use RunAs verb with Shellexecute
    // it shows an credential dialog
    if not (epfNoUi in ElevationProcessFlags) then
    begin
      //ShellExecute can show credential prompt
      Exclude(UACFlags, sefNoUi);
      //ignore UAC
      Include(UACFlags, sefIgnoreElevationIfNotAvailable);
      Include(UACFlags, sefFixDirWithRunAs);

      //in 2000/XP, don't use UAC instead use RunAs verb
      result := RunUAC
    end
    else
      //otherwise use CreateProcessWithLogonW with event
      //because noUI is set. ShellExecute does not work this way
      result := RunCreateProcess;
  end;
end;


function JwShellExecute(const hWnd: HWND;  FileName, Parameters,
  Directory: TJwString; ShowCmd: Integer; Flags : TJwShellExecuteFlags = [sefNoClosehProcess]): HANDLE;
var
  shExecInfo : {$IFDEF UNICODE}SHELLEXECUTEINFOW{$ELSE}SHELLEXECUTEINFOA{$ENDIF};
  Token : TJwSecurityToken;
  IsElevated : Boolean;
begin
  result := 0;

  SetLastError(0);
  ZeroMemory(@shExecInfo,sizeof(shExecInfo));

  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_QUERY or TOKEN_READ);
  try
    try
      IsElevated := Token.RunElevation <> 0;
    except
      on E : EJwsclSecurityException do
      begin
        //On 2000,XP admin is elevated
        IsElevated := JwCheckAdministratorAccess;
        FreeAndNil(Token);
        if not (sefIgnoreElevationIfNotAvailable in Flags) then
          raise;
      end;
    end;

    if not IsElevated then
      shExecInfo.lpVerb := TJwPChar(TJwString('runas'))
    else //type mismatch? Recompile whole project: ansicode <-> unicode
      shExecInfo.lpVerb := TJwPChar(TJwString('open'));


    shExecInfo.cbSize := sizeof(SHELLEXECUTEINFO);
    shExecInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
    if sefNoUi in Flags then
      shExecInfo.fMask := shExecInfo.fMask or SEE_MASK_FLAG_NO_UI;
    
    shExecInfo.hwnd := hWnd;


    if (sefFixDirWithRunAs in Flags) and (Length(Directory) > 0) and
      not IsElevated then
    begin
      shExecInfo.lpFile := 'cmd.exe';
      {Shellexecute does not set directory when combined with verb "runas"
      so we call cmd, set the directory and then call the application
      This may lead to a remaining cmd window
      }
      shExecInfo.lpParameters := TJwPChar(TJwString('/C cd /d "'+Directory+'" & '+ FileName + ' '+Parameters));
      //shExecInfo.lpFile := 'cmd';
      //shExecInfo.lpParameters := TJwPChar(TJwString('/c "start /D "'+Directory+'" "'+ FileName + '" '+Parameters)+'"');
      shExecInfo.lpDirectory := nil;
    end
    else
    begin
      shExecInfo.lpFile := TJwPChar(FileName);
      shExecInfo.lpParameters := TJwPChar(Parameters);
      shExecInfo.lpDirectory := TJwPChar(Directory);
    end;
    //shExecInfo.lpDirectory := TJwPChar('"'+Directory+'"');



    shExecInfo.nShow := ShowCmd;
    shExecInfo.hInstApp := NULL;

    SetLastError(0);
    if {$IFDEF UNICODE}ShellExecuteExW{$ELSE}ShellExecuteExA{$ENDIF}(shExecInfo) then
      result := shExecInfo.hProcess
    else
      raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,'src','',RsUNElevation,0,
            true,'ShellExecuteEx',['ShellExecuteEx']);

    if (result <> 0) and not (sefNoClosehProcess in Flags) then
    begin
      CloseHandle(result);
      result := 0;
    end;
  finally
    FreeAndNil(Token);
  end;
end;




threadvar ResultValue : HRESULT;

function JwCoCreateInstanceAsEx(
  const MonikerSequence : WideString;
  const ParentWindowHandle: HWND;
  const ClassId: TGUID;
  const IID: TGUID;
  out ObjectInterface) : HRESULT;
var
  MonikerName : WideString;
  BindOptions : TBindOpts3;
  Token : TJwSecurityToken;

  iLen : Cardinal;
begin
  ResultValue := 0;

  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_QUERY or TOKEN_READ);

  try
    if Token.RunElevation = 0 then
    begin
      MonikerName := MonikerSequence + GUIDToString(ClassId);

      iLen := SizeOf(TBindOpts3);
      FillChar(BindOptions, iLen, 0);

      BindOptions.cbStruct := iLen;
      BindOptions.dwClassContext := CLSCTX_LOCAL_SERVER;
      BindOptions.hwnd := ParentWindowHandle;

      result := JwaWindows.CoGetObject(PWideChar(MonikerName), @BindOptions, IID, ObjectInterface);
      if result = E_USER_CANCELED_OPERATION then
        ResultValue := ERROR_CANCELLED
      else
      if result = E_CLASS_IS_NOT_SETUP then
        ResultValue := E_CLASS_IS_NOT_SETUP//ERROR_INVALID_ACCESS
      else
        //ResultValue := ERROR_ACCESS_DENIED;
        ResultValue := result;
    end
    else
    begin
      result := CoCreateInstance(ClassID, nil, CLSCTX_ALL, IID, ObjectInterface);
    end;
  finally
    Token.Free;
  end;
end;

function JwCoCreateInstanceAsAdmin(
  const ParentWindowHandle: HWND;           
  const ClassId: TGUID;
  const IID: TGUID;
 out ObjectInterface) : HRESULT;
begin
  result := JwCoCreateInstanceAsEx(
    'Elevation:Administrator!new:', ParentWindowHandle, ClassId, IID, ObjectInterface);
  SetLastError(ResultValue);
end;


function JwCoCreateInstanceAsHighest(
  const ParentWindowHandle: HWND;
  const ClassId: TGUID;
  const IID: TGUID;
  out ObjectInterface) : HRESULT;
begin
  result := JwCoCreateInstanceAsEx(
    'Elevation:Highest!new:', ParentWindowHandle, ClassId, IID, ObjectInterface);
  SetLastError(ResultValue);
end;

function JwCoGetClassFactoyAsAdmin(
  const ParentWindowHandle: HWND;
  const ClassId: TGUID;
  const IID: TGUID;
  out ObjectInterface) : HRESULT;
begin
  result := JwCoCreateInstanceAsEx(
    'Elevation:Administrator!clsid:', ParentWindowHandle, ClassId, IID, ObjectInterface);
  SetLastError(ResultValue);
end;


{ TJwElevationClassFactory }

constructor TJwElevationClassFactory.Create(const ResourceId: AnsiString;
  const DisableProcessIsolation : Boolean;
  const ComServer: TComServerObject; const TypedComClass: TTypedComClass;
  const ClassId: TGUID; const Instancing: TClassInstancing;
  const ThreadingModel: TThreadingModel);
begin
  inherited Create(ComServer, TypedComClass, ClassId, Instancing, ThreadingModel);
  fResourceId := ResourceId;
  fDisableProcessIsolation := DisableProcessIsolation;
end;

constructor TJwElevationClassFactory.Create(const ResourceId: PResStringRec;
  const DisableProcessIsolation : Boolean;
  const ComServer: TComServerObject; const TypedComClass: TTypedComClass;
  const ClassId: TGUID; const Instancing: TClassInstancing;
  const ThreadingModel: TThreadingModel);
begin
  Create(AnsiString(IntToStr(GetResourceStringIdentifier(ResourceId))),
    DisableProcessIsolation, ComServer, TypedComClass, ClassId, Instancing, ThreadingModel);
end;

(*
HKEY_LOCAL_MACHINE\SOFTWARE\Classes
{
  CLSID\{6BCFB187-C1DD-4807-96AD-F91AB4AB08AC}
  {
    (Default):       REG_SZ = 'MyPrivilegedObject'
    AppID:           REG_SZ = '{6BCFB187-C1DD-4807-96AD-F91AB4AB08AC}'
    LocalizedString: REG_SZ = '@C:\Your\Path\Here\PrivilegedLib.dll,-101'

    Elevation
    {
      (Default): REG_SZ    = null
      Enabled:   REG_DWORD = 1
    }
  }
}
*)


{$IFDEF UNIT_TEST}
class procedure TJwElevationClassFactory.UpdateRegistry(RegisterFactory: Boolean);
{$ELSE}
procedure TJwElevationClassFactory.UpdateRegistry(RegisterFactory: Boolean);

  procedure RaiseRegError(Reason, Key : String);
  begin
    try
      raise EJwsclAccessDenied.CreateFmtEx(
        Reason,
        'UpdateRegistry', ClassName, RsUNElevation, 0, false,
        [Key]);
    except
      on E : Exception do
      begin
        if Self.ShowErrors then
          MessageBoxW(0, PWideChar(WideString(E.Message)), 'Error', MB_ICONHAND or MB_OK)
        else
          raise;
      end;
    end;
  end;
{$ENDIF UNIT_TEST}
var
  Reg : TRegistry;
  GuidString,
  DllPath,
  DllName : String;

  SD : TJwSecurityDescriptor;
  pSecDescr : PSecurityDescriptor;
  SecSize : Cardinal;
  RootKey : HKEY;


const
  ClsIdKey = 'CLSID\';
  AppIdKey = 'AppID\';
  ClassesKey = 'SOFTWARE\Classes\';
  DllSurrogateValue = 'DllSurrogate';
  ElevationKey = '\Elevation';
  AppIdValue = 'AppID';
  AccessPermissionValue = 'AccessPermission';
  LocalizedStringValue = 'LocalizedString';
  EnabledValue = 'Enabled';
  DisableProcessIsolationValue = 'DisableProcessIsolation';

{$IFDEF UNIT_TEST}
  Description = 'mydescr';
  fResourceId = '123';
  fDisableProcessIsolation = false;
{$ENDIF UNIT_TEST}

begin
  //JwInitWellKnownSIDs;

{$IFDEF UNIT_TEST}
  //
  // This is not production code!
  //
  DllPath := 'c:\programme\mydll.dll';
  DllName := ExtractFileName(DllPath);
  GuidString := '{E108B186-B399-4E46-99B4-345F8179C26E}';
  RootKey := HKEY_CURRENT_USER;
{$ELSE}
  DllPath := ComServer.ServerFileName;
  DllName := ExtractFileName(DllPath);
  GuidString := GUIDToString(Self.ClassId);
  RootKey := HKEY_LOCAL_MACHINE;
{$ENDIF UNIT_TEST}

  if RegisterFactory then
  begin
    (*
    HKEY_LOCAL_MACHINE\SOFTWARE\Classes
    {
    AppID\xxxxxxxxxxxxx.dll
    {
     (Default): REG_SZ = null
     AppID:     REG_SZ = '{xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}'
    }
    *)

    Reg := TRegistry.Create(KEY_SET_VALUE or KEY_WRITE);
    Reg.RootKey := RootKey;
    try
      if Reg.OpenKey(ClassesKey+AppIdKey+DllName,true) then
        Reg.WriteString(AppIdValue, GuidString)
      else
        RaiseRegError(RsElevationRegCreateError, ClassesKey+AppIdKey+DllName);
    finally
      Reg.Free;
    end;

    (*
    HKEY_LOCAL_MACHINE\SOFTWARE\Classes
    {
    AppID\{xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}
    {
      (Default):        REG_SZ     = <Description>
      AccessPermission: REG_BINARY = <BINARY VALUE>
      DllSurrogate:     REG_SZ     = ''
    }
    *)

    Reg := TRegistry.Create(KEY_SET_VALUE or KEY_WRITE);
    Reg.RootKey := RootKey;
    try
      if Reg.OpenKey(ClassesKey+AppIdKey+GuidString,true) then
      begin
        Reg.WriteString('', Description);
        Reg.WriteString(DllSurrogateValue, '');

        { Request local call permissions for InteractiveUser and System. }
        SD := TJwSecurityDescriptor.Create('O:BAG:BAD:(A;;0x3;;;IU)(A;;0x3;;;SY)');

        try
         { SD.OwnOwner := false;
          SD.Owner := JwAdministratorsSID;
          SD.OwnPrimaryGroup := false;
          SD.Owner := JwAdministratorsSID;


          SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create
            (nil,[],KEY_QUERY_VALUE or KEY_SET_VALUE, JwLocalSystemSID, false));
          SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create
            (nil,[],KEY_QUERY_VALUE or KEY_SET_VALUE, TJwSecurityId.Create('S-1-5-4')));
                                             }
          pSecDescr := SD.Create_SD(SecSize, true);
          try
            Reg.WriteBinaryData(AccessPermissionValue, pSecDescr^, SecSize);
          finally
            SD.Free_SD(pSecDescr);
          end;

        finally
          SD.Free;
        end;

      end
      else
        RaiseRegError(RsElevationRegCreateError, ClassesKey+AppIdKey+GuidString);
    finally
      Reg.Free;
    end;

    (*
    HKEY_LOCAL_MACHINE\SOFTWARE\Classes
{
  CLSID\{xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}
  {
    (Default):       REG_SZ = <Description>
    AppID:           REG_SZ = '{xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}'
    LocalizedString: REG_SZ = '@<dllpath>,-<resourceid>'


  }
    *)
    Reg := TRegistry.Create(KEY_SET_VALUE or KEY_WRITE);
    Reg.RootKey := RootKey;
    try
      if Reg.OpenKey(ClassesKey+ClsIdKey+GuidString,true) then
      begin
        Reg.WriteString('', Description);
        Reg.WriteString(AppIdValue, GuidString);
        Reg.WriteString(LocalizedStringValue, Format('@%s,-%s',[DllPath,fResourceId]));

        Reg.WriteBool(DisableProcessIsolationValue, fDisableProcessIsolation);
      end
      else
        RaiseRegError(RsElevationRegCreateError, ClassesKey+ClsIdKey+GuidString);
    finally
      Reg.Free;
    end;

    (*
    HKEY_LOCAL_MACHINE\SOFTWARE\Classes\CLSID\{xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}\Elevation
    {
      (Default): REG_SZ    = null
      Enabled:   REG_DWORD = 1
    }
    *)
    Reg := TRegistry.Create(KEY_SET_VALUE or KEY_WRITE);
    Reg.RootKey := RootKey;
    try
      if Reg.OpenKey(ClassesKey+ClsIdKey+GuidString+ElevationKey,true) then
        Reg.WriteInteger(EnabledValue, 1)
      else
        RaiseRegError(RsElevationRegCreateError, ClassesKey+ClsIdKey+GuidString+ElevationKey);
    finally
      Reg.Free;
    end;

{$IFNDEF UNIT_TEST}
    inherited UpdateRegistry(RegisterFactory);
{$ENDIF UNIT_TEST}

  end
  else
  begin
    Reg := TRegistry.Create(KEY_SET_VALUE or KEY_WRITE or DELETE);
    Reg.RootKey := RootKey;
    try
      if Reg.KeyExists(ClassesKey+AppIdKey+DllName) then
        if not Reg.DeleteKey(ClassesKey+AppIdKey+DllName) then;
          //RaiseRegError(RsElevationRegDeleteError, ClassesKey+AppIdKey+DllName);

      if Reg.KeyExists(ClassesKey+AppIdKey+GuidString) then
        if not Reg.DeleteKey(ClassesKey+AppIdKey+GuidString) then;
          //RaiseRegError(RsElevationRegDeleteError, ClassesKey+AppIdKey+GuidString);

      if Reg.KeyExists(ClassesKey+ClsIdKey+GuidString+ElevationKey) then
        if not Reg.DeleteKey(ClassesKey+ClsIdKey+GuidString+ElevationKey) then;
          //RaiseRegError(RsElevationRegDeleteError, ClassesKey+ClsIdKey+GuidString+ElevationKey);

      if Reg.KeyExists(ClassesKey+ClsIdKey+GuidString) then
        if not Reg.DeleteKey(ClassesKey+ClsIdKey+GuidString) then;
          //RaiseRegError(RsElevationRegDeleteError, ClassesKey+ClsIdKey+GuidString);
    finally
      Reg.Free;
    end;

    inherited UpdateRegistry(RegisterFactory);
  end;
end;

{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}

initialization
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INITIALIZATION_SECTION}
{$ENDIF SL_INITIALIZATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}
