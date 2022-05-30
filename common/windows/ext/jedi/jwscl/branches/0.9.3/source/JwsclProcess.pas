{
Description
Project JEDI Windows Security Code Library (JWSCL)

Contains process, thread und library classes that are used by the units of JWSCL

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
The Original Code is JwsclProcess.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclProcess;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $

interface

uses SysUtils, Classes,
  JwaWindows,
  JwsclTypes, JwsclToken, JwsclSid, JwsclTerminalServer, JwsclUtils,
  JwsclSecureObjects, JwsclResource,
  JwsclLogging, JwsclLsa, JwsclDescriptor,JwsclEnumerations, JwsclComUtils,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!
{$ENDIF SL_OMIT_SECTIONS}


{$IFNDEF SL_IMPLEMENTATION_SECTION}

const
//  IID_IJwJobObject = '{5F6DBA7A-B8DC-498E-A151-49AD0DCD8CF8}';

  //Internal job name prefix
  IOJOBNAME = 'IOJobCompletion\';

type
  {<B>TJwLibraryUtilities</B> contains methods related to libraries.}
  TJwLibraryUtilities = class
  private
  protected
  public
    {<B>LoadLibProc</B> tries to get a pointer to a function within a DLL.
     @return Return value is a function pointer to the specified function.

     raise
      EJwaLoadLibraryError This exception is raised if the given library name could not be found.
      EJwaGetProcAddressError This exception is raised if the given function name could not be found.
    }
    class function LoadLibProc(const LibName: AnsiString;
      const ProcName: AnsiString): Pointer;
  end;
{
  IJwJobObject = interface
   [IID_IJwJobObject]
     procedure AssignProcessToJobObject(hProcess : TJwProcessHandle);

BOOL WINAPI AssignProcessToJobObject(HANDLE hJob, HANDLE hProcess);
HANDLE WINAPI CreateJobObject(LPSECURITY_ATTRIBUTES lpJobAttributes,LPCTSTR lpName);
HANDLE WINAPI OpenJobObject(DWORD dwDesiredAccess,BOOL bInheritHandles, LPCTSTR lpName);
BOOL WINAPI SetInformationJobObject(HANDLE hJob,JOBOBJECTINFOCLASS JobObjectInfoClass,
        LPVOID lpJobObjectInfo,DWORD cbJobObjectInfoLength);
BOOL WINAPI QueryInformationJobObject(HANDLE hJob, JOBOBJECTINFOCLASS JobObjectInfoClass,
        LPVOID lpJobObjectInfo, DWORD cbJobObjectInfoLength, LPDWORD lpReturnLength);
BOOL WINAPI IsProcessInJob(HANDLE ProcessHandle, HANDLE JobHandle, PBOOL Result);
BOOL WINAPI TerminateJobObject(HANDLE hJob, UINT uExitCode);

  end;
 }
  TJwProcessList = array of TJwProcessId;


  {<B>TJwWaitState</B> defines return values for the method
   WaitForAllotedCPUTimeSignal.
   For more information see this method.}
  TJwWaitState = (
    wsNonSignaled,
    wsSignaled,
    wsTimeOut);



  TJwJobObject = class;
  TJwJobObjectSessionList = class;

  {<B>TJwOnJobNotification</B> is called when a job notification occurs.
    @param Sender Contains the JobObject that received the job message.
    @param ProcessID Contains the ID of the process that generated the message.
    @param JobMessages Contains the type of job message.
    @param Data Contains the given custom data set by AssignProcessToJobObject.
  }
  TJwOnJobNotification = procedure (Sender : TJwJobObject; ProcessId : TJwProcessId;
    JobMessages : TJwJobMessages; Data : Pointer) of object;

  {<B>TJwOnNoActiveProcesses</B> will be called when the job object ran out
  of processes.
  @param Sender Contains the JobObject that received the job message.
  }
  TJwOnNoActiveProcesses = procedure (Sender : TJwJobObject) of object;

  {<B>TJwInternalJobObjectIOCompletitionThread</B> is for internal use only.}
  TJwInternalJobObjectIOCompletitionThread = Class(TJwThread)
  protected
    fJwJobObject : TJwJobObject;
    fIOHandle : THandle;
    fRemainPort : Boolean;
  public
    constructor Create(const Parent: TJwJobObject;
      const CreateSuspended: Boolean; const Name: TJwString);
    constructor CreateWithIOPort(const Parent: TJwJobObject; IOPort : THandle;
      const CreateSuspended: Boolean; const Name: TJwString);
    procedure Execute; override;

    procedure Terminate; reintroduce;
    property IOHandle : THandle read fIOHandle;
  end;

  {<B>TJwJobObject</B> is the main job class. It encapsulates a
  job object and provides methods and properties to maintain it.
  There are also events that are fired on special job messages.

  All processes of a session must have the same token session id,
  otherwise the assignment fails.
  }
  TJwJobObject = class
  protected
    fHandle : THandle;
    fAccessMask : TJwAccessMask;

    fLock : TMultiReadExclusiveWriteSynchronizer;
    fDataList : TJwIntTupleList;

    fIOUniqueID : Integer;

    fJobName : TJwString;
    fSession : TJwSessionId;

    fTerminateOnDestroy : Boolean;

    fNotification : TJwOnJobNotification;
    fOnNoActiveProcesses : TJwOnNoActiveProcesses;

    fThread: TJwInternalJobObjectIOCompletitionThread;

    function GetJobObjectInformationLength(
       const JobObjectInfoClass : JOBOBJECTINFOCLASS) : Cardinal;

    procedure GetJobObjectInformation(
       const JobObjectInfoClass : JOBOBJECTINFOCLASS;
       out Data : Pointer);
  protected
    function GetProcesses : TJwProcessList;
    function GetProcessesCount : Cardinal;

    function GetBasicAndIOInformation : TJobObjectBasicAndIoAccountingInformation;
    function GetBasicLimitInformation : TJobObjectBasicLimitInformation;
    function GetBasicUIRestrictions : TJobObjectBasicUiRestrictions;
    procedure SetBasicUIRestrictions(Info : TJobObjectBasicUiRestrictions);

    function GetExtendedLimitInformation : TJobObjectExtendedLimitInformation;
    procedure SetExtendedLimitInformation(Info : TJobObjectExtendedLimitInformation);

    procedure SetBasicLimitInformation(Info : TJobObjectBasicLimitInformation);


    function GetAllotedCPUTimeSignalState : Boolean;

    function GetJobLimit : TJwJobLimits;
    procedure SetJobLimit(const Limit : TJwJobLimits);

    function GetJobUiLimit : TJwJobUiLimits;
    procedure SetJobUiLimit(const Limit : TJwJobUiLimits);

    function GetActiveProcessCount : Cardinal;

    procedure SetObjectAssociateCompletionPortInformation(
        const CompletionKey : Pointer; CompletionPort : THandle);

    //not supported by winapi
    procedure GetObjectAssociateCompletionPortInformation(
        out CompletionKey : Pointer; out CompletionPort : THandle);

    function GetIOHandle : THandle;


  public
    {@Param SecurityAttributes defines the security information for the job object.
      Use TJwSecurityDescriptor.InheritHandles to control handle inheritance.
    }
    constructor Create(const Name : TJwString;
      const ErrorIfAlreadyExists : Boolean;
      const SecurityAttributes : TJwSecurityDescriptor); overload;

    {<B>Create</B> creates a new job object using an existing completition port.}  
    constructor Create(const Name : TJwString;
      const ErrorIfAlreadyExists : Boolean;
      const SecurityAttributes : TJwSecurityDescriptor;
      CompletionKey : Integer; CompletionPort : THandle); overload;

    {<B>Create</B> creates a new job object using an existing job object
    @param Name defines the name of the existing job object 
    @param DesiredAccess defines the desired access to open the job object 
    @param InheritHandles defines whether processes created by this process
        will inherit the handle. Otherwise, the processes do not inherit this handle.
    @param CompletionKey defines an existing completion key to be assigned or
     used by the opened object. It is used for notifications. 
    @param CompletionPort defines an existing completion handle to be assigned or
     used by the opened object. It is used for notifications. 

    }
    constructor Create(const Name : TJwString; const DesiredAccess : TJwAccessMask;
      const InheritHandles : Boolean; CompletionKey : Integer; CompletionPort : THandle); overload;

    destructor Destroy; override;

    {<B>IsProcessInJob</B> returns whether a process is assigned to the job.
    @param hProcess defines any handle to the process that is tested for membership.
    @param Returns tre if the process is a member of the job; otherwise false.
    @return Returns true if the given process is assigned to the current job instance; otherwise false. 
    raises
 EJwsclWinCallFailedException:  can be raised if the call to an winapi function failed.

    }
    function IsProcessInJob(hProcess : TJwProcessHandle) : Boolean;

    {
    <B>AssignProcessToJobObject</B> assigns an existing process to the job. The process must not already be
    assigned to a job. The process can be created with the flag CREATE_BREAKAWAY_FROM_JOB
    to be reassignable.
    raises
 EJwsclWinCallFailedException:  can be raised if the call to an winapi function failed. 
    }
    procedure AssignProcessToJobObject(hProcess : TJwProcessHandle; Data : Pointer);

    {<B>TerminateJobObject</B> terminates all processes in the job with a predefined
     exit code.
    raises
 EJwsclWinCallFailedException:  can be raised if the call to an winapi function failed. 
    }
    procedure TerminateJobObject(const ExitCode : DWORD);

    {<B>ResetIOThread</B> resets the internal IO completition thread. This thread is necessary
    for triggering OnNotification and OnNoActiveProcesses.
      @param Force defines whether the currenct IO thread should be forcibly terminated
       and restarted. If true the thread is shutdown and a new thread is created.
       However the IO port is not replaced. In fact there is no way to reassign a new
       port to the existing job object. If the parameter Force is set to false,
       the thread is only reset if it is not running. 
    raises
 EJwsclWinCallFailedException:  can be raised if the call to an winapi function failed. 
    }
    procedure ResetIOThread(const Force : Boolean);

    {Waits on the job object with a possible timeout and returns the
     result of the waiting.

     @param Timeout Takes a timeout in mili seconds or INFINITE if no timeout.
     @return Returns wsSignaled if the job object was signled or wsTimeOut if
       the given timeout occured. Any other state results in a wsNonSignaled return value.
    raises
       EJwsclWinCallFailedException:  can be raised if the call to an winapi function failed.
    }
    function WaitForAllotedCPUTimeSignal(const TimeOut : DWORD) : TJwWaitState;

    {<B>GetSecurityDescriptor</B> returns a security descriptor filled with the parts given
     in parameter Si.}
    function GetSecurityDescriptor(const Si : TJwSecurityInformationFlagSet) : TJwSecurityDescriptor;

    {<B>TerminateOnDestroy</B> defines whether all processes should be terminated. If set to true
     and the @Classname instance is freed all processes assigned to this job
     are terminated. Default value is false. No process will be terminated.
    }
    property TerminateOnDestroy : Boolean read fTerminateOnDestroy write fTerminateOnDestroy;

    {<B>UiLimits</B> reads or sets User Interface limits of this job.
     This property has the same effect as property BasicUIRestrictions.

     May raise EJwsclWinCallFailedException if an error occurs.
    }
    property UiLimits : TJwJobUiLimits read GetJobUiLimit write SetJobUiLimit;

    {<B>AllotedCPUTimeSignalState</B> receives the signal state of the job. It is set to true
    if all processes has allotted their CPU time restriction; otherwise it is false.
    }
    property AllotedCPUTimeSignalState : Boolean read GetAllotedCPUTimeSignalState;

    {<B>BasicLimitInformation</B> reads or sets basic limit information of this job.
     May raise EJwsclWinCallFailedException if an error occurs.
    }
    property BasicLimitInformation : TJobObjectBasicLimitInformation
      read GetBasicLimitInformation write SetBasicLimitInformation;

    {<B>BasicAndIOInformation</B> reads or sets basic limit and IO information of this job.
     May raise EJwsclWinCallFailedException if an error occurs.
    }
    property BasicAndIOInformation : TJobObjectBasicAndIoAccountingInformation read GetBasicAndIOInformation;

    {<B>BasicUiRestrictions</B> reads or sets basic limit and IO information of this job.
     This property has the same effect as property UiLimits.

     May raise EJwsclWinCallFailedException if an error occurs.
    }
    property BasicUiRestrictions : TJobObjectBasicUiRestrictions read GetBasicUIRestrictions write SetBasicUIRestrictions;

    {<B>ExtendedLimitInformation</B> reads or sets extended limit information of this job.
     May raise EJwsclWinCallFailedException if an error occurs.
    }
    property ExtendedLimitInformation : TJobObjectExtendedLimitInformation read GetExtendedLimitInformation write SetExtendedLimitInformation;

    {<B>ActiveProcessCount</B> returns the current active count of processes in this job.
    May raise EJwsclWinCallFailedException if an error occurs.
    }
    property ActiveProcessCount : Cardinal read GetActiveProcessCount;

    {<B>Handle</B> returns the handle of the job}
    property Handle : THandle read fHandle;

    {<B>AccessMask</B> returns the access mask of this job object as specified in Create
    when opening an existing job object}
    property AccessMask : TJwAccessMask read fAccessMask;

    {<B>Processes</B> returns an array of process ID values.
     Valud values are from low(Processes) to high(Processes).
     Always use a temporary variable for this property because
     each access to this property needs a system call.
     }
    property Processes : TJwProcessList read GetProcesses;

    {<B>IOUniqueID</B> returns a unique IO completion ID that was assigned to the IO
     completion port as key ID.
    }
    property IOUniqueID : Integer read fIOUniqueID;
    property IOHandle : THandle read GetIOHandle;

    {<B>Name</B> returns the name of the job object give at creation time.}
    property Name : TJwString read fJobName;

    {<B>OnNotification</B> can be used to be informed about changed information of a process }
    property OnNotification : TJwOnJobNotification read fNotification write fNotification;

    {<B>OnNoActiveProcesses</B> will be called when the last process terminates}
    property OnNoActiveProcesses : TJwOnNoActiveProcesses read fOnNoActiveProcesses write fOnNoActiveProcesses;

    {<B>Session</B> defines the session of the containg projects}
    property Session : TJwSessionId read fSession write fSession;

    {<B>Lock</B> returns the internal thread syncronisation object.
    It can be used to avoid problems with several threads.
    It should be used for property JobObject because calls to this property
     may become invalid during processing.
    <code lang="Delphi">
        Job.Lock.BeginWrite;
        try
          do sth with Job.JobObject[x]
        finally
          Job.Lock.EndWrite;
        end;                                                                    
    </code>
    }
    property Lock : TMultiReadExclusiveWriteSynchronizer read fLock;

    {<B>DataList</B> contains all process handles and their associated data
    (if any) assigned by the method AssignProcessToJobObject.
    }
    property DataList : TJwIntTupleList read fDataList;
  end;




  {<B>TJwOnNewJobObject</B> is called by TJwJobObjectSessionList.AssignProcessToJob
  for every new job object that mus be created.
  The callback event must create a new job object and return it
   through parameter NewJobObject. The return value must not be nil.
  The new job object does not need to have a name.
  @param Sender defines the job object list instance that calls this event 
  @param ProcessHandle defines the process to be assigned to the job
  @param ProcessSessionID defines the session id that the given process
   belongs to 
  @param CurrentSessionID defines a session index. The event method can
   be called several times for one call of AssignProcessToJob. This happens
   when the job list contains a lot less job objects than the process session ID.
   E.g. if the job object list contains no
    jobs (Count = 0) and a job with session ID 2 is to be assigned, the
    event method is called 3 times (session 0,1,2) and the process
    is assigned to the job object with index 2.  
  @param NewJobObject receives a valid instance of a job object.
    Must not be nil; otherwise AssignProcessToJob will fail. 
  }
  TJwOnNewJobObject = procedure (Sender : TJwJobObjectSessionList;
      ProcessHandle : TJwProcessHandle;
      ProcessSessionID,
      CurrentSessionID : Cardinal;
      var NewJobObject : TJwJobObject) of object;

  {<B>TJwJobObjectSessionList</B> manages a list of job objects threadsafe.
  Since every process in a job must be in the same session,
   the list manages one job object per session.

  <b>This solution is only available in Windows Vista and later.</b>

  }
  TJwJobObjectSessionList = class
  private
    fList : TList;
    fLock : TMultiReadExclusiveWriteSynchronizer;
    fOnNewJobObject: TJwOnNewJobObject;

    function GetJobObject(SessionIndex : Cardinal) : TJwJobObject; virtual;
    function GetProcessHandle(SessionIndex, ProcessIndex : Cardinal) : THandle; virtual;
    function GetCount : Cardinal; virtual;

    procedure SetTerminateJobsOnFree(Terminate : Boolean); virtual;
  public
    {<B>Create</B> creates a new instance of @Classname.
     @param NewJobObjectEvent defines an event that is called in
       AssignProcessToJob. It must not be nil; otherwise EJwsclNILParameterException
       will be raised 
      raises EJwsclNILParameterException will be raised if parameter NewJobObjectEvent
       is nil 
    }
    constructor Create(const NewJobObjectEvent: TJwOnNewJobObject);
    
    destructor Destroy; override;

    {<B>Clear</B> removes all job objects in the list.
     @param TerminateJob defines whether or not processes in the job objects
      are to be terminated.
      jtAll terminates all processes in all jobs.
      jtNone leaves all processes intact.
      Set jtSubjection to use TJwJobObject.TerminateOnDestroy instead.
    }
    procedure Clear(const TerminateJob : TJwJobTermination = jtSubjection); virtual;

    {<B>AssignProcessToJob</B> adds a process to a job using the process' session id to determine
    which job object must be used. If the job object with the neccessary session
    id is not available it will be created. For this task the event
    OnNewJobObject must be set. OnNewJobObject will be called for every
    job object that must be created. E.g. if the job object list contains no
    jobs (Count = 0) and a job with session ID 2 is to be assigned, the
    event OnNewJobObject is called 3 times (session 0,1,2) and the process
    is assigned to the job object with index 2.

    The assignment is threadsafe to avoid complication with other methods.

    @param Process defines a process handle to be added to an job object 
    raises
 EJwsclMissingEvent:  will be raised if event OnNewJobObject is nil 
     EJwsclInvalidParameterException: will be raised if the job object
      pointer returned by OnNewJobObject is nil 
     EJwsclWinCallFailedException: can be raised if the call to an winapi function failed. 
    }
    procedure AssignProcessToJob(Process : TJwProcessHandle; Data : Pointer); overload; virtual;

    {<B>AssignProcessToJob</B> adds a process to a job using the process' session id to determine
    which job object must be used. If the job object with the neccessary session
    id is not available it will be created. For this task the event
    OnNewJobObject must be set. OnNewJobObject will be called for every
    job object that must be created. E.g. if the job object list contains no
    jobs (Count = 0) and a job with session ID 2 is to be assigned, the
    event OnNewJobObject is called 3 times (session 0,1,2) and the process
    is assigned to the job object with index 2.

    The assignment is threadsafe to avoid complication with other methods.

    @param Process defines a process handle to be added to an job object 
    @param JobObjectIndex returns the session ID (or job object index) of the
      process wherein it was assigned to 
    raises
 EJwsclMissingEvent:  will be raised if event OnNewJobObject is nil 
     EJwsclInvalidParameterException: will be raised if the job object
      pointer returned by OnNewJobObject is nil 
     EJwsclWinCallFailedException: can be raised if the call to an winapi function failed. 
    }
    procedure AssignProcessToJob(Process : TJwProcessHandle; Data : Pointer; out JobObjectIndex : Cardinal); overload; virtual;

    {<B>JobObject[SessionIndex</B> returns a pointer to an internal job object.
    The assignment is threadsafe to avoid complication with other methods.
    }
    property JobObject[SessionIndex : Cardinal] : TJwJobObject read GetJobObject;

    {<B>ProcessIndex</B> returns a process handle of a specific job object.

    The assignment is threadsafe to avoid complication with other methods.

     The exception EJwsclInvalidIndex will be raised if parameter SessionIndex
     is out of range.
     The exception ERangeError will be raised if parameter ProcessIndex is
     out of range.
    }
    property ProcessHandle[SessionIndex, ProcessIndex : Cardinal] : THandle read GetProcessHandle;

    {<B>Count</B> returns the count of available job objects/sessions.}
    property Count : Cardinal read GetCount;

    {<B>TerminateJobsOnFree</B> defines whether all or no processes in all job objects
     should be terminated. This property has no read value because
     it just goes through all job objects and sets TJwJobObject.TerminateOnDestroy
     to the given value. 
    }
    property TerminateJobsOnFree : Boolean write SetTerminateJobsOnFree;

    {<B>OnNewJobObject</B> defines a callback event that is called every time a new job object
    must be generated. It is used in AssignProcessToJob.
    The event must not be nil otherwise the method fails.
    }
    property OnNewJobObject: TJwOnNewJobObject read fOnNewJobObject write fOnNewJobObject;

    {<B>Lock</B> returns the internal thread syncronisation object.
    It can be used to avoid problems with several threads.
    It should be used for property JobObject because calls to this property
     may become invalid during processing.
    <code lang="Delphi">
        Job.Lock.BeginWrite;
        try
          do sth with Job.JobObject[x]
        finally
          Job.Lock.EndWrite;
        end;
    </code>
    }
    property Lock : TMultiReadExclusiveWriteSynchronizer read fLock;
  end;

  {<B>TJwProcessOutputInformation</B> contains information about result from JwCreateProcessInSession.}
  TJwProcessOutputInformation = record
    {<B>UserToken</B> contains the token from the given Session ID.
     Free the token if it is no more necessary.   }
    UserToken  : TJwSecurityToken;
    {<B>ProcessInfo</B> receives information about the started process.  }
    ProcessInfo: TProcessInformation;
    {<B>EnvBlock</B> receives an environment block used the new process.
     Call DestroyEnvironmentBlock to free its memory. }
    EnvBlock : Pointer;
    {<B>ProfileInfo</B> contains the users profile.
     You must call TJwSecurityToken.UnLoadUserProfile to unload this profile
     after the process has ended. }
    ProfileInfo : TJwProfileInfo;
  end;


{<B>JwCreateProcessInSession</B> creates a new process in a user's session using various ways to
achieve success.
This procedure needs JwInitWellKnownSIDs to be called.

To run a process in another session the process needs SYSTEM rights. It means that
the current process token must have the TOKEN_ASSIGN_PRIMARY right for the target
token. Otherwise the CreateProcessAsUser function fails with bad error explanation
(like "A call to an OS function failed").
However the procedure won't stop you from doing this!

@param ApplicationName defines the application to be run in the session 
@param CommandLine defines the parameters for the application 
@param CurrentDirectory defines the start folder of the app.  
@param SessionID defines the target session where the new application is to be started. 
@param CreationFlags defines creation flags that are delivered to CreateProcess parameter with
 same name 
@param Desktop defines the target windowstation and desktop name. If empty
the default target is "winsta0\default" 
@param StartupInfo defines startup info delivered to to CreateProcess parameter with
 same name. Don't forget to initialize the structure first before calling this procedure.
<code lang="delphi>
ZeroMemory(@StartupInfo, sizeof(StartupInfo));
</code>
@param WaitForProcess defines whether the procedure should wait for the process to end
and clean up all allocated resources or just return to the caller. In last case
the caller is responsible to free the returned token, the environment block and
the users profile 
@param Output contains returned data in case parameter WaitForProcess is false.
The caller is responsible to free the contained member allocation 
@param LogServer receives a log server instance. It is used to log events for
mostly debugging purposes. If this parameter is nil, no events are logged   

raises
 EJwsclProcessIdNotAvailable:  will be raised if no token could be found
for the given SessionID 
 EJwsclNilPointer: will be raised if JwInitWellKnownSIDs was not called before
}
procedure JwCreateProcessInSession(
  const ApplicationName : TJwString;
  const CommandLine : TJwString;
  const CurrentDirectory : TJwString;

  const SessionID : DWORD;
  const CreationFlags : DWORD;
  const Desktop: TJwString;

  StartupInfo : {$IFDEF UNICODE}TStartupInfoW{$ELSE}TStartupInfoA{$ENDIF};

  WaitForProcess : Boolean;
  out Output : TJwProcessOutputInformation;
  LogServer : IJwLogServer
  );

{<B>JwGetTokenFromProcess</B> tries to retrieve a token from a process.
For this purpose it enumerates all processes on the local machine (even from
other users) and calls the callback method OnProcessFound to determine
whether the given process should be used to return the token.
@param OnProcessFound is a callback method that is called each time a process
was found. The callback function determines whether the process should be used
to return the token. If the process cannot be used to retrieve the token, <B>JwGetTokenFromProcess</B>
will continue enumerating 
@param LogServer receives a logging instance where log events are logged to.
 Can be nil if no logging is used
@param Data may contain user defined data to be assigned to a call to OnProcessFound 
@return <B>JwGetTokenFromProcess</B> returns the primary token of a process. If no process could be used
to get a token the return value is nil. 

raises
 EJwsclNILParameterException:  will be raised if parameter OnProcessFound is nil
}
function JwGetTokenFromProcess (const OnProcessFound : TJwOnProcessFound;
  LogServer : IJwLogServer; Data : Pointer) : TJwSecurityToken;



{<B>GetProcessSessionID</B> returns the session ID of a process given by handle or ID.
@param ProcessIDorHandle defines the process ID or handle. Which one is
  used, is defined by parameter ParameterType. Can be zero or -1 to use current process.
@param ParameterType defines whether parameter ProcessIDorHandle is a process
  or a handle
@return Returns the session ID (zero based).
raises
 EJwsclSecurityException:  <B>GetProcessSessionID</B> can raise a child class of this
  exception class. The following functions are used that can fail:

     # TJwSecurityToken.CreateTokenByProcess
     # TJwSecurityToken.CreateTokenByProcessId
     # TJwSecurityToken.TokenSessionId

}
function JwGetProcessSessionID(ProcessIDorHandle : TJwProcessId;
  const ParameterType : TJwProcessParameterType) : TJwSessionId;


{<B>JwProcessIdToSessionId</B> returns the session ID of a process.
This function uses the new API of Vista or otherwise calls just JwGetProcessSessionID.
It ist faster because it does not create an object if run on Vista or newer.

@param ProcessID defines the process ID. Can be zero or -1 to use current process.
@return Returns the session ID (zero based).
raises
 EJwsclWinCallFailedException: Is raised if ProcessIdToSessionId fails.
 EJwsclSecurityException: See JwGetProcessSessionID for more information.

remarks
 If you want to use the WinAPI function ProcessIdToSessionId and you also use JWSCL
 you should consider this function instead. It works on all Windows NT versions
 (>=2000).
}
function JwProcessIdToSessionId(const ProcessID : TJwProcessId) : TJwSessionId;



type {<B>TJwCreateProcessParameters</B> contains information supplied to CreateProcessAsUser}
     TJwCreateProcessParameters = record
        {<B>lpCommandLine</B> defines the application to be run with administrators group}
        lpApplicationName,
        {<B>lpCommandLine</B> defines the parameters for the application}
        lpCommandLine         :  TJwString;
        {<B>ThreadAttributes</B> defines the new process security descriptor}
        ProcessAttributes,
        {<B>ThreadAttributes</B> defines the new thread security descriptor}
        ThreadAttributes      : TJwSecurityDescriptor;

        {<B>bInheritHandles</B> defines the handle inheritance of the ProcessAttributes member}
        bProcInheritHandles,
        {<B>bInheritHandles</B> defines the handle inheritance of the ThreadAttributes member}
        bThreadInheritHandles,

        {<B>bInheritHandles</B> defines whether handles are inherited. Supplied to CreateProcess.}
        bInheritHandles       : Boolean;
        {<B>dwCreationFlags</B> defines creation flags that are delivered to CreateProcess parameter with
        same name}
        dwCreationFlags       : DWORD;
        {<B>lpCurrentDirectory</B> defines the start folder of the app.}
        lpCurrentDirectory    : TJwString;
      end;

     {<b>TJwCreateProcessInfo</b> contains extra information for JwCreateProcessAsAdminUser}
     TJwCreateProcessInfo = record
       {<B>StartupInfo</B> defines startup info delivered to to CreateProcess parameter with
       same name}
       StartupInfo : {$IFDEF UNICODE}TStartupInfoW{$ELSE}TStartupInfoA{$ENDIF};

       AdditionalGroups : TJwSecurityIdList; //zusätzliche Groups fürs Token


       {<B>SourceName</B> defines the source name which is stored in the token}
       SourceName : AnsiString;
       {<B>OriginName</B> defines the initiator name of the token}
       OriginName : AnsiString;

       {<B>SessionID</B> defines the target session ID of the new process
       if UseSessionID is true the new process will be spawn with this session ID.}
       SessionID  : Cardinal;
       {<B>UseSessionID</B> defines whether the new process should get the sessionID.}
       UseSessionID : Boolean;

       {<B>DefaultDesktop</B> If true the value StartupInfo.lpDesktop will be ignored and
        'WinSta0\Default' used instead}
       DefaultDesktop : Boolean;

       {Defines the logon process name for LSA connection.
        It must not exceed 127 characters. Only ansicode is supported.}
       LogonProcessName : AnsiString;

       //entweder LogonToken oder LogonSID oder keines!
       {<B>LogonToken</B> can contain the logon sid to be used for the new token.
        May be nil. In this case the member LogonSID is used.}
       LogonToken : TJwSecurityToken; //optionales logon Token für Tokengroups - LogonSid wird in diesem Token gesucht
       {<B>LogonSID</B> can be the logon sid to be used for the new token.
        May be nil. In this case (and LogonToken = nil) the logon sid of the token with the given SessionID
        is used.}
       LogonSID : TJwSecurityID; //optionales logon SID für Tokengroups - eigenes Token

       {<B>Parameters</B> contains parameters for CreateProcessAsUser}
       Parameters : TJwCreateProcessParameters; //Parameter für CP

       {<b>MaximumTryCount</b> defines the maximum try count for CreateProcessAsUser.
       The call is only repeated if CPAU returns ERROR_PIPE_BUSY which is a known
       bug in Windows XP.
       Set this value to INFINITE if CPAU should be called indefinitely on error ERROR_PIPE_BUSY.
       }
       MaximumTryCount : Integer;

       {<b>BusyPipeSleep</b> defines the sleep time before CPAU is called again.
       Values are possible between 100msec and 60 000msec.
       If the value is out of the bounds the default value of 5sec is used.}
       BusyPipeSleep : DWORD;
     end;

     {<B>TJwCreateProcessOut</B> contains output information after the process has started.
      Some of these information must be freed manually.}
     TJwCreateProcessOut = record
       {<B>ProcessInfo</B> contains process information of the new process}
       ProcessInfo: TProcessInformation;  //ProcessInfo von CP

       {<B>LinkedToken</B> receives the newly created token from the user logon.
        This token may be restricted in Vista. (administrator groups is for deny only
        access check)

        Free the instance manually (even when an exception has occured.)
       }
       UserToken,             //Normales Token (in Vista ohne Admin)
       {<B>LinkedToken</B> receives the twin token in Vista.
       This member is only valid if the user is member of administrator group.
       In this case the token has the administrator group enabled.
       In pre Vista OS versions this member is always nil.

       Free the instance manually (even when an exception has occured.)
       }
       LinkedToken : TJwSecurityToken; //elevated Token (in Vista), sonst nil

       {<B>IsLinked</B> flags whether a linked token is available (true) or not (false).}
       IsLinked : Boolean; //true, wenn Vista Elevation aktiv

       {<B>ProfInfo</B> receives the profile information from LoadUserProfile.
       Call TJwSecurityToken.UnloadUserProfile to unload the profile when
       process finished.
       }
       ProfInfo: TJwProfileInfo; //LoadUserProfile output -> UnloadUserProfile

       {<B>EnvironmentBlock</B> receives the users environment block.
        Call DestroyEnvironmentBlock to free the space.}
       EnvironmentBlock: Pointer;  //Environmentblock ->DestroyEnvBlock

       //LSALogonUser Input
       {<B>Source</B> receives the unique source ID generated by
        AllocateLocallyUniqueID}
       Source: TTokenSource; //Token source

       {<B>LSA</B> receives the LSA instance that was used to create the token.
        The instance must be freed if no exception occured.     }
       LSA : TJwSecurityLsa;  //LSA

       //Output von LsaLogonUser
       {<B>ProfBuffer</B> receives profile information about the logged on user.
       Call LsaFreeReturnBuffer to free it if no exception occured.
       }
       ProfBuffer: PMSV1_0_INTERACTIVE_PROFILE; //->   LsaFreeReturnBuffer(ProfBuffer);
       {<B>ProfBufferLen</B> receives the memory size of ProfBuffer}
       ProfBufferLen: Cardinal;

       {<B>TokenLuid</B> receives the LUID of the UserToken and LinkedToken.}
       TokenLuid: TLUID;  //LUID des neuen Tokens (UserToken + LinkedToken)

       {<B>QuotaLimits</B> receives information about quota limits}
       QuotaLimits: QUOTA_LIMITS;
       {<B>SubStatus</B> receives extended error information from LsaLogonUser}
       SubStatus: NTSTATUS; //Fehler von LSALogonUser
     end;

{<B>JwCreateProcessAsAdminUser</B> logs on a user and creates a new process under its logon session.
The user will be a member of administrators group for this process but not
in the user database.
In Vista the linked token will be retrieved which has the administrator group enabled.
The user does not have to be an administrator at all!

Remarks
 This procedure needs JwInitWellKnownSIDs to be called.
<B>JwCreateProcessAsAdminUser</B> can only run within a SYSTEM account and with TCB privilege available.

<B>BETA: This function has not been tested thoroughly!</B>  

@param LogServer receives a logging instance where log events are logged to.
 Can be nil if no logging is used 
raises
 EJwsclNilPointer:  will be raised if JwInitWellKnownSIDs was not called before 
 EJwsclPrivilegeException: will be raised if the TCB privilege is not available 
}

procedure JwCreateProcessAsAdminUser(
   const UserName, Domain, Password : TJwString;
   const InVars : TJwCreateProcessInfo;
   out OutVars : TJwCreateProcessOut;
   LogServer : IJwLogServer
   );





{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation
uses Math, D5impl, JwsclExceptions,
  JwsclKnownSid, JwsclVersion,
  JwsclAcl, JwsclConstants;

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}

function JwProcessIdToSessionId(const ProcessID : TJwProcessId) : TJwSessionId;
begin
  if TJwWindowsVersion.IsWindowsVista(true) and
     TJwWindowsVersion.IsWindows2008(true) then
  begin
    if ProcessIdToSessionId(ProcessID, result) then
      raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'JwProcessIdToSessionId',                                //sSourceProc
          '',                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'ProcessIdToSessionId',                   //sWinCall
          ['ProcessIdToSessionId']);                                  //const Args: array of const
  end
  else
    result := JwGetProcessSessionID(ProcessID, pptID);
end;


function JwGetProcessSessionID(ProcessIDorHandle : TJwProcessId;
  const ParameterType : TJwProcessParameterType) : TJwSessionId;
var Token : TJwSecurityToken;
begin
  Token := nil;
  result := WTS_CURRENT_SESSION;

  //make sure we can access 0 or -1
  if (ProcessIDorHandle = 0) or
     (ProcessIDorHandle = GetCurrentProcessId) then
  case ParameterType of
    pptHandle : ProcessIDorHandle := 0;
    pptID     : ProcessIDorHandle := GetCurrentProcessId;
  end;

  case ParameterType of
    pptHandle : Token := TJwSecurityToken.CreateTokenByProcess(ProcessIDorHandle, TOKEN_READ or TOKEN_QUERY);
    pptID     : Token := TJwSecurityToken.CreateTokenByProcessId(ProcessIDorHandle, TOKEN_READ or TOKEN_QUERY);
  end;

  if Assigned(Token) then
  try
    result := Token.TokenSessionId;
  finally
    Token.Free;
  end;
end;


class function TJwLibraryUtilities.LoadLibProc(const LibName: AnsiString; const ProcName: AnsiString): Pointer;
var
  R : Pointer;
begin
  R := nil;
  //problem with direct use of Result in this procedure!
  GetProcedureAddress(R, LibName, ProcName);
  Result := R;

 { LibHandle := LoadLibraryA(PAnsiChar(LibName));
  if LibHandle = 0 then
    Result := nil
  else
  begin
    try
      Result := GetProcAddress(LibHandle, PAnsiChar(ProcName));
    finally
      FreeLibrary(LibHandle); // Free Memory Allocated for the DLL
    end;
  end;}
end;




procedure JwCreateProcessAsAdminUser(
   const UserName, Domain, Password : TJwString;
   const InVars : TJwCreateProcessInfo;
   out OutVars : TJwCreateProcessOut;
   LogServer : IJwLogServer);



var pLogonData : PMSV1_0_INTERACTIVE_LOGON;
    Authlen: Cardinal;
    Groups : TJwSecurityIDList;
    LogonSessionSID,
    SID : TJwSecurityID;
    SessionLogonToken : TJwSecurityToken;

    UserToken : TJwSecurityToken;
    StartupInfo : {$IFDEF UNICODE}TStartupInfoW{$ELSE}TStartupInfoA{$ENDIF};
    LastError : DWORD;
//
    CurrentDirectory,
    AppName, CmdLine : TJwPchar;

    Log : IJwLogClient;

    lpProcAttr, lpThreadAttr :  PSecurityAttributes;

    i : Integer;

    CPAUResult : Boolean;
begin
  JwRaiseOnNilMemoryBlock(JwLocalSystemSID,'JwCreateProcessAsAdminUser','','JwsclProcess.pas');

  //log to /dev/null
  if not Assigned(LogServer) then
    LogServer := CreateLogServer(nil, nil);

  Log := LogServer.Connect(etFunction, '', 'JwCreateProcessAsAdminUser', 'JwsclProcess.pas','');

  {Safety first.
   We clear the output buffer to avoid freeing stuff
   that was never initialised.}
  ZeroMemory(@OutVars, sizeof(OutVars));

  try //1.
    Log.Log(lsMessage, 'Try to enable TCB privilege...');
    JwEnablePrivilege(SE_TCB_NAME,pst_Enable);
    Log.Log(lsMessage, 'Success');


    Log.Log('Calling TJwSecurityLsa.Create');
    OutVars.LSA := TJwSecurityLsa.Create(InVars.LogonProcessName);
    Log.Log('Success');

    ZeroMemory(@OutVars.Source.SourceName, 0);
    StrLCopy(@OutVars.Source.SourceName,PAnsiChar(InVars.SourceName),
                Min(sizeof(OutVars.Source.SourceName), Length(InVars.SourceName)));
    AllocateLocallyUniqueID(OutVars.Source.SourceIdentifier);

    SessionLogonToken := nil;
    //
    // Init LSALogonUser parameters
    //
    pLogonData := JwCreate_MSV1_0_INTERACTIVE_LOGON(
                      MsV1_0InteractiveLogon, Domain, UserName, Password, Authlen);

    Groups := TJwSecurityIDList.Create(True);
    try //2.

      //
      // Add logon sid from caller if any.
      //
      if Assigned(InVars.LogonSID) then
      begin
        Log.Log('Adding user defined LogonSessionID to TokenGroups');
        SID := TJwSecurityId.Create(InVars.LogonSID);
        SID.Attributes := SE_GROUP_MANDATORY or
                          SE_GROUP_ENABLED or
                          SE_GROUP_ENABLED_BY_DEFAULT or
                          SE_GROUP_LOGON_ID;
        Groups.Add(SID); //"Groups" owns SID now
      end
      else
      // ...otherwise
      // Either use the callers token from InVars.LogonToken
      // or get the token from a specified SessionID. 
      //
      begin
        //
        // The user did not provide use with a token
        // so we must get the logon sid ourselves.
        //
        if not Assigned(InVars.LogonToken) then 
        try //3.
          Log.Log('Checking Windows Version...');

          //
          // Use a newer way to get the session token
          //
          if (TJwWindowsVersion.IsWindowsXP(true) or
             TJwWindowsVersion.IsWindows2003(true) or
             TJwWindowsVersion.IsWindows2003R2(true) or
             TJwWindowsVersion.IsWindowsVista(true) or
             TJwWindowsVersion.IsWindows2008(true)) then
          begin
            Log.Log('Getting user token from session: '+IntToStr(InVars.SessionID));

            {
            WARNING:
              TCB priv must be available and process must run under SYSTEM account!
            }
            try
              SessionLogonToken := TJwSecurityToken.CreateWTSQueryUserToken(InVars.SessionID);
            except
              {
              Do the old way: if we cannot get a connection
              and the session is should be the current session ID.
              This way the code can run in a none service app if the
               session id remains the current one.
              }
              on E : EJwsclWinCallFailedException do
                if (E.LastError = ERROR_ACCESS_DENIED)
                  and ((InVars.SessionID = INVALID_HANDLE_VALUE) or
                       (InVars.SessionID = WTSGetActiveConsoleSessionId))
                   then
                  SessionLogonToken := TJwSecurityToken.CreateCompatibilityQueryUserToken(TOKEN_READ or TOKEN_QUERY or TOKEN_DUPLICATE)
                else
                  raise;
            end;
          end
          else
          //
          // On Windows 2000 and earlier we use an old fashioned way
          //
          begin
            Log.Log('Getting user token from session which has explorer.exe: ');
            //TODO: use same mechanism as JwCreateProcessInSession to not rely on explorer.exe
            SessionLogonToken := TJwSecurityToken.CreateCompatibilityQueryUserToken(TOKEN_READ or TOKEN_QUERY or TOKEN_DUPLICATE);
          end;
        except //3.
          on E : Exception do
          begin
            SessionLogonToken := nil;
            if InVars.UseSessionID then
            begin
              Log.Exception(E);

              Log.Log(lsError,'Could not retrieve LogonSID ('+IntToStr(InVars.SessionID) + '): ');
              raise EJwsclNoSuchLogonSession.CreateFmt('Could not retrieve a LogonSID %d: ',[InVars.SessionID]);
            end
            else
              Log.Log('Failed to get LogonSID. Omitting step.');
          end;
        end; //3.
      end;

      //
      // The user did not specify a logon SID
      // but she provided a token OR
      //  we got our own token
      //
      if not Assigned(InVars.LogonSID) and
         (Assigned(SessionLogonToken) or Assigned(InVars.LogonToken)) then
      begin
        //
        // replace SessionLogonToken by the users one
        //
        if Assigned(InVars.LogonToken) then
        begin
          FreeAndNil(SessionLogonToken);
          SessionLogonToken := InVars.LogonToken;
        end;

        try
          //
          // Parse the logon sid from that token
          //
          Log.Log('Getting LogonSID...');
          try
            LogonSessionSID := JwGetLogonSID(SessionLogonToken);
          except
            on E : Exception do
            begin
              Log.Exception(E);
              LogonSessionSID := nil;
              Log.Log(lsError,'...call to JwGetLogonSID failed: ');
            end;
          end;

          //
          // add the logon sid from that token to the new token
          //
          if LogonSessionSID <> nil then
          begin
            Log.Log('Adding LogonSessionID to TokenGroups');
            SID := LogonSessionSID;
            SID.Attributes:= SE_GROUP_MANDATORY or
                             SE_GROUP_ENABLED or
                             SE_GROUP_ENABLED_BY_DEFAULT or
                             SE_GROUP_LOGON_ID;
            Groups.Add(SID); //"Groups" owns SID now
          end;
        finally
          if not Assigned(InVars.LogonToken) then
            FreeAndNil(SessionLogonToken);
        end;
      end;

      //
      // Add administrator group and enable it by default
      //
      Log.Log('Adding Administrator group to new token groups.');
      SID := TJwSecurityID.Create(JwAdministratorsSID);
      SID.Attributes:= SE_GROUP_MANDATORY or
                        SE_GROUP_ENABLED or
                        SE_GROUP_ENABLED_BY_DEFAULT;
      Groups.Add(SID); //"Groups" owns SID now

      //
      // add user groups to that token
      //
      if Assigned(InVars.AdditionalGroups) then
      begin
        for i := 0 to InVars.AdditionalGroups.Count -1 do
        begin
          Sid := TJwSecurityId.Create(InVars.AdditionalGroups[i]);

          //
          // we do not want a foreign logon ID to be in it 
          //
          if (sidaGroupLogonId in Sid.AttributesType) then
            Sid.AttributesType := Sid.AttributesType - [sidaGroupLogonId];

          Groups.Add(Sid);
        end;
      end;

      //
      // Logon user with special adapted token information
      //
      Log.Log('Calling LsaLogonUser...');
      try //4.
        OutVars.LSA.LsaLogonUser(InVars.OriginName, JwaWindows.Interactive, MSV1_0_PACKAGE_NAME,
                              pLogonData, Authlen, Groups, OutVars.Source,
                           {out...}
                              Pointer(OutVars.ProfBuffer), OutVars.ProfBufferLen,
                              OutVars.TokenLuid, OutVars.UserToken, OutVars.QuotaLimits, OutVars.SubStatus);
      except //4.
        on E: Exception do
        begin
          FreeAndNil(Groups);

          Log.Exception(E);
          Log.Log(lsError,'LsaLogonUser failed');
          raise;
        end;
      end; //4.

      Log.Log('...successfully.');
      FreeAndNil(Groups);

      //
      // On Vista or server 2008 LsaLogonUser creates a duplicate token
      // if it encounters the administrator group.
      // So we have to use the twin token for the new process
      // which we want to start with administrator group.
      //
      try
        OutVars.IsLinked := true;
        OutVars.LinkedToken := OutVars.UserToken.LinkedToken;
        UserToken := OutVars.LinkedToken; //token for CreateProcessAsUser
      except
        on E : Exception do
        begin
          Log.Exception(E);
          Log.Log(lsError,'Could not get Linked Token. Using token from LsaLogonUser...continue');

          OutVars.IsLinked := false;
          OutVars.LinkedToken := nil;
          UserToken := OutVars.UserToken;
        end;
      end;

      StartupInfo := InVars.StartupInfo;
      StartupInfo.cb := SizeOf(StartupInfo);

      if InVars.DefaultDesktop then
        StartupInfo.lpDesktop := 'WinSta0\Default';


      //
      // If the caller wants to set the tokens SessionID
      // we do it here.
      // The token needs the correct session ID to be set.
      //
      if InVars.UseSessionID then
      begin
        Log.Log('Setting TokenSession ID...');
        try
          UserToken.TokenSessionId := InVars.SessionID;//WtsGetActiveConsoleSessionID;
        except
          on E : Exception do
          begin
            Log.Exception(E);
            Log.Log(lsError,'...Failed to set TokenSessionId. ');
            raise;
          end;
        end;
        Log.Log('...successfully.');
      end;

      //
      // Load user profile
      //  may throw exception
      //
      Log.Log('Try to load user profile...');
      UserToken.LoadUserProfile(OutVars.ProfInfo, [{automatic config}]);
      Log.Log('Profile loaded');


      //
      // Create environment variables for the user
      //
      if not CreateEnvironmentBlock(@OutVars.EnvironmentBlock, UserToken.TokenHandle, true) then
        Log.Log(lsWarning,'Call to CreateEnvironmentBlock failed: '+EJwsclSecurityException.GetLastErrorMessage());

      try //6.
        AppName := nil;
        CmdLine := nil;
        CurrentDirectory := nil;
        lpProcAttr := nil;
        lpThreadAttr := nil;

        //
        // Copy variables to memory for CreateProcessAsUserX 
        //  CreateProcessAsUser behaves different for
        //    nil and empty strings. 

        if Length(InVars.Parameters.lpApplicationName) > 0 then
          AppName := TJwPChar(InVars.Parameters.lpApplicationName);

        if Length(InVars.Parameters.lpCommandLine) > 0 then
          CmdLine := TJwPChar(InVars.Parameters.lpCommandLine);

        if Length(InVars.Parameters.lpCurrentDirectory) > 0 then
          CurrentDirectory := TJwPChar(InVars.Parameters.lpCurrentDirectory);

        if Assigned(InVars.Parameters.ProcessAttributes) then
          lpProcAttr := InVars.Parameters.ProcessAttributes.Create_SA(InVars.Parameters.bProcInheritHandles);

        if Assigned(InVars.Parameters.ThreadAttributes) then
          lpThreadAttr := InVars.Parameters.ThreadAttributes.Create_SA(InVars.Parameters.bThreadInheritHandles);

        //
        // Create new process
        //
        Log.Log('Calling CreateProcessAsUser...');


        i := 1; //first try
        repeat
          SetLastError(0);
          CPAUResult := {$IFDEF UNICODE}CreateProcessAsUserW{$ELSE}CreateProcessAsUserA{$ENDIF}(
              UserToken.TokenHandle,//HANDLE hToken,
              AppName,//__in_opt     LPCTSTR lpApplicationName,
              CmdLine, //__inout_opt  LPTSTR lpCommandLine,
              LPSECURITY_ATTRIBUTES(lpProcAttr),//__in_opt     LPSECURITY_ATTRIBUTES lpProcessAttributes,
              LPSECURITY_ATTRIBUTES(lpThreadAttr),//LPSECURITY_ATTRIBUTES(InVars.Parameters.lpThreadAttributes),//__in_opt     LPSECURITY_ATTRIBUTES lpThreadAttributes,
              InVars.Parameters.bInheritHandles,//__in         BOOL bInheritHandles,
              InVars.Parameters.dwCreationFlags,//__in         DWORD dwCreationFlags,
              OutVars.EnvironmentBlock,//__in_opt     LPVOID lpEnvironment,
              CurrentDirectory,//'',//TJwPChar(InVars.Parameters.lpCurrentDirectory),//__in_opt     LPCTSTR lpCurrentDirectory,
              StartupInfo,//__in         LPSTARTUPINFO lpStartupInfo,
              OutVars.ProcessInfo //__out        LPPROCESS_INFORMATION lpProcessInformation
             );

          {Check for a known XP bug
          The pipe is not available at some time
          so wait for it
          }
          if GetLastError() = ERROR_PIPE_NOT_CONNECTED then
          begin
            if (InVars.MaximumTryCount >= 0) and (i >= InVars.MaximumTryCount) then
              Break;

            if (InVars.BusyPipeSleep < 100) or (InVars.BusyPipeSleep > 60* 100) then
               Sleep(InVars.BusyPipeSleep)
            else
               Sleep(5* 1000); //default 5sec

            Inc(i);
          end;
        until (GetLastError() <> ERROR_PIPE_NOT_CONNECTED);
        
        if not CPAUResult then
        begin
          Log.Log('Call to CreateProcessAsUser succeeded. Returning.');
        end
        else
        begin
          LastError := GetLastError();
          Log.Log(lsError,'CreateProcessAsUser failed: '+ EJwsclSecurityException.GetLastErrorMessage(LastError));

          raise EJwsclCreateProcessFailed.CreateFmtEx(
             'CreateProcessAsUser failed.',
             'CreateProcessAsAdminUser', '', '0',
             0, True, ['CreateProcessAsUser']);
        end;
      except //6.
        on E : Exception do
        begin
          Log.Log('Clean up after CreateProcessAsUser failure....');

          if lpProcAttr <> nil then
            TJwSecurityDescriptor.Free_SA(lpProcAttr);

          if lpThreadAttr <> nil then
            TJwSecurityDescriptor.Free_SA(lpThreadAttr);

          DestroyEnvironmentBlock(OutVars.EnvironmentBlock);
            OutVars.EnvironmentBlock := nil;

          OutVars.UserToken.UnloadUserProfile(OutVars.Profinfo);

          FreeAndNil(OutVars.LinkedToken);
          FreeAndNil(OutVars.UserToken);
          //UserToken := nil;

          Log.Log('...sucessfully.');
          raise;
        end;
      end; //6.

    except //2.
      LocalFree(HLOCAL(pLogonData));
      //pLogonData := nil;

      LsaFreeReturnBuffer(OutVars.ProfBuffer);
      OutVars.ProfBuffer := nil;
      OutVars.ProfBufferLen := 0;

      raise;
    end; //2.

    //
    //vars that must be freed
    //
    if pLogonData <> nil then
      LocalFree(HLOCAL(pLogonData));
  except //1.
    on E : Exception do
    begin
      Log.Exception(E);

      FreeAndNil(OutVars.LSA);

      raise;
    end;
  end; //1.
end;








type
  PInternalProcessData = ^TInternalProcessData;
  TInternalProcessData = record
    SessionID : DWORD;
  end;

procedure OnProcessFoundInSameSession(const Sender1, Sender2 : TJwTerminalServer; var Process : TJwWTSProcess;
      var Cancel : Boolean; Data : Pointer);
var ProcessData : PInternalProcessData absolute Data;
begin
  try
    Cancel := Assigned(Process) and
        {
          same session
        }
        (Process.SessionId = ProcessData.SessionID)

        {
          no idle process
        }
        and (Process.ProcessId > 0)
        {
          We have to check for system processes in that session.
          So we ignore them otherwise the user gets a system elevated process.
        }
        and (Assigned(Process.UserSid) and not Process.UserSid.EqualSid(JwLocalSystemSID));
  except
    on E : Exception do
    begin
      //don't bother about the problem
      Cancel := false;
    end;
  end;      
end;



function JwGetTokenFromProcess (const OnProcessFound : TJwOnProcessFound; LogServer : IJwLogServer; Data : Pointer) : TJwSecurityToken;


  function InternalEnumerate(const Srv : TJwTerminalServer) : DWORD;
  var
    hSnap : THandle;
    ProcEntry : TProcessEntry32W;
    Continue : Boolean;

    SrvProcess : TJwWTSProcess;

    ProcToken : TJwSecurityToken;

  begin
    hSnap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    result := 0;

    try
      ZeroMemory(@ProcEntry, sizeof(ProcEntry));
      ProcEntry.dwSize := sizeof(ProcEntry);
      Continue := Process32FirstW(hSnap, ProcEntry);

      while (continue) do
      begin
        try
          SrvProcess := nil;

          //ignore the system process with id 0
          if ProcEntry.th32ProcessID > 0 then
          begin
            ProcToken := TJwSecurityToken.CreateTokenByProcessId(ProcEntry.th32ProcessID, TOKEN_READ or TOKEN_QUERY);
            try
              SrvProcess := TJwWTSProcess.Create(Srv.Processes, ProcToken.TokenSessionId,
                ProcEntry.th32ProcessID, TJwString(ProcEntry.szExeFile), ProcToken.UserName);
            finally
              ProcToken.Free;
            end;
          end;
        except
          SrvProcess := nil;
        end;

        if not Assigned(SrvProcess) then
          SrvProcess := TJwWTSProcess.Create(Srv.Processes, WTS_CURRENT_SESSION,
            ProcEntry.th32ProcessID, TJwString(ProcEntry.szExeFile), '');
                          
        Inc(result);
        Srv.Processes.Add(SrvProcess);

        continue := Process32NextW(hSnap,ProcEntry);
      end;
    finally
      CloseHandle(hSnap);
    end;
  end;




var TSrv : TJwTerminalServer;
    i : Integer;
    ProcessID : DWORD;
    Succ : Boolean;
    //Sid : TJwSecurityId;
    Cancel : Boolean;
    Process : TJwWTSProcess;
    Log : IJwLogClient;
begin
  JwRaiseOnNilParameter(@OnProcessFound,'OnProcessFound','JwGetTokenFromProcess','','JwsclProcess.pas');

  //log to /dev/null
  if not Assigned(LogServer) then
    LogServer := CreateLogServer(nil, nil);

  Log := LogServer.Connect(etFunction, '', 'JwGetTokenFromProcess123', 'JwsclProcess.pas','');

  result := nil;
  Succ := false;

  //try to enable debug privs if available - otherwise nothing
  JwEnablePrivilege(SE_DEBUG_NAME,pst_EnableIfAvail);

  Log.Log(lsMessage,Format('Running CreateTokenByProcessAndSession(SessionID: %d',[0]));

  TSrv := TJwTerminalServer.Create;
  try
    TSrv.Connect;

    ProcessID := 0;

    {If the following function fails,
     we will use a compatible version to enumerate
     processes.
    }
    if not TSrv.EnumerateProcesses then
    begin
      Log.Log(lsWarning, 'TJwTerminalServer.EnumerateProcesses failed. Falling back into compatible mode.');
      InternalEnumerate(TSrv);
    end;

    if TSrv.Processes.Count > 0 then
    begin
      Log.Log(lsMessage, 'Proc count: ' + IntToStr(TSrv.Processes.Count));
      for i := 0 to TSrv.Processes.Count-1 do
      begin
        Log.Log(lsMessage, Format('Proc: %d, Name= %s SessionID: %d',[TSrv.Processes[i].ProcessId,
          TSrv.Processes[i].ProcessName, TSrv.Processes[i].SessionId]));


        Cancel := true;
        Process := TSrv.Processes[i];
        OnProcessFound(TSrv, Process, Cancel, Data);
        if Cancel then
        begin
          //found a process in that specified session
          ProcessID := TSrv.Processes[i].ProcessId;

          try
            Succ := true;
            
            Log.Log(lsMessage,'call CreateDuplicateExistingToken');
            {
              Get token by process handle and duplicate it
              
              TSrv.Processes[i].Token may be nil if the token could not be retrieved.
              That may happen for processes in other session or constrained tokens (adapted DACL).
              We skip it into the except branch!
            }            
            result := TJwSecurityToken.CreateDuplicateExistingToken(TSrv.Processes[i].Token.TokenHandle,
                TOKEN_ASSIGN_PRIMARY or
                TOKEN_QUERY or TOKEN_IMPERSONATE or TOKEN_DUPLICATE or TOKEN_READ);

            
          except
            On E : Exception do
            begin
              Log.Log(lsWarning, 'CreateDuplicateExistingToken failed: '#13#10+E.Message);

              //try to get the token the old fashioned way
              try
                Succ := true;

                result := TJwSecurityToken.CreateTokenByProcessId(ProcessID,
                //these are the only necessary rights
                  TOKEN_ASSIGN_PRIMARY or
                  TOKEN_QUERY or TOKEN_IMPERSONATE or TOKEN_DUPLICATE or TOKEN_READ);

                //  MAXIMUM_ALLOWED); CreateTokenByProcessId copies the token handle so we always get full access

              except
                On E : Exception do
                begin
                  Log.Exception(E);
                  //Log.Add('Could not get user token by Process: '#13#10+E.Message);
                  Succ := False;
                  ProcessID := 0;
                end;
              end;
            end;
          end;

          //quit loop if success and process is found
          if Succ and (ProcessID > 0) then
            break;
        end;
      end
    end
    else
    begin
      raise EJwsclEnumerateProcessFailed.CreateFmtWinCall(
        RsEnumerateProcessesFailed,
        'JwGetTokenFromProcess',                                //sSourceProc
        '',                                //sSourceClass
        RsUNProcess,                          //sSourceFile
        0,                                           //iSourceLine
        True,                                  //bShowLastError
        'TJwTerminalServer.EnumerateProcesses',                   //sWinCall
        []);                                  //const Args: array of const
      Log.Log(lsMessage,'EnumerateProcesses failed.');
    end;

    if ProcessID = 0 then
      Log.Log(lsMessage,'Could not find any process ID.');         
  finally
    TSrv.Free;
//    Log.Log(lsMessage,'Exiting CreateTokenByProcessAndSession.');
  end;
end;


procedure JwCreateProcessInSession(
  const ApplicationName : TJwString;
  const CommandLine : TJwString;
  const CurrentDirectory : TJwString;

  const SessionID : DWORD;
  const CreationFlags : DWORD;
  const Desktop: TJwString;

  StartupInfo : {$IFDEF UNICODE}TStartupInfoW{$ELSE}TStartupInfoA{$ENDIF};

  WaitForProcess : Boolean;
  out Output : TJwProcessOutputInformation;
  LogServer : IJwLogServer
  );


  procedure GetPChar(const Str : TJwString; var CharPtr : TJwPChar);
  begin
    if Length(Str) > 0 then
      CharPtr := TJwPChar(Str)
    else
      CharPtr := nil;
  end;

  function CreateTokenByProcessAndSession(
    const SessionID : DWORD) : TJwSecurityToken;
  var TSrv : TJwTerminalServer;
      //i : Integer;
      //Succ : Boolean;
      //Sid : TJwSecurityId;
      Meth : TMethod;
      Data : TInternalProcessData;
      Log : IJwLogClient;
  begin
    Log := LogServer.Connect(etFunction, '', 'CreateTokenByProcessAndSession', 'JwsclProcess.pas','');

    //try to enable debug privs if available - otherwise nothing
    JwEnablePrivilege(SE_DEBUG_NAME,pst_EnableIfAvail);

    Log.Log(lsMessage,Format('Running CreateTokenByProcessAndSession(SessionID: %d',[SessionID]));

    TSrv := TJwTerminalServer.Create;
    try
      TSrv.Connect;

      //convert procedure into method
      Meth.Code := @OnProcessFoundInSameSession;
      Meth.Data := nil; //Self pointer

      Data.SessionID := SessionID;
      // enumerate all processes of the terminal server
      // we also get many
      //
      result := JwGetTokenFromProcess (TJwOnProcessFound(Meth), LogServer, @Data);
    finally
      TSrv.Free;
      Log.Log(lsMessage,'Exiting CreateTokenByProcessAndSession.');
    end;
  end;

  procedure CheckStartupInfo;
  begin
    if (StartupInfo.cb <> sizeof(StartupInfo)) and
       (StartupInfo.cb <> 0) then
       raise EJwsclInvalidStartupInfo.CreateFmtEx(
      RsInvalidStartupInfo,
      'JwCreateProcessInSession', '', RsUNProcess, 0, false, []);
  end;


var
    lpApplicationName  : TJwPChar;
    lpCommandLine      : TJwPChar;
    lpCurrentDirectory : TJwPChar;

    Log : IJwLogClient;
begin
  JwCheckInitKnownSid([JwLocalSystemSID], ['JwLocalSystemSID'],
    'JwCreateProcessInSession','',RsUNProcess);

  CheckStartupInfo;


  //log to /dev/null
  if not Assigned(LogServer) then
    LogServer := CreateLogServer(nil, nil);

  Log := LogServer.Connect(etFunction, '', 'JwCreateProcessInSession', 'JwsclProcess.pas','');



  Output.UserToken := nil;
  ZeroMemory(@Output.ProcessInfo, sizeof(Output.ProcessInfo));
  Output.EnvBlock := nil;
  ZeroMemory(@Output.ProfileInfo, sizeof(ProfileInfo));

  //First try WTS call to get token
  try
    Log.Log(lsMessage, Format('Running CreateProcessInSession(Sesion=%d):',[SessionID]));
    try
      Log.Log(lsMessage,'Getting user token CreateWTSQueryUserTokenEx...');
      Output.UserToken := TJwSecurityToken.CreateWTSQueryUserTokenEx(nil, SessionID);
    except
      On E2 : Exception do
      begin
        //as second chance we try to get the token from a process
        try
          Log.Log(lsMessage,'Getting user token CreateTokenByProcessAndSession...');
          Output.UserToken := CreateTokenByProcessAndSession(SessionId);
        except
          on E : Exception do
          begin
            Log.Exception(E); 

            raise;
          end;
        end;
      end;
    end;

    if not Assigned(Output.UserToken) then
    begin
      try
        raise EJwsclProcessIdNotAvailable.Create('There were no token found for specified session: '+IntToStr(SessionId));
      except
        on E : Exception do
        begin
          //Log.Log(lsMessage,E.Message);
          Log.Exception(E);
          raise;
        end;
      end;

      exit;
    end;


    try
      Log.Log(lsMessage, 'Loading user profile');
      // Load user's profile
      // We do not apply any in parameters, let the method do it automatically
      // may fail with an exception
      try
       Output.UserToken.LoadUserProfile(Output.ProfileInfo, []);
      except
        Output.ProfileInfo.Profile := 0;
      end;

      with StartupInfo do
      begin
        cb          := SizeOf(StartupInfo);
        //set default desktop if caller does not specify it
        if Length(Desktop) = 0 then
          lpDesktop   := 'WinSta0\Default'
        else
          lpDesktop   := TJwPChar(Desktop);
      end;


      Log.Log(lsMessage,'Init strings');
      //get p(w)char pointer from string
      GetPChar(ApplicationName, lpApplicationName);
      GetPChar(CommandLine, lpCommandLine);
      GetPChar(CurrentDirectory, lpCurrentDirectory);


      Log.Log(lsMessage, 'Init env block');
      //Create environment block from user token
      //we do fail on that
      if not CreateEnvironmentBlock(@Output.EnvBlock, Output.UserToken.TokenHandle, false) then
        Log.Log(lsMessage, 'CreateEnvironmentBlock failed: '+IntToStr(GetLastError));

      Log.Log(lsMessage, 'Call CreateProcessAsUser');
      if not {$IFDEF UNICODE}CreateProcessAsUserW{$ELSE}CreateProcessAsUserA{$ENDIF}(
        Output.UserToken.TokenHandle,//HANDLE hToken,
        lpApplicationName,//__in_opt     LPCTSTR lpApplicationName,
        lpCommandLine, //__inout_opt  LPTSTR lpCommandLine,
        nil,//__in_opt     LPSECURITY_ATTRIBUTES lpProcessAttributes,
        nil,//__in_opt     LPSECURITY_ATTRIBUTES lpThreadAttributes,
        false,//__in         BOOL bInheritHandles,
        CreationFlags or CREATE_UNICODE_ENVIRONMENT,//__in         DWORD dwCreationFlags,
        Output.EnvBlock,//__in_opt     LPVOID lpEnvironment,
        lpCurrentDirectory,//__in_opt     LPCTSTR lpCurrentDirectory,
        StartupInfo,//__in         LPSTARTUPINFO lpStartupInfo,
        Output.ProcessInfo //__out        LPPROCESS_INFORMATION lpProcessInformation
      ) then
      begin
        Log.Log(lsMessage,'Failed CreateProcessAsUser.');
        RaiseLastOSError;
      end;

      if WaitForProcess then
      begin
        Log.Signal(stWait,'','','Waiting for process to finish.');
        WaitForSingleObject(Output.ProcessInfo.hProcess, INFINITE);
        Log.Signal(stReceived,'','','Process finished.');

        DestroyEnvironmentBlock(Output.EnvBlock);
        Output.EnvBlock := nil;
        Output.UserToken.UnLoadUserProfile(Output.ProfileInfo);
        FreeAndNil(Output.UserToken);
      end;

    except
      on E : Exception do
      begin
        DestroyEnvironmentBlock(Output.EnvBlock);
        Output.EnvBlock := nil;
        Output.UserToken.UnLoadUserProfile(Output.ProfileInfo);
        FreeAndNil(Output.UserToken);

        Log.Log(lsMessage,'Exception (between LoadUserProfile and CreateProcessAsUser) : '#13#10+E.Message);
        raise;
      end;
    end;

  except
    //make sure log is filled with exception data
    on E : Exception do
    begin
     // LogInfo := LogInfo + #13#10 + Log.Text;

      {if E is EJwsclSecurityException then
        (E as EJwsclSecurityException).Log := LogInfo;
      Log.Free;}
      Log.Exception(E);

      raise;
    end;
  end;

end;

{

BOOL WINAPI AssignProcessToJobObject(HANDLE hJob, HANDLE hProcess);
HANDLE WINAPI CreateJobObject(LPSECURITY_ATTRIBUTES lpJobAttributes,LPCTSTR lpName);
HANDLE WINAPI OpenJobObject(DWORD dwDesiredAccess,BOOL bInheritHandles, LPCTSTR lpName);
BOOL WINAPI SetInformationJobObject(HANDLE hJob,JOBOBJECTINFOCLASS JobObjectInfoClass,
        LPVOID lpJobObjectInfo,DWORD cbJobObjectInfoLength);
BOOL WINAPI QueryInformationJobObject(HANDLE hJob, JOBOBJECTINFOCLASS JobObjectInfoClass,
        LPVOID lpJobObjectInfo, DWORD cbJobObjectInfoLength, LPDWORD lpReturnLength);
BOOL WINAPI IsProcessInJob(HANDLE ProcessHandle, HANDLE JobHandle, PBOOL Result);
BOOL WINAPI TerminateJobObject(HANDLE hJob, UINT uExitCode);
}

{TJwJobObject}
var CompletionUniqueID : Integer = 1;


constructor TJwJobObject.Create(const Name : TJwString;
  const ErrorIfAlreadyExists : Boolean;
  const SecurityAttributes : TJwSecurityDescriptor);

var
  pSA : LPSECURITY_ATTRIBUTES;
begin
  SetLastError(0);

  OnNotification := nil;
  OnNoActiveProcesses := nil;
  fIOUniqueID := InterlockedIncrement(CompletionUniqueID);

  fJobName := Name;


  if Assigned(SecurityAttributes) then
    pSA := LPSECURITY_ATTRIBUTES(SecurityAttributes.Create_SA2())
  else
    pSA := nil;


  if Length(Name) > 0 then
  begin
    fHandle := {$IFDEF UNICODE}CreateJobObjectW{$ELSE}CreateJobObjectA{$ENDIF}
      (pSA, TJwPChar(Name));
    fThread := TJwInternalJobObjectIOCompletitionThread.Create(Self, true,IOJOBNAME+Name);
  end
  else
  begin
    fHandle := {$IFDEF UNICODE}CreateJobObjectW{$ELSE}CreateJobObjectA{$ENDIF}
      (pSA, nil);
    fThread := TJwInternalJobObjectIOCompletitionThread.Create(Self, true,IOJOBNAME+IntToStr(GetCurrentThreadId));
  end;

  TJwSecurityDescriptor.Free_SA(PSecurityAttributes(pSA));

  fAccessMask := JOB_OBJECT_ALL_ACCESS;
  fTerminateOnDestroy := false;


  if (fHandle = 0) or
   (ErrorIfAlreadyExists and (GetLastError() = ERROR_ALREADY_EXISTS))
  then
  begin
    FreeAndNil(fThread);
    if (GetLastError() <> 0) and (fHandle <> 0) then
      CloseHandle(fHandle);
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      '',
      'Create',                                //sSourceProc
      ClassName,                                //sSourceClass
      '',                          //sSourceFile
      0,                                           //iSourceLine
      True,                                  //bShowLastError
      'CreateJobObject',                   //sWinCall
      ['CreateJobObject']);                                  //const Args: array of const
  end;

  fLock := TMultiReadExclusiveWriteSynchronizer.Create;
  fDataList := TJwIntTupleList.Create;


  fThread.Resume;

  SetObjectAssociateCompletionPortInformation(Pointer(fIOUniqueID),
    fThread.IOHandle);
end;


constructor TJwJobObject.Create(const Name : TJwString;
      const ErrorIfAlreadyExists : Boolean;
      const SecurityAttributes : TJwSecurityDescriptor;
      CompletionKey : Integer; CompletionPort : THandle);
var
  pSA : LPSECURITY_ATTRIBUTES;
begin
  SetLastError(0);

  OnNotification := nil;
  OnNoActiveProcesses := nil;
  fIOUniqueID := CompletionKey;

  fJobName := Name;


  if Assigned(SecurityAttributes) then
    pSA := LPSECURITY_ATTRIBUTES(SecurityAttributes.Create_SA2())
  else
    pSA := nil;


  if Length(Name) > 0 then
  begin
    fHandle := {$IFDEF UNICODE}CreateJobObjectW{$ELSE}CreateJobObjectA{$ENDIF}
      (pSA, TJwPChar(Name));
  end
  else
  begin
    fHandle := {$IFDEF UNICODE}CreateJobObjectW{$ELSE}CreateJobObjectA{$ENDIF}
      (pSA, nil);
  end;

  TJwSecurityDescriptor.Free_SA(PSecurityAttributes(pSA));

  fAccessMask := JOB_OBJECT_ALL_ACCESS;
  fTerminateOnDestroy := false;


  if (fHandle = 0) or
   (ErrorIfAlreadyExists and (GetLastError() = ERROR_ALREADY_EXISTS))
  then
  begin
    if (GetLastError() <> 0) and (fHandle <> 0) then
      CloseHandle(fHandle);
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      '',
      'Create',                                //sSourceProc
      ClassName,                                //sSourceClass
      '',                          //sSourceFile
      0,                                           //iSourceLine
      True,                                  //bShowLastError
      'CreateJobObject',                   //sWinCall
      ['CreateJobObject']);                                  //const Args: array of const
  end;

  fLock := TMultiReadExclusiveWriteSynchronizer.Create;
  fDataList := TJwIntTupleList.Create;

  SetObjectAssociateCompletionPortInformation(Pointer(CompletionKey),
    CompletionPort);
end;


constructor TJwJobObject.Create(const Name : TJwString; const DesiredAccess : TJwAccessMask;
  const InheritHandles : Boolean; CompletionKey : Integer; CompletionPort : THandle);
var
  IOPort : THandle;
begin
  fHandle := {$IFDEF UNICODE}OpenJobObjectW{$ELSE}OpenJobObjectA{$ENDIF}
    (DesiredAccess, InheritHandles, TJwPChar(Name));

  fJobName := Name;

  OnNotification := nil;
  OnNoActiveProcesses := nil;

  fAccessMask := DesiredAccess;
  fTerminateOnDestroy := false;


  if (fHandle = 0) then
  begin
    if fHandle <> 0 then
      CloseHandle(fHandle);
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      '',
      'Create',                                //sSourceProc
      ClassName,                                //sSourceClass
      '',                          //sSourceFile
      0,                                           //iSourceLine
      True,                                  //bShowLastError
      'OpenJobObject',                   //sWinCall
      ['OpenJobObject']);                                  //const Args: array of const
  end;

  fIOUniqueID := Integer(INVALID_HANDLE_VALUE);
  fThread := nil;



  {We cannot get CompletionKey and CompletionPort for an existing job object.
  So the user has to supply them
  }
  if (CompletionPort <> INVALID_HANDLE_VALUE) and
     (CompletionPort <> 0)
    and not DuplicateHandle(
     GetCurrentProcess,//__in        HANDLE hSourceProcessHandle,
      CompletionPort,//__in        HANDLE hSourceHandle,
     GetCurrentProcess,//__in        HANDLE hTargetProcessHandle,
      @IOPort,//__deref_out LPHANDLE lpTargetHandle,
      0,//__in        DWORD dwDesiredAccess,
      true,//__in        BOOL bInheritHandle,
      DUPLICATE_SAME_ACCESS//__in        DWORD dwOptions
    )
  then
  begin
    if fHandle <> 0 then
      CloseHandle(fHandle);
    RaiseLastOSError;
  end
  else
  if (CompletionPort <> INVALID_HANDLE_VALUE) and
     (CompletionPort <> 0) then
  begin
    fIOUniqueID := CompletionKey;
    fThread := TJwInternalJobObjectIOCompletitionThread.CreateWithIOPort(Self, IOPort, false, IOJOBNAME+Name);

    try
      SetObjectAssociateCompletionPortInformation(Pointer(fIOUniqueID),
        fThread.IOHandle);
    except
      //may fail, but doesn't matter
    end;
  end;

  fLock := TMultiReadExclusiveWriteSynchronizer.Create;
  fDataList := TJwIntTupleList.Create;
end;

destructor TJwJobObject.Destroy;
begin
  if TerminateOnDestroy then
    TerminateJobObject(0);

  fLock.BeginWrite;
  try
    FreeAndNil(fDataList);
  finally
    fLock.EndWrite;
  end;
  FreeAndNil(fLock);

  if Assigned(fThread) then
  begin
    fThread.Terminate;
    fThread.WaitFor;
    FreeAndNil(fThread);
  end;

  if (fHandle <> 0) then
    CloseHandle(fHandle);



end;

function TJwJobObject.GetAllotedCPUTimeSignalState : Boolean;
begin
  result := WaitForSingleObject(fHandle,0) = WAIT_OBJECT_0;
end;

function TJwJobObject.WaitForAllotedCPUTimeSignal(const TimeOut : DWORD) : TJwWaitState;
begin
  case WaitForSingleObject(fHandle,TimeOut) of
    WAIT_OBJECT_0 : result := wsSignaled;
    WAIT_TIMEOUT  : result := wsTimeOut
  else
    result := wsNonSignaled;
  end;
end;


function TJwJobObject.GetProcesses : TJwProcessList;
var
  List : PJobObjectBasicProcessIdList;
  len, i,
  rlen : DWORD;
begin
  rlen := 0;

  Len := GetJobObjectInformationLength(JobObjectBasicProcessIdList);

  GetMem(List, len);
  ZeroMemory(List, len);

  try
    if not QueryInformationJobObject(
      fHandle,//__in_opt   HANDLE hJob,
      JobObjectBasicProcessIdList,//__in       JOBOBJECTINFOCLASS JobObjectInfoClass,
      List,//__out      LPVOID lpJobObjectInfo,
      len,//__in       DWORD cbJobObjectInfoLength,
      @rlen//__out_opt  LPDWORD lpReturnLength
      ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'GetJobObjectInformation',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'QueryInformationJobObject',                   //sWinCall
          ['QueryInformationJobObject']);                                  //const Args: array of const

    SetLength(result, List^.NumberOfProcessIdsInList);

    for i := low(result) to high(result) do
      result[i] :=   List^.ProcessIdList[i];

  finally
    FreeMem(List);
  end;
end;

function TJwJobObject.GetProcessesCount : Cardinal;
//var Data : Pointer;
//    List : PJobObjectBasicProcessIdList;
begin
  try
    result := GetBasicAndIOInformation.BasicInfo.ActiveProcesses;
  except
    on E : EJwsclWinCallFailedException do
    begin
      E.SourceProc := 'GetProcessesCount';
      raise E;
    end;
  end;
end;

function TJwJobObject.GetBasicAndIOInformation : TJobObjectBasicAndIoAccountingInformation;
var
  len : DWORD;
begin
  Len := sizeof(result); //return static structure

  try
    if not QueryInformationJobObject(
      fHandle,//__in_opt   HANDLE hJob,
      JobObjectBasicAndIoAccountingInformation,//__in       JOBOBJECTINFOCLASS JobObjectInfoClass,
      @result,//__out      LPVOID lpJobObjectInfo,
      len,//__in       DWORD cbJobObjectInfoLength,
      nil//__out_opt  LPDWORD lpReturnLength
      ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'GetBasicInformation',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'QueryInformationJobObject',                   //sWinCall
          ['QueryInformationJobObject']);                                  //const Args: array of const
  finally
  end;
end;

function TJwJobObject.GetBasicLimitInformation : TJobObjectBasicLimitInformation;
var
  len : DWORD;
begin
  Len := sizeof(result); //return static structure

  try
    if not QueryInformationJobObject(
      fHandle,//__in_opt   HANDLE hJob,
      JobObjectBasicLimitInformation,//__in       JOBOBJECTINFOCLASS JobObjectInfoClass,
      @result,//__out      LPVOID lpJobObjectInfo,
      len,//__in       DWORD cbJobObjectInfoLength,
      nil//__out_opt  LPDWORD lpReturnLength
      ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'GetBasicLimitInformation',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'QueryInformationJobObject',                   //sWinCall
          ['QueryInformationJobObject']);                                  //const Args: array of const
  finally
  end;
end;



function TJwJobObject.GetBasicUIRestrictions : TJobObjectBasicUiRestrictions;
var
  len : DWORD;
begin
  Len := sizeof(result); //return static structure

  try
    if not QueryInformationJobObject(
      fHandle,//__in_opt   HANDLE hJob,
      JobObjectBasicUIRestrictions,//__in       JOBOBJECTINFOCLASS JobObjectInfoClass,
      @result,//__out      LPVOID lpJobObjectInfo,
      len,//__in       DWORD cbJobObjectInfoLength,
      nil//__out_opt  LPDWORD lpReturnLength
      ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'GetBasicUIRestrictions',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'QueryInformationJobObject',                   //sWinCall
          ['QueryInformationJobObject']);                                  //const Args: array of const
  finally
  end;
end;


procedure TJwJobObject.SetBasicLimitInformation(Info : TJobObjectBasicLimitInformation);
begin
  if not SetInformationJobObject(
      fHandle,//__in  HANDLE hJob,
      JobObjectBasicLimitInformation,//__in  JOBOBJECTINFOCLASS JobObjectInfoClass,
      @Info,//__in  LPVOID lpJobObjectInfo,
      sizeof(Info)//__in  DWORD cbJobObjectInfoLength
    ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'GetBasicUIRestrictions',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'QueryInformationJobObject',                   //sWinCall
          ['QueryInformationJobObject']);                                  //const Args: array of const
end;

procedure TJwJobObject.SetBasicUIRestrictions(Info : TJobObjectBasicUiRestrictions);
begin
  if not SetInformationJobObject(
      fHandle,//__in  HANDLE hJob,
      JobObjectBasicUIRestrictions,//__in  JOBOBJECTINFOCLASS JobObjectInfoClass,
      @Info,//__in  LPVOID lpJobObjectInfo,
      sizeof(Info)//__in  DWORD cbJobObjectInfoLength
    ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'GetBasicUIRestrictions',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'QueryInformationJobObject',                   //sWinCall
          ['QueryInformationJobObject']);                                  //const Args: array of const
end;

procedure TJwJobObject.GetObjectAssociateCompletionPortInformation(
        out CompletionKey : Pointer; out CompletionPort : THandle);
var
  len : DWORD;
  Data : TJobObjectAssociateCompletionPort;
begin
  Len := sizeof(Data); //return static structure

  try
    if not QueryInformationJobObject(
      fHandle,//__in_opt   HANDLE hJob,
      JobObjectAssociateCompletionPortInformation,//__in       JOBOBJECTINFOCLASS JobObjectInfoClass,
      @Data,//__out      LPVOID lpJobObjectInfo,
      Len,//__in       DWORD cbJobObjectInfoLength,
      nil//__out_opt  LPDWORD lpReturnLength
      ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'GetBasicUIRestrictions',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'QueryInformationJobObject',                   //sWinCall
          ['QueryInformationJobObject']);                                  //const Args: array of const
  finally
  end;
  CompletionKey := Data.CompletionKey;
  CompletionPort := Data.CompletionPort;
end;

function TJwJobObject.GetIOHandle : THandle;
begin
  if Assigned(fThread) then
  begin
    result := fThread.fIOHandle;
  end
  else
    result := INVALID_HANDLE_VALUE;
end;

procedure TJwJobObject.SetObjectAssociateCompletionPortInformation(
    const CompletionKey : Pointer; CompletionPort : THandle);
var
  Data : TJobObjectAssociateCompletionPort;
  Len : Cardinal;
begin
  Len := sizeof(Data);
  Data.CompletionKey := CompletionKey;
  Data.CompletionPort := CompletionPort;

  if not SetInformationJobObject(
      fHandle,//__in  HANDLE hJob,
      JobObjectAssociateCompletionPortInformation,//__in  JOBOBJECTINFOCLASS JobObjectInfoClass,
      @Data,//__in  LPVOID lpJobObjectInfo,
      len//__in  DWORD cbJobObjectInfoLength
    ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'SetObjectAssociateCompletionPortInformation',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'SetInformationJobObject',                   //sWinCall
          ['SetInformationJobObject']);                                  //const Args: array of const
end;

function TJwJobObject.GetExtendedLimitInformation : TJobObjectExtendedLimitInformation;
var
  len : DWORD;
begin
  Len := sizeof(result); //return static structure

  ZeroMemory(@result, sizeof(result));

  try
    if not QueryInformationJobObject(
      fHandle,//__in_opt   HANDLE hJob,
      JobObjectExtendedLimitInformation,//__in       JOBOBJECTINFOCLASS JobObjectInfoClass,
      @result,//__out      LPVOID lpJobObjectInfo,
      len,//__in       DWORD cbJobObjectInfoLength,
      nil//__out_opt  LPDWORD lpReturnLength
      ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'GetBasicUIRestrictions',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'QueryInformationJobObject',                   //sWinCall
          ['QueryInformationJobObject']);                                  //const Args: array of const
  finally
  end;
end;

procedure TJwJobObject.SetExtendedLimitInformation(Info : TJobObjectExtendedLimitInformation);
var i : Integer;
begin
  i := sizeof(Info);
  if not SetInformationJobObject(
      fHandle,//__in  HANDLE hJob,
      JobObjectExtendedLimitInformation,//__in  JOBOBJECTINFOCLASS JobObjectInfoClass,
      @Info,//__in  LPVOID lpJobObjectInfo,
      i//__in  DWORD cbJobObjectInfoLength
    ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'SetExtendedLimitInformation',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'SetInformationJobObject',                   //sWinCall
          ['SetInformationJobObject']);                                  //const Args: array of const
end;

function TJwJobObject.GetSecurityDescriptor(const Si :
  TJwSecurityInformationFlagSet) : TJwSecurityDescriptor;
begin
  result := TJwSecureGeneralObject.GetSecurityInfo(Handle, SE_KERNEL_OBJECT,
    Si);
end;


function TJwJobObject.GetJobObjectInformationLength(
   const JobObjectInfoClass : JOBOBJECTINFOCLASS) : Cardinal;
var
  //len,
  //rlen,
  //res : DWORD;
  len,
  rlen : DWORD;
  Data : Pointer;
begin
  rlen := 0;
  len := 32;


  SetLastError(ERROR_MORE_DATA);

  while (GetLastError = ERROR_MORE_DATA) do
  begin
    GetMem(Data, len);
    try
      SetLastError(0);

      if not QueryInformationJobObject(
        fHandle,//__in_opt   HANDLE hJob,
        JobObjectInfoClass,//__in       JOBOBJECTINFOCLASS JobObjectInfoClass,
        Data,//__out      LPVOID lpJobObjectInfo,
        len,//__in       DWORD cbJobObjectInfoLength,
        @rlen//__out_opt  LPDWORD lpReturnLength
        )
        and (GetLastError <> ERROR_MORE_DATA) then

       raise EJwsclWinCallFailedException.CreateFmtWinCall(
            '',
            'GetJobObjectInformationLength',                                //sSourceProc
            ClassName,                                //sSourceClass
            '',                          //sSourceFile
            0,                                           //iSourceLine
            True,                                  //bShowLastError
            'QueryInformationJobObject',                   //sWinCall
            ['QueryInformationJobObject'])
      else
      begin
        SetLastError(0);
        rlen := len;
      end;
    finally
      FreeMem(Data);
      if (GetLastError = ERROR_MORE_DATA) then
      begin
        Inc(Len, 32);
        SetLastError(ERROR_MORE_DATA);
      end;
    end;
  end;
  result := rlen;
end;

procedure TJwJobObject.GetJobObjectInformation(
   const JobObjectInfoClass : JOBOBJECTINFOCLASS;
   out Data : Pointer);
var
  //len,
  //rlen,
  //res : DWORD;
  len,
  rlen : DWORD;

begin
  rlen := 0;
  len := GetJobObjectInformationLength(JobObjectInfoClass);
  GetMem(Data, len);
  ZeroMemory(Data, len);

  if Data = nil then
    RaiseLastOSError;

  if not QueryInformationJobObject(
    fHandle,//__in_opt   HANDLE hJob,
    JobObjectInfoClass,//__in       JOBOBJECTINFOCLASS JobObjectInfoClass,
    Data,//__out      LPVOID lpJobObjectInfo,
    len,//__in       DWORD cbJobObjectInfoLength,
    @rlen//__out_opt  LPDWORD lpReturnLength
    ) then
   raise EJwsclWinCallFailedException.CreateFmtWinCall(
        '',
        'GetJobObjectInformation',                                //sSourceProc
        ClassName,                                //sSourceClass
        '',                          //sSourceFile
        0,                                           //iSourceLine
        True,                                  //bShowLastError
        'QueryInformationJobObject',                   //sWinCall
        ['QueryInformationJobObject']);                                  //const Args: array of const
end;

function TJwJobObject.IsProcessInJob(hProcess : TJwProcessHandle) : Boolean;
var LB : LongBool;
begin
  LB := false; 
  if not JwaWindows.IsProcessInJob(hProcess, fHandle, LB) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
        '',
        'IsProcessInJob',                                //sSourceProc
        ClassName,                                //sSourceClass
        '',                          //sSourceFile
        0,                                           //iSourceLine
        True,                                  //bShowLastError
        'IsProcessInJob',                   //sWinCall
        ['IsProcessInJob']);                                  //const Args: array of const
  result := LB;
end;

procedure TJwJobObject.AssignProcessToJobObject(hProcess : TJwProcessHandle; Data : Pointer);
begin
  fLock.BeginWrite;
  try
    fDataList.Add(GetProcessId(hProcess),Data);

    try
      if not JwaWindows.AssignProcessToJobObject(fHandle, hProcess) then
         raise EJwsclWinCallFailedException.CreateFmtWinCall(
            '',
            'AssignProcessToJobObject',                                //sSourceProc
            ClassName,                                //sSourceClass
            '',                          //sSourceFile
            0,                                           //iSourceLine
            True,                                  //bShowLastError
            'AssignProcessToJobObject',                   //sWinCall
            ['AssignProcessToJobObject']);                                  //const Args: array of const
    except
      fDataList.DeleteIndex(GetProcessId(hProcess));
      raise
    end;
  finally
    fLock.EndWrite;
  end;
end;

procedure TJwJobObject.TerminateJobObject(const ExitCode : DWORD);
begin
  if not JwaWindows.TerminateJobObject(fHandle, ExitCode) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
        '',
        'TerminateJobObject',                                //sSourceProc
        ClassName,                                //sSourceClass
        '',                          //sSourceFile
        0,                                           //iSourceLine
        True,                                  //bShowLastError
        'TerminateJobObject',                   //sWinCall
        ['TerminateJobObject']);                                  //const Args: array of const
end;

function TJwJobObject.GetJobLimit : TJwJobLimits;
var
  Info : TJobObjectBasicLimitInformation;
begin
  Info := GetBasicLimitInformation;
  Result := TJwEnumMap.ConvertJobLimit(Info.LimitFlags);
end;

procedure TJwJobObject.SetJobLimit(const Limit : TJwJobLimits);
var
  Info : TJobObjectBasicLimitInformation;
begin
  Info := GetBasicLimitInformation;
  Info.LimitFlags := TJwEnumMap.ConvertJobLimitType(Limit);
  SetBasicLimitInformation(Info);
end;

function TJwJobObject.GetJobUiLimit : TJwJobUiLimits;
var
  Info : TJobObjectBasicUiRestrictions;
begin
  Info := GetBasicUIRestrictions;
  Result := TJwEnumMap.ConvertJobUiLimit(Info.UIRestrictionsClass);
end;

procedure TJwJobObject.SetJobUiLimit(const Limit : TJwJobUiLimits);
var
  Info : TJobObjectBasicUiRestrictions;
begin
  Info := GetBasicUIRestrictions;
  Info.UIRestrictionsClass := TJwEnumMap.ConvertJobUiLimitType(Limit);
  SetBasicUIRestrictions(Info);
end;

function TJwJobObject.GetActiveProcessCount : Cardinal;
begin
  result := GetBasicAndIOInformation.BasicInfo.ActiveProcesses;
end;


procedure TJwJobObject.ResetIOThread(const Force : Boolean);
var
  IOPort : THandle;
begin
  if not Assigned(fThread) then
  begin
    fThread := TJwInternalJobObjectIOCompletitionThread.Create(Self, false,IOJOBNAME+Name);
  end
  else 
  begin
    if not Force and not fThread.Terminated then
      exit;

    fThread.fRemainPort := true;
    IOPort := fThread.IOHandle;

    fThread.Terminate;
    case WaitForSingleObject(fThread.Handle, 4000) of
      WAIT_TIMEOUT : TerminateThread(fThread.Handle, ERROR_TIMEOUT);
      WAIT_OBJECT_0 : ;
    end;

    try
      FreeAndNil(fThread);
    except
    end;
    
    fThread := TJwInternalJobObjectIOCompletitionThread.CreateWithIOPort(Self, IOPort,false,IOJOBNAME+Name);
  end;
end;


constructor TJwInternalJobObjectIOCompletitionThread.Create(
 const Parent: TJwJobObject;const CreateSuspended: Boolean; const Name: TJwString);
begin
  fJwJobObject := Parent;
  fIOHandle := CreateIoCompletionPort(
                  INVALID_HANDLE_VALUE,//__in      HANDLE FileHandle,
                  0,//__in_opt  HANDLE ExistingCompletionPort,
                  Parent.IOUniqueID,//__in      ULONG_PTR CompletionKey,
                  0//__in      DWORD NumberOfConcurrentThreads
                );
  if fIOHandle = 0 then
    RaiseLastOSError;

  Self.FreeOnTerminate := false;
  fRemainPort := false;

  inherited Create(CreateSuspended,Name);
end;

constructor TJwInternalJobObjectIOCompletitionThread.CreateWithIOPort(
  const Parent: TJwJobObject; IOPort : THandle;
  const CreateSuspended: Boolean; const Name: TJwString);
begin
  fJwJobObject := Parent;
  fIOHandle := IOPort;
  if fIOHandle = 0 then
  begin
    SetLastError(ERROR_INVALID_HANDLE);
    RaiseLastOSError;
  end;

  Self.FreeOnTerminate := false;
  fRemainPort := false;

  inherited Create(CreateSuspended,Name);
end;

procedure TJwInternalJobObjectIOCompletitionThread.Execute;

  function GetUserData(ProcessID : DWORD) : Pointer;
  //var i : Integer;
  begin
    fJwJobObject.fLock.BeginRead;
    try
      try
        result := fJwJobObject.fDataList[ProcessID];
      except
        result := nil;
      end;
    finally
      fJwJobObject.fLock.EndRead;
    end;
  end;
var
  pOV : POverlapped;
  lpNumberOfBytes,
  lpCompletionKey : Cardinal;
  Res : Boolean;
  L : DWORD;
  //ID : DWORD;
  Data : Pointer;
  
const ERROR_ABANDONED_WAIT_0 = 735;

begin
  inherited; //sets thread name
  
  fRemainPort := false;
  pOV := nil;
  ReturnValue := 0;

  try
    repeat
      SetLastError(0);
      Res := GetQueuedCompletionStatus(
        fIOHandle,//__in   HANDLE CompletionPort,
        lpNumberOfBytes,//__out  LPDWORD lpNumberOfBytes,
        lpCompletionKey,//__out  PULONG_PTR lpCompletionKey,
        pOV,//__out  LPOVERLAPPED* lpOverlapped,
        INFINITE//__in   DWORD dwMilliseconds
      );

      L := GetLastError();
      if (not Res and (L <> 0)) or
         fRemainPort or Terminated then
      begin
        ReturnValue := L;

        inherited Terminate;
        break;
      end
      else
      if Assigned(fJwJobObject) and Res then
      begin
        if Assigned(fJwJobObject.OnNoActiveProcesses) and
          (fJwJobObject.GetActiveProcessCount = 0) then
        begin
          try
            fJwJobObject.OnNoActiveProcesses(fJwJobObject);
          except
          end;
        end;


        if Assigned(fJwJobObject.OnNotification) and Assigned(pOV) then
        begin
          Data := GetUserData(TJwProcessId(pOV));
          try
            fJwJobObject.OnNotification(fJwJobObject, TJwProcessId(pOV),
              TJwEnumMap.ConvertJobMessage(lpNumberOfBytes), Data);
          except
          end;
        end;
      end;
    until Terminated;
  finally
    if not fRemainPort then
    begin
      CloseHandle(fIOHandle);
      fIOHandle := INVALID_HANDLE_VALUE;
      fRemainPort := false;
    end;
  end;
end;


procedure TJwInternalJobObjectIOCompletitionThread.Terminate;
begin
  inherited;
  if not fRemainPort then
  begin
    //send dummy completion for GetQueuedCompletionStatus to return
    PostQueuedCompletionStatus(fIOHandle,0,0,nil);
  end;
end;


{TJwJobObjectSessionList}
function TJwJobObjectSessionList.GetJobObject(SessionIndex : Cardinal) : TJwJobObject;
begin
  fLock.BeginRead;
  try
    result := TJwJobObject(fList[SessionIndex]);
  finally
    fLock.EndRead;
  end;
end;

procedure TJwJobObjectSessionList.Clear(const TerminateJob : TJwJobTermination = jtSubjection);
var i : Integer;
begin
  fLock.BeginWrite;
  try
    for i := 0 to Count -1 do
    begin
      if fList[i] <> nil then
      begin
        try
          case TerminateJob of
            jtAll  : TJwJobObject(fList[i]).TerminateOnDestroy := true;
            jtNone : TJwJobObject(fList[i]).TerminateOnDestroy := false;
          end;
          TJwJobObject(fList[i]).Free;
        finally
          fList[i] := nil;
        end;
      end;
    end;
    fList.Clear;
  finally
    fLock.EndWrite;
  end;
end;

function TJwJobObjectSessionList.GetProcessHandle(SessionIndex, ProcessIndex : Cardinal) : THandle;
var Processes : TJwProcessList;
begin
  result := INVALID_HANDLE_VALUE;
  Processes := Nil;

  fLock.BeginRead;
  try
    if (SessionIndex >= Cardinal(fList.Count)) then
      raise EJwsclInvalidIndex.CreateFmt(RsInvalidParameterIndex, [SessionIndex,'SessionIndex']);

    Processes := TJwJobObject(fList[SessionIndex]).Processes;

    if (ProcessIndex > Cardinal(High(Processes))) then
      raise ERangeError.CreateFmt(RsInvalidParameterIndex, [ProcessIndex,'ProcessIndex']);

    if Processes <> nil then
      result := Processes[ProcessIndex];

  finally
    fLock.EndRead;
  end;
end;


function TJwJobObjectSessionList.GetCount : Cardinal;
begin
  fLock.BeginRead;
  try
    result := fList.Count;
  finally
    fLock.EndRead;
  end;
end;

constructor TJwJobObjectSessionList.Create(const NewJobObjectEvent: TJwOnNewJobObject);
begin
  JwRaiseOnNilParameter(@NewJobObjectEvent, 'NewJobObjectEvent','Create', ClassName, RsUNProcess);

  inherited Create;
  fLock := TMultiReadExclusiveWriteSynchronizer.Create;

  fList := TList.Create;
  fOnNewJobObject := NewJobObjectEvent;
end;

destructor TJwJobObjectSessionList.Destroy;
begin
  Clear(jtSubjection);

  fLock.BeginWrite;
  try
    FreeAndNil(fList);
  finally
    fLock.EndWrite;
    FreeAndNil(fLock);
  end;

  inherited;
end;


procedure TJwJobObjectSessionList.AssignProcessToJob(Process : TJwProcessHandle; Data : Pointer);
var JobObjectIndex : Cardinal;
begin
  AssignProcessToJob(Process, Data, JobObjectIndex);
end;

procedure TJwJobObjectSessionList.AssignProcessToJob(Process : TJwProcessHandle; Data : Pointer; out JobObjectIndex : Cardinal);
var
  //i, ID : Cardinal;
  ID : Cardinal;
  NewObject : TJwJobObject;
begin
  if not Assigned(OnNewJobObject) then
    raise EJwsclMissingEvent.CreateFmtEx(RsMissingEvent,
        'AssignProcessToJob',ClassName, RsUNProcess, 0, 0, ['OnNewJobObject']);

  fLock.BeginWrite;
  try
    ID := JwGetProcessSessionID(Process, pptHandle);
    JobObjectIndex := ID;
    NewObject := nil;

    {Just to be sure we check for negative count
     otherwise this loop may run forever.
    }
    while (fList.Count >= 0) and (ID >= Cardinal(fList.Count)) do
    begin
      NewObject := nil;
      OnNewJobObject(Self, Process, ID, fList.Count ,NewObject);
      if not Assigned(NewObject) then
        raise EJwsclInvalidParameterException.CreateFmtEx(RsInvalidJobObject,
          'AssignProcessToJob',ClassName, RsUNProcess, 0, 0, ['OnNewJobObject']);

      fList.Add(NewObject);
    end;
    if fList.Count = 0 then
    begin
      OnNewJobObject(Self, Process, 0, 0 ,NewObject);
      fList.Add(NewObject);
    end;

    if not Assigned(NewObject) then
      NewObject := TJwJobObject(fList[ID]);

    NewObject.AssignProcessToJobObject(Process, Data);
  finally
    fLock.EndWrite;
  end;
end;

procedure TJwJobObjectSessionList.SetTerminateJobsOnFree(Terminate : Boolean);
var i : Integer;
begin
  fLock.BeginWrite;
  try
    for i := Count -1 downto 0 do
    begin
      if fList[i] <> nil then
        TJwJobObject(fList[i]).TerminateOnDestroy := Terminate;
    end;
  finally
    fLock.EndWrite;
  end;
end;




{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}