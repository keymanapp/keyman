{ Description
  Project JEDI Windows Security Code Library (JWSCL)
  
  This unit provides access to Terminal Server api functions through it's key
  object TJwTerminalServer
  Author
  Remko Weijnen
  License
  The contents of this file are subject to the Mozilla Public License Version 1.1
  (the "License"); you may not use this file except in compliance with the
  \License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
  
  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
  specific language governing rights and limitations under the License.
  
  Alternatively, the contents of this file may be used under the terms of the GNU
  Lesser General Public License (the "LGPL License"), in which case the provisions
  of the LGPL License are applicable instead of those above. If you wish to allow
  use of your version of this file only under the terms of the LGPL License and
  not to allow others to use your version of this file under the MPL, indicate
  your decision by deleting the provisions above and replace them with the notice
  and other provisions required by the LGPL License. If you do not delete the
  provisions above, a recipient may use your version of this file under either the
  MPL or the LGPL License.
  
  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
  Note
  The Original Code is JwsclTerminalServer.pas.
  
  The Initial Developer of the Original Code is Remko Weijnen. Portions created by
  Remko Weijnen are Copyright (C) Remko Weijnen. All rights reserved.
  
  The central object of the JwsclTerminalServer unit is the TJwTerminalServer
  object. It represents a Terminal Server, the connection to this server and holds
  the session- and processlist.
  
  Some Key functions of TJwTerminalServer are:
  
    * TJwTerminalServer.EnumerateSessions enumerates all Terminal Server
  sessions into a TJwSessionList which can be accessed by the Sessions property.
    * TJwTerminalServer.EnumerateProcesses enumerates all Terminal Server
  processes into a TJwProcessList which can be accessed by the Processes property.
    * TJwTerminalServer.EnumerateServers enumerates all Terminal Servers in a
  domain.
    * TJwTerminalServer.Shutdown Shuts down and optionally restarts the specified
  Terminal Server.

  TJwTerminalServer also offers Events to monitor Terminal Server activity such as
  OnSessionConnect, OnSessionCreate, OnSessionLogon and OnSessionLogoff.

  A unique feature of TJwTerminalServer is that it's able to return detailled
  information about Terminal Server, Sessions and Processes that is not available
  using the normal Terminal Server API's or Microsoft Tools! This includes
  detailled process memory usage information and extended session information such
  as ShadowMode, ShadowState and Remote Address.

  The schema belows shows the relations between TJwTerminalServer, the
  TJwWTSSessionList with TJwWTSSessions and the TJwWTSProcessList with
  TjwWTSSessions.

  <image TJwTerminalServer-Hierarchy>                                              }

{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclTerminalServer;
{$INCLUDE ..\includes\Jwscl.inc}
{$TYPEINFO ON}
interface

uses
  Classes, Contnrs, SysUtils, Registry,
  JwaWindows,
  JwsclExceptions, JwsclResource, JwsclKnownSid, JwsclSid, JwsclTypes,
  JwsclUtils, JwsclToken, JwsclVersion, JwsclStrings;

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type

  { forward declarations }
  TJwTerminalServer = class;
  TJwTerminalServerList = class;
  TJwWTSEventThread = class;
  TJwWTSEnumServersThread = class;
  TJwWTSSessionShadow = class;
  TJwWTSSessionStatistics = class;
  TJwWTSSession = class;
  TJwWTSSessionList = class;
  TJwWTSProcess = class;
  TJwWTSProcessList = class;

  { The <b>CachedUsername</b> record is use internally in the <link TJwTerminalServer.CachedGetUserFromSessionId@DWORD, CachedGetUserFromSessionId>
    function.                                                                                                                                       }
  TCachedUser = record
    SessionId: DWORD;
    Username: TJwString;
  end;

  TJwSessionsEnumerator = class;
  TJwProcessEnumerator = class;

  {<B>TJwOnProcessFound</B> is called by EnumerateProcesses everytime a process is enumerated.
   @param Sender contains the instance which enumerates a process 
   @param Process contains information about the enumerated process.
     The caller must free this instance because it will not be used anymore 
   @param Cancel stops the enumeration if true; otherwise continues 
   @param Data contains user data supplied by EnumerateProcesses' parmaeter Data 
  }
  TJwOnProcessFound = procedure(const Sender: TJwTerminalServer;
    var Process: TJwWTSProcess; var Cancel: Boolean; Data: Pointer) of object;

  {<B>PJwTerminalServer</B> is a pointer to a TJwTerminalServer instance}
  PJwTerminalServer = ^TJwTerminalServer;
  { <b>AbstractTJwTerminalServer</b> is the central object of JwsclTerminalServer
    and holds the session- and processlist.
    
    <b>TJwTerminalServer</b> offers connection to a Terminal Server which you can
    specify with the Server property. Key functions of <b>TJwTerminalServer</b> are:
    
      * EnumerateSessions enumerates all Terminal Server sessions into a
    TJwSessionList which can be accessed by the Sessions property.
      * EnumerateProcesses enumerates all Terminal Server processes into a
    TJwProcessList which can be accessed by the Processes property.
      * EnumerateServers enumerates all Terminal Servers in a domain.
      * Shutdown Shuts down and optionally restarts the specified
    Terminal Server.
    
    <b>TJwTerminalServer</b> also offers Events to monitor Terminal Server activity
    such as OnSessionConnect, OnSessionCreate, OnSessionLogon and OnSessionLogoff.
    
    The schema below shows the relations between TJwTerminalServer, the
    TJwWTSSessionList with TJwWTSSessions and the TJwWTSProcessList with
    TjwWTSSessions.
    
    <image TJwTerminalServer-Hierarchy>
                                                                                     }
  TJwTerminalServer = class(TObject)
  private
    function GetSessionStatistics: TJwWTSSessionStatistics;
  protected
    CachedUser: TCachedUser;
    {@exclude}
    FComputerName: TJwString;
    {@exclude}
    FConnected: Boolean;
    {@exclude}
    FData: Pointer;
    {@exclude}
    FEnumServersThread: TJwWTSEnumServersThread;
    {@exclude}
    FIdleProcessName: TJwString;
    {@exclude}
    FLastEventFlag: DWORD;
    {@exclude}
    FOnServersEnumerated: TNotifyEvent;
    {@exclude}
    FOnSessionConnect: TNotifyEvent;
    {@exclude}
    FOnSessionCreate: TNotifyEvent;
    {@exclude}
    FOnSessionDelete: TNotifyEvent;
    {@exclude}
    FOnSessionDisconnect: TNotifyEvent;
    {@exclude}
    FOnSessionEvent: TNotifyEvent;
    {@exclude}
    FOnLicenseStateChange: TNotifyEvent;
    {@exclude}
    FOnSessionLogon: TNotifyEvent;
    {@exclude}
    FOnSessionLogoff: TNotifyEvent;
    {@exclude}
    FOnSessionStateChange: TNotifyEvent;
    {@exclude}
    FOnWinStationRename: TNotifyEvent;
    {@exclude}
    FPhysicalMemory: Int64;
    {@exclude}
    FProcessors: Integer;
    {@exclude}
    FServerHandle: THandle;
    {@exclude}
    FServers: TStringList;
    {@exclude}
    FSessions: TJwWTSSessionList;
    {@exclude}
    FSessionStatistics: TJwWTSSessionStatistics;
    {@exclude}
    FProcesses: TJwWTSProcessList;
    {@exclude}
    FTerminalServerEventThread: TJwWTSEventThread;
    {@exclude}
    FServer: TJwString;
    {@exclude}
    FSystemUserName: TJwString;
    {@exclude}
    FTag: Integer;


    function CachedGetUserFromSessionId(const Sessionid: DWORD): TJwString;
    {@exclude}
    function GetIdleProcessName: TJwString;
    {@exclude}
    function GetPhysicalMemory: Int64;
    {@exclude}
    function GetServers: TStringList;
    {@exclude}
    function GetServer: TJwString;
    {@exclude}
    function GetSystemUsername: TJwString;
    {@exclude}
    function GetWinStationName(const SessionId: DWORD): TJwString;
    {@exclude}
    procedure OnEnumServersThreadTerminate(Sender: TObject);
    {@exclude}
    procedure SetServer(const Value: TJwString);
    {@exclude}
    procedure FireEvent(EventFlag: DWORD);
    {@exclude}
    procedure OnInternalProcessFound(const Sender: TJwTerminalServer;
      var Process: TJwWTSProcess; var Cancel: Boolean; Data: Pointer); virtual;
  published
  public

    {<B>Connect</B> sets up the connection with the Terminal Server specified in the
     Server property.
     The Connected property can be used to check if we're already connected.
     raises
 EJwsclWinCallFailedException:  will be raised if the connection
     attempt was unsuccessfull 
     
     Remarks
  EnumerateSessions and EnumerateProcesses will automatically
     connect to the Terminal Server when needed.
     
     Example:
     <code lang="Delphi">
     var
       ATerminalServer: TJwTerminalServer;
       i: Integer;
     begin
       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TjwTerminalServer.Create;

       // Set servername (only in case of remote server)
       ATerminalServer.Server := 'TS001';

       // Remember that EnumerateProcesses will automatically connect to the
       // Terminal Server for you. The connect function raises an Exception
       // if the connection attempt was unsuccessfull, so better use try..except
       try
         Connect
       except
         on E: EJwsclWinCallFailedException do
         begin
           // Handle Exception here
         end;
       end;

       // Free Memory
       ATerminalServer.Free;
     end;
     </code>
     }
    procedure Connect;

    {<B>Data</B> allows storage of a pointer to user specific data and can be freely
     used.
     
     Example:
     <code lang="Delphi">
     var
       ATerminalServer: TJwTerminalServer;
       s: AnsiString;
     begin

       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TJwTerminalServer.Create;

       s := 'Remember this text';

       // Store pointer in Data property
       ATerminalServer.Data := PAnsiChar(s);

       s := '';
       ...

       // and retreive it!
       s := ATerminalServer.Data;

       // Don't forget to free!
       ATerminalServer.Free;

     end;
     </code>
     }
    property Data: Pointer read FData write FData;

    {<B>Disconnect</B> will disconnect an existing connection to the Terminal Server.
     The Connected property can be used to check if we're already connected.
     
     Remarks
  If you disconnect you will not receive Session Events!
     
     Example:
     <code lang="Delphi">
     var
       ATerminalServer: TJwTerminalServer;
       i: Integer;
     begin
       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TjwTerminalServer.Create;

       // Set servername (only in case of remote server)
       ATerminalServer.Server := 'TS001';

       // Remember that EnumerateProcesses will automatically connect to the
       // Terminal Server for you. The connect function raises an Exception
       // if the connection attempt was unsuccessfull, so better use try..except
       try
         Connect
       except
         on E: EJwsclWinCallFailedException do
         begin
           // Handle Exception here
         end;
       end;

       ...

       // Disconnect
       ATerminalServer.Disconnect;

       // Free Memory (note that the free procedure will automatically
       // disconnect from the server if connected)
       ATerminalServer.Free;
     end;
    </code>
     }
    procedure Disconnect;

    {<B>ComputerName</B> returns the local computername.
     This property is convenient if you are connecting to Terminal Server
     locally or want to check if a servername = computername.
    }
    property ComputerName: TJwString read FComputerName;

    {<B>Connected</B> indicates if we are connected to the Terminal Server
    }
    property Connected: Boolean read FConnected;

    {The <B>Create</B> constructor creates a TJwTerminalServer instance and reservers
     memory for it.
     
     Example:
     <code lang="Delphi">
     var
       ATerminalServer: TJwTerminalServer;
       i: Integer;
     begin
       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TjwTerminalServer.Create;

       // Set servername (only in case of remote server)
       ATerminalServer.Server := 'TS001';

       // Remember that EnumerateProcesses will automatically connect to the
       // Terminal Server for you. The connect function raises an Exception
       // if the connection attempt was unsuccessfull, so better use try..except
       try
         if ATerminalServer.EnumerateProcesses then
         begin

           // Now loop through the list
           for i := 0 to ATerminalServer.Processes.Count - 1 do
           begin
             Memo1.Lines.Add(ATerminalServer.Processes[i].ProcessName);
           end;

         end;
       except
         on E: EJwsclWinCallFailedException do
         begin
           // Handle Exception here
         end;
       end;

       // Free Memory
       ATerminalServer.Free;
     end;
    </code>
    }
    constructor Create;
    {@exclude}
    destructor Destroy; override;

    {<B>EnumerateProcesses</B> enumerates all processes on the Terminal Server and fills the
     Processes property with a TJwProcessList. This list contains all processes
     and their properties such as Process Name, Process Id, Username, Memory
     Usage and so on.

      This method can only be used on Windows XP or newer. Windows 2000 Server (with Terminal Server)
    is also supported but not Windows 2000 Workstation.


     Example:
     <code lang="Delphi">
     var
       ATerminalServer: TJwTerminalServer;
       i: Integer;
     begin
       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TjwTerminalServer.Create;

       // Set servername (only in case of remote server)
       ATerminalServer.Server := 'TS001';

       // Remember that EnumerateProcesses will automatically connect to the
       // Terminal Server for you. The connect function raises an Exception
       // if the connection attempt was unsuccessfull, so better use try..except
       try
         if ATerminalServer.EnumerateProcesses then
         begin

           // Now loop through the list
           for i := 0 to ATerminalServer.Processes.Count - 1 do
           begin
             Memo1.Lines.Add(ATerminalServer.Processes[i].ProcessName);
           end;

         end;
       except
         on E: EJwsclWinCallFailedException do
         begin
           // Handle Exception here
         end;
       end;

       // Free Memory
       ATerminalServer.Free;
     end;
    </code>
	
    <B>Check for a nil property value Processes</B>

    }
    function EnumerateProcesses: Boolean; overload;

     {<B>EnumerateProcessesEx</B> behaves like EnumerateProcessesEx() but
    calls an event every time a process was found.
    This method can only be used on Windows XP or newer. Windows 2000 Server (with Terminal Server)
    is also supported but not Windows 2000 Workstation.

    @param OnProcessFound defines the callback method. Can be nil
    @param Data defines user defined data to be delivered to the callback method


    raises
     EJwsclEnumerateProcessFailed will be raised if an call to the terminal service failed.

    }
    function EnumerateProcesses(const OnProcessFound : TJwOnProcessFound;
        Data : Pointer) : Boolean; overload;

    {<B>EnumerateProcesses</B> enumerates all processes on the Terminal Server and fills the
     Processes property with a TJwProcessList. This list contains all processes
     and their properties such as Process Name, Process Id, Username, Memory
     Usage and so on.

      This method can only be used on Windows XP or newer. Windows 2000 Server (with Terminal Server)
    is also supported but not Windows 2000 Workstation.


     Example:
     <code lang="Delphi">
     var
       ATerminalServer: TJwTerminalServer;
       i: Integer;
     begin
       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TjwTerminalServer.Create;

       // Set servername (only in case of remote server)
       ATerminalServer.Server := 'TS001';

       // Remember that EnumerateProcesses will automatically connect to the
       // Terminal Server for you. The connect function raises an Exception
       // if the connection attempt was unsuccessfull, so better use try..except
       try
         ATerminalServer.EnumerateProcesses;

         // Now loop through the list
         for i := 0 to ATerminalServer.Processes.Count - 1 do
         begin
           Memo1.Lines.Add(ATerminalServer.Processes[i].ProcessName);
         end;

       except
         on E : EJwsclEnumerateProcessFailed do
         begin
         end;

         on E: EJwsclWinCallFailedException do
         begin
           // Handle Exception here
         end;
       end;

       // Free Memory
       ATerminalServer.Free;
     end;
    </code>
	
    <B>Check for a nil property value Processes</B>

    raises
      EJwsclEnumerateProcessFailed will be raised if an call to the terminal service failed.
    }
    procedure EnumerateProcessesEx;overload;

    {<B>EnumerateProcessesEx</B> behaves like EnumerateProcessesEx() but
    calls an event every time a process was found.
    This method can only be used on Windows XP or newer. Windows 2000 Server (with Terminal Server)
    is also supported but not Windows 2000 Workstation.

    @param OnProcessFound defines the callback method. Can be nil
    @param Data defines user defined data to be delivered to the callback method

    raises
      EJwsclEnumerateProcessFailed will be raised if an call to the terminal service failed.

    }
    procedure EnumerateProcessesEx(const OnProcessFound : TJwOnProcessFound;
       Data : Pointer); overload;


    {<B>EnumerateServers</B> enumerates all Terminal Servers in the specified domain.
	 The result will be stored in <B>readonly</B>  property Servers.
	 	 
	 
     @Param ADomain name of the Domain to be queried, if empty string is
     specified the current domain is queried
     @returns If the function fails you can use GetLastError to get extended
     error information 
     Remarks
  This functions enumerates all Terminal Servers that
     advertise themselves on the network. By default only Terminal Servers in
     Application Mode advertise themselves. You can override this behaviour by
     modifying the following registry key:
     <code lang="Delphi">
     HKLM\SYSTEM\CurrentControlSet\Control\Terminal Server
     "TSAdvertise" = REG_DWORD:1
     </code>
	 <B>Changing this value may be an security issue. Do not alter it without
	  the consent of the user!</B> 
	 <B>Only use the values 0 and 1. Do not use any other values!</B> 
	 
	 
     
     
     Please note that enumerating Terminal Servers in large environments might
     take some time (especially over slow WAN links). Therefore this function
     runs in a seperate thread and signals the OnServersEnumerated Event.
     The enumerated servers can be retreived by reading the Servers property.
     
     If the TJwTerminalServer Instance is destroyed and the enumeration thread
     is still busy, the TJwTerminalServer will wait max. 1 second for the thread
     to finish and then terminates it.
	 
	 <B>This method is not threadsafe! Do not call or use the instance from
	  several threads without locking mechanism</B>

	 <B>Check for a nil property value Servers</B> 
    }
    function EnumerateServers(const ADomain: TJwString):Boolean;

    {<B>EnumerateSessions</B> enumerates all sessions on the Terminal Server and fills the
     Sessions property with a TJwSessionList. This list contains all sessions
     and their properties such as Username, Session Id, Connection State, Idle
     Time and so on.
     @returns If the function fails you can use GetLastError to get extended
     error information 
     
     Example:
     <code lang="Delphi">
     var
       ATerminalServer: TJwTerminalServer;
       i: Integer;
     begin
       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TjwTerminalServer.Create;

       // Set servername (only in case of remote server)
       ATerminalServer.Server := 'TS001';

       // Remember that EnumerateSessions will automatically connect to the
       // Terminal Server for you. The connect function raises an Exception
       // if the connection attempt was unsuccessfull, so better use try..except
       try
         if ATerminalServer.EnumerateSessions then
         begin

           // Now loop through the list
           for i := 0 to ATerminalServer.Sessions.Count - 1 do
           begin
             Memo1.Lines.Add(ATerminalServer.Sessions[i].Username);
           end;

         end;
       except
         on E: EJwsclWinCallFailedException do
         begin
           // Handle Exception here
         end;
       end;

       // Free Memory
       ATerminalServer.Free;
     end;
    </code>

	 <B>Check for a nil property value Sessions</B>
    }
    function EnumerateSessions: boolean;

    {<B>FileTime2DateTime</B> can be used to convert a non local (GMT time) FileTime to a
     localised TDateTime var.
     @param FileTime TFileTime in GMT
     @returns TDateTime in local time

     Remarks
  A TFileTime can be casted to Int64 (number 100-nanosecond
     intervals since January 1, 1601) and vice versa.
     }
    class function FileTime2DateTime(FileTime: TFileTime): TDateTime;
    {@exclude}
    property IdleProcessName: TJwString read GetIdleProcessName;

    {The <B>LastEventFlag</B> property can be used to see the Last Session Event that occured.
     This is usefull if you are listening on multiple event types.
     @seealso(OnSessionEvent)
    }
    property LastEventFlag: DWORD read FLastEventFlag;

    {The <B>OnServersEnumerated</B> event signals that the Server Enumeration thread has finished.
     The Enumerated Servers can be read through the Servers property.
     
     Example:
     <code lang="Delphi">
     procedure TMainForm.OnEnumerateServersDone(Sender: TObject);
     var
       i: Integer;
       TerminalServer: TJwTerminalServer;
      begin

        // Cast Sender to TJwTerminalServer
        TerminalServer := (Sender as TJwTerminalServer);

        // Loop through the enumerated Terminal Servers, if no servers were
        // found the count is 0.
        for i := 0 to TerminalServer.Servers.Count-1 do
        begin
          Memo1.Lines.Add(TerminalServer.Servers[i]);
        end;

        // Don't free TerminalServer var here!
      end;
     </code>
    }
    property OnServersEnumerated: TNotifyEvent read FOnServersEnumerated write FOnServersEnumerated;

    {The <B>OnSessionEvent</B> is a generic event which is fired if anything happens that is
     session related, like statechange, logon/logoff, disconnect and (re)connect.
     
     The table below shows which Terminal Server event triggers which event:
     <image TJwWTSEvents-Table>
    }
    property OnSessionEvent: TNotifyEvent read FOnSessionEvent write FOnSessionEvent;

    {The <B>OnSessionConnect</B> event is fired when a client connects to a session
     @seealso(OnSessionEvent Overview of which events are triggered and when)
    }
    property OnSessionConnect: TNotifyEvent read FOnSessionConnect write FOnSessionConnect;

    {The <B>OnSessionCreate</B> event is fired when a session is created
     @seealso(OnSessionEvent Overview of which events are triggered and when)
    }
    property OnSessionCreate: TNotifyEvent read FOnSessionCreate write FOnSessionCreate;

    {The <B>OnSessionDelete</B> event is fired when a session is deleted
     @seealso(OnSessionEvent Overview of which events are triggered and when)
    }
    property OnSessionDelete: TNotifyEvent read FOnSessionDelete write FOnSessionDelete;

    {The <B>OnSessionDisconnect</B> event is fired when a session is disconnected
     @seealso(OnSessionEvent Overview of which events are triggered and when)
    }
    property OnSessionDisconnect: TNotifyEvent read FOnSessionDisconnect write FOnSessionDisconnect;

    {The <B>OnLicenseStateChange</B> event is fired when when a license is added or deleted using
     License Manager.
     @seealso(OnSessionEvent Overview of which events are triggered and when)
    }
    property OnLicenseStateChange: TNotifyEvent read FOnLicenseStateChange write FOnLicenseStateChange;

    {The <B>OnSessionLogon</B> event is fired when a client logs on either through the console
     or a session
     @seealso(OnSessionEvent Overview of which events are triggered and when)
    }
    property OnSessionLogon: TNotifyEvent read FOnSessionLogon write FOnSessionLogon;

    {The <B>OnSessionLogoff</B> event is fired when a client logs off either from the console
     or a session
     @seealso(OnSessionEvent Overview of which events are triggered and when)
    }
    property OnSessionLogoff: TNotifyEvent read FOnSessionLogoff write FOnSessionLogoff;

    {The <B>OnWinStationRename</B> event is fired when an existing session has been renamed
     @seealso(OnSessionEvent Overview of which events are triggered and when)
    }
    property OnWinStationRename: TNotifyEvent read FOnWinStationRename write FOnWinStationRename;

    {The <B>OnSessionStateChange</B> event is fired when the connectstate of a session has changed
     @seealso(OnSessionEvent Overview of which events are triggered and when)
    }
    property OnSessionStateChange: TNotifyEvent read FOnSessionStateChange write FOnSessionStateChange;

    property Processors: Integer read FProcessors;
    {<B>Processes</B> contains a TJwWTSProcessList of which each item contains a
     TJwWTSProcess. This processlist contains all enumerated processes
     and their properties such as Process Name, Process Id, Username, Memory
     Usage and so on.

     Remarks
  The Processlist is filled by calling the EnumerateProcesses
     function.
    }
    property Processes: TJwWTSProcessList read FProcesses write FProcesses;

    property PhysicalMemory: Int64 read GetPhysicalMemory write FPhysicalMemory;
    {<B>Server</B> the netbios name of the Terminal Server.

     Remarks
  If you want to connect to a Terminal Server locally
     you should not specify the server name. Please note that in the case of a
     local connection this property <B>will return the computername</B> )

     Note that Windows XP SP 2 by default does not allow remote
     RPC connection to Terminal Server (enumerating sessions and processes).
     You can change this behaviour by creating the following registry entry
     on the XP machine:

     <code lang="Delphi">
     HKLM\SYSTEM\CurrentControlSet\Control\Terminal Server
     "AllowRemoteRPC" = REG_DWORD:1
     </code>

     Also make sure that the Windows firewall on the client is configured to
     allow File and Print sharing, as well as Remote Desktop.
    }
    property Server: TJwString read GetServer write SetServer;
    {@exclude}
    property ServerHandle: THandle read FServerHandle;

    {<B>Servers</B> contains the list of Enumerated Terminal Servers. 
	 <B>May be nil!</B> 
     @seealso(EnumerateServers)
     }
    property Servers: TStringList read GetServers;

    {<B>Sessions</B> contains a TJwWTSSessionList of which each item contains a
     TJwWTSSession. This sessionlist contains all enumerated sessions
     and their properties such as Username, Connection State, Idle Time and so
     on.
     
     Remarks
  The Sessionlist is filled by calling the EnumerateSessions
     function.
    }
    property Sessions: TJwWTSSessionList read FSessions write FSessions;

    { <b>Shutdown</b> shuts down (and optionally restarts) the specified terminal
      server.
      Parameters
      AShutdownFlag :  A value defined in the Remarks section.
      Exceptions
      EJwsclWinCallFailedException :  will be raised if the call fails.
      Remarks
      The following values can be used with the aShutdownFlag parameter.
      <table 30c%>
      Value                Meaning
      -------------------  -------------------------------------------------------------------
      WTS_WSD_LOGOFF       Forces all client sessions to log off (except the session calling
                            WTSShutdownSystem) and disables any subsequent remote logons. This
                            can be used as a preliminary step before shutting down. Logons
                            will be re-enabled when the terminal services service is
                            restarted. Use this value only on the Terminal Services console.
      WTS_WSD_POWEROFF     Shuts down the system on the terminal server and, on computers
                            that support software control of AC power, turns off the power.
                            This is equivalent to calling ExitWindowsEx with EWX_SHUTDOWN and
                            EWX_POWEROFF. The calling process must have the SE_SHUTDOWN_NAME
                            privilege enabled.
      WTS_WSD_REBOOT       Shuts down and then restarts the system on the terminal server.
                            This is equivalent to calling ExitWindowsEx with EWX_REBOOT. The
                            calling process must have the SE_SHUTDOWN_NAME privilege enabled.
      WTS_WSD_SHUTDOWN     Shuts down the system on the terminal server. This is equivalent
                            to calling the ExitWindowsEx function with EWX_SHUTDOWN. The
                            calling process must have the SE_SHUTDOWN_NAME privilege enabled.
      WTS_WSD_FASTREBOOT   This value is not supported currently.
      </table>                                                                                 }
    procedure Shutdown(AShutdownFlag: DWORD);

    property Statistics: TJwWTSSessionStatistics read GetSessionStatistics;

    {<B>SystemUserName</B> returns the (localised) name of the system user}
    property SystemUserName: TJwString read GetSystemUserName;

    {<B>Tag</B> has no predefined meaning. The Tag property is provided for the
     convenience of developers. It can be used for storing an additional integer
     value or it can be typecast to any 32-bit value such as a component
     reference or a pointer.
    }
    property Tag: Integer read FTag write FTag;
  end;

  {<B>PJwTerminalServerList</B> is a pointer to a TJwTerminalServerList}
  PJwTerminalServerList = ^TJwTerminalServerList;
  {<B>TJwTerminalServerList</B> is a List of TJwTerminalServer Objects. 

   Each item in the list points to a TJwTerminalServer object that can be queried
   and manipulated.
   The list can be filled by adding TJwTerminalServer instances.
   Example:
   <code lang="Delphi">
   var
     ATerminalServerList : TjwTerminalServerList;
     ATerminalServer : TJwTerminalServer;
   begin
     ATerminalServerList := TjwTerminalServerList.Create;

     // Create a Terminal Server instance
     ATerminalServer := TJwTerminalServer.Create;

     // and add it to the list
     ATerminalServerList.Add(ATerminalServer);

     // Freeing the TerminalServerList will also free the Terminal Server
     // instances it owns.
     ATerminalServerList.Free;
   end;
   </code>
  }
  TJwTerminalServerList = class(TObjectList)
  protected
    {@exclude}
    FOwner: TComponent;
  protected
    {@exclude}
    function GetItem(Index: Integer): TJwTerminalServer;
    {@exclude}
    procedure SetItem(Index: Integer; ATerminalServer: TJwTerminalServer);
    {@exclude}
    procedure SetOwner(const Value: TComponent);
  public
    {@exclude}
    destructor Destroy; reintroduce;

    {<B>Add</B> adds a TerminalServer to the end of the TerminalServerList
     @returns returns the index of the inserted object. 
    }
    function Add(ATerminalServer: TJwTerminalServer): Integer;
    {<B>FindByServer</B> looks up a Terminal Server in the List by Servername
     @Param ServerName The Servername which is to be found 
     @Param IgnoreCase Default = True 
     @Returns If the server was found a TJwTerminalServer instance is returned.
     Always check if it's not nil! 
    }
    function FindByServer(const ServerName: WideString;
      const IgnoreCase: boolean = False): TJwTerminalServer;

    {@Returns the index of the TerminalServer object in the TerminalServerList. 
    }
    function IndexOf(ATerminalServer: TJwTerminalServer): Integer;

    {<B>Insert</B> adds a TerminalServer to the end of the TerminalServerList
     @returns returns the index of the inserted object. 
    }
    procedure Insert(Index: Integer; ATerminalServer: TJwTerminalServer);

    {The <B>Items[Index</B> property can be used to access the TerminalServer instances that
     are held by the list.
    }
    property Items[Index: Integer]: TJwTerminalServer read GetItem write SetItem; default;
    {A @ClassName can be owned by any component eg a TApplication or TForm
    }
    property Owner: TComponent read FOwner write SetOwner;

    {<B>Remove</B> removes the specified Terminal Server  from the TerminalServerList
     and if OwnsObjects is true (default) frees the TerminalServer.
     @returns The value returned is the index of the object in the Items array
     before it was removed. If the specified object is not found on the list,
     Remove returns –1. 
    }
    function Remove(ATerminalServer: TJwTerminalServer): Integer;
  end;

   {The <B>TJwWTSEventThread</B> Thread waits for Terminal Server Events and notifies the
    caller by firing Events. 

    It's not necessary to manually create an <B>TJwWTSEventThread</B> Thread because
    TJwTerminalServer does this automatically after a successfull call to the
    EnumerateSessions function.
    
    <B>TJwWTSEventThread</B> is Owned by a TJwTerminalServer instance
    
    Remarks
  <B>TJwWTSEventThread</B> uses the WTSWaitSystemEvent API Call which can hang
    on Windows Vista after sending a WTS_FLUSH event. The bug was first
    corrected in winsta.dll version 6.0.6000.20664.
    
    See also: http://www.remkoweijnen.nl/blog/2008/01/25/using-wtswaitsystemevent/
  }
  TJwWTSEventThread = class(TJwThread)
  protected
    {@exclude}
    FOwner: TJwTerminalServer;
    {@exclude}
    FEventFlag: DWORD;
    {@exclude}
    procedure DispatchEvent;
  public

    {Call <B>Create</B> to create a @classname Thread.
     @Param CreateSuspended If CreateSuspended is False, Execute is called
     immediately. If CreateSuspended is True, Execute won't be called until
     after Resume is called.
     @Param Owner Specifies the TJwTerminalServer instance that owns the thread 
    }
    constructor Create(CreateSuspended: Boolean; AOwner: TJwTerminalServer);

    {raises
 EJwsclWinCallFailedException:  will be raised if WTSWaitSystemEvent
     failed. 
    }
    procedure Execute; override;
  end;

  {<B>TJwWTSEnumServersThread</B> is a Thread that enumerates all Terminal Server in the
   specified domain. 

   The Enumeration is done from a thread because it can take some time to
   enumerate all server, especially over a slow WAN connection.
   
   The thread is created by calling the EnumerateServers procedure from a
   TJwTerminalServer instance. Although allowed you normally don't create
   a TJwWTSEnumServersThread manually.
   
   Enumerated servers are returned by firing the OnServerEnumerated Event
   from the parent TJwTerminalServer instance.
   
   A TJwWTSEnumServersThread is owned by a TJwTerminalServer instance.
   }
  TJwWTSEnumServersThread = class(TJwThread)
  protected
    {@exclude}
    FDomain: TJwString;
    {@exclude}
    FOwner: TJwTerminalServer;
    {@exclude}
    FServer: TJwString;
    {@exclude}
    FTerminatedEvent: THandle;
    {@exclude}
    procedure AddToServerList;
    {@exclude}
    procedure ClearServerList;
    {@exclude}
    procedure DispatchEvent;
  public
    {Call <B>Create</B> to create a @classname Thread.
     @Param CreateSuspended If CreateSuspended is False, Execute is called
     immediately. If CreateSuspended is True, Execute won't be called until
     after Resume is called. 
     @Param Owner Specifies the TJwTerminalServer instance that owns the thread 
     @Param Domain Specifies the Domain that should be Enumerated. if you want
     to Enumerate the current domain (from a domain member) you can specify an
     empty string 
    }
    constructor Create(CreateSuspended: Boolean; Owner: TJwTerminalServer;
      Domain: TJwString);
    procedure Execute; override;
  end;

  {<B>PJwWTSSession</B> is a pointer to a TJwWTSSession}
  PJwWTSSession = ^TJwWTSSession;

  {<B>TJwWTSSession</B> is a Class that encapsulates a Terminal Server session and
   it's properties 

   A session is uniquely identified with a SessionID, this is a number
   between 0 and 65535.
   
   A TJwWTSSession is owned by a JTwWTSSessionList.
   }
  TJwWTSSession = class(TObjectList)
  protected
    {@exclude}
    FApplicationName: TJwString;
    {@exclude}
    FClientAddress: TJwString;
    {@exclude}
    FClientBuildNumber: DWORD;
    {@exclude}
    FColorDepth: DWORD;
    {@exclude}
    FClientDirectory: TJwString;
    {@exclude}
    FClientHardwareId: DWORD;
    {@exclude}
    FClientName: TJwString;
    {@exclude}
    FClientProductId: WORD;
    {@exclude}
    FClientProtocolType: WORD;
    {@exclude}
    FClientProtocolStr: TJwString;
    {@exclude}
    FCompressionRatio: TJwString;
    {@exclude}
    FConnectState: TWtsConnectStateClass;
    {@exclude}
    FConnectStateStr: TJwString;
    {@exclude}
    FConnectTime: TDateTime;
    {@exclude}
    FCurrentTime: TDateTime;
    {@exclude}
    FCurrentTimeEx: Int64;
    {@exclude}
    FData: Integer;
    {@exclude}
    FDisconnectTime: TDateTime;
    {@exclude}
    FDomain: TJwString;
    {@exclude}
    FIdleTime: Int64;
    {@exclude}
    FIdleTimeStr: TJwString;
    {@exclude}
    FIncomingBytes: DWORD;
    {@exclude}
    FIncomingCompressedBytes: DWORD;
    {@exclude}
    FIncomingFrames: DWORD;
    {@exclude}
    FHorizontalResolution: DWORD;
    {@exclude}
    FInitialProgram: TJwString;
    {@exclude}
    FLastInputTime: TDateTime;
    {@exclude}
    FLogonTime: Int64;
    {@exclude}
    FLogonTimeStr: TJwString;
    {@exclude}
    FOwner: TJwWTSSessionList;
    {@exclude}
    FOutgoingBytes: DWORD;
    {@exclude}
    FOutgoingCompressBytes: DWORD;
    {@exclude}
    FOutgoingFrames: DWORD;
    {@exclude}
    FProtocolTypeStr: TJwString;
    {@exclude}
    FRemoteAddress: TJwString;
    {@exclude}
    FRemotePort: WORD;
    {@exclude}
    FSessionId: TJwSessionId;
    {@exclude}
    FUsername: TJwString;
    {@exclude}
    FVerticalResolution: DWORD;
    {@exclude}
    FWdFlag: DWORD;
    {@exclude}
    FWdName: TJwString;
    {@exclude}
    FWinStationName: TJwString;
    {@exclude}
    FWorkingDirectory: TJwString;
    {@exclude}
    FShadow : TJwWTSSessionShadow;
    {@exclude}

    {@exclude}
    FToken : TJwSecurityToken;
    {@exclude}
    FUserSid : TJwSecurityID;

    {@exclude}
    procedure GetClientDisplay;
    {@exclude}
    function GetServer: TJwString;
    {@exclude}
    function GetSessionInfoDWORD(const WTSInfoClass: WTS_INFO_CLASS): DWORD;
    {@exclude}
    procedure GetSessionInfoPtr(const WTSInfoClass: WTS_INFO_CLASS;
      var ABuffer: Pointer);
    {@exclude}
    function GetSessionInfoStr(const WTSInfoClass: WTS_INFO_CLASS): TJwString;
    {@exclude}
    procedure GetWinStationInformation;
    {@exclude}
    procedure GetWinStationDriver;

    {@exclude}
    function GetToken: TJwSecurityToken;
    {@exclude}
    function GetUserSid: TJwSecurityID;
  private
  published
    {The <B>Create</B> constructor creates a TJwWTSSession instance and allocates memory for it
     @Param Owner Specifies the TJwTerminalServer instance that owns the session
     @Param SessionId The Session Identifier
     @Param WinStationName The Session Name
     @Param ConnectState The current connection state of the session

     Remarks
  It's not necessary to manually create a session instance.
     Enumerating sessions with the EnumerateSessions function will create a
     SessionList filled with Sessions.
     @seealso(TJwTerminalServer.EnumerateSessions)
    }
    constructor Create(const Owner: TJwWTSSessionList;
      const SessionId: TJwSessionId; const WinStationName: TJwString;
      const ConnectState: TWtsConnectStateClass);

    {The <B>Destroy</B> destructor disposes the Session object.

     Remarks
  Since a session is Owned by a SessionList by default
     <B>you should not destroy/free a session manually</B> . The only scenario
     where a sessions would need to be freed is when you manually create a
     sessionlist and specify False for the OwnsObject parameter.
    }
    destructor Destroy; override;

    {<B>ApplicationName</B> returns the the startup application as specified in the
     Terminal Server client. If no startup application was specified
     an empty string is returned.

     Remarks
  Console sessions always returns empty value.
     }
    property ApplicationName: TJwString read FApplicationName;

    {<B>ClientAddress</B> returns the Client IP Address as string. This is the local IP
     address of a client as reported by the Terminal Server Client

     Remarks
  Console sessions always returns empty value.
     }
    property ClientAddress: TJwString read FClientAddress;

    {<B>ClientBuildNumber</B> returns the version number of the Terminal Server Client
     
     Remarks
  Console sessions always returns empty value.
     See Also
     * RemoteAddress
     * RemotePort
     }
     property ClientBuildNumber: DWORD read FClientBuildNumber write FClientBuildNumber;

    {<B>ClientDirectory</B> returns the version number of the Terminal Server Client
     
     Remarks
  Console sessions always returns empty value.
     }
    property ClientDirectory: TJwString read FClientDirectory;

    {<B>ClientHardwareId</B> returns a client-specific hardware identifier
     
     Remarks
  Console sessions always returns empty value.
     }
    property ClientHardwareId: DWORD read FClientHardwareId;

    {<B>ClientName</B> returns the local computer name of the client
     
     Remarks
  Console sessions always returns empty value.
     }
    property ClientName: TJwString read FClientName;

    {<B>ClientProductId</B> returns a client-specific product identifier.
     
     Remarks
  Console sessions always returns empty value.
     }
    property ClientProductId: WORD read FClientProductId;

    { <b>ClientProtocolType</b> returns a value that indicates the protocol type This
      is one of the following values:
      <table 33c%>
      ClientProtocolType          Meaning
      --------------------------  --------------------
      WTS_PROTOCOL_TYPE_CONSOLE   The Console session
      WTS_PROTOCOL_TYPE_ICA       The ICA protocol
      WTS_PROTOCOL_TYPE_RDP       The RDP protocol
      </table>
      
      See Also
      * ClientProtocolStr
      * RemoteAddress
      * RemotePort	
      \ \                                                                             }
    property ClientProtocolType: WORD read FClientProtocolType;

    {<B>ClientProtocolStr</B> returns a string  that indicates the protocol type
     This is one of the following values:
     <table 33c%>
      ClientProtocolType         Value
      ----------------------     -----------------
      WTS_PROTOCOL_TYPE_CONSOLE  Console
      WTS_PROTOCOL_TYPE_ICA      ICA
      WTS_PROTOCOL_TYPE_RDP      RDP
     </table> 
     
    See Also
    * ClientProtocolType
    * RemoteAddress
    * RemotePort
      }
    property ClientProtocolStr: TJwString read FClientProtocolStr;

    { <b>ColorDepth</b> returns the number of colors used in the terminal session. }
    property ColorDepth: DWORD read FColorDepth;

    {<B>CompressionRatio</B> returns the current compression ratio as string with 2 decimals.
     Compression Ratio equals OutgoingCompressBytes / OutgoingBytescompressed
     Console sessions always returns empty value.
     
     See Also
     * IncomingBytes
     * OutgoingBytes
    }
    property CompressionRatio: TJwString read FCompressionRatio;

    {The <B>Connect</B> function allows you to connect to another Terminal Server session.
     Remarks
  You can always connect to a session in which you are logged
     on with the same user account. To connect to another user's session, you
     must have either Full Control or User Access permission.
     
     You can connect to another session only from within an existing session.
     You must use the <B>Connect</B> function from within a session to be able to connect
     to another session.
     
     You can connect to a session only if it is in either an active or
     disconnected state.

     You cannot connect to another session from the console session.
     
     Note that if you connect to another session your existing session will be
     disconnected.
    }
    function Connect(const Password: WideString): Boolean;
    {<B>ConnectState</B> returns the connection state of the session. Which can be one of the
     following values:
     <table 33c%>
      Session State       Description
      ------------------  ------------------------------
      WTSActive           The session is connected, and a user is logged on to the server.
      WTSConnected        The session is connected, but there is no user logged on to the server.
      WTSConnectQuery     The session is in the process of connecting. If this state continues, it indicates a problem with the connection.
      WTSShadow           The session is in the process of remotely controlling another session.
      WTSDisconnected     The user is disconnected from the session, but the session is still attached to the server and can be reconnected at any time.
      WTSIdle             The session is initialized and ready to accept a connection. To optimize the performance of a server, two default (idle) sessions are initialized before any client connections are made.
      WTSReset            The session failed to initialize correctly or could not be terminated, and is not available. If this state continues, it indicates a problem with the connection of the session.
      WTSInit             The session is in the process of initializing.
     </table> 
     
     Remarks
  On Windows XP, however, the state for session 0 can be
     misleading because it will be WTSDisconnected even if there is no user
     logged on. To accurately determine if a user has logged on to session 0,
     you can use the Username property
    
    http://support.microsoft.com/kb/307642/en-us
    }
    property ConnectState: TWtsConnectStateClass read FConnectState;
    {<B>ConnectStateStr</B> returns a localised connection state string.
     }
    property ConnectStateStr: TJwString read FConnectStateStr;

    {<B>ConnectTime</B> The most recent client connection time.
     }
    property ConnectTime: TDateTime read FConnectTime;

    {<B>CurrentTime</B> the time that the TJwWTSSession info was queried. This can be
     used to calculate time differences such as idle time
     }
    property CurrentTime: TDateTime read FCurrentTime;

    property CurrentTimeEx: Int64 read FCurrentTimeEx;

    {<B>Data</B> allows storage of a pointer to user specific data and can be freely
     used.}
    property Data: Integer read FData write FData;

    {The <B>Disconnect</B> function disconnects the logged-on user from the specified
     Terminal Services session without closing the session. The session remains
     attached to the terminal server in the disconnected state and currently
     running applications continue to run. When you attempt to reconnect to the
     <B> same server</B> , you are reconnected to the same session from which you
     disconnected, even if you are reconnecting from a different computer.
     Applications that were left open when you disconnected remain running when
     you reconnect to the session, with no loss of data.
     In NLB (Network Load Balancing) environments the Session Directory
     (starting from Server 2008 this is called TS Session Broker) care of
     redirecting a user to the server where he has a disconnected session.
     @param bWait Indicates whether the operation is synchronous. Specify TRUE
     to wait for the operation to complete, or FALSE to return immediately.

     raises
 EJwsclWinCallFailedException:  will be raised if the call fails.
     }
    procedure Disconnect(bWait: Boolean);

    {<B>DisconnectTime</B> the last client disconnection time.
    }
    property DisconnectTime: TDateTime read FDisconnectTime;
    {<B>Domain</B> the domain of the logged-on user
    }
    property Domain: TJwString read FDomain;

    {@exclude}
    function GetClientAddress: TJwString;

    {@exclude}
    function GetServerHandle: THandle;
    { <b>HorizontalResolution</b> returns the width resolution of the terminal session
      display in pixels.                                                               }
    property HorizontalResolution: DWORD read FHorizontalResolution;

    {<B>IdleTime</B> the elapsed time (relative to CurrentTime) since last user input in
     the session expressed in the number of 100-nanosecond intervals since
     January 1, 1601 (TFileTime).
     
     Remarks
  Please note the following remarks about Idle Time:
     A disconnected session is Idle since DisconnectTime. A session without a
     user is never idle, usually these are special sessions like Listener,
     Services or console session.
     IdleTimeStr returns a convenient formatted idle time string
     which can be used for displaying. This value is more convenient however for
     calculations such as sorting or comparing idle times.
     See Also 
     * IdleTimeStr
    }
    property IdleTime: Int64 read FIdleTime;

    {<B>IdleTimeStr</B> the elapsed time (relative to CurrentTime) since last user input in
     the session as formatted string. The string is formatted according to the
     table below:
     <table 30c%>
     days   hours   minutes    value
     -----  ------  ---------  ---------
     \> 0    any     any        +d+hh:mm
     0      \> 0     any        hh:mm
     0      0       any        mm
     0      0       0          .
     </table> 
     
     Remarks
  Please note the following remarks about Idle Time:
     A disconnected session is Idle since DisconnectTime. A session without a
     user is never idle, usually these are special sessions like Listener,
     Services or console session.
     See Also
     * IdleTimeStr
     * CurrentTime
    }
    property IdleTimeStr: TJwString read FIdleTimeStr;

    {<B>IncomingBytes</B> uncompressed Remote Desktop Protocol (RDP) data from the client
     to the server.
     Remarks
  This value is not returned for console sessions.
     
     See Also
     * OutgoingBytes
     * CompressionRatio
    }
    property IncomingBytes: DWORD read FIncomingBytes;

    {<B>InitialProgram</B> string containing the name of the initial program that
     Terminal Services runs when the user logs on.
    }
    property InitialProgram: TJwString read FInitialProgram;

    {<B>LastInputTime</B> the time of the last user input in the session.
    }
    property LastInputTime: TDateTime read FLastInputTime;

    {The <B>Logoff</B> function logs off a specified Terminal Services session
     @param bWait Indicates whether the operation is synchronous. Specify TRUE
     to wait for the operation to complete, or FALSE to return immediately. 

     raises
 EJwsclWinCallFailedException:  will be raised if the call fails. 
     }
    procedure Logoff(bWait: Boolean);

    {<B>LogonTime</B> the time that the user logged on to the session in the number
     of 100-nanosecond intervals since January 1, 1601 (TFileTime).
     @seealso(LogonTimeStr)
    }
    property LogonTime: Int64 read FLogonTime;

    {<B>LogonTimeStr</B> the time that the user logged on to the session as a localised
     Date Time string.
     @seealso(LogonTime)
    }
    property LogonTimeStr: TJwString read FLogonTimeStr;

    {<B>Owner</B> of this session object, which can only be a TJwWTSSessionList
    }
    property Owner: TJwWTSSessionList read FOwner write FOwner;
    {<B>OutgoingBytes</B> uncompressed RDP data from the server to the client.

     Remarks
  This value is not returned for console sessions.
     @seealso(IncomingBytes)
     @seealso(CompressionRatio)
    }
    property OutgoingBytes: DWORD read FOutgoingBytes;

    {The <B>PostMessage</B> function displays a message box on the client desktop.
     @param AMessage: string that contains the message to be displayed 
     @param ACaption: string that contains the dialog box title. 
     @param uType: Specifies the contents and behavior of the message box.
     This value is typically MB_OK. For a complete list of values, see the
     uType parameter of the MessageBox function. 
     
     @returns If the function fails you can use GetLastError to get extended
     error information 

     Remarks
  PostMessage does not wait for the user to respond.
     See Also
      * SendMessage
    }
    function PostMessage(const AMessage: TJwString; const ACaption: TJwString;
      const uType: DWORD): DWORD;

    {@exclude}
    function ProtocolTypeToStr(const AProtocolType: DWORD): TJwString;

    {<B>RemoteAddress</B> returns the real IP Address that is connected to the Terminal Server.
     
     Remarks
  <B>RemoteAddress</B> returns the IP address that is currently connected
     to the Terminal Server (as opposed to ClientAddress which returns the
     address as reported by the client which is usually just it's local ip
     address).
     <B>RemoteAddress</B> is the same adddress you will see when you examine netstat output
     <code lang="Delphi">
     C:\Documents and Settings\Remko>netstat -n | find /i "3389"
     TCP    192.168.2.2:3389       192.168.2.3:4096       ESTABLISHED
     </code>
     In the output above, 192.168.2.2 is the IP Address of the Terminal Server
     which listens on port 3389. It has currently one Session from Remote IP
     192.168.2.3 on TCP port 4096. The <B>RemoteAddress</B> property is usefull because netstat
     cannot relate a connection to a Session Id.
     If you want to convert the to IP Address to a sockaddr structure you can
     use the WSAStringToAddress API.
     @seealso(RemotePort)
    }
    property RemoteAddress: TJwString read FRemoteAddress;

    {<B>RemotePort</B> returns the Remote Port number which is is connected to the
     Terminal Server. The Terminal Server listens (by default) on port 3389
     but the client connects with a random available port.
     <B>RemotePort</B> is the same port number you will see when you examine netstat output
     <code lang="Delphi">
     C:\Documents and Settings\Remko>netstat -n | find /i "3389"
     TCP    192.168.2.2:3389       192.168.2.3:4096       ESTABLISHED
     </code>
     In the output above, 192.168.2.2 is the IP Address of the Terminal Server
     which listens on port 3389. It has currently one Session from Remote IP
     192.168.2.3 on TCP port 4096. The RemoteAddress and <B>RemotePort</B> properties are
     usefull because netstat cannot relate a connection to a Session Id.
     @seealso(RemoteAddress)
    }
    property RemotePort: WORD read FRemotePort;

    {The <B>SendMessage</B> function displays a message box on the client desktop.
     @param AMessage string that contains the message to be displayed. 
     @param ACaption string that contains the dialog box title. 
     @param uType Specifies the contents and behavior of the message box.
     This value is typically MB_OK. For a complete list of values, see the
     uType parameter of the MessageBox function. 
     @param ATimeOut Specifies the time, in seconds, that the SendMessage
     function waits for the user's response. If the user does not respond within
     the time-out interval, the pResponse parameter returns IDTIMEOUT.
     If the Timeout parameter is zero, WTSSendMessage will wait indefinitely
     for the user to respond. 
     @return <B>SendMessage</B> returns the user's response, which can be one of the
     following values:
     <table 33c%>
      Value        Meaning
      -----------  ------------------------------------------
      IDABORT      Abort button was selected.
      IDCANCEL     Cancel button was selected.
      IDIGNORE)    Ignore button was selected.
      IDNO         No button was selected.
      IDRETRY      Retry button was selected.
      IDYES        Yes button was selected.
      IDASYNC      The bWait parameter was FALSE, so the function returned without waiting for a response.
      IDTIMEOUT    The bWait parameter was TRUE and the time-out interval elapsed.
     </table>
     
     @returns If the function fails you can use GetLastError to get extended
     error information 

     Remarks  If you don't need to wait for the user's response you can
     use the PostMessage function
     See Also
     * PostMessage
    }
    function SendMessage(const AMessage: TJwString; const ACaption: TJwString;
      const uType: DWORD; const ATimeOut: DWORD): DWORD;

    class function SendLocalMessage(const SessionID : TJwSessionId; const AMessage: TJwString;
  const ACaption: TJwString; const uType: DWORD; const ATimeOut: DWORD): DWORD;


    {<B>Server</B> the netbios name of the Terminal Server.

     Remarks
  If you want to connect to a Terminal Server locally
     you should not specify the server name. Please note that in the case of a
     local connection this property <B>will return the computername</B> )
    }
    property Server: TJwString read GetServer;

    {<B>SessionId</B> the session identifier

     There are some reserved SessionId's that serve a special purpose. The
     following table lists the reserved SessionId's:

     <table 33c%>
      Value     Meaning
      --------  -----------------
      0         Console or Services session, see remarks
      65536     RDP Listener
      65537     ICA Listener
     </table> 
      
      Remarks
 
      <B>Console Sessions</B> 
      The system console session is usually identified as session 0 in the
      Session list when you connect to a terminal server. A console session is
      defined as the session you connect to at the physical console of the
      remote computer, as though you were logging on locally instead of
      remotely. You can send a message to the console session, but you cannot
      perform any of the other administrative actions on it.
      In Windows XP, Microsoft Windows Server 2003, and earlier versions of the
      Windows operating system, all services run in the same session as the
      first user who logs on to the console. This session is called Session 0.
      Running services and user applications together in Session 0 poses a
      security risk because services run at elevated privilege and therefore are
      targets for malicious agents who are looking for a way to elevate their
      own privilege level. The Vista operating system mitigates this security
      risk by isolating services in Session 0 and making Session 0
      noninteractive. In Windows Vista, only system processes and services run
      in Session 0. The first user logs on to Session 1, and subsequent users
      log on to subsequent sessions. This means that services never run in the
      same session as users’ applications and are therefore protected from
      attacks that originate in application code.
      
      <B>Listener Sessions</B> 
      Listener sessions are different from regular sessions.
      These sessions listen for and accept new Remote Desktop Protocol (RDP)
      client connections, thereby creating new sessions for the client requests.
      If you have configured more than one connection in Terminal Services
      Configuration, several listener sessions are available.
      You have the option to reset a listener session. However, this is not
      recommended, because doing so <B>resets all sessions that use the same
      Terminal Services connection.</B>  Resetting a user's session without warning
      can result in loss of data at the client.

      <B>Idle sessions</B> 
      To optimize the performance of a terminal server, idle sessions are
      initialized by the server before client connections are made.
      These sessions are available to clients for connection.
      Two idle sessions are created by default. User sessions can also be in
      idle state.
    }
    property SessionId: TJwSessionId read FSessionId;

    {The <B>Shadow</B> function starts the remote control of another Terminal Services
     session. You must call this function from a remote session.
     @Param Hotkey The virtual-key code that represents the key to press to
     stop remote control of the session. The key that is defined in this
     parameter is used with the HotkeyModifiers parameter. Default is VK_MULTIPY
     (* on the numeric keypad). 
     @Param HKModifier The virtual modifier that represents the key to
     press to stop remote control of the session. The virtual modifier is used
     with the Hotkey parameter. The value can be:
     <table 33c%>
      Value  		   Meaning
      ------------  ----------------
      MOD_SHIFT     The SHIFT key
      MOD_CONTROL   The CTRL key
      MOD_ALT       The ALT key
     </table>
     @Return If the function fails, the return value is zero. To get extended
     error information, call GetLastError 
     
     Remarks
     By default the console session cannot be shadowed. You can
     change this by modifying the following registry keys:
     <code lang="Delphi">
     HKLM\SYSTEM\CurrentControlSet\Control\Terminal Server\WinStations\Console
     "fInheritShadow" = REG_DWORD:1
     "Shadow" = REG_DWORD:1
     </code>
     Where Shadow can be one of the TJwShadowMode values.
     See Also
      * ShadowInformation
      * TJwShadowMode
      * TJwShadowState
    }
    function Shadow(const Hotkey: DWORD = VK_MULTIPLY;
      const HKModifier: DWORD = MOD_CONTROL): Boolean;

    {<B>ShadowInformation</B> returns information about the Shadow State and Shadow Mode of
    a session.
    
    Shadow State shows if the session is shadowing another session or is being
    shadowed by another session.
    
    Shadow Mode queries the shadow permissions for this session.
    See Also
    * Shadow
    * TJwShadowMode
    * TJwShadowState
    }
    property ShadowInformation: TJwWTSSessionShadow read FShadow;

    {<B>Username</B> the name of the user associated with the session.
    }
    property Username: TJwString read FUsername;

    {<B>Token</B> returns the token of the session.
     This call needs the TCB privilege and the process must run under
     SYSTEM account; otherwise EJwsclPrivilegeCheckException,
     EJwsclWinCallFailedException is raised.
     The returned value is cached and must not be freed!
    }
    property Token : TJwSecurityToken read GetToken;

    {<B>UserSid</B> returns the logged on User of the session.
     This call needs the TCB privilege and the process must run under
     SYSTEM account; otherwise EJwsclPrivilegeCheckException,
     EJwsclWinCallFailedException is raised.
     
     The returned token is the user's primary access token which can be
     passed directly to CreateProcessAsUser in order to launch a process in
     the user's Session.
     
     Remarks
  The returned value is cached and must not be freed!
     If the value cannot be obtained the return value is @nil.
    }
    property UserSid : TJwSecurityID read GetUserSid;

    { <b>VerticalResolution</b> returns the height resolution of the terminal session
      display in pixels.                                                              }
    property VerticalResolution: DWORD read FVerticalResolution;

    {WinStationDriver Flag (<B>WdFlag</B>) returns a value indicating the protocol and
     connection type. It's usefull for easy determination of console session.
     Possible values:
     <table 33c%>
     Value               Meaning
     ---------------     ------------------------
     WD_FLAG_CONSOLE_XP  XP Console sessions
     WD_FLAG_CONSOLE     2003/2008 Console Session
     WD_FLAG_RDP         RDP Session
     WD_FLAG_ICA         ICA Session
     </table>
    }
    property WdFlag: DWORD read FWdFlag;

    {WinStationDriver Name (<B>WinStationDriverName</B>) returns a value indicating the protocol and
     protocol type.
     Known Microsoft values:
     <table 33c%>
     Operating System     Value
     -------------------  ----------------------
     Windows 2000         Microsoft RDP 5.0
     Windows XP           Microsoft RDP 5.1
     Windows 2003         Microsoft RDP 5.2
     Windows 2008/Vista   Microsoft RDP 6.0
     </table> 
     Known Citrix values:
     <table 40c%>
     Version  								   Value
     --------------------------------------  -------------------
     Citrix Presentation Server 4            Citrix ICA 3.0
      )

    }
    property WinStationDriverName: TJwString read FWdName;

    {<B>WinStationName</B> returns the session name.

     Remarks
  Despite its name, specifying this property does not return
     the window station name. Rather, it returns the name of the Terminal
     Services session.
     For RDP this will be something like RDP-Tcp#023
     For ICA this will be something like ICA-tcp#014
         }
    property WinStationName: TJwString read FWinStationName;

    {<B>WorkingDirectory</B> the default directory used when launching the initial program.}
    property WorkingDirectory: TJwString read FWorkingDirectory;

  end;

  {<B>PJwWTSSessionList</B> is a pointer to a TJwWTSSessionList}
  PJwWTSSessionList = ^TJwWTSSessionList;
  {<B>TJwWTSSessionList</B> is a List of all Sessions running on the Terminal Server
   and their properties

   Each item in the list points to a TJwWTSSession object that can be queried
   and manipulated.
   The list is filled by calling the EnumerateSessions function of the owning
   TJwTerminalServer instance.

   Example:
   <code lang="Delphi">
   var
     ATerminalServer: TJwTerminalServer;
     i: Integer;
   begin
     // Create Terminal Server instance and allocate memory for it
     ATerminalServer := TjwTerminalServer.Create;

     // Set servername (only in case of remote server)
     ATerminalServer.Server := 'TS001';

     // Remember that EnumerateSessions will automatically connect to the
     // Terminal Server for you. The connect function raises an Exception
     // if the connection attempt was unsuccessfull, so better use try..except
     try
       if ATerminalServer.EnumerateSessions then
       begin
         for i := 0 to ATerminalServer.Sessions.Count-1 do
         begin
           Memo1.Lines.Add(ATerminalServer.Sessions[i].Username);
         end;
       end;
     except
       on E: EJwsclWinCallFailedException do
       begin
        // Handle Exception here
       end;
     end;

     // Free Memory
     ATerminalServer.Free;
   end;
   </code>
  }

  TJwWTSSessionList = class(TObjectList)
  protected
    {@exclude}
    FOwner: TJwTerminalServer;
    {@exclude}
    function GetItem(Index: Integer): TJwWTSSession;
    {@exclude}
    procedure SetItem(Index: Integer; ASession: TJwWTSSession);
    {@exclude}
    procedure SetOwner(const Value: TJwTerminalServer);
  public

    {The <B>Destroy</B> destructor destroys the @Classname instance.
     Note that it is not necessary to manually free a @Classname as it will be
     freed when the TJwTerminalServer instance that owns the @Classname
     (Owner property) is freed.
     
     Remarks
  If you free a @Classname be sure to also set it to nil to
     prevent the Owner to free it as well (which would produce an Access
     Violation).
    }
    destructor Destroy; reintroduce;

    {<B>Add</B> adds a Session to the end of the Sessionlist
     @returns returns the index of the inserted object. 
    }
    function Add(ASession: TJwWTSSession): Integer;

    {<B>FindBySessionId</B> loops through the @Classname and returns the Session associated
     with the requested SessionId.
     @Param SessionId The Session Identifier 
     @Returns TJwWTSSession 

     Remarks
  If the SessionId was not found return value will be @nil.
    }
    function FindBySessionId(const SessionId: DWORD): TJwWTSSession;

    {<B>FindByUsername</B> loops through the @Classname and returns the first Session associated
     with the requested Username which is compared case insensitive.
     @Param Username The windows username 
     @Returns TJwWTSSession 
     
     Remarks
  If the Username was not found return value will be @nil.
    }
    function FindByUsername(const Username: TJwString): TJwWTSSession;


    {(<B>GetEnumerator</B> returns an enumerator that can be used to iterate through
     the image list collection with Delphi's for in loop (Delphi 2005 and
     higher).
     
     <code lang="Delphi">
     var
       ATerminalServer: TJwTerminalServer;
       Session: TJwWTSSession;
     begin
       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TJwTerminalServer.Create;

       // It's recommended to wrap the Connect call in a try..except block since
       // a connection failure will raise an exception.
       try
         ATerminalServer.Connect;
       except
         on E: EJwsclWinCallFailedException do
         begin
           // Handle Exception here
         end;
       end;

       // EnumerateSessions might fail and return false, so always check the
       // result.
       if ATerminalServer.EnumerateSessions then
       begin

         // Loop through the Sessions with the for..in loop (needs Delphi 2005
         // or higher).
         for Session in ATerminalServer.Sessions do
         begin
           Memo1.Lines.Add(Format('User %s has session %d, the session state is %s',
             [Session.Username, Session.SessionId, Session.ConnectStateStr]));
         end;

       end;

       // Free Memory (note that free will automatically disconnect an active
       // connection)

       ATerminalServer.Free;

     end;
     </code>
    }
    function GetEnumerator: TJwSessionsEnumerator;


    {@Returns the index of the Session object in the SessionList. 
    }
    function IndexOf(ASession: TJwWTSSession): Integer;

    {<B>Insert</B> Adds an object to the list at a specified position
    }
    procedure Insert(Index: Integer; ASession: TJwWTSSession);

    {The <B>Items[Index</B> properties gives access to a Session and it's properties
     Example:
     <code lang="Delphi">
     var
       ATerminalServer: TJwTerminalServer;
       i: Integer;
     begin
       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TjwTerminalServer.Create;

       // Set servername (only in case of remote server)
       ATerminalServer.Server := 'TS001';

       // Remember that EnumerateSessions will automatically connect to the
       // Terminal Server for you. The connect function raises an Exception
       // if the connection attempt was unsuccessfull, so better use try..except
       try
         if ATerminalServer.EnumerateSessions then
         begin
           for i := 0 to ATerminalServer.Sessions.Count-1 do
           begin
             Memo1.Lines.Add(ATerminalServer.Sessions[i].Username);
           end;
         end;
       except
         on E: EJwsclWinCallFailedException do
         begin
          // Handle Exception here
         end;
       end;

       // Free Memory
       ATerminalServer.Free;
     end;
     </code>
    }
    property Items[Index: Integer]: TJwWTSSession read GetItem write SetItem; default;

    {<B>Owner</B> Specifies the TJwTerminalServer instance that owns the @Classname
    }
    property Owner: TJwTerminalServer read FOwner write SetOwner;

    {<B>Remove</B> removes the specified Session from the SessionList and if
     OwnsObjects is true (default) frees the Session.
     @returns The value returned is the index of the object in the Items array
     before it was removed. If the specified object is not found on the list,
     Remove returns –1. 
    }
    function Remove(ASession: TJwWTSSession): Integer;
  end;

  {<B>TJwWTSProcess</B> is the class that encapsulates a process that is running on
   a Terminal Server. 

   A process is uniquely identified by the Process Id (PID) in combination with
   it's Creation Time (the OS reused PID's).
   
   A <B>TJwWTSProcess</B> is owned by a TJwWTSProcessList.
   }
  TJwWTSProcess = class(TObject)
  protected
    {@exlude}
    FData: Integer;
    {@exclude}
    FOwner: TJwWTSProcessList;
    {@exclude}
    FProcessAge: Int64;
    {@exclude}
    FProcessAgeStr: TJwString;
    {@exclude}
    FProcessCreateTime: Int64;
    {@exclude}
    FProcessCreateTimeStr: TJwString;
    {@exclude}
    FProcessCPU: Integer;
    {@exclude}
    FProcessCPUTime: Int64;
    {@exclude}
    FProcessCPUTimeStr: TJwString;
    {@exclude}
    FProcessId: TJwProcessID;
    {@exclude}
    FProcessMemUsage: DWORD;
    {@exclude}
    FProcessName: TJwString;
    {@exclude}
    FProcessVirtualSize: DWORD;
    {@exclude}
    FProcessVMSize: DWORD;
    {@exclude}
    FSessionId: TJwSessionID;
    {@exclude}
    FUsername: TJwString;
    {@exclude}
    FWinStationName: TJwString;
    {@exclude}
    FToken : TJwSecurityToken;
    {@exclude}
    FUserSid : TJwSecurityID;
    {@exclude}
    function GetServer: TJwString;
    {@exclude}
    function GetToken : TJwSecurityToken;
    {@exclude}
    function GetUserSid : TJwSecurityID;
    function GetServerHandle: THandle; virtual;
  public
    {<B>Create</B> create a TJwWTSProcess instance.
     @Param Owner Specifies the TJwTerminalServer instance that owns the process. 
     @Param SessionId The Session Identifier. 
     @Param ProcessId The Process Identifier. 
     @Param ProcessName The Process Name. 
    {@Param Username the name of the user associated with the process. 
    }
    constructor Create(const Owner: TJwWTSProcessList;
      const SessionId: TJwSessionId; const ProcessID: TJwProcessId;
      const ProcessName: TJwString; const Username: TJwString);
    destructor Destroy; override;
  public
    {<B>Data</B> allows storage of a pointer to user specific data and can be freely
     used.}
    property Data: Integer read FData write FData;

    {The <B>Terminate</B> function terminates the specified process on the specified
     terminal server.
    }
    function Terminate: boolean; overload;
    {The <B>Terminate</B> function terminates the specified process on the specified
     terminal server.
     @Param dwExitCode Specifies the exit code for the terminated process. 
    }
    function Terminate(const dwExitCode: DWORD): boolean; overload;
  public
   {<B>Owner</B> specifies the TJwTerminalServer instance that owns the session)
    }
    property Owner: TJwWTSProcessList read FOwner write FOwner;
    {<B>SessionId</B> the session identifier
    }
    property SessionId: TJwSessionId read FSessionId;
    {<B>ProcessAge</B> the elapsed time since the process was created in
     100-nanosecond intervals since January 1, 1601 (TFileTime).
    See Also
    * ProcessAgeStr
    }
    property ProcessAge: Int64 read FProcessAge;
    {<B>ProcessAgeStr</B> the elapsed time since the process was created as formatted
     string. The string is formatted according to the table below:
     <table>
     days   hours  minutes  value
     -----  -----  -------  ----------
     \> 0   any    any      +d+hh:mm
     0      \>0    any      hh:mm
     0      0      any      mm
     0      0      0        .
      )
     
     See Also
     * ProcessAge
    }
    property ProcessAgeStr: TJwString read FProcessAgeStr;


    {<B>ProcessCPU</B> can be used to store the relative CPU Time as percentage
     total. Needs to be calculated by caller... }
    property ProcessCPU: Integer read FProcessCPU write FProcessCPU;

    {<B>ProcessCPUTime</B> the total CPU Time (Usertime + Kerneltime) for the given process
     in 100-nanosecond intervals since January 1, 1601 (TFileTime).

     Remarks
     This value matches the CPU Time column in Task Manager.
     See Also
     * ProcessCPUTimeStr
    }
    property ProcessCPUTime: Int64 read FProcessCPUTime;

    {<B>ProcessCPUTimeStr</B> the total CPU Time (Usertime + Kerneltime) for the given process
     as formatted string. (On Delphi 7 and higher this is a localised string
     for older version it is fixed at hh:mm)
     
     Remarks
     This value matches the CPU Time column in Task Manager.
     See Also
     * ProcessCPUTime
    }
    property ProcessCPUTimeStr: TJwString read FProcessCPUTimeStr;

    {<B>ProcessCreateTime</B> the Process Creation Time }
    property ProcessCreateTime: Int64 read FProcessCreateTime;
    {<B>ProcessCreateTime</B> the Process Creation Time formatted as localised string.
    }
    property ProcessCreateTimeStr: TJwString read FProcessCreateTimeStr;

    {<B>ProcessId</B> the Process Identifier or PID
    }
    property ProcessId: TJwProcessId read FProcessId;

    {<B>ProcessName</B> the Process Name
     
     Remarks
     Windows XP (at least SP2) has the following bug:
     The Process Name is cut off at 18 characters for process on the local
     machine and at 15 characters for remote servers (even if the remote server
     is Windows Server 2003 which does not suffer from this bug).

    }
    property ProcessName: TJwString read FProcessName;

    {<B>ProcessMemUsage</B> the Amount of memory in Bytes used by the process
     
     Remarks
     This value matches the Mem Usage column in Task Manager.
    }
    property ProcessMemUsage: DWORD read FProcessMemUsage;

    property ProcessVirtualSize: DWORD read FProcessVirtualSize;

    {<B>ProcessVMSize</B> the Amount of Virtual memory in Bytes used by the process
     
     Remarks
  This value matches the VM Size column in Task Manager.
    }
    property ProcessVMSize: DWORD read FProcessVMSize;

    {<B>Server</B> the netbios name of the Terminal Server.
     
     Remarks
  If you want to connect to a Terminal Server locally
     you should not specify the server name. Please note that in the case of a
     local connection this property <B>will return the computername</B> )
    }
    property Server: TJwString read GetServer;

    {<B>Token</B> returns the token of the session.
     The returned value is cached and must not be freed!

     You may need to enable the DEBUG privilege if necessary
     to obtain a process handle internally.

     If the value cannot be obtained the return value is nil.
    }
    property Token : TJwSecurityToken read GetToken;

    {<B>UserSid</B> returns a JwsclSid.TJwSecurityID instance pointing to the SID of the
     user that is associated with the process.

     Remarks
    The returned value is cached and must not be freed!
     If the value cannot be obtained the return value is nil.
    }
    property UserSid : TJwSecurityID read GetUserSid;

    {<B>Username</B> the name of the user associated with the process.
    }
    property Username: TJwString read FUsername;

    {<B>WinStationName</B> returns the session name.
     
     Remarks
  Despite its name, specifying this property does not return
     the window station name. Rather, it returns the name of the Terminal
     Services session.
     For RDP this will be something like RDP-Tcp#023
     For ICA this will be something like ICA-tcp#014
    }
    property WinStationName: TJwString read FWinStationname;
  end;

  {<B>PJwWTSProcessList</B> is a pointer to a TJwWTSProcessList}
  PJwWTSProcessList = ^TJwWTSProcessList;

  {<B>TJwWTSProcessList</B> is a List of all Processes running on the Terminal Server
   and their properties 

   Each item in the list points to a TJwWTSProcess object that can be queried
   and manipulated.
   The list is filled by calling the EnumerateProcesses function of the owning
   TJwTerminalServer instance.
   
   Example:
   <code lang="Delphi">
   var
     ATerminalServer: TJwTerminalServer;
     i: Integer;
   begin
     // Create Terminal Server instance and allocate memory for it
     ATerminalServer := TjwTerminalServer.Create;

     // Set servername (only in case of remote server)
     ATerminalServer.Server := 'TS001';

     // Remember that EnumerateProcesses will automatically connect to the
     // Terminal Server for you. The connect function raises an Exception
     // if the connection attempt was unsuccessfull, so better use try..except
     try
       if ATerminalServer.EnumerateProcesses then
       begin
         for i := 0 to ATerminalServer.Processes.Count-1 do
         begin
           Memo1.Lines.Add(ATerminalServer.Processes[i].ProcessName);
         end;
       end;
     except
       on E: EJwsclWinCallFailedException do
       begin
        // Handle Exception here
       end;
     end;

     // Free Memory
     ATerminalServer.Free;
   end;
   </code>
  }
  TJwWTSProcessList = class(TObjectList)
  private
  protected
    {@exclude}
    FOwner: TJwTerminalServer;
    {@exclude}
    FIdleTime: Int64;
    {@exclude}
    FCPUTime: Int64;
    {@exclude}
    FCommitSizeTotal: Int64;
    {@exclude}
    FPagedPoolTotal: Int64;
    {@exclude}
    FNonPagedPoolTotal: Int64;
    {@exclude}
    FWorkingSetTotal: Int64;
    {@exclude}
    function GetItem(Index: Integer): TJwWTSProcess;
    {@exclude}
    procedure SetItem(Index: Integer; AProcess: TJwWTSProcess);
    {@exclude}
    procedure SetOwner(const Value: TJwTerminalServer);
  published
  public
    {<B>Add</B> adds a Process to the end of the Processlist
     @returns returns the index of the inserted object. 
    }
    function Add(AProcess: TJwWTSProcess): Integer;

    function FindByPid(const PID: DWORD): TJwWTSProcess;
    {<B>GetEnumerator</B> returns an enumerator that can be used to iterate through
     the image list collection with Delphi's for in loop (Delphi 2005 and
     higher).
     
     <code lang="Delphi">
     var
       ATerminalServer: TJwTerminalServer;
       Process: TJwWTSProcess;
     begin
       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TJwTerminalServer.Create;

       // It's recommended to wrap the Connect call in a try..except block since
       // a connection failure will raise an exception.
       try
         ATerminalServer.Connect;
       except
         on E: EJwsclWinCallFailedException do
         begin
           // Handle Exception here
         end;
       end;

       // EnumerateProcesses might fail and return false, so always check the
       // result.
       if ATerminalServer.EnumerateProcesses then
       begin

         // Loop through the Processes with the for..in loop (needs Delphi 2005
         // or higher).
         for Process in ATerminalServer.Processes do
         begin
           Memo1.Lines.Add(Format('Process %s runs in  session %d with PID %d',
             [Process.Processname, Process.SessionId, Process.ProcessId]));
         end;

       end;

       // Free Memory (note that free will automatically disconnect an active
       // connection)

       ATerminalServer.Free;

     end;
     </code>
    }
    function GetEnumerator: TJwProcessEnumerator;

    {@Returns the total IdleTime (sum of UserMode + KernelMode of the Idle Process).
    }
    property IdleTime: Int64 read FIdleTime;

    {@Returns the total CPUTime (sum of UserMode + KernelMode of all Processes
     except the Idle Process).
    }
    property CPUTime: Int64 read FCPUTime;

    {@Returns the index of the Process object in the ProcessList.
    }
    function IndexOf(AProcess: TJwWTSProcess): Integer;

    {<B>Insert</B> Adds an object to the list at a specified position
    }
    procedure Insert(Index: Integer; AProcess: TJwWTSProcess);

    {The <B>Items[Index</B> properties gives access to a Process and it's properties
     Example:
     <code lang="Delphi">
     var
       ATerminalServer: TJwTerminalServer;
       i: Integer;
     begin
       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TjwTerminalServer.Create;

       // Set servername (only in case of remote server)
       ATerminalServer.Server := 'TS001';

       // Remember that EnumerateProcesses will automatically connect to the
       // Terminal Server for you. The connect function raises an Exception
       // if the connection attempt was unsuccessfull, so better use try..except
       try
         if ATerminalServer.EnumerateProcesses then
         begin
           for i := 0 to ATerminalServer.Processes.Count-1 do
           begin
             Memo1.Lines.Add(ATerminalServer.Processes[i].ProcessName);
           end;
         end;
       except
         on E: EJwsclWinCallFailedException do
         begin
          // Handle Exception here
         end;
       end;

       // Free Memory
       ATerminalServer.Free;
     end;
     </code>
    }
    property Items[Index: Integer]: TJwWTSProcess read GetItem write SetItem; default;

    {<B>Owner</B> specifies the TJwTerminalServer instance that owns the ProcessList
    }
    property Owner: TJwTerminalServer read FOwner write SetOwner;

    {<B>Remove</B> removes the specified Session from the SessionList and if
     OwnsObjects is true (default) frees the Session.
     @returns The value returned is the index of the object in the Items array
     before it was removed. If the specified object is not found on the list,
     Remove returns –1.
    }
    function Remove(AProcess: TJwWTSProcess): Integer;
    property CommitSizeTotal: Int64 read FCommitSizeTotal;
    property WorkingSetTotal: Int64 read FWorkingSetTotal;
    property PagedPoolTotal: Int64 read FPagedPoolTotal;
    property NonPagedPoolTotal: Int64 read FNonPagedPoolTotal;
  end;

  {<B>TJwShadowState</B> indicates the Shadow State of a session}
  TJwShadowState =
    (
     {The session is not Shadowing or Being Shadowed}
     ssNone,
     {The session is not Shadowing another session}
     ssShadowing,
     {The session is being Shadowed by another session}
     ssBeingShadowed
  );

  {<B>TJwShadowMode</B> indicates the Shadow Permissions of a session}
  TJwShadowMode = (
    {The sessions cannot be shadowed}
    smNoneAllowed,
    {The sessions be shadowed but needs the user's permission}
    smFullControlWithPermission,
    {The sessions be shadowed without the user's permission}
    smFullControlWithoutPermission,
    {The sessions can be viewed but needs the user's permission}
    smViewOnlyWithPermission,
    {The sessions can be viewed without the user's permission}
    smViewOnlyWithoutPermission
  );

  {<B>TJwWTSSessionShadow</B> class gives access to the ShadowState and Shadowmode of a
   session.

   Remarks
  Please note that changing the shadow mode with the SetShadow
   function does not take affect until the sessions has been disconnected
   and reconnected.
   @seealso(TJwShadowMode)
   @seealso(TJwShadowState)
  }
  TJwWTSSessionShadow = class
  private
    {@exclude}
    FWinStationShadowInformation : TWinStationShadowInformation;
    {@exclude}
    FOwner : TJwWTSSession;
  protected
    {@exclude}
    function GetShadowState : TJwShadowState;
    {@exclude}
    function GetShadowMode : TJwShadowMode;
    {@exclude}
    procedure SetShadowMode(const Value : TJwShadowMode);
    {@exclude}
    procedure UpdateShadowInformation(const Modify : Boolean);
  public
    {The <B>Create</B> constructor creates an @ClassName instance.
    }
    constructor Create(AOwner: TJwWTSSession);

    {The <B>ShadowState</B> property indicates the ShadowState of the Session
    }
    property ShadowState: TJwShadowState read GetShadowState;

    {The <B>ShadowMode</B> property indicates the ShadowMode of the Session
    }
    property ShadowMode: TJwShadowMode read GetShadowMode write SetShadowMode;
  end;

  TJwWTSSessionStatistics = class
  private
    function GetLoadIndicatorData: TWinStationLoadIndicatorData;
    protected
      FLoadIndicatorData: TWinStationLoadIndicatorData;
      TSCounterArray: array[1..12] of TTSCounter;
      FOwner: TJwTerminalServer;
      function GetCounterValue(const CounterID: Integer): Integer;
  published
    public
      {
      Raises
        EJwsclNILParameterException: will be raised if AOwner is nil. 
      }
      constructor Create(const AOwner: TJwTerminalServer);
      property TotalSessionsCreated: Integer index TERMSRV_TOTAL_SESSIONS read GetCounterValue;
      property TotalSessionsDisconnected: Integer index TERMSRV_DISC_SESSIONS read GetCounterValue;
      property TotalSessionsReconnected: Integer index TERMSRV_RECON_SESSIONS read GetCounterValue;
      property CurrentActiveSessions: Integer index TERMSRV_CURRENT_ACTIVE_SESSIONS read GetCounterValue;
      property CurrentDisconnectedSessions: Integer index TERMSRV_CURRENT_DISC_SESSIONS read GetCounterValue;
      property PendingSessions: Integer index TERMSRV_PENDING_SESSIONS read GetCounterValue;
      property TotalLogons: Integer index TERMSRV_SUCC_TOTAL_LOGONS read GetCounterValue;
      property TotalLocalLogons: Integer index TERMSRV_SUCC_LOCAL_LOGONS read GetCounterValue;
      property TotalRemoteLogons: Integer index TERMSRV_SUCC_REMOTE_LOGONS read GetCounterValue;
      property TotalConsoleLogons: Integer index TERMSRV_SUCC_SESSION0_LOGONS read GetCounterValue;
      property CurrentTerminatingSessions: Integer index TERMSRV_CURRENT_TERMINATING_SESSIONS read GetCounterValue;
      property CurrentLoggedOnSessions: Integer index TERMSRV_CURRENT_LOGGEDON_SESSIONS read GetCounterValue;
      property LoadIndicatorData: TWinStationLoadIndicatorData read GetLoadIndicatorData;
      procedure UpdateCounters;
  end;



  { This class is used by TJwWTSSessionList to provide extended enumeration of
    session using Delphi 2005 and newer "for in" syntax.                       }
  TJwSessionsEnumerator = class
  private
    FIndex: Integer;
    FSessions: TJwWTSSessionList;
  public
    constructor Create(ASessionList: TJwWTSSessionList);
    function GetCurrent: TJwWTSSession;
    function MoveNext: Boolean;
    property Current: TJwWTSSession read GetCurrent;
  end;



  { This class is used by TJwWTSProcessList to provide extended enumeration of
    processes using Delphi 2005 and newer "for in" syntax.                     }
  TJwProcessEnumerator = class
  private
    FIndex: Integer;
    FProcesses: TJwWTSProcessList;
  public
    constructor Create(AProcessList: TJwWTSProcessList);
    function GetCurrent: TJwWTSProcess;
    function MoveNext: Boolean;
    property Current: TJwWTSProcess read GetCurrent;
  end;


{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}

type
  { array of TWtsSessionInfoA }
  PJwWTSSessionInfoAArray = ^TJwWTSSessionInfoAArray;
  TJwWTSSessionInfoAArray = array[0..ANYSIZE_ARRAY-1] of TWtsSessionInfoA;

  { array of TWtsSessionInfoW }
  PJwWTSSessionInfoWArray = ^TJwWTSSessionInfoWArray;
  TJwWTSSessionInfoWArray = array[0..ANYSIZE_ARRAY-1] of TWtsSessionInfoW;

  { array of TWtsProcessInfoA }
  PJwWTSProcessInfoAArray = ^TJwWTSProcessInfoAArray;
  TJwWTSProcessInfoAArray = array[0..ANYSIZE_ARRAY-1] of TWtsProcessInfoA;

  { array of TWtsProcessInfoW }
  PJwWTSProcessInfoWArray = ^TJwWTSProcessInfoWArray;
  TJwWTSProcessInfoWArray = array[0..ANYSIZE_ARRAY-1] of TWtsProcessInfoW;

  { array of TWtsServerInfoA }
  PJwWtsServerInfoAArray = ^TJwWtsServerInfoAArray;
  TJwWtsServerInfoAArray = array[0..ANYSIZE_ARRAY-1] of TWtsServerInfoA;

  { array of TWtsServerInfoW }
  PJwWtsServerInfoWArray = ^TJwWtsServerInfoWArray;
  TJwWtsServerInfoWArray = array[0..ANYSIZE_ARRAY-1] of TWtsServerInfoW;

constructor TJwTerminalServer.Create;
begin
  inherited Create;
  FSessions := TJwWTSSessionList.Create(True);
  FSessions.Owner := Self;

  FProcesses := TJwWTSProcessList.Create(True);
  FProcesses.Owner := Self;

  FTerminalServerEventThread := nil;
  FServers := TStringList.Create;

  FOnServersEnumerated := nil;

  FSessionStatistics := nil;

  CachedUser.SessionId := DWORD(-1);
  FPhysicalMemory := -1;
end;

//function RpcCancelThread(ThreadHandle: HANDLE): RPC_STATUS; stdcall; external 'rpcrt4.dll';

destructor TJwTerminalServer.Destroy;
var
  EventFlag: DWORD;
  ThreadHandle: THandle;
  dwResult: DWORD;
begin
  // Close connection
  if Assigned(FEnumServersThread) then
  begin
    // Don't handle any more events
    FOnServersEnumerated := nil;

    // Signal Termination to the thread
    FEnumServersThread.Terminate;

    ThreadHandle := FEnumServersThread.Handle;
    // Wait a while, see if thread terminates
    if WaitForSingleObject(ThreadHandle, 500) = WAIT_TIMEOUT then
    begin
      // it didn't, so kill it (we don't want the user to wait forever)!
      // TSAdmin does it the same way...
      TerminateThread(ThreadHandle, 0);
    end;

  end;

    // Free the SessionList
    FreeAndNil(FSessions);

    // Free the Statistics
    FreeAndNil(FSessionStatistics);

    // Free the ProcessList
    FreeAndNil(FProcesses);

  // Free the Serverlist
    FreeAndNil(FServers);

  // Terminate the Event Thread before closing the connection.
  if Assigned(FTerminalServerEventThread) then
  begin
    ThreadHandle := FTerminalServerEventThread.Handle;

    // Terminate Event Thread
    FTerminalServerEventThread.Terminate;

    // unblock the waiter
    WTSWaitSystemEvent(FServerHandle, WTS_EVENT_FLUSH, EventFlag);
    // wait for the thread to finish

    // Wait a while, see if thread terminates
    OutputDebugString('WAITING FOR THREAD TO END.....................');
    dwResult := WaitForSingleObject(ThreadHandle, 500);

    if dwResult = WAIT_TIMEOUT then
    begin
      // The thread didn't close, probably because it doesn't respond to
      // WTS_EVENT_FLUSH, this is a bug in winsta.dll.
      // see http://support.microsoft.com/kb/941561
      //
      // WORKAROUND: Kill the thread...
{$IFDEF DEBUG}
      OutputDebugString('Terminating TJwWTSEnumServersThread thread because WTSWAIT did not return');
{$ENDIF}
      TerminateThread(ThreadHandle, 0);
//      i := RpcCancelThread(FTerminalServerEventThread.Handle);
//      OutputDebugString(PChar(Format('result=%d', [i])));
    end;

    // Free Memory
    FreeAndNil(FTerminalServerEventThread);

  end;

  if Connected then
  begin
    Disconnect;
  end;

  inherited Destroy;
end;


{
The following table lists the events that trigger the different flags.
Events are listed across the top and the flags are listed down the
left column. An “X” indicates that the event triggers the flag.
+-------------+------+------+------+-------+----------+-----+------+-------+
| EventFlag   |Create|Delete|Rename|Connect|Disconnect|Logon|Logoff|License|
+-------------+------+------+------+-------+----------+-----+------+-------+
| Create      | X    |      |      | X     |          |     |      |       |
+-------------+------+------+------+-------+----------+-----+------+-------+
| Delete      |      | X    |      |       |          |     | X    |       |
+-------------+------+------+------+-------+----------+-----+------+-------+
| Rename      |      |      | X    |       |          |     |      |       |
+-------------+------+------+------+-------+----------+-----+------+-------+
| Connect     |      |      |      | X     |          |     |      |       |
+-------------+------+------+------+-------+----------+-----+------+-------+
| Disconnect  |      |      |      |       | X        |     |      |       |
+-------------+------+------+------+-------+----------+-----+------+-------+
| Logon       |      |      |      |       |          | X   |      |       |
+-------------+------+------+------+-------+----------+-----+------+-------+
| Logoff      |      |      |      |       |          |     | X    |       |
+-------------+------+------+------+-------+----------+-----+------+-------+
| License     |      |      |      |       |          |     |      | X     |
+-------------+------+------+------+-------+----------+-----+------+-------+
| StateChange | X    | X    |      | X     | X        | X   | X    |       |
+-------------+------+------+------+-------+----------+-----+------+-------+
| All         | X    | X    | X    | X     | X        | X   | X    | X     |
+-------------+------+------+------+-------+----------+-----+------+-------+

An WinStation is created when a user connects. When a user logs off, the
Winstation is deleted. When a user logs on to a disconnected session, the
existing session is deleted and the Delete flag is triggered. When users
connect to a disconnected session from within a session, their session is
disconnected and the Disconnect flag is triggered instead of the Delete flag.}

procedure TJwTerminalServer.FireEvent(EventFlag: DWORD);
begin
  // Set LastEventFlag property
  FLastEventFlag := EventFlag;

  // The OnSessionEvent should be fired if anything happens that is session
  // related, like statechange, logon/logoff, disconnect and (re)connect.
  if (EventFlag > WTS_EVENT_CONNECT) and (EventFlag < WTS_EVENT_LICENSE) then
  begin
    if Assigned(FOnSessionEvent) then
    begin
      OnSessionEvent(Self);
    end;
  end;

  if (EventFlag and WTS_EVENT_LICENSE = WTS_EVENT_LICENSE) and
    Assigned(OnLicenseStateChange) then
  begin
    OnLicenseStateChange(Self);
  end;
  if (EventFlag and WTS_EVENT_STATECHANGE = WTS_EVENT_STATECHANGE) and
    Assigned(FOnSessionStateChange) then
  begin
    OnSessionStateChange(Self);
  end;
  if (EventFlag and WTS_EVENT_LOGOFF = WTS_EVENT_LOGOFF) and
    Assigned(FOnSessionLogoff) then
  begin
    OnSessionLogoff(Self);
  end;
  if (EventFlag and WTS_EVENT_LOGON = WTS_EVENT_LOGON) and
    Assigned(FOnSessionLogon) then
  begin
    OnSessionLogon(Self);
  end;
  if (EventFlag and WTS_EVENT_DISCONNECT = WTS_EVENT_DISCONNECT) and
    Assigned(FOnSessionDisconnect) then
  begin
    OnSessionDisconnect(Self);
  end;
  if (EventFlag and WTS_EVENT_CONNECT = WTS_EVENT_CONNECT) and
    Assigned(FOnSessionConnect) then
  begin
    OnSessionConnect(Self);
  end;
  if (EventFlag and WTS_EVENT_RENAME = WTS_EVENT_RENAME) and
    Assigned(FOnWinStationRename) then
  begin
    OnWinStationRename(Self);
  end;
  if (EventFlag and WTS_EVENT_DELETE = WTS_EVENT_DELETE) and
    Assigned(FOnSessionDelete) then
  begin
    OnSessionDelete(Self);
  end;
  if (EventFlag and WTS_EVENT_CREATE = WTS_EVENT_CREATE) and
    Assigned(FOnSessionCreate) then
  begin
    OnSessionCreate(Self);
  end;

end;

function TJwTerminalServer.GetServer: TJwString;
var
  nSize: DWORD;
  pComputerName: TJwPChar;
begin
  // If no server was specified we return the local computername
  // (we cache this in FComputerName)
  if FServer = '' then
  begin
    if FComputerName = '' then
    begin
      nSize := MAX_COMPUTERNAME_LENGTH + 1;
      GetMem(pComputerName, nSize * TJwCharSize);
{$IFDEF UNICODE}
      GetComputerNameW(pComputerName, nSize);
{$ELSE}
      GetComputerNameA(pComputerName, nSize);
{$ENDIF}
      FComputerName := pComputerName;
      FreeMem(pComputerName);
    end;
    Result := FComputerName;
  end
  else
  begin
    Result := FServer;
  end;
end;

function TJwTerminalServer.GetSystemUsername: TJwString;
var
  JwSid: TJwSecurityId;
begin
  if FSystemUsername = '' then
  begin
    JwSid := TJwSecurityId.Create('S-1-5-18');
    FSystemUsername := JwSid.GetCachedUserFromSid;
    if FSystemUsername = '' then FSystemUsername := 'SYSTEM';
    JwSid.Free;
  end;

  Result := FSystemUsername;
end;

function TJwTerminalServer.GetWinStationName(const SessionId: DWORD): TJwString;
const
  BufferOverrunGuardFactor = 2;
var
  WinStationName: array[0..WINSTATIONNAME_LENGTH*BufferOverrunGuardFactor+1] of WChar;
begin
  Result := '';
  // Only use Unicode version since Ansi version doesn't seem to work on Vista
  ZeroMemory(@WinStationName, SizeOf(WinStationName));
  if WinStationNameFromLogonIdW(FServerHandle, SessionId, WinStationName) then
  begin
    Result := WinStationName;
  end;

  if Result = '' then
  begin
    // Confirm to TSAdmin behaviour and list sessionname as (Idle)
    // (we use StrConnectState api to localise)
    Result := '(' + JwPWideCharToJwString(StrConnectState(WTSIdle, False)) + ')';
  end;
end;

procedure TJwTerminalServer.OnEnumServersThreadTerminate(Sender: TObject);
begin
  // nil it!
  FEnumServersThread := nil;
end;

procedure TJwTerminalServer.SetServer(const Value: TJwString);
begin
  FServer := Value;
  // Clear the computername variable (cache)
  FComputerName := '';
end;

procedure TJwTerminalServer.OnInternalProcessFound(
      const Sender : TJwTerminalServer; var Process : TJwWTSProcess;
      var Cancel : Boolean; Data : Pointer);
begin
  Sender.FProcesses.Add(Process);
end;


procedure TJwTerminalServer.EnumerateProcessesEx;
begin
  FProcesses.Clear;
  FProcesses.FIdleTime := 0;
  FProcesses.FCPUTime := 0;
  FProcesses.FWorkingSetTotal := 0;
  FProcesses.FCommitSizeTotal := 0;
  FProcesses.FPagedPoolTotal := 0;
  FProcesses.FNonPagedPoolTotal := 0;

  EnumerateProcessesEx(OnInternalProcessFound, nil);
end;

function TJwTerminalServer.EnumerateProcesses: Boolean;
begin
  FProcesses.Clear;
  FProcesses.FIdleTime := 0;
  FProcesses.FCPUTime := 0;

  Result := EnumerateProcesses(OnInternalProcessFound, nil);
end;

procedure TJwTerminalServer.EnumerateProcessesEx(const OnProcessFound : TJwOnProcessFound; Data : Pointer);
begin
  if not EnumerateProcesses(OnProcessFound, Data) then
    raise EJwsclEnumerateProcessFailed.CreateFmtWinCall(RsWinCallFailed,
          'EnumerateProcessesEx', ClassName, RsUNTerminalServer, 0, True,
          'WinStationGetAllProcesses', ['WinStationGetAllProcesses', FServer]);
end;

function TJwTerminalServer.EnumerateProcesses(const OnProcessFound : TJwOnProcessFound;
    Data : Pointer) : Boolean;
var
  Count: Integer;
  ProcessInfoPtr: PTS_ALL_PROCESSES_INFO_ARRAY;
  i: Integer;
  AProcess: TJwWTSProcess;
  strProcessName: TJwString;
  strUsername: TJwString;
  lpBuffer: PWideChar;
  DiffTime: TDiffTime;
  Cancel : Boolean;
  LastError: Integer;
  JwSid: TJwSecurityId;
begin
  ProcessInfoPtr := nil;
  Count := 0;

  JwRaiseOnNilParameter(@OnProcessFound, 'OnProcessFound','EnumerateProcesses',ClassName,RsUNTerminalServer);

  if not Connected then
  begin
    Connect;
  end;

  Cancel := false;
  ProcessInfoPtr := nil;

  Result := WinStationGetAllProcesses(FServerHandle, 0, Count, ProcessInfoPtr);
  LastError := GetLastError;

  // Ignore Error 997 Overlapped I/O in progress which somtimes happens but
  // seems to return correct results...
  if ((not Result) and (LastError = 997) and (Count > 0)and (ProcessInfoPtr <>  nil)) or
  // Ignore Error 1753 The sub received bad data which sometimes happen if the
  // server is under stress...
    ((not Result) and (LastError = 1783) and (Count > 0) and (ProcessInfoPtr <>  nil)) then Result := True;

  // Ignore Error 997 Overlapped I/O in progress which somtimes happens but
  // seems to return correct results...
//  if (not Result) and (LastError = 997) and (Count > 0) then Result := True;

  try
    if Result then
    begin
      for i := 0 to Count-1 do
      begin
        strUsername := '';

        with ProcessInfoPtr^[i], pTsProcessInfo^ do
        begin

          Inc(FProcesses.FCPUTime, UserTime.QuadPart + KernelTime.QuadPart);
          Inc(FProcesses.FWorkingSetTotal, WorkingSetSize);
          Inc(FProcesses.FCommitSizeTotal, VirtualSize);
          Inc(FProcesses.FPagedPoolTotal, QuotaPagedPoolUsage);
          Inc(FProcesses.FNonPagedPoolTotal, QuotaNonPagedPoolUsage);
          // System Idle Process
          if UniqueProcessId = 0 then
          begin
            FProcessors := NumberOfThreads;
            strProcessName := GetIdleProcessName;
            strUserName := SystemUsername;
            FProcesses.FIdleTime := UserTime.QuadPart + KernelTime.QuadPart;
          end
          else if UniqueProcessId = 4 then
          begin
            strProcessName := JwTSUnicodeStringToJwString(ImageName);
            strUserName := SystemUsername;
          end
          else begin
            strProcessName := JwTSUnicodeStringToJwString(ImageName);

            if IsValidSid(UserSid) then
            begin
              try
                try
                  JwSid := TJwSecurityID.Create(UserSid);
                  strUsername := JwSid.GetCachedUserFromSid;
                finally
                  FreeAndNil(JwSid);
                end;
              except
                strUsername := 'SYSTEM';
              end;
            end;

            if (strUsername = '') or (CompareText(strUsername, '(unknown)') = 0) then
            begin
              // if User is nonadmin WinStationGetAllProcesses returns empty or
              // invalid SID, in this case we try to obtain the username by
              // other means...
              strUsername := CachedGetUserFromSessionId(SessionId);

              // sort of a hack, if we cannot determine the user it is probably SYSTEM
              if (strUsername = '') or (CompareText(strUsername, '(unknown)') = 0) then
              begin
                strUsername := 'SYSTEM';
              end;

            end;
          end;

          AProcess := TJwWTSProcess.Create(FProcesses, SessionId,
            UniqueProcessId, strProcessName, strUsername);
          with AProcess do
          begin
            // Calculate Process Age
            CalculateElapsedTime(@CreateTime, DiffTime);

              // Reserve Memory
              GetMem(lpBuffer, ELAPSED_TIME_STRING_LENGTH * SizeOf(WCHAR));
            try
              // Format Elapsed Time String
              ElapsedTimeStringSafe(@DiffTime, False, lpBuffer,
                  ELAPSED_TIME_STRING_LENGTH);
              FProcessAge := (DiffTime.wDays * SECONDS_PER_DAY) +
                (DiffTime.wHours * SECONDS_PER_HOUR) +
                (DiffTime.wMinutes * SECONDS_PER_MINUTE);
              FProcessAgeStr := JwPWideCharToJwString(lpBuffer);
            finally
              // Free mem
              FreeMem(lpBuffer);
              //lpBuffer := nil;
            end;

            // Some of the used counters are explained here:
            // http://msdn2.microsoft.com/en-us/library/aa394372.aspx

            FProcessCreateTime := CreateTime.QuadPart;
            FProcessCreateTimeStr :=
              TimeToStr(FileTime2DateTime(FILETIME(CreateTime)));

            // Set ProcessCPU to -1 (callee has to calculate it...)
            FProcessCPU := -1;

            // The CPU Time column in Taskmgr.exe is Usertime + Kerneltime
            // So we take the sum of it and call it ProcessCPUTime
            FProcessCPUTime := UserTime.QuadPart + KernelTime.QuadPart;

            FProcessCPUTimeStr := TJwString(CPUTime2Str(
              LARGE_INTEGER(UserTime.QuadPart + KernelTime.QuadPart)));
            // Amount of memory in bytes that a process needs to execute
            // efficiently. Maps to Mem Size column in Task Manager.
            // So we call it ProcessMemUsage
            FProcessMemUsage := WorkingSetSize;
            FProcessVirtualSize := VirtualSize;
            // Pagefileusage is the amount of page file space that a process is
            // using currently. This value is consistent with the VMSize value
            // in TaskMgr.exe. So we call it ProcessVMSize
            FProcessVMSize := PagefileUsage;
          end;

          try
            OnProcessFound(Self, AProcess, Cancel, Data);
          finally
            //warning: AProcess may be deleted in there
          end;

          if Cancel then
              break;
        end;
      end;

    end
    else begin
      raise EJwsclEnumerateProcessFailed.CreateFmtWinCall(RsWinCallFailed,
       'EnumerateProcesses', ClassName, RsUNTerminalServer, 0, True,
          'WinStationGetAllProcesses', ['WinStationGetAllProcesses', FServer]);
    end;
  finally
    // Cleanup
    if ProcessInfoPtr <> nil then
    begin
      WinStationFreeGAPMemory(0, ProcessInfoPtr, Count);
    end;
  end;

  ProcessInfoPtr := nil;
end;

function TJwTerminalServer.GetServers: TStringList;
begin
  // Create the list
  if not Assigned(FServers) then
  begin
    FServers := TStringList.Create;
    // The list was empty so fill it!
    EnumerateServers('');
  end;

  // Return the serverlist
  //TODO: Warning: User can free returned list (on purpose) this can
  //lead to problems in EnumerateServers
  Result := FServers;
end;

function TJwTerminalServer.GetSessionStatistics: TJwWTSSessionStatistics;
begin
  if not Assigned(FSessionStatistics) then
  begin
    FSessionStatistics := TJwWTSSessionStatistics.Create(Self);
  end;

  Result := FSessionStatistics;
end;

function TJwTerminalServer.EnumerateSessions: boolean;
var
  SessionInfoPtr:
{$IFDEF UNICODE}
  PJwWTSSessionInfoWArray;
{$ELSE}
  PJwWTSSessionInfoAArray;
{$ENDIF UNICODE}
  pCount: DWORD;
  i: integer;
  Res: Longbool;
  ASession: TJwWTSSession;
  LastError: Integer;
begin
  if not Connected then
  begin
    Connect;
  end;

  // Clear the sessionslist
  FSessions.Clear;
  pCount := 0;

  Res :=
{$IFDEF UNICODE}
    WTSEnumerateSessionsW(FServerHandle, 0, 1, PWTS_SESSION_INFOW(SessionInfoPtr),
      pCount);
{$ELSE}
    WTSEnumerateSessions(FServerHandle, 0, 1, PWTS_SESSION_INFOA(SessionInfoPtr),
      pCount);
{$ENDIF UNICODE}
  LastError := GetLastError;

  // Ignore Error 997 Overlapped I/O in progress which somtimes happens but
  // seems to return correct results...
  if ((not Res) and (LastError = 997) and (pCount > 0)) or
  // Ignore Error 1783 The stub received bad data which sometimes happen if the
  // server is under stress...
    ((not Res) and (LastError = 1783) and (pCount > 0)) then Res := True;

  if not Res then begin
    raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
      'EnumerateSessions', ClassName, RsUNTerminalServer, 0, True,
          'WTSEnumerateSessions', ['WTSEnumerateSessions']);
  end;

  // Add all sessions to the SessionList
  for i := 0 to pCount - 1 do
  begin
    ASession := TJwWTSSession.Create(FSessions, SessionInfoPtr^[i].SessionId,
      GetWinStationName(SessionInfoPtr^[i].SessionId),
      TWtsConnectStateClass(SessionInfoPtr^[i].State));
    FSessions.Add(ASession);
  end;

  // After enumerating we create an event thread to listen for session changes
  if Res and (FTerminalServerEventThread = nil) then
  begin
    FTerminalServerEventThread := TJwWTSEventThread.Create(False, Self);
  end;

  if pCount > 0 then
  begin
    WTSFreeMemory(SessionInfoPtr);
  end;

  SessionInfoPtr := nil;

  // Pass the result
  Result := Res;
end;

function TJwTerminalServer.EnumerateServers(const ADomain: TJwString): Boolean;
begin
  // Does the thread exist?
  if Assigned(FEnumServersThread) then
  begin
    Result := False;
  end
  else
  begin
    // Create the thread
    FEnumServersThread := TJwWTSEnumServersThread.Create(True, Self, ADomain);
    FEnumServersThread.OnTerminate := OnEnumServersThreadTerminate;
    FEnumServersThread.Resume;
    Result := True;
  end;
end;

{ this is a workaround for a bug in Vista
  When opening a server connection with WTSOpenServer it is supposed to return
  0 if the connection fails but instead it returns a handle to the localmachine.
  This function tries to open the Ctx_WinStation_API_service Pipe
}
function IsConnectionValid(const Servername: String): Boolean;
const
  CtxWinStationAPIservice: TJwString = 'Ctx_WinStation_API_service';
var
  PipeName: TJwPChar;
  PipeHandle: THandle;
begin
  // Pipename will be \\server\pipe\Ctx_WinStation_API_service
  // Note that this pipe can be opened only remotely
  PipeName := TJwPChar(Format('\\%s\pipe\%s', [Servername, CtxWinStationAPIservice]));

  // Try to open the Named Pipe with least possible rights, if the call succeeds
  // we trust the connection
  PipeHandle := {$IFDEF UNICODE}CreateFileW{$ELSE}CreateFileA{$ENDIF}(
    PipeName,       //__in      LPCTSTR lpFileName,
    SYNCHRONIZE,    //__in      DWORD dwDesiredAccess,
    0,              //__in      DWORD dwShareMode,
    nil,            //__in_opt  LPSECURITY_ATTRIBUTES lpSecurityAttributes,
    OPEN_EXISTING,  //__in      DWORD dwCreationDisposition,
    0,              //__in      DWORD dwFlagsAndAttributes,
    0               //__in_opt  HANDLE hTemplateFile
  );

  // Return False if PipeHandle = INVALID_HANDLE_VALUE
  Result := PipeHandle <> INVALID_HANDLE_VALUE;

  // Close the Handle
  if PipeHandle <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle(PipeHandle);
  end;
end;

procedure TJwTerminalServer.Connect;
begin
  if not FConnected then
  begin
    if FServer = '' then
    begin
      FServerHandle := WTS_CURRENT_SERVER_HANDLE;
      FConnected := True;
    end
    else
    begin
      FServerHandle :=
{$IFDEF UNICODE}
      WTSOpenServerW(PWideChar(WideString(FServer)));
{$ELSE}
      WTSOpenServerA(PAnsiChar(FServer));
{$ENDIF}
      // If WTSOpenServer fails the return value is 0
      // Workaround for (possible) bug, see comments in IsConnectionValid function
      if (FServerHandle = 0) {or (not IsConnectionValid(FServer)) }then
      begin
        // Mark handle as invalid
        FServerHandle := INVALID_HANDLE_VALUE;
        FConnected := False;

        // and raise exception
        raise EJwsclTerminalServerConnectException.CreateFmtWinCall(RsWinCallFailed,
          'WTSOpenServer', ClassName, RsUNTerminalServer, 0, True,
          'WTSOpenServer', ['WTSOpenServer', FServer]);
{        raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
          'WTSOpenServer', ClassName, RsUNTerminalServer, 0, True,
          'WTSOpenServer', ['WTSOpenServer', FServer]);}
      end
      else
      begin
        FConnected := True;
      end;
    end;

  end;
end;

procedure TJwTerminalServer.Disconnect;
begin

  if FServerHandle <> WTS_CURRENT_SERVER_HANDLE then
  begin
    WTSCloseServer(FServerHandle);
  end;

  FServerHandle := INVALID_HANDLE_VALUE;
  FConnected := False;
end;

class function TJwTerminalServer.FileTime2DateTime(FileTime: _FILETIME): TDateTime;
var
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  // Return 0 in case of failure
  Result := 0;
  if not FileTimeToLocalFileTime(FileTime, LocalFileTime) then Exit;
  if not FileTimeToSystemTime(LocalFileTime, SystemTime) then Exit;

  try
    Result := SystemTimeToDateTime(SystemTime);
  except
    // Ignore Exception
  end;
end;

procedure TJwTerminalServer.Shutdown(AShutdownFlag: DWORD);
begin
  if not WTSShutdownSystem(FServerHandle, AShutdownFlag) then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
        'Shutdown', ClassName, RsUNTerminalServer, 0, True,
        '', ['WTSShutdownSystem']);
end;

constructor TJwWTSEventThread.Create(CreateSuspended: Boolean;
  AOwner: TJwTerminalServer);
begin
  inherited Create(CreateSuspended, Format('%s (%s)', [ClassName, AOwner.Server]));
  FreeOnTerminate := False;
  FOwner := AOwner;
end;



procedure TJwWTSEventThread.Execute;
begin
  inherited Execute;

  while not Terminated do
  begin
    // Wait some time to prevent duplicate event dispatch on Windows 2000
    // see http://support.microsoft.com/kb/249315
//    Sleep(50);
    if WTSWaitSystemEvent(FOwner.ServerHandle, WTS_EVENT_ALL, FEventFlag) then
    begin

      if FEventFlag > WTS_EVENT_NONE then
      begin
        Synchronize(DispatchEvent);
      end;
    end
    else if FEventFlag > WTS_EVENT_FLUSH then
    begin
      raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
        'WTSWaitSystemEvent', ClassName, RsUNTerminalServer, 0, True,
        'Server: %s LastEvent: %d', [FOwner.Server, FOwner.LastEventFlag]);
    end;
  end;
end;

procedure TJwWTSEventThread.DispatchEvent;
begin
  if FEventFlag > WTS_EVENT_NONE then
  begin
    FOwner.FireEvent(FEventFlag);
    FEventFlag := WTS_EVENT_NONE;
  end;
end;

constructor TJwWTSEnumServersThread.Create(CreateSuspended: Boolean;
  Owner: TJwTerminalServer; Domain: TJwString);
begin
  JwRaiseOnNilParameter(Owner, 'Owner','Create', ClassName, RsUNTerminalServer);

  inherited Create(CreateSuspended, Format('%s (%s)', [ClassName, Owner.Server]));

  FTerminatedEvent := CreateEvent(nil, False, False, nil);
  FOwner := Owner;
  FDomain := Domain;
  FreeOnTerminate := True;
end;

procedure TJwWTSEnumServersThread.Execute;
type
  PWTS_SERVER_INFO = PWTS_SERVER_INFOA;
var
  ServerInfoPtr: PJwWtsServerInfoAArray;
  pCount: DWORD;
  i: DWORD;
begin
  inherited Execute;

  // Clear the serverlist
  Synchronize(ClearServerList);

  ServerInfoPtr := nil;
  // Since we return to a Stringlist (which does not support unicode)
  // we only use WTSEnumerateServersA
  if WTSEnumerateServersA(PAnsiChar(AnsiString(FDomain)), 0, 1,
    PWTS_SERVER_INFOA(ServerInfoPtr), pCount) then
  begin                                      
    for i := 0 to pCount - 1 do
    begin
      // If the thread is terminated then leave the loop
      if Terminated then Break;
      FServer := String(ServerInfoPtr^[i].pServerName);
      Synchronize(AddToServerList);
    end;

    // Note that on failure of WTSEnumerateServers we don't produce an
    // exception but return an empty ServerList instead. This is by design

    // If we have not been terminated we fire the OnServersEnumerated Event
    if not Terminated then
    begin
      Synchronize(DispatchEvent);
    end;
  end;

  // Cleanup
  if ServerInfoPtr <> nil then
  begin
    WTSFreeMemory(ServerInfoPtr);
  end;

  // Signal Wait procedure that we are finished.
  SetEvent(FTerminatedEvent);
end;

procedure TJwWTSEnumServersThread.AddToServerList;
begin
  FOwner.Servers.Add(FServer);
end;

procedure TJwWTSEnumServersThread.ClearServerList;
begin
  FOwner.Servers.Clear;
end;

procedure TJwWTSEnumServersThread.DispatchEvent;
begin
  if Assigned(FOwner.OnServersEnumerated) then
  begin
    // Fire the OnServersEnumerated event
    FOwner.OnServersEnumerated(FOwner);
  end;
end;


{procedure TJwWTSEnumServersThread.Wait;
var Res: DWORD;
begin
  // we should wait only from the MainThreadId!
  if GetCurrentThreadID = MainThreadID then
  begin
    Res := WAIT_OBJECT_0+1;

    while (Res = WAIT_OBJECT_0+1) do
    begin
      // Wait for the thread to trigger the Terminated Event
      Res := WaitForSingleObject(FTerminatedEvent, INFINITE);
      OutputDebugString('WaitForSingleObject done');
    end;
  end;
end;}

// Borland's WaitFor procedure contains a bug when using Waitfor in combination
// with FreeOnTerminate := True; During the loop in WaitFor the TThread object
// can be freed and its Handle invalidated.  When MsgWaitForMultipleObjects()
// is called again, it fails, and then a call to CheckThreadError() afterwards
// throws on EOSError exception with an error code of 6 and an error message of
//  "the handle is invalid". http://qc.borland.com/wc/qcmain.aspx?d=6080
// Therefore we override the WaitFor function and Create an Exception
{function TJwWTSEnumServersThread.WaitFor: LongWord;
begin
  // Return error
  //Result := ERROR_NOT_SUPPORTED;

  raise EJwsclUnimplemented.CreateFmtWinCall(RsWinCallFailed,
    'WaitFor function is not supported, please use Wait instead', ClassName,
    RsUNTerminalServer, 1203, False,
    'WaitFor function is not supported, please use Wait instead',
    ['TJwWTSEnumServersThread.WaitFor']);
end;}

destructor TJwWTSSessionList.Destroy;
begin
  inherited Destroy;
end;

function TJwWTSSessionList.Add(ASession: TJwWTSSession): Integer;
begin
  Result := inherited Add(ASession);
end;

function TJwWTSSessionList.FindBySessionId(const SessionId: DWORD): TJwWTSSession;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Count - 1 do
  begin
    if Items[i].SessionId = SessionId then
    begin
      Result := Items[i];
      Break;
    end;
  end;

end;

function TJwWTSSessionList.FindByUsername(const Username: TJwString): TJwWTSSession;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Count - 1 do
  begin
    if JwCompareString(Items[i].Username, Username, True) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
  end;

end;


function TJwWTSSessionList.GetEnumerator: TJwSessionsEnumerator;
begin
  Result := TJwSessionsEnumerator.Create(Self);
end;


function TJwWTSSessionList.GetItem(Index: Integer): TJwWTSSession;
begin
  Result := TJwWTSSession(inherited Items[Index]);
end;

function TJwWTSSessionList.IndexOf(ASession: TJwWTSSession): Integer;
begin
  Result := inherited IndexOf(ASession);
end;

procedure TJwWTSSessionList.Insert(Index: Integer; ASession: TJwWTSSession);
begin
  inherited Insert(Index, ASession);
end;

function TJwWTSSessionList.Remove(ASession: TJwWTSSession): Integer;
begin
  Result := inherited Remove(ASession);
end;

procedure TJwWTSSessionList.SetItem(Index: Integer; ASession: TJwWTSSession);
begin
  inherited Items[Index] := ASession;
end;

procedure TJwWTSSessionList.SetOwner(const Value: TJwTerminalServer);
begin
  FOwner := Value;
end;

function TJwWTSProcessList.Add(AProcess: TJwWTSProcess): Integer;
begin
  Result := inherited Add(AProcess);
end;

function TJwWTSProcessList.FindByPid(const PID: Cardinal): TJwWTSProcess;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Self.Count - 1 do
  begin
    if Items[i].FProcessId = PID then
    begin
      Result := Items[i];
    end;
  end;
end;

function TJwWTSProcessList.GetEnumerator: TJwProcessEnumerator;
begin
  Result := TJwProcessEnumerator.Create(Self);
end;

function TJwWTSProcessList.GetItem(Index: Integer): TJwWTSProcess;
begin
  Result := TJwWTSProcess(inherited Items[Index]);
end;

function TJwWTSProcessList.IndexOf(AProcess: TJwWTSProcess): Integer;
begin
  Result := inherited IndexOf(AProcess);
end;

procedure TJwWTSProcessList.Insert(Index: Integer; AProcess: TJwWTSProcess);
begin
  inherited Insert(Index, AProcess);
end;

function TJwWTSProcessList.Remove(AProcess: TJwWTSProcess): Integer;
begin
  Result := inherited Remove(AProcess);
end;

procedure TJwWTSProcessList.SetItem(Index: Integer; AProcess: TJwWTSProcess);
begin
  inherited Items[Index] := AProcess;
end;

constructor TJwWTSSessionShadow.Create(AOwner : TJwWTSSession);
begin
  FOwner := AOwner;
end;

function TJwWTSSessionShadow.GetShadowMode;
begin
  UpdateShadowInformation(False);
  Result := TJwShadowMode(FWinStationShadowInformation.ShadowMode);
end;

function TJwWTSSessionShadow.GetShadowState : TJwShadowState;
begin
  UpdateShadowInformation(False);
  Result := TJwShadowState(FWinStationShadowInformation.CurrentShadowState);
end;

procedure TJwWTSSessionShadow.SetShadowMode(Const Value : TJwShadowMode);
begin
  FWinStationShadowInformation.ShadowMode := Ord(Value);
  UpdateShadowInformation(True);
end;

procedure TJwWTSSessionShadow.UpdateShadowInformation(const Modify : Boolean);
var
  ReturnedLength : DWORD;
begin
  if not Modify then
  begin
  // WinStationShadowInformation = 26
{    if not }WinStationQueryInformationW(FOwner.GetServerHandle, FOwner.SessionId,
     WinStationShadowInfo, @FWinStationShadowInformation,
     SizeOf(FWinstationShadowInformation), ReturnedLength);{ then
      raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
       'UpdateShadowInformation', ClassName, RsUNTerminalServer, 0, True,
       'WinStationQueryInformationW', ['WinStationQueryInformationW']);}
  end
  else
    if not WinStationSetInformationW(FOwner.GetServerHandle, FOwner.SessionId,
     WinStationShadowInfo, @FWinStationShadowInformation,
     SizeOf(FWinstationShadowInformation)) then
      raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
       'UpdateShadowInformation', ClassName, RsUNTerminalServer, 0, True,
       'WinStationSetInformationW', ['WinStationSetInformationW']);
end;

constructor TJwWTSSessionStatistics.Create(const AOwner: TJwTerminalServer);
begin
  JwRaiseOnNilParameter(AOwner, 'AOwner', 'Create', ClassName, RsUNTerminalServer);

  FOwner := AOwner;
  UpdateCounters;
end;

function TJwWTSSessionStatistics.GetCounterValue(const CounterID: Integer): Integer;
begin
  with TSCounterArray[CounterID], counterHead do
  begin
    if bResult then Result := dwValue else Result := -1;
  end;
end;

function TJwWTSSessionStatistics.GetLoadIndicatorData: TWinStationLoadIndicatorData;
var
  dwReturnLength: DWORD;
  bSucceeded: Boolean;
begin
  ZeroMemory(@FLoadIndicatorData, SizeOf(FLoadIndicatorData));
  FOwner.Connect;
  dwReturnLength := 0;
  bSucceeded := WinStationQueryInformationW(FOwner.ServerHandle, 0,
    WinStationLoadIndicator, @FLoadIndicatorData, SizeOf(FLoadIndicatorData),
    dwReturnLength);
  if not bSucceeded then
  begin
    raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
      'TJwWTSSessionStatistics.GetLoadIndicatorData', ClassName, RsUNTerminalServer, 0, True,
          'WinStationQueryInformationW', ['WinStationLoadIndicator']);
  end;

  Result := FLoadIndicatorData;
end;

procedure TJwWTSSessionStatistics.UpdateCounters;
var
  i: Integer;
begin
  { Initialize Array }
  for i := Low(TSCounterArray) to High(TSCounterArray) do
  begin
    TSCounterArray[i].counterHead.dwCounterID := i;
  end;

  { connect to the Terminal Server }
  FOwner.Connect;

  { retrieve the counters }
  if not WinStationGetTermSrvCountersValue(FOwner.ServerHandle,
    Length(TSCounterArray), PTS_COUNTER(@TSCounterArray)) then
  begin
    raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
      'TJwWTSSessionStatistics.UpdateCounters', ClassName, RsUNTerminalServer, 0, True,
          'WinStationGetTermSrvCountersValue', ['WinStationGetTermSrvCountersValue']);
  end;


end;

function TJwTerminalServer.CachedGetUserFromSessionId(const Sessionid: Cardinal): TJwString;
var
  WinStationInfo: _WINSTATIONINFORMATIONW;
  dwReturnLength: DWORD;
begin
  Result := '(unknown)';

  if SessionId = CachedUser.SessionId  then
  begin
    Result := CachedUser.Username;
  end
  else begin
    ZeroMemory(@WinStationInfo, SizeOf(WinStationInfo));

    if WinStationQueryInformationW(ServerHandle, SessionId,
      WinStationInformation, @WinStationInfo, SizeOf(WinStationInfo),
      dwReturnLength) then
    begin
      CachedUser.SessionId := SessionId;
      CachedUser.Username := TJwString(WinStationInfo.UserName);
    end;
  end;
end;

function TJwTerminalServer.GetIdleProcessName: TJwString;
var
  hModule: THandle;
  lpBuffer: PWideChar;
  nBufferMax: Integer;
begin
  // The "System Idle Process" name is language dependant, therefore
  // we obtain it from Taskmgr and cache it in IdleProcessName property
  if FIdleProcessName = '' then
  begin
    hModule := LoadLibrary('taskmgr.exe');
    if hModule > 0 then
    begin
      nBufferMax := 256;  // 256 Chars seems safe for a resource string
      GetMem(lpBuffer, nBufferMax * SizeOf(WCHAR));
      // Windows NT4 and Windows XP Taskmgr have the System Idle Process
      // as resource string 10005. Windows Vista has it in MUI file
      // taskmgr.exe.mui with same id
      if LoadStringW(hModule, 10005, lpBuffer, nBufferMax) > 0 then
      begin
        FIdleProcessName := JwPWideCharToJwString(lpBuffer);
      end;
      // Cleanup
      FreeMem(lpBuffer);
      FreeLibrary(hModule);
    end;
  end;

  Result := FIdleProcessName;
end;

{ This functions reads out the total amount of Physical Memory in Bytes for
  local or remote systems using the registry. Locally user rights are needed
  but for Remote Systems admin righs are needed. If reading the memory fails
  (eg because of access denied) the returned result is 0, any exceptions are
  surpressed }
function TJwTerminalServer.GetPhysicalMemory;
const
  EnvKey: String = '\SYSTEM\CurrentControlSet\Control\Session Manager\Environment';
  EnvVal: String = 'PROCESSOR_ARCHITECTURE';
  PhysMemKey: String = '\HARDWARE\RESOURCEMAP\System Resources\Physical Memory';
  PhysMemVal: String = '.Translated';
var
  Is64Bit: Boolean;
  Reg: TRegistry;
  RootKey: HKEY;
  cbSize: Integer;
  ResourceListPtr: PCM_RESOURCE_LIST;
  PartDescript: PCM_PARTIAL_RESOURCE_DESCRIPTOR;
  PartDescriptEx: PCM_PARTIAL_RESOURCE_DESCRIPTOR_EX;
  Index: Integer;
  Counter: Integer;
  s: TJwString;
begin
  if FPhysicalMemory > -1 then
  begin
    Result := FPhysicalMemory;
    Exit;
  end;

  // Default result = 0
  Result := 0;
  ResourceListPtr := nil;

  // Prepend \\ to machinename if needed
  s := FServer;
  if Pos('\\', s) <> 0 then
  begin
    s := '\\' + s;
  end;

  { Connect to (remote) Registry. Please note there is a bug in Windows 2003 SP1
    from http://support.microsoft.com/kb/906570: After you apply Microsoft
    Windows Server 2003 Service Pack 1 (SP1) or install an x64-based version of
    Windows Server 2003, a custom program that uses the RegConnectRegistry
    function can no longer access the registry of a remote computer.
    I was not able to test if this bug prevents us from reading the value
    remotely }
  if RegConnectRegistry(TJwPChar(s), HKEY_LOCAL_MACHINE, RootKey) <> Integer(ERROR_SUCCESS) then
  begin
    Exit;
  end;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := RootKey;
    { First we read out the Environment variable PROCESSOR_ARCHITECTURE to
      determine is the system is x86 or x64 }
    if Reg.OpenKeyReadOnly(EnvKey) then
    begin
      try
        { Both intel and Amd 64 bit systems have the value AMD64 }
        Is64Bit := CompareText(Reg.ReadString(EnvVal), 'AMD64') = 0;

        // Now open the Physical Memory key
        if Reg.OpenKeyReadOnly(PhysMemKey) then
        begin

          { Get Value Size in bytes }
          cbSize := Reg.GetDataSize(PhysMemVal);

          { Check is Size > 0 }
          if cbSize > 0 then begin

            { Allocate and zero Memory }
            ResourceListPtr := AllocMem(cbSize);

            { Read data into ResourceListPtr var }
            Reg.ReadBinaryData(PhysMemVal, ResourceListPtr^, cbSize);

            { Loop throught the ResourceList(s), usually it's only 1 }
            for Index := 0 to ResourceListPtr^.Count-1 do
            begin
              { x64 systems have a slightly different structure because the
                memory size parameter is not ULONG but ULONGLONG. To solve this we
                assign the PartialDescriptors array to 2 pointers. }

              PartDescript := @ResourceListPtr^.List[Index].PartialResourceList.PartialDescriptors;   // x86
              PartDescriptEx := @ResourceListPtr^.List[Index].PartialResourceList.PartialDescriptors; // x64
              for Counter := 0 to ResourceListPtr^.List[Index].PartialResourceList.Count - 1 do
              begin
                begin
                  if Is64Bit then { for x64 we use PartDescriptEx }

                  begin
                    { Check if the Resourcetype = Memory and that the memory is
                      read/write }
                    if (PartDescriptEx.ResType = CmResourceTypeMemory) and
                      (PartDescriptEx.Flags = CM_RESOURCE_MEMORY_READ_WRITE) then
                    begin
                      Result := Result + PartDescriptEx.Memory.Length;
                    end;
                    { The Inc operator increases the pointer by
                      SizeOf(PartDescriptEx^) which jumps to the next item in
                      the array }
                    Inc(PartDescriptEx);
                  end
                  else begin { for x86 we use PartDescript }
                    { Check if the Resourcetype = Memory and that the memory is
                      read/write }
                    if (PartDescript.ResType = CmResourceTypeMemory) and
                      (PartDescript.Flags = CM_RESOURCE_MEMORY_READ_WRITE) then
                    begin
                      Result := Result + PartDescript.Memory.Length;
                    end;
                    { The Inc operator increases the pointer by
                      SizeOf(PartDescriptEx^) which jumps to the next item in
                      the array }
                    Inc(PartDescript);
                  end;
                end;
              end;
            end;
          end;
        end;
      except
        { Return 0 on any exception }
        Result := 0;
      end;

      { Free the memory }
      if ResourceListPtr <> nil then FreeMem(ResourceListPtr);
    end;

  finally
    { Close the key }
    RegCloseKey(RootKey);
    FreeAndNil(Reg);
  end;

  if Result > 0 then
  begin
    FPhysicalMemory := Result;
  end
  else begin
    Result := -1;
  end;
end;

function TJwTerminalServerList.FindByServer(const ServerName: WideString; const IgnoreCase: boolean = False): TJwTerminalServer;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
  begin
    //if Items[i].Server = AServer then
    if JwCompareString(Items[i].Server,ServerName,IgnoreCase) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
  end;
end;

function TJwTerminalServerList.GetItem(Index: Integer): TJwTerminalServer;
begin
  Result := TJwTerminalServer(inherited Items[Index]);
end;

function TJwTerminalServerList.Add(ATerminalServer: TJwTerminalServer): Integer;
begin
  Result := inherited Add(ATerminalServer);
end;

function TJwTerminalServerList.IndexOf(ATerminalServer: TJwTerminalServer): Integer;
begin
  Result := inherited IndexOf(ATerminalServer);
end;

function TJwTerminalServerList.Remove(ATerminalServer: TJwTerminalServer): Integer;
begin
  Result := inherited Remove(ATerminalServer);
end;

procedure TJwTerminalServerList.SetItem(Index: Integer; ATerminalServer: TJwTerminalServer);
begin
  inherited SetItem(Index, ATerminalServer);
end;

procedure TJwTerminalServerList.SetOwner(const Value: TComponent);
begin
  FOwner := Value;
end;

procedure TJwTerminalServerList.Insert(Index: Integer; ATerminalServer: TJwTerminalServer);
begin
  inherited Insert(Index, ATerminalServer);
end;

destructor TJwTerminalServerList.Destroy;
begin
  inherited Destroy;
end;

procedure TJwWTSProcessList.SetOwner(const Value: TJwTerminalServer);
begin
  FOwner := Value;
end;

function TJwWTSSession.ProtocolTypeToStr(const AProtocolType: DWORD): TJwString;
begin
  //TODO: use resource strings
  case AProtocolType of
    WTS_PROTOCOL_TYPE_CONSOLE: Result := 'Console';
    WTS_PROTOCOL_TYPE_ICA: Result := 'ICA';
    WTS_PROTOCOL_TYPE_RDP: Result := 'RDP';
  else
    Result := '';  // Should never happen
  end;
end;

procedure TJwWTSSession.GetSessionInfoPtr(const WTSInfoClass: _WTS_INFO_CLASS;
  var ABuffer: Pointer);
var
  dwBytesReturned: DWORD;
begin
  ABuffer := nil;
{$IFDEF UNICODE}
    WTSQuerySessionInformationW(GetServerHandle, FSessionId, WTSInfoClass,
      ABuffer, dwBytesReturned);
{$ELSE}
    WTSQuerySessionInformationA(GetServerHandle, FSessionId, WTSInfoClass,
      ABuffer, dwBytesReturned);
{$ENDIF}
end;

function TJwWTSSession.GetSessionInfoStr(const WTSInfoClass: _WTS_INFO_CLASS):
  TJwString;
var
  aBuffer: Pointer;
begin
  result := '';
  GetSessionInfoPtr(WTSInfoClass, aBuffer);
  if ABuffer <> nil then
  begin
    Result := TJwString(TJwPChar(aBuffer));
    WTSFreeMemory(aBuffer);
  end;
end;

function TJwWTSSession.GetSessionInfoDWORD(const WTSInfoClass: _WTS_INFO_CLASS): DWORD;
var
  ABuffer: Pointer;
begin
  result := 0;
  GetSessionInfoPtr(WTSInfoClass, aBuffer);
  if ABuffer <> nil then
  begin
    Result := PDWord(ABuffer)^;
    WTSFreeMemory(ABuffer);
  end;
end;

function TJwWTSSession.GetServerHandle;
begin
  // The ServerHandle is stored in TJwTerminalServer
  JwRaiseOnNilMemoryBlock(Owner, 'GetServerHandle', ClassName, RsUNTerminalServer);
  JwRaiseOnNilMemoryBlock(Owner.Owner, 'GetServerHandle', ClassName, RsUNTerminalServer);

  Result := Owner.Owner.FServerHandle;
end;

procedure TJwWTSSession.GetWinStationDriver;
var
  WinStationDriver: _WD_CONFIGW;
  dwReturnLength: DWORD;
begin
  FWdName := '';
  FWdFlag := 0;
  // ZeroMemory
  ZeroMemory(@WinStationDriver, SizeOf(WinStationDriver));

  // WdConfig = 3
  if WinStationQueryInformationW(GetServerHandle, FSessionId,
    WinStationWd, @WinStationDriver, SizeOf(WinStationDriver),
    dwReturnLength) then
  begin
    FWdName := JwPWideCharToJwString(WinStationDriver.WdName);
    FWdFlag := WinStationDriver.WdFlag;
  end;
end;

// #todo Remove IdleTime helper from JwaWinsta
procedure TJwWTSSession.GetWinStationInformation;
var
//  WinStationInfo: _WINSTATION_INFORMATIONW;
  WinStationInfo: _WINSTATIONINFORMATIONW;
  dwReturnLength: DWORD;
  lpBuffer: PWideChar;
begin
  // ZeroMemory
  ZeroMemory(@WinStationInfo, SizeOf(WinStationInfo));
  lpBuffer := nil;
  // WinStationInformation = 8
  if WinStationQueryInformationW(GetServerHandle, FSessionId,
    WinStationInformation, @WinStationInfo, SizeOf(WinStationInfo),
    dwReturnLength) then
  begin
    // Only Active Session has Logon Time
    if FConnectState = WTSActive then
    begin
      // Reserve memory
      GetMem(lpBuffer, MAX_PATH * SizeOf(WCHAR));
      try
        // Format LogonTime string
        DateTimeStringSafe(@WinStationInfo.LogonTime, lpBuffer, MAX_PATH);
        FLogonTimeStr := JwPWideCharToJwString(lpBuffer);
      finally
        FreeMem(lpBuffer);
        lpBuffer := nil;
      end;

      if FWdFlag > WD_FLAG_CONSOLE then
      begin
        // Counter values (Status from TSAdmin)
        FIncomingBytes := WinStationInfo.Status.Input.WdBytes;
        FIncomingCompressedBytes := WinStationInfo.Status.Input.CompressedBytes;
        FIncomingFrames := WinStationInfo.Status.Input.WdFrames;


        FOutgoingBytes := WinStationInfo.Status.Output.WdBytes;
        FOutgoingCompressBytes := WinStationInfo.Status.Output.CompressedBytes;
        FOutgoingFrames := WinStationInfo.Status.Output.WdFrames;

        // Calculate Compression ratio and store as formatted string
        if FOutgoingBytes > 0 then // 0 division check

        begin
          FCompressionRatio := Format('%1.2f',
            [FOutgoingCompressBytes / FOutgoingBytes]);
        end
        else
          FCompressionRatio := '(inf)'; //infinite output
      end;
    end
    else if FConnectState = WTSDisconnected then
    begin
      // A disconnected session is Idle since DisconnectTime
      WinStationInfo.LastInputTime := WinStationInfo.DisconnectTime;
    end;

    //  Vista returns LastInputTime 0
    if (FUsername = '') or (WinStationInfo.LastInputTime.QuadPart = 0) then
    begin
      // A session without a user is not idle, usually these are special
      // sessions like Listener, Services or console session
      FIdleTimeStr := '.';
     // Store the IdleTime as elapsed seconds
      FIdleTime := 0;
    end
    else
    begin
      // Store the IdleTime as elapsed seconds
      FIdleTime := CalculateDiffTime(WinStationInfo.LastInputTime.QuadPart,
        WinStationInfo.CurrentTime.QuadPart);
      // Calculate & Format Idle Time String, DiffTimeString allocates the
      // memory for us
      DiffTimeString(FileTime(WinStationInfo.LastInputTime), FileTime(WinStationInfo.CurrentTime),
        lpBuffer);
      try
        FIdleTimeStr := JwPWideCharToJwString(lpBuffer);
      finally
        // We free the memory DiffTimeString has allocated for us
        FreeMem(lpBuffer);
      end;
    end;

    FConnectTime := FileTime2DateTime(FileTime(WinStationInfo.ConnectTime));
    FDisconnectTime := FileTime2DateTime(FileTime(WinStationInfo.DisconnectTime));
    // for A disconnected session LastInputTime has been set to DisconnectTime
    FLastInputTime := FileTime2DateTime(FileTime(WinStationInfo.LastInputTime));
    FLogonTime := WinStationInfo.LogonTime.QuadPart;
    FCurrentTime := FileTime2DateTime(FileTime(WinStationInfo.CurrentTime));
    FCurrentTimeEx := WinStationInfo.CurrentTime.QuadPart
  end;

end;

function TJwWTSSession.GetClientAddress: TJwString;
var
  ClientAddressPtr: PWtsClientAddress;
begin
  GetSessionInfoPtr(WTSClientAddress, Pointer(ClientAddressPtr));
  if ClientAddressPtr <> nil then
  begin
    {Note that the first byte of the IP address returned in the ppBuffer
     parameter will be located at an offset of 2 bytes from the start of
     the Address member of the returned WTS_CLIENT_ADDRESS structure.}
    case ClientAddressPtr^.AddressFamily of
      AF_INET:
        Result := Format('%d.%d.%d.%d', [ClientAddressPtr^.Address[2],
          ClientAddressPtr^.Address[3], ClientAddressPtr^.Address[4],
          ClientAddressPtr^.Address[5]]);
      AF_INET6:
        Result := 'IPv6 address not yet supported';
      AF_IPX:
        Result := 'IPX is no longer supported';
      AF_NETBIOS:
        Result := 'NETBIOS is not supported';
      AF_UNSPEC:
        Result := '';
    end;

    // Cleanup
    WTSFreeMemory(ClientAddressPtr);
  end;
end;

procedure TJwWTSSession.GetClientDisplay;
var
  ClientDisplayPtr: PWtsClientDisplay;
begin
  GetSessionInfoPtr(WTSClientDisplay, Pointer(ClientDisplayPtr));
  if ClientDisplayPtr <> nil then
  begin
    FHorizontalResolution := ClientDisplayPtr^.HorizontalResolution;
    FVerticalResolution := ClientDisplayPtr^.VerticalResolution;
    FColorDepth := ClientDisplayPtr^.ColorDepth;
    // Cleanup
    WTSFreeMemory(ClientDisplayPtr);
  end;
end;

{constructor TJwWTSSession.CreateEmpty;
begin
  inherited Create;
end;}

constructor TJwWTSSession.Create(const Owner: TJwWTSSessionList;
  const SessionId: TJwSessionId; const WinStationName: TJwString;
  const ConnectState: TWtsConnectStateClass);
var
  tempStr : WideString;
begin
  JwRaiseOnNilMemoryBlock(Owner, 'Create', ClassName, RsUNTerminalServer);
  JwRaiseOnNilMemoryBlock(Owner.Owner, 'Create', ClassName, RsUNTerminalServer);

  inherited Create;

  FOwner := Owner; // Session is owned by the SessionList
  // First store the SessionID
  FSessionId := SessionId;
  FShadow := TJwWTSSessionShadow.Create(Self);
  FConnectState := ConnectState;
  FConnectStateStr := JwPWideCharToJwString(StrConnectState(FConnectState, False));
  FWinStationName := WinStationName;
  FApplicationName := GetSessionInfoStr(WTSApplicationName);
  FClientAddress := GetClientAddress;
  FClientBuildNumber := GetSessionInfoDWORD(WTSClientBuildNumber);
  FClientDirectory := GetSessionInfoStr(WTSClientDirectory);
  FClientHardwareId := GetSessionInfoDWORD(WTSClientHardwareId);
  FClientName := GetSessionInfoStr(WTSClientName);
  FClientProductId := GetSessionInfoDWORD(WTSClientProductId);
  FClientProtocolType := GetSessionInfoDWORD(WTSClientProtocolType);
  FClientProtocolStr := ProtocolTypeToStr(FClientProtocolType);
  FInitialProgram := GetSessionInfoStr(WTSInitialProgram);
  FWorkingDirectory := GetSessionInfoStr(WTSWorkingDirectory);
  FDomain := GetSessionInfoStr(WTSDomainName);
  FUsername := GetSessionInfoStr(WTSUsername);
  // This retreives WinStationDriver info
  GetWinStationDriver;
  // Retreive WinStationInformation
  GetWinStationInformation;

  // This function queries Terminal Server for the real remote ip address
  // and port (as opposed to WTSClientAddress which retreives the client's
  // local ip address
  tempStr := WideString(FRemoteAddress);
  WinStationGetRemoteIPAddress(GetServerHandle, SessionId, {WideString}tempStr,
    FRemotePort);

  FRemoteAddress := WideString(tempStr);

  FToken := nil;
  FUserSid := nil;
end;

function TJwWTSSession.Connect(const Password: WideString): Boolean;
begin
  Result := WinStationConnectW(Owner.Owner.FServerHandle, WTS_CURRENT_SESSION,
    FSessionId, PWideChar(WideString(Password)), False);
  if not Result then raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
      'Connect', ClassName, RsUNTerminalServer, 0, True,
          'WinStationConnectW', ['WinStationConnectW']);

end;

destructor TJwWTSSession.Destroy;
begin
  FreeAndNil(FShadow);
  FreeAndNil(FToken);
  FreeAndNil(FUserSid);
end;

function TJwWTSSession.GetToken : TJwSecurityToken;
begin
  result := FToken;
  if Assigned(FToken) then
    exit;

  JwRaiseOnNilMemoryBlock(Owner,'GetToken',ClassName,RsUNTerminalServer);


  try
    FToken := TJwSecurityToken.CreateWTSQueryUserTokenEx(Owner.Owner, FSessionId);
  except
    on E : EJwsclSecurityException do
      FToken := nil;
  end;

  result := FToken;
end;

function TJwWTSSession.GetUserSid : TJwSecurityID;
begin
  GetToken;

  result := FUserSid;

  if Assigned(FUserSid) then
    exit;

  if Assigned(FToken) then
  begin
    try
      FUserSid := FToken.GetTokenUser;
    except
      on E : EJwsclSecurityException do
        FUserSid := nil;
    end;
  end
  else
    FUserSid  := nil;

  result := FUserSid;
end;

function TJwWTSSession.GetServer: TJwString;
begin
  JwRaiseOnNilMemoryBlock(Owner, 'GetServerName', ClassName, RsUNTerminalServer);
  JwRaiseOnNilMemoryBlock(Owner.Owner, 'GetServerName', ClassName, RsUNTerminalServer);

  Result := Owner.Owner.Server;
end;

procedure TJwWTSSession.Logoff(bWait: Boolean);
begin
  if not WTSLogoffSession(GetServerHandle, FSessionId, bWait) then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
        'Logoff', ClassName, RsUNTerminalServer, 0, True,
        '', ['WTSLogoffSession']);
end;

procedure TJwWTSSession.Disconnect(bWait: Boolean);
begin
  if not WTSDisconnectSession(GetServerHandle, FSessionId, bWait) then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
        'Disconnect', ClassName, RsUNTerminalServer, 0, True,
        '', ['WTSDisconnectSession']);
end;

function TJwWTSSession.SendMessage(const AMessage: TJwString;
  const ACaption: TJwString; const uType: DWORD; const ATimeOut: DWORD): DWORD;
begin
{$IFDEF UNICODE}
  WTSSendMessageW(GetServerHandle, FSessionId, PWideChar(ACaption),
    Length(ACaption) * SizeOf(WCHAR), PWideChar(AMessage),
    Length(AMessage) * SizeOf(WCHAR), uType, ATimeOut, Result, ATimeOut <> 0);
{$ELSE}
  WTSSendMessageA(GetServerHandle, FSessionId, PAnsiChar(ACaption),
    Length(ACaption), PAnsiChar(AMessage), Length(AMessage), uType, ATimeOut,
    Result, ATimeOut <> 0);
{$ENDIF UNICODE}
end;

class function TJwWTSSession.SendLocalMessage(const SessionID : TJwSessionId; const AMessage: TJwString;
  const ACaption: TJwString; const uType: DWORD; const ATimeOut: DWORD): DWORD;
begin
{$IFDEF UNICODE}
  WTSSendMessageW(0, SessionID, PWideChar(ACaption),
    Length(ACaption) * SizeOf(WCHAR), PWideChar(AMessage),
    Length(AMessage) * SizeOf(WCHAR), uType, ATimeOut, Result, ATimeOut <> 0);
{$ELSE}
  WTSSendMessageA(0, SessionID, PAnsiChar(ACaption),
    Length(ACaption), PAnsiChar(AMessage), Length(AMessage), uType, ATimeOut,
    Result, ATimeOut <> 0);
{$ENDIF UNICODE}
end;



function TJwWTSSession.PostMessage(const AMessage: TJwString;
  const ACaption: TJwString; const uType: DWORD): DWORD;
begin
{$IFDEF UNICODE}
  WTSSendMessageW(GetServerHandle, FSessionId, PWideChar(ACaption),
    Length(ACaption) * SizeOf(WCHAR), PWideChar(AMessage),
    Length(AMessage) * SizeOf(WCHAR), uType, 0, Result, False);
{$ELSE}
    WTSSendMessageA(GetServerHandle, FSessionId, PAnsiChar(ACaption),
      Length(ACaption), PAnsiChar(AMessage), Length(AMessage), uType, 0,
      Result, False);
{$ENDIF UNICODE}
end;

function TJwWTSSession.Shadow(const Hotkey: Cardinal = VK_MULTIPLY;
  const HKModifier: Cardinal = MOD_CONTROL): Boolean;
begin
  // This function only exists in Unicode...
  Result := WinStationShadow(GetServerHandle,
    PWideChar(WideString(GetServer)), FSessionId, HotKey,
    HKModifier);
end;

constructor TJwWTSProcess.Create(const Owner: TJwWTSProcessList;
  const SessionId: TJwSessionId; const ProcessID: TJwProcessId;
  const ProcessName: TJwString; const Username: TjwString);
begin
  JwRaiseOnNilParameter(Owner, 'Owner','TJwWTSProcess.Create', ClassName, RsUNTerminalServer);
  JwRaiseOnNilParameter(Owner.Owner, 'Owner.Owner','TJwWTSProcess.Create', ClassName, RsUNTerminalServer);

  inherited Create;

  FOwner := Owner;
  FSessionID := SessionId;

  FWinStationName := FOwner.Owner.GetWinStationName(SessionId);

  FProcessId := ProcessId;
  FProcessName := ProcessName;
  FUsername := Username;

  FToken := nil;
  FUserSid := nil;

end;

destructor TJwWTSProcess.Destroy;
begin
  FreeAndNil(FToken);
  FreeAndNil(FUserSid);
  inherited;
end;

function TJwWTSProcess.GetToken : TJwSecurityToken;
begin
  result := FToken;
  if Assigned(FToken) then
    exit;

  try
    //try to get a token in by all means
    //even if we don't get that much power over it.
    FToken := TJwSecurityToken.CreateTokenByProcessId(ProcessID, MAXIMUM_ALLOWED);
  except
    //any exception returns a nil pointer
    on E : EJwsclSecurityException do
      FToken := nil;
  end;

  result := FToken;
end;

function TJwWTSProcess.GetUserSid : TJwSecurityID;
begin
  GetToken;

  result := FUserSid;

  if Assigned(FUserSid) then
    exit;

  if Assigned(FToken) then
  begin
    try
      FUserSid := FToken.GetTokenUser;
    except
      on E : EJwsclSecurityException do
        FUserSid := nil;
    end;
  end
  else
    FUserSid  := nil;

  result := FUserSid;
end;

function TJwWTSProcess.GetServer;
begin
  JwRaiseOnNilMemoryBlock(Owner, 'GetServerName', ClassName, RsUNTerminalServer);
  JwRaiseOnNilMemoryBlock(Owner.Owner, 'GetServerName', ClassName, RsUNTerminalServer);

  // The Server is stored in TJwTerminalServer
  Result := Owner.Owner.Server;
end;

function TJwWTSProcess.GetServerHandle: THandle;
begin
  JwRaiseOnNilMemoryBlock(Owner, 'TJwWTSProcess.GetServerHandle', ClassName, RsUNTerminalServer);
  JwRaiseOnNilMemoryBlock(Owner.Owner, 'TJwWTSProcess.GetServerHandle', ClassName, RsUNTerminalServer);

  // The ServerHandle is stored in TJwTerminalServer
  Result := Owner.Owner.FServerHandle;
end;

function TjwWTSProcess.Terminate: Boolean;
begin
  Result := WTSTerminateProcess(GetServerHandle, ProcessId, 0);
  if not Result then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
        'Terminate', ClassName, RsUNTerminalServer, 0, True,
        '', ['WTSTerminate']);
end;

function TJwWTSProcess.Terminate(const dwExitCode: DWORD): Boolean;
begin
  Result := WTSTerminateProcess(GetServerHandle, ProcessId, dwExitCode);
  if not Result then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
        'Terminate', ClassName, RsUNTerminalServer, 0, True,
        '', ['WTSTerminate']);
end;


constructor TJwSessionsEnumerator.Create(ASessionList: TJwWTSSessionList);
begin
  inherited Create;
  FIndex := -1;
  FSessions := ASessionList;
end;

function TJwSessionsEnumerator.GetCurrent;
begin
  Result := FSessions[FIndex];
end;

function TJwSessionsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FSessions.Count - 1;
  if Result then
    Inc(FIndex);
end;

constructor TJwProcessEnumerator.Create(AProcessList: TJwWTSProcessList);
begin
  inherited Create;
  FIndex := -1;
  FProcesses := AProcessList;
end;

function TJwProcessEnumerator.GetCurrent;
begin
  Result := FProcesses[FIndex];
end;

function TJwProcessEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FProcesses.Count - 1;
  if Result then
    Inc(FIndex);
end;


{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}

