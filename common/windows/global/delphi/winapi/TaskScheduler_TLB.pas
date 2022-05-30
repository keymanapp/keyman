unit TaskScheduler_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 52393 $
// File generated on 27/10/2020 6:32:08 PM from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Windows\SysWOW64\taskschd.dll (1)
// LIBID: {E34CB9F1-C7F7-424C-BE29-027DCC09363A}
// LCID: 0
// Helpfile: 
// HelpString: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// SYS_KIND: SYS_WIN32
// Errors:
//   Hint: TypeInfo 'TaskScheduler' changed to 'TaskScheduler_'
//   Hint: Parameter 'Type' of ITriggerCollection.Create changed to 'Type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Parameter 'Type' of IActionCollection.Create changed to 'Type_'
//   Hint: Member 'To' of 'IEmailAction' changed to 'To_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Winapi.ActiveX;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  TaskSchedulerMajorVersion = 1;
  TaskSchedulerMinorVersion = 0;

  LIBID_TaskScheduler: TGUID = '{E34CB9F1-C7F7-424C-BE29-027DCC09363A}';

  IID_ITaskFolderCollection: TGUID = '{79184A66-8664-423F-97F1-637356A5D812}';
  IID_ITaskFolder: TGUID = '{8CFAC062-A080-4C15-9A88-AA7C2AF80DFC}';
  IID_IRegisteredTask: TGUID = '{9C86F320-DEE3-4DD1-B972-A303F26B061E}';
  IID_IRunningTask: TGUID = '{653758FB-7B9A-4F1E-A471-BEEB8E9B834E}';
  IID_IRunningTaskCollection: TGUID = '{6A67614B-6828-4FEC-AA54-6D52E8F1F2DB}';
  IID_ITaskDefinition: TGUID = '{F5BC8FC5-536D-4F77-B852-FBC1356FDEB6}';
  IID_IRegistrationInfo: TGUID = '{416D8B73-CB41-4EA1-805C-9BE9A5AC4A74}';
  IID_ITriggerCollection: TGUID = '{85DF5081-1B24-4F32-878A-D9D14DF4CB77}';
  IID_ITrigger: TGUID = '{09941815-EA89-4B5B-89E0-2A773801FAC3}';
  IID_IRepetitionPattern: TGUID = '{7FB9ACF1-26BE-400E-85B5-294B9C75DFD6}';
  IID_ITaskSettings: TGUID = '{8FD4711D-2D02-4C8C-87E3-EFF699DE127E}';
  IID_IIdleSettings: TGUID = '{84594461-0053-4342-A8FD-088FABF11F32}';
  IID_INetworkSettings: TGUID = '{9F7DEA84-C30B-4245-80B6-00E9F646F1B4}';
  IID_IPrincipal: TGUID = '{D98D51E5-C9B4-496A-A9C1-18980261CF0F}';
  IID_IActionCollection: TGUID = '{02820E19-7B98-4ED2-B2E8-FDCCCEFF619B}';
  IID_IAction: TGUID = '{BAE54997-48B1-4CBE-9965-D6BE263EBEA4}';
  IID_IRegisteredTaskCollection: TGUID = '{86627EB4-42A7-41E4-A4D9-AC33A72F2D52}';
  IID_ITaskService: TGUID = '{2FABA4C7-4DA9-4013-9697-20CC3FD40F85}';
  IID_ITaskHandler: TGUID = '{839D7762-5121-4009-9234-4F0D19394F04}';
  IID_ITaskHandlerStatus: TGUID = '{EAEC7A8F-27A0-4DDC-8675-14726A01A38A}';
  IID_ITaskVariables: TGUID = '{3E4C9351-D966-4B8B-BB87-CEBA68BB0107}';
  IID_ITaskNamedValuePair: TGUID = '{39038068-2B46-4AFD-8662-7BB6F868D221}';
  IID_ITaskNamedValueCollection: TGUID = '{B4EF826B-63C3-46E4-A504-EF69E4F7EA4D}';
  IID_IIdleTrigger: TGUID = '{D537D2B0-9FB3-4D34-9739-1FF5CE7B1EF3}';
  IID_ILogonTrigger: TGUID = '{72DADE38-FAE4-4B3E-BAF4-5D009AF02B1C}';
  IID_ISessionStateChangeTrigger: TGUID = '{754DA71B-4385-4475-9DD9-598294FA3641}';
  IID_IEventTrigger: TGUID = '{D45B0167-9653-4EEF-B94F-0732CA7AF251}';
  IID_ITimeTrigger: TGUID = '{B45747E0-EBA7-4276-9F29-85C5BB300006}';
  IID_IDailyTrigger: TGUID = '{126C5CD8-B288-41D5-8DBF-E491446ADC5C}';
  IID_IWeeklyTrigger: TGUID = '{5038FC98-82FF-436D-8728-A512A57C9DC1}';
  IID_IMonthlyTrigger: TGUID = '{97C45EF1-6B02-4A1A-9C0E-1EBFBA1500AC}';
  IID_IMonthlyDOWTrigger: TGUID = '{77D025A3-90FA-43AA-B52E-CDA5499B946A}';
  IID_IBootTrigger: TGUID = '{2A9C35DA-D357-41F4-BBC1-207AC1B1F3CB}';
  IID_IRegistrationTrigger: TGUID = '{4C8FEC3A-C218-4E0C-B23D-629024DB91A2}';
  IID_IExecAction: TGUID = '{4C3D624D-FD6B-49A3-B9B7-09CB3CD3F047}';
  IID_IExecAction2: TGUID = '{F2A82542-BDA5-4E6B-9143-E2BF4F8987B6}';
  IID_IShowMessageAction: TGUID = '{505E9E68-AF89-46B8-A30F-56162A83D537}';
  IID_IComHandlerAction: TGUID = '{6D2FD252-75C5-4F66-90BA-2A7D8CC3039F}';
  IID_IEmailAction: TGUID = '{10F62C64-7E16-4314-A0C2-0C3683F99D40}';
  IID_IPrincipal2: TGUID = '{248919AE-E345-4A6D-8AEB-E0D3165C904E}';
  IID_ITaskSettings2: TGUID = '{2C05C3F0-6EED-4C05-A15F-ED7D7A98A369}';
  IID_ITaskSettings3: TGUID = '{0AD9D0D7-0C7F-4EBB-9A5F-D1C648DCA528}';
  IID_IMaintenanceSettings: TGUID = '{A6024FA8-9652-4ADB-A6BF-5CFCD877A7BA}';
  CLASS_TaskScheduler_: TGUID = '{0F87369F-A4E5-4CFC-BD3E-73E6154572DD}';
  CLASS_TaskHandlerPS: TGUID = '{F2A69DB7-DA2C-4352-9066-86FEE6DACAC9}';
  CLASS_TaskHandlerStatusPS: TGUID = '{9F15266D-D7BA-48F0-93C1-E6895F6FE5AC}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum _TASK_STATE
type
  _TASK_STATE = TOleEnum;
const
  TASK_STATE_UNKNOWN = $00000000;
  TASK_STATE_DISABLED = $00000001;
  TASK_STATE_QUEUED = $00000002;
  TASK_STATE_READY = $00000003;
  TASK_STATE_RUNNING = $00000004;

// Constants for enum _TASK_TRIGGER_TYPE2
type
  _TASK_TRIGGER_TYPE2 = TOleEnum;
const
  TASK_TRIGGER_EVENT = $00000000;
  TASK_TRIGGER_TIME = $00000001;
  TASK_TRIGGER_DAILY = $00000002;
  TASK_TRIGGER_WEEKLY = $00000003;
  TASK_TRIGGER_MONTHLY = $00000004;
  TASK_TRIGGER_MONTHLYDOW = $00000005;
  TASK_TRIGGER_IDLE = $00000006;
  TASK_TRIGGER_REGISTRATION = $00000007;
  TASK_TRIGGER_BOOT = $00000008;
  TASK_TRIGGER_LOGON = $00000009;
  TASK_TRIGGER_SESSION_STATE_CHANGE = $0000000B;
  TASK_TRIGGER_CUSTOM_TRIGGER_01 = $0000000C;

// Constants for enum _TASK_INSTANCES_POLICY
type
  _TASK_INSTANCES_POLICY = TOleEnum;
const
  TASK_INSTANCES_PARALLEL = $00000000;
  TASK_INSTANCES_QUEUE = $00000001;
  TASK_INSTANCES_IGNORE_NEW = $00000002;
  TASK_INSTANCES_STOP_EXISTING = $00000003;

// Constants for enum _TASK_COMPATIBILITY
type
  _TASK_COMPATIBILITY = TOleEnum;
const
  TASK_COMPATIBILITY_AT = $00000000;
  TASK_COMPATIBILITY_V1 = $00000001;
  TASK_COMPATIBILITY_V2 = $00000002;
  TASK_COMPATIBILITY_V2_1 = $00000003;
  TASK_COMPATIBILITY_V2_2 = $00000004;
  TASK_COMPATIBILITY_V2_3 = $00000005;
  TASK_COMPATIBILITY_V2_4 = $00000006;

// Constants for enum _TASK_LOGON_TYPE
type
  _TASK_LOGON_TYPE = TOleEnum;
const
  TASK_LOGON_NONE = $00000000;
  TASK_LOGON_PASSWORD = $00000001;
  TASK_LOGON_S4U = $00000002;
  TASK_LOGON_INTERACTIVE_TOKEN = $00000003;
  TASK_LOGON_GROUP = $00000004;
  TASK_LOGON_SERVICE_ACCOUNT = $00000005;
  TASK_LOGON_INTERACTIVE_TOKEN_OR_PASSWORD = $00000006;

// Constants for enum _TASK_RUNLEVEL
type
  _TASK_RUNLEVEL = TOleEnum;
const
  TASK_RUNLEVEL_LUA = $00000000;
  TASK_RUNLEVEL_HIGHEST = $00000001;

// Constants for enum _TASK_ACTION_TYPE
type
  _TASK_ACTION_TYPE = TOleEnum;
const
  TASK_ACTION_EXEC = $00000000;
  TASK_ACTION_COM_HANDLER = $00000005;
  TASK_ACTION_SEND_EMAIL = $00000006;
  TASK_ACTION_SHOW_MESSAGE = $00000007;

// Constants for enum _TASK_SESSION_STATE_CHANGE_TYPE
type
  _TASK_SESSION_STATE_CHANGE_TYPE = TOleEnum;
const
  TASK_CONSOLE_CONNECT = $00000001;
  TASK_CONSOLE_DISCONNECT = $00000002;
  TASK_REMOTE_CONNECT = $00000003;
  TASK_REMOTE_DISCONNECT = $00000004;
  TASK_SESSION_LOCK = $00000007;
  TASK_SESSION_UNLOCK = $00000008;

// Constants for enum _TASK_PROCESSTOKENSID
type
  _TASK_PROCESSTOKENSID = TOleEnum;
const
  TASK_PROCESSTOKENSID_NONE = $00000000;
  TASK_PROCESSTOKENSID_UNRESTRICTED = $00000001;
  TASK_PROCESSTOKENSID_DEFAULT = $00000002;

// Constants for enum _TASK_RUN_FLAGS
type
  _TASK_RUN_FLAGS = TOleEnum;
const
  TASK_RUN_NO_FLAGS = $00000000;
  TASK_RUN_AS_SELF = $00000001;
  TASK_RUN_IGNORE_CONSTRAINTS = $00000002;
  TASK_RUN_USE_SESSION_ID = $00000004;
  TASK_RUN_USER_SID = $00000008;

// Constants for enum _TASK_ENUM_FLAGS
type
  _TASK_ENUM_FLAGS = TOleEnum;
const
  TASK_ENUM_HIDDEN = $00000001;

// Constants for enum _TASK_CREATION
type
  _TASK_CREATION = TOleEnum;
const
  TASK_VALIDATE_ONLY = $00000001;
  TASK_CREATE = $00000002;
  TASK_UPDATE = $00000004;
  TASK_CREATE_OR_UPDATE = $00000006;
  TASK_DISABLE = $00000008;
  TASK_DONT_ADD_PRINCIPAL_ACE = $00000010;
  TASK_IGNORE_REGISTRATION_TRIGGERS = $00000020;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ITaskFolderCollection = interface;
  ITaskFolderCollectionDisp = dispinterface;
  ITaskFolder = interface;
  ITaskFolderDisp = dispinterface;
  IRegisteredTask = interface;
  IRegisteredTaskDisp = dispinterface;
  IRunningTask = interface;
  IRunningTaskDisp = dispinterface;
  IRunningTaskCollection = interface;
  IRunningTaskCollectionDisp = dispinterface;
  ITaskDefinition = interface;
  ITaskDefinitionDisp = dispinterface;
  IRegistrationInfo = interface;
  IRegistrationInfoDisp = dispinterface;
  ITriggerCollection = interface;
  ITriggerCollectionDisp = dispinterface;
  ITrigger = interface;
  ITriggerDisp = dispinterface;
  IRepetitionPattern = interface;
  IRepetitionPatternDisp = dispinterface;
  ITaskSettings = interface;
  ITaskSettingsDisp = dispinterface;
  IIdleSettings = interface;
  IIdleSettingsDisp = dispinterface;
  INetworkSettings = interface;
  INetworkSettingsDisp = dispinterface;
  IPrincipal = interface;
  IPrincipalDisp = dispinterface;
  IActionCollection = interface;
  IActionCollectionDisp = dispinterface;
  IAction = interface;
  IActionDisp = dispinterface;
  IRegisteredTaskCollection = interface;
  IRegisteredTaskCollectionDisp = dispinterface;
  ITaskService = interface;
  ITaskServiceDisp = dispinterface;
  ITaskHandler = interface;
  ITaskHandlerStatus = interface;
  ITaskVariables = interface;
  ITaskNamedValuePair = interface;
  ITaskNamedValuePairDisp = dispinterface;
  ITaskNamedValueCollection = interface;
  ITaskNamedValueCollectionDisp = dispinterface;
  IIdleTrigger = interface;
  IIdleTriggerDisp = dispinterface;
  ILogonTrigger = interface;
  ILogonTriggerDisp = dispinterface;
  ISessionStateChangeTrigger = interface;
  ISessionStateChangeTriggerDisp = dispinterface;
  IEventTrigger = interface;
  IEventTriggerDisp = dispinterface;
  ITimeTrigger = interface;
  ITimeTriggerDisp = dispinterface;
  IDailyTrigger = interface;
  IDailyTriggerDisp = dispinterface;
  IWeeklyTrigger = interface;
  IWeeklyTriggerDisp = dispinterface;
  IMonthlyTrigger = interface;
  IMonthlyTriggerDisp = dispinterface;
  IMonthlyDOWTrigger = interface;
  IMonthlyDOWTriggerDisp = dispinterface;
  IBootTrigger = interface;
  IBootTriggerDisp = dispinterface;
  IRegistrationTrigger = interface;
  IRegistrationTriggerDisp = dispinterface;
  IExecAction = interface;
  IExecActionDisp = dispinterface;
  IExecAction2 = interface;
  IExecAction2Disp = dispinterface;
  IShowMessageAction = interface;
  IShowMessageActionDisp = dispinterface;
  IComHandlerAction = interface;
  IComHandlerActionDisp = dispinterface;
  IEmailAction = interface;
  IEmailActionDisp = dispinterface;
  IPrincipal2 = interface;
  IPrincipal2Disp = dispinterface;
  ITaskSettings2 = interface;
  ITaskSettings2Disp = dispinterface;
  ITaskSettings3 = interface;
  ITaskSettings3Disp = dispinterface;
  IMaintenanceSettings = interface;
  IMaintenanceSettingsDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  TaskScheduler_ = ITaskService;
  TaskHandlerPS = ITaskHandler;
  TaskHandlerStatusPS = ITaskHandlerStatus;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PUserType1 = ^_SYSTEMTIME; {*}

{$ALIGN 2}
  _SYSTEMTIME = record
    wYear: Word;
    wMonth: Word;
    wDayOfWeek: Word;
    wDay: Word;
    wHour: Word;
    wMinute: Word;
    wSecond: Word;
    wMilliseconds: Word;
  end;


// *********************************************************************//
// Interface: ITaskFolderCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {79184A66-8664-423F-97F1-637356A5D812}
// *********************************************************************//
  ITaskFolderCollection = interface(IDispatch)
    ['{79184A66-8664-423F-97F1-637356A5D812}']
    function Get_Count: Integer; safecall;
    function Get_Item(index: OleVariant): ITaskFolder; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[index: OleVariant]: ITaskFolder read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ITaskFolderCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {79184A66-8664-423F-97F1-637356A5D812}
// *********************************************************************//
  ITaskFolderCollectionDisp = dispinterface
    ['{79184A66-8664-423F-97F1-637356A5D812}']
    property Count: Integer readonly dispid 1610743808;
    property Item[index: OleVariant]: ITaskFolder readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ITaskFolder
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8CFAC062-A080-4C15-9A88-AA7C2AF80DFC}
// *********************************************************************//
  ITaskFolder = interface(IDispatch)
    ['{8CFAC062-A080-4C15-9A88-AA7C2AF80DFC}']
    function Get_Name: WideString; safecall;
    function Get_Path: WideString; safecall;
    function GetFolder(const Path: WideString): ITaskFolder; safecall;
    function GetFolders(flags: Integer): ITaskFolderCollection; safecall;
    function CreateFolder(const subFolderName: WideString; sddl: OleVariant): ITaskFolder; safecall;
    procedure DeleteFolder(const subFolderName: WideString; flags: Integer); safecall;
    function GetTask(const Path: WideString): IRegisteredTask; safecall;
    function GetTasks(flags: Integer): IRegisteredTaskCollection; safecall;
    procedure DeleteTask(const Name: WideString; flags: Integer); safecall;
    function RegisterTask(const Path: WideString; const XmlText: WideString; flags: Integer; 
                          UserId: OleVariant; password: OleVariant; LogonType: _TASK_LOGON_TYPE; 
                          sddl: OleVariant): IRegisteredTask; safecall;
    function RegisterTaskDefinition(const Path: WideString; const pDefinition: ITaskDefinition; 
                                    flags: Integer; UserId: OleVariant; password: OleVariant; 
                                    LogonType: _TASK_LOGON_TYPE; sddl: OleVariant): IRegisteredTask; safecall;
    function GetSecurityDescriptor(securityInformation: Integer): WideString; safecall;
    procedure SetSecurityDescriptor(const sddl: WideString; flags: Integer); safecall;
    property Name: WideString read Get_Name;
    property Path: WideString read Get_Path;
  end;

// *********************************************************************//
// DispIntf:  ITaskFolderDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8CFAC062-A080-4C15-9A88-AA7C2AF80DFC}
// *********************************************************************//
  ITaskFolderDisp = dispinterface
    ['{8CFAC062-A080-4C15-9A88-AA7C2AF80DFC}']
    property Name: WideString readonly dispid 1;
    property Path: WideString readonly dispid 0;
    function GetFolder(const Path: WideString): ITaskFolder; dispid 3;
    function GetFolders(flags: Integer): ITaskFolderCollection; dispid 4;
    function CreateFolder(const subFolderName: WideString; sddl: OleVariant): ITaskFolder; dispid 5;
    procedure DeleteFolder(const subFolderName: WideString; flags: Integer); dispid 6;
    function GetTask(const Path: WideString): IRegisteredTask; dispid 7;
    function GetTasks(flags: Integer): IRegisteredTaskCollection; dispid 8;
    procedure DeleteTask(const Name: WideString; flags: Integer); dispid 9;
    function RegisterTask(const Path: WideString; const XmlText: WideString; flags: Integer; 
                          UserId: OleVariant; password: OleVariant; LogonType: _TASK_LOGON_TYPE; 
                          sddl: OleVariant): IRegisteredTask; dispid 10;
    function RegisterTaskDefinition(const Path: WideString; const pDefinition: ITaskDefinition; 
                                    flags: Integer; UserId: OleVariant; password: OleVariant; 
                                    LogonType: _TASK_LOGON_TYPE; sddl: OleVariant): IRegisteredTask; dispid 11;
    function GetSecurityDescriptor(securityInformation: Integer): WideString; dispid 12;
    procedure SetSecurityDescriptor(const sddl: WideString; flags: Integer); dispid 13;
  end;

// *********************************************************************//
// Interface: IRegisteredTask
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {9C86F320-DEE3-4DD1-B972-A303F26B061E}
// *********************************************************************//
  IRegisteredTask = interface(IDispatch)
    ['{9C86F320-DEE3-4DD1-B972-A303F26B061E}']
    function Get_Name: WideString; safecall;
    function Get_Path: WideString; safecall;
    function Get_State: _TASK_STATE; safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(pEnabled: WordBool); safecall;
    function Run(params: OleVariant): IRunningTask; safecall;
    function RunEx(params: OleVariant; flags: Integer; sessionID: Integer; const user: WideString): IRunningTask; safecall;
    function GetInstances(flags: Integer): IRunningTaskCollection; safecall;
    function Get_LastRunTime: TDateTime; safecall;
    function Get_LastTaskResult: Integer; safecall;
    function Get_NumberOfMissedRuns: Integer; safecall;
    function Get_NextRunTime: TDateTime; safecall;
    function Get_Definition: ITaskDefinition; safecall;
    function Get_Xml: WideString; safecall;
    function GetSecurityDescriptor(securityInformation: Integer): WideString; safecall;
    procedure SetSecurityDescriptor(const sddl: WideString; flags: Integer); safecall;
    procedure Stop(flags: Integer); safecall;
    procedure GetRunTimes(var pstStart: _SYSTEMTIME; var pstEnd: _SYSTEMTIME; var pCount: LongWord; 
                          out pRunTimes: PUserType1); safecall;
    property Name: WideString read Get_Name;
    property Path: WideString read Get_Path;
    property State: _TASK_STATE read Get_State;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property LastRunTime: TDateTime read Get_LastRunTime;
    property LastTaskResult: Integer read Get_LastTaskResult;
    property NumberOfMissedRuns: Integer read Get_NumberOfMissedRuns;
    property NextRunTime: TDateTime read Get_NextRunTime;
    property Definition: ITaskDefinition read Get_Definition;
    property Xml: WideString read Get_Xml;
  end;

// *********************************************************************//
// DispIntf:  IRegisteredTaskDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {9C86F320-DEE3-4DD1-B972-A303F26B061E}
// *********************************************************************//
  IRegisteredTaskDisp = dispinterface
    ['{9C86F320-DEE3-4DD1-B972-A303F26B061E}']
    property Name: WideString readonly dispid 1;
    property Path: WideString readonly dispid 0;
    property State: _TASK_STATE readonly dispid 2;
    property Enabled: WordBool dispid 3;
    function Run(params: OleVariant): IRunningTask; dispid 5;
    function RunEx(params: OleVariant; flags: Integer; sessionID: Integer; const user: WideString): IRunningTask; dispid 6;
    function GetInstances(flags: Integer): IRunningTaskCollection; dispid 7;
    property LastRunTime: TDateTime readonly dispid 8;
    property LastTaskResult: Integer readonly dispid 9;
    property NumberOfMissedRuns: Integer readonly dispid 11;
    property NextRunTime: TDateTime readonly dispid 12;
    property Definition: ITaskDefinition readonly dispid 13;
    property Xml: WideString readonly dispid 14;
    function GetSecurityDescriptor(securityInformation: Integer): WideString; dispid 15;
    procedure SetSecurityDescriptor(const sddl: WideString; flags: Integer); dispid 16;
    procedure Stop(flags: Integer); dispid 17;
    procedure GetRunTimes(var pstStart: {NOT_OLEAUTO(_SYSTEMTIME)}OleVariant; 
                          var pstEnd: {NOT_OLEAUTO(_SYSTEMTIME)}OleVariant; var pCount: LongWord; 
                          out pRunTimes: {NOT_OLEAUTO(PUserType1)}OleVariant); dispid 1610743825;
  end;

// *********************************************************************//
// Interface: IRunningTask
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {653758FB-7B9A-4F1E-A471-BEEB8E9B834E}
// *********************************************************************//
  IRunningTask = interface(IDispatch)
    ['{653758FB-7B9A-4F1E-A471-BEEB8E9B834E}']
    function Get_Name: WideString; safecall;
    function Get_InstanceGuid: WideString; safecall;
    function Get_Path: WideString; safecall;
    function Get_State: _TASK_STATE; safecall;
    function Get_CurrentAction: WideString; safecall;
    procedure Stop; safecall;
    procedure Refresh; safecall;
    function Get_EnginePID: LongWord; safecall;
    property Name: WideString read Get_Name;
    property InstanceGuid: WideString read Get_InstanceGuid;
    property Path: WideString read Get_Path;
    property State: _TASK_STATE read Get_State;
    property CurrentAction: WideString read Get_CurrentAction;
    property EnginePID: LongWord read Get_EnginePID;
  end;

// *********************************************************************//
// DispIntf:  IRunningTaskDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {653758FB-7B9A-4F1E-A471-BEEB8E9B834E}
// *********************************************************************//
  IRunningTaskDisp = dispinterface
    ['{653758FB-7B9A-4F1E-A471-BEEB8E9B834E}']
    property Name: WideString readonly dispid 1;
    property InstanceGuid: WideString readonly dispid 0;
    property Path: WideString readonly dispid 2;
    property State: _TASK_STATE readonly dispid 3;
    property CurrentAction: WideString readonly dispid 4;
    procedure Stop; dispid 5;
    procedure Refresh; dispid 6;
    property EnginePID: LongWord readonly dispid 7;
  end;

// *********************************************************************//
// Interface: IRunningTaskCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {6A67614B-6828-4FEC-AA54-6D52E8F1F2DB}
// *********************************************************************//
  IRunningTaskCollection = interface(IDispatch)
    ['{6A67614B-6828-4FEC-AA54-6D52E8F1F2DB}']
    function Get_Count: Integer; safecall;
    function Get_Item(index: OleVariant): IRunningTask; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[index: OleVariant]: IRunningTask read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IRunningTaskCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {6A67614B-6828-4FEC-AA54-6D52E8F1F2DB}
// *********************************************************************//
  IRunningTaskCollectionDisp = dispinterface
    ['{6A67614B-6828-4FEC-AA54-6D52E8F1F2DB}']
    property Count: Integer readonly dispid 1;
    property Item[index: OleVariant]: IRunningTask readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ITaskDefinition
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {F5BC8FC5-536D-4F77-B852-FBC1356FDEB6}
// *********************************************************************//
  ITaskDefinition = interface(IDispatch)
    ['{F5BC8FC5-536D-4F77-B852-FBC1356FDEB6}']
    function Get_RegistrationInfo: IRegistrationInfo; safecall;
    procedure Set_RegistrationInfo(const ppRegistrationInfo: IRegistrationInfo); safecall;
    function Get_Triggers: ITriggerCollection; safecall;
    procedure Set_Triggers(const ppTriggers: ITriggerCollection); safecall;
    function Get_Settings: ITaskSettings; safecall;
    procedure Set_Settings(const ppSettings: ITaskSettings); safecall;
    function Get_Data: WideString; safecall;
    procedure Set_Data(const pData: WideString); safecall;
    function Get_Principal: IPrincipal; safecall;
    procedure Set_Principal(const ppPrincipal: IPrincipal); safecall;
    function Get_Actions: IActionCollection; safecall;
    procedure Set_Actions(const ppActions: IActionCollection); safecall;
    function Get_XmlText: WideString; safecall;
    procedure Set_XmlText(const pXml: WideString); safecall;
    property RegistrationInfo: IRegistrationInfo read Get_RegistrationInfo write Set_RegistrationInfo;
    property Triggers: ITriggerCollection read Get_Triggers write Set_Triggers;
    property Settings: ITaskSettings read Get_Settings write Set_Settings;
    property Data: WideString read Get_Data write Set_Data;
    property Principal: IPrincipal read Get_Principal write Set_Principal;
    property Actions: IActionCollection read Get_Actions write Set_Actions;
    property XmlText: WideString read Get_XmlText write Set_XmlText;
  end;

// *********************************************************************//
// DispIntf:  ITaskDefinitionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {F5BC8FC5-536D-4F77-B852-FBC1356FDEB6}
// *********************************************************************//
  ITaskDefinitionDisp = dispinterface
    ['{F5BC8FC5-536D-4F77-B852-FBC1356FDEB6}']
    property RegistrationInfo: IRegistrationInfo dispid 1;
    property Triggers: ITriggerCollection dispid 2;
    property Settings: ITaskSettings dispid 7;
    property Data: WideString dispid 11;
    property Principal: IPrincipal dispid 12;
    property Actions: IActionCollection dispid 13;
    property XmlText: WideString dispid 14;
  end;

// *********************************************************************//
// Interface: IRegistrationInfo
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {416D8B73-CB41-4EA1-805C-9BE9A5AC4A74}
// *********************************************************************//
  IRegistrationInfo = interface(IDispatch)
    ['{416D8B73-CB41-4EA1-805C-9BE9A5AC4A74}']
    function Get_Description: WideString; safecall;
    procedure Set_Description(const pDescription: WideString); safecall;
    function Get_Author: WideString; safecall;
    procedure Set_Author(const pAuthor: WideString); safecall;
    function Get_Version: WideString; safecall;
    procedure Set_Version(const pVersion: WideString); safecall;
    function Get_Date: WideString; safecall;
    procedure Set_Date(const pDate: WideString); safecall;
    function Get_Documentation: WideString; safecall;
    procedure Set_Documentation(const pDocumentation: WideString); safecall;
    function Get_XmlText: WideString; safecall;
    procedure Set_XmlText(const pText: WideString); safecall;
    function Get_URI: WideString; safecall;
    procedure Set_URI(const pUri: WideString); safecall;
    function Get_SecurityDescriptor: OleVariant; safecall;
    procedure Set_SecurityDescriptor(pSddl: OleVariant); safecall;
    function Get_Source: WideString; safecall;
    procedure Set_Source(const pSource: WideString); safecall;
    property Description: WideString read Get_Description write Set_Description;
    property Author: WideString read Get_Author write Set_Author;
    property Version: WideString read Get_Version write Set_Version;
    property Date: WideString read Get_Date write Set_Date;
    property Documentation: WideString read Get_Documentation write Set_Documentation;
    property XmlText: WideString read Get_XmlText write Set_XmlText;
    property URI: WideString read Get_URI write Set_URI;
    property SecurityDescriptor: OleVariant read Get_SecurityDescriptor write Set_SecurityDescriptor;
    property Source: WideString read Get_Source write Set_Source;
  end;

// *********************************************************************//
// DispIntf:  IRegistrationInfoDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {416D8B73-CB41-4EA1-805C-9BE9A5AC4A74}
// *********************************************************************//
  IRegistrationInfoDisp = dispinterface
    ['{416D8B73-CB41-4EA1-805C-9BE9A5AC4A74}']
    property Description: WideString dispid 1;
    property Author: WideString dispid 2;
    property Version: WideString dispid 4;
    property Date: WideString dispid 5;
    property Documentation: WideString dispid 6;
    property XmlText: WideString dispid 9;
    property URI: WideString dispid 10;
    property SecurityDescriptor: OleVariant dispid 11;
    property Source: WideString dispid 12;
  end;

// *********************************************************************//
// Interface: ITriggerCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {85DF5081-1B24-4F32-878A-D9D14DF4CB77}
// *********************************************************************//
  ITriggerCollection = interface(IDispatch)
    ['{85DF5081-1B24-4F32-878A-D9D14DF4CB77}']
    function Get_Count: Integer; safecall;
    function Get_Item(index: Integer): ITrigger; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Create(Type_: _TASK_TRIGGER_TYPE2): ITrigger; safecall;
    procedure Remove(index: OleVariant); safecall;
    procedure Clear; safecall;
    property Count: Integer read Get_Count;
    property Item[index: Integer]: ITrigger read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ITriggerCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {85DF5081-1B24-4F32-878A-D9D14DF4CB77}
// *********************************************************************//
  ITriggerCollectionDisp = dispinterface
    ['{85DF5081-1B24-4F32-878A-D9D14DF4CB77}']
    property Count: Integer readonly dispid 1;
    property Item[index: Integer]: ITrigger readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
    function Create(Type_: _TASK_TRIGGER_TYPE2): ITrigger; dispid 2;
    procedure Remove(index: OleVariant); dispid 4;
    procedure Clear; dispid 5;
  end;

// *********************************************************************//
// Interface: ITrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {09941815-EA89-4B5B-89E0-2A773801FAC3}
// *********************************************************************//
  ITrigger = interface(IDispatch)
    ['{09941815-EA89-4B5B-89E0-2A773801FAC3}']
    function Get_type_: _TASK_TRIGGER_TYPE2; safecall;
    function Get_Id: WideString; safecall;
    procedure Set_Id(const pId: WideString); safecall;
    function Get_Repetition: IRepetitionPattern; safecall;
    procedure Set_Repetition(const ppRepeat: IRepetitionPattern); safecall;
    function Get_ExecutionTimeLimit: WideString; safecall;
    procedure Set_ExecutionTimeLimit(const pTimeLimit: WideString); safecall;
    function Get_StartBoundary: WideString; safecall;
    procedure Set_StartBoundary(const pStart: WideString); safecall;
    function Get_EndBoundary: WideString; safecall;
    procedure Set_EndBoundary(const pEnd: WideString); safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(pEnabled: WordBool); safecall;
    property type_: _TASK_TRIGGER_TYPE2 read Get_type_;
    property Id: WideString read Get_Id write Set_Id;
    property Repetition: IRepetitionPattern read Get_Repetition write Set_Repetition;
    property ExecutionTimeLimit: WideString read Get_ExecutionTimeLimit write Set_ExecutionTimeLimit;
    property StartBoundary: WideString read Get_StartBoundary write Set_StartBoundary;
    property EndBoundary: WideString read Get_EndBoundary write Set_EndBoundary;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
  end;

// *********************************************************************//
// DispIntf:  ITriggerDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {09941815-EA89-4B5B-89E0-2A773801FAC3}
// *********************************************************************//
  ITriggerDisp = dispinterface
    ['{09941815-EA89-4B5B-89E0-2A773801FAC3}']
    property type_: _TASK_TRIGGER_TYPE2 readonly dispid 1;
    property Id: WideString dispid 2;
    property Repetition: IRepetitionPattern dispid 3;
    property ExecutionTimeLimit: WideString dispid 4;
    property StartBoundary: WideString dispid 5;
    property EndBoundary: WideString dispid 6;
    property Enabled: WordBool dispid 7;
  end;

// *********************************************************************//
// Interface: IRepetitionPattern
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {7FB9ACF1-26BE-400E-85B5-294B9C75DFD6}
// *********************************************************************//
  IRepetitionPattern = interface(IDispatch)
    ['{7FB9ACF1-26BE-400E-85B5-294B9C75DFD6}']
    function Get_Interval: WideString; safecall;
    procedure Set_Interval(const pInterval: WideString); safecall;
    function Get_Duration: WideString; safecall;
    procedure Set_Duration(const pDuration: WideString); safecall;
    function Get_StopAtDurationEnd: WordBool; safecall;
    procedure Set_StopAtDurationEnd(pStop: WordBool); safecall;
    property Interval: WideString read Get_Interval write Set_Interval;
    property Duration: WideString read Get_Duration write Set_Duration;
    property StopAtDurationEnd: WordBool read Get_StopAtDurationEnd write Set_StopAtDurationEnd;
  end;

// *********************************************************************//
// DispIntf:  IRepetitionPatternDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {7FB9ACF1-26BE-400E-85B5-294B9C75DFD6}
// *********************************************************************//
  IRepetitionPatternDisp = dispinterface
    ['{7FB9ACF1-26BE-400E-85B5-294B9C75DFD6}']
    property Interval: WideString dispid 1;
    property Duration: WideString dispid 2;
    property StopAtDurationEnd: WordBool dispid 3;
  end;

// *********************************************************************//
// Interface: ITaskSettings
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8FD4711D-2D02-4C8C-87E3-EFF699DE127E}
// *********************************************************************//
  ITaskSettings = interface(IDispatch)
    ['{8FD4711D-2D02-4C8C-87E3-EFF699DE127E}']
    function Get_AllowDemandStart: WordBool; safecall;
    procedure Set_AllowDemandStart(pAllowDemandStart: WordBool); safecall;
    function Get_RestartInterval: WideString; safecall;
    procedure Set_RestartInterval(const pRestartInterval: WideString); safecall;
    function Get_RestartCount: SYSINT; safecall;
    procedure Set_RestartCount(pRestartCount: SYSINT); safecall;
    function Get_MultipleInstances: _TASK_INSTANCES_POLICY; safecall;
    procedure Set_MultipleInstances(pPolicy: _TASK_INSTANCES_POLICY); safecall;
    function Get_StopIfGoingOnBatteries: WordBool; safecall;
    procedure Set_StopIfGoingOnBatteries(pStopIfOnBatteries: WordBool); safecall;
    function Get_DisallowStartIfOnBatteries: WordBool; safecall;
    procedure Set_DisallowStartIfOnBatteries(pDisallowStart: WordBool); safecall;
    function Get_AllowHardTerminate: WordBool; safecall;
    procedure Set_AllowHardTerminate(pAllowHardTerminate: WordBool); safecall;
    function Get_StartWhenAvailable: WordBool; safecall;
    procedure Set_StartWhenAvailable(pStartWhenAvailable: WordBool); safecall;
    function Get_XmlText: WideString; safecall;
    procedure Set_XmlText(const pText: WideString); safecall;
    function Get_RunOnlyIfNetworkAvailable: WordBool; safecall;
    procedure Set_RunOnlyIfNetworkAvailable(pRunOnlyIfNetworkAvailable: WordBool); safecall;
    function Get_ExecutionTimeLimit: WideString; safecall;
    procedure Set_ExecutionTimeLimit(const pExecutionTimeLimit: WideString); safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(pEnabled: WordBool); safecall;
    function Get_DeleteExpiredTaskAfter: WideString; safecall;
    procedure Set_DeleteExpiredTaskAfter(const pExpirationDelay: WideString); safecall;
    function Get_Priority: SYSINT; safecall;
    procedure Set_Priority(pPriority: SYSINT); safecall;
    function Get_Compatibility: _TASK_COMPATIBILITY; safecall;
    procedure Set_Compatibility(pCompatLevel: _TASK_COMPATIBILITY); safecall;
    function Get_Hidden: WordBool; safecall;
    procedure Set_Hidden(pHidden: WordBool); safecall;
    function Get_IdleSettings: IIdleSettings; safecall;
    procedure Set_IdleSettings(const ppIdleSettings: IIdleSettings); safecall;
    function Get_RunOnlyIfIdle: WordBool; safecall;
    procedure Set_RunOnlyIfIdle(pRunOnlyIfIdle: WordBool); safecall;
    function Get_WakeToRun: WordBool; safecall;
    procedure Set_WakeToRun(pWake: WordBool); safecall;
    function Get_NetworkSettings: INetworkSettings; safecall;
    procedure Set_NetworkSettings(const ppNetworkSettings: INetworkSettings); safecall;
    property AllowDemandStart: WordBool read Get_AllowDemandStart write Set_AllowDemandStart;
    property RestartInterval: WideString read Get_RestartInterval write Set_RestartInterval;
    property RestartCount: SYSINT read Get_RestartCount write Set_RestartCount;
    property MultipleInstances: _TASK_INSTANCES_POLICY read Get_MultipleInstances write Set_MultipleInstances;
    property StopIfGoingOnBatteries: WordBool read Get_StopIfGoingOnBatteries write Set_StopIfGoingOnBatteries;
    property DisallowStartIfOnBatteries: WordBool read Get_DisallowStartIfOnBatteries write Set_DisallowStartIfOnBatteries;
    property AllowHardTerminate: WordBool read Get_AllowHardTerminate write Set_AllowHardTerminate;
    property StartWhenAvailable: WordBool read Get_StartWhenAvailable write Set_StartWhenAvailable;
    property XmlText: WideString read Get_XmlText write Set_XmlText;
    property RunOnlyIfNetworkAvailable: WordBool read Get_RunOnlyIfNetworkAvailable write Set_RunOnlyIfNetworkAvailable;
    property ExecutionTimeLimit: WideString read Get_ExecutionTimeLimit write Set_ExecutionTimeLimit;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property DeleteExpiredTaskAfter: WideString read Get_DeleteExpiredTaskAfter write Set_DeleteExpiredTaskAfter;
    property Priority: SYSINT read Get_Priority write Set_Priority;
    property Compatibility: _TASK_COMPATIBILITY read Get_Compatibility write Set_Compatibility;
    property Hidden: WordBool read Get_Hidden write Set_Hidden;
    property IdleSettings: IIdleSettings read Get_IdleSettings write Set_IdleSettings;
    property RunOnlyIfIdle: WordBool read Get_RunOnlyIfIdle write Set_RunOnlyIfIdle;
    property WakeToRun: WordBool read Get_WakeToRun write Set_WakeToRun;
    property NetworkSettings: INetworkSettings read Get_NetworkSettings write Set_NetworkSettings;
  end;

// *********************************************************************//
// DispIntf:  ITaskSettingsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8FD4711D-2D02-4C8C-87E3-EFF699DE127E}
// *********************************************************************//
  ITaskSettingsDisp = dispinterface
    ['{8FD4711D-2D02-4C8C-87E3-EFF699DE127E}']
    property AllowDemandStart: WordBool dispid 3;
    property RestartInterval: WideString dispid 4;
    property RestartCount: SYSINT dispid 5;
    property MultipleInstances: _TASK_INSTANCES_POLICY dispid 6;
    property StopIfGoingOnBatteries: WordBool dispid 7;
    property DisallowStartIfOnBatteries: WordBool dispid 8;
    property AllowHardTerminate: WordBool dispid 9;
    property StartWhenAvailable: WordBool dispid 10;
    property XmlText: WideString dispid 11;
    property RunOnlyIfNetworkAvailable: WordBool dispid 12;
    property ExecutionTimeLimit: WideString dispid 13;
    property Enabled: WordBool dispid 14;
    property DeleteExpiredTaskAfter: WideString dispid 15;
    property Priority: SYSINT dispid 16;
    property Compatibility: _TASK_COMPATIBILITY dispid 17;
    property Hidden: WordBool dispid 18;
    property IdleSettings: IIdleSettings dispid 19;
    property RunOnlyIfIdle: WordBool dispid 20;
    property WakeToRun: WordBool dispid 21;
    property NetworkSettings: INetworkSettings dispid 22;
  end;

// *********************************************************************//
// Interface: IIdleSettings
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {84594461-0053-4342-A8FD-088FABF11F32}
// *********************************************************************//
  IIdleSettings = interface(IDispatch)
    ['{84594461-0053-4342-A8FD-088FABF11F32}']
    function Get_IdleDuration: WideString; safecall;
    procedure Set_IdleDuration(const pDelay: WideString); safecall;
    function Get_WaitTimeout: WideString; safecall;
    procedure Set_WaitTimeout(const pTimeout: WideString); safecall;
    function Get_StopOnIdleEnd: WordBool; safecall;
    procedure Set_StopOnIdleEnd(pStop: WordBool); safecall;
    function Get_RestartOnIdle: WordBool; safecall;
    procedure Set_RestartOnIdle(pRestart: WordBool); safecall;
    property IdleDuration: WideString read Get_IdleDuration write Set_IdleDuration;
    property WaitTimeout: WideString read Get_WaitTimeout write Set_WaitTimeout;
    property StopOnIdleEnd: WordBool read Get_StopOnIdleEnd write Set_StopOnIdleEnd;
    property RestartOnIdle: WordBool read Get_RestartOnIdle write Set_RestartOnIdle;
  end;

// *********************************************************************//
// DispIntf:  IIdleSettingsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {84594461-0053-4342-A8FD-088FABF11F32}
// *********************************************************************//
  IIdleSettingsDisp = dispinterface
    ['{84594461-0053-4342-A8FD-088FABF11F32}']
    property IdleDuration: WideString dispid 1;
    property WaitTimeout: WideString dispid 2;
    property StopOnIdleEnd: WordBool dispid 3;
    property RestartOnIdle: WordBool dispid 4;
  end;

// *********************************************************************//
// Interface: INetworkSettings
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {9F7DEA84-C30B-4245-80B6-00E9F646F1B4}
// *********************************************************************//
  INetworkSettings = interface(IDispatch)
    ['{9F7DEA84-C30B-4245-80B6-00E9F646F1B4}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const pName: WideString); safecall;
    function Get_Id: WideString; safecall;
    procedure Set_Id(const pId: WideString); safecall;
    property Name: WideString read Get_Name write Set_Name;
    property Id: WideString read Get_Id write Set_Id;
  end;

// *********************************************************************//
// DispIntf:  INetworkSettingsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {9F7DEA84-C30B-4245-80B6-00E9F646F1B4}
// *********************************************************************//
  INetworkSettingsDisp = dispinterface
    ['{9F7DEA84-C30B-4245-80B6-00E9F646F1B4}']
    property Name: WideString dispid 1;
    property Id: WideString dispid 2;
  end;

// *********************************************************************//
// Interface: IPrincipal
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D98D51E5-C9B4-496A-A9C1-18980261CF0F}
// *********************************************************************//
  IPrincipal = interface(IDispatch)
    ['{D98D51E5-C9B4-496A-A9C1-18980261CF0F}']
    function Get_Id: WideString; safecall;
    procedure Set_Id(const pId: WideString); safecall;
    function Get_DisplayName: WideString; safecall;
    procedure Set_DisplayName(const pName: WideString); safecall;
    function Get_UserId: WideString; safecall;
    procedure Set_UserId(const pUser: WideString); safecall;
    function Get_LogonType: _TASK_LOGON_TYPE; safecall;
    procedure Set_LogonType(pLogon: _TASK_LOGON_TYPE); safecall;
    function Get_GroupId: WideString; safecall;
    procedure Set_GroupId(const pGroup: WideString); safecall;
    function Get_RunLevel: _TASK_RUNLEVEL; safecall;
    procedure Set_RunLevel(pRunLevel: _TASK_RUNLEVEL); safecall;
    property Id: WideString read Get_Id write Set_Id;
    property DisplayName: WideString read Get_DisplayName write Set_DisplayName;
    property UserId: WideString read Get_UserId write Set_UserId;
    property LogonType: _TASK_LOGON_TYPE read Get_LogonType write Set_LogonType;
    property GroupId: WideString read Get_GroupId write Set_GroupId;
    property RunLevel: _TASK_RUNLEVEL read Get_RunLevel write Set_RunLevel;
  end;

// *********************************************************************//
// DispIntf:  IPrincipalDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D98D51E5-C9B4-496A-A9C1-18980261CF0F}
// *********************************************************************//
  IPrincipalDisp = dispinterface
    ['{D98D51E5-C9B4-496A-A9C1-18980261CF0F}']
    property Id: WideString dispid 1;
    property DisplayName: WideString dispid 2;
    property UserId: WideString dispid 3;
    property LogonType: _TASK_LOGON_TYPE dispid 4;
    property GroupId: WideString dispid 5;
    property RunLevel: _TASK_RUNLEVEL dispid 6;
  end;

// *********************************************************************//
// Interface: IActionCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {02820E19-7B98-4ED2-B2E8-FDCCCEFF619B}
// *********************************************************************//
  IActionCollection = interface(IDispatch)
    ['{02820E19-7B98-4ED2-B2E8-FDCCCEFF619B}']
    function Get_Count: Integer; safecall;
    function Get_Item(index: Integer): IAction; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_XmlText: WideString; safecall;
    procedure Set_XmlText(const pText: WideString); safecall;
    function Create(Type_: _TASK_ACTION_TYPE): IAction; safecall;
    procedure Remove(index: OleVariant); safecall;
    procedure Clear; safecall;
    function Get_Context: WideString; safecall;
    procedure Set_Context(const pContext: WideString); safecall;
    property Count: Integer read Get_Count;
    property Item[index: Integer]: IAction read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
    property XmlText: WideString read Get_XmlText write Set_XmlText;
    property Context: WideString read Get_Context write Set_Context;
  end;

// *********************************************************************//
// DispIntf:  IActionCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {02820E19-7B98-4ED2-B2E8-FDCCCEFF619B}
// *********************************************************************//
  IActionCollectionDisp = dispinterface
    ['{02820E19-7B98-4ED2-B2E8-FDCCCEFF619B}']
    property Count: Integer readonly dispid 1;
    property Item[index: Integer]: IAction readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
    property XmlText: WideString dispid 2;
    function Create(Type_: _TASK_ACTION_TYPE): IAction; dispid 3;
    procedure Remove(index: OleVariant); dispid 4;
    procedure Clear; dispid 5;
    property Context: WideString dispid 6;
  end;

// *********************************************************************//
// Interface: IAction
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {BAE54997-48B1-4CBE-9965-D6BE263EBEA4}
// *********************************************************************//
  IAction = interface(IDispatch)
    ['{BAE54997-48B1-4CBE-9965-D6BE263EBEA4}']
    function Get_Id: WideString; safecall;
    procedure Set_Id(const pId: WideString); safecall;
    function Get_type_: _TASK_ACTION_TYPE; safecall;
    property Id: WideString read Get_Id write Set_Id;
    property type_: _TASK_ACTION_TYPE read Get_type_;
  end;

// *********************************************************************//
// DispIntf:  IActionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {BAE54997-48B1-4CBE-9965-D6BE263EBEA4}
// *********************************************************************//
  IActionDisp = dispinterface
    ['{BAE54997-48B1-4CBE-9965-D6BE263EBEA4}']
    property Id: WideString dispid 1;
    property type_: _TASK_ACTION_TYPE readonly dispid 2;
  end;

// *********************************************************************//
// Interface: IRegisteredTaskCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {86627EB4-42A7-41E4-A4D9-AC33A72F2D52}
// *********************************************************************//
  IRegisteredTaskCollection = interface(IDispatch)
    ['{86627EB4-42A7-41E4-A4D9-AC33A72F2D52}']
    function Get_Count: Integer; safecall;
    function Get_Item(index: OleVariant): IRegisteredTask; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[index: OleVariant]: IRegisteredTask read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IRegisteredTaskCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {86627EB4-42A7-41E4-A4D9-AC33A72F2D52}
// *********************************************************************//
  IRegisteredTaskCollectionDisp = dispinterface
    ['{86627EB4-42A7-41E4-A4D9-AC33A72F2D52}']
    property Count: Integer readonly dispid 1610743808;
    property Item[index: OleVariant]: IRegisteredTask readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ITaskService
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {2FABA4C7-4DA9-4013-9697-20CC3FD40F85}
// *********************************************************************//
  ITaskService = interface(IDispatch)
    ['{2FABA4C7-4DA9-4013-9697-20CC3FD40F85}']
    function GetFolder(const Path: WideString): ITaskFolder; safecall;
    function GetRunningTasks(flags: Integer): IRunningTaskCollection; safecall;
    function NewTask(flags: LongWord): ITaskDefinition; safecall;
    procedure Connect(serverName: OleVariant; user: OleVariant; domain: OleVariant; 
                      password: OleVariant); safecall;
    function Get_Connected: WordBool; safecall;
    function Get_TargetServer: WideString; safecall;
    function Get_ConnectedUser: WideString; safecall;
    function Get_ConnectedDomain: WideString; safecall;
    function Get_HighestVersion: LongWord; safecall;
    property Connected: WordBool read Get_Connected;
    property TargetServer: WideString read Get_TargetServer;
    property ConnectedUser: WideString read Get_ConnectedUser;
    property ConnectedDomain: WideString read Get_ConnectedDomain;
    property HighestVersion: LongWord read Get_HighestVersion;
  end;

// *********************************************************************//
// DispIntf:  ITaskServiceDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {2FABA4C7-4DA9-4013-9697-20CC3FD40F85}
// *********************************************************************//
  ITaskServiceDisp = dispinterface
    ['{2FABA4C7-4DA9-4013-9697-20CC3FD40F85}']
    function GetFolder(const Path: WideString): ITaskFolder; dispid 1;
    function GetRunningTasks(flags: Integer): IRunningTaskCollection; dispid 2;
    function NewTask(flags: LongWord): ITaskDefinition; dispid 3;
    procedure Connect(serverName: OleVariant; user: OleVariant; domain: OleVariant; 
                      password: OleVariant); dispid 4;
    property Connected: WordBool readonly dispid 5;
    property TargetServer: WideString readonly dispid 0;
    property ConnectedUser: WideString readonly dispid 6;
    property ConnectedDomain: WideString readonly dispid 7;
    property HighestVersion: LongWord readonly dispid 8;
  end;

// *********************************************************************//
// Interface: ITaskHandler
// Flags:     (0)
// GUID:      {839D7762-5121-4009-9234-4F0D19394F04}
// *********************************************************************//
  ITaskHandler = interface(IUnknown)
    ['{839D7762-5121-4009-9234-4F0D19394F04}']
    function Start(const pHandlerServices: IUnknown; const Data: WideString): HResult; stdcall;
    function Stop(out pRetCode: HResult): HResult; stdcall;
    function Pause: HResult; stdcall;
    function Resume: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITaskHandlerStatus
// Flags:     (0)
// GUID:      {EAEC7A8F-27A0-4DDC-8675-14726A01A38A}
// *********************************************************************//
  ITaskHandlerStatus = interface(IUnknown)
    ['{EAEC7A8F-27A0-4DDC-8675-14726A01A38A}']
    function UpdateStatus(percentComplete: Smallint; const statusMessage: WideString): HResult; stdcall;
    function TaskCompleted(taskErrCode: HResult): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITaskVariables
// Flags:     (0)
// GUID:      {3E4C9351-D966-4B8B-BB87-CEBA68BB0107}
// *********************************************************************//
  ITaskVariables = interface(IUnknown)
    ['{3E4C9351-D966-4B8B-BB87-CEBA68BB0107}']
    function GetInput(out pInput: WideString): HResult; stdcall;
    function SetOutput(const input: WideString): HResult; stdcall;
    function GetContext(out pContext: WideString): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITaskNamedValuePair
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {39038068-2B46-4AFD-8662-7BB6F868D221}
// *********************************************************************//
  ITaskNamedValuePair = interface(IDispatch)
    ['{39038068-2B46-4AFD-8662-7BB6F868D221}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const pName: WideString); safecall;
    function Get_Value: WideString; safecall;
    procedure Set_Value(const pValue: WideString); safecall;
    property Name: WideString read Get_Name write Set_Name;
    property Value: WideString read Get_Value write Set_Value;
  end;

// *********************************************************************//
// DispIntf:  ITaskNamedValuePairDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {39038068-2B46-4AFD-8662-7BB6F868D221}
// *********************************************************************//
  ITaskNamedValuePairDisp = dispinterface
    ['{39038068-2B46-4AFD-8662-7BB6F868D221}']
    property Name: WideString dispid 0;
    property Value: WideString dispid 1;
  end;

// *********************************************************************//
// Interface: ITaskNamedValueCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B4EF826B-63C3-46E4-A504-EF69E4F7EA4D}
// *********************************************************************//
  ITaskNamedValueCollection = interface(IDispatch)
    ['{B4EF826B-63C3-46E4-A504-EF69E4F7EA4D}']
    function Get_Count: Integer; safecall;
    function Get_Item(index: Integer): ITaskNamedValuePair; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Create(const Name: WideString; const Value: WideString): ITaskNamedValuePair; safecall;
    procedure Remove(index: Integer); safecall;
    procedure Clear; safecall;
    property Count: Integer read Get_Count;
    property Item[index: Integer]: ITaskNamedValuePair read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ITaskNamedValueCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B4EF826B-63C3-46E4-A504-EF69E4F7EA4D}
// *********************************************************************//
  ITaskNamedValueCollectionDisp = dispinterface
    ['{B4EF826B-63C3-46E4-A504-EF69E4F7EA4D}']
    property Count: Integer readonly dispid 1;
    property Item[index: Integer]: ITaskNamedValuePair readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
    function Create(const Name: WideString; const Value: WideString): ITaskNamedValuePair; dispid 2;
    procedure Remove(index: Integer); dispid 4;
    procedure Clear; dispid 5;
  end;

// *********************************************************************//
// Interface: IIdleTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D537D2B0-9FB3-4D34-9739-1FF5CE7B1EF3}
// *********************************************************************//
  IIdleTrigger = interface(ITrigger)
    ['{D537D2B0-9FB3-4D34-9739-1FF5CE7B1EF3}']
  end;

// *********************************************************************//
// DispIntf:  IIdleTriggerDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D537D2B0-9FB3-4D34-9739-1FF5CE7B1EF3}
// *********************************************************************//
  IIdleTriggerDisp = dispinterface
    ['{D537D2B0-9FB3-4D34-9739-1FF5CE7B1EF3}']
    property type_: _TASK_TRIGGER_TYPE2 readonly dispid 1;
    property Id: WideString dispid 2;
    property Repetition: IRepetitionPattern dispid 3;
    property ExecutionTimeLimit: WideString dispid 4;
    property StartBoundary: WideString dispid 5;
    property EndBoundary: WideString dispid 6;
    property Enabled: WordBool dispid 7;
  end;

// *********************************************************************//
// Interface: ILogonTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {72DADE38-FAE4-4B3E-BAF4-5D009AF02B1C}
// *********************************************************************//
  ILogonTrigger = interface(ITrigger)
    ['{72DADE38-FAE4-4B3E-BAF4-5D009AF02B1C}']
    function Get_Delay: WideString; safecall;
    procedure Set_Delay(const pDelay: WideString); safecall;
    function Get_UserId: WideString; safecall;
    procedure Set_UserId(const pUser: WideString); safecall;
    property Delay: WideString read Get_Delay write Set_Delay;
    property UserId: WideString read Get_UserId write Set_UserId;
  end;

// *********************************************************************//
// DispIntf:  ILogonTriggerDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {72DADE38-FAE4-4B3E-BAF4-5D009AF02B1C}
// *********************************************************************//
  ILogonTriggerDisp = dispinterface
    ['{72DADE38-FAE4-4B3E-BAF4-5D009AF02B1C}']
    property Delay: WideString dispid 20;
    property UserId: WideString dispid 21;
    property type_: _TASK_TRIGGER_TYPE2 readonly dispid 1;
    property Id: WideString dispid 2;
    property Repetition: IRepetitionPattern dispid 3;
    property ExecutionTimeLimit: WideString dispid 4;
    property StartBoundary: WideString dispid 5;
    property EndBoundary: WideString dispid 6;
    property Enabled: WordBool dispid 7;
  end;

// *********************************************************************//
// Interface: ISessionStateChangeTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {754DA71B-4385-4475-9DD9-598294FA3641}
// *********************************************************************//
  ISessionStateChangeTrigger = interface(ITrigger)
    ['{754DA71B-4385-4475-9DD9-598294FA3641}']
    function Get_Delay: WideString; safecall;
    procedure Set_Delay(const pDelay: WideString); safecall;
    function Get_UserId: WideString; safecall;
    procedure Set_UserId(const pUser: WideString); safecall;
    function Get_StateChange: _TASK_SESSION_STATE_CHANGE_TYPE; safecall;
    procedure Set_StateChange(pType: _TASK_SESSION_STATE_CHANGE_TYPE); safecall;
    property Delay: WideString read Get_Delay write Set_Delay;
    property UserId: WideString read Get_UserId write Set_UserId;
    property StateChange: _TASK_SESSION_STATE_CHANGE_TYPE read Get_StateChange write Set_StateChange;
  end;

// *********************************************************************//
// DispIntf:  ISessionStateChangeTriggerDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {754DA71B-4385-4475-9DD9-598294FA3641}
// *********************************************************************//
  ISessionStateChangeTriggerDisp = dispinterface
    ['{754DA71B-4385-4475-9DD9-598294FA3641}']
    property Delay: WideString dispid 20;
    property UserId: WideString dispid 21;
    property StateChange: _TASK_SESSION_STATE_CHANGE_TYPE dispid 22;
    property type_: _TASK_TRIGGER_TYPE2 readonly dispid 1;
    property Id: WideString dispid 2;
    property Repetition: IRepetitionPattern dispid 3;
    property ExecutionTimeLimit: WideString dispid 4;
    property StartBoundary: WideString dispid 5;
    property EndBoundary: WideString dispid 6;
    property Enabled: WordBool dispid 7;
  end;

// *********************************************************************//
// Interface: IEventTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D45B0167-9653-4EEF-B94F-0732CA7AF251}
// *********************************************************************//
  IEventTrigger = interface(ITrigger)
    ['{D45B0167-9653-4EEF-B94F-0732CA7AF251}']
    function Get_Subscription: WideString; safecall;
    procedure Set_Subscription(const pQuery: WideString); safecall;
    function Get_Delay: WideString; safecall;
    procedure Set_Delay(const pDelay: WideString); safecall;
    function Get_ValueQueries: ITaskNamedValueCollection; safecall;
    procedure Set_ValueQueries(const ppNamedXPaths: ITaskNamedValueCollection); safecall;
    property Subscription: WideString read Get_Subscription write Set_Subscription;
    property Delay: WideString read Get_Delay write Set_Delay;
    property ValueQueries: ITaskNamedValueCollection read Get_ValueQueries write Set_ValueQueries;
  end;

// *********************************************************************//
// DispIntf:  IEventTriggerDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D45B0167-9653-4EEF-B94F-0732CA7AF251}
// *********************************************************************//
  IEventTriggerDisp = dispinterface
    ['{D45B0167-9653-4EEF-B94F-0732CA7AF251}']
    property Subscription: WideString dispid 20;
    property Delay: WideString dispid 21;
    property ValueQueries: ITaskNamedValueCollection dispid 22;
    property type_: _TASK_TRIGGER_TYPE2 readonly dispid 1;
    property Id: WideString dispid 2;
    property Repetition: IRepetitionPattern dispid 3;
    property ExecutionTimeLimit: WideString dispid 4;
    property StartBoundary: WideString dispid 5;
    property EndBoundary: WideString dispid 6;
    property Enabled: WordBool dispid 7;
  end;

// *********************************************************************//
// Interface: ITimeTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B45747E0-EBA7-4276-9F29-85C5BB300006}
// *********************************************************************//
  ITimeTrigger = interface(ITrigger)
    ['{B45747E0-EBA7-4276-9F29-85C5BB300006}']
    function Get_RandomDelay: WideString; safecall;
    procedure Set_RandomDelay(const pRandomDelay: WideString); safecall;
    property RandomDelay: WideString read Get_RandomDelay write Set_RandomDelay;
  end;

// *********************************************************************//
// DispIntf:  ITimeTriggerDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B45747E0-EBA7-4276-9F29-85C5BB300006}
// *********************************************************************//
  ITimeTriggerDisp = dispinterface
    ['{B45747E0-EBA7-4276-9F29-85C5BB300006}']
    property RandomDelay: WideString dispid 20;
    property type_: _TASK_TRIGGER_TYPE2 readonly dispid 1;
    property Id: WideString dispid 2;
    property Repetition: IRepetitionPattern dispid 3;
    property ExecutionTimeLimit: WideString dispid 4;
    property StartBoundary: WideString dispid 5;
    property EndBoundary: WideString dispid 6;
    property Enabled: WordBool dispid 7;
  end;

// *********************************************************************//
// Interface: IDailyTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {126C5CD8-B288-41D5-8DBF-E491446ADC5C}
// *********************************************************************//
  IDailyTrigger = interface(ITrigger)
    ['{126C5CD8-B288-41D5-8DBF-E491446ADC5C}']
    function Get_DaysInterval: Smallint; safecall;
    procedure Set_DaysInterval(pDays: Smallint); safecall;
    function Get_RandomDelay: WideString; safecall;
    procedure Set_RandomDelay(const pRandomDelay: WideString); safecall;
    property DaysInterval: Smallint read Get_DaysInterval write Set_DaysInterval;
    property RandomDelay: WideString read Get_RandomDelay write Set_RandomDelay;
  end;

// *********************************************************************//
// DispIntf:  IDailyTriggerDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {126C5CD8-B288-41D5-8DBF-E491446ADC5C}
// *********************************************************************//
  IDailyTriggerDisp = dispinterface
    ['{126C5CD8-B288-41D5-8DBF-E491446ADC5C}']
    property DaysInterval: Smallint dispid 25;
    property RandomDelay: WideString dispid 20;
    property type_: _TASK_TRIGGER_TYPE2 readonly dispid 1;
    property Id: WideString dispid 2;
    property Repetition: IRepetitionPattern dispid 3;
    property ExecutionTimeLimit: WideString dispid 4;
    property StartBoundary: WideString dispid 5;
    property EndBoundary: WideString dispid 6;
    property Enabled: WordBool dispid 7;
  end;

// *********************************************************************//
// Interface: IWeeklyTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {5038FC98-82FF-436D-8728-A512A57C9DC1}
// *********************************************************************//
  IWeeklyTrigger = interface(ITrigger)
    ['{5038FC98-82FF-436D-8728-A512A57C9DC1}']
    function Get_DaysOfWeek: Smallint; safecall;
    procedure Set_DaysOfWeek(pDays: Smallint); safecall;
    function Get_WeeksInterval: Smallint; safecall;
    procedure Set_WeeksInterval(pWeeks: Smallint); safecall;
    function Get_RandomDelay: WideString; safecall;
    procedure Set_RandomDelay(const pRandomDelay: WideString); safecall;
    property DaysOfWeek: Smallint read Get_DaysOfWeek write Set_DaysOfWeek;
    property WeeksInterval: Smallint read Get_WeeksInterval write Set_WeeksInterval;
    property RandomDelay: WideString read Get_RandomDelay write Set_RandomDelay;
  end;

// *********************************************************************//
// DispIntf:  IWeeklyTriggerDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {5038FC98-82FF-436D-8728-A512A57C9DC1}
// *********************************************************************//
  IWeeklyTriggerDisp = dispinterface
    ['{5038FC98-82FF-436D-8728-A512A57C9DC1}']
    property DaysOfWeek: Smallint dispid 25;
    property WeeksInterval: Smallint dispid 26;
    property RandomDelay: WideString dispid 20;
    property type_: _TASK_TRIGGER_TYPE2 readonly dispid 1;
    property Id: WideString dispid 2;
    property Repetition: IRepetitionPattern dispid 3;
    property ExecutionTimeLimit: WideString dispid 4;
    property StartBoundary: WideString dispid 5;
    property EndBoundary: WideString dispid 6;
    property Enabled: WordBool dispid 7;
  end;

// *********************************************************************//
// Interface: IMonthlyTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {97C45EF1-6B02-4A1A-9C0E-1EBFBA1500AC}
// *********************************************************************//
  IMonthlyTrigger = interface(ITrigger)
    ['{97C45EF1-6B02-4A1A-9C0E-1EBFBA1500AC}']
    function Get_DaysOfMonth: Integer; safecall;
    procedure Set_DaysOfMonth(pDays: Integer); safecall;
    function Get_MonthsOfYear: Smallint; safecall;
    procedure Set_MonthsOfYear(pMonths: Smallint); safecall;
    function Get_RunOnLastDayOfMonth: WordBool; safecall;
    procedure Set_RunOnLastDayOfMonth(pLastDay: WordBool); safecall;
    function Get_RandomDelay: WideString; safecall;
    procedure Set_RandomDelay(const pRandomDelay: WideString); safecall;
    property DaysOfMonth: Integer read Get_DaysOfMonth write Set_DaysOfMonth;
    property MonthsOfYear: Smallint read Get_MonthsOfYear write Set_MonthsOfYear;
    property RunOnLastDayOfMonth: WordBool read Get_RunOnLastDayOfMonth write Set_RunOnLastDayOfMonth;
    property RandomDelay: WideString read Get_RandomDelay write Set_RandomDelay;
  end;

// *********************************************************************//
// DispIntf:  IMonthlyTriggerDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {97C45EF1-6B02-4A1A-9C0E-1EBFBA1500AC}
// *********************************************************************//
  IMonthlyTriggerDisp = dispinterface
    ['{97C45EF1-6B02-4A1A-9C0E-1EBFBA1500AC}']
    property DaysOfMonth: Integer dispid 25;
    property MonthsOfYear: Smallint dispid 26;
    property RunOnLastDayOfMonth: WordBool dispid 27;
    property RandomDelay: WideString dispid 20;
    property type_: _TASK_TRIGGER_TYPE2 readonly dispid 1;
    property Id: WideString dispid 2;
    property Repetition: IRepetitionPattern dispid 3;
    property ExecutionTimeLimit: WideString dispid 4;
    property StartBoundary: WideString dispid 5;
    property EndBoundary: WideString dispid 6;
    property Enabled: WordBool dispid 7;
  end;

// *********************************************************************//
// Interface: IMonthlyDOWTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {77D025A3-90FA-43AA-B52E-CDA5499B946A}
// *********************************************************************//
  IMonthlyDOWTrigger = interface(ITrigger)
    ['{77D025A3-90FA-43AA-B52E-CDA5499B946A}']
    function Get_DaysOfWeek: Smallint; safecall;
    procedure Set_DaysOfWeek(pDays: Smallint); safecall;
    function Get_WeeksOfMonth: Smallint; safecall;
    procedure Set_WeeksOfMonth(pWeeks: Smallint); safecall;
    function Get_MonthsOfYear: Smallint; safecall;
    procedure Set_MonthsOfYear(pMonths: Smallint); safecall;
    function Get_RunOnLastWeekOfMonth: WordBool; safecall;
    procedure Set_RunOnLastWeekOfMonth(pLastWeek: WordBool); safecall;
    function Get_RandomDelay: WideString; safecall;
    procedure Set_RandomDelay(const pRandomDelay: WideString); safecall;
    property DaysOfWeek: Smallint read Get_DaysOfWeek write Set_DaysOfWeek;
    property WeeksOfMonth: Smallint read Get_WeeksOfMonth write Set_WeeksOfMonth;
    property MonthsOfYear: Smallint read Get_MonthsOfYear write Set_MonthsOfYear;
    property RunOnLastWeekOfMonth: WordBool read Get_RunOnLastWeekOfMonth write Set_RunOnLastWeekOfMonth;
    property RandomDelay: WideString read Get_RandomDelay write Set_RandomDelay;
  end;

// *********************************************************************//
// DispIntf:  IMonthlyDOWTriggerDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {77D025A3-90FA-43AA-B52E-CDA5499B946A}
// *********************************************************************//
  IMonthlyDOWTriggerDisp = dispinterface
    ['{77D025A3-90FA-43AA-B52E-CDA5499B946A}']
    property DaysOfWeek: Smallint dispid 25;
    property WeeksOfMonth: Smallint dispid 26;
    property MonthsOfYear: Smallint dispid 27;
    property RunOnLastWeekOfMonth: WordBool dispid 28;
    property RandomDelay: WideString dispid 20;
    property type_: _TASK_TRIGGER_TYPE2 readonly dispid 1;
    property Id: WideString dispid 2;
    property Repetition: IRepetitionPattern dispid 3;
    property ExecutionTimeLimit: WideString dispid 4;
    property StartBoundary: WideString dispid 5;
    property EndBoundary: WideString dispid 6;
    property Enabled: WordBool dispid 7;
  end;

// *********************************************************************//
// Interface: IBootTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {2A9C35DA-D357-41F4-BBC1-207AC1B1F3CB}
// *********************************************************************//
  IBootTrigger = interface(ITrigger)
    ['{2A9C35DA-D357-41F4-BBC1-207AC1B1F3CB}']
    function Get_Delay: WideString; safecall;
    procedure Set_Delay(const pDelay: WideString); safecall;
    property Delay: WideString read Get_Delay write Set_Delay;
  end;

// *********************************************************************//
// DispIntf:  IBootTriggerDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {2A9C35DA-D357-41F4-BBC1-207AC1B1F3CB}
// *********************************************************************//
  IBootTriggerDisp = dispinterface
    ['{2A9C35DA-D357-41F4-BBC1-207AC1B1F3CB}']
    property Delay: WideString dispid 20;
    property type_: _TASK_TRIGGER_TYPE2 readonly dispid 1;
    property Id: WideString dispid 2;
    property Repetition: IRepetitionPattern dispid 3;
    property ExecutionTimeLimit: WideString dispid 4;
    property StartBoundary: WideString dispid 5;
    property EndBoundary: WideString dispid 6;
    property Enabled: WordBool dispid 7;
  end;

// *********************************************************************//
// Interface: IRegistrationTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4C8FEC3A-C218-4E0C-B23D-629024DB91A2}
// *********************************************************************//
  IRegistrationTrigger = interface(ITrigger)
    ['{4C8FEC3A-C218-4E0C-B23D-629024DB91A2}']
    function Get_Delay: WideString; safecall;
    procedure Set_Delay(const pDelay: WideString); safecall;
    property Delay: WideString read Get_Delay write Set_Delay;
  end;

// *********************************************************************//
// DispIntf:  IRegistrationTriggerDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4C8FEC3A-C218-4E0C-B23D-629024DB91A2}
// *********************************************************************//
  IRegistrationTriggerDisp = dispinterface
    ['{4C8FEC3A-C218-4E0C-B23D-629024DB91A2}']
    property Delay: WideString dispid 20;
    property type_: _TASK_TRIGGER_TYPE2 readonly dispid 1;
    property Id: WideString dispid 2;
    property Repetition: IRepetitionPattern dispid 3;
    property ExecutionTimeLimit: WideString dispid 4;
    property StartBoundary: WideString dispid 5;
    property EndBoundary: WideString dispid 6;
    property Enabled: WordBool dispid 7;
  end;

// *********************************************************************//
// Interface: IExecAction
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4C3D624D-FD6B-49A3-B9B7-09CB3CD3F047}
// *********************************************************************//
  IExecAction = interface(IAction)
    ['{4C3D624D-FD6B-49A3-B9B7-09CB3CD3F047}']
    function Get_Path: WideString; safecall;
    procedure Set_Path(const pPath: WideString); safecall;
    function Get_Arguments: WideString; safecall;
    procedure Set_Arguments(const pArgument: WideString); safecall;
    function Get_WorkingDirectory: WideString; safecall;
    procedure Set_WorkingDirectory(const pWorkingDirectory: WideString); safecall;
    property Path: WideString read Get_Path write Set_Path;
    property Arguments: WideString read Get_Arguments write Set_Arguments;
    property WorkingDirectory: WideString read Get_WorkingDirectory write Set_WorkingDirectory;
  end;

// *********************************************************************//
// DispIntf:  IExecActionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4C3D624D-FD6B-49A3-B9B7-09CB3CD3F047}
// *********************************************************************//
  IExecActionDisp = dispinterface
    ['{4C3D624D-FD6B-49A3-B9B7-09CB3CD3F047}']
    property Path: WideString dispid 10;
    property Arguments: WideString dispid 11;
    property WorkingDirectory: WideString dispid 12;
    property Id: WideString dispid 1;
    property type_: _TASK_ACTION_TYPE readonly dispid 2;
  end;

// *********************************************************************//
// Interface: IExecAction2
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {F2A82542-BDA5-4E6B-9143-E2BF4F8987B6}
// *********************************************************************//
  IExecAction2 = interface(IExecAction)
    ['{F2A82542-BDA5-4E6B-9143-E2BF4F8987B6}']
    function Get_HideAppWindow: WordBool; safecall;
    procedure Set_HideAppWindow(pHideAppWindow: WordBool); safecall;
    property HideAppWindow: WordBool read Get_HideAppWindow write Set_HideAppWindow;
  end;

// *********************************************************************//
// DispIntf:  IExecAction2Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {F2A82542-BDA5-4E6B-9143-E2BF4F8987B6}
// *********************************************************************//
  IExecAction2Disp = dispinterface
    ['{F2A82542-BDA5-4E6B-9143-E2BF4F8987B6}']
    property HideAppWindow: WordBool dispid 13;
    property Path: WideString dispid 10;
    property Arguments: WideString dispid 11;
    property WorkingDirectory: WideString dispid 12;
    property Id: WideString dispid 1;
    property type_: _TASK_ACTION_TYPE readonly dispid 2;
  end;

// *********************************************************************//
// Interface: IShowMessageAction
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {505E9E68-AF89-46B8-A30F-56162A83D537}
// *********************************************************************//
  IShowMessageAction = interface(IAction)
    ['{505E9E68-AF89-46B8-A30F-56162A83D537}']
    function Get_Title: WideString; safecall;
    procedure Set_Title(const pTitle: WideString); safecall;
    function Get_MessageBody: WideString; safecall;
    procedure Set_MessageBody(const pMessageBody: WideString); safecall;
    property Title: WideString read Get_Title write Set_Title;
    property MessageBody: WideString read Get_MessageBody write Set_MessageBody;
  end;

// *********************************************************************//
// DispIntf:  IShowMessageActionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {505E9E68-AF89-46B8-A30F-56162A83D537}
// *********************************************************************//
  IShowMessageActionDisp = dispinterface
    ['{505E9E68-AF89-46B8-A30F-56162A83D537}']
    property Title: WideString dispid 10;
    property MessageBody: WideString dispid 11;
    property Id: WideString dispid 1;
    property type_: _TASK_ACTION_TYPE readonly dispid 2;
  end;

// *********************************************************************//
// Interface: IComHandlerAction
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {6D2FD252-75C5-4F66-90BA-2A7D8CC3039F}
// *********************************************************************//
  IComHandlerAction = interface(IAction)
    ['{6D2FD252-75C5-4F66-90BA-2A7D8CC3039F}']
    function Get_ClassId: WideString; safecall;
    procedure Set_ClassId(const pClsid: WideString); safecall;
    function Get_Data: WideString; safecall;
    procedure Set_Data(const pData: WideString); safecall;
    property ClassId: WideString read Get_ClassId write Set_ClassId;
    property Data: WideString read Get_Data write Set_Data;
  end;

// *********************************************************************//
// DispIntf:  IComHandlerActionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {6D2FD252-75C5-4F66-90BA-2A7D8CC3039F}
// *********************************************************************//
  IComHandlerActionDisp = dispinterface
    ['{6D2FD252-75C5-4F66-90BA-2A7D8CC3039F}']
    property ClassId: WideString dispid 10;
    property Data: WideString dispid 11;
    property Id: WideString dispid 1;
    property type_: _TASK_ACTION_TYPE readonly dispid 2;
  end;

// *********************************************************************//
// Interface: IEmailAction
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {10F62C64-7E16-4314-A0C2-0C3683F99D40}
// *********************************************************************//
  IEmailAction = interface(IAction)
    ['{10F62C64-7E16-4314-A0C2-0C3683F99D40}']
    function Get_Server: WideString; safecall;
    procedure Set_Server(const pServer: WideString); safecall;
    function Get_Subject: WideString; safecall;
    procedure Set_Subject(const pSubject: WideString); safecall;
    function Get_To_: WideString; safecall;
    procedure Set_To_(const pTo: WideString); safecall;
    function Get_Cc: WideString; safecall;
    procedure Set_Cc(const pCc: WideString); safecall;
    function Get_Bcc: WideString; safecall;
    procedure Set_Bcc(const pBcc: WideString); safecall;
    function Get_ReplyTo: WideString; safecall;
    procedure Set_ReplyTo(const pReplyTo: WideString); safecall;
    function Get_From: WideString; safecall;
    procedure Set_From(const pFrom: WideString); safecall;
    function Get_HeaderFields: ITaskNamedValueCollection; safecall;
    procedure Set_HeaderFields(const ppHeaderFields: ITaskNamedValueCollection); safecall;
    function Get_Body: WideString; safecall;
    procedure Set_Body(const pBody: WideString); safecall;
    function Get_Attachments: PSafeArray; safecall;
    procedure Set_Attachments(pAttachements: PSafeArray); safecall;
    property Server: WideString read Get_Server write Set_Server;
    property Subject: WideString read Get_Subject write Set_Subject;
    property To_: WideString read Get_To_ write Set_To_;
    property Cc: WideString read Get_Cc write Set_Cc;
    property Bcc: WideString read Get_Bcc write Set_Bcc;
    property ReplyTo: WideString read Get_ReplyTo write Set_ReplyTo;
    property From: WideString read Get_From write Set_From;
    property HeaderFields: ITaskNamedValueCollection read Get_HeaderFields write Set_HeaderFields;
    property Body: WideString read Get_Body write Set_Body;
    property Attachments: PSafeArray read Get_Attachments write Set_Attachments;
  end;

// *********************************************************************//
// DispIntf:  IEmailActionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {10F62C64-7E16-4314-A0C2-0C3683F99D40}
// *********************************************************************//
  IEmailActionDisp = dispinterface
    ['{10F62C64-7E16-4314-A0C2-0C3683F99D40}']
    property Server: WideString dispid 10;
    property Subject: WideString dispid 11;
    property To_: WideString dispid 12;
    property Cc: WideString dispid 13;
    property Bcc: WideString dispid 14;
    property ReplyTo: WideString dispid 15;
    property From: WideString dispid 16;
    property HeaderFields: ITaskNamedValueCollection dispid 17;
    property Body: WideString dispid 18;
    property Attachments: {NOT_OLEAUTO(PSafeArray)}OleVariant dispid 19;
    property Id: WideString dispid 1;
    property type_: _TASK_ACTION_TYPE readonly dispid 2;
  end;

// *********************************************************************//
// Interface: IPrincipal2
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {248919AE-E345-4A6D-8AEB-E0D3165C904E}
// *********************************************************************//
  IPrincipal2 = interface(IDispatch)
    ['{248919AE-E345-4A6D-8AEB-E0D3165C904E}']
    function Get_ProcessTokenSidType: _TASK_PROCESSTOKENSID; safecall;
    procedure Set_ProcessTokenSidType(pProcessTokenSidType: _TASK_PROCESSTOKENSID); safecall;
    function Get_RequiredPrivilegeCount: Integer; safecall;
    function Get_RequiredPrivilege(index: Integer): WideString; safecall;
    procedure AddRequiredPrivilege(const privilege: WideString); safecall;
    property ProcessTokenSidType: _TASK_PROCESSTOKENSID read Get_ProcessTokenSidType write Set_ProcessTokenSidType;
    property RequiredPrivilegeCount: Integer read Get_RequiredPrivilegeCount;
    property RequiredPrivilege[index: Integer]: WideString read Get_RequiredPrivilege;
  end;

// *********************************************************************//
// DispIntf:  IPrincipal2Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {248919AE-E345-4A6D-8AEB-E0D3165C904E}
// *********************************************************************//
  IPrincipal2Disp = dispinterface
    ['{248919AE-E345-4A6D-8AEB-E0D3165C904E}']
    property ProcessTokenSidType: _TASK_PROCESSTOKENSID dispid 10;
    property RequiredPrivilegeCount: Integer readonly dispid 11;
    property RequiredPrivilege[index: Integer]: WideString readonly dispid 12;
    procedure AddRequiredPrivilege(const privilege: WideString); dispid 13;
  end;

// *********************************************************************//
// Interface: ITaskSettings2
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {2C05C3F0-6EED-4C05-A15F-ED7D7A98A369}
// *********************************************************************//
  ITaskSettings2 = interface(IDispatch)
    ['{2C05C3F0-6EED-4C05-A15F-ED7D7A98A369}']
    function Get_DisallowStartOnRemoteAppSession: WordBool; safecall;
    procedure Set_DisallowStartOnRemoteAppSession(pDisallowStart: WordBool); safecall;
    function Get_UseUnifiedSchedulingEngine: WordBool; safecall;
    procedure Set_UseUnifiedSchedulingEngine(pUseUnifiedEngine: WordBool); safecall;
    property DisallowStartOnRemoteAppSession: WordBool read Get_DisallowStartOnRemoteAppSession write Set_DisallowStartOnRemoteAppSession;
    property UseUnifiedSchedulingEngine: WordBool read Get_UseUnifiedSchedulingEngine write Set_UseUnifiedSchedulingEngine;
  end;

// *********************************************************************//
// DispIntf:  ITaskSettings2Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {2C05C3F0-6EED-4C05-A15F-ED7D7A98A369}
// *********************************************************************//
  ITaskSettings2Disp = dispinterface
    ['{2C05C3F0-6EED-4C05-A15F-ED7D7A98A369}']
    property DisallowStartOnRemoteAppSession: WordBool dispid 30;
    property UseUnifiedSchedulingEngine: WordBool dispid 31;
  end;

// *********************************************************************//
// Interface: ITaskSettings3
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0AD9D0D7-0C7F-4EBB-9A5F-D1C648DCA528}
// *********************************************************************//
  ITaskSettings3 = interface(ITaskSettings)
    ['{0AD9D0D7-0C7F-4EBB-9A5F-D1C648DCA528}']
    function Get_DisallowStartOnRemoteAppSession: WordBool; safecall;
    procedure Set_DisallowStartOnRemoteAppSession(pDisallowStart: WordBool); safecall;
    function Get_UseUnifiedSchedulingEngine: WordBool; safecall;
    procedure Set_UseUnifiedSchedulingEngine(pUseUnifiedEngine: WordBool); safecall;
    function Get_MaintenanceSettings: IMaintenanceSettings; safecall;
    procedure Set_MaintenanceSettings(const ppMaintenanceSettings: IMaintenanceSettings); safecall;
    function CreateMaintenanceSettings: IMaintenanceSettings; safecall;
    function Get_Volatile: WordBool; safecall;
    procedure Set_Volatile(pVolatile: WordBool); safecall;
    property DisallowStartOnRemoteAppSession: WordBool read Get_DisallowStartOnRemoteAppSession write Set_DisallowStartOnRemoteAppSession;
    property UseUnifiedSchedulingEngine: WordBool read Get_UseUnifiedSchedulingEngine write Set_UseUnifiedSchedulingEngine;
    property MaintenanceSettings: IMaintenanceSettings read Get_MaintenanceSettings write Set_MaintenanceSettings;
    property Volatile: WordBool read Get_Volatile write Set_Volatile;
  end;

// *********************************************************************//
// DispIntf:  ITaskSettings3Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0AD9D0D7-0C7F-4EBB-9A5F-D1C648DCA528}
// *********************************************************************//
  ITaskSettings3Disp = dispinterface
    ['{0AD9D0D7-0C7F-4EBB-9A5F-D1C648DCA528}']
    property DisallowStartOnRemoteAppSession: WordBool dispid 30;
    property UseUnifiedSchedulingEngine: WordBool dispid 31;
    property MaintenanceSettings: IMaintenanceSettings dispid 40;
    function CreateMaintenanceSettings: IMaintenanceSettings; dispid 41;
    property Volatile: WordBool dispid 42;
    property AllowDemandStart: WordBool dispid 3;
    property RestartInterval: WideString dispid 4;
    property RestartCount: SYSINT dispid 5;
    property MultipleInstances: _TASK_INSTANCES_POLICY dispid 6;
    property StopIfGoingOnBatteries: WordBool dispid 7;
    property DisallowStartIfOnBatteries: WordBool dispid 8;
    property AllowHardTerminate: WordBool dispid 9;
    property StartWhenAvailable: WordBool dispid 10;
    property XmlText: WideString dispid 11;
    property RunOnlyIfNetworkAvailable: WordBool dispid 12;
    property ExecutionTimeLimit: WideString dispid 13;
    property Enabled: WordBool dispid 14;
    property DeleteExpiredTaskAfter: WideString dispid 15;
    property Priority: SYSINT dispid 16;
    property Compatibility: _TASK_COMPATIBILITY dispid 17;
    property Hidden: WordBool dispid 18;
    property IdleSettings: IIdleSettings dispid 19;
    property RunOnlyIfIdle: WordBool dispid 20;
    property WakeToRun: WordBool dispid 21;
    property NetworkSettings: INetworkSettings dispid 22;
  end;

// *********************************************************************//
// Interface: IMaintenanceSettings
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A6024FA8-9652-4ADB-A6BF-5CFCD877A7BA}
// *********************************************************************//
  IMaintenanceSettings = interface(IDispatch)
    ['{A6024FA8-9652-4ADB-A6BF-5CFCD877A7BA}']
    procedure Set_Period(const target: WideString); safecall;
    function Get_Period: WideString; safecall;
    procedure Set_Deadline(const target: WideString); safecall;
    function Get_Deadline: WideString; safecall;
    procedure Set_Exclusive(target: WordBool); safecall;
    function Get_Exclusive: WordBool; safecall;
    property Period: WideString read Get_Period write Set_Period;
    property Deadline: WideString read Get_Deadline write Set_Deadline;
    property Exclusive: WordBool read Get_Exclusive write Set_Exclusive;
  end;

// *********************************************************************//
// DispIntf:  IMaintenanceSettingsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A6024FA8-9652-4ADB-A6BF-5CFCD877A7BA}
// *********************************************************************//
  IMaintenanceSettingsDisp = dispinterface
    ['{A6024FA8-9652-4ADB-A6BF-5CFCD877A7BA}']
    property Period: WideString dispid 34;
    property Deadline: WideString dispid 35;
    property Exclusive: WordBool dispid 36;
  end;

// *********************************************************************//
// The Class CoTaskScheduler_ provides a Create and CreateRemote method to          
// create instances of the default interface ITaskService exposed by              
// the CoClass TaskScheduler_. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoTaskScheduler_ = class
    class function Create: ITaskService;
    class function CreateRemote(const MachineName: string): ITaskService;
  end;

// *********************************************************************//
// The Class CoTaskHandlerPS provides a Create and CreateRemote method to          
// create instances of the default interface ITaskHandler exposed by              
// the CoClass TaskHandlerPS. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoTaskHandlerPS = class
    class function Create: ITaskHandler;
    class function CreateRemote(const MachineName: string): ITaskHandler;
  end;

// *********************************************************************//
// The Class CoTaskHandlerStatusPS provides a Create and CreateRemote method to          
// create instances of the default interface ITaskHandlerStatus exposed by              
// the CoClass TaskHandlerStatusPS. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoTaskHandlerStatusPS = class
    class function Create: ITaskHandlerStatus;
    class function CreateRemote(const MachineName: string): ITaskHandlerStatus;
  end;

implementation

uses System.Win.ComObj;

class function CoTaskScheduler_.Create: ITaskService;
begin
  Result := CreateComObject(CLASS_TaskScheduler_) as ITaskService;
end;

class function CoTaskScheduler_.CreateRemote(const MachineName: string): ITaskService;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_TaskScheduler_) as ITaskService;
end;

class function CoTaskHandlerPS.Create: ITaskHandler;
begin
  Result := CreateComObject(CLASS_TaskHandlerPS) as ITaskHandler;
end;

class function CoTaskHandlerPS.CreateRemote(const MachineName: string): ITaskHandler;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_TaskHandlerPS) as ITaskHandler;
end;

class function CoTaskHandlerStatusPS.Create: ITaskHandlerStatus;
begin
  Result := CreateComObject(CLASS_TaskHandlerStatusPS) as ITaskHandlerStatus;
end;

class function CoTaskHandlerStatusPS.CreateRemote(const MachineName: string): ITaskHandlerStatus;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_TaskHandlerStatusPS) as ITaskHandlerStatus;
end;

end.
