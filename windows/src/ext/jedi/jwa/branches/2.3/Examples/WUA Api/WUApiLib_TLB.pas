unit WUApiLib_TLB;

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

// $Rev: 8291 $
// File generated on 05.06.2008 11:31:47 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Windows\system32\wuapi.dll (1)
// LIBID: {B596CC9F-56E5-419E-A622-E01BB457431E}
// LCID: 0
// Helpfile: 
// HelpString: WUAPI 2.0 Type Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
// Errors:
//   Hint: Member 'NotificationLevel' of 'IAutomaticUpdatesSettings' changed to 'NotificationLevel1'
//   Hint: Member 'IncludeRecommendedUpdates' of 'IAutomaticUpdatesSettings2' changed to 'IncludeRecommendedUpdates1'
//   Hint: Member 'CanAutomaticallyUpgradeService' of 'IUpdateSearcher' changed to 'CanAutomaticallyUpgradeService1'
//   Hint: Member 'ClientApplicationID' of 'IUpdateSearcher' changed to 'ClientApplicationID1'
//   Hint: Member 'ServerSelection' of 'IUpdateSearcher' changed to 'ServerSelection1'
//   Hint: Member 'Address' of 'IWebProxy' changed to 'Address1'
//   Hint: Member 'ClientApplicationID' of 'IUpdateDownloader' changed to 'ClientApplicationID1'
//   Hint: Member 'ClientApplicationID' of 'IUpdateInstaller' changed to 'ClientApplicationID1'
//   Hint: Member 'ClientApplicationID' of 'IUpdateSession' changed to 'ClientApplicationID1'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
// ************************************************************************ //
// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  WUApiLibMajorVersion = 2;
  WUApiLibMinorVersion = 0;

  LIBID_WUApiLib: TGUID = '{B596CC9F-56E5-419E-A622-E01BB457431E}';

  IID_IUpdateLockdown: TGUID = '{A976C28D-75A1-42AA-94AE-8AF8B872089A}';
  IID_IUpdateException: TGUID = '{A376DD5E-09D4-427F-AF7C-FED5B6E1C1D6}';
  IID_IInvalidProductLicenseException: TGUID = '{A37D00F5-7BB0-4953-B414-F9E98326F2E8}';
  IID_IAutomaticUpdatesSettings: TGUID = '{2EE48F22-AF3C-405F-8970-F71BE12EE9A2}';
  IID_IAutomaticUpdatesSettings2: TGUID = '{6ABC136A-C3CA-4384-8171-CB2B1E59B8DC}';
  IID_IUpdate: TGUID = '{6A92B07A-D821-4682-B423-5C805022CC4D}';
  IID_IUpdateCollection: TGUID = '{07F7438C-7709-4CA5-B518-91279288134E}';
  IID_ICategoryCollection: TGUID = '{3A56BFB8-576C-43F7-9335-FE4838FD7E37}';
  IID_ICategory: TGUID = '{81DDC1B8-9D35-47A6-B471-5B80F519223B}';
  IID_IImageInformation: TGUID = '{7C907864-346C-4AEB-8F3F-57DA289F969F}';
  IID_IUpdateIdentity: TGUID = '{46297823-9940-4C09-AED9-CD3EA6D05968}';
  IID_IInstallationBehavior: TGUID = '{D9A59339-E245-4DBD-9686-4D5763E39624}';
  IID_IStringCollection: TGUID = '{EFF90582-2DDC-480F-A06D-60F3FBC362C3}';
  IID_IUpdateDownloadContentCollection: TGUID = '{BC5513C8-B3B8-4BF7-A4D4-361C0D8C88BA}';
  IID_IUpdateDownloadContent: TGUID = '{54A2CB2D-9A0C-48B6-8A50-9ABB69EE2D02}';
  IID_IUpdate2: TGUID = '{144FE9B0-D23D-4A8B-8634-FB4457533B7A}';
  IID_IUpdate3: TGUID = '{112EDA6B-95B3-476F-9D90-AEE82C6B8181}';
  IID_IWindowsDriverUpdate: TGUID = '{B383CD1A-5CE9-4504-9F63-764B1236F191}';
  IID_IWindowsDriverUpdate2: TGUID = '{615C4269-7A48-43BD-96B7-BF6CA27D6C3E}';
  IID_IWindowsDriverUpdate3: TGUID = '{49EBD502-4A96-41BD-9E3E-4C5057F4250C}';
  IID_ISearchCompletedCallback: TGUID = '{88AEE058-D4B0-4725-A2F1-814A67AE964C}';
  IID_ISearchJob: TGUID = '{7366EA16-7A1A-4EA2-B042-973D3E9CD99B}';
  IID_ISearchCompletedCallbackArgs: TGUID = '{A700A634-2850-4C47-938A-9E4B6E5AF9A6}';
  IID_IDownloadCompletedCallback: TGUID = '{77254866-9F5B-4C8E-B9E2-C77A8530D64B}';
  IID_IDownloadJob: TGUID = '{C574DE85-7358-43F6-AAE8-8697E62D8BA7}';
  IID_IDownloadProgress: TGUID = '{D31A5BAC-F719-4178-9DBB-5E2CB47FD18A}';
  IID_IUpdateDownloadResult: TGUID = '{BF99AF76-B575-42AD-8AA4-33CBB5477AF1}';
  IID_IDownloadCompletedCallbackArgs: TGUID = '{FA565B23-498C-47A0-979D-E7D5B1813360}';
  IID_IDownloadProgressChangedCallback: TGUID = '{8C3F1CDD-6173-4591-AEBD-A56A53CA77C1}';
  IID_IDownloadProgressChangedCallbackArgs: TGUID = '{324FF2C6-4981-4B04-9412-57481745AB24}';
  IID_IInstallationCompletedCallback: TGUID = '{45F4F6F3-D602-4F98-9A8A-3EFA152AD2D3}';
  IID_IInstallationJob: TGUID = '{5C209F0B-BAD5-432A-9556-4699BED2638A}';
  IID_IInstallationProgress: TGUID = '{345C8244-43A3-4E32-A368-65F073B76F36}';
  IID_IUpdateInstallationResult: TGUID = '{D940F0F8-3CBB-4FD0-993F-471E7F2328AD}';
  IID_IInstallationCompletedCallbackArgs: TGUID = '{250E2106-8EFB-4705-9653-EF13C581B6A1}';
  IID_IInstallationProgressChangedCallback: TGUID = '{E01402D5-F8DA-43BA-A012-38894BD048F1}';
  IID_IInstallationProgressChangedCallbackArgs: TGUID = '{E4F14E1E-689D-4218-A0B9-BC189C484A01}';
  IID_IUpdateHistoryEntry: TGUID = '{BE56A644-AF0E-4E0E-A311-C1D8E695CBFF}';
  IID_IUpdateHistoryEntry2: TGUID = '{C2BFB780-4539-4132-AB8C-0A8772013AB6}';
  IID_IUpdateDownloadContent2: TGUID = '{C97AD11B-F257-420B-9D9F-377F733F6F68}';
  CLASS_StringCollection: TGUID = '{72C97D74-7C3B-40AE-B77D-ABDB22EBA6FB}';
  IID_IUpdateSearcher: TGUID = '{8F45ABF1-F9AE-4B95-A933-F0F66E5056EA}';
  IID_IUpdateSearcher2: TGUID = '{4CBDCB2D-1589-4BEB-BD1C-3E582FF0ADD0}';
  CLASS_UpdateSearcher: TGUID = '{B699E5E8-67FF-4177-88B0-3684A3388BFB}';
  IID_ISearchResult: TGUID = '{D40CFF62-E08C-4498-941A-01E25F0FD33C}';
  IID_IUpdateExceptionCollection: TGUID = '{503626A3-8E14-4729-9355-0FE664BD2321}';
  IID_IUpdateHistoryEntryCollection: TGUID = '{A7F04F3C-A290-435B-AADF-A116C3357A5C}';
  IID_IWebProxy: TGUID = '{174C81FE-AECD-4DAE-B8A0-2C6318DD86A8}';
  CLASS_WebProxy: TGUID = '{650503CF-9108-4DDC-A2CE-6C2341E1C582}';
  IID_ISystemInformation: TGUID = '{ADE87BF7-7B56-4275-8FAB-B9B0E591844B}';
  CLASS_SystemInformation: TGUID = '{C01B9BA0-BEA7-41BA-B604-D0A36F469133}';
  IID_IWindowsUpdateAgentInfo: TGUID = '{85713FA1-7796-4FA2-BE3B-E2D6124DD373}';
  CLASS_WindowsUpdateAgentInfo: TGUID = '{C2E88C2F-6F5B-4AAA-894B-55C847AD3A2D}';
  IID_IAutomaticUpdates: TGUID = '{673425BF-C082-4C7C-BDFD-569464B8E0CE}';
  IID_IAutomaticUpdates2: TGUID = '{4A2F5C31-CFD9-410E-B7FB-29A653973A0F}';
  CLASS_AutomaticUpdates: TGUID = '{BFE18E9C-6D87-4450-B37C-E02F0B373803}';
  IID_IAutomaticUpdatesResults: TGUID = '{E7A4D634-7942-4DD9-A111-82228BA33901}';
  CLASS_UpdateCollection: TGUID = '{13639463-00DB-4646-803D-528026140D88}';
  IID_IUpdateDownloader: TGUID = '{68F1C6F9-7ECC-4666-A464-247FE12496C3}';
  CLASS_UpdateDownloader: TGUID = '{5BAF654A-5A07-4264-A255-9FF54C7151E7}';
  IID_IDownloadResult: TGUID = '{DAA4FDD0-4727-4DBE-A1E7-745DCA317144}';
  IID_IUpdateInstaller: TGUID = '{7B929C68-CCDC-4226-96B1-8724600B54C2}';
  IID_IUpdateInstaller2: TGUID = '{3442D4FE-224D-4CEE-98CF-30E0C4D229E6}';
  CLASS_UpdateInstaller: TGUID = '{D2E0FE7F-D23E-48E1-93C0-6FA8CC346474}';
  IID_IInstallationResult: TGUID = '{A43C56D6-7451-48D4-AF96-B6CD2D0D9B7A}';
  IID_IUpdateSession: TGUID = '{816858A4-260D-4260-933A-2585F1ABC76B}';
  IID_IUpdateSession2: TGUID = '{91CAF7B0-EB23-49ED-9937-C52D817F46F7}';
  IID_IUpdateSession3: TGUID = '{918EFD1E-B5D8-4C90-8540-AEB9BDC56F9D}';
  CLASS_UpdateSession: TGUID = '{4CB43D7F-7EEE-4906-8698-60DA1C38F2FE}';
  IID_IUpdateServiceManager: TGUID = '{23857E3C-02BA-44A3-9423-B1C900805F37}';
  IID_IUpdateServiceManager2: TGUID = '{0BB8531D-7E8D-424F-986C-A0B8F60A3E7B}';
  IID_IUpdateServiceCollection: TGUID = '{9B0353AA-0E52-44FF-B8B0-1F7FA0437F88}';
  IID_IUpdateService: TGUID = '{76B3B17E-AED6-4DA5-85F0-83587F81ABE3}';
  IID_IUpdateServiceRegistration: TGUID = '{DDE02280-12B3-4E0B-937B-6747F6ACB286}';
  IID_IUpdateService2: TGUID = '{1518B460-6518-4172-940F-C75883B24CEB}';
  CLASS_UpdateServiceManager: TGUID = '{F8D253D9-89A4-4DAA-87B6-1168369F0B21}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum tagUpdateLockdownOption
type
  tagUpdateLockdownOption = TOleEnum;
const
  uloForWebsiteAccess = $00000001;

// Constants for enum tagAddServiceFlag
type
  tagAddServiceFlag = TOleEnum;
const
  asfAllowPendingRegistration = $00000001;
  asfAllowOnlineRegistration = $00000002;
  asfRegisterServiceWithAU = $00000004;

// Constants for enum tagUpdateServiceOption
type
  tagUpdateServiceOption = TOleEnum;
const
  usoNonVolatileService = $00000001;

// Constants for enum tagUpdateExceptionContext
type
  tagUpdateExceptionContext = TOleEnum;
const
  uecGeneral = $00000001;
  uecWindowsDriver = $00000002;
  uecWindowsInstaller = $00000003;

// Constants for enum tagAutomaticUpdatesNotificationLevel
type
  tagAutomaticUpdatesNotificationLevel = TOleEnum;
const
  aunlNotConfigured = $00000000;
  aunlDisabled = $00000001;
  aunlNotifyBeforeDownload = $00000002;
  aunlNotifyBeforeInstallation = $00000003;
  aunlScheduledInstallation = $00000004;

// Constants for enum tagAutomaticUpdatesScheduledInstallationDay
type
  tagAutomaticUpdatesScheduledInstallationDay = TOleEnum;
const
  ausidEveryDay = $00000000;
  ausidEverySunday = $00000001;
  ausidEveryMonday = $00000002;
  ausidEveryTuesday = $00000003;
  ausidEveryWednesday = $00000004;
  ausidEveryThursday = $00000005;
  ausidEveryFriday = $00000006;
  ausidEverySaturday = $00000007;

// Constants for enum tagAutomaticUpdatesUserType
type
  tagAutomaticUpdatesUserType = TOleEnum;
const
  auutCurrentUser = $00000001;
  auutLocalAdministrator = $00000002;

// Constants for enum tagAutomaticUpdatesPermissionType
type
  tagAutomaticUpdatesPermissionType = TOleEnum;
const
  auptSetNotificationLevel = $00000001;
  auptDisableAutomaticUpdates = $00000002;
  auptSetIncludeRecommendedUpdates = $00000003;

// Constants for enum tagInstallationImpact
type
  tagInstallationImpact = TOleEnum;
const
  iiNormal = $00000000;
  iiMinor = $00000001;
  iiRequiresExclusiveHandling = $00000002;

// Constants for enum tagInstallationRebootBehavior
type
  tagInstallationRebootBehavior = TOleEnum;
const
  irbNeverReboots = $00000000;
  irbAlwaysRequiresReboot = $00000001;
  irbCanRequestReboot = $00000002;

// Constants for enum tagUpdateType
type
  tagUpdateType = TOleEnum;
const
  utSoftware = $00000001;
  utDriver = $00000002;

// Constants for enum tagDeploymentAction
type
  tagDeploymentAction = TOleEnum;
const
  daNone = $00000000;
  daInstallation = $00000001;
  daUninstallation = $00000002;
  daDetection = $00000003;

// Constants for enum tagDownloadPriority
type
  tagDownloadPriority = TOleEnum;
const
  dpLow = $00000001;
  dpNormal = $00000002;
  dpHigh = $00000003;

// Constants for enum tagOperationResultCode
type
  tagOperationResultCode = TOleEnum;
const
  orcNotStarted = $00000000;
  orcInProgress = $00000001;
  orcSucceeded = $00000002;
  orcSucceededWithErrors = $00000003;
  orcFailed = $00000004;
  orcAborted = $00000005;

// Constants for enum tagDownloadPhase
type
  tagDownloadPhase = TOleEnum;
const
  dphInitializing = $00000001;
  dphDownloading = $00000002;
  dphVerifying = $00000003;

// Constants for enum tagUpdateOperation
type
  tagUpdateOperation = TOleEnum;
const
  uoInstallation = $00000001;
  uoUninstallation = $00000002;

// Constants for enum tagServerSelection
type
  tagServerSelection = TOleEnum;
const
  ssDefault = $00000000;
  ssManagedServer = $00000001;
  ssWindowsUpdate = $00000002;
  ssOthers = $00000003;

// Constants for enum tagUpdateServiceRegistrationState
type
  tagUpdateServiceRegistrationState = TOleEnum;
const
  usrsNotRegistered = $00000001;
  usrsRegistrationPending = $00000002;
  usrsRegistered = $00000003;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IUpdateLockdown = interface;
  IUpdateException = interface;
  IUpdateExceptionDisp = dispinterface;
  IInvalidProductLicenseException = interface;
  IInvalidProductLicenseExceptionDisp = dispinterface;
  IAutomaticUpdatesSettings = interface;
  IAutomaticUpdatesSettingsDisp = dispinterface;
  IAutomaticUpdatesSettings2 = interface;
  IAutomaticUpdatesSettings2Disp = dispinterface;
  IUpdate = interface;
  IUpdateDisp = dispinterface;
  IUpdateCollection = interface;
  IUpdateCollectionDisp = dispinterface;
  ICategoryCollection = interface;
  ICategoryCollectionDisp = dispinterface;
  ICategory = interface;
  ICategoryDisp = dispinterface;
  IImageInformation = interface;
  IImageInformationDisp = dispinterface;
  IUpdateIdentity = interface;
  IUpdateIdentityDisp = dispinterface;
  IInstallationBehavior = interface;
  IInstallationBehaviorDisp = dispinterface;
  IStringCollection = interface;
  IStringCollectionDisp = dispinterface;
  IUpdateDownloadContentCollection = interface;
  IUpdateDownloadContentCollectionDisp = dispinterface;
  IUpdateDownloadContent = interface;
  IUpdateDownloadContentDisp = dispinterface;
  IUpdate2 = interface;
  IUpdate2Disp = dispinterface;
  IUpdate3 = interface;
  IUpdate3Disp = dispinterface;
  IWindowsDriverUpdate = interface;
  IWindowsDriverUpdateDisp = dispinterface;
  IWindowsDriverUpdate2 = interface;
  IWindowsDriverUpdate2Disp = dispinterface;
  IWindowsDriverUpdate3 = interface;
  IWindowsDriverUpdate3Disp = dispinterface;
  ISearchCompletedCallback = interface;
  ISearchJob = interface;
  ISearchJobDisp = dispinterface;
  ISearchCompletedCallbackArgs = interface;
  ISearchCompletedCallbackArgsDisp = dispinterface;
  IDownloadCompletedCallback = interface;
  IDownloadJob = interface;
  IDownloadJobDisp = dispinterface;
  IDownloadProgress = interface;
  IDownloadProgressDisp = dispinterface;
  IUpdateDownloadResult = interface;
  IUpdateDownloadResultDisp = dispinterface;
  IDownloadCompletedCallbackArgs = interface;
  IDownloadCompletedCallbackArgsDisp = dispinterface;
  IDownloadProgressChangedCallback = interface;
  IDownloadProgressChangedCallbackArgs = interface;
  IDownloadProgressChangedCallbackArgsDisp = dispinterface;
  IInstallationCompletedCallback = interface;
  IInstallationJob = interface;
  IInstallationJobDisp = dispinterface;
  IInstallationProgress = interface;
  IInstallationProgressDisp = dispinterface;
  IUpdateInstallationResult = interface;
  IUpdateInstallationResultDisp = dispinterface;
  IInstallationCompletedCallbackArgs = interface;
  IInstallationCompletedCallbackArgsDisp = dispinterface;
  IInstallationProgressChangedCallback = interface;
  IInstallationProgressChangedCallbackArgs = interface;
  IInstallationProgressChangedCallbackArgsDisp = dispinterface;
  IUpdateHistoryEntry = interface;
  IUpdateHistoryEntryDisp = dispinterface;
  IUpdateHistoryEntry2 = interface;
  IUpdateHistoryEntry2Disp = dispinterface;
  IUpdateDownloadContent2 = interface;
  IUpdateDownloadContent2Disp = dispinterface;
  IUpdateSearcher = interface;
  IUpdateSearcherDisp = dispinterface;
  IUpdateSearcher2 = interface;
  IUpdateSearcher2Disp = dispinterface;
  ISearchResult = interface;
  ISearchResultDisp = dispinterface;
  IUpdateExceptionCollection = interface;
  IUpdateExceptionCollectionDisp = dispinterface;
  IUpdateHistoryEntryCollection = interface;
  IUpdateHistoryEntryCollectionDisp = dispinterface;
  IWebProxy = interface;
  IWebProxyDisp = dispinterface;
  ISystemInformation = interface;
  ISystemInformationDisp = dispinterface;
  IWindowsUpdateAgentInfo = interface;
  IWindowsUpdateAgentInfoDisp = dispinterface;
  IAutomaticUpdates = interface;
  IAutomaticUpdatesDisp = dispinterface;
  IAutomaticUpdates2 = interface;
  IAutomaticUpdates2Disp = dispinterface;
  IAutomaticUpdatesResults = interface;
  IAutomaticUpdatesResultsDisp = dispinterface;
  IUpdateDownloader = interface;
  IUpdateDownloaderDisp = dispinterface;
  IDownloadResult = interface;
  IDownloadResultDisp = dispinterface;
  IUpdateInstaller = interface;
  IUpdateInstallerDisp = dispinterface;
  IUpdateInstaller2 = interface;
  IUpdateInstaller2Disp = dispinterface;
  IInstallationResult = interface;
  IInstallationResultDisp = dispinterface;
  IUpdateSession = interface;
  IUpdateSessionDisp = dispinterface;
  IUpdateSession2 = interface;
  IUpdateSession2Disp = dispinterface;
  IUpdateSession3 = interface;
  IUpdateSession3Disp = dispinterface;
  IUpdateServiceManager = interface;
  IUpdateServiceManagerDisp = dispinterface;
  IUpdateServiceManager2 = interface;
  IUpdateServiceManager2Disp = dispinterface;
  IUpdateServiceCollection = interface;
  IUpdateServiceCollectionDisp = dispinterface;
  IUpdateService = interface;
  IUpdateServiceDisp = dispinterface;
  IUpdateServiceRegistration = interface;
  IUpdateServiceRegistrationDisp = dispinterface;
  IUpdateService2 = interface;
  IUpdateService2Disp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  StringCollection = IStringCollection;
  UpdateSearcher = IUpdateSearcher2;
  WebProxy = IWebProxy;
  SystemInformation = ISystemInformation;
  WindowsUpdateAgentInfo = IWindowsUpdateAgentInfo;
  AutomaticUpdates = IAutomaticUpdates2;
  UpdateCollection = IUpdateCollection;
  UpdateDownloader = IUpdateDownloader;
  UpdateInstaller = IUpdateInstaller2;
  UpdateSession = IUpdateSession3;
  UpdateServiceManager = IUpdateServiceManager2;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  wireHWND = ^_RemotableHandle; 

  UpdateLockdownOption = tagUpdateLockdownOption; 
  AddServiceFlag = tagAddServiceFlag; 
  UpdateServiceOption = tagUpdateServiceOption; 
  UpdateExceptionContext = tagUpdateExceptionContext; 
  AutomaticUpdatesNotificationLevel = tagAutomaticUpdatesNotificationLevel; 
  AutomaticUpdatesScheduledInstallationDay = tagAutomaticUpdatesScheduledInstallationDay; 
  AutomaticUpdatesUserType = tagAutomaticUpdatesUserType; 
  AutomaticUpdatesPermissionType = tagAutomaticUpdatesPermissionType; 
  InstallationImpact = tagInstallationImpact; 
  InstallationRebootBehavior = tagInstallationRebootBehavior; 
  UpdateType = tagUpdateType; 
  DeploymentAction = tagDeploymentAction; 
  DownloadPriority = tagDownloadPriority; 
  OperationResultCode = tagOperationResultCode; 
  DownloadPhase = tagDownloadPhase; 
  UpdateOperation = tagUpdateOperation; 
  ServerSelection = tagServerSelection; 

  __MIDL_IWinTypes_0009 = record
    case Integer of
      0: (hInproc: Integer);
      1: (hRemote: Integer);
  end;

  _RemotableHandle = packed record
    fContext: Integer;
    u: __MIDL_IWinTypes_0009;
  end;

  UpdateServiceRegistrationState = tagUpdateServiceRegistrationState; 

// *********************************************************************//
// Interface: IUpdateLockdown
// Flags:     (384) NonExtensible OleAutomation
// GUID:      {A976C28D-75A1-42AA-94AE-8AF8B872089A}
// *********************************************************************//
  IUpdateLockdown = interface(IUnknown)
    ['{A976C28D-75A1-42AA-94AE-8AF8B872089A}']
    function LockDown(flags: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IUpdateException
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A376DD5E-09D4-427F-AF7C-FED5B6E1C1D6}
// *********************************************************************//
  IUpdateException = interface(IDispatch)
    ['{A376DD5E-09D4-427F-AF7C-FED5B6E1C1D6}']
    function Get_Message: WideString; safecall;
    function Get_HResult: Integer; safecall;
    function Get_Context: UpdateExceptionContext; safecall;
    property Message: WideString read Get_Message;
    property HResult: Integer read Get_HResult;
    property Context: UpdateExceptionContext read Get_Context;
  end;

// *********************************************************************//
// DispIntf:  IUpdateExceptionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A376DD5E-09D4-427F-AF7C-FED5B6E1C1D6}
// *********************************************************************//
  IUpdateExceptionDisp = dispinterface
    ['{A376DD5E-09D4-427F-AF7C-FED5B6E1C1D6}']
    property Message: WideString readonly dispid 0;
    property HResult: Integer readonly dispid 1610743809;
    property Context: UpdateExceptionContext readonly dispid 1610743810;
  end;

// *********************************************************************//
// Interface: IInvalidProductLicenseException
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A37D00F5-7BB0-4953-B414-F9E98326F2E8}
// *********************************************************************//
  IInvalidProductLicenseException = interface(IUpdateException)
    ['{A37D00F5-7BB0-4953-B414-F9E98326F2E8}']
    function Get_Product: WideString; safecall;
    property Product: WideString read Get_Product;
  end;

// *********************************************************************//
// DispIntf:  IInvalidProductLicenseExceptionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A37D00F5-7BB0-4953-B414-F9E98326F2E8}
// *********************************************************************//
  IInvalidProductLicenseExceptionDisp = dispinterface
    ['{A37D00F5-7BB0-4953-B414-F9E98326F2E8}']
    property Product: WideString readonly dispid 1610809345;
    property Message: WideString readonly dispid 0;
    property HResult: Integer readonly dispid 1610743809;
    property Context: UpdateExceptionContext readonly dispid 1610743810;
  end;

// *********************************************************************//
// Interface: IAutomaticUpdatesSettings
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {2EE48F22-AF3C-405F-8970-F71BE12EE9A2}
// *********************************************************************//
  IAutomaticUpdatesSettings = interface(IDispatch)
    ['{2EE48F22-AF3C-405F-8970-F71BE12EE9A2}']
    function Get_NotificationLevel: AutomaticUpdatesNotificationLevel; safecall;
    procedure Set_NotificationLevel1(retval: AutomaticUpdatesNotificationLevel); safecall;
    function Get_ReadOnly: WordBool; safecall;
    function Get_Required: WordBool; safecall;
    function Get_ScheduledInstallationDay: AutomaticUpdatesScheduledInstallationDay; safecall;
    procedure Set_ScheduledInstallationDay(retval: AutomaticUpdatesScheduledInstallationDay); safecall;
    function Get_ScheduledInstallationTime: Integer; safecall;
    procedure Set_ScheduledInstallationTime(retval: Integer); safecall;
    procedure Refresh; safecall;
    procedure Save; safecall;
    property NotificationLevel: AutomaticUpdatesNotificationLevel read Get_NotificationLevel write Set_NotificationLevel1;
    property ReadOnly: WordBool read Get_ReadOnly;
    property Required: WordBool read Get_Required;
    property ScheduledInstallationDay: AutomaticUpdatesScheduledInstallationDay read Get_ScheduledInstallationDay write Set_ScheduledInstallationDay;
    property ScheduledInstallationTime: Integer read Get_ScheduledInstallationTime write Set_ScheduledInstallationTime;
  end;

// *********************************************************************//
// DispIntf:  IAutomaticUpdatesSettingsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {2EE48F22-AF3C-405F-8970-F71BE12EE9A2}
// *********************************************************************//
  IAutomaticUpdatesSettingsDisp = dispinterface
    ['{2EE48F22-AF3C-405F-8970-F71BE12EE9A2}']
    property NotificationLevel: AutomaticUpdatesNotificationLevel dispid 1610743809;
    property ReadOnly: WordBool readonly dispid 1610743810;
    property Required: WordBool readonly dispid 1610743811;
    property ScheduledInstallationDay: AutomaticUpdatesScheduledInstallationDay dispid 1610743812;
    property ScheduledInstallationTime: Integer dispid 1610743813;
    procedure Refresh; dispid 1610743814;
    procedure Save; dispid 1610743815;
  end;

// *********************************************************************//
// Interface: IAutomaticUpdatesSettings2
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {6ABC136A-C3CA-4384-8171-CB2B1E59B8DC}
// *********************************************************************//
  IAutomaticUpdatesSettings2 = interface(IAutomaticUpdatesSettings)
    ['{6ABC136A-C3CA-4384-8171-CB2B1E59B8DC}']
    function Get_IncludeRecommendedUpdates: WordBool; safecall;
    procedure Set_IncludeRecommendedUpdates1(retval: WordBool); safecall;
    function CheckPermission(userType: AutomaticUpdatesUserType; 
                             permissionType: AutomaticUpdatesPermissionType): WordBool; safecall;
    property IncludeRecommendedUpdates: WordBool read Get_IncludeRecommendedUpdates write Set_IncludeRecommendedUpdates1;
  end;

// *********************************************************************//
// DispIntf:  IAutomaticUpdatesSettings2Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {6ABC136A-C3CA-4384-8171-CB2B1E59B8DC}
// *********************************************************************//
  IAutomaticUpdatesSettings2Disp = dispinterface
    ['{6ABC136A-C3CA-4384-8171-CB2B1E59B8DC}']
    property IncludeRecommendedUpdates: WordBool dispid 1610809345;
    function CheckPermission(userType: AutomaticUpdatesUserType; 
                             permissionType: AutomaticUpdatesPermissionType): WordBool; dispid 1610809346;
    property NotificationLevel: AutomaticUpdatesNotificationLevel dispid 1610743809;
    property ReadOnly: WordBool readonly dispid 1610743810;
    property Required: WordBool readonly dispid 1610743811;
    property ScheduledInstallationDay: AutomaticUpdatesScheduledInstallationDay dispid 1610743812;
    property ScheduledInstallationTime: Integer dispid 1610743813;
    procedure Refresh; dispid 1610743814;
    procedure Save; dispid 1610743815;
  end;

// *********************************************************************//
// Interface: IUpdate
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {6A92B07A-D821-4682-B423-5C805022CC4D}
// *********************************************************************//
  IUpdate = interface(IDispatch)
    ['{6A92B07A-D821-4682-B423-5C805022CC4D}']
    function Get_Title: WideString; safecall;
    function Get_AutoSelectOnWebSites: WordBool; safecall;
    function Get_BundledUpdates: IUpdateCollection; safecall;
    function Get_CanRequireSource: WordBool; safecall;
    function Get_Categories: ICategoryCollection; safecall;
    function Get_Deadline: OleVariant; safecall;
    function Get_DeltaCompressedContentAvailable: WordBool; safecall;
    function Get_DeltaCompressedContentPreferred: WordBool; safecall;
    function Get_Description: WideString; safecall;
    function Get_EulaAccepted: WordBool; safecall;
    function Get_EulaText: WideString; safecall;
    function Get_HandlerID: WideString; safecall;
    function Get_Identity: IUpdateIdentity; safecall;
    function Get_Image: IImageInformation; safecall;
    function Get_InstallationBehavior: IInstallationBehavior; safecall;
    function Get_IsBeta: WordBool; safecall;
    function Get_IsDownloaded: WordBool; safecall;
    function Get_IsHidden: WordBool; safecall;
    procedure Set_IsHidden(retval: WordBool); safecall;
    function Get_IsInstalled: WordBool; safecall;
    function Get_IsMandatory: WordBool; safecall;
    function Get_IsUninstallable: WordBool; safecall;
    function Get_Languages: IStringCollection; safecall;
    function Get_LastDeploymentChangeTime: TDateTime; safecall;
    function Get_MaxDownloadSize: TDecimal; safecall;
    function Get_MinDownloadSize: TDecimal; safecall;
    function Get_MoreInfoUrls: IStringCollection; safecall;
    function Get_MsrcSeverity: WideString; safecall;
    function Get_RecommendedCpuSpeed: Integer; safecall;
    function Get_RecommendedHardDiskSpace: Integer; safecall;
    function Get_RecommendedMemory: Integer; safecall;
    function Get_ReleaseNotes: WideString; safecall;
    function Get_SecurityBulletinIDs: IStringCollection; safecall;
    function Get_SupersededUpdateIDs: IStringCollection; safecall;
    function Get_SupportUrl: WideString; safecall;
    function Get_type_: UpdateType; safecall;
    function Get_UninstallationNotes: WideString; safecall;
    function Get_UninstallationBehavior: IInstallationBehavior; safecall;
    function Get_UninstallationSteps: IStringCollection; safecall;
    function Get_KBArticleIDs: IStringCollection; safecall;
    procedure AcceptEula; safecall;
    function Get_DeploymentAction: DeploymentAction; safecall;
    procedure CopyFromCache(const path: WideString; toExtractCabFiles: WordBool); safecall;
    function Get_DownloadPriority: DownloadPriority; safecall;
    function Get_DownloadContents: IUpdateDownloadContentCollection; safecall;
    property Title: WideString read Get_Title;
    property AutoSelectOnWebSites: WordBool read Get_AutoSelectOnWebSites;
    property BundledUpdates: IUpdateCollection read Get_BundledUpdates;
    property CanRequireSource: WordBool read Get_CanRequireSource;
    property Categories: ICategoryCollection read Get_Categories;
    property Deadline: OleVariant read Get_Deadline;
    property DeltaCompressedContentAvailable: WordBool read Get_DeltaCompressedContentAvailable;
    property DeltaCompressedContentPreferred: WordBool read Get_DeltaCompressedContentPreferred;
    property Description: WideString read Get_Description;
    property EulaAccepted: WordBool read Get_EulaAccepted;
    property EulaText: WideString read Get_EulaText;
    property HandlerID: WideString read Get_HandlerID;
    property Identity: IUpdateIdentity read Get_Identity;
    property Image: IImageInformation read Get_Image;
    property InstallationBehavior: IInstallationBehavior read Get_InstallationBehavior;
    property IsBeta: WordBool read Get_IsBeta;
    property IsDownloaded: WordBool read Get_IsDownloaded;
    property IsHidden: WordBool read Get_IsHidden write Set_IsHidden;
    property IsInstalled: WordBool read Get_IsInstalled;
    property IsMandatory: WordBool read Get_IsMandatory;
    property IsUninstallable: WordBool read Get_IsUninstallable;
    property Languages: IStringCollection read Get_Languages;
    property LastDeploymentChangeTime: TDateTime read Get_LastDeploymentChangeTime;
    property MaxDownloadSize: TDecimal read Get_MaxDownloadSize;
    property MinDownloadSize: TDecimal read Get_MinDownloadSize;
    property MoreInfoUrls: IStringCollection read Get_MoreInfoUrls;
    property MsrcSeverity: WideString read Get_MsrcSeverity;
    property RecommendedCpuSpeed: Integer read Get_RecommendedCpuSpeed;
    property RecommendedHardDiskSpace: Integer read Get_RecommendedHardDiskSpace;
    property RecommendedMemory: Integer read Get_RecommendedMemory;
    property ReleaseNotes: WideString read Get_ReleaseNotes;
    property SecurityBulletinIDs: IStringCollection read Get_SecurityBulletinIDs;
    property SupersededUpdateIDs: IStringCollection read Get_SupersededUpdateIDs;
    property SupportUrl: WideString read Get_SupportUrl;
    property type_: UpdateType read Get_type_;
    property UninstallationNotes: WideString read Get_UninstallationNotes;
    property UninstallationBehavior: IInstallationBehavior read Get_UninstallationBehavior;
    property UninstallationSteps: IStringCollection read Get_UninstallationSteps;
    property KBArticleIDs: IStringCollection read Get_KBArticleIDs;
    property DeploymentAction: DeploymentAction read Get_DeploymentAction;
    property DownloadPriority: DownloadPriority read Get_DownloadPriority;
    property DownloadContents: IUpdateDownloadContentCollection read Get_DownloadContents;
  end;

// *********************************************************************//
// DispIntf:  IUpdateDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {6A92B07A-D821-4682-B423-5C805022CC4D}
// *********************************************************************//
  IUpdateDisp = dispinterface
    ['{6A92B07A-D821-4682-B423-5C805022CC4D}']
    property Title: WideString readonly dispid 0;
    property AutoSelectOnWebSites: WordBool readonly dispid 1610743809;
    property BundledUpdates: IUpdateCollection readonly dispid 1610743810;
    property CanRequireSource: WordBool readonly dispid 1610743811;
    property Categories: ICategoryCollection readonly dispid 1610743812;
    property Deadline: OleVariant readonly dispid 1610743813;
    property DeltaCompressedContentAvailable: WordBool readonly dispid 1610743814;
    property DeltaCompressedContentPreferred: WordBool readonly dispid 1610743815;
    property Description: WideString readonly dispid 1610743816;
    property EulaAccepted: WordBool readonly dispid 1610743817;
    property EulaText: WideString readonly dispid 1610743818;
    property HandlerID: WideString readonly dispid 1610743819;
    property Identity: IUpdateIdentity readonly dispid 1610743820;
    property Image: IImageInformation readonly dispid 1610743821;
    property InstallationBehavior: IInstallationBehavior readonly dispid 1610743822;
    property IsBeta: WordBool readonly dispid 1610743823;
    property IsDownloaded: WordBool readonly dispid 1610743824;
    property IsHidden: WordBool dispid 1610743825;
    property IsInstalled: WordBool readonly dispid 1610743826;
    property IsMandatory: WordBool readonly dispid 1610743827;
    property IsUninstallable: WordBool readonly dispid 1610743828;
    property Languages: IStringCollection readonly dispid 1610743829;
    property LastDeploymentChangeTime: TDateTime readonly dispid 1610743830;
    property MaxDownloadSize: {??TDecimal}OleVariant readonly dispid 1610743831;
    property MinDownloadSize: {??TDecimal}OleVariant readonly dispid 1610743832;
    property MoreInfoUrls: IStringCollection readonly dispid 1610743833;
    property MsrcSeverity: WideString readonly dispid 1610743834;
    property RecommendedCpuSpeed: Integer readonly dispid 1610743835;
    property RecommendedHardDiskSpace: Integer readonly dispid 1610743836;
    property RecommendedMemory: Integer readonly dispid 1610743837;
    property ReleaseNotes: WideString readonly dispid 1610743838;
    property SecurityBulletinIDs: IStringCollection readonly dispid 1610743839;
    property SupersededUpdateIDs: IStringCollection readonly dispid 1610743841;
    property SupportUrl: WideString readonly dispid 1610743842;
    property type_: UpdateType readonly dispid 1610743843;
    property UninstallationNotes: WideString readonly dispid 1610743844;
    property UninstallationBehavior: IInstallationBehavior readonly dispid 1610743845;
    property UninstallationSteps: IStringCollection readonly dispid 1610743846;
    property KBArticleIDs: IStringCollection readonly dispid 1610743848;
    procedure AcceptEula; dispid 1610743847;
    property DeploymentAction: DeploymentAction readonly dispid 1610743849;
    procedure CopyFromCache(const path: WideString; toExtractCabFiles: WordBool); dispid 1610743850;
    property DownloadPriority: DownloadPriority readonly dispid 1610743851;
    property DownloadContents: IUpdateDownloadContentCollection readonly dispid 1610743852;
  end;

// *********************************************************************//
// Interface: IUpdateCollection
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {07F7438C-7709-4CA5-B518-91279288134E}
// *********************************************************************//
  IUpdateCollection = interface(IDispatch)
    ['{07F7438C-7709-4CA5-B518-91279288134E}']
    function Get_Item(index: Integer): IUpdate; safecall;
    procedure Set_Item(index: Integer; const retval: IUpdate); safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_Count: Integer; safecall;
    function Get_ReadOnly: WordBool; safecall;
    function Add(const value: IUpdate): Integer; safecall;
    procedure Clear; safecall;
    function Copy: IUpdateCollection; safecall;
    procedure Insert(index: Integer; const value: IUpdate); safecall;
    procedure RemoveAt(index: Integer); safecall;
    property Item[index: Integer]: IUpdate read Get_Item write Set_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
    property ReadOnly: WordBool read Get_ReadOnly;
  end;

// *********************************************************************//
// DispIntf:  IUpdateCollectionDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {07F7438C-7709-4CA5-B518-91279288134E}
// *********************************************************************//
  IUpdateCollectionDisp = dispinterface
    ['{07F7438C-7709-4CA5-B518-91279288134E}']
    property Item[index: Integer]: IUpdate dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
    property Count: Integer readonly dispid 1610743809;
    property ReadOnly: WordBool readonly dispid 1610743810;
    function Add(const value: IUpdate): Integer; dispid 1610743811;
    procedure Clear; dispid 1610743812;
    function Copy: IUpdateCollection; dispid 1610743813;
    procedure Insert(index: Integer; const value: IUpdate); dispid 1610743814;
    procedure RemoveAt(index: Integer); dispid 1610743815;
  end;

// *********************************************************************//
// Interface: ICategoryCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {3A56BFB8-576C-43F7-9335-FE4838FD7E37}
// *********************************************************************//
  ICategoryCollection = interface(IDispatch)
    ['{3A56BFB8-576C-43F7-9335-FE4838FD7E37}']
    function Get_Item(index: Integer): ICategory; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_Count: Integer; safecall;
    property Item[index: Integer]: ICategory read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  ICategoryCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {3A56BFB8-576C-43F7-9335-FE4838FD7E37}
// *********************************************************************//
  ICategoryCollectionDisp = dispinterface
    ['{3A56BFB8-576C-43F7-9335-FE4838FD7E37}']
    property Item[index: Integer]: ICategory readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
    property Count: Integer readonly dispid 1610743809;
  end;

// *********************************************************************//
// Interface: ICategory
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {81DDC1B8-9D35-47A6-B471-5B80F519223B}
// *********************************************************************//
  ICategory = interface(IDispatch)
    ['{81DDC1B8-9D35-47A6-B471-5B80F519223B}']
    function Get_Name: WideString; safecall;
    function Get_CategoryID: WideString; safecall;
    function Get_Children: ICategoryCollection; safecall;
    function Get_Description: WideString; safecall;
    function Get_Image: IImageInformation; safecall;
    function Get_Order: Integer; safecall;
    function Get_Parent: ICategory; safecall;
    function Get_type_: WideString; safecall;
    function Get_Updates: IUpdateCollection; safecall;
    property Name: WideString read Get_Name;
    property CategoryID: WideString read Get_CategoryID;
    property Children: ICategoryCollection read Get_Children;
    property Description: WideString read Get_Description;
    property Image: IImageInformation read Get_Image;
    property Order: Integer read Get_Order;
    property Parent: ICategory read Get_Parent;
    property type_: WideString read Get_type_;
    property Updates: IUpdateCollection read Get_Updates;
  end;

// *********************************************************************//
// DispIntf:  ICategoryDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {81DDC1B8-9D35-47A6-B471-5B80F519223B}
// *********************************************************************//
  ICategoryDisp = dispinterface
    ['{81DDC1B8-9D35-47A6-B471-5B80F519223B}']
    property Name: WideString readonly dispid 0;
    property CategoryID: WideString readonly dispid 1610743809;
    property Children: ICategoryCollection readonly dispid 1610743810;
    property Description: WideString readonly dispid 1610743811;
    property Image: IImageInformation readonly dispid 1610743812;
    property Order: Integer readonly dispid 1610743813;
    property Parent: ICategory readonly dispid 1610743814;
    property type_: WideString readonly dispid 1610743815;
    property Updates: IUpdateCollection readonly dispid 1610743816;
  end;

// *********************************************************************//
// Interface: IImageInformation
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {7C907864-346C-4AEB-8F3F-57DA289F969F}
// *********************************************************************//
  IImageInformation = interface(IDispatch)
    ['{7C907864-346C-4AEB-8F3F-57DA289F969F}']
    function Get_AltText: WideString; safecall;
    function Get_Height: Integer; safecall;
    function Get_Source: WideString; safecall;
    function Get_Width: Integer; safecall;
    property AltText: WideString read Get_AltText;
    property Height: Integer read Get_Height;
    property Source: WideString read Get_Source;
    property Width: Integer read Get_Width;
  end;

// *********************************************************************//
// DispIntf:  IImageInformationDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {7C907864-346C-4AEB-8F3F-57DA289F969F}
// *********************************************************************//
  IImageInformationDisp = dispinterface
    ['{7C907864-346C-4AEB-8F3F-57DA289F969F}']
    property AltText: WideString readonly dispid 1610743809;
    property Height: Integer readonly dispid 1610743810;
    property Source: WideString readonly dispid 1610743811;
    property Width: Integer readonly dispid 1610743812;
  end;

// *********************************************************************//
// Interface: IUpdateIdentity
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {46297823-9940-4C09-AED9-CD3EA6D05968}
// *********************************************************************//
  IUpdateIdentity = interface(IDispatch)
    ['{46297823-9940-4C09-AED9-CD3EA6D05968}']
    function Get_RevisionNumber: Integer; safecall;
    function Get_UpdateID: WideString; safecall;
    property RevisionNumber: Integer read Get_RevisionNumber;
    property UpdateID: WideString read Get_UpdateID;
  end;

// *********************************************************************//
// DispIntf:  IUpdateIdentityDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {46297823-9940-4C09-AED9-CD3EA6D05968}
// *********************************************************************//
  IUpdateIdentityDisp = dispinterface
    ['{46297823-9940-4C09-AED9-CD3EA6D05968}']
    property RevisionNumber: Integer readonly dispid 1610743810;
    property UpdateID: WideString readonly dispid 1610743811;
  end;

// *********************************************************************//
// Interface: IInstallationBehavior
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D9A59339-E245-4DBD-9686-4D5763E39624}
// *********************************************************************//
  IInstallationBehavior = interface(IDispatch)
    ['{D9A59339-E245-4DBD-9686-4D5763E39624}']
    function Get_CanRequestUserInput: WordBool; safecall;
    function Get_Impact: InstallationImpact; safecall;
    function Get_RebootBehavior: InstallationRebootBehavior; safecall;
    function Get_RequiresNetworkConnectivity: WordBool; safecall;
    property CanRequestUserInput: WordBool read Get_CanRequestUserInput;
    property Impact: InstallationImpact read Get_Impact;
    property RebootBehavior: InstallationRebootBehavior read Get_RebootBehavior;
    property RequiresNetworkConnectivity: WordBool read Get_RequiresNetworkConnectivity;
  end;

// *********************************************************************//
// DispIntf:  IInstallationBehaviorDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D9A59339-E245-4DBD-9686-4D5763E39624}
// *********************************************************************//
  IInstallationBehaviorDisp = dispinterface
    ['{D9A59339-E245-4DBD-9686-4D5763E39624}']
    property CanRequestUserInput: WordBool readonly dispid 1610743809;
    property Impact: InstallationImpact readonly dispid 1610743810;
    property RebootBehavior: InstallationRebootBehavior readonly dispid 1610743811;
    property RequiresNetworkConnectivity: WordBool readonly dispid 1610743812;
  end;

// *********************************************************************//
// Interface: IStringCollection
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {EFF90582-2DDC-480F-A06D-60F3FBC362C3}
// *********************************************************************//
  IStringCollection = interface(IDispatch)
    ['{EFF90582-2DDC-480F-A06D-60F3FBC362C3}']
    function Get_Item(index: Integer): WideString; safecall;
    procedure Set_Item(index: Integer; const retval: WideString); safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_Count: Integer; safecall;
    function Get_ReadOnly: WordBool; safecall;
    function Add(const value: WideString): Integer; safecall;
    procedure Clear; safecall;
    function Copy: IStringCollection; safecall;
    procedure Insert(index: Integer; const value: WideString); safecall;
    procedure RemoveAt(index: Integer); safecall;
    property Item[index: Integer]: WideString read Get_Item write Set_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
    property ReadOnly: WordBool read Get_ReadOnly;
  end;

// *********************************************************************//
// DispIntf:  IStringCollectionDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {EFF90582-2DDC-480F-A06D-60F3FBC362C3}
// *********************************************************************//
  IStringCollectionDisp = dispinterface
    ['{EFF90582-2DDC-480F-A06D-60F3FBC362C3}']
    property Item[index: Integer]: WideString dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
    property Count: Integer readonly dispid 1610743809;
    property ReadOnly: WordBool readonly dispid 1610743810;
    function Add(const value: WideString): Integer; dispid 1610743811;
    procedure Clear; dispid 1610743812;
    function Copy: IStringCollection; dispid 1610743813;
    procedure Insert(index: Integer; const value: WideString); dispid 1610743814;
    procedure RemoveAt(index: Integer); dispid 1610743815;
  end;

// *********************************************************************//
// Interface: IUpdateDownloadContentCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {BC5513C8-B3B8-4BF7-A4D4-361C0D8C88BA}
// *********************************************************************//
  IUpdateDownloadContentCollection = interface(IDispatch)
    ['{BC5513C8-B3B8-4BF7-A4D4-361C0D8C88BA}']
    function Get_Item(index: Integer): IUpdateDownloadContent; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_Count: Integer; safecall;
    property Item[index: Integer]: IUpdateDownloadContent read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IUpdateDownloadContentCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {BC5513C8-B3B8-4BF7-A4D4-361C0D8C88BA}
// *********************************************************************//
  IUpdateDownloadContentCollectionDisp = dispinterface
    ['{BC5513C8-B3B8-4BF7-A4D4-361C0D8C88BA}']
    property Item[index: Integer]: IUpdateDownloadContent readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
    property Count: Integer readonly dispid 1610743809;
  end;

// *********************************************************************//
// Interface: IUpdateDownloadContent
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {54A2CB2D-9A0C-48B6-8A50-9ABB69EE2D02}
// *********************************************************************//
  IUpdateDownloadContent = interface(IDispatch)
    ['{54A2CB2D-9A0C-48B6-8A50-9ABB69EE2D02}']
    function Get_DownloadUrl: WideString; safecall;
    property DownloadUrl: WideString read Get_DownloadUrl;
  end;

// *********************************************************************//
// DispIntf:  IUpdateDownloadContentDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {54A2CB2D-9A0C-48B6-8A50-9ABB69EE2D02}
// *********************************************************************//
  IUpdateDownloadContentDisp = dispinterface
    ['{54A2CB2D-9A0C-48B6-8A50-9ABB69EE2D02}']
    property DownloadUrl: WideString readonly dispid 1610743809;
  end;

// *********************************************************************//
// Interface: IUpdate2
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {144FE9B0-D23D-4A8B-8634-FB4457533B7A}
// *********************************************************************//
  IUpdate2 = interface(IUpdate)
    ['{144FE9B0-D23D-4A8B-8634-FB4457533B7A}']
    function Get_RebootRequired: WordBool; safecall;
    function Get_IsPresent: WordBool; safecall;
    function Get_CveIDs: IStringCollection; safecall;
    procedure CopyToCache(const pFiles: IStringCollection); safecall;
    property RebootRequired: WordBool read Get_RebootRequired;
    property IsPresent: WordBool read Get_IsPresent;
    property CveIDs: IStringCollection read Get_CveIDs;
  end;

// *********************************************************************//
// DispIntf:  IUpdate2Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {144FE9B0-D23D-4A8B-8634-FB4457533B7A}
// *********************************************************************//
  IUpdate2Disp = dispinterface
    ['{144FE9B0-D23D-4A8B-8634-FB4457533B7A}']
    property RebootRequired: WordBool readonly dispid 1610809345;
    property IsPresent: WordBool readonly dispid 1610809347;
    property CveIDs: IStringCollection readonly dispid 1610809348;
    procedure CopyToCache(const pFiles: IStringCollection); dispid 1610809346;
    property Title: WideString readonly dispid 0;
    property AutoSelectOnWebSites: WordBool readonly dispid 1610743809;
    property BundledUpdates: IUpdateCollection readonly dispid 1610743810;
    property CanRequireSource: WordBool readonly dispid 1610743811;
    property Categories: ICategoryCollection readonly dispid 1610743812;
    property Deadline: OleVariant readonly dispid 1610743813;
    property DeltaCompressedContentAvailable: WordBool readonly dispid 1610743814;
    property DeltaCompressedContentPreferred: WordBool readonly dispid 1610743815;
    property Description: WideString readonly dispid 1610743816;
    property EulaAccepted: WordBool readonly dispid 1610743817;
    property EulaText: WideString readonly dispid 1610743818;
    property HandlerID: WideString readonly dispid 1610743819;
    property Identity: IUpdateIdentity readonly dispid 1610743820;
    property Image: IImageInformation readonly dispid 1610743821;
    property InstallationBehavior: IInstallationBehavior readonly dispid 1610743822;
    property IsBeta: WordBool readonly dispid 1610743823;
    property IsDownloaded: WordBool readonly dispid 1610743824;
    property IsHidden: WordBool dispid 1610743825;
    property IsInstalled: WordBool readonly dispid 1610743826;
    property IsMandatory: WordBool readonly dispid 1610743827;
    property IsUninstallable: WordBool readonly dispid 1610743828;
    property Languages: IStringCollection readonly dispid 1610743829;
    property LastDeploymentChangeTime: TDateTime readonly dispid 1610743830;
    property MaxDownloadSize: {??TDecimal}OleVariant readonly dispid 1610743831;
    property MinDownloadSize: {??TDecimal}OleVariant readonly dispid 1610743832;
    property MoreInfoUrls: IStringCollection readonly dispid 1610743833;
    property MsrcSeverity: WideString readonly dispid 1610743834;
    property RecommendedCpuSpeed: Integer readonly dispid 1610743835;
    property RecommendedHardDiskSpace: Integer readonly dispid 1610743836;
    property RecommendedMemory: Integer readonly dispid 1610743837;
    property ReleaseNotes: WideString readonly dispid 1610743838;
    property SecurityBulletinIDs: IStringCollection readonly dispid 1610743839;
    property SupersededUpdateIDs: IStringCollection readonly dispid 1610743841;
    property SupportUrl: WideString readonly dispid 1610743842;
    property type_: UpdateType readonly dispid 1610743843;
    property UninstallationNotes: WideString readonly dispid 1610743844;
    property UninstallationBehavior: IInstallationBehavior readonly dispid 1610743845;
    property UninstallationSteps: IStringCollection readonly dispid 1610743846;
    property KBArticleIDs: IStringCollection readonly dispid 1610743848;
    procedure AcceptEula; dispid 1610743847;
    property DeploymentAction: DeploymentAction readonly dispid 1610743849;
    procedure CopyFromCache(const path: WideString; toExtractCabFiles: WordBool); dispid 1610743850;
    property DownloadPriority: DownloadPriority readonly dispid 1610743851;
    property DownloadContents: IUpdateDownloadContentCollection readonly dispid 1610743852;
  end;

// *********************************************************************//
// Interface: IUpdate3
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {112EDA6B-95B3-476F-9D90-AEE82C6B8181}
// *********************************************************************//
  IUpdate3 = interface(IUpdate2)
    ['{112EDA6B-95B3-476F-9D90-AEE82C6B8181}']
    function Get_BrowseOnly: WordBool; safecall;
    property BrowseOnly: WordBool read Get_BrowseOnly;
  end;

// *********************************************************************//
// DispIntf:  IUpdate3Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {112EDA6B-95B3-476F-9D90-AEE82C6B8181}
// *********************************************************************//
  IUpdate3Disp = dispinterface
    ['{112EDA6B-95B3-476F-9D90-AEE82C6B8181}']
    property BrowseOnly: WordBool readonly dispid 1610874881;
    property RebootRequired: WordBool readonly dispid 1610809345;
    property IsPresent: WordBool readonly dispid 1610809347;
    property CveIDs: IStringCollection readonly dispid 1610809348;
    procedure CopyToCache(const pFiles: IStringCollection); dispid 1610809346;
    property Title: WideString readonly dispid 0;
    property AutoSelectOnWebSites: WordBool readonly dispid 1610743809;
    property BundledUpdates: IUpdateCollection readonly dispid 1610743810;
    property CanRequireSource: WordBool readonly dispid 1610743811;
    property Categories: ICategoryCollection readonly dispid 1610743812;
    property Deadline: OleVariant readonly dispid 1610743813;
    property DeltaCompressedContentAvailable: WordBool readonly dispid 1610743814;
    property DeltaCompressedContentPreferred: WordBool readonly dispid 1610743815;
    property Description: WideString readonly dispid 1610743816;
    property EulaAccepted: WordBool readonly dispid 1610743817;
    property EulaText: WideString readonly dispid 1610743818;
    property HandlerID: WideString readonly dispid 1610743819;
    property Identity: IUpdateIdentity readonly dispid 1610743820;
    property Image: IImageInformation readonly dispid 1610743821;
    property InstallationBehavior: IInstallationBehavior readonly dispid 1610743822;
    property IsBeta: WordBool readonly dispid 1610743823;
    property IsDownloaded: WordBool readonly dispid 1610743824;
    property IsHidden: WordBool dispid 1610743825;
    property IsInstalled: WordBool readonly dispid 1610743826;
    property IsMandatory: WordBool readonly dispid 1610743827;
    property IsUninstallable: WordBool readonly dispid 1610743828;
    property Languages: IStringCollection readonly dispid 1610743829;
    property LastDeploymentChangeTime: TDateTime readonly dispid 1610743830;
    property MaxDownloadSize: {??TDecimal}OleVariant readonly dispid 1610743831;
    property MinDownloadSize: {??TDecimal}OleVariant readonly dispid 1610743832;
    property MoreInfoUrls: IStringCollection readonly dispid 1610743833;
    property MsrcSeverity: WideString readonly dispid 1610743834;
    property RecommendedCpuSpeed: Integer readonly dispid 1610743835;
    property RecommendedHardDiskSpace: Integer readonly dispid 1610743836;
    property RecommendedMemory: Integer readonly dispid 1610743837;
    property ReleaseNotes: WideString readonly dispid 1610743838;
    property SecurityBulletinIDs: IStringCollection readonly dispid 1610743839;
    property SupersededUpdateIDs: IStringCollection readonly dispid 1610743841;
    property SupportUrl: WideString readonly dispid 1610743842;
    property type_: UpdateType readonly dispid 1610743843;
    property UninstallationNotes: WideString readonly dispid 1610743844;
    property UninstallationBehavior: IInstallationBehavior readonly dispid 1610743845;
    property UninstallationSteps: IStringCollection readonly dispid 1610743846;
    property KBArticleIDs: IStringCollection readonly dispid 1610743848;
    procedure AcceptEula; dispid 1610743847;
    property DeploymentAction: DeploymentAction readonly dispid 1610743849;
    procedure CopyFromCache(const path: WideString; toExtractCabFiles: WordBool); dispid 1610743850;
    property DownloadPriority: DownloadPriority readonly dispid 1610743851;
    property DownloadContents: IUpdateDownloadContentCollection readonly dispid 1610743852;
  end;

// *********************************************************************//
// Interface: IWindowsDriverUpdate
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B383CD1A-5CE9-4504-9F63-764B1236F191}
// *********************************************************************//
  IWindowsDriverUpdate = interface(IUpdate)
    ['{B383CD1A-5CE9-4504-9F63-764B1236F191}']
    function Get_DriverClass: WideString; safecall;
    function Get_DriverHardwareID: WideString; safecall;
    function Get_DriverManufacturer: WideString; safecall;
    function Get_DriverModel: WideString; safecall;
    function Get_DriverProvider: WideString; safecall;
    function Get_DriverVerDate: TDateTime; safecall;
    function Get_DeviceProblemNumber: Integer; safecall;
    function Get_DeviceStatus: Integer; safecall;
    property DriverClass: WideString read Get_DriverClass;
    property DriverHardwareID: WideString read Get_DriverHardwareID;
    property DriverManufacturer: WideString read Get_DriverManufacturer;
    property DriverModel: WideString read Get_DriverModel;
    property DriverProvider: WideString read Get_DriverProvider;
    property DriverVerDate: TDateTime read Get_DriverVerDate;
    property DeviceProblemNumber: Integer read Get_DeviceProblemNumber;
    property DeviceStatus: Integer read Get_DeviceStatus;
  end;

// *********************************************************************//
// DispIntf:  IWindowsDriverUpdateDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B383CD1A-5CE9-4504-9F63-764B1236F191}
// *********************************************************************//
  IWindowsDriverUpdateDisp = dispinterface
    ['{B383CD1A-5CE9-4504-9F63-764B1236F191}']
    property DriverClass: WideString readonly dispid 1610809345;
    property DriverHardwareID: WideString readonly dispid 1610809346;
    property DriverManufacturer: WideString readonly dispid 1610809347;
    property DriverModel: WideString readonly dispid 1610809348;
    property DriverProvider: WideString readonly dispid 1610809349;
    property DriverVerDate: TDateTime readonly dispid 1610809350;
    property DeviceProblemNumber: Integer readonly dispid 1610809351;
    property DeviceStatus: Integer readonly dispid 1610809352;
    property Title: WideString readonly dispid 0;
    property AutoSelectOnWebSites: WordBool readonly dispid 1610743809;
    property BundledUpdates: IUpdateCollection readonly dispid 1610743810;
    property CanRequireSource: WordBool readonly dispid 1610743811;
    property Categories: ICategoryCollection readonly dispid 1610743812;
    property Deadline: OleVariant readonly dispid 1610743813;
    property DeltaCompressedContentAvailable: WordBool readonly dispid 1610743814;
    property DeltaCompressedContentPreferred: WordBool readonly dispid 1610743815;
    property Description: WideString readonly dispid 1610743816;
    property EulaAccepted: WordBool readonly dispid 1610743817;
    property EulaText: WideString readonly dispid 1610743818;
    property HandlerID: WideString readonly dispid 1610743819;
    property Identity: IUpdateIdentity readonly dispid 1610743820;
    property Image: IImageInformation readonly dispid 1610743821;
    property InstallationBehavior: IInstallationBehavior readonly dispid 1610743822;
    property IsBeta: WordBool readonly dispid 1610743823;
    property IsDownloaded: WordBool readonly dispid 1610743824;
    property IsHidden: WordBool dispid 1610743825;
    property IsInstalled: WordBool readonly dispid 1610743826;
    property IsMandatory: WordBool readonly dispid 1610743827;
    property IsUninstallable: WordBool readonly dispid 1610743828;
    property Languages: IStringCollection readonly dispid 1610743829;
    property LastDeploymentChangeTime: TDateTime readonly dispid 1610743830;
    property MaxDownloadSize: {??TDecimal}OleVariant readonly dispid 1610743831;
    property MinDownloadSize: {??TDecimal}OleVariant readonly dispid 1610743832;
    property MoreInfoUrls: IStringCollection readonly dispid 1610743833;
    property MsrcSeverity: WideString readonly dispid 1610743834;
    property RecommendedCpuSpeed: Integer readonly dispid 1610743835;
    property RecommendedHardDiskSpace: Integer readonly dispid 1610743836;
    property RecommendedMemory: Integer readonly dispid 1610743837;
    property ReleaseNotes: WideString readonly dispid 1610743838;
    property SecurityBulletinIDs: IStringCollection readonly dispid 1610743839;
    property SupersededUpdateIDs: IStringCollection readonly dispid 1610743841;
    property SupportUrl: WideString readonly dispid 1610743842;
    property type_: UpdateType readonly dispid 1610743843;
    property UninstallationNotes: WideString readonly dispid 1610743844;
    property UninstallationBehavior: IInstallationBehavior readonly dispid 1610743845;
    property UninstallationSteps: IStringCollection readonly dispid 1610743846;
    property KBArticleIDs: IStringCollection readonly dispid 1610743848;
    procedure AcceptEula; dispid 1610743847;
    property DeploymentAction: DeploymentAction readonly dispid 1610743849;
    procedure CopyFromCache(const path: WideString; toExtractCabFiles: WordBool); dispid 1610743850;
    property DownloadPriority: DownloadPriority readonly dispid 1610743851;
    property DownloadContents: IUpdateDownloadContentCollection readonly dispid 1610743852;
  end;

// *********************************************************************//
// Interface: IWindowsDriverUpdate2
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {615C4269-7A48-43BD-96B7-BF6CA27D6C3E}
// *********************************************************************//
  IWindowsDriverUpdate2 = interface(IWindowsDriverUpdate)
    ['{615C4269-7A48-43BD-96B7-BF6CA27D6C3E}']
    function Get_RebootRequired: WordBool; safecall;
    function Get_IsPresent: WordBool; safecall;
    function Get_CveIDs: IStringCollection; safecall;
    procedure CopyToCache(const pFiles: IStringCollection); safecall;
    property RebootRequired: WordBool read Get_RebootRequired;
    property IsPresent: WordBool read Get_IsPresent;
    property CveIDs: IStringCollection read Get_CveIDs;
  end;

// *********************************************************************//
// DispIntf:  IWindowsDriverUpdate2Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {615C4269-7A48-43BD-96B7-BF6CA27D6C3E}
// *********************************************************************//
  IWindowsDriverUpdate2Disp = dispinterface
    ['{615C4269-7A48-43BD-96B7-BF6CA27D6C3E}']
    property RebootRequired: WordBool readonly dispid 1610874881;
    property IsPresent: WordBool readonly dispid 1610874883;
    property CveIDs: IStringCollection readonly dispid 1610874884;
    procedure CopyToCache(const pFiles: IStringCollection); dispid 1610874882;
    property DriverClass: WideString readonly dispid 1610809345;
    property DriverHardwareID: WideString readonly dispid 1610809346;
    property DriverManufacturer: WideString readonly dispid 1610809347;
    property DriverModel: WideString readonly dispid 1610809348;
    property DriverProvider: WideString readonly dispid 1610809349;
    property DriverVerDate: TDateTime readonly dispid 1610809350;
    property DeviceProblemNumber: Integer readonly dispid 1610809351;
    property DeviceStatus: Integer readonly dispid 1610809352;
    property Title: WideString readonly dispid 0;
    property AutoSelectOnWebSites: WordBool readonly dispid 1610743809;
    property BundledUpdates: IUpdateCollection readonly dispid 1610743810;
    property CanRequireSource: WordBool readonly dispid 1610743811;
    property Categories: ICategoryCollection readonly dispid 1610743812;
    property Deadline: OleVariant readonly dispid 1610743813;
    property DeltaCompressedContentAvailable: WordBool readonly dispid 1610743814;
    property DeltaCompressedContentPreferred: WordBool readonly dispid 1610743815;
    property Description: WideString readonly dispid 1610743816;
    property EulaAccepted: WordBool readonly dispid 1610743817;
    property EulaText: WideString readonly dispid 1610743818;
    property HandlerID: WideString readonly dispid 1610743819;
    property Identity: IUpdateIdentity readonly dispid 1610743820;
    property Image: IImageInformation readonly dispid 1610743821;
    property InstallationBehavior: IInstallationBehavior readonly dispid 1610743822;
    property IsBeta: WordBool readonly dispid 1610743823;
    property IsDownloaded: WordBool readonly dispid 1610743824;
    property IsHidden: WordBool dispid 1610743825;
    property IsInstalled: WordBool readonly dispid 1610743826;
    property IsMandatory: WordBool readonly dispid 1610743827;
    property IsUninstallable: WordBool readonly dispid 1610743828;
    property Languages: IStringCollection readonly dispid 1610743829;
    property LastDeploymentChangeTime: TDateTime readonly dispid 1610743830;
    property MaxDownloadSize: {??TDecimal}OleVariant readonly dispid 1610743831;
    property MinDownloadSize: {??TDecimal}OleVariant readonly dispid 1610743832;
    property MoreInfoUrls: IStringCollection readonly dispid 1610743833;
    property MsrcSeverity: WideString readonly dispid 1610743834;
    property RecommendedCpuSpeed: Integer readonly dispid 1610743835;
    property RecommendedHardDiskSpace: Integer readonly dispid 1610743836;
    property RecommendedMemory: Integer readonly dispid 1610743837;
    property ReleaseNotes: WideString readonly dispid 1610743838;
    property SecurityBulletinIDs: IStringCollection readonly dispid 1610743839;
    property SupersededUpdateIDs: IStringCollection readonly dispid 1610743841;
    property SupportUrl: WideString readonly dispid 1610743842;
    property type_: UpdateType readonly dispid 1610743843;
    property UninstallationNotes: WideString readonly dispid 1610743844;
    property UninstallationBehavior: IInstallationBehavior readonly dispid 1610743845;
    property UninstallationSteps: IStringCollection readonly dispid 1610743846;
    property KBArticleIDs: IStringCollection readonly dispid 1610743848;
    procedure AcceptEula; dispid 1610743847;
    property DeploymentAction: DeploymentAction readonly dispid 1610743849;
    procedure CopyFromCache(const path: WideString; toExtractCabFiles: WordBool); dispid 1610743850;
    property DownloadPriority: DownloadPriority readonly dispid 1610743851;
    property DownloadContents: IUpdateDownloadContentCollection readonly dispid 1610743852;
  end;

// *********************************************************************//
// Interface: IWindowsDriverUpdate3
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {49EBD502-4A96-41BD-9E3E-4C5057F4250C}
// *********************************************************************//
  IWindowsDriverUpdate3 = interface(IWindowsDriverUpdate2)
    ['{49EBD502-4A96-41BD-9E3E-4C5057F4250C}']
    function Get_BrowseOnly: WordBool; safecall;
    property BrowseOnly: WordBool read Get_BrowseOnly;
  end;

// *********************************************************************//
// DispIntf:  IWindowsDriverUpdate3Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {49EBD502-4A96-41BD-9E3E-4C5057F4250C}
// *********************************************************************//
  IWindowsDriverUpdate3Disp = dispinterface
    ['{49EBD502-4A96-41BD-9E3E-4C5057F4250C}']
    property BrowseOnly: WordBool readonly dispid 1610940417;
    property RebootRequired: WordBool readonly dispid 1610874881;
    property IsPresent: WordBool readonly dispid 1610874883;
    property CveIDs: IStringCollection readonly dispid 1610874884;
    procedure CopyToCache(const pFiles: IStringCollection); dispid 1610874882;
    property DriverClass: WideString readonly dispid 1610809345;
    property DriverHardwareID: WideString readonly dispid 1610809346;
    property DriverManufacturer: WideString readonly dispid 1610809347;
    property DriverModel: WideString readonly dispid 1610809348;
    property DriverProvider: WideString readonly dispid 1610809349;
    property DriverVerDate: TDateTime readonly dispid 1610809350;
    property DeviceProblemNumber: Integer readonly dispid 1610809351;
    property DeviceStatus: Integer readonly dispid 1610809352;
    property Title: WideString readonly dispid 0;
    property AutoSelectOnWebSites: WordBool readonly dispid 1610743809;
    property BundledUpdates: IUpdateCollection readonly dispid 1610743810;
    property CanRequireSource: WordBool readonly dispid 1610743811;
    property Categories: ICategoryCollection readonly dispid 1610743812;
    property Deadline: OleVariant readonly dispid 1610743813;
    property DeltaCompressedContentAvailable: WordBool readonly dispid 1610743814;
    property DeltaCompressedContentPreferred: WordBool readonly dispid 1610743815;
    property Description: WideString readonly dispid 1610743816;
    property EulaAccepted: WordBool readonly dispid 1610743817;
    property EulaText: WideString readonly dispid 1610743818;
    property HandlerID: WideString readonly dispid 1610743819;
    property Identity: IUpdateIdentity readonly dispid 1610743820;
    property Image: IImageInformation readonly dispid 1610743821;
    property InstallationBehavior: IInstallationBehavior readonly dispid 1610743822;
    property IsBeta: WordBool readonly dispid 1610743823;
    property IsDownloaded: WordBool readonly dispid 1610743824;
    property IsHidden: WordBool dispid 1610743825;
    property IsInstalled: WordBool readonly dispid 1610743826;
    property IsMandatory: WordBool readonly dispid 1610743827;
    property IsUninstallable: WordBool readonly dispid 1610743828;
    property Languages: IStringCollection readonly dispid 1610743829;
    property LastDeploymentChangeTime: TDateTime readonly dispid 1610743830;
    property MaxDownloadSize: {??TDecimal}OleVariant readonly dispid 1610743831;
    property MinDownloadSize: {??TDecimal}OleVariant readonly dispid 1610743832;
    property MoreInfoUrls: IStringCollection readonly dispid 1610743833;
    property MsrcSeverity: WideString readonly dispid 1610743834;
    property RecommendedCpuSpeed: Integer readonly dispid 1610743835;
    property RecommendedHardDiskSpace: Integer readonly dispid 1610743836;
    property RecommendedMemory: Integer readonly dispid 1610743837;
    property ReleaseNotes: WideString readonly dispid 1610743838;
    property SecurityBulletinIDs: IStringCollection readonly dispid 1610743839;
    property SupersededUpdateIDs: IStringCollection readonly dispid 1610743841;
    property SupportUrl: WideString readonly dispid 1610743842;
    property type_: UpdateType readonly dispid 1610743843;
    property UninstallationNotes: WideString readonly dispid 1610743844;
    property UninstallationBehavior: IInstallationBehavior readonly dispid 1610743845;
    property UninstallationSteps: IStringCollection readonly dispid 1610743846;
    property KBArticleIDs: IStringCollection readonly dispid 1610743848;
    procedure AcceptEula; dispid 1610743847;
    property DeploymentAction: DeploymentAction readonly dispid 1610743849;
    procedure CopyFromCache(const path: WideString; toExtractCabFiles: WordBool); dispid 1610743850;
    property DownloadPriority: DownloadPriority readonly dispid 1610743851;
    property DownloadContents: IUpdateDownloadContentCollection readonly dispid 1610743852;
  end;

// *********************************************************************//
// Interface: ISearchCompletedCallback
// Flags:     (384) NonExtensible OleAutomation
// GUID:      {88AEE058-D4B0-4725-A2F1-814A67AE964C}
// *********************************************************************//
  ISearchCompletedCallback = interface(IUnknown)
    ['{88AEE058-D4B0-4725-A2F1-814A67AE964C}']
    function Invoke(const searchJob: ISearchJob; const callbackArgs: ISearchCompletedCallbackArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISearchJob
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {7366EA16-7A1A-4EA2-B042-973D3E9CD99B}
// *********************************************************************//
  ISearchJob = interface(IDispatch)
    ['{7366EA16-7A1A-4EA2-B042-973D3E9CD99B}']
    function Get_AsyncState: OleVariant; safecall;
    function Get_IsCompleted: WordBool; safecall;
    procedure CleanUp; safecall;
    procedure RequestAbort; safecall;
    property AsyncState: OleVariant read Get_AsyncState;
    property IsCompleted: WordBool read Get_IsCompleted;
  end;

// *********************************************************************//
// DispIntf:  ISearchJobDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {7366EA16-7A1A-4EA2-B042-973D3E9CD99B}
// *********************************************************************//
  ISearchJobDisp = dispinterface
    ['{7366EA16-7A1A-4EA2-B042-973D3E9CD99B}']
    property AsyncState: OleVariant readonly dispid 1610743809;
    property IsCompleted: WordBool readonly dispid 1610743810;
    procedure CleanUp; dispid 1610743811;
    procedure RequestAbort; dispid 1610743812;
  end;

// *********************************************************************//
// Interface: ISearchCompletedCallbackArgs
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A700A634-2850-4C47-938A-9E4B6E5AF9A6}
// *********************************************************************//
  ISearchCompletedCallbackArgs = interface(IDispatch)
    ['{A700A634-2850-4C47-938A-9E4B6E5AF9A6}']
  end;

// *********************************************************************//
// DispIntf:  ISearchCompletedCallbackArgsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A700A634-2850-4C47-938A-9E4B6E5AF9A6}
// *********************************************************************//
  ISearchCompletedCallbackArgsDisp = dispinterface
    ['{A700A634-2850-4C47-938A-9E4B6E5AF9A6}']
  end;

// *********************************************************************//
// Interface: IDownloadCompletedCallback
// Flags:     (384) NonExtensible OleAutomation
// GUID:      {77254866-9F5B-4C8E-B9E2-C77A8530D64B}
// *********************************************************************//
  IDownloadCompletedCallback = interface(IUnknown)
    ['{77254866-9F5B-4C8E-B9E2-C77A8530D64B}']
    function Invoke(const downloadJob: IDownloadJob; 
                    const callbackArgs: IDownloadCompletedCallbackArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDownloadJob
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C574DE85-7358-43F6-AAE8-8697E62D8BA7}
// *********************************************************************//
  IDownloadJob = interface(IDispatch)
    ['{C574DE85-7358-43F6-AAE8-8697E62D8BA7}']
    function Get_AsyncState: OleVariant; safecall;
    function Get_IsCompleted: WordBool; safecall;
    function Get_Updates: IUpdateCollection; safecall;
    procedure CleanUp; safecall;
    function GetProgress: IDownloadProgress; safecall;
    procedure RequestAbort; safecall;
    property AsyncState: OleVariant read Get_AsyncState;
    property IsCompleted: WordBool read Get_IsCompleted;
    property Updates: IUpdateCollection read Get_Updates;
  end;

// *********************************************************************//
// DispIntf:  IDownloadJobDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C574DE85-7358-43F6-AAE8-8697E62D8BA7}
// *********************************************************************//
  IDownloadJobDisp = dispinterface
    ['{C574DE85-7358-43F6-AAE8-8697E62D8BA7}']
    property AsyncState: OleVariant readonly dispid 1610743809;
    property IsCompleted: WordBool readonly dispid 1610743810;
    property Updates: IUpdateCollection readonly dispid 1610743811;
    procedure CleanUp; dispid 1610743812;
    function GetProgress: IDownloadProgress; dispid 1610743813;
    procedure RequestAbort; dispid 1610743814;
  end;

// *********************************************************************//
// Interface: IDownloadProgress
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D31A5BAC-F719-4178-9DBB-5E2CB47FD18A}
// *********************************************************************//
  IDownloadProgress = interface(IDispatch)
    ['{D31A5BAC-F719-4178-9DBB-5E2CB47FD18A}']
    function Get_CurrentUpdateBytesDownloaded: TDecimal; safecall;
    function Get_CurrentUpdateBytesToDownload: TDecimal; safecall;
    function Get_CurrentUpdateIndex: Integer; safecall;
    function Get_PercentComplete: Integer; safecall;
    function Get_TotalBytesDownloaded: TDecimal; safecall;
    function Get_TotalBytesToDownload: TDecimal; safecall;
    function GetUpdateResult(updateIndex: Integer): IUpdateDownloadResult; safecall;
    function Get_CurrentUpdateDownloadPhase: DownloadPhase; safecall;
    function Get_CurrentUpdatePercentComplete: Integer; safecall;
    property CurrentUpdateBytesDownloaded: TDecimal read Get_CurrentUpdateBytesDownloaded;
    property CurrentUpdateBytesToDownload: TDecimal read Get_CurrentUpdateBytesToDownload;
    property CurrentUpdateIndex: Integer read Get_CurrentUpdateIndex;
    property PercentComplete: Integer read Get_PercentComplete;
    property TotalBytesDownloaded: TDecimal read Get_TotalBytesDownloaded;
    property TotalBytesToDownload: TDecimal read Get_TotalBytesToDownload;
    property CurrentUpdateDownloadPhase: DownloadPhase read Get_CurrentUpdateDownloadPhase;
    property CurrentUpdatePercentComplete: Integer read Get_CurrentUpdatePercentComplete;
  end;

// *********************************************************************//
// DispIntf:  IDownloadProgressDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D31A5BAC-F719-4178-9DBB-5E2CB47FD18A}
// *********************************************************************//
  IDownloadProgressDisp = dispinterface
    ['{D31A5BAC-F719-4178-9DBB-5E2CB47FD18A}']
    property CurrentUpdateBytesDownloaded: {??TDecimal}OleVariant readonly dispid 1610743809;
    property CurrentUpdateBytesToDownload: {??TDecimal}OleVariant readonly dispid 1610743810;
    property CurrentUpdateIndex: Integer readonly dispid 1610743811;
    property PercentComplete: Integer readonly dispid 1610743812;
    property TotalBytesDownloaded: {??TDecimal}OleVariant readonly dispid 1610743813;
    property TotalBytesToDownload: {??TDecimal}OleVariant readonly dispid 1610743814;
    function GetUpdateResult(updateIndex: Integer): IUpdateDownloadResult; dispid 1610743815;
    property CurrentUpdateDownloadPhase: DownloadPhase readonly dispid 1610743816;
    property CurrentUpdatePercentComplete: Integer readonly dispid 1610743817;
  end;

// *********************************************************************//
// Interface: IUpdateDownloadResult
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {BF99AF76-B575-42AD-8AA4-33CBB5477AF1}
// *********************************************************************//
  IUpdateDownloadResult = interface(IDispatch)
    ['{BF99AF76-B575-42AD-8AA4-33CBB5477AF1}']
    function Get_HResult: Integer; safecall;
    function Get_ResultCode: OperationResultCode; safecall;
    property HResult: Integer read Get_HResult;
    property ResultCode: OperationResultCode read Get_ResultCode;
  end;

// *********************************************************************//
// DispIntf:  IUpdateDownloadResultDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {BF99AF76-B575-42AD-8AA4-33CBB5477AF1}
// *********************************************************************//
  IUpdateDownloadResultDisp = dispinterface
    ['{BF99AF76-B575-42AD-8AA4-33CBB5477AF1}']
    property HResult: Integer readonly dispid 1610743809;
    property ResultCode: OperationResultCode readonly dispid 1610743810;
  end;

// *********************************************************************//
// Interface: IDownloadCompletedCallbackArgs
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {FA565B23-498C-47A0-979D-E7D5B1813360}
// *********************************************************************//
  IDownloadCompletedCallbackArgs = interface(IDispatch)
    ['{FA565B23-498C-47A0-979D-E7D5B1813360}']
  end;

// *********************************************************************//
// DispIntf:  IDownloadCompletedCallbackArgsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {FA565B23-498C-47A0-979D-E7D5B1813360}
// *********************************************************************//
  IDownloadCompletedCallbackArgsDisp = dispinterface
    ['{FA565B23-498C-47A0-979D-E7D5B1813360}']
  end;

// *********************************************************************//
// Interface: IDownloadProgressChangedCallback
// Flags:     (384) NonExtensible OleAutomation
// GUID:      {8C3F1CDD-6173-4591-AEBD-A56A53CA77C1}
// *********************************************************************//
  IDownloadProgressChangedCallback = interface(IUnknown)
    ['{8C3F1CDD-6173-4591-AEBD-A56A53CA77C1}']
    function Invoke(const downloadJob: IDownloadJob; 
                    const callbackArgs: IDownloadProgressChangedCallbackArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDownloadProgressChangedCallbackArgs
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {324FF2C6-4981-4B04-9412-57481745AB24}
// *********************************************************************//
  IDownloadProgressChangedCallbackArgs = interface(IDispatch)
    ['{324FF2C6-4981-4B04-9412-57481745AB24}']
    function Get_Progress: IDownloadProgress; safecall;
    property Progress: IDownloadProgress read Get_Progress;
  end;

// *********************************************************************//
// DispIntf:  IDownloadProgressChangedCallbackArgsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {324FF2C6-4981-4B04-9412-57481745AB24}
// *********************************************************************//
  IDownloadProgressChangedCallbackArgsDisp = dispinterface
    ['{324FF2C6-4981-4B04-9412-57481745AB24}']
    property Progress: IDownloadProgress readonly dispid 1610743809;
  end;

// *********************************************************************//
// Interface: IInstallationCompletedCallback
// Flags:     (384) NonExtensible OleAutomation
// GUID:      {45F4F6F3-D602-4F98-9A8A-3EFA152AD2D3}
// *********************************************************************//
  IInstallationCompletedCallback = interface(IUnknown)
    ['{45F4F6F3-D602-4F98-9A8A-3EFA152AD2D3}']
    function Invoke(const installationJob: IInstallationJob; 
                    const callbackArgs: IInstallationCompletedCallbackArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IInstallationJob
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {5C209F0B-BAD5-432A-9556-4699BED2638A}
// *********************************************************************//
  IInstallationJob = interface(IDispatch)
    ['{5C209F0B-BAD5-432A-9556-4699BED2638A}']
    function Get_AsyncState: OleVariant; safecall;
    function Get_IsCompleted: WordBool; safecall;
    function Get_Updates: IUpdateCollection; safecall;
    procedure CleanUp; safecall;
    function GetProgress: IInstallationProgress; safecall;
    procedure RequestAbort; safecall;
    property AsyncState: OleVariant read Get_AsyncState;
    property IsCompleted: WordBool read Get_IsCompleted;
    property Updates: IUpdateCollection read Get_Updates;
  end;

// *********************************************************************//
// DispIntf:  IInstallationJobDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {5C209F0B-BAD5-432A-9556-4699BED2638A}
// *********************************************************************//
  IInstallationJobDisp = dispinterface
    ['{5C209F0B-BAD5-432A-9556-4699BED2638A}']
    property AsyncState: OleVariant readonly dispid 1610743809;
    property IsCompleted: WordBool readonly dispid 1610743810;
    property Updates: IUpdateCollection readonly dispid 1610743811;
    procedure CleanUp; dispid 1610743812;
    function GetProgress: IInstallationProgress; dispid 1610743813;
    procedure RequestAbort; dispid 1610743814;
  end;

// *********************************************************************//
// Interface: IInstallationProgress
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {345C8244-43A3-4E32-A368-65F073B76F36}
// *********************************************************************//
  IInstallationProgress = interface(IDispatch)
    ['{345C8244-43A3-4E32-A368-65F073B76F36}']
    function Get_CurrentUpdateIndex: Integer; safecall;
    function Get_CurrentUpdatePercentComplete: Integer; safecall;
    function Get_PercentComplete: Integer; safecall;
    function GetUpdateResult(updateIndex: Integer): IUpdateInstallationResult; safecall;
    property CurrentUpdateIndex: Integer read Get_CurrentUpdateIndex;
    property CurrentUpdatePercentComplete: Integer read Get_CurrentUpdatePercentComplete;
    property PercentComplete: Integer read Get_PercentComplete;
  end;

// *********************************************************************//
// DispIntf:  IInstallationProgressDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {345C8244-43A3-4E32-A368-65F073B76F36}
// *********************************************************************//
  IInstallationProgressDisp = dispinterface
    ['{345C8244-43A3-4E32-A368-65F073B76F36}']
    property CurrentUpdateIndex: Integer readonly dispid 1610743809;
    property CurrentUpdatePercentComplete: Integer readonly dispid 1610743810;
    property PercentComplete: Integer readonly dispid 1610743811;
    function GetUpdateResult(updateIndex: Integer): IUpdateInstallationResult; dispid 1610743812;
  end;

// *********************************************************************//
// Interface: IUpdateInstallationResult
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D940F0F8-3CBB-4FD0-993F-471E7F2328AD}
// *********************************************************************//
  IUpdateInstallationResult = interface(IDispatch)
    ['{D940F0F8-3CBB-4FD0-993F-471E7F2328AD}']
    function Get_HResult: Integer; safecall;
    function Get_RebootRequired: WordBool; safecall;
    function Get_ResultCode: OperationResultCode; safecall;
    property HResult: Integer read Get_HResult;
    property RebootRequired: WordBool read Get_RebootRequired;
    property ResultCode: OperationResultCode read Get_ResultCode;
  end;

// *********************************************************************//
// DispIntf:  IUpdateInstallationResultDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D940F0F8-3CBB-4FD0-993F-471E7F2328AD}
// *********************************************************************//
  IUpdateInstallationResultDisp = dispinterface
    ['{D940F0F8-3CBB-4FD0-993F-471E7F2328AD}']
    property HResult: Integer readonly dispid 1610743809;
    property RebootRequired: WordBool readonly dispid 1610743810;
    property ResultCode: OperationResultCode readonly dispid 1610743811;
  end;

// *********************************************************************//
// Interface: IInstallationCompletedCallbackArgs
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {250E2106-8EFB-4705-9653-EF13C581B6A1}
// *********************************************************************//
  IInstallationCompletedCallbackArgs = interface(IDispatch)
    ['{250E2106-8EFB-4705-9653-EF13C581B6A1}']
  end;

// *********************************************************************//
// DispIntf:  IInstallationCompletedCallbackArgsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {250E2106-8EFB-4705-9653-EF13C581B6A1}
// *********************************************************************//
  IInstallationCompletedCallbackArgsDisp = dispinterface
    ['{250E2106-8EFB-4705-9653-EF13C581B6A1}']
  end;

// *********************************************************************//
// Interface: IInstallationProgressChangedCallback
// Flags:     (384) NonExtensible OleAutomation
// GUID:      {E01402D5-F8DA-43BA-A012-38894BD048F1}
// *********************************************************************//
  IInstallationProgressChangedCallback = interface(IUnknown)
    ['{E01402D5-F8DA-43BA-A012-38894BD048F1}']
    function Invoke(const installationJob: IInstallationJob; 
                    const callbackArgs: IInstallationProgressChangedCallbackArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IInstallationProgressChangedCallbackArgs
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {E4F14E1E-689D-4218-A0B9-BC189C484A01}
// *********************************************************************//
  IInstallationProgressChangedCallbackArgs = interface(IDispatch)
    ['{E4F14E1E-689D-4218-A0B9-BC189C484A01}']
    function Get_Progress: IInstallationProgress; safecall;
    property Progress: IInstallationProgress read Get_Progress;
  end;

// *********************************************************************//
// DispIntf:  IInstallationProgressChangedCallbackArgsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {E4F14E1E-689D-4218-A0B9-BC189C484A01}
// *********************************************************************//
  IInstallationProgressChangedCallbackArgsDisp = dispinterface
    ['{E4F14E1E-689D-4218-A0B9-BC189C484A01}']
    property Progress: IInstallationProgress readonly dispid 1610743809;
  end;

// *********************************************************************//
// Interface: IUpdateHistoryEntry
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {BE56A644-AF0E-4E0E-A311-C1D8E695CBFF}
// *********************************************************************//
  IUpdateHistoryEntry = interface(IDispatch)
    ['{BE56A644-AF0E-4E0E-A311-C1D8E695CBFF}']
    function Get_Operation: UpdateOperation; safecall;
    function Get_ResultCode: OperationResultCode; safecall;
    function Get_HResult: Integer; safecall;
    function Get_Date: TDateTime; safecall;
    function Get_UpdateIdentity: IUpdateIdentity; safecall;
    function Get_Title: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_UnmappedResultCode: Integer; safecall;
    function Get_ClientApplicationID: WideString; safecall;
    function Get_ServerSelection: ServerSelection; safecall;
    function Get_ServiceID: WideString; safecall;
    function Get_UninstallationSteps: IStringCollection; safecall;
    function Get_UninstallationNotes: WideString; safecall;
    function Get_SupportUrl: WideString; safecall;
    property Operation: UpdateOperation read Get_Operation;
    property ResultCode: OperationResultCode read Get_ResultCode;
    property HResult: Integer read Get_HResult;
    property Date: TDateTime read Get_Date;
    property UpdateIdentity: IUpdateIdentity read Get_UpdateIdentity;
    property Title: WideString read Get_Title;
    property Description: WideString read Get_Description;
    property UnmappedResultCode: Integer read Get_UnmappedResultCode;
    property ClientApplicationID: WideString read Get_ClientApplicationID;
    property ServerSelection: ServerSelection read Get_ServerSelection;
    property ServiceID: WideString read Get_ServiceID;
    property UninstallationSteps: IStringCollection read Get_UninstallationSteps;
    property UninstallationNotes: WideString read Get_UninstallationNotes;
    property SupportUrl: WideString read Get_SupportUrl;
  end;

// *********************************************************************//
// DispIntf:  IUpdateHistoryEntryDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {BE56A644-AF0E-4E0E-A311-C1D8E695CBFF}
// *********************************************************************//
  IUpdateHistoryEntryDisp = dispinterface
    ['{BE56A644-AF0E-4E0E-A311-C1D8E695CBFF}']
    property Operation: UpdateOperation readonly dispid 1610743809;
    property ResultCode: OperationResultCode readonly dispid 1610743810;
    property HResult: Integer readonly dispid 1610743811;
    property Date: TDateTime readonly dispid 1610743812;
    property UpdateIdentity: IUpdateIdentity readonly dispid 1610743813;
    property Title: WideString readonly dispid 1610743814;
    property Description: WideString readonly dispid 1610743815;
    property UnmappedResultCode: Integer readonly dispid 1610743816;
    property ClientApplicationID: WideString readonly dispid 1610743817;
    property ServerSelection: ServerSelection readonly dispid 1610743818;
    property ServiceID: WideString readonly dispid 1610743819;
    property UninstallationSteps: IStringCollection readonly dispid 1610743820;
    property UninstallationNotes: WideString readonly dispid 1610743821;
    property SupportUrl: WideString readonly dispid 1610743822;
  end;

// *********************************************************************//
// Interface: IUpdateHistoryEntry2
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C2BFB780-4539-4132-AB8C-0A8772013AB6}
// *********************************************************************//
  IUpdateHistoryEntry2 = interface(IUpdateHistoryEntry)
    ['{C2BFB780-4539-4132-AB8C-0A8772013AB6}']
    function Get_Categories: ICategoryCollection; safecall;
    property Categories: ICategoryCollection read Get_Categories;
  end;

// *********************************************************************//
// DispIntf:  IUpdateHistoryEntry2Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C2BFB780-4539-4132-AB8C-0A8772013AB6}
// *********************************************************************//
  IUpdateHistoryEntry2Disp = dispinterface
    ['{C2BFB780-4539-4132-AB8C-0A8772013AB6}']
    property Categories: ICategoryCollection readonly dispid 1610809345;
    property Operation: UpdateOperation readonly dispid 1610743809;
    property ResultCode: OperationResultCode readonly dispid 1610743810;
    property HResult: Integer readonly dispid 1610743811;
    property Date: TDateTime readonly dispid 1610743812;
    property UpdateIdentity: IUpdateIdentity readonly dispid 1610743813;
    property Title: WideString readonly dispid 1610743814;
    property Description: WideString readonly dispid 1610743815;
    property UnmappedResultCode: Integer readonly dispid 1610743816;
    property ClientApplicationID: WideString readonly dispid 1610743817;
    property ServerSelection: ServerSelection readonly dispid 1610743818;
    property ServiceID: WideString readonly dispid 1610743819;
    property UninstallationSteps: IStringCollection readonly dispid 1610743820;
    property UninstallationNotes: WideString readonly dispid 1610743821;
    property SupportUrl: WideString readonly dispid 1610743822;
  end;

// *********************************************************************//
// Interface: IUpdateDownloadContent2
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C97AD11B-F257-420B-9D9F-377F733F6F68}
// *********************************************************************//
  IUpdateDownloadContent2 = interface(IUpdateDownloadContent)
    ['{C97AD11B-F257-420B-9D9F-377F733F6F68}']
    function Get_IsDeltaCompressedContent: WordBool; safecall;
    property IsDeltaCompressedContent: WordBool read Get_IsDeltaCompressedContent;
  end;

// *********************************************************************//
// DispIntf:  IUpdateDownloadContent2Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C97AD11B-F257-420B-9D9F-377F733F6F68}
// *********************************************************************//
  IUpdateDownloadContent2Disp = dispinterface
    ['{C97AD11B-F257-420B-9D9F-377F733F6F68}']
    property IsDeltaCompressedContent: WordBool readonly dispid 1610809345;
    property DownloadUrl: WideString readonly dispid 1610743809;
  end;

// *********************************************************************//
// Interface: IUpdateSearcher
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8F45ABF1-F9AE-4B95-A933-F0F66E5056EA}
// *********************************************************************//
  IUpdateSearcher = interface(IDispatch)
    ['{8F45ABF1-F9AE-4B95-A933-F0F66E5056EA}']
    function Get_CanAutomaticallyUpgradeService: WordBool; safecall;
    procedure Set_CanAutomaticallyUpgradeService1(retval: WordBool); safecall;
    function Get_ClientApplicationID: WideString; safecall;
    procedure Set_ClientApplicationID1(const retval: WideString); safecall;
    function Get_IncludePotentiallySupersededUpdates: WordBool; safecall;
    procedure Set_IncludePotentiallySupersededUpdates(retval: WordBool); safecall;
    function Get_ServerSelection: ServerSelection; safecall;
    procedure Set_ServerSelection1(retval: ServerSelection); safecall;
    function BeginSearch(const criteria: WideString; const onCompleted: IUnknown; state: OleVariant): ISearchJob; safecall;
    function EndSearch(const searchJob: ISearchJob): ISearchResult; safecall;
    function EscapeString(const unescaped: WideString): WideString; safecall;
    function QueryHistory(startIndex: Integer; Count: Integer): IUpdateHistoryEntryCollection; safecall;
    function Search(const criteria: WideString): ISearchResult; safecall;
    function Get_Online: WordBool; safecall;
    procedure Set_Online(retval: WordBool); safecall;
    function GetTotalHistoryCount: Integer; safecall;
    function Get_ServiceID: WideString; safecall;
    procedure Set_ServiceID(const retval: WideString); safecall;
    property CanAutomaticallyUpgradeService: WordBool read Get_CanAutomaticallyUpgradeService write Set_CanAutomaticallyUpgradeService1;
    property ClientApplicationID: WideString read Get_ClientApplicationID write Set_ClientApplicationID1;
    property IncludePotentiallySupersededUpdates: WordBool read Get_IncludePotentiallySupersededUpdates write Set_IncludePotentiallySupersededUpdates;
    property ServerSelection: ServerSelection read Get_ServerSelection write Set_ServerSelection1;
    property Online: WordBool read Get_Online write Set_Online;
    property ServiceID: WideString read Get_ServiceID write Set_ServiceID;
  end;

// *********************************************************************//
// DispIntf:  IUpdateSearcherDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8F45ABF1-F9AE-4B95-A933-F0F66E5056EA}
// *********************************************************************//
  IUpdateSearcherDisp = dispinterface
    ['{8F45ABF1-F9AE-4B95-A933-F0F66E5056EA}']
    property CanAutomaticallyUpgradeService: WordBool dispid 1610743809;
    property ClientApplicationID: WideString dispid 1610743811;
    property IncludePotentiallySupersededUpdates: WordBool dispid 1610743812;
    property ServerSelection: ServerSelection dispid 1610743815;
    function BeginSearch(const criteria: WideString; const onCompleted: IUnknown; state: OleVariant): ISearchJob; dispid 1610743816;
    function EndSearch(const searchJob: ISearchJob): ISearchResult; dispid 1610743817;
    function EscapeString(const unescaped: WideString): WideString; dispid 1610743818;
    function QueryHistory(startIndex: Integer; Count: Integer): IUpdateHistoryEntryCollection; dispid 1610743819;
    function Search(const criteria: WideString): ISearchResult; dispid 1610743820;
    property Online: WordBool dispid 1610743821;
    function GetTotalHistoryCount: Integer; dispid 1610743822;
    property ServiceID: WideString dispid 1610743823;
  end;

// *********************************************************************//
// Interface: IUpdateSearcher2
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4CBDCB2D-1589-4BEB-BD1C-3E582FF0ADD0}
// *********************************************************************//
  IUpdateSearcher2 = interface(IUpdateSearcher)
    ['{4CBDCB2D-1589-4BEB-BD1C-3E582FF0ADD0}']
    function Get_IgnoreDownloadPriority: WordBool; safecall;
    procedure Set_IgnoreDownloadPriority(retval: WordBool); safecall;
    property IgnoreDownloadPriority: WordBool read Get_IgnoreDownloadPriority write Set_IgnoreDownloadPriority;
  end;

// *********************************************************************//
// DispIntf:  IUpdateSearcher2Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4CBDCB2D-1589-4BEB-BD1C-3E582FF0ADD0}
// *********************************************************************//
  IUpdateSearcher2Disp = dispinterface
    ['{4CBDCB2D-1589-4BEB-BD1C-3E582FF0ADD0}']
    property IgnoreDownloadPriority: WordBool dispid 1610809345;
    property CanAutomaticallyUpgradeService: WordBool dispid 1610743809;
    property ClientApplicationID: WideString dispid 1610743811;
    property IncludePotentiallySupersededUpdates: WordBool dispid 1610743812;
    property ServerSelection: ServerSelection dispid 1610743815;
    function BeginSearch(const criteria: WideString; const onCompleted: IUnknown; state: OleVariant): ISearchJob; dispid 1610743816;
    function EndSearch(const searchJob: ISearchJob): ISearchResult; dispid 1610743817;
    function EscapeString(const unescaped: WideString): WideString; dispid 1610743818;
    function QueryHistory(startIndex: Integer; Count: Integer): IUpdateHistoryEntryCollection; dispid 1610743819;
    function Search(const criteria: WideString): ISearchResult; dispid 1610743820;
    property Online: WordBool dispid 1610743821;
    function GetTotalHistoryCount: Integer; dispid 1610743822;
    property ServiceID: WideString dispid 1610743823;
  end;

// *********************************************************************//
// Interface: ISearchResult
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D40CFF62-E08C-4498-941A-01E25F0FD33C}
// *********************************************************************//
  ISearchResult = interface(IDispatch)
    ['{D40CFF62-E08C-4498-941A-01E25F0FD33C}']
    function Get_ResultCode: OperationResultCode; safecall;
    function Get_RootCategories: ICategoryCollection; safecall;
    function Get_Updates: IUpdateCollection; safecall;
    function Get_Warnings: IUpdateExceptionCollection; safecall;
    property ResultCode: OperationResultCode read Get_ResultCode;
    property RootCategories: ICategoryCollection read Get_RootCategories;
    property Updates: IUpdateCollection read Get_Updates;
    property Warnings: IUpdateExceptionCollection read Get_Warnings;
  end;

// *********************************************************************//
// DispIntf:  ISearchResultDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D40CFF62-E08C-4498-941A-01E25F0FD33C}
// *********************************************************************//
  ISearchResultDisp = dispinterface
    ['{D40CFF62-E08C-4498-941A-01E25F0FD33C}']
    property ResultCode: OperationResultCode readonly dispid 1610743809;
    property RootCategories: ICategoryCollection readonly dispid 1610743810;
    property Updates: IUpdateCollection readonly dispid 1610743811;
    property Warnings: IUpdateExceptionCollection readonly dispid 1610743812;
  end;

// *********************************************************************//
// Interface: IUpdateExceptionCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {503626A3-8E14-4729-9355-0FE664BD2321}
// *********************************************************************//
  IUpdateExceptionCollection = interface(IDispatch)
    ['{503626A3-8E14-4729-9355-0FE664BD2321}']
    function Get_Item(index: Integer): IUpdateException; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_Count: Integer; safecall;
    property Item[index: Integer]: IUpdateException read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IUpdateExceptionCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {503626A3-8E14-4729-9355-0FE664BD2321}
// *********************************************************************//
  IUpdateExceptionCollectionDisp = dispinterface
    ['{503626A3-8E14-4729-9355-0FE664BD2321}']
    property Item[index: Integer]: IUpdateException readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
    property Count: Integer readonly dispid 1610743809;
  end;

// *********************************************************************//
// Interface: IUpdateHistoryEntryCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A7F04F3C-A290-435B-AADF-A116C3357A5C}
// *********************************************************************//
  IUpdateHistoryEntryCollection = interface(IDispatch)
    ['{A7F04F3C-A290-435B-AADF-A116C3357A5C}']
    function Get_Item(index: Integer): IUpdateHistoryEntry; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_Count: Integer; safecall;
    property Item[index: Integer]: IUpdateHistoryEntry read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IUpdateHistoryEntryCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A7F04F3C-A290-435B-AADF-A116C3357A5C}
// *********************************************************************//
  IUpdateHistoryEntryCollectionDisp = dispinterface
    ['{A7F04F3C-A290-435B-AADF-A116C3357A5C}']
    property Item[index: Integer]: IUpdateHistoryEntry readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
    property Count: Integer readonly dispid 1610743809;
  end;

// *********************************************************************//
// Interface: IWebProxy
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {174C81FE-AECD-4DAE-B8A0-2C6318DD86A8}
// *********************************************************************//
  IWebProxy = interface(IDispatch)
    ['{174C81FE-AECD-4DAE-B8A0-2C6318DD86A8}']
    function Get_Address: WideString; safecall;
    procedure Set_Address1(const retval: WideString); safecall;
    function Get_BypassList: IStringCollection; safecall;
    procedure Set_BypassList(const retval: IStringCollection); safecall;
    function Get_BypassProxyOnLocal: WordBool; safecall;
    procedure Set_BypassProxyOnLocal(retval: WordBool); safecall;
    function Get_ReadOnly: WordBool; safecall;
    function Get_UserName: WideString; safecall;
    procedure Set_UserName(const retval: WideString); safecall;
    procedure SetPassword(const value: WideString); safecall;
    procedure PromptForCredentials(const parentWindow: IUnknown; const Title: WideString); safecall;
    procedure PromptForCredentialsFromHwnd(var parentWindow: _RemotableHandle; 
                                           const Title: WideString); safecall;
    function Get_AutoDetect: WordBool; safecall;
    procedure Set_AutoDetect(retval: WordBool); safecall;
    property Address: WideString read Get_Address write Set_Address1;
    property BypassList: IStringCollection read Get_BypassList write Set_BypassList;
    property BypassProxyOnLocal: WordBool read Get_BypassProxyOnLocal write Set_BypassProxyOnLocal;
    property ReadOnly: WordBool read Get_ReadOnly;
    property UserName: WideString read Get_UserName write Set_UserName;
    property AutoDetect: WordBool read Get_AutoDetect write Set_AutoDetect;
  end;

// *********************************************************************//
// DispIntf:  IWebProxyDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {174C81FE-AECD-4DAE-B8A0-2C6318DD86A8}
// *********************************************************************//
  IWebProxyDisp = dispinterface
    ['{174C81FE-AECD-4DAE-B8A0-2C6318DD86A8}']
    property Address: WideString dispid 1610743809;
    property BypassList: IStringCollection dispid 1610743810;
    property BypassProxyOnLocal: WordBool dispid 1610743811;
    property ReadOnly: WordBool readonly dispid 1610743812;
    property UserName: WideString dispid 1610743813;
    procedure SetPassword(const value: WideString); dispid 1610743814;
    procedure PromptForCredentials(const parentWindow: IUnknown; const Title: WideString); dispid 1610743815;
    procedure PromptForCredentialsFromHwnd(var parentWindow: {??_RemotableHandle}OleVariant; 
                                           const Title: WideString); dispid 1610743816;
    property AutoDetect: WordBool dispid 1610743817;
  end;

// *********************************************************************//
// Interface: ISystemInformation
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {ADE87BF7-7B56-4275-8FAB-B9B0E591844B}
// *********************************************************************//
  ISystemInformation = interface(IDispatch)
    ['{ADE87BF7-7B56-4275-8FAB-B9B0E591844B}']
    function Get_OemHardwareSupportLink: WideString; safecall;
    function Get_RebootRequired: WordBool; safecall;
    property OemHardwareSupportLink: WideString read Get_OemHardwareSupportLink;
    property RebootRequired: WordBool read Get_RebootRequired;
  end;

// *********************************************************************//
// DispIntf:  ISystemInformationDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {ADE87BF7-7B56-4275-8FAB-B9B0E591844B}
// *********************************************************************//
  ISystemInformationDisp = dispinterface
    ['{ADE87BF7-7B56-4275-8FAB-B9B0E591844B}']
    property OemHardwareSupportLink: WideString readonly dispid 1610743809;
    property RebootRequired: WordBool readonly dispid 1610743810;
  end;

// *********************************************************************//
// Interface: IWindowsUpdateAgentInfo
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {85713FA1-7796-4FA2-BE3B-E2D6124DD373}
// *********************************************************************//
  IWindowsUpdateAgentInfo = interface(IDispatch)
    ['{85713FA1-7796-4FA2-BE3B-E2D6124DD373}']
    function GetInfo(varInfoIdentifier: OleVariant): OleVariant; safecall;
  end;

// *********************************************************************//
// DispIntf:  IWindowsUpdateAgentInfoDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {85713FA1-7796-4FA2-BE3B-E2D6124DD373}
// *********************************************************************//
  IWindowsUpdateAgentInfoDisp = dispinterface
    ['{85713FA1-7796-4FA2-BE3B-E2D6124DD373}']
    function GetInfo(varInfoIdentifier: OleVariant): OleVariant; dispid 1610743809;
  end;

// *********************************************************************//
// Interface: IAutomaticUpdates
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {673425BF-C082-4C7C-BDFD-569464B8E0CE}
// *********************************************************************//
  IAutomaticUpdates = interface(IDispatch)
    ['{673425BF-C082-4C7C-BDFD-569464B8E0CE}']
    procedure DetectNow; safecall;
    procedure Pause; safecall;
    procedure Resume; safecall;
    procedure ShowSettingsDialog; safecall;
    function Get_Settings: IAutomaticUpdatesSettings; safecall;
    function Get_ServiceEnabled: WordBool; safecall;
    procedure EnableService; safecall;
    property Settings: IAutomaticUpdatesSettings read Get_Settings;
    property ServiceEnabled: WordBool read Get_ServiceEnabled;
  end;

// *********************************************************************//
// DispIntf:  IAutomaticUpdatesDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {673425BF-C082-4C7C-BDFD-569464B8E0CE}
// *********************************************************************//
  IAutomaticUpdatesDisp = dispinterface
    ['{673425BF-C082-4C7C-BDFD-569464B8E0CE}']
    procedure DetectNow; dispid 1610743809;
    procedure Pause; dispid 1610743810;
    procedure Resume; dispid 1610743811;
    procedure ShowSettingsDialog; dispid 1610743812;
    property Settings: IAutomaticUpdatesSettings readonly dispid 1610743813;
    property ServiceEnabled: WordBool readonly dispid 1610743814;
    procedure EnableService; dispid 1610743815;
  end;

// *********************************************************************//
// Interface: IAutomaticUpdates2
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4A2F5C31-CFD9-410E-B7FB-29A653973A0F}
// *********************************************************************//
  IAutomaticUpdates2 = interface(IAutomaticUpdates)
    ['{4A2F5C31-CFD9-410E-B7FB-29A653973A0F}']
    function Get_Results: IAutomaticUpdatesResults; safecall;
    property Results: IAutomaticUpdatesResults read Get_Results;
  end;

// *********************************************************************//
// DispIntf:  IAutomaticUpdates2Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4A2F5C31-CFD9-410E-B7FB-29A653973A0F}
// *********************************************************************//
  IAutomaticUpdates2Disp = dispinterface
    ['{4A2F5C31-CFD9-410E-B7FB-29A653973A0F}']
    property Results: IAutomaticUpdatesResults readonly dispid 1610809345;
    procedure DetectNow; dispid 1610743809;
    procedure Pause; dispid 1610743810;
    procedure Resume; dispid 1610743811;
    procedure ShowSettingsDialog; dispid 1610743812;
    property Settings: IAutomaticUpdatesSettings readonly dispid 1610743813;
    property ServiceEnabled: WordBool readonly dispid 1610743814;
    procedure EnableService; dispid 1610743815;
  end;

// *********************************************************************//
// Interface: IAutomaticUpdatesResults
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {E7A4D634-7942-4DD9-A111-82228BA33901}
// *********************************************************************//
  IAutomaticUpdatesResults = interface(IDispatch)
    ['{E7A4D634-7942-4DD9-A111-82228BA33901}']
    function Get_LastSearchSuccessDate: OleVariant; safecall;
    function Get_LastInstallationSuccessDate: OleVariant; safecall;
    property LastSearchSuccessDate: OleVariant read Get_LastSearchSuccessDate;
    property LastInstallationSuccessDate: OleVariant read Get_LastInstallationSuccessDate;
  end;

// *********************************************************************//
// DispIntf:  IAutomaticUpdatesResultsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {E7A4D634-7942-4DD9-A111-82228BA33901}
// *********************************************************************//
  IAutomaticUpdatesResultsDisp = dispinterface
    ['{E7A4D634-7942-4DD9-A111-82228BA33901}']
    property LastSearchSuccessDate: OleVariant readonly dispid 1610743809;
    property LastInstallationSuccessDate: OleVariant readonly dispid 1610743810;
  end;

// *********************************************************************//
// Interface: IUpdateDownloader
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {68F1C6F9-7ECC-4666-A464-247FE12496C3}
// *********************************************************************//
  IUpdateDownloader = interface(IDispatch)
    ['{68F1C6F9-7ECC-4666-A464-247FE12496C3}']
    function Get_ClientApplicationID: WideString; safecall;
    procedure Set_ClientApplicationID1(const retval: WideString); safecall;
    function Get_IsForced: WordBool; safecall;
    procedure Set_IsForced(retval: WordBool); safecall;
    function Get_Priority: DownloadPriority; safecall;
    procedure Set_Priority(retval: DownloadPriority); safecall;
    function Get_Updates: IUpdateCollection; safecall;
    procedure Set_Updates(const retval: IUpdateCollection); safecall;
    function BeginDownload(const onProgressChanged: IUnknown; const onCompleted: IUnknown; 
                           state: OleVariant): IDownloadJob; safecall;
    function Download: IDownloadResult; safecall;
    function EndDownload(const value: IDownloadJob): IDownloadResult; safecall;
    property ClientApplicationID: WideString read Get_ClientApplicationID write Set_ClientApplicationID1;
    property IsForced: WordBool read Get_IsForced write Set_IsForced;
    property Priority: DownloadPriority read Get_Priority write Set_Priority;
    property Updates: IUpdateCollection read Get_Updates write Set_Updates;
  end;

// *********************************************************************//
// DispIntf:  IUpdateDownloaderDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {68F1C6F9-7ECC-4666-A464-247FE12496C3}
// *********************************************************************//
  IUpdateDownloaderDisp = dispinterface
    ['{68F1C6F9-7ECC-4666-A464-247FE12496C3}']
    property ClientApplicationID: WideString dispid 1610743809;
    property IsForced: WordBool dispid 1610743810;
    property Priority: DownloadPriority dispid 1610743811;
    property Updates: IUpdateCollection dispid 1610743812;
    function BeginDownload(const onProgressChanged: IUnknown; const onCompleted: IUnknown; 
                           state: OleVariant): IDownloadJob; dispid 1610743813;
    function Download: IDownloadResult; dispid 1610743814;
    function EndDownload(const value: IDownloadJob): IDownloadResult; dispid 1610743815;
  end;

// *********************************************************************//
// Interface: IDownloadResult
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {DAA4FDD0-4727-4DBE-A1E7-745DCA317144}
// *********************************************************************//
  IDownloadResult = interface(IDispatch)
    ['{DAA4FDD0-4727-4DBE-A1E7-745DCA317144}']
    function Get_HResult: Integer; safecall;
    function Get_ResultCode: OperationResultCode; safecall;
    function GetUpdateResult(updateIndex: Integer): IUpdateDownloadResult; safecall;
    property HResult: Integer read Get_HResult;
    property ResultCode: OperationResultCode read Get_ResultCode;
  end;

// *********************************************************************//
// DispIntf:  IDownloadResultDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {DAA4FDD0-4727-4DBE-A1E7-745DCA317144}
// *********************************************************************//
  IDownloadResultDisp = dispinterface
    ['{DAA4FDD0-4727-4DBE-A1E7-745DCA317144}']
    property HResult: Integer readonly dispid 1610743809;
    property ResultCode: OperationResultCode readonly dispid 1610743810;
    function GetUpdateResult(updateIndex: Integer): IUpdateDownloadResult; dispid 1610743811;
  end;

// *********************************************************************//
// Interface: IUpdateInstaller
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {7B929C68-CCDC-4226-96B1-8724600B54C2}
// *********************************************************************//
  IUpdateInstaller = interface(IDispatch)
    ['{7B929C68-CCDC-4226-96B1-8724600B54C2}']
    function Get_ClientApplicationID: WideString; safecall;
    procedure Set_ClientApplicationID1(const retval: WideString); safecall;
    function Get_IsForced: WordBool; safecall;
    procedure Set_IsForced(retval: WordBool); safecall;
    function Get_ParentHwnd: wireHWND; safecall;
    procedure Set_ParentHwnd(retval: wireHWND); safecall;
    procedure Set_parentWindow(const retval: IUnknown); safecall;
    function Get_parentWindow: IUnknown; safecall;
    function Get_Updates: IUpdateCollection; safecall;
    procedure Set_Updates(const retval: IUpdateCollection); safecall;
    function BeginInstall(const onProgressChanged: IUnknown; const onCompleted: IUnknown; 
                          state: OleVariant): IInstallationJob; safecall;
    function BeginUninstall(const onProgressChanged: IUnknown; const onCompleted: IUnknown; 
                            state: OleVariant): IInstallationJob; safecall;
    function EndInstall(const value: IInstallationJob): IInstallationResult; safecall;
    function EndUninstall(const value: IInstallationJob): IInstallationResult; safecall;
    function Install: IInstallationResult; safecall;
    function RunWizard(const dialogTitle: WideString): IInstallationResult; safecall;
    function Get_IsBusy: WordBool; safecall;
    function Uninstall: IInstallationResult; safecall;
    function Get_AllowSourcePrompts: WordBool; safecall;
    procedure Set_AllowSourcePrompts(retval: WordBool); safecall;
    function Get_RebootRequiredBeforeInstallation: WordBool; safecall;
    property ClientApplicationID: WideString read Get_ClientApplicationID write Set_ClientApplicationID1;
    property IsForced: WordBool read Get_IsForced write Set_IsForced;
    property ParentHwnd: wireHWND read Get_ParentHwnd write Set_ParentHwnd;
    property parentWindow: IUnknown read Get_parentWindow write Set_parentWindow;
    property Updates: IUpdateCollection read Get_Updates write Set_Updates;
    property IsBusy: WordBool read Get_IsBusy;
    property AllowSourcePrompts: WordBool read Get_AllowSourcePrompts write Set_AllowSourcePrompts;
    property RebootRequiredBeforeInstallation: WordBool read Get_RebootRequiredBeforeInstallation;
  end;

// *********************************************************************//
// DispIntf:  IUpdateInstallerDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {7B929C68-CCDC-4226-96B1-8724600B54C2}
// *********************************************************************//
  IUpdateInstallerDisp = dispinterface
    ['{7B929C68-CCDC-4226-96B1-8724600B54C2}']
    property ClientApplicationID: WideString dispid 1610743809;
    property IsForced: WordBool dispid 1610743810;
    property ParentHwnd: {??wireHWND}OleVariant dispid 1610743811;
    property parentWindow: IUnknown dispid 1610743812;
    property Updates: IUpdateCollection dispid 1610743813;
    function BeginInstall(const onProgressChanged: IUnknown; const onCompleted: IUnknown; 
                          state: OleVariant): IInstallationJob; dispid 1610743814;
    function BeginUninstall(const onProgressChanged: IUnknown; const onCompleted: IUnknown; 
                            state: OleVariant): IInstallationJob; dispid 1610743815;
    function EndInstall(const value: IInstallationJob): IInstallationResult; dispid 1610743816;
    function EndUninstall(const value: IInstallationJob): IInstallationResult; dispid 1610743817;
    function Install: IInstallationResult; dispid 1610743818;
    function RunWizard(const dialogTitle: WideString): IInstallationResult; dispid 1610743819;
    property IsBusy: WordBool readonly dispid 1610743820;
    function Uninstall: IInstallationResult; dispid 1610743821;
    property AllowSourcePrompts: WordBool dispid 1610743822;
    property RebootRequiredBeforeInstallation: WordBool readonly dispid 1610743823;
  end;

// *********************************************************************//
// Interface: IUpdateInstaller2
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {3442D4FE-224D-4CEE-98CF-30E0C4D229E6}
// *********************************************************************//
  IUpdateInstaller2 = interface(IUpdateInstaller)
    ['{3442D4FE-224D-4CEE-98CF-30E0C4D229E6}']
    function Get_ForceQuiet: WordBool; safecall;
    procedure Set_ForceQuiet(retval: WordBool); safecall;
    property ForceQuiet: WordBool read Get_ForceQuiet write Set_ForceQuiet;
  end;

// *********************************************************************//
// DispIntf:  IUpdateInstaller2Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {3442D4FE-224D-4CEE-98CF-30E0C4D229E6}
// *********************************************************************//
  IUpdateInstaller2Disp = dispinterface
    ['{3442D4FE-224D-4CEE-98CF-30E0C4D229E6}']
    property ForceQuiet: WordBool dispid 1610809345;
    property ClientApplicationID: WideString dispid 1610743809;
    property IsForced: WordBool dispid 1610743810;
    property ParentHwnd: {??wireHWND}OleVariant dispid 1610743811;
    property parentWindow: IUnknown dispid 1610743812;
    property Updates: IUpdateCollection dispid 1610743813;
    function BeginInstall(const onProgressChanged: IUnknown; const onCompleted: IUnknown; 
                          state: OleVariant): IInstallationJob; dispid 1610743814;
    function BeginUninstall(const onProgressChanged: IUnknown; const onCompleted: IUnknown; 
                            state: OleVariant): IInstallationJob; dispid 1610743815;
    function EndInstall(const value: IInstallationJob): IInstallationResult; dispid 1610743816;
    function EndUninstall(const value: IInstallationJob): IInstallationResult; dispid 1610743817;
    function Install: IInstallationResult; dispid 1610743818;
    function RunWizard(const dialogTitle: WideString): IInstallationResult; dispid 1610743819;
    property IsBusy: WordBool readonly dispid 1610743820;
    function Uninstall: IInstallationResult; dispid 1610743821;
    property AllowSourcePrompts: WordBool dispid 1610743822;
    property RebootRequiredBeforeInstallation: WordBool readonly dispid 1610743823;
  end;

// *********************************************************************//
// Interface: IInstallationResult
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A43C56D6-7451-48D4-AF96-B6CD2D0D9B7A}
// *********************************************************************//
  IInstallationResult = interface(IDispatch)
    ['{A43C56D6-7451-48D4-AF96-B6CD2D0D9B7A}']
    function Get_HResult: Integer; safecall;
    function Get_RebootRequired: WordBool; safecall;
    function Get_ResultCode: OperationResultCode; safecall;
    function GetUpdateResult(updateIndex: Integer): IUpdateInstallationResult; safecall;
    property HResult: Integer read Get_HResult;
    property RebootRequired: WordBool read Get_RebootRequired;
    property ResultCode: OperationResultCode read Get_ResultCode;
  end;

// *********************************************************************//
// DispIntf:  IInstallationResultDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A43C56D6-7451-48D4-AF96-B6CD2D0D9B7A}
// *********************************************************************//
  IInstallationResultDisp = dispinterface
    ['{A43C56D6-7451-48D4-AF96-B6CD2D0D9B7A}']
    property HResult: Integer readonly dispid 1610743809;
    property RebootRequired: WordBool readonly dispid 1610743810;
    property ResultCode: OperationResultCode readonly dispid 1610743811;
    function GetUpdateResult(updateIndex: Integer): IUpdateInstallationResult; dispid 1610743812;
  end;

// *********************************************************************//
// Interface: IUpdateSession
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {816858A4-260D-4260-933A-2585F1ABC76B}
// *********************************************************************//
  IUpdateSession = interface(IDispatch)
    ['{816858A4-260D-4260-933A-2585F1ABC76B}']
    function Get_ClientApplicationID: WideString; safecall;
    procedure Set_ClientApplicationID1(const retval: WideString); safecall;
    function Get_ReadOnly: WordBool; safecall;
    function Get_WebProxy: IWebProxy; safecall;
    procedure Set_WebProxy(const retval: IWebProxy); safecall;
    function CreateUpdateSearcher: IUpdateSearcher; safecall;
    function CreateUpdateDownloader: IUpdateDownloader; safecall;
    function CreateUpdateInstaller: IUpdateInstaller; safecall;
    property ClientApplicationID: WideString read Get_ClientApplicationID write Set_ClientApplicationID1;
    property ReadOnly: WordBool read Get_ReadOnly;
    property WebProxy: IWebProxy read Get_WebProxy write Set_WebProxy;
  end;

// *********************************************************************//
// DispIntf:  IUpdateSessionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {816858A4-260D-4260-933A-2585F1ABC76B}
// *********************************************************************//
  IUpdateSessionDisp = dispinterface
    ['{816858A4-260D-4260-933A-2585F1ABC76B}']
    property ClientApplicationID: WideString dispid 1610743809;
    property ReadOnly: WordBool readonly dispid 1610743810;
    property WebProxy: IWebProxy dispid 1610743811;
    function CreateUpdateSearcher: IUpdateSearcher; dispid 1610743812;
    function CreateUpdateDownloader: IUpdateDownloader; dispid 1610743813;
    function CreateUpdateInstaller: IUpdateInstaller; dispid 1610743814;
  end;

// *********************************************************************//
// Interface: IUpdateSession2
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {91CAF7B0-EB23-49ED-9937-C52D817F46F7}
// *********************************************************************//
  IUpdateSession2 = interface(IUpdateSession)
    ['{91CAF7B0-EB23-49ED-9937-C52D817F46F7}']
    function Get_UserLocale: LongWord; safecall;
    procedure Set_UserLocale(retval: LongWord); safecall;
    property UserLocale: LongWord read Get_UserLocale write Set_UserLocale;
  end;

// *********************************************************************//
// DispIntf:  IUpdateSession2Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {91CAF7B0-EB23-49ED-9937-C52D817F46F7}
// *********************************************************************//
  IUpdateSession2Disp = dispinterface
    ['{91CAF7B0-EB23-49ED-9937-C52D817F46F7}']
    property UserLocale: LongWord dispid 1610809345;
    property ClientApplicationID: WideString dispid 1610743809;
    property ReadOnly: WordBool readonly dispid 1610743810;
    property WebProxy: IWebProxy dispid 1610743811;
    function CreateUpdateSearcher: IUpdateSearcher; dispid 1610743812;
    function CreateUpdateDownloader: IUpdateDownloader; dispid 1610743813;
    function CreateUpdateInstaller: IUpdateInstaller; dispid 1610743814;
  end;

// *********************************************************************//
// Interface: IUpdateSession3
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {918EFD1E-B5D8-4C90-8540-AEB9BDC56F9D}
// *********************************************************************//
  IUpdateSession3 = interface(IUpdateSession2)
    ['{918EFD1E-B5D8-4C90-8540-AEB9BDC56F9D}']
    function CreateUpdateServiceManager: IUpdateServiceManager2; safecall;
    function QueryHistory(const criteria: WideString; startIndex: Integer; Count: Integer): IUpdateHistoryEntryCollection; safecall;
  end;

// *********************************************************************//
// DispIntf:  IUpdateSession3Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {918EFD1E-B5D8-4C90-8540-AEB9BDC56F9D}
// *********************************************************************//
  IUpdateSession3Disp = dispinterface
    ['{918EFD1E-B5D8-4C90-8540-AEB9BDC56F9D}']
    function CreateUpdateServiceManager: IUpdateServiceManager2; dispid 1610874881;
    function QueryHistory(const criteria: WideString; startIndex: Integer; Count: Integer): IUpdateHistoryEntryCollection; dispid 1610874882;
    property UserLocale: LongWord dispid 1610809345;
    property ClientApplicationID: WideString dispid 1610743809;
    property ReadOnly: WordBool readonly dispid 1610743810;
    property WebProxy: IWebProxy dispid 1610743811;
    function CreateUpdateSearcher: IUpdateSearcher; dispid 1610743812;
    function CreateUpdateDownloader: IUpdateDownloader; dispid 1610743813;
    function CreateUpdateInstaller: IUpdateInstaller; dispid 1610743814;
  end;

// *********************************************************************//
// Interface: IUpdateServiceManager
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {23857E3C-02BA-44A3-9423-B1C900805F37}
// *********************************************************************//
  IUpdateServiceManager = interface(IDispatch)
    ['{23857E3C-02BA-44A3-9423-B1C900805F37}']
    function Get_Services: IUpdateServiceCollection; safecall;
    function AddService(const ServiceID: WideString; const authorizationCabPath: WideString): IUpdateService; safecall;
    procedure RegisterServiceWithAU(const ServiceID: WideString); safecall;
    procedure RemoveService(const ServiceID: WideString); safecall;
    procedure UnregisterServiceWithAU(const ServiceID: WideString); safecall;
    function AddScanPackageService(const serviceName: WideString; 
                                   const scanFileLocation: WideString; flags: Integer): IUpdateService; safecall;
    procedure SetOption(const optionName: WideString; optionValue: OleVariant); safecall;
    property Services: IUpdateServiceCollection read Get_Services;
  end;

// *********************************************************************//
// DispIntf:  IUpdateServiceManagerDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {23857E3C-02BA-44A3-9423-B1C900805F37}
// *********************************************************************//
  IUpdateServiceManagerDisp = dispinterface
    ['{23857E3C-02BA-44A3-9423-B1C900805F37}']
    property Services: IUpdateServiceCollection readonly dispid 1610743809;
    function AddService(const ServiceID: WideString; const authorizationCabPath: WideString): IUpdateService; dispid 1610743810;
    procedure RegisterServiceWithAU(const ServiceID: WideString); dispid 1610743811;
    procedure RemoveService(const ServiceID: WideString); dispid 1610743812;
    procedure UnregisterServiceWithAU(const ServiceID: WideString); dispid 1610743813;
    function AddScanPackageService(const serviceName: WideString; 
                                   const scanFileLocation: WideString; flags: Integer): IUpdateService; dispid 1610743814;
    procedure SetOption(const optionName: WideString; optionValue: OleVariant); dispid 1610678279;
  end;

// *********************************************************************//
// Interface: IUpdateServiceManager2
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0BB8531D-7E8D-424F-986C-A0B8F60A3E7B}
// *********************************************************************//
  IUpdateServiceManager2 = interface(IUpdateServiceManager)
    ['{0BB8531D-7E8D-424F-986C-A0B8F60A3E7B}']
    function Get_ClientApplicationID: WideString; safecall;
    procedure Set_ClientApplicationID(const retval: WideString); safecall;
    function QueryServiceRegistration(const ServiceID: WideString): IUpdateServiceRegistration; safecall;
    function AddService2(const ServiceID: WideString; flags: Integer; 
                         const authorizationCabPath: WideString): IUpdateServiceRegistration; safecall;
    property ClientApplicationID: WideString read Get_ClientApplicationID write Set_ClientApplicationID;
  end;

// *********************************************************************//
// DispIntf:  IUpdateServiceManager2Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0BB8531D-7E8D-424F-986C-A0B8F60A3E7B}
// *********************************************************************//
  IUpdateServiceManager2Disp = dispinterface
    ['{0BB8531D-7E8D-424F-986C-A0B8F60A3E7B}']
    property ClientApplicationID: WideString dispid 1610809345;
    function QueryServiceRegistration(const ServiceID: WideString): IUpdateServiceRegistration; dispid 1610809346;
    function AddService2(const ServiceID: WideString; flags: Integer; 
                         const authorizationCabPath: WideString): IUpdateServiceRegistration; dispid 1610809347;
    property Services: IUpdateServiceCollection readonly dispid 1610743809;
    function AddService(const ServiceID: WideString; const authorizationCabPath: WideString): IUpdateService; dispid 1610743810;
    procedure RegisterServiceWithAU(const ServiceID: WideString); dispid 1610743811;
    procedure RemoveService(const ServiceID: WideString); dispid 1610743812;
    procedure UnregisterServiceWithAU(const ServiceID: WideString); dispid 1610743813;
    function AddScanPackageService(const serviceName: WideString; 
                                   const scanFileLocation: WideString; flags: Integer): IUpdateService; dispid 1610743814;
    procedure SetOption(const optionName: WideString; optionValue: OleVariant); dispid 1610678279;
  end;

// *********************************************************************//
// Interface: IUpdateServiceCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {9B0353AA-0E52-44FF-B8B0-1F7FA0437F88}
// *********************************************************************//
  IUpdateServiceCollection = interface(IDispatch)
    ['{9B0353AA-0E52-44FF-B8B0-1F7FA0437F88}']
    function Get_Item(index: Integer): IUpdateService; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_Count: Integer; safecall;
    property Item[index: Integer]: IUpdateService read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IUpdateServiceCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {9B0353AA-0E52-44FF-B8B0-1F7FA0437F88}
// *********************************************************************//
  IUpdateServiceCollectionDisp = dispinterface
    ['{9B0353AA-0E52-44FF-B8B0-1F7FA0437F88}']
    property Item[index: Integer]: IUpdateService readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
    property Count: Integer readonly dispid 1610743809;
  end;

// *********************************************************************//
// Interface: IUpdateService
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {76B3B17E-AED6-4DA5-85F0-83587F81ABE3}
// *********************************************************************//
  IUpdateService = interface(IDispatch)
    ['{76B3B17E-AED6-4DA5-85F0-83587F81ABE3}']
    function Get_Name: WideString; safecall;
    function Get_ContentValidationCert: OleVariant; safecall;
    function Get_ExpirationDate: TDateTime; safecall;
    function Get_IsManaged: WordBool; safecall;
    function Get_IsRegisteredWithAU: WordBool; safecall;
    function Get_IssueDate: TDateTime; safecall;
    function Get_OffersWindowsUpdates: WordBool; safecall;
    function Get_RedirectUrls: IStringCollection; safecall;
    function Get_ServiceID: WideString; safecall;
    function Get_IsScanPackageService: WordBool; safecall;
    function Get_CanRegisterWithAU: WordBool; safecall;
    function Get_ServiceUrl: WideString; safecall;
    function Get_SetupPrefix: WideString; safecall;
    property Name: WideString read Get_Name;
    property ContentValidationCert: OleVariant read Get_ContentValidationCert;
    property ExpirationDate: TDateTime read Get_ExpirationDate;
    property IsManaged: WordBool read Get_IsManaged;
    property IsRegisteredWithAU: WordBool read Get_IsRegisteredWithAU;
    property IssueDate: TDateTime read Get_IssueDate;
    property OffersWindowsUpdates: WordBool read Get_OffersWindowsUpdates;
    property RedirectUrls: IStringCollection read Get_RedirectUrls;
    property ServiceID: WideString read Get_ServiceID;
    property IsScanPackageService: WordBool read Get_IsScanPackageService;
    property CanRegisterWithAU: WordBool read Get_CanRegisterWithAU;
    property ServiceUrl: WideString read Get_ServiceUrl;
    property SetupPrefix: WideString read Get_SetupPrefix;
  end;

// *********************************************************************//
// DispIntf:  IUpdateServiceDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {76B3B17E-AED6-4DA5-85F0-83587F81ABE3}
// *********************************************************************//
  IUpdateServiceDisp = dispinterface
    ['{76B3B17E-AED6-4DA5-85F0-83587F81ABE3}']
    property Name: WideString readonly dispid 0;
    property ContentValidationCert: OleVariant readonly dispid 1610743809;
    property ExpirationDate: TDateTime readonly dispid 1610743810;
    property IsManaged: WordBool readonly dispid 1610743811;
    property IsRegisteredWithAU: WordBool readonly dispid 1610743812;
    property IssueDate: TDateTime readonly dispid 1610743813;
    property OffersWindowsUpdates: WordBool readonly dispid 1610743814;
    property RedirectUrls: IStringCollection readonly dispid 1610743815;
    property ServiceID: WideString readonly dispid 1610743816;
    property IsScanPackageService: WordBool readonly dispid 1610743818;
    property CanRegisterWithAU: WordBool readonly dispid 1610743819;
    property ServiceUrl: WideString readonly dispid 1610743820;
    property SetupPrefix: WideString readonly dispid 1610743821;
  end;

// *********************************************************************//
// Interface: IUpdateServiceRegistration
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {DDE02280-12B3-4E0B-937B-6747F6ACB286}
// *********************************************************************//
  IUpdateServiceRegistration = interface(IDispatch)
    ['{DDE02280-12B3-4E0B-937B-6747F6ACB286}']
    function Get_RegistrationState: UpdateServiceRegistrationState; safecall;
    function Get_ServiceID: WideString; safecall;
    function Get_IsPendingRegistrationWithAU: WordBool; safecall;
    function Get_Service: IUpdateService2; safecall;
    property RegistrationState: UpdateServiceRegistrationState read Get_RegistrationState;
    property ServiceID: WideString read Get_ServiceID;
    property IsPendingRegistrationWithAU: WordBool read Get_IsPendingRegistrationWithAU;
    property Service: IUpdateService2 read Get_Service;
  end;

// *********************************************************************//
// DispIntf:  IUpdateServiceRegistrationDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {DDE02280-12B3-4E0B-937B-6747F6ACB286}
// *********************************************************************//
  IUpdateServiceRegistrationDisp = dispinterface
    ['{DDE02280-12B3-4E0B-937B-6747F6ACB286}']
    property RegistrationState: UpdateServiceRegistrationState readonly dispid 0;
    property ServiceID: WideString readonly dispid 1610743809;
    property IsPendingRegistrationWithAU: WordBool readonly dispid 1610743810;
    property Service: IUpdateService2 readonly dispid 1610743811;
  end;

// *********************************************************************//
// Interface: IUpdateService2
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {1518B460-6518-4172-940F-C75883B24CEB}
// *********************************************************************//
  IUpdateService2 = interface(IUpdateService)
    ['{1518B460-6518-4172-940F-C75883B24CEB}']
    function Get_IsDefaultAUService: WordBool; safecall;
    property IsDefaultAUService: WordBool read Get_IsDefaultAUService;
  end;

// *********************************************************************//
// DispIntf:  IUpdateService2Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {1518B460-6518-4172-940F-C75883B24CEB}
// *********************************************************************//
  IUpdateService2Disp = dispinterface
    ['{1518B460-6518-4172-940F-C75883B24CEB}']
    property IsDefaultAUService: WordBool readonly dispid 1610809345;
    property Name: WideString readonly dispid 0;
    property ContentValidationCert: OleVariant readonly dispid 1610743809;
    property ExpirationDate: TDateTime readonly dispid 1610743810;
    property IsManaged: WordBool readonly dispid 1610743811;
    property IsRegisteredWithAU: WordBool readonly dispid 1610743812;
    property IssueDate: TDateTime readonly dispid 1610743813;
    property OffersWindowsUpdates: WordBool readonly dispid 1610743814;
    property RedirectUrls: IStringCollection readonly dispid 1610743815;
    property ServiceID: WideString readonly dispid 1610743816;
    property IsScanPackageService: WordBool readonly dispid 1610743818;
    property CanRegisterWithAU: WordBool readonly dispid 1610743819;
    property ServiceUrl: WideString readonly dispid 1610743820;
    property SetupPrefix: WideString readonly dispid 1610743821;
  end;

// *********************************************************************//
// The Class CoStringCollection provides a Create and CreateRemote method to          
// create instances of the default interface IStringCollection exposed by              
// the CoClass StringCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStringCollection = class
    class function Create: IStringCollection;
    class function CreateRemote(const MachineName: string): IStringCollection;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TStringCollection
// Help String      : StringCollection Class
// Default Interface: IStringCollection
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TStringCollectionProperties= class;
{$ENDIF}
  TStringCollection = class(TOleServer)
  private
    FIntf: IStringCollection;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TStringCollectionProperties;
    function GetServerProperties: TStringCollectionProperties;
{$ENDIF}
    function GetDefaultInterface: IStringCollection;
  protected
    procedure InitServerData; override;
    function Get_Item(index: Integer): WideString;
    procedure Set_Item(index: Integer; const retval: WideString);
    function Get__NewEnum: IUnknown;
    function Get_Count: Integer;
    function Get_ReadOnly: WordBool;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IStringCollection);
    procedure Disconnect; override;
    function Add(const value: WideString): Integer;
    procedure Clear;
    function Copy: IStringCollection;
    procedure Insert(index: Integer; const value: WideString);
    procedure RemoveAt(index: Integer);
    property DefaultInterface: IStringCollection read GetDefaultInterface;
    property Item[index: Integer]: WideString read Get_Item write Set_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
    property ReadOnly: WordBool read Get_ReadOnly;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TStringCollectionProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TStringCollection
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TStringCollectionProperties = class(TPersistent)
  private
    FServer:    TStringCollection;
    function    GetDefaultInterface: IStringCollection;
    constructor Create(AServer: TStringCollection);
  protected
    function Get_Item(index: Integer): WideString;
    procedure Set_Item(index: Integer; const retval: WideString);
    function Get__NewEnum: IUnknown;
    function Get_Count: Integer;
    function Get_ReadOnly: WordBool;
  public
    property DefaultInterface: IStringCollection read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoUpdateSearcher provides a Create and CreateRemote method to          
// create instances of the default interface IUpdateSearcher2 exposed by              
// the CoClass UpdateSearcher. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoUpdateSearcher = class
    class function Create: IUpdateSearcher2;
    class function CreateRemote(const MachineName: string): IUpdateSearcher2;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TUpdateSearcher
// Help String      : UpdateSearcher Class
// Default Interface: IUpdateSearcher2
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TUpdateSearcherProperties= class;
{$ENDIF}
  TUpdateSearcher = class(TOleServer)
  private
    FIntf: IUpdateSearcher2;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TUpdateSearcherProperties;
    function GetServerProperties: TUpdateSearcherProperties;
{$ENDIF}
    function GetDefaultInterface: IUpdateSearcher2;
  protected
    procedure InitServerData; override;
    function Get_IgnoreDownloadPriority: WordBool;
    procedure Set_IgnoreDownloadPriority(retval: WordBool);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IUpdateSearcher2);
    procedure Disconnect; override;
    property DefaultInterface: IUpdateSearcher2 read GetDefaultInterface;
    property IgnoreDownloadPriority: WordBool read Get_IgnoreDownloadPriority write Set_IgnoreDownloadPriority;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TUpdateSearcherProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TUpdateSearcher
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TUpdateSearcherProperties = class(TPersistent)
  private
    FServer:    TUpdateSearcher;
    function    GetDefaultInterface: IUpdateSearcher2;
    constructor Create(AServer: TUpdateSearcher);
  protected
    function Get_IgnoreDownloadPriority: WordBool;
    procedure Set_IgnoreDownloadPriority(retval: WordBool);
  public
    property DefaultInterface: IUpdateSearcher2 read GetDefaultInterface;
  published
    property IgnoreDownloadPriority: WordBool read Get_IgnoreDownloadPriority write Set_IgnoreDownloadPriority;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoWebProxy provides a Create and CreateRemote method to          
// create instances of the default interface IWebProxy exposed by              
// the CoClass WebProxy. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoWebProxy = class
    class function Create: IWebProxy;
    class function CreateRemote(const MachineName: string): IWebProxy;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TWebProxy
// Help String      : WebProxy Class
// Default Interface: IWebProxy
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TWebProxyProperties= class;
{$ENDIF}
  TWebProxy = class(TOleServer)
  private
    FIntf: IWebProxy;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TWebProxyProperties;
    function GetServerProperties: TWebProxyProperties;
{$ENDIF}
    function GetDefaultInterface: IWebProxy;
  protected
    procedure InitServerData; override;
    function Get_Address: WideString;
    procedure Set_Address1(const retval: WideString);
    function Get_BypassList: IStringCollection;
    procedure Set_BypassList(const retval: IStringCollection);
    function Get_BypassProxyOnLocal: WordBool;
    procedure Set_BypassProxyOnLocal(retval: WordBool);
    function Get_ReadOnly: WordBool;
    function Get_UserName: WideString;
    procedure Set_UserName(const retval: WideString);
    function Get_AutoDetect: WordBool;
    procedure Set_AutoDetect(retval: WordBool);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IWebProxy);
    procedure Disconnect; override;
    procedure SetPassword(const value: WideString);
    procedure PromptForCredentials(const parentWindow: IUnknown; const Title: WideString);
    property DefaultInterface: IWebProxy read GetDefaultInterface;
    property ReadOnly: WordBool read Get_ReadOnly;
    property Address: WideString read Get_Address write Set_Address1;
    property BypassList: IStringCollection read Get_BypassList write Set_BypassList;
    property BypassProxyOnLocal: WordBool read Get_BypassProxyOnLocal write Set_BypassProxyOnLocal;
    property UserName: WideString read Get_UserName write Set_UserName;
    property AutoDetect: WordBool read Get_AutoDetect write Set_AutoDetect;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TWebProxyProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TWebProxy
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TWebProxyProperties = class(TPersistent)
  private
    FServer:    TWebProxy;
    function    GetDefaultInterface: IWebProxy;
    constructor Create(AServer: TWebProxy);
  protected
    function Get_Address: WideString;
    procedure Set_Address1(const retval: WideString);
    function Get_BypassList: IStringCollection;
    procedure Set_BypassList(const retval: IStringCollection);
    function Get_BypassProxyOnLocal: WordBool;
    procedure Set_BypassProxyOnLocal(retval: WordBool);
    function Get_ReadOnly: WordBool;
    function Get_UserName: WideString;
    procedure Set_UserName(const retval: WideString);
    function Get_AutoDetect: WordBool;
    procedure Set_AutoDetect(retval: WordBool);
  public
    property DefaultInterface: IWebProxy read GetDefaultInterface;
  published
    property Address: WideString read Get_Address write Set_Address;
    property BypassList: IStringCollection read Get_BypassList write Set_BypassList;
    property BypassProxyOnLocal: WordBool read Get_BypassProxyOnLocal write Set_BypassProxyOnLocal;
    property UserName: WideString read Get_UserName write Set_UserName;
    property AutoDetect: WordBool read Get_AutoDetect write Set_AutoDetect;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoSystemInformation provides a Create and CreateRemote method to          
// create instances of the default interface ISystemInformation exposed by              
// the CoClass SystemInformation. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSystemInformation = class
    class function Create: ISystemInformation;
    class function CreateRemote(const MachineName: string): ISystemInformation;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TSystemInformation
// Help String      : SystemInformation Class
// Default Interface: ISystemInformation
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TSystemInformationProperties= class;
{$ENDIF}
  TSystemInformation = class(TOleServer)
  private
    FIntf: ISystemInformation;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TSystemInformationProperties;
    function GetServerProperties: TSystemInformationProperties;
{$ENDIF}
    function GetDefaultInterface: ISystemInformation;
  protected
    procedure InitServerData; override;
    function Get_OemHardwareSupportLink: WideString;
    function Get_RebootRequired: WordBool;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ISystemInformation);
    procedure Disconnect; override;
    property DefaultInterface: ISystemInformation read GetDefaultInterface;
    property OemHardwareSupportLink: WideString read Get_OemHardwareSupportLink;
    property RebootRequired: WordBool read Get_RebootRequired;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TSystemInformationProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TSystemInformation
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TSystemInformationProperties = class(TPersistent)
  private
    FServer:    TSystemInformation;
    function    GetDefaultInterface: ISystemInformation;
    constructor Create(AServer: TSystemInformation);
  protected
    function Get_OemHardwareSupportLink: WideString;
    function Get_RebootRequired: WordBool;
  public
    property DefaultInterface: ISystemInformation read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoWindowsUpdateAgentInfo provides a Create and CreateRemote method to          
// create instances of the default interface IWindowsUpdateAgentInfo exposed by              
// the CoClass WindowsUpdateAgentInfo. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoWindowsUpdateAgentInfo = class
    class function Create: IWindowsUpdateAgentInfo;
    class function CreateRemote(const MachineName: string): IWindowsUpdateAgentInfo;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TWindowsUpdateAgentInfo
// Help String      : WindowsUpdateAgentInfo Class
// Default Interface: IWindowsUpdateAgentInfo
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TWindowsUpdateAgentInfoProperties= class;
{$ENDIF}
  TWindowsUpdateAgentInfo = class(TOleServer)
  private
    FIntf: IWindowsUpdateAgentInfo;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TWindowsUpdateAgentInfoProperties;
    function GetServerProperties: TWindowsUpdateAgentInfoProperties;
{$ENDIF}
    function GetDefaultInterface: IWindowsUpdateAgentInfo;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IWindowsUpdateAgentInfo);
    procedure Disconnect; override;
    function GetInfo(varInfoIdentifier: OleVariant): OleVariant;
    property DefaultInterface: IWindowsUpdateAgentInfo read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TWindowsUpdateAgentInfoProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TWindowsUpdateAgentInfo
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TWindowsUpdateAgentInfoProperties = class(TPersistent)
  private
    FServer:    TWindowsUpdateAgentInfo;
    function    GetDefaultInterface: IWindowsUpdateAgentInfo;
    constructor Create(AServer: TWindowsUpdateAgentInfo);
  protected
  public
    property DefaultInterface: IWindowsUpdateAgentInfo read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoAutomaticUpdates provides a Create and CreateRemote method to          
// create instances of the default interface IAutomaticUpdates2 exposed by              
// the CoClass AutomaticUpdates. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoAutomaticUpdates = class
    class function Create: IAutomaticUpdates2;
    class function CreateRemote(const MachineName: string): IAutomaticUpdates2;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TAutomaticUpdates
// Help String      : AutomaticUpdates Class
// Default Interface: IAutomaticUpdates2
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TAutomaticUpdatesProperties= class;
{$ENDIF}
  TAutomaticUpdates = class(TOleServer)
  private
    FIntf: IAutomaticUpdates2;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TAutomaticUpdatesProperties;
    function GetServerProperties: TAutomaticUpdatesProperties;
{$ENDIF}
    function GetDefaultInterface: IAutomaticUpdates2;
  protected
    procedure InitServerData; override;
    function Get_Results: IAutomaticUpdatesResults;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IAutomaticUpdates2);
    procedure Disconnect; override;
    property DefaultInterface: IAutomaticUpdates2 read GetDefaultInterface;
    property Results: IAutomaticUpdatesResults read Get_Results;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TAutomaticUpdatesProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TAutomaticUpdates
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TAutomaticUpdatesProperties = class(TPersistent)
  private
    FServer:    TAutomaticUpdates;
    function    GetDefaultInterface: IAutomaticUpdates2;
    constructor Create(AServer: TAutomaticUpdates);
  protected
    function Get_Results: IAutomaticUpdatesResults;
  public
    property DefaultInterface: IAutomaticUpdates2 read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoUpdateCollection provides a Create and CreateRemote method to          
// create instances of the default interface IUpdateCollection exposed by              
// the CoClass UpdateCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoUpdateCollection = class
    class function Create: IUpdateCollection;
    class function CreateRemote(const MachineName: string): IUpdateCollection;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TUpdateCollection
// Help String      : UpdateCollection Class
// Default Interface: IUpdateCollection
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TUpdateCollectionProperties= class;
{$ENDIF}
  TUpdateCollection = class(TOleServer)
  private
    FIntf: IUpdateCollection;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TUpdateCollectionProperties;
    function GetServerProperties: TUpdateCollectionProperties;
{$ENDIF}
    function GetDefaultInterface: IUpdateCollection;
  protected
    procedure InitServerData; override;
    function Get_Item(index: Integer): IUpdate;
    procedure Set_Item(index: Integer; const retval: IUpdate);
    function Get__NewEnum: IUnknown;
    function Get_Count: Integer;
    function Get_ReadOnly: WordBool;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IUpdateCollection);
    procedure Disconnect; override;
    function Add(const value: IUpdate): Integer;
    procedure Clear;
    function Copy: IUpdateCollection;
    procedure Insert(index: Integer; const value: IUpdate);
    procedure RemoveAt(index: Integer);
    property DefaultInterface: IUpdateCollection read GetDefaultInterface;
    property Item[index: Integer]: IUpdate read Get_Item write Set_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
    property ReadOnly: WordBool read Get_ReadOnly;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TUpdateCollectionProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TUpdateCollection
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TUpdateCollectionProperties = class(TPersistent)
  private
    FServer:    TUpdateCollection;
    function    GetDefaultInterface: IUpdateCollection;
    constructor Create(AServer: TUpdateCollection);
  protected
    function Get_Item(index: Integer): IUpdate;
    procedure Set_Item(index: Integer; const retval: IUpdate);
    function Get__NewEnum: IUnknown;
    function Get_Count: Integer;
    function Get_ReadOnly: WordBool;
  public
    property DefaultInterface: IUpdateCollection read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoUpdateDownloader provides a Create and CreateRemote method to          
// create instances of the default interface IUpdateDownloader exposed by              
// the CoClass UpdateDownloader. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoUpdateDownloader = class
    class function Create: IUpdateDownloader;
    class function CreateRemote(const MachineName: string): IUpdateDownloader;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TUpdateDownloader
// Help String      : UpdateDownloader Class
// Default Interface: IUpdateDownloader
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TUpdateDownloaderProperties= class;
{$ENDIF}
  TUpdateDownloader = class(TOleServer)
  private
    FIntf: IUpdateDownloader;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TUpdateDownloaderProperties;
    function GetServerProperties: TUpdateDownloaderProperties;
{$ENDIF}
    function GetDefaultInterface: IUpdateDownloader;
  protected
    procedure InitServerData; override;
    function Get_ClientApplicationID: WideString;
    procedure Set_ClientApplicationID1(const retval: WideString);
    function Get_IsForced: WordBool;
    procedure Set_IsForced(retval: WordBool);
    function Get_Priority: DownloadPriority;
    procedure Set_Priority(retval: DownloadPriority);
    function Get_Updates: IUpdateCollection;
    procedure Set_Updates(const retval: IUpdateCollection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IUpdateDownloader);
    procedure Disconnect; override;
    function BeginDownload(const onProgressChanged: IUnknown; const onCompleted: IUnknown; 
                           state: OleVariant): IDownloadJob;
    function Download: IDownloadResult;
    function EndDownload(const value: IDownloadJob): IDownloadResult;
    property DefaultInterface: IUpdateDownloader read GetDefaultInterface;
    property ClientApplicationID: WideString read Get_ClientApplicationID write Set_ClientApplicationID1;
    property IsForced: WordBool read Get_IsForced write Set_IsForced;
    property Priority: DownloadPriority read Get_Priority write Set_Priority;
    property Updates: IUpdateCollection read Get_Updates write Set_Updates;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TUpdateDownloaderProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TUpdateDownloader
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TUpdateDownloaderProperties = class(TPersistent)
  private
    FServer:    TUpdateDownloader;
    function    GetDefaultInterface: IUpdateDownloader;
    constructor Create(AServer: TUpdateDownloader);
  protected
    function Get_ClientApplicationID: WideString;
    procedure Set_ClientApplicationID1(const retval: WideString);
    function Get_IsForced: WordBool;
    procedure Set_IsForced(retval: WordBool);
    function Get_Priority: DownloadPriority;
    procedure Set_Priority(retval: DownloadPriority);
    function Get_Updates: IUpdateCollection;
    procedure Set_Updates(const retval: IUpdateCollection);
  public
    property DefaultInterface: IUpdateDownloader read GetDefaultInterface;
  published
    property ClientApplicationID: WideString read Get_ClientApplicationID write Set_ClientApplicationID;
    property IsForced: WordBool read Get_IsForced write Set_IsForced;
    property Priority: DownloadPriority read Get_Priority write Set_Priority;
    property Updates: IUpdateCollection read Get_Updates write Set_Updates;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoUpdateInstaller provides a Create and CreateRemote method to          
// create instances of the default interface IUpdateInstaller2 exposed by              
// the CoClass UpdateInstaller. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoUpdateInstaller = class
    class function Create: IUpdateInstaller2;
    class function CreateRemote(const MachineName: string): IUpdateInstaller2;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TUpdateInstaller
// Help String      : UpdateInstaller Class
// Default Interface: IUpdateInstaller2
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TUpdateInstallerProperties= class;
{$ENDIF}
  TUpdateInstaller = class(TOleServer)
  private
    FIntf: IUpdateInstaller2;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TUpdateInstallerProperties;
    function GetServerProperties: TUpdateInstallerProperties;
{$ENDIF}
    function GetDefaultInterface: IUpdateInstaller2;
  protected
    procedure InitServerData; override;
    function Get_ForceQuiet: WordBool;
    procedure Set_ForceQuiet(retval: WordBool);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IUpdateInstaller2);
    procedure Disconnect; override;
    property DefaultInterface: IUpdateInstaller2 read GetDefaultInterface;
    property ForceQuiet: WordBool read Get_ForceQuiet write Set_ForceQuiet;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TUpdateInstallerProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TUpdateInstaller
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TUpdateInstallerProperties = class(TPersistent)
  private
    FServer:    TUpdateInstaller;
    function    GetDefaultInterface: IUpdateInstaller2;
    constructor Create(AServer: TUpdateInstaller);
  protected
    function Get_ForceQuiet: WordBool;
    procedure Set_ForceQuiet(retval: WordBool);
  public
    property DefaultInterface: IUpdateInstaller2 read GetDefaultInterface;
  published
    property ForceQuiet: WordBool read Get_ForceQuiet write Set_ForceQuiet;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoUpdateSession provides a Create and CreateRemote method to          
// create instances of the default interface IUpdateSession3 exposed by              
// the CoClass UpdateSession. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoUpdateSession = class
    class function Create: IUpdateSession3;
    class function CreateRemote(const MachineName: string): IUpdateSession3;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TUpdateSession
// Help String      : UpdateSession Class
// Default Interface: IUpdateSession3
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TUpdateSessionProperties= class;
{$ENDIF}
  TUpdateSession = class(TOleServer)
  private
    FIntf: IUpdateSession3;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TUpdateSessionProperties;
    function GetServerProperties: TUpdateSessionProperties;
{$ENDIF}
    function GetDefaultInterface: IUpdateSession3;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IUpdateSession3);
    procedure Disconnect; override;
    function CreateUpdateServiceManager: IUpdateServiceManager2;
    function QueryHistory(const criteria: WideString; startIndex: Integer; Count: Integer): IUpdateHistoryEntryCollection;
    property DefaultInterface: IUpdateSession3 read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TUpdateSessionProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TUpdateSession
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TUpdateSessionProperties = class(TPersistent)
  private
    FServer:    TUpdateSession;
    function    GetDefaultInterface: IUpdateSession3;
    constructor Create(AServer: TUpdateSession);
  protected
  public
    property DefaultInterface: IUpdateSession3 read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoUpdateServiceManager provides a Create and CreateRemote method to          
// create instances of the default interface IUpdateServiceManager2 exposed by              
// the CoClass UpdateServiceManager. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoUpdateServiceManager = class
    class function Create: IUpdateServiceManager2;
    class function CreateRemote(const MachineName: string): IUpdateServiceManager2;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TUpdateServiceManager
// Help String      : UpdateServiceManager Class
// Default Interface: IUpdateServiceManager2
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TUpdateServiceManagerProperties= class;
{$ENDIF}
  TUpdateServiceManager = class(TOleServer)
  private
    FIntf: IUpdateServiceManager2;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TUpdateServiceManagerProperties;
    function GetServerProperties: TUpdateServiceManagerProperties;
{$ENDIF}
    function GetDefaultInterface: IUpdateServiceManager2;
  protected
    procedure InitServerData; override;
    function Get_ClientApplicationID: WideString;
    procedure Set_ClientApplicationID(const retval: WideString);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IUpdateServiceManager2);
    procedure Disconnect; override;
    function QueryServiceRegistration(const ServiceID: WideString): IUpdateServiceRegistration;
    function AddService2(const ServiceID: WideString; flags: Integer; 
                         const authorizationCabPath: WideString): IUpdateServiceRegistration;
    property DefaultInterface: IUpdateServiceManager2 read GetDefaultInterface;
    property ClientApplicationID: WideString read Get_ClientApplicationID write Set_ClientApplicationID;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TUpdateServiceManagerProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TUpdateServiceManager
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TUpdateServiceManagerProperties = class(TPersistent)
  private
    FServer:    TUpdateServiceManager;
    function    GetDefaultInterface: IUpdateServiceManager2;
    constructor Create(AServer: TUpdateServiceManager);
  protected
    function Get_ClientApplicationID: WideString;
    procedure Set_ClientApplicationID(const retval: WideString);
  public
    property DefaultInterface: IUpdateServiceManager2 read GetDefaultInterface;
  published
    property ClientApplicationID: WideString read Get_ClientApplicationID write Set_ClientApplicationID;
  end;
{$ENDIF}


procedure Register;

resourcestring
  dtlServerPage = '(none)';

  dtlOcxPage = '(none)';

implementation

uses ComObj;

class function CoStringCollection.Create: IStringCollection;
begin
  Result := CreateComObject(CLASS_StringCollection) as IStringCollection;
end;

class function CoStringCollection.CreateRemote(const MachineName: string): IStringCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_StringCollection) as IStringCollection;
end;

procedure TStringCollection.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{72C97D74-7C3B-40AE-B77D-ABDB22EBA6FB}';
    IntfIID:   '{EFF90582-2DDC-480F-A06D-60F3FBC362C3}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TStringCollection.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IStringCollection;
  end;
end;

procedure TStringCollection.ConnectTo(svrIntf: IStringCollection);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TStringCollection.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TStringCollection.GetDefaultInterface: IStringCollection;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TStringCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TStringCollectionProperties.Create(Self);
{$ENDIF}
end;

destructor TStringCollection.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TStringCollection.GetServerProperties: TStringCollectionProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TStringCollection.Get_Item(index: Integer): WideString;
begin
    Result := DefaultInterface.Item[index];
end;

procedure TStringCollection.Set_Item(index: Integer; const retval: WideString);
  { Warning: The property Item has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Item := retval;
end;

function TStringCollection.Get__NewEnum: IUnknown;
begin
    Result := DefaultInterface._NewEnum;
end;

function TStringCollection.Get_Count: Integer;
begin
    Result := DefaultInterface.Count;
end;

function TStringCollection.Get_ReadOnly: WordBool;
begin
    Result := DefaultInterface.ReadOnly;
end;

function TStringCollection.Add(const value: WideString): Integer;
begin
  Result := DefaultInterface.Add(value);
end;

procedure TStringCollection.Clear;
begin
  DefaultInterface.Clear;
end;

function TStringCollection.Copy: IStringCollection;
begin
  Result := DefaultInterface.Copy;
end;

procedure TStringCollection.Insert(index: Integer; const value: WideString);
begin
  DefaultInterface.Insert(index, value);
end;

procedure TStringCollection.RemoveAt(index: Integer);
begin
  DefaultInterface.RemoveAt(index);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TStringCollectionProperties.Create(AServer: TStringCollection);
begin
  inherited Create;
  FServer := AServer;
end;

function TStringCollectionProperties.GetDefaultInterface: IStringCollection;
begin
  Result := FServer.DefaultInterface;
end;

function TStringCollectionProperties.Get_Item(index: Integer): WideString;
begin
    Result := DefaultInterface.Item[index];
end;

procedure TStringCollectionProperties.Set_Item(index: Integer; const retval: WideString);
  { Warning: The property Item has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Item := retval;
end;

function TStringCollectionProperties.Get__NewEnum: IUnknown;
begin
    Result := DefaultInterface._NewEnum;
end;

function TStringCollectionProperties.Get_Count: Integer;
begin
    Result := DefaultInterface.Count;
end;

function TStringCollectionProperties.Get_ReadOnly: WordBool;
begin
    Result := DefaultInterface.ReadOnly;
end;

{$ENDIF}

class function CoUpdateSearcher.Create: IUpdateSearcher2;
begin
  Result := CreateComObject(CLASS_UpdateSearcher) as IUpdateSearcher2;
end;

class function CoUpdateSearcher.CreateRemote(const MachineName: string): IUpdateSearcher2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_UpdateSearcher) as IUpdateSearcher2;
end;

procedure TUpdateSearcher.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{B699E5E8-67FF-4177-88B0-3684A3388BFB}';
    IntfIID:   '{4CBDCB2D-1589-4BEB-BD1C-3E582FF0ADD0}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TUpdateSearcher.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IUpdateSearcher2;
  end;
end;

procedure TUpdateSearcher.ConnectTo(svrIntf: IUpdateSearcher2);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TUpdateSearcher.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TUpdateSearcher.GetDefaultInterface: IUpdateSearcher2;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TUpdateSearcher.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TUpdateSearcherProperties.Create(Self);
{$ENDIF}
end;

destructor TUpdateSearcher.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TUpdateSearcher.GetServerProperties: TUpdateSearcherProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TUpdateSearcher.Get_IgnoreDownloadPriority: WordBool;
begin
    Result := DefaultInterface.IgnoreDownloadPriority;
end;

procedure TUpdateSearcher.Set_IgnoreDownloadPriority(retval: WordBool);
begin
  DefaultInterface.Set_IgnoreDownloadPriority(retval);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TUpdateSearcherProperties.Create(AServer: TUpdateSearcher);
begin
  inherited Create;
  FServer := AServer;
end;

function TUpdateSearcherProperties.GetDefaultInterface: IUpdateSearcher2;
begin
  Result := FServer.DefaultInterface;
end;

function TUpdateSearcherProperties.Get_IgnoreDownloadPriority: WordBool;
begin
    Result := DefaultInterface.IgnoreDownloadPriority;
end;

procedure TUpdateSearcherProperties.Set_IgnoreDownloadPriority(retval: WordBool);
begin
  DefaultInterface.Set_IgnoreDownloadPriority(retval);
end;

{$ENDIF}

class function CoWebProxy.Create: IWebProxy;
begin
  Result := CreateComObject(CLASS_WebProxy) as IWebProxy;
end;

class function CoWebProxy.CreateRemote(const MachineName: string): IWebProxy;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_WebProxy) as IWebProxy;
end;

procedure TWebProxy.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{650503CF-9108-4DDC-A2CE-6C2341E1C582}';
    IntfIID:   '{174C81FE-AECD-4DAE-B8A0-2C6318DD86A8}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TWebProxy.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IWebProxy;
  end;
end;

procedure TWebProxy.ConnectTo(svrIntf: IWebProxy);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TWebProxy.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TWebProxy.GetDefaultInterface: IWebProxy;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TWebProxy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TWebProxyProperties.Create(Self);
{$ENDIF}
end;

destructor TWebProxy.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TWebProxy.GetServerProperties: TWebProxyProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TWebProxy.Get_Address: WideString;
begin
    Result := DefaultInterface.Address;
end;

procedure TWebProxy.Set_Address1(const retval: WideString);
  { Warning: The property Address1 has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Address1 := retval;
end;

function TWebProxy.Get_BypassList: IStringCollection;
begin
    Result := DefaultInterface.BypassList;
end;

procedure TWebProxy.Set_BypassList(const retval: IStringCollection);
begin
  DefaultInterface.Set_BypassList(retval);
end;

function TWebProxy.Get_BypassProxyOnLocal: WordBool;
begin
    Result := DefaultInterface.BypassProxyOnLocal;
end;

procedure TWebProxy.Set_BypassProxyOnLocal(retval: WordBool);
begin
  DefaultInterface.Set_BypassProxyOnLocal(retval);
end;

function TWebProxy.Get_ReadOnly: WordBool;
begin
    Result := DefaultInterface.ReadOnly;
end;

function TWebProxy.Get_UserName: WideString;
begin
    Result := DefaultInterface.UserName;
end;

procedure TWebProxy.Set_UserName(const retval: WideString);
  { Warning: The property UserName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.UserName := retval;
end;

function TWebProxy.Get_AutoDetect: WordBool;
begin
    Result := DefaultInterface.AutoDetect;
end;

procedure TWebProxy.Set_AutoDetect(retval: WordBool);
begin
  DefaultInterface.Set_AutoDetect(retval);
end;

procedure TWebProxy.SetPassword(const value: WideString);
begin
  DefaultInterface.SetPassword(value);
end;

procedure TWebProxy.PromptForCredentials(const parentWindow: IUnknown; const Title: WideString);
begin
  DefaultInterface.PromptForCredentials(parentWindow, Title);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TWebProxyProperties.Create(AServer: TWebProxy);
begin
  inherited Create;
  FServer := AServer;
end;

function TWebProxyProperties.GetDefaultInterface: IWebProxy;
begin
  Result := FServer.DefaultInterface;
end;

function TWebProxyProperties.Get_Address: WideString;
begin
    Result := DefaultInterface.Address;
end;

procedure TWebProxyProperties.Set_Address1(const retval: WideString);
  { Warning: The property Address1 has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Address1 := retval;
end;

function TWebProxyProperties.Get_BypassList: IStringCollection;
begin
    Result := DefaultInterface.BypassList;
end;

procedure TWebProxyProperties.Set_BypassList(const retval: IStringCollection);
begin
  DefaultInterface.Set_BypassList(retval);
end;

function TWebProxyProperties.Get_BypassProxyOnLocal: WordBool;
begin
    Result := DefaultInterface.BypassProxyOnLocal;
end;

procedure TWebProxyProperties.Set_BypassProxyOnLocal(retval: WordBool);
begin
  DefaultInterface.Set_BypassProxyOnLocal(retval);
end;

function TWebProxyProperties.Get_ReadOnly: WordBool;
begin
    Result := DefaultInterface.ReadOnly;
end;

function TWebProxyProperties.Get_UserName: WideString;
begin
    Result := DefaultInterface.UserName;
end;

procedure TWebProxyProperties.Set_UserName(const retval: WideString);
  { Warning: The property UserName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.UserName := retval;
end;

function TWebProxyProperties.Get_AutoDetect: WordBool;
begin
    Result := DefaultInterface.AutoDetect;
end;

procedure TWebProxyProperties.Set_AutoDetect(retval: WordBool);
begin
  DefaultInterface.Set_AutoDetect(retval);
end;

{$ENDIF}

class function CoSystemInformation.Create: ISystemInformation;
begin
  Result := CreateComObject(CLASS_SystemInformation) as ISystemInformation;
end;

class function CoSystemInformation.CreateRemote(const MachineName: string): ISystemInformation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SystemInformation) as ISystemInformation;
end;

procedure TSystemInformation.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{C01B9BA0-BEA7-41BA-B604-D0A36F469133}';
    IntfIID:   '{ADE87BF7-7B56-4275-8FAB-B9B0E591844B}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TSystemInformation.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ISystemInformation;
  end;
end;

procedure TSystemInformation.ConnectTo(svrIntf: ISystemInformation);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TSystemInformation.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TSystemInformation.GetDefaultInterface: ISystemInformation;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TSystemInformation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TSystemInformationProperties.Create(Self);
{$ENDIF}
end;

destructor TSystemInformation.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TSystemInformation.GetServerProperties: TSystemInformationProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TSystemInformation.Get_OemHardwareSupportLink: WideString;
begin
    Result := DefaultInterface.OemHardwareSupportLink;
end;

function TSystemInformation.Get_RebootRequired: WordBool;
begin
    Result := DefaultInterface.RebootRequired;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TSystemInformationProperties.Create(AServer: TSystemInformation);
begin
  inherited Create;
  FServer := AServer;
end;

function TSystemInformationProperties.GetDefaultInterface: ISystemInformation;
begin
  Result := FServer.DefaultInterface;
end;

function TSystemInformationProperties.Get_OemHardwareSupportLink: WideString;
begin
    Result := DefaultInterface.OemHardwareSupportLink;
end;

function TSystemInformationProperties.Get_RebootRequired: WordBool;
begin
    Result := DefaultInterface.RebootRequired;
end;

{$ENDIF}

class function CoWindowsUpdateAgentInfo.Create: IWindowsUpdateAgentInfo;
begin
  Result := CreateComObject(CLASS_WindowsUpdateAgentInfo) as IWindowsUpdateAgentInfo;
end;

class function CoWindowsUpdateAgentInfo.CreateRemote(const MachineName: string): IWindowsUpdateAgentInfo;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_WindowsUpdateAgentInfo) as IWindowsUpdateAgentInfo;
end;

procedure TWindowsUpdateAgentInfo.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{C2E88C2F-6F5B-4AAA-894B-55C847AD3A2D}';
    IntfIID:   '{85713FA1-7796-4FA2-BE3B-E2D6124DD373}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TWindowsUpdateAgentInfo.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IWindowsUpdateAgentInfo;
  end;
end;

procedure TWindowsUpdateAgentInfo.ConnectTo(svrIntf: IWindowsUpdateAgentInfo);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TWindowsUpdateAgentInfo.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TWindowsUpdateAgentInfo.GetDefaultInterface: IWindowsUpdateAgentInfo;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TWindowsUpdateAgentInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TWindowsUpdateAgentInfoProperties.Create(Self);
{$ENDIF}
end;

destructor TWindowsUpdateAgentInfo.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TWindowsUpdateAgentInfo.GetServerProperties: TWindowsUpdateAgentInfoProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TWindowsUpdateAgentInfo.GetInfo(varInfoIdentifier: OleVariant): OleVariant;
begin
  Result := DefaultInterface.GetInfo(varInfoIdentifier);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TWindowsUpdateAgentInfoProperties.Create(AServer: TWindowsUpdateAgentInfo);
begin
  inherited Create;
  FServer := AServer;
end;

function TWindowsUpdateAgentInfoProperties.GetDefaultInterface: IWindowsUpdateAgentInfo;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoAutomaticUpdates.Create: IAutomaticUpdates2;
begin
  Result := CreateComObject(CLASS_AutomaticUpdates) as IAutomaticUpdates2;
end;

class function CoAutomaticUpdates.CreateRemote(const MachineName: string): IAutomaticUpdates2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_AutomaticUpdates) as IAutomaticUpdates2;
end;

procedure TAutomaticUpdates.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{BFE18E9C-6D87-4450-B37C-E02F0B373803}';
    IntfIID:   '{4A2F5C31-CFD9-410E-B7FB-29A653973A0F}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TAutomaticUpdates.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IAutomaticUpdates2;
  end;
end;

procedure TAutomaticUpdates.ConnectTo(svrIntf: IAutomaticUpdates2);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TAutomaticUpdates.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TAutomaticUpdates.GetDefaultInterface: IAutomaticUpdates2;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TAutomaticUpdates.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TAutomaticUpdatesProperties.Create(Self);
{$ENDIF}
end;

destructor TAutomaticUpdates.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TAutomaticUpdates.GetServerProperties: TAutomaticUpdatesProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TAutomaticUpdates.Get_Results: IAutomaticUpdatesResults;
begin
    Result := DefaultInterface.Results;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TAutomaticUpdatesProperties.Create(AServer: TAutomaticUpdates);
begin
  inherited Create;
  FServer := AServer;
end;

function TAutomaticUpdatesProperties.GetDefaultInterface: IAutomaticUpdates2;
begin
  Result := FServer.DefaultInterface;
end;

function TAutomaticUpdatesProperties.Get_Results: IAutomaticUpdatesResults;
begin
    Result := DefaultInterface.Results;
end;

{$ENDIF}

class function CoUpdateCollection.Create: IUpdateCollection;
begin
  Result := CreateComObject(CLASS_UpdateCollection) as IUpdateCollection;
end;

class function CoUpdateCollection.CreateRemote(const MachineName: string): IUpdateCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_UpdateCollection) as IUpdateCollection;
end;

procedure TUpdateCollection.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{13639463-00DB-4646-803D-528026140D88}';
    IntfIID:   '{07F7438C-7709-4CA5-B518-91279288134E}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TUpdateCollection.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IUpdateCollection;
  end;
end;

procedure TUpdateCollection.ConnectTo(svrIntf: IUpdateCollection);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TUpdateCollection.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TUpdateCollection.GetDefaultInterface: IUpdateCollection;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TUpdateCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TUpdateCollectionProperties.Create(Self);
{$ENDIF}
end;

destructor TUpdateCollection.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TUpdateCollection.GetServerProperties: TUpdateCollectionProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TUpdateCollection.Get_Item(index: Integer): IUpdate;
begin
    Result := DefaultInterface.Item[index];
end;

procedure TUpdateCollection.Set_Item(index: Integer; const retval: IUpdate);
begin
  DefaultInterface.Item[index] := retval;
end;

function TUpdateCollection.Get__NewEnum: IUnknown;
begin
    Result := DefaultInterface._NewEnum;
end;

function TUpdateCollection.Get_Count: Integer;
begin
    Result := DefaultInterface.Count;
end;

function TUpdateCollection.Get_ReadOnly: WordBool;
begin
    Result := DefaultInterface.ReadOnly;
end;

function TUpdateCollection.Add(const value: IUpdate): Integer;
begin
  Result := DefaultInterface.Add(value);
end;

procedure TUpdateCollection.Clear;
begin
  DefaultInterface.Clear;
end;

function TUpdateCollection.Copy: IUpdateCollection;
begin
  Result := DefaultInterface.Copy;
end;

procedure TUpdateCollection.Insert(index: Integer; const value: IUpdate);
begin
  DefaultInterface.Insert(index, value);
end;

procedure TUpdateCollection.RemoveAt(index: Integer);
begin
  DefaultInterface.RemoveAt(index);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TUpdateCollectionProperties.Create(AServer: TUpdateCollection);
begin
  inherited Create;
  FServer := AServer;
end;

function TUpdateCollectionProperties.GetDefaultInterface: IUpdateCollection;
begin
  Result := FServer.DefaultInterface;
end;

function TUpdateCollectionProperties.Get_Item(index: Integer): IUpdate;
begin
    Result := DefaultInterface.Item[index];
end;

procedure TUpdateCollectionProperties.Set_Item(index: Integer; const retval: IUpdate);
begin
  DefaultInterface.Item[index] := retval;
end;

function TUpdateCollectionProperties.Get__NewEnum: IUnknown;
begin
    Result := DefaultInterface._NewEnum;
end;

function TUpdateCollectionProperties.Get_Count: Integer;
begin
    Result := DefaultInterface.Count;
end;

function TUpdateCollectionProperties.Get_ReadOnly: WordBool;
begin
    Result := DefaultInterface.ReadOnly;
end;

{$ENDIF}

class function CoUpdateDownloader.Create: IUpdateDownloader;
begin
  Result := CreateComObject(CLASS_UpdateDownloader) as IUpdateDownloader;
end;

class function CoUpdateDownloader.CreateRemote(const MachineName: string): IUpdateDownloader;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_UpdateDownloader) as IUpdateDownloader;
end;

procedure TUpdateDownloader.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{5BAF654A-5A07-4264-A255-9FF54C7151E7}';
    IntfIID:   '{68F1C6F9-7ECC-4666-A464-247FE12496C3}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TUpdateDownloader.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IUpdateDownloader;
  end;
end;

procedure TUpdateDownloader.ConnectTo(svrIntf: IUpdateDownloader);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TUpdateDownloader.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TUpdateDownloader.GetDefaultInterface: IUpdateDownloader;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TUpdateDownloader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TUpdateDownloaderProperties.Create(Self);
{$ENDIF}
end;

destructor TUpdateDownloader.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TUpdateDownloader.GetServerProperties: TUpdateDownloaderProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TUpdateDownloader.Get_ClientApplicationID: WideString;
begin
    Result := DefaultInterface.ClientApplicationID;
end;

procedure TUpdateDownloader.Set_ClientApplicationID1(const retval: WideString);
  { Warning: The property ClientApplicationID1 has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ClientApplicationID1 := retval;
end;

function TUpdateDownloader.Get_IsForced: WordBool;
begin
    Result := DefaultInterface.IsForced;
end;

procedure TUpdateDownloader.Set_IsForced(retval: WordBool);
begin
  DefaultInterface.Set_IsForced(retval);
end;

function TUpdateDownloader.Get_Priority: DownloadPriority;
begin
    Result := DefaultInterface.Priority;
end;

procedure TUpdateDownloader.Set_Priority(retval: DownloadPriority);
begin
  DefaultInterface.Set_Priority(retval);
end;

function TUpdateDownloader.Get_Updates: IUpdateCollection;
begin
    Result := DefaultInterface.Updates;
end;

procedure TUpdateDownloader.Set_Updates(const retval: IUpdateCollection);
begin
  DefaultInterface.Set_Updates(retval);
end;

function TUpdateDownloader.BeginDownload(const onProgressChanged: IUnknown; 
                                         const onCompleted: IUnknown; state: OleVariant): IDownloadJob;
begin
  Result := DefaultInterface.BeginDownload(onProgressChanged, onCompleted, state);
end;

function TUpdateDownloader.Download: IDownloadResult;
begin
  Result := DefaultInterface.Download;
end;

function TUpdateDownloader.EndDownload(const value: IDownloadJob): IDownloadResult;
begin
  Result := DefaultInterface.EndDownload(value);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TUpdateDownloaderProperties.Create(AServer: TUpdateDownloader);
begin
  inherited Create;
  FServer := AServer;
end;

function TUpdateDownloaderProperties.GetDefaultInterface: IUpdateDownloader;
begin
  Result := FServer.DefaultInterface;
end;

function TUpdateDownloaderProperties.Get_ClientApplicationID: WideString;
begin
    Result := DefaultInterface.ClientApplicationID;
end;

procedure TUpdateDownloaderProperties.Set_ClientApplicationID1(const retval: WideString);
  { Warning: The property ClientApplicationID1 has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ClientApplicationID1 := retval;
end;

function TUpdateDownloaderProperties.Get_IsForced: WordBool;
begin
    Result := DefaultInterface.IsForced;
end;

procedure TUpdateDownloaderProperties.Set_IsForced(retval: WordBool);
begin
  DefaultInterface.Set_IsForced(retval);
end;

function TUpdateDownloaderProperties.Get_Priority: DownloadPriority;
begin
    Result := DefaultInterface.Priority;
end;

procedure TUpdateDownloaderProperties.Set_Priority(retval: DownloadPriority);
begin
  DefaultInterface.Set_Priority(retval);
end;

function TUpdateDownloaderProperties.Get_Updates: IUpdateCollection;
begin
    Result := DefaultInterface.Updates;
end;

procedure TUpdateDownloaderProperties.Set_Updates(const retval: IUpdateCollection);
begin
  DefaultInterface.Set_Updates(retval);
end;

{$ENDIF}

class function CoUpdateInstaller.Create: IUpdateInstaller2;
begin
  Result := CreateComObject(CLASS_UpdateInstaller) as IUpdateInstaller2;
end;

class function CoUpdateInstaller.CreateRemote(const MachineName: string): IUpdateInstaller2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_UpdateInstaller) as IUpdateInstaller2;
end;

procedure TUpdateInstaller.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{D2E0FE7F-D23E-48E1-93C0-6FA8CC346474}';
    IntfIID:   '{3442D4FE-224D-4CEE-98CF-30E0C4D229E6}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TUpdateInstaller.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IUpdateInstaller2;
  end;
end;

procedure TUpdateInstaller.ConnectTo(svrIntf: IUpdateInstaller2);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TUpdateInstaller.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TUpdateInstaller.GetDefaultInterface: IUpdateInstaller2;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TUpdateInstaller.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TUpdateInstallerProperties.Create(Self);
{$ENDIF}
end;

destructor TUpdateInstaller.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TUpdateInstaller.GetServerProperties: TUpdateInstallerProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TUpdateInstaller.Get_ForceQuiet: WordBool;
begin
    Result := DefaultInterface.ForceQuiet;
end;

procedure TUpdateInstaller.Set_ForceQuiet(retval: WordBool);
begin
  DefaultInterface.Set_ForceQuiet(retval);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TUpdateInstallerProperties.Create(AServer: TUpdateInstaller);
begin
  inherited Create;
  FServer := AServer;
end;

function TUpdateInstallerProperties.GetDefaultInterface: IUpdateInstaller2;
begin
  Result := FServer.DefaultInterface;
end;

function TUpdateInstallerProperties.Get_ForceQuiet: WordBool;
begin
    Result := DefaultInterface.ForceQuiet;
end;

procedure TUpdateInstallerProperties.Set_ForceQuiet(retval: WordBool);
begin
  DefaultInterface.Set_ForceQuiet(retval);
end;

{$ENDIF}

class function CoUpdateSession.Create: IUpdateSession3;
begin
  Result := CreateComObject(CLASS_UpdateSession) as IUpdateSession3;
end;

class function CoUpdateSession.CreateRemote(const MachineName: string): IUpdateSession3;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_UpdateSession) as IUpdateSession3;
end;

procedure TUpdateSession.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{4CB43D7F-7EEE-4906-8698-60DA1C38F2FE}';
    IntfIID:   '{918EFD1E-B5D8-4C90-8540-AEB9BDC56F9D}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TUpdateSession.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IUpdateSession3;
  end;
end;

procedure TUpdateSession.ConnectTo(svrIntf: IUpdateSession3);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TUpdateSession.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TUpdateSession.GetDefaultInterface: IUpdateSession3;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TUpdateSession.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TUpdateSessionProperties.Create(Self);
{$ENDIF}
end;

destructor TUpdateSession.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TUpdateSession.GetServerProperties: TUpdateSessionProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TUpdateSession.CreateUpdateServiceManager: IUpdateServiceManager2;
begin
  Result := DefaultInterface.CreateUpdateServiceManager;
end;

function TUpdateSession.QueryHistory(const criteria: WideString; startIndex: Integer; Count: Integer): IUpdateHistoryEntryCollection;
begin
  Result := DefaultInterface.QueryHistory(criteria, startIndex, Count);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TUpdateSessionProperties.Create(AServer: TUpdateSession);
begin
  inherited Create;
  FServer := AServer;
end;

function TUpdateSessionProperties.GetDefaultInterface: IUpdateSession3;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoUpdateServiceManager.Create: IUpdateServiceManager2;
begin
  Result := CreateComObject(CLASS_UpdateServiceManager) as IUpdateServiceManager2;
end;

class function CoUpdateServiceManager.CreateRemote(const MachineName: string): IUpdateServiceManager2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_UpdateServiceManager) as IUpdateServiceManager2;
end;

procedure TUpdateServiceManager.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{F8D253D9-89A4-4DAA-87B6-1168369F0B21}';
    IntfIID:   '{0BB8531D-7E8D-424F-986C-A0B8F60A3E7B}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TUpdateServiceManager.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IUpdateServiceManager2;
  end;
end;

procedure TUpdateServiceManager.ConnectTo(svrIntf: IUpdateServiceManager2);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TUpdateServiceManager.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TUpdateServiceManager.GetDefaultInterface: IUpdateServiceManager2;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TUpdateServiceManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TUpdateServiceManagerProperties.Create(Self);
{$ENDIF}
end;

destructor TUpdateServiceManager.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TUpdateServiceManager.GetServerProperties: TUpdateServiceManagerProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TUpdateServiceManager.Get_ClientApplicationID: WideString;
begin
    Result := DefaultInterface.ClientApplicationID;
end;

procedure TUpdateServiceManager.Set_ClientApplicationID(const retval: WideString);
  { Warning: The property ClientApplicationID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ClientApplicationID := retval;
end;

function TUpdateServiceManager.QueryServiceRegistration(const ServiceID: WideString): IUpdateServiceRegistration;
begin
  Result := DefaultInterface.QueryServiceRegistration(ServiceID);
end;

function TUpdateServiceManager.AddService2(const ServiceID: WideString; flags: Integer; 
                                           const authorizationCabPath: WideString): IUpdateServiceRegistration;
begin
  Result := DefaultInterface.AddService2(ServiceID, flags, authorizationCabPath);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TUpdateServiceManagerProperties.Create(AServer: TUpdateServiceManager);
begin
  inherited Create;
  FServer := AServer;
end;

function TUpdateServiceManagerProperties.GetDefaultInterface: IUpdateServiceManager2;
begin
  Result := FServer.DefaultInterface;
end;

function TUpdateServiceManagerProperties.Get_ClientApplicationID: WideString;
begin
    Result := DefaultInterface.ClientApplicationID;
end;

procedure TUpdateServiceManagerProperties.Set_ClientApplicationID(const retval: WideString);
  { Warning: The property ClientApplicationID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ClientApplicationID := retval;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents(dtlServerPage, [TStringCollection, TUpdateSearcher, TWebProxy, TSystemInformation, 
    TWindowsUpdateAgentInfo, TAutomaticUpdates, TUpdateCollection, TUpdateDownloader, TUpdateInstaller, 
    TUpdateSession, TUpdateServiceManager]);
end;

end.
