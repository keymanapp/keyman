unit COMSVCSLib_TLB;

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
// File generated on 05.06.2008 12:00:48 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Windows\system32\comsvcs.dll (1)
// LIBID: {2A005C00-A5DE-11CF-9E66-00AA00A3F464}
// LCID: 0
// Helpfile: C:\Windows\system32\cossdk.chm
// HelpString: COM+ Services Type Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
// Errors:
//   Hint: Member 'Property' of 'ISharedPropertyGroup' changed to 'Property_'
//   Hint: Symbol 'type' renamed to 'type_'
//   Error creating palette bitmap of (TTransactionContext) : Server C:\Windows\system32\comsvcs.dll contains no icons
//   Error creating palette bitmap of (TTransactionContextEx) : Server C:\Windows\system32\comsvcs.dll contains no icons
//   Error creating palette bitmap of (TCServiceConfig) : Server C:\Windows\system32\comsvcs.dll contains no icons
//   Error creating palette bitmap of (TServicePool) : Server C:\Windows\system32\comsvcs.dll contains no icons
//   Error creating palette bitmap of (TServicePoolConfig) : Server C:\Windows\system32\comsvcs.dll contains no icons
//   Error creating palette bitmap of (TSharedPropertyGroupManager) : Server C:\Windows\system32\comsvcs.dll contains no icons
//   Error creating palette bitmap of (TCOMEvents) : Server C:\Windows\system32\comsvcs.dll contains no icons
//   Error creating palette bitmap of (TCoMTSLocator) : Server C:\Windows\system32\comsvcs.dll contains no icons
//   Error creating palette bitmap of (TMtsGrp) : Server C:\Windows\system32\comsvcs.dll contains no icons
//   Error creating palette bitmap of (TCRMClerk) : Server C:\Windows\system32\comsvcs.dll contains no icons
//   Error creating palette bitmap of (TCRMRecoveryClerk) : Server C:\Windows\system32\comsvcs.dll contains no icons
//   Error creating palette bitmap of (TMessageMover) : Server C:\Windows\system32\comsvcs.dll contains no icons
//   Error creating palette bitmap of (TPoolMgr) : Server C:\Windows\system32\comsvcs.dll contains no icons
//   Error creating palette bitmap of (TPartitionMoniker) : Server C:\Windows\system32\comsvcs.dll contains no icons
//   Error creating palette bitmap of (TSoapMoniker) : Server C:\Windows\system32\comsvcs.dll contains no icons
//   Error creating palette bitmap of (TAppDomainHelper) : Server C:\Windows\system32\comsvcs.dll contains no icons
//   Error creating palette bitmap of (TClrAssemblyLocator) : Server C:\Windows\system32\comsvcs.dll contains no icons
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
  COMSVCSLibMajorVersion = 1;
  COMSVCSLibMinorVersion = 0;

  LIBID_COMSVCSLib: TGUID = '{2A005C00-A5DE-11CF-9E66-00AA00A3F464}';

  IID_ISecurityCertificateColl: TGUID = '{CAFC823B-B441-11D1-B82B-0000F8757E2A}';
  CLASS_SecurityCertificate: TGUID = '{ECABB0A4-7F19-11D2-978E-0000F8757E2A}';
  IID_ISecurityIdentityColl: TGUID = '{CAFC823C-B441-11D1-B82B-0000F8757E2A}';
  CLASS_SecurityIdentity: TGUID = '{ECABB0A5-7F19-11D2-978E-0000F8757E2A}';
  IID_ISecurityCallersColl: TGUID = '{CAFC823D-B441-11D1-B82B-0000F8757E2A}';
  CLASS_SecurityCallers: TGUID = '{ECABB0A6-7F19-11D2-978E-0000F8757E2A}';
  IID_ISecurityCallContext: TGUID = '{CAFC823E-B441-11D1-B82B-0000F8757E2A}';
  CLASS_SecurityCallContext: TGUID = '{ECABB0A7-7F19-11D2-978E-0000F8757E2A}';
  IID_IGetSecurityCallContext: TGUID = '{CAFC823F-B441-11D1-B82B-0000F8757E2A}';
  CLASS_GetSecurityCallContextAppObject: TGUID = '{ECABB0A8-7F19-11D2-978E-0000F8757E2A}';
  IID_IContextState: TGUID = '{3C05E54B-A42A-11D2-AFC4-00C04F8EE1C4}';
  IID_IObjectContext: TGUID = '{51372AE0-CAE7-11CF-BE81-00AA00A2FA25}';
  IID_IObjectContextActivity: TGUID = '{51372AFC-CAE7-11CF-BE81-00AA00A2FA25}';
  IID_IObjectContextInfo: TGUID = '{75B52DDB-E8ED-11D1-93AD-00AA00BA3258}';
  IID_IObjectConstruct: TGUID = '{41C4F8B3-7439-11D2-98CB-00C04F8EE1C4}';
  IID_IObjectConstructString: TGUID = '{41C4F8B2-7439-11D2-98CB-00C04F8EE1C4}';
  IID_IObjectControl: TGUID = '{51372AEC-CAE7-11CF-BE81-00AA00A2FA25}';
  IID_IObjectContextTip: TGUID = '{92FD41CA-BAD9-11D2-9A2D-00C04F797BC9}';
  IID_IPlaybackControl: TGUID = '{51372AFD-CAE7-11CF-BE81-00AA00A2FA25}';
  IID_ISecurityProperty: TGUID = '{51372AEA-CAE7-11CF-BE81-00AA00A2FA25}';
  IID_ICrmCompensator: TGUID = '{BBC01830-8D3B-11D1-82EC-00A0C91EEDE9}';
  IID_ICrmCompensatorVariants: TGUID = '{F0BAF8E4-7804-11D1-82E9-00A0C91EEDE9}';
  IID_ICrmLogControl: TGUID = '{A0E174B3-D26E-11D2-8F84-00805FC7BCD9}';
  IID_ICrmFormatLogRecords: TGUID = '{9C51D821-C98B-11D1-82FB-00A0C91EEDE9}';
  IID_IServiceCall: TGUID = '{BD3E2E12-42DD-40F4-A09A-95A50C58304B}';
  IID_IServiceActivity: TGUID = '{67532E0C-9E2F-4450-A354-035633944E17}';
  IID_ITransactionStatus: TGUID = '{61F589E8-3724-4898-A0A4-664AE9E1D1B4}';
  IID_ICheckSxsConfig: TGUID = '{0FF5A96F-11FC-47D1-BAA6-25DD347E7242}';
  IID_IAsyncErrorNotify: TGUID = '{FE6777FB-A674-4177-8F32-6D707E113484}';
  IID_IProcessInitializer: TGUID = '{1113F52D-DC7F-4943-AED6-88D04027E32A}';
  IID_IManagedObjectInfo: TGUID = '{1427C51A-4584-49D8-90A0-C50D8086CBE9}';
  IID_IManagedPooledObj: TGUID = '{C5DA4BEA-1B42-4437-8926-B6A38860A770}';
  CLASS_Dummy30040732: TGUID = '{ECABB0A9-7F19-11D2-978E-0000F8757E2A}';
  IID_ContextInfo: TGUID = '{19A5A02C-0AC8-11D2-B286-00C04F8EF934}';
  IID_ContextInfo2: TGUID = '{C99D6E75-2375-11D4-8331-00C04F605588}';
  IID_ObjectControl: TGUID = '{7DC41850-0C31-11D0-8B79-00AA00B8A790}';
  IID_IMTxAS: TGUID = '{74C08641-CEDB-11CF-8B49-00AA00B8A790}';
  CLASS_AppServer: TGUID = '{71E38F91-7E88-11CF-9EDE-0080C78B7F89}';
  IID_ObjectContext: TGUID = '{74C08646-CEDB-11CF-8B49-00AA00B8A790}';
  IID_SecurityProperty: TGUID = '{E74A7215-014D-11D1-A63C-00A0C911B4E0}';
  IID_ITransactionContext: TGUID = '{7999FC21-D3C6-11CF-ACAB-00A024A55AEF}';
  CLASS_TransactionContext: TGUID = '{7999FC25-D3C6-11CF-ACAB-00A024A55AEF}';
  IID_ITransactionContextEx: TGUID = '{7999FC22-D3C6-11CF-ACAB-00A024A55AEF}';
  CLASS_TransactionContextEx: TGUID = '{5CB66670-D3D4-11CF-ACAB-00A024A55AEF}';
  IID_ICreateWithTipTransactionEx: TGUID = '{455ACF59-5345-11D2-99CF-00C04F797BC9}';
  IID_ICreateWithTransactionEx: TGUID = '{455ACF57-5345-11D2-99CF-00C04F797BC9}';
  CLASS_ByotServerEx: TGUID = '{ECABB0AA-7F19-11D2-978E-0000F8757E2A}';
  IID_ITransaction: TGUID = '{0FB15084-AF41-11CE-BD2B-204C4F4F5020}';
  IID_IServiceInheritanceConfig: TGUID = '{92186771-D3B4-4D77-A8EA-EE842D586F35}';
  IID_IServiceThreadPoolConfig: TGUID = '{186D89BC-F277-4BCC-80D5-4DF7B836EF4A}';
  IID_IServiceTransactionConfigBase: TGUID = '{772B3FBE-6FFD-42FB-B5F8-8F9B260F3810}';
  IID_IServiceTransactionConfig: TGUID = '{59F4C2A3-D3D7-4A31-B6E4-6AB3177C50B9}';
  IID_IServiceSynchronizationConfig: TGUID = '{FD880E81-6DCE-4C58-AF83-A208846C0030}';
  IID_IServiceIISIntrinsicsConfig: TGUID = '{1A0CF920-D452-46F4-BC36-48118D54EA52}';
  IID_IServiceComTIIntrinsicsConfig: TGUID = '{09E6831E-04E1-4ED4-9D0F-E8B168BAFEAF}';
  IID_IServiceTrackerConfig: TGUID = '{6C3A3E1D-0BA6-4036-B76F-D0404DB816C9}';
  IID_IServiceSxsConfig: TGUID = '{C7CD7379-F3F2-4634-811B-703281D73E08}';
  IID_IServicePartitionConfig: TGUID = '{80182D03-5EA4-4831-AE97-55BEFFC2E590}';
  CLASS_CServiceConfig: TGUID = '{ECABB0C8-7F19-11D2-978E-0000F8757E2A}';
  IID_IServicePool: TGUID = '{B302DF81-EA45-451E-99A2-09F9FD1B1E13}';
  CLASS_ServicePool: TGUID = '{ECABB0C9-7F19-11D2-978E-0000F8757E2A}';
  IID_IServicePoolConfig: TGUID = '{A9690656-5BCA-470C-8451-250C1F43A33E}';
  CLASS_ServicePoolConfig: TGUID = '{ECABB0CA-7F19-11D2-978E-0000F8757E2A}';
  IID_IClassFactory: TGUID = '{00000001-0000-0000-C000-000000000046}';
  IID_ISharedProperty: TGUID = '{2A005C01-A5DE-11CF-9E66-00AA00A3F464}';
  CLASS_SharedProperty: TGUID = '{2A005C05-A5DE-11CF-9E66-00AA00A3F464}';
  IID_ISharedPropertyGroup: TGUID = '{2A005C07-A5DE-11CF-9E66-00AA00A3F464}';
  CLASS_SharedPropertyGroup: TGUID = '{2A005C0B-A5DE-11CF-9E66-00AA00A3F464}';
  IID_ISharedPropertyGroupManager: TGUID = '{2A005C0D-A5DE-11CF-9E66-00AA00A3F464}';
  CLASS_SharedPropertyGroupManager: TGUID = '{2A005C11-A5DE-11CF-9E66-00AA00A3F464}';
  IID_IMtsEvents: TGUID = '{BACEDF4D-74AB-11D0-B162-00AA00BA3258}';
  IID_IMtsEventInfo: TGUID = '{D56C3DC1-8482-11D0-B170-00AA00BA3258}';
  CLASS_COMEvents: TGUID = '{ECABB0AB-7F19-11D2-978E-0000F8757E2A}';
  IID_IMTSLocator: TGUID = '{D19B8BFD-7F88-11D0-B16E-00AA00BA3258}';
  CLASS_CoMTSLocator: TGUID = '{ECABB0AC-7F19-11D2-978E-0000F8757E2A}';
  IID_IMtsGrp: TGUID = '{4B2E958C-0393-11D1-B1AB-00AA00BA3258}';
  CLASS_MtsGrp: TGUID = '{4B2E958D-0393-11D1-B1AB-00AA00BA3258}';
  IID_IComThreadEvents: TGUID = '{683130A5-2E50-11D2-98A5-00C04F8EE1C4}';
  IID_IComUserEvent: TGUID = '{683130A4-2E50-11D2-98A5-00C04F8EE1C4}';
  IID_IComAppEvents: TGUID = '{683130A6-2E50-11D2-98A5-00C04F8EE1C4}';
  IID_IComInstanceEvents: TGUID = '{683130A7-2E50-11D2-98A5-00C04F8EE1C4}';
  IID_IComTransactionEvents: TGUID = '{683130A8-2E50-11D2-98A5-00C04F8EE1C4}';
  IID_IComMethodEvents: TGUID = '{683130A9-2E50-11D2-98A5-00C04F8EE1C4}';
  IID_IComObjectEvents: TGUID = '{683130AA-2E50-11D2-98A5-00C04F8EE1C4}';
  IID_IComResourceEvents: TGUID = '{683130AB-2E50-11D2-98A5-00C04F8EE1C4}';
  IID_IComSecurityEvents: TGUID = '{683130AC-2E50-11D2-98A5-00C04F8EE1C4}';
  IID_IComObjectPoolEvents: TGUID = '{683130AD-2E50-11D2-98A5-00C04F8EE1C4}';
  IID_IComObjectPoolEvents2: TGUID = '{683130AE-2E50-11D2-98A5-00C04F8EE1C4}';
  IID_IComObjectConstructionEvents: TGUID = '{683130AF-2E50-11D2-98A5-00C04F8EE1C4}';
  IID_IComActivityEvents: TGUID = '{683130B0-2E50-11D2-98A5-00C04F8EE1C4}';
  IID_IComIdentityEvents: TGUID = '{683130B1-2E50-11D2-98A5-00C04F8EE1C4}';
  IID_IComQCEvents: TGUID = '{683130B2-2E50-11D2-98A5-00C04F8EE1C4}';
  IID_IComExceptionEvents: TGUID = '{683130B3-2E50-11D2-98A5-00C04F8EE1C4}';
  IID_IComCRMEvents: TGUID = '{683130B5-2E50-11D2-98A5-00C04F8EE1C4}';
  IID_IComMethod2Events: TGUID = '{FB388AAA-567D-4024-AF8E-6E93EE748573}';
  IID_IComTrackingInfoEvents: TGUID = '{4E6CDCC9-FB25-4FD5-9CC5-C9F4B6559CEC}';
  IID_IComApp2Events: TGUID = '{1290BC1A-B219-418D-B078-5934DED08242}';
  IID_IComTransaction2Events: TGUID = '{A136F62A-2F94-4288-86E0-D8A1FA4C0299}';
  IID_IComInstance2Events: TGUID = '{20E3BF07-B506-4AD5-A50C-D2CA5B9C158E}';
  IID_IComObjectPool2Events: TGUID = '{65BF6534-85EA-4F64-8CF4-3D974B2AB1CF}';
  IID_IComObjectConstruction2Events: TGUID = '{4B5A7827-8DF2-45C0-8F6F-57EA1F856A9F}';
  IID_IComLTxEvents: TGUID = '{605CF82C-578E-4298-975D-82BABCD9E053}';
  CLASS_ComServiceEvents: TGUID = '{ECABB0C3-7F19-11D2-978E-0000F8757E2A}';
  IID_ISystemAppEventData: TGUID = '{D6D48A3C-D5C5-49E7-8C74-99E4889ED52F}';
  CLASS_ComSystemAppEventData: TGUID = '{ECABB0C6-7F19-11D2-978E-0000F8757E2A}';
  IID_ICrmMonitorLogRecords: TGUID = '{70C8E441-C7ED-11D1-82FB-00A0C91EEDE9}';
  CLASS_CRMClerk: TGUID = '{ECABB0BD-7F19-11D2-978E-0000F8757E2A}';
  IID_ICrmMonitor: TGUID = '{70C8E443-C7ED-11D1-82FB-00A0C91EEDE9}';
  CLASS_CRMRecoveryClerk: TGUID = '{ECABB0BE-7F19-11D2-978E-0000F8757E2A}';
  IID_ICrmMonitorClerks: TGUID = '{70C8E442-C7ED-11D1-82FB-00A0C91EEDE9}';
  IID_IMessageMover: TGUID = '{588A085A-B795-11D1-8054-00C04FC340EE}';
  CLASS_MessageMover: TGUID = '{ECABB0BF-7F19-11D2-978E-0000F8757E2A}';
  IID_IDispenserManager: TGUID = '{5CB31E10-2B5F-11CF-BE10-00AA00A2FA25}';
  IID_IDispenserManagerShutdownGuarantee: TGUID = '{5CB31E11-2B5F-11CF-BE10-00AA00A2FA25}';
  IID_IDispenserDriver: TGUID = '{208B3651-2B48-11CF-BE10-00AA00A2FA25}';
  IID_IHolder: TGUID = '{BF6A1850-2B45-11CF-BE10-00AA00A2FA25}';
  CLASS_DispenserManager: TGUID = '{ECABB0C0-7F19-11D2-978E-0000F8757E2A}';
  IID_IPoolManager: TGUID = '{0A469861-5A91-43A0-99B6-D5E179BB0631}';
  CLASS_PoolMgr: TGUID = '{ECABAFB5-7F19-11D2-978E-0000F8757E2A}';
  IID_IEventServerTrace: TGUID = '{9A9F12B8-80AF-47AB-A579-35EA57725370}';
  IID_IEventServer: TGUID = '{F1CB0608-EC04-11D1-93AE-00AA00BA3258}';
  IID_IEventServer2: TGUID = '{378F3CA7-BD24-481C-8DC3-5E5ECE1BCAD7}';
  CLASS_EventServer: TGUID = '{ECABAFBC-7F19-11D2-978E-0000F8757E2A}';
  IID_IReceiveAppData: TGUID = '{413DAFB0-BCF4-11D1-861D-0080C729264D}';
  IID_IGetAppData: TGUID = '{B60040E0-BCF3-11D1-861D-0080C729264D}';
  CLASS_TrackerServer: TGUID = '{ECABAFB9-7F19-11D2-978E-0000F8757E2A}';
  IID_IProcessDump: TGUID = '{23C9DD26-2355-4FE2-84DE-F779A238ADBD}';
  CLASS_ProcessDump: TGUID = '{ECABB0C4-7F19-11D2-978E-0000F8757E2A}';
  IID_IPersist: TGUID = '{0000010C-0000-0000-C000-000000000046}';
  IID_IPersistStream: TGUID = '{00000109-0000-0000-C000-000000000046}';
  IID_IMoniker: TGUID = '{0000000F-0000-0000-C000-000000000046}';
  CLASS_PartitionMoniker: TGUID = '{ECABB0C5-7F19-11D2-978E-0000F8757E2A}';
  IID_ISequentialStream: TGUID = '{0C733A30-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IStream: TGUID = '{0000000C-0000-0000-C000-000000000046}';
  IID_IBindCtx: TGUID = '{0000000E-0000-0000-C000-000000000046}';
  IID_IRunningObjectTable: TGUID = '{00000010-0000-0000-C000-000000000046}';
  IID_IEnumMoniker: TGUID = '{00000102-0000-0000-C000-000000000046}';
  IID_IEnumString: TGUID = '{00000101-0000-0000-C000-000000000046}';
  CLASS_SoapMoniker: TGUID = '{ECABB0C7-7F19-11D2-978E-0000F8757E2A}';
  IID_IAppDomainHelper: TGUID = '{C7B67079-8255-42C6-9EC0-6994A3548780}';
  CLASS_AppDomainHelper: TGUID = '{EF24F689-14F8-4D92-B4AF-D7B1F0E70FD4}';
  IID_IAssemblyLocator: TGUID = '{391FFBB9-A8EE-432A-ABC8-BAA238DAB90F}';
  CLASS_ClrAssemblyLocator: TGUID = '{458AA3B5-265A-4B75-BC05-9BEA4630CF18}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum tagTransactionVote
type
  tagTransactionVote = TOleEnum;
const
  TxCommit = $00000000;
  TxAbort = $00000001;

// Constants for enum __MIDL___MIDL_itf_autosvcs_0000_0129_0001
type
  __MIDL___MIDL_itf_autosvcs_0000_0129_0001 = TOleEnum;
const
  mtsErrCtxAborted = $8004E002;
  mtsErrCtxAborting = $8004E003;
  mtsErrCtxNoContext = $8004E004;
  mtsErrCtxNotRegistered = $8004E005;
  mtsErrCtxSynchTimeout = $8004E006;
  mtsErrCtxOldReference = $8004E007;
  mtsErrCtxRoleNotFound = $8004E00C;
  mtsErrCtxNoSecurity = $8004E00D;
  mtsErrCtxWrongThread = $8004E00E;
  mtsErrCtxTMNotAvailable = $8004E00F;
  comQCErrApplicationNotQueued = $80110600;
  comQCErrNoQueueableInterfaces = $80110601;
  comQCErrQueuingServiceNotAvailable = $80110602;
  comQCErrQueueTransactMismatch = $80110603;
  comqcErrRecorderMarshalled = $80110604;
  comqcErrOutParam = $80110605;
  comqcErrRecorderNotTrusted = $80110606;
  comqcErrPSLoad = $80110607;
  comqcErrMarshaledObjSameTxn = $80110608;
  comqcErrInvalidMessage = $80110650;
  comqcErrMsmqSidUnavailable = $80110651;
  comqcErrWrongMsgExtension = $80110652;
  comqcErrMsmqServiceUnavailable = $80110653;
  comqcErrMsgNotAuthenticated = $80110654;
  comqcErrMsmqConnectorUsed = $80110655;
  comqcErrBadMarshaledObject = $80110656;

// Constants for enum tagCSC_InheritanceConfig
type
  tagCSC_InheritanceConfig = TOleEnum;
const
  CSC_Inherit = $00000000;
  CSC_Ignore = $00000001;

// Constants for enum tagCSC_ThreadPool
type
  tagCSC_ThreadPool = TOleEnum;
const
  CSC_ThreadPoolNone = $00000000;
  CSC_ThreadPoolInherit = $00000001;
  CSC_STAThreadPool = $00000002;
  CSC_MTAThreadPool = $00000003;

// Constants for enum tagCSC_Binding
type
  tagCSC_Binding = TOleEnum;
const
  CSC_NoBinding = $00000000;
  CSC_BindToPoolThread = $00000001;

// Constants for enum tagCSC_TransactionConfig
type
  tagCSC_TransactionConfig = TOleEnum;
const
  CSC_NoTransaction = $00000000;
  CSC_IfContainerIsTransactional = $00000001;
  CSC_CreateTransactionIfNecessary = $00000002;
  CSC_NewTransaction = $00000003;

// Constants for enum COMAdminTxIsolationLevelOptions
type
  COMAdminTxIsolationLevelOptions = TOleEnum;
const
  COMAdminTxIsolationLevelAny = $00000000;
  COMAdminTxIsolationLevelReadUnCommitted = $00000001;
  COMAdminTxIsolationLevelReadCommitted = $00000002;
  COMAdminTxIsolationLevelRepeatableRead = $00000003;
  COMAdminTxIsolationLevelSerializable = $00000004;

// Constants for enum tagCSC_SynchronizationConfig
type
  tagCSC_SynchronizationConfig = TOleEnum;
const
  CSC_NoSynchronization = $00000000;
  CSC_IfContainerIsSynchronized = $00000001;
  CSC_NewSynchronizationIfNecessary = $00000002;
  CSC_NewSynchronization = $00000003;

// Constants for enum tagCSC_IISIntrinsicsConfig
type
  tagCSC_IISIntrinsicsConfig = TOleEnum;
const
  CSC_NoIISIntrinsics = $00000000;
  CSC_InheritIISIntrinsics = $00000001;

// Constants for enum tagCSC_COMTIIntrinsicsConfig
type
  tagCSC_COMTIIntrinsicsConfig = TOleEnum;
const
  CSC_NoCOMTIIntrinsics = $00000000;
  CSC_InheritCOMTIIntrinsics = $00000001;

// Constants for enum tagCSC_TrackerConfig
type
  tagCSC_TrackerConfig = TOleEnum;
const
  CSC_DontUseTracker = $00000000;
  CSC_UseTracker = $00000001;

// Constants for enum tagCSC_SxsConfig
type
  tagCSC_SxsConfig = TOleEnum;
const
  CSC_NoSxs = $00000000;
  CSC_InheritSxs = $00000001;
  CSC_NewSxs = $00000002;

// Constants for enum tagCSC_PartitionConfig
type
  tagCSC_PartitionConfig = TOleEnum;
const
  CSC_NoPartition = $00000000;
  CSC_InheritPartition = $00000001;
  CSC_NewPartition = $00000002;

// Constants for enum __MIDL___MIDL_itf_autosvcs_0000_0129_0002
type
  __MIDL___MIDL_itf_autosvcs_0000_0129_0002 = TOleEnum;
const
  LockSetGet = $00000000;
  LockMethod = $00000001;

// Constants for enum __MIDL___MIDL_itf_autosvcs_0000_0129_0003
type
  __MIDL___MIDL_itf_autosvcs_0000_0129_0003 = TOleEnum;
const
  Standard = $00000000;
  Process = $00000001;

// Constants for enum tagCRMFLAGS
type
  tagCRMFLAGS = TOleEnum;
const
  CRMFLAG_FORGETTARGET = $00000001;
  CRMFLAG_WRITTENDURINGPREPARE = $00000002;
  CRMFLAG_WRITTENDURINGCOMMIT = $00000004;
  CRMFLAG_WRITTENDURINGABORT = $00000008;
  CRMFLAG_WRITTENDURINGRECOVERY = $00000010;
  CRMFLAG_WRITTENDURINGREPLAY = $00000020;
  CRMFLAG_REPLAYINPROGRESS = $00000040;

// Constants for enum tagCRMREGFLAGS
type
  tagCRMREGFLAGS = TOleEnum;
const
  CRMREGFLAG_PREPAREPHASE = $00000001;
  CRMREGFLAG_COMMITPHASE = $00000002;
  CRMREGFLAG_ABORTPHASE = $00000004;
  CRMREGFLAG_ALLPHASES = $00000007;
  CRMREGFLAG_FAILIFINDOUBTSREMAIN = $00000010;

// Constants for enum tagCrmTransactionState
type
  tagCrmTransactionState = TOleEnum;
const
  TxState_Active = $00000000;
  TxState_Committed = $00000001;
  TxState_Aborted = $00000002;
  TxState_Indoubt = $00000003;

// Constants for enum tagCOMPLUS_APPTYPE
type
  tagCOMPLUS_APPTYPE = TOleEnum;
const
  APPTYPE_UNKNOWN = $FFFFFFFF;
  APPTYPE_SERVER = $00000001;
  APPTYPE_LIBRARY = $00000000;
  APPTYPE_SWC = $00000002;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ISecurityCertificateColl = interface;
  ISecurityCertificateCollDisp = dispinterface;
  ISecurityIdentityColl = interface;
  ISecurityIdentityCollDisp = dispinterface;
  ISecurityCallersColl = interface;
  ISecurityCallersCollDisp = dispinterface;
  ISecurityCallContext = interface;
  ISecurityCallContextDisp = dispinterface;
  IGetSecurityCallContext = interface;
  IGetSecurityCallContextDisp = dispinterface;
  IContextState = interface;
  IObjectContext = interface;
  IObjectContextActivity = interface;
  IObjectContextInfo = interface;
  IObjectConstruct = interface;
  IObjectConstructString = interface;
  IObjectConstructStringDisp = dispinterface;
  IObjectControl = interface;
  IObjectContextTip = interface;
  IPlaybackControl = interface;
  ISecurityProperty = interface;
  ICrmCompensator = interface;
  ICrmCompensatorVariants = interface;
  ICrmLogControl = interface;
  ICrmFormatLogRecords = interface;
  IServiceCall = interface;
  IServiceActivity = interface;
  ITransactionStatus = interface;
  ICheckSxsConfig = interface;
  IAsyncErrorNotify = interface;
  IProcessInitializer = interface;
  IManagedObjectInfo = interface;
  IManagedPooledObj = interface;
  ContextInfo = interface;
  ContextInfoDisp = dispinterface;
  ContextInfo2 = interface;
  ContextInfo2Disp = dispinterface;
  ObjectControl = interface;
  IMTxAS = interface;
  IMTxASDisp = dispinterface;
  ObjectContext = interface;
  ObjectContextDisp = dispinterface;
  SecurityProperty = interface;
  SecurityPropertyDisp = dispinterface;
  ITransactionContext = interface;
  ITransactionContextDisp = dispinterface;
  ITransactionContextEx = interface;
  ICreateWithTipTransactionEx = interface;
  ICreateWithTransactionEx = interface;
  ITransaction = interface;
  IServiceInheritanceConfig = interface;
  IServiceThreadPoolConfig = interface;
  IServiceTransactionConfigBase = interface;
  IServiceTransactionConfig = interface;
  IServiceSynchronizationConfig = interface;
  IServiceIISIntrinsicsConfig = interface;
  IServiceComTIIntrinsicsConfig = interface;
  IServiceTrackerConfig = interface;
  IServiceSxsConfig = interface;
  IServicePartitionConfig = interface;
  IServicePool = interface;
  IServicePoolConfig = interface;
  IClassFactory = interface;
  ISharedProperty = interface;
  ISharedPropertyDisp = dispinterface;
  ISharedPropertyGroup = interface;
  ISharedPropertyGroupDisp = dispinterface;
  ISharedPropertyGroupManager = interface;
  ISharedPropertyGroupManagerDisp = dispinterface;
  IMtsEvents = interface;
  IMtsEventsDisp = dispinterface;
  IMtsEventInfo = interface;
  IMtsEventInfoDisp = dispinterface;
  IMTSLocator = interface;
  IMTSLocatorDisp = dispinterface;
  IMtsGrp = interface;
  IMtsGrpDisp = dispinterface;
  IComThreadEvents = interface;
  IComUserEvent = interface;
  IComAppEvents = interface;
  IComInstanceEvents = interface;
  IComTransactionEvents = interface;
  IComMethodEvents = interface;
  IComObjectEvents = interface;
  IComResourceEvents = interface;
  IComSecurityEvents = interface;
  IComObjectPoolEvents = interface;
  IComObjectPoolEvents2 = interface;
  IComObjectConstructionEvents = interface;
  IComActivityEvents = interface;
  IComIdentityEvents = interface;
  IComQCEvents = interface;
  IComExceptionEvents = interface;
  IComCRMEvents = interface;
  IComMethod2Events = interface;
  IComTrackingInfoEvents = interface;
  IComApp2Events = interface;
  IComTransaction2Events = interface;
  IComInstance2Events = interface;
  IComObjectPool2Events = interface;
  IComObjectConstruction2Events = interface;
  IComLTxEvents = interface;
  ISystemAppEventData = interface;
  ICrmMonitorLogRecords = interface;
  ICrmMonitor = interface;
  ICrmMonitorClerks = interface;
  ICrmMonitorClerksDisp = dispinterface;
  IMessageMover = interface;
  IMessageMoverDisp = dispinterface;
  IDispenserManager = interface;
  IDispenserManagerShutdownGuarantee = interface;
  IDispenserDriver = interface;
  IHolder = interface;
  IPoolManager = interface;
  IEventServerTrace = interface;
  IEventServerTraceDisp = dispinterface;
  IEventServer = interface;
  IEventServer2 = interface;
  IReceiveAppData = interface;
  IGetAppData = interface;
  IProcessDump = interface;
  IProcessDumpDisp = dispinterface;
  IPersist = interface;
  IPersistStream = interface;
  IMoniker = interface;
  ISequentialStream = interface;
  IStream = interface;
  IBindCtx = interface;
  IRunningObjectTable = interface;
  IEnumMoniker = interface;
  IEnumString = interface;
  IAppDomainHelper = interface;
  IAssemblyLocator = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  SecurityCertificate = ISecurityCertificateColl;
  SecurityIdentity = ISecurityIdentityColl;
  SecurityCallers = ISecurityCallersColl;
  SecurityCallContext = ISecurityCallContext;
  GetSecurityCallContextAppObject = IGetSecurityCallContext;
  Dummy30040732 = IObjectContext;
  AppServer = IMTxAS;
  TransactionContext = ITransactionContext;
  TransactionContextEx = ITransactionContextEx;
  ByotServerEx = ICreateWithTipTransactionEx;
  CServiceConfig = IServiceInheritanceConfig;
  ServicePool = IServicePool;
  ServicePoolConfig = IServicePoolConfig;
  SharedProperty = ISharedProperty;
  SharedPropertyGroup = ISharedPropertyGroup;
  SharedPropertyGroupManager = ISharedPropertyGroupManager;
  COMEvents = IMtsEvents;
  CoMTSLocator = IMTSLocator;
  MtsGrp = IMtsGrp;
  ComServiceEvents = IComThreadEvents;
  ComSystemAppEventData = ISystemAppEventData;
  CRMClerk = ICrmLogControl;
  CRMRecoveryClerk = ICrmMonitor;
  MessageMover = IMessageMover;
  DispenserManager = IDispenserManager;
  PoolMgr = IPoolManager;
  EventServer = IEventServerTrace;
  TrackerServer = IReceiveAppData;
  ProcessDump = IProcessDump;
  PartitionMoniker = IMoniker;
  SoapMoniker = IMoniker;
  AppDomainHelper = IAppDomainHelper;
  ClrAssemblyLocator = IAssemblyLocator;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  POleVariant1 = ^OleVariant; {*}
  PUserType1 = ^TGUID; {*}
  PIUnknown1 = ^IUnknown; {*}
  PPPrivateAlias1 = ^Pointer; {*}
  PUserType2 = ^tagBLOB; {*}
  PHResult1 = ^HResult; {*}
  PUserType3 = ^BOID; {*}
  PUserType4 = ^COMSVCSEVENTINFO; {*}
  PByte1 = ^Byte; {*}
  PUserType5 = ^RECYCLE_INFO; {*}
  PUserType6 = ^CLSIDDATA2; {*}
  PUserType7 = ^appData; {*}
  PUserType8 = ^CLSIDDATA; {*}
  PUserType9 = ^tagBIND_OPTS2; {*}
  PUserType10 = ^_FILETIME; {*}

  tagBLOB = packed record
    cbSize: LongWord;
    pBlobData: ^Byte;
  end;

  tagCrmLogRecordRead = packed record
    dwCrmFlags: LongWord;
    dwSequenceNumber: LongWord;
    blobUserData: tagBLOB;
  end;

  Error_Constants = __MIDL___MIDL_itf_autosvcs_0000_0129_0001; 

  BOID = packed record
    rgb: array[0..15] of Byte;
  end;

  XACTTRANSINFO = packed record
    uow: BOID;
    isoLevel: Integer;
    isoFlags: LongWord;
    grfTCSupported: LongWord;
    grfRMSupported: LongWord;
    grfTCSupportedRetaining: LongWord;
    grfRMSupportedRetaining: LongWord;
  end;

  LockModes = __MIDL___MIDL_itf_autosvcs_0000_0129_0002; 
  ReleaseModes = __MIDL___MIDL_itf_autosvcs_0000_0129_0003; 

  __MIDL___MIDL_itf_autosvcs_0000_0015_0001 = packed record
    cbSize: LongWord;
    dwPid: LongWord;
    lTime: Int64;
    lMicroTime: Integer;
    perfCount: Int64;
    guidApp: TGUID;
    sMachineName: PWideChar;
  end;

  COMSVCSEVENTINFO = __MIDL___MIDL_itf_autosvcs_0000_0015_0001; 
  ULONG_PTR = LongWord; 

  _RECYCLE_INFO = packed record
    guidCombaseProcessIdentifier: TGUID;
    ProcessStartTime: Int64;
    dwRecycleLifetimeLimit: LongWord;
    dwRecycleMemoryLimit: LongWord;
    dwRecycleExpirationTimeout: LongWord;
  end;

  RECYCLE_INFO = _RECYCLE_INFO; 

  CAppStatistics = packed record
    m_cTotalCalls: LongWord;
    m_cTotalInstances: LongWord;
    m_cTotalClasses: LongWord;
    m_cCallsPerSecond: LongWord;
  end;

  APPSTATISTICS = CAppStatistics; 

  CCLSIDData2 = packed record
    m_clsid: TGUID;
    m_appid: TGUID;
    m_partid: TGUID;
    m_pwszAppName: PWideChar;
    m_pwszCtxName: PWideChar;
    m_eAppType: tagCOMPLUS_APPTYPE;
    m_cReferences: LongWord;
    m_cBound: LongWord;
    m_cPooled: LongWord;
    m_cInCall: LongWord;
    m_dwRespTime: LongWord;
    m_cCallsCompleted: LongWord;
    m_cCallsFailed: LongWord;
  end;

  CLSIDDATA2 = CCLSIDData2; 

  CAppData = packed record
    m_idApp: LongWord;
    m_szAppGuid: array[0..39] of Word;
    m_dwAppProcessId: LongWord;
    m_AppStatistics: CAppStatistics;
  end;

  appData = CAppData; 

  CCLSIDData = packed record
    m_clsid: TGUID;
    m_cReferences: LongWord;
    m_cBound: LongWord;
    m_cPooled: LongWord;
    m_cInCall: LongWord;
    m_dwRespTime: LongWord;
    m_cCallsCompleted: LongWord;
    m_cCallsFailed: LongWord;
  end;

  CLSIDDATA = CCLSIDData; 

  _LARGE_INTEGER = packed record
    QuadPart: Int64;
  end;

  _ULARGE_INTEGER = packed record
    QuadPart: Largeuint;
  end;

  _FILETIME = packed record
    dwLowDateTime: LongWord;
    dwHighDateTime: LongWord;
  end;

  tagSTATSTG = packed record
    pwcsName: PWideChar;
    type_: LongWord;
    cbSize: _ULARGE_INTEGER;
    mtime: _FILETIME;
    ctime: _FILETIME;
    atime: _FILETIME;
    grfMode: LongWord;
    grfLocksSupported: LongWord;
    clsid: TGUID;
    grfStateBits: LongWord;
    reserved: LongWord;
  end;

  _COAUTHIDENTITY = packed record
    User: ^Word;
    UserLength: LongWord;
    Domain: ^Word;
    DomainLength: LongWord;
    Password: ^Word;
    PasswordLength: LongWord;
    Flags: LongWord;
  end;

  _COAUTHINFO = packed record
    dwAuthnSvc: LongWord;
    dwAuthzSvc: LongWord;
    pwszServerPrincName: PWideChar;
    dwAuthnLevel: LongWord;
    dwImpersonationLevel: LongWord;
    pAuthIdentityData: ^_COAUTHIDENTITY;
    dwCapabilities: LongWord;
  end;

  _COSERVERINFO = packed record
    dwReserved1: LongWord;
    pwszName: PWideChar;
    pAuthInfo: ^_COAUTHINFO;
    dwReserved2: LongWord;
  end;

  tagBIND_OPTS2 = packed record
    cbStruct: LongWord;
    grfFlags: LongWord;
    grfMode: LongWord;
    dwTickCountDeadline: LongWord;
    dwTrackFlags: LongWord;
    dwClassContext: LongWord;
    locale: LongWord;
    pServerInfo: ^_COSERVERINFO;
  end;

  PrivateAlias1 = array[0..59] of Word; {*}

// *********************************************************************//
// Interface: ISecurityCertificateColl
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CAFC823B-B441-11D1-B82B-0000F8757E2A}
// *********************************************************************//
  ISecurityCertificateColl = interface(IDispatch)
    ['{CAFC823B-B441-11D1-B82B-0000F8757E2A}']
    function Get_Count: Integer; safecall;
    function Get_Item(const name: WideString): OleVariant; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[const name: WideString]: OleVariant read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ISecurityCertificateCollDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CAFC823B-B441-11D1-B82B-0000F8757E2A}
// *********************************************************************//
  ISecurityCertificateCollDisp = dispinterface
    ['{CAFC823B-B441-11D1-B82B-0000F8757E2A}']
    property Count: Integer readonly dispid 1610743808;
    property Item[const name: WideString]: OleVariant readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ISecurityIdentityColl
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CAFC823C-B441-11D1-B82B-0000F8757E2A}
// *********************************************************************//
  ISecurityIdentityColl = interface(IDispatch)
    ['{CAFC823C-B441-11D1-B82B-0000F8757E2A}']
    function Get_Count: Integer; safecall;
    function Get_Item(const name: WideString): OleVariant; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[const name: WideString]: OleVariant read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ISecurityIdentityCollDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CAFC823C-B441-11D1-B82B-0000F8757E2A}
// *********************************************************************//
  ISecurityIdentityCollDisp = dispinterface
    ['{CAFC823C-B441-11D1-B82B-0000F8757E2A}']
    property Count: Integer readonly dispid 1610743808;
    property Item[const name: WideString]: OleVariant readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ISecurityCallersColl
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CAFC823D-B441-11D1-B82B-0000F8757E2A}
// *********************************************************************//
  ISecurityCallersColl = interface(IDispatch)
    ['{CAFC823D-B441-11D1-B82B-0000F8757E2A}']
    function Get_Count: Integer; safecall;
    function Get_Item(lIndex: Integer): ISecurityIdentityColl; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[lIndex: Integer]: ISecurityIdentityColl read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ISecurityCallersCollDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CAFC823D-B441-11D1-B82B-0000F8757E2A}
// *********************************************************************//
  ISecurityCallersCollDisp = dispinterface
    ['{CAFC823D-B441-11D1-B82B-0000F8757E2A}']
    property Count: Integer readonly dispid 1610743808;
    property Item[lIndex: Integer]: ISecurityIdentityColl readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ISecurityCallContext
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CAFC823E-B441-11D1-B82B-0000F8757E2A}
// *********************************************************************//
  ISecurityCallContext = interface(IDispatch)
    ['{CAFC823E-B441-11D1-B82B-0000F8757E2A}']
    function Get_Count: Integer; safecall;
    function Get_Item(const name: WideString): OleVariant; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function IsCallerInRole(const bstrRole: WideString): WordBool; safecall;
    function IsSecurityEnabled: WordBool; safecall;
    function IsUserInRole(var pUser: OleVariant; const bstrRole: WideString): WordBool; safecall;
    property Count: Integer read Get_Count;
    property Item[const name: WideString]: OleVariant read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ISecurityCallContextDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CAFC823E-B441-11D1-B82B-0000F8757E2A}
// *********************************************************************//
  ISecurityCallContextDisp = dispinterface
    ['{CAFC823E-B441-11D1-B82B-0000F8757E2A}']
    property Count: Integer readonly dispid 1610743813;
    property Item[const name: WideString]: OleVariant readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
    function IsCallerInRole(const bstrRole: WideString): WordBool; dispid 1610743814;
    function IsSecurityEnabled: WordBool; dispid 1610743815;
    function IsUserInRole(var pUser: OleVariant; const bstrRole: WideString): WordBool; dispid 1610743816;
  end;

// *********************************************************************//
// Interface: IGetSecurityCallContext
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CAFC823F-B441-11D1-B82B-0000F8757E2A}
// *********************************************************************//
  IGetSecurityCallContext = interface(IDispatch)
    ['{CAFC823F-B441-11D1-B82B-0000F8757E2A}']
    function GetSecurityCallContext: ISecurityCallContext; safecall;
  end;

// *********************************************************************//
// DispIntf:  IGetSecurityCallContextDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CAFC823F-B441-11D1-B82B-0000F8757E2A}
// *********************************************************************//
  IGetSecurityCallContextDisp = dispinterface
    ['{CAFC823F-B441-11D1-B82B-0000F8757E2A}']
    function GetSecurityCallContext: ISecurityCallContext; dispid 1610743808;
  end;

// *********************************************************************//
// Interface: IContextState
// Flags:     (0)
// GUID:      {3C05E54B-A42A-11D2-AFC4-00C04F8EE1C4}
// *********************************************************************//
  IContextState = interface(IUnknown)
    ['{3C05E54B-A42A-11D2-AFC4-00C04F8EE1C4}']
    function SetDeactivateOnReturn(bDeactivate: WordBool): HResult; stdcall;
    function GetDeactivateOnReturn(out pbDeactivate: WordBool): HResult; stdcall;
    function SetMyTransactionVote(txVote: tagTransactionVote): HResult; stdcall;
    function GetMyTransactionVote(out ptxVote: tagTransactionVote): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IObjectContext
// Flags:     (0)
// GUID:      {51372AE0-CAE7-11CF-BE81-00AA00A2FA25}
// *********************************************************************//
  IObjectContext = interface(IUnknown)
    ['{51372AE0-CAE7-11CF-BE81-00AA00A2FA25}']
    function CreateInstance(var rclsid: TGUID; var riid: TGUID; out ppv: Pointer): HResult; stdcall;
    function SetComplete: HResult; stdcall;
    function SetAbort: HResult; stdcall;
    function EnableCommit: HResult; stdcall;
    function DisableCommit: HResult; stdcall;
    function IsInTransaction: Integer; stdcall;
    function IsSecurityEnabled: Integer; stdcall;
    function IsCallerInRole(const bstrRole: WideString; out pfIsInRole: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IObjectContextActivity
// Flags:     (0)
// GUID:      {51372AFC-CAE7-11CF-BE81-00AA00A2FA25}
// *********************************************************************//
  IObjectContextActivity = interface(IUnknown)
    ['{51372AFC-CAE7-11CF-BE81-00AA00A2FA25}']
    function GetActivityId(out pGUID: TGUID): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IObjectContextInfo
// Flags:     (0)
// GUID:      {75B52DDB-E8ED-11D1-93AD-00AA00BA3258}
// *********************************************************************//
  IObjectContextInfo = interface(IUnknown)
    ['{75B52DDB-E8ED-11D1-93AD-00AA00BA3258}']
    function IsInTransaction: Integer; stdcall;
    function GetTransaction(var pptrans: IUnknown): HResult; stdcall;
    function GetTransactionId(out pGUID: TGUID): HResult; stdcall;
    function GetActivityId(out pGUID: TGUID): HResult; stdcall;
    function GetContextId(out pGUID: TGUID): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IObjectConstruct
// Flags:     (0)
// GUID:      {41C4F8B3-7439-11D2-98CB-00C04F8EE1C4}
// *********************************************************************//
  IObjectConstruct = interface(IUnknown)
    ['{41C4F8B3-7439-11D2-98CB-00C04F8EE1C4}']
    function Construct(const pCtorObj: IDispatch): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IObjectConstructString
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41C4F8B2-7439-11D2-98CB-00C04F8EE1C4}
// *********************************************************************//
  IObjectConstructString = interface(IDispatch)
    ['{41C4F8B2-7439-11D2-98CB-00C04F8EE1C4}']
    function Get_ConstructString: WideString; safecall;
    property ConstructString: WideString read Get_ConstructString;
  end;

// *********************************************************************//
// DispIntf:  IObjectConstructStringDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41C4F8B2-7439-11D2-98CB-00C04F8EE1C4}
// *********************************************************************//
  IObjectConstructStringDisp = dispinterface
    ['{41C4F8B2-7439-11D2-98CB-00C04F8EE1C4}']
    property ConstructString: WideString readonly dispid 1;
  end;

// *********************************************************************//
// Interface: IObjectControl
// Flags:     (0)
// GUID:      {51372AEC-CAE7-11CF-BE81-00AA00A2FA25}
// *********************************************************************//
  IObjectControl = interface(IUnknown)
    ['{51372AEC-CAE7-11CF-BE81-00AA00A2FA25}']
    function Activate: HResult; stdcall;
    procedure Deactivate; stdcall;
    function CanBePooled: Integer; stdcall;
  end;

// *********************************************************************//
// Interface: IObjectContextTip
// Flags:     (0)
// GUID:      {92FD41CA-BAD9-11D2-9A2D-00C04F797BC9}
// *********************************************************************//
  IObjectContextTip = interface(IUnknown)
    ['{92FD41CA-BAD9-11D2-9A2D-00C04F797BC9}']
    function GetTipUrl(out pTipUrl: WideString): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IPlaybackControl
// Flags:     (0)
// GUID:      {51372AFD-CAE7-11CF-BE81-00AA00A2FA25}
// *********************************************************************//
  IPlaybackControl = interface(IUnknown)
    ['{51372AFD-CAE7-11CF-BE81-00AA00A2FA25}']
    function FinalClientRetry: HResult; stdcall;
    function FinalServerRetry: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISecurityProperty
// Flags:     (0)
// GUID:      {51372AEA-CAE7-11CF-BE81-00AA00A2FA25}
// *********************************************************************//
  ISecurityProperty = interface(IUnknown)
    ['{51372AEA-CAE7-11CF-BE81-00AA00A2FA25}']
    function GetDirectCreatorSID(pSID: PPPrivateAlias1): HResult; stdcall;
    function GetOriginalCreatorSID(pSID: PPPrivateAlias1): HResult; stdcall;
    function GetDirectCallerSID(pSID: PPPrivateAlias1): HResult; stdcall;
    function GetOriginalCallerSID(pSID: PPPrivateAlias1): HResult; stdcall;
    function ReleaseSID(var pSID: Pointer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICrmCompensator
// Flags:     (0)
// GUID:      {BBC01830-8D3B-11D1-82EC-00A0C91EEDE9}
// *********************************************************************//
  ICrmCompensator = interface(IUnknown)
    ['{BBC01830-8D3B-11D1-82EC-00A0C91EEDE9}']
    function SetLogControl(const pLogControl: ICrmLogControl): HResult; stdcall;
    function BeginPrepare: HResult; stdcall;
    function PrepareRecord(crmLogRec: tagCrmLogRecordRead; out pfForget: Integer): HResult; stdcall;
    function EndPrepare(out pfOkToPrepare: Integer): HResult; stdcall;
    function BeginCommit(fRecovery: Integer): HResult; stdcall;
    function CommitRecord(crmLogRec: tagCrmLogRecordRead; out pfForget: Integer): HResult; stdcall;
    function EndCommit: HResult; stdcall;
    function BeginAbort(fRecovery: Integer): HResult; stdcall;
    function AbortRecord(crmLogRec: tagCrmLogRecordRead; out pfForget: Integer): HResult; stdcall;
    function EndAbort: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICrmCompensatorVariants
// Flags:     (0)
// GUID:      {F0BAF8E4-7804-11D1-82E9-00A0C91EEDE9}
// *********************************************************************//
  ICrmCompensatorVariants = interface(IUnknown)
    ['{F0BAF8E4-7804-11D1-82E9-00A0C91EEDE9}']
    function SetLogControlVariants(const pLogControl: ICrmLogControl): HResult; stdcall;
    function BeginPrepareVariants: HResult; stdcall;
    function PrepareRecordVariants(var pLogRecord: OleVariant; out pbForget: WordBool): HResult; stdcall;
    function EndPrepareVariants(out pbOkToPrepare: WordBool): HResult; stdcall;
    function BeginCommitVariants(bRecovery: WordBool): HResult; stdcall;
    function CommitRecordVariants(var pLogRecord: OleVariant; out pbForget: WordBool): HResult; stdcall;
    function EndCommitVariants: HResult; stdcall;
    function BeginAbortVariants(bRecovery: WordBool): HResult; stdcall;
    function AbortRecordVariants(var pLogRecord: OleVariant; out pbForget: WordBool): HResult; stdcall;
    function EndAbortVariants: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICrmLogControl
// Flags:     (0)
// GUID:      {A0E174B3-D26E-11D2-8F84-00805FC7BCD9}
// *********************************************************************//
  ICrmLogControl = interface(IUnknown)
    ['{A0E174B3-D26E-11D2-8F84-00805FC7BCD9}']
    function Get_TransactionUOW(out pVal: WideString): HResult; stdcall;
    function RegisterCompensator(lpcwstrProgIdCompensator: PWideChar; 
                                 lpcwstrDescription: PWideChar; lCrmRegFlags: Integer): HResult; stdcall;
    function WriteLogRecordVariants(var pLogRecord: OleVariant): HResult; stdcall;
    function ForceLog: HResult; stdcall;
    function ForgetLogRecord: HResult; stdcall;
    function ForceTransactionToAbort: HResult; stdcall;
    function WriteLogRecord(var rgBlob: tagBLOB; cBlob: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICrmFormatLogRecords
// Flags:     (0)
// GUID:      {9C51D821-C98B-11D1-82FB-00A0C91EEDE9}
// *********************************************************************//
  ICrmFormatLogRecords = interface(IUnknown)
    ['{9C51D821-C98B-11D1-82FB-00A0C91EEDE9}']
    function GetColumnCount(out plColumnCount: Integer): HResult; stdcall;
    function GetColumnHeaders(out pHeaders: OleVariant): HResult; stdcall;
    function GetColumn(crmLogRec: tagCrmLogRecordRead; out pFormattedLogRecord: OleVariant): HResult; stdcall;
    function GetColumnVariants(LogRecord: OleVariant; out pFormattedLogRecord: OleVariant): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IServiceCall
// Flags:     (0)
// GUID:      {BD3E2E12-42DD-40F4-A09A-95A50C58304B}
// *********************************************************************//
  IServiceCall = interface(IUnknown)
    ['{BD3E2E12-42DD-40F4-A09A-95A50C58304B}']
    function OnCall: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IServiceActivity
// Flags:     (0)
// GUID:      {67532E0C-9E2F-4450-A354-035633944E17}
// *********************************************************************//
  IServiceActivity = interface(IUnknown)
    ['{67532E0C-9E2F-4450-A354-035633944E17}']
    function SynchronousCall(const pIServiceCall: IServiceCall): HResult; stdcall;
    function AsynchronousCall(const pIServiceCall: IServiceCall): HResult; stdcall;
    function BindToCurrentThread: HResult; stdcall;
    function UnbindFromThread: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITransactionStatus
// Flags:     (0)
// GUID:      {61F589E8-3724-4898-A0A4-664AE9E1D1B4}
// *********************************************************************//
  ITransactionStatus = interface(IUnknown)
    ['{61F589E8-3724-4898-A0A4-664AE9E1D1B4}']
    function SetTransactionStatus(hrStatus: HResult): HResult; stdcall;
    function GetTransactionStatus(var pHrStatus: HResult): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICheckSxsConfig
// Flags:     (0)
// GUID:      {0FF5A96F-11FC-47D1-BAA6-25DD347E7242}
// *********************************************************************//
  ICheckSxsConfig = interface(IUnknown)
    ['{0FF5A96F-11FC-47D1-BAA6-25DD347E7242}']
    function IsSameSxsConfig(wszSxsName: PWideChar; wszSxsDirectory: PWideChar; 
                             wszSxsAppName: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IAsyncErrorNotify
// Flags:     (0)
// GUID:      {FE6777FB-A674-4177-8F32-6D707E113484}
// *********************************************************************//
  IAsyncErrorNotify = interface(IUnknown)
    ['{FE6777FB-A674-4177-8F32-6D707E113484}']
    function OnError(hr: HResult): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IProcessInitializer
// Flags:     (256) OleAutomation
// GUID:      {1113F52D-DC7F-4943-AED6-88D04027E32A}
// *********************************************************************//
  IProcessInitializer = interface(IUnknown)
    ['{1113F52D-DC7F-4943-AED6-88D04027E32A}']
    function Startup(const punkProcessControl: IUnknown): HResult; stdcall;
    function Shutdown: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IManagedObjectInfo
// Flags:     (0)
// GUID:      {1427C51A-4584-49D8-90A0-C50D8086CBE9}
// *********************************************************************//
  IManagedObjectInfo = interface(IUnknown)
    ['{1427C51A-4584-49D8-90A0-C50D8086CBE9}']
    function GetIUnknown(out pUnk: IUnknown): HResult; stdcall;
    function GetIObjectControl(out pCtrl: IObjectControl): HResult; stdcall;
    function SetInPool(bInPool: Integer; const pPooledObj: IManagedPooledObj): HResult; stdcall;
    function SetWrapperStrength(bStrong: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IManagedPooledObj
// Flags:     (0)
// GUID:      {C5DA4BEA-1B42-4437-8926-B6A38860A770}
// *********************************************************************//
  IManagedPooledObj = interface(IUnknown)
    ['{C5DA4BEA-1B42-4437-8926-B6A38860A770}']
    function SetHeld(m_bHeld: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ContextInfo
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {19A5A02C-0AC8-11D2-B286-00C04F8EF934}
// *********************************************************************//
  ContextInfo = interface(IDispatch)
    ['{19A5A02C-0AC8-11D2-B286-00C04F8EF934}']
    function IsInTransaction: WordBool; safecall;
    function GetTransaction: IUnknown; safecall;
    function GetTransactionId: WideString; safecall;
    function GetActivityId: WideString; safecall;
    function GetContextId: WideString; safecall;
  end;

// *********************************************************************//
// DispIntf:  ContextInfoDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {19A5A02C-0AC8-11D2-B286-00C04F8EF934}
// *********************************************************************//
  ContextInfoDisp = dispinterface
    ['{19A5A02C-0AC8-11D2-B286-00C04F8EF934}']
    function IsInTransaction: WordBool; dispid 100671488;
    function GetTransaction: IUnknown; dispid 1610743811;
    function GetTransactionId: WideString; dispid 1610743812;
    function GetActivityId: WideString; dispid 1610743813;
    function GetContextId: WideString; dispid 1610743814;
  end;

// *********************************************************************//
// Interface: ContextInfo2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C99D6E75-2375-11D4-8331-00C04F605588}
// *********************************************************************//
  ContextInfo2 = interface(ContextInfo)
    ['{C99D6E75-2375-11D4-8331-00C04F605588}']
    function GetPartitionId: WideString; safecall;
    function GetApplicationId: WideString; safecall;
    function GetApplicationInstanceId: WideString; safecall;
  end;

// *********************************************************************//
// DispIntf:  ContextInfo2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C99D6E75-2375-11D4-8331-00C04F605588}
// *********************************************************************//
  ContextInfo2Disp = dispinterface
    ['{C99D6E75-2375-11D4-8331-00C04F605588}']
    function GetPartitionId: WideString; dispid 1610743815;
    function GetApplicationId: WideString; dispid 1610743816;
    function GetApplicationInstanceId: WideString; dispid 1610743817;
    function IsInTransaction: WordBool; dispid 100671488;
    function GetTransaction: IUnknown; dispid 1610743811;
    function GetTransactionId: WideString; dispid 1610743812;
    function GetActivityId: WideString; dispid 1610743813;
    function GetContextId: WideString; dispid 1610743814;
  end;

// *********************************************************************//
// Interface: ObjectControl
// Flags:     (256) OleAutomation
// GUID:      {7DC41850-0C31-11D0-8B79-00AA00B8A790}
// *********************************************************************//
  ObjectControl = interface(IUnknown)
    ['{7DC41850-0C31-11D0-8B79-00AA00B8A790}']
    function Activate: HResult; stdcall;
    function Deactivate: HResult; stdcall;
    function CanBePooled(out pbPoolable: WordBool): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMTxAS
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {74C08641-CEDB-11CF-8B49-00AA00B8A790}
// *********************************************************************//
  IMTxAS = interface(IDispatch)
    ['{74C08641-CEDB-11CF-8B49-00AA00B8A790}']
    function GetObjectContext: ObjectContext; safecall;
    function SafeRef(vRefIn: OleVariant): OleVariant; safecall;
    procedure RecycleSurrogate(lReasonCode: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IMTxASDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {74C08641-CEDB-11CF-8B49-00AA00B8A790}
// *********************************************************************//
  IMTxASDisp = dispinterface
    ['{74C08641-CEDB-11CF-8B49-00AA00B8A790}']
    function GetObjectContext: ObjectContext; dispid 1610743808;
    function SafeRef(vRefIn: OleVariant): OleVariant; dispid 1610743809;
    procedure RecycleSurrogate(lReasonCode: Integer); dispid 1610743810;
  end;

// *********************************************************************//
// Interface: ObjectContext
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {74C08646-CEDB-11CF-8B49-00AA00B8A790}
// *********************************************************************//
  ObjectContext = interface(IDispatch)
    ['{74C08646-CEDB-11CF-8B49-00AA00B8A790}']
    function CreateInstance(const bstrProgID: WideString): OleVariant; safecall;
    procedure SetComplete; safecall;
    procedure SetAbort; safecall;
    procedure EnableCommit; safecall;
    procedure DisableCommit; safecall;
    function IsInTransaction: WordBool; safecall;
    function IsSecurityEnabled: WordBool; safecall;
    function IsCallerInRole(const bstrRole: WideString): WordBool; safecall;
    function Get_Count: Integer; safecall;
    function Get_Item(const name: WideString): OleVariant; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_Security: SecurityProperty; safecall;
    function Get_ContextInfo: ContextInfo; safecall;
    property Count: Integer read Get_Count;
    property Item[const name: WideString]: OleVariant read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Security: SecurityProperty read Get_Security;
    property ContextInfo: ContextInfo read Get_ContextInfo;
  end;

// *********************************************************************//
// DispIntf:  ObjectContextDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {74C08646-CEDB-11CF-8B49-00AA00B8A790}
// *********************************************************************//
  ObjectContextDisp = dispinterface
    ['{74C08646-CEDB-11CF-8B49-00AA00B8A790}']
    function CreateInstance(const bstrProgID: WideString): OleVariant; dispid 1610743808;
    procedure SetComplete; dispid 1610743809;
    procedure SetAbort; dispid 1610743810;
    procedure EnableCommit; dispid 1610743811;
    procedure DisableCommit; dispid 1610743812;
    function IsInTransaction: WordBool; dispid 1610743813;
    function IsSecurityEnabled: WordBool; dispid 1610743814;
    function IsCallerInRole(const bstrRole: WideString): WordBool; dispid 1610743815;
    property Count: Integer readonly dispid 1610743840;
    property Item[const name: WideString]: OleVariant readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
    property Security: SecurityProperty readonly dispid 1610743843;
    property ContextInfo: ContextInfo readonly dispid 1610743844;
  end;

// *********************************************************************//
// Interface: SecurityProperty
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E74A7215-014D-11D1-A63C-00A0C911B4E0}
// *********************************************************************//
  SecurityProperty = interface(IDispatch)
    ['{E74A7215-014D-11D1-A63C-00A0C911B4E0}']
    function GetDirectCallerName: WideString; safecall;
    function GetDirectCreatorName: WideString; safecall;
    function GetOriginalCallerName: WideString; safecall;
    function GetOriginalCreatorName: WideString; safecall;
  end;

// *********************************************************************//
// DispIntf:  SecurityPropertyDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E74A7215-014D-11D1-A63C-00A0C911B4E0}
// *********************************************************************//
  SecurityPropertyDisp = dispinterface
    ['{E74A7215-014D-11D1-A63C-00A0C911B4E0}']
    function GetDirectCallerName: WideString; dispid 1610743808;
    function GetDirectCreatorName: WideString; dispid 1610743809;
    function GetOriginalCallerName: WideString; dispid 1610743810;
    function GetOriginalCreatorName: WideString; dispid 1610743811;
  end;

// *********************************************************************//
// Interface: ITransactionContext
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7999FC21-D3C6-11CF-ACAB-00A024A55AEF}
// *********************************************************************//
  ITransactionContext = interface(IDispatch)
    ['{7999FC21-D3C6-11CF-ACAB-00A024A55AEF}']
    function CreateInstance(const pszProgId: WideString): OleVariant; safecall;
    procedure Commit; safecall;
    procedure Abort; safecall;
  end;

// *********************************************************************//
// DispIntf:  ITransactionContextDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7999FC21-D3C6-11CF-ACAB-00A024A55AEF}
// *********************************************************************//
  ITransactionContextDisp = dispinterface
    ['{7999FC21-D3C6-11CF-ACAB-00A024A55AEF}']
    function CreateInstance(const pszProgId: WideString): OleVariant; dispid 0;
    procedure Commit; dispid 1;
    procedure Abort; dispid 2;
  end;

// *********************************************************************//
// Interface: ITransactionContextEx
// Flags:     (0)
// GUID:      {7999FC22-D3C6-11CF-ACAB-00A024A55AEF}
// *********************************************************************//
  ITransactionContextEx = interface(IUnknown)
    ['{7999FC22-D3C6-11CF-ACAB-00A024A55AEF}']
    function CreateInstance(var rclsid: TGUID; var riid: TGUID; out pObject: Pointer): HResult; stdcall;
    function Commit: HResult; stdcall;
    function Abort: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICreateWithTipTransactionEx
// Flags:     (0)
// GUID:      {455ACF59-5345-11D2-99CF-00C04F797BC9}
// *********************************************************************//
  ICreateWithTipTransactionEx = interface(IUnknown)
    ['{455ACF59-5345-11D2-99CF-00C04F797BC9}']
    function CreateInstance(const bstrTipUrl: WideString; var rclsid: TGUID; var riid: TGUID; 
                            out pObject: Pointer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICreateWithTransactionEx
// Flags:     (0)
// GUID:      {455ACF57-5345-11D2-99CF-00C04F797BC9}
// *********************************************************************//
  ICreateWithTransactionEx = interface(IUnknown)
    ['{455ACF57-5345-11D2-99CF-00C04F797BC9}']
    function CreateInstance(const pTransaction: ITransaction; var rclsid: TGUID; var riid: TGUID; 
                            out pObject: Pointer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITransaction
// Flags:     (0)
// GUID:      {0FB15084-AF41-11CE-BD2B-204C4F4F5020}
// *********************************************************************//
  ITransaction = interface(IUnknown)
    ['{0FB15084-AF41-11CE-BD2B-204C4F4F5020}']
    function Commit(fRetaining: Integer; grfTC: LongWord; grfRM: LongWord): HResult; stdcall;
    function Abort(var pboidReason: BOID; fRetaining: Integer; fAsync: Integer): HResult; stdcall;
    function GetTransactionInfo(out pinfo: XACTTRANSINFO): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IServiceInheritanceConfig
// Flags:     (0)
// GUID:      {92186771-D3B4-4D77-A8EA-EE842D586F35}
// *********************************************************************//
  IServiceInheritanceConfig = interface(IUnknown)
    ['{92186771-D3B4-4D77-A8EA-EE842D586F35}']
    function ContainingContextTreatment(inheritanceConfig: tagCSC_InheritanceConfig): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IServiceThreadPoolConfig
// Flags:     (0)
// GUID:      {186D89BC-F277-4BCC-80D5-4DF7B836EF4A}
// *********************************************************************//
  IServiceThreadPoolConfig = interface(IUnknown)
    ['{186D89BC-F277-4BCC-80D5-4DF7B836EF4A}']
    function SelectThreadPool(threadPool: tagCSC_ThreadPool): HResult; stdcall;
    function SetBindingInfo(binding: tagCSC_Binding): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IServiceTransactionConfigBase
// Flags:     (0)
// GUID:      {772B3FBE-6FFD-42FB-B5F8-8F9B260F3810}
// *********************************************************************//
  IServiceTransactionConfigBase = interface(IUnknown)
    ['{772B3FBE-6FFD-42FB-B5F8-8F9B260F3810}']
    function ConfigureTransaction(transactionConfig: tagCSC_TransactionConfig): HResult; stdcall;
    function IsolationLevel(option: COMAdminTxIsolationLevelOptions): HResult; stdcall;
    function TransactionTimeout(ulTimeoutSec: LongWord): HResult; stdcall;
    function BringYourOwnTransaction(szTipURL: PWideChar): HResult; stdcall;
    function NewTransactionDescription(szTxDesc: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IServiceTransactionConfig
// Flags:     (0)
// GUID:      {59F4C2A3-D3D7-4A31-B6E4-6AB3177C50B9}
// *********************************************************************//
  IServiceTransactionConfig = interface(IServiceTransactionConfigBase)
    ['{59F4C2A3-D3D7-4A31-B6E4-6AB3177C50B9}']
    function ConfigureBYOT(const pITxByot: ITransaction): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IServiceSynchronizationConfig
// Flags:     (0)
// GUID:      {FD880E81-6DCE-4C58-AF83-A208846C0030}
// *********************************************************************//
  IServiceSynchronizationConfig = interface(IUnknown)
    ['{FD880E81-6DCE-4C58-AF83-A208846C0030}']
    function ConfigureSynchronization(synchConfig: tagCSC_SynchronizationConfig): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IServiceIISIntrinsicsConfig
// Flags:     (0)
// GUID:      {1A0CF920-D452-46F4-BC36-48118D54EA52}
// *********************************************************************//
  IServiceIISIntrinsicsConfig = interface(IUnknown)
    ['{1A0CF920-D452-46F4-BC36-48118D54EA52}']
    function IISIntrinsicsConfig(IISIntrinsicsConfig: tagCSC_IISIntrinsicsConfig): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IServiceComTIIntrinsicsConfig
// Flags:     (0)
// GUID:      {09E6831E-04E1-4ED4-9D0F-E8B168BAFEAF}
// *********************************************************************//
  IServiceComTIIntrinsicsConfig = interface(IUnknown)
    ['{09E6831E-04E1-4ED4-9D0F-E8B168BAFEAF}']
    function ComTIIntrinsicsConfig(ComTIIntrinsicsConfig: tagCSC_COMTIIntrinsicsConfig): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IServiceTrackerConfig
// Flags:     (0)
// GUID:      {6C3A3E1D-0BA6-4036-B76F-D0404DB816C9}
// *********************************************************************//
  IServiceTrackerConfig = interface(IUnknown)
    ['{6C3A3E1D-0BA6-4036-B76F-D0404DB816C9}']
    function TrackerConfig(TrackerConfig: tagCSC_TrackerConfig; szTrackerAppName: PWideChar; 
                           szTrackerCtxName: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IServiceSxsConfig
// Flags:     (0)
// GUID:      {C7CD7379-F3F2-4634-811B-703281D73E08}
// *********************************************************************//
  IServiceSxsConfig = interface(IUnknown)
    ['{C7CD7379-F3F2-4634-811B-703281D73E08}']
    function SxsConfig(scsConfig: tagCSC_SxsConfig): HResult; stdcall;
    function SxsName(szSxsName: PWideChar): HResult; stdcall;
    function SxsDirectory(szSxsDirectory: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IServicePartitionConfig
// Flags:     (0)
// GUID:      {80182D03-5EA4-4831-AE97-55BEFFC2E590}
// *********************************************************************//
  IServicePartitionConfig = interface(IUnknown)
    ['{80182D03-5EA4-4831-AE97-55BEFFC2E590}']
    function PartitionConfig(PartitionConfig: tagCSC_PartitionConfig): HResult; stdcall;
    function PartitionID(var guidPartitionID: TGUID): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IServicePool
// Flags:     (0)
// GUID:      {B302DF81-EA45-451E-99A2-09F9FD1B1E13}
// *********************************************************************//
  IServicePool = interface(IUnknown)
    ['{B302DF81-EA45-451E-99A2-09F9FD1B1E13}']
    function Initialize(const pPoolConfig: IUnknown): HResult; stdcall;
    function GetObject(var riid: TGUID; out ppv: Pointer): HResult; stdcall;
    function Shutdown: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IServicePoolConfig
// Flags:     (0)
// GUID:      {A9690656-5BCA-470C-8451-250C1F43A33E}
// *********************************************************************//
  IServicePoolConfig = interface(IUnknown)
    ['{A9690656-5BCA-470C-8451-250C1F43A33E}']
    function Set_MaxPoolSize(pdwMaxPool: LongWord): HResult; stdcall;
    function Get_MaxPoolSize(out pdwMaxPool: LongWord): HResult; stdcall;
    function Set_MinPoolSize(pdwMinPool: LongWord): HResult; stdcall;
    function Get_MinPoolSize(out pdwMinPool: LongWord): HResult; stdcall;
    function Set_CreationTimeout(pdwCreationTimeout: LongWord): HResult; stdcall;
    function Get_CreationTimeout(out pdwCreationTimeout: LongWord): HResult; stdcall;
    function Set_TransactionAffinity(pfTxAffinity: Integer): HResult; stdcall;
    function Get_TransactionAffinity(out pfTxAffinity: Integer): HResult; stdcall;
    function Set_ClassFactory(const pFactory: IClassFactory): HResult; stdcall;
    function Get_ClassFactory(out pFactory: IClassFactory): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IClassFactory
// Flags:     (0)
// GUID:      {00000001-0000-0000-C000-000000000046}
// *********************************************************************//
  IClassFactory = interface(IUnknown)
    ['{00000001-0000-0000-C000-000000000046}']
    function RemoteCreateInstance(var riid: TGUID; out ppvObject: IUnknown): HResult; stdcall;
    function RemoteLockServer(fLock: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISharedProperty
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2A005C01-A5DE-11CF-9E66-00AA00A3F464}
// *********************************************************************//
  ISharedProperty = interface(IDispatch)
    ['{2A005C01-A5DE-11CF-9E66-00AA00A3F464}']
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(pVal: OleVariant); safecall;
    property Value: OleVariant read Get_Value write Set_Value;
  end;

// *********************************************************************//
// DispIntf:  ISharedPropertyDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2A005C01-A5DE-11CF-9E66-00AA00A3F464}
// *********************************************************************//
  ISharedPropertyDisp = dispinterface
    ['{2A005C01-A5DE-11CF-9E66-00AA00A3F464}']
    property Value: OleVariant dispid 0;
  end;

// *********************************************************************//
// Interface: ISharedPropertyGroup
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2A005C07-A5DE-11CF-9E66-00AA00A3F464}
// *********************************************************************//
  ISharedPropertyGroup = interface(IDispatch)
    ['{2A005C07-A5DE-11CF-9E66-00AA00A3F464}']
    function CreatePropertyByPosition(Index: SYSINT; out fExists: WordBool): ISharedProperty; safecall;
    function Get_PropertyByPosition(Index: SYSINT): ISharedProperty; safecall;
    function CreateProperty(const name: WideString; out fExists: WordBool): ISharedProperty; safecall;
    function Get_Property_(const name: WideString): ISharedProperty; safecall;
    property PropertyByPosition[Index: SYSINT]: ISharedProperty read Get_PropertyByPosition;
    property Property_[const name: WideString]: ISharedProperty read Get_Property_;
  end;

// *********************************************************************//
// DispIntf:  ISharedPropertyGroupDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2A005C07-A5DE-11CF-9E66-00AA00A3F464}
// *********************************************************************//
  ISharedPropertyGroupDisp = dispinterface
    ['{2A005C07-A5DE-11CF-9E66-00AA00A3F464}']
    function CreatePropertyByPosition(Index: SYSINT; out fExists: WordBool): ISharedProperty; dispid 1;
    property PropertyByPosition[Index: SYSINT]: ISharedProperty readonly dispid 2;
    function CreateProperty(const name: WideString; out fExists: WordBool): ISharedProperty; dispid 3;
    property Property_[const name: WideString]: ISharedProperty readonly dispid 4;
  end;

// *********************************************************************//
// Interface: ISharedPropertyGroupManager
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2A005C0D-A5DE-11CF-9E66-00AA00A3F464}
// *********************************************************************//
  ISharedPropertyGroupManager = interface(IDispatch)
    ['{2A005C0D-A5DE-11CF-9E66-00AA00A3F464}']
    function CreatePropertyGroup(const name: WideString; var dwIsoMode: Integer; 
                                 var dwRelMode: Integer; out fExists: WordBool): ISharedPropertyGroup; safecall;
    function Get_Group(const name: WideString): ISharedPropertyGroup; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Group[const name: WideString]: ISharedPropertyGroup read Get_Group;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ISharedPropertyGroupManagerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2A005C0D-A5DE-11CF-9E66-00AA00A3F464}
// *********************************************************************//
  ISharedPropertyGroupManagerDisp = dispinterface
    ['{2A005C0D-A5DE-11CF-9E66-00AA00A3F464}']
    function CreatePropertyGroup(const name: WideString; var dwIsoMode: Integer; 
                                 var dwRelMode: Integer; out fExists: WordBool): ISharedPropertyGroup; dispid 1;
    property Group[const name: WideString]: ISharedPropertyGroup readonly dispid 2;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IMtsEvents
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BACEDF4D-74AB-11D0-B162-00AA00BA3258}
// *********************************************************************//
  IMtsEvents = interface(IDispatch)
    ['{BACEDF4D-74AB-11D0-B162-00AA00BA3258}']
    function Get_PackageName: WideString; safecall;
    function Get_PackageGuid: WideString; safecall;
    procedure PostEvent(var vEvent: OleVariant); safecall;
    function Get_FireEvents: WordBool; safecall;
    function GetProcessID: Integer; safecall;
    property PackageName: WideString read Get_PackageName;
    property PackageGuid: WideString read Get_PackageGuid;
    property FireEvents: WordBool read Get_FireEvents;
  end;

// *********************************************************************//
// DispIntf:  IMtsEventsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BACEDF4D-74AB-11D0-B162-00AA00BA3258}
// *********************************************************************//
  IMtsEventsDisp = dispinterface
    ['{BACEDF4D-74AB-11D0-B162-00AA00BA3258}']
    property PackageName: WideString readonly dispid 1;
    property PackageGuid: WideString readonly dispid 2;
    procedure PostEvent(var vEvent: OleVariant); dispid 5;
    property FireEvents: WordBool readonly dispid 6;
    function GetProcessID: Integer; dispid 7;
  end;

// *********************************************************************//
// Interface: IMtsEventInfo
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D56C3DC1-8482-11D0-B170-00AA00BA3258}
// *********************************************************************//
  IMtsEventInfo = interface(IDispatch)
    ['{D56C3DC1-8482-11D0-B170-00AA00BA3258}']
    function Get_Names: IUnknown; safecall;
    function Get_DisplayName: WideString; safecall;
    function Get_EventID: WideString; safecall;
    function Get_Count: Integer; safecall;
    function Get_Value(const sKey: WideString): OleVariant; safecall;
    property Names: IUnknown read Get_Names;
    property DisplayName: WideString read Get_DisplayName;
    property EventID: WideString read Get_EventID;
    property Count: Integer read Get_Count;
    property Value[const sKey: WideString]: OleVariant read Get_Value;
  end;

// *********************************************************************//
// DispIntf:  IMtsEventInfoDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D56C3DC1-8482-11D0-B170-00AA00BA3258}
// *********************************************************************//
  IMtsEventInfoDisp = dispinterface
    ['{D56C3DC1-8482-11D0-B170-00AA00BA3258}']
    property Names: IUnknown readonly dispid 1610743808;
    property DisplayName: WideString readonly dispid 1;
    property EventID: WideString readonly dispid 1610743810;
    property Count: Integer readonly dispid 1610743811;
    property Value[const sKey: WideString]: OleVariant readonly dispid 1610743812;
  end;

// *********************************************************************//
// Interface: IMTSLocator
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D19B8BFD-7F88-11D0-B16E-00AA00BA3258}
// *********************************************************************//
  IMTSLocator = interface(IDispatch)
    ['{D19B8BFD-7F88-11D0-B16E-00AA00BA3258}']
    function GetEventDispatcher: IUnknown; safecall;
  end;

// *********************************************************************//
// DispIntf:  IMTSLocatorDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D19B8BFD-7F88-11D0-B16E-00AA00BA3258}
// *********************************************************************//
  IMTSLocatorDisp = dispinterface
    ['{D19B8BFD-7F88-11D0-B16E-00AA00BA3258}']
    function GetEventDispatcher: IUnknown; dispid 1;
  end;

// *********************************************************************//
// Interface: IMtsGrp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4B2E958C-0393-11D1-B1AB-00AA00BA3258}
// *********************************************************************//
  IMtsGrp = interface(IDispatch)
    ['{4B2E958C-0393-11D1-B1AB-00AA00BA3258}']
    function Get_Count: Integer; safecall;
    procedure Item(lIndex: Integer; out ppUnkDispatcher: IUnknown); safecall;
    procedure Refresh; safecall;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IMtsGrpDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4B2E958C-0393-11D1-B1AB-00AA00BA3258}
// *********************************************************************//
  IMtsGrpDisp = dispinterface
    ['{4B2E958C-0393-11D1-B1AB-00AA00BA3258}']
    property Count: Integer readonly dispid 1;
    procedure Item(lIndex: Integer; out ppUnkDispatcher: IUnknown); dispid 2;
    procedure Refresh; dispid 3;
  end;

// *********************************************************************//
// Interface: IComThreadEvents
// Flags:     (16) Hidden
// GUID:      {683130A5-2E50-11D2-98A5-00C04F8EE1C4}
// *********************************************************************//
  IComThreadEvents = interface(IUnknown)
    ['{683130A5-2E50-11D2-98A5-00C04F8EE1C4}']
    function OnThreadStart(var pinfo: COMSVCSEVENTINFO; ThreadID: Largeuint; dwThread: LongWord; 
                           dwTheadCnt: LongWord): HResult; stdcall;
    function OnThreadTerminate(var pinfo: COMSVCSEVENTINFO; ThreadID: Largeuint; 
                               dwThread: LongWord; dwTheadCnt: LongWord): HResult; stdcall;
    function OnThreadBindToApartment(var pinfo: COMSVCSEVENTINFO; ThreadID: Largeuint; 
                                     AptID: Largeuint; dwActCnt: LongWord; dwLowCnt: LongWord): HResult; stdcall;
    function OnThreadUnBind(var pinfo: COMSVCSEVENTINFO; ThreadID: Largeuint; AptID: Largeuint; 
                            dwActCnt: LongWord): HResult; stdcall;
    function OnThreadWorkEnque(var pinfo: COMSVCSEVENTINFO; ThreadID: Largeuint; 
                               MsgWorkID: Largeuint; QueueLen: LongWord): HResult; stdcall;
    function OnThreadWorkPrivate(var pinfo: COMSVCSEVENTINFO; ThreadID: Largeuint; 
                                 MsgWorkID: Largeuint): HResult; stdcall;
    function OnThreadWorkPublic(var pinfo: COMSVCSEVENTINFO; ThreadID: Largeuint; 
                                MsgWorkID: Largeuint; QueueLen: LongWord): HResult; stdcall;
    function OnThreadWorkRedirect(var pinfo: COMSVCSEVENTINFO; ThreadID: Largeuint; 
                                  MsgWorkID: Largeuint; QueueLen: LongWord; ThreadNum: Largeuint): HResult; stdcall;
    function OnThreadWorkReject(var pinfo: COMSVCSEVENTINFO; ThreadID: Largeuint; 
                                MsgWorkID: Largeuint; QueueLen: LongWord): HResult; stdcall;
    function OnThreadAssignApartment(var pinfo: COMSVCSEVENTINFO; var guidActivity: TGUID; 
                                     AptID: Largeuint): HResult; stdcall;
    function OnThreadUnassignApartment(var pinfo: COMSVCSEVENTINFO; AptID: Largeuint): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComUserEvent
// Flags:     (16) Hidden
// GUID:      {683130A4-2E50-11D2-98A5-00C04F8EE1C4}
// *********************************************************************//
  IComUserEvent = interface(IUnknown)
    ['{683130A4-2E50-11D2-98A5-00C04F8EE1C4}']
    function OnUserEvent(var pinfo: COMSVCSEVENTINFO; var pvarEvent: OleVariant): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComAppEvents
// Flags:     (16) Hidden
// GUID:      {683130A6-2E50-11D2-98A5-00C04F8EE1C4}
// *********************************************************************//
  IComAppEvents = interface(IUnknown)
    ['{683130A6-2E50-11D2-98A5-00C04F8EE1C4}']
    function OnAppActivation(var pinfo: COMSVCSEVENTINFO; guidApp: TGUID): HResult; stdcall;
    function OnAppShutdown(var pinfo: COMSVCSEVENTINFO; guidApp: TGUID): HResult; stdcall;
    function OnAppForceShutdown(var pinfo: COMSVCSEVENTINFO; guidApp: TGUID): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComInstanceEvents
// Flags:     (16) Hidden
// GUID:      {683130A7-2E50-11D2-98A5-00C04F8EE1C4}
// *********************************************************************//
  IComInstanceEvents = interface(IUnknown)
    ['{683130A7-2E50-11D2-98A5-00C04F8EE1C4}']
    function OnObjectCreate(var pinfo: COMSVCSEVENTINFO; var guidActivity: TGUID; var clsid: TGUID; 
                            var tsid: TGUID; CtxtID: Largeuint; ObjectID: Largeuint): HResult; stdcall;
    function OnObjectDestroy(var pinfo: COMSVCSEVENTINFO; CtxtID: Largeuint): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComTransactionEvents
// Flags:     (16) Hidden
// GUID:      {683130A8-2E50-11D2-98A5-00C04F8EE1C4}
// *********************************************************************//
  IComTransactionEvents = interface(IUnknown)
    ['{683130A8-2E50-11D2-98A5-00C04F8EE1C4}']
    function OnTransactionStart(var pinfo: COMSVCSEVENTINFO; var guidTx: TGUID; var tsid: TGUID; 
                                fRoot: Integer): HResult; stdcall;
    function OnTransactionPrepare(var pinfo: COMSVCSEVENTINFO; var guidTx: TGUID; fVoteYes: Integer): HResult; stdcall;
    function OnTransactionAbort(var pinfo: COMSVCSEVENTINFO; var guidTx: TGUID): HResult; stdcall;
    function OnTransactionCommit(var pinfo: COMSVCSEVENTINFO; var guidTx: TGUID): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComMethodEvents
// Flags:     (16) Hidden
// GUID:      {683130A9-2E50-11D2-98A5-00C04F8EE1C4}
// *********************************************************************//
  IComMethodEvents = interface(IUnknown)
    ['{683130A9-2E50-11D2-98A5-00C04F8EE1C4}']
    function OnMethodCall(var pinfo: COMSVCSEVENTINFO; oid: Largeuint; var guidCid: TGUID; 
                          var guidRid: TGUID; iMeth: LongWord): HResult; stdcall;
    function OnMethodReturn(var pinfo: COMSVCSEVENTINFO; oid: Largeuint; var guidCid: TGUID; 
                            var guidRid: TGUID; iMeth: LongWord; hresult: HResult): HResult; stdcall;
    function OnMethodException(var pinfo: COMSVCSEVENTINFO; oid: Largeuint; var guidCid: TGUID; 
                               var guidRid: TGUID; iMeth: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComObjectEvents
// Flags:     (16) Hidden
// GUID:      {683130AA-2E50-11D2-98A5-00C04F8EE1C4}
// *********************************************************************//
  IComObjectEvents = interface(IUnknown)
    ['{683130AA-2E50-11D2-98A5-00C04F8EE1C4}']
    function OnObjectActivate(var pinfo: COMSVCSEVENTINFO; CtxtID: Largeuint; ObjectID: Largeuint): HResult; stdcall;
    function OnObjectDeactivate(var pinfo: COMSVCSEVENTINFO; CtxtID: Largeuint; ObjectID: Largeuint): HResult; stdcall;
    function OnDisableCommit(var pinfo: COMSVCSEVENTINFO; CtxtID: Largeuint): HResult; stdcall;
    function OnEnableCommit(var pinfo: COMSVCSEVENTINFO; CtxtID: Largeuint): HResult; stdcall;
    function OnSetComplete(var pinfo: COMSVCSEVENTINFO; CtxtID: Largeuint): HResult; stdcall;
    function OnSetAbort(var pinfo: COMSVCSEVENTINFO; CtxtID: Largeuint): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComResourceEvents
// Flags:     (16) Hidden
// GUID:      {683130AB-2E50-11D2-98A5-00C04F8EE1C4}
// *********************************************************************//
  IComResourceEvents = interface(IUnknown)
    ['{683130AB-2E50-11D2-98A5-00C04F8EE1C4}']
    function OnResourceCreate(var pinfo: COMSVCSEVENTINFO; ObjectID: Largeuint; pszType: PWideChar; 
                              resId: Largeuint; enlisted: Integer): HResult; stdcall;
    function OnResourceAllocate(var pinfo: COMSVCSEVENTINFO; ObjectID: Largeuint; 
                                pszType: PWideChar; resId: Largeuint; enlisted: Integer; 
                                NumRated: LongWord; Rating: LongWord): HResult; stdcall;
    function OnResourceRecycle(var pinfo: COMSVCSEVENTINFO; ObjectID: Largeuint; 
                               pszType: PWideChar; resId: Largeuint): HResult; stdcall;
    function OnResourceDestroy(var pinfo: COMSVCSEVENTINFO; ObjectID: Largeuint; hr: HResult; 
                               pszType: PWideChar; resId: Largeuint): HResult; stdcall;
    function OnResourceTrack(var pinfo: COMSVCSEVENTINFO; ObjectID: Largeuint; pszType: PWideChar; 
                             resId: Largeuint; enlisted: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComSecurityEvents
// Flags:     (16) Hidden
// GUID:      {683130AC-2E50-11D2-98A5-00C04F8EE1C4}
// *********************************************************************//
  IComSecurityEvents = interface(IUnknown)
    ['{683130AC-2E50-11D2-98A5-00C04F8EE1C4}']
    function OnAuthenticate(var pinfo: COMSVCSEVENTINFO; var guidActivity: TGUID; 
                            ObjectID: Largeuint; var guidIID: TGUID; iMeth: LongWord; 
                            cbByteOrig: LongWord; var pSidOriginalUser: Byte; cbByteCur: LongWord; 
                            var pSidCurrentUser: Byte; bCurrentUserInpersonatingInProc: Integer): HResult; stdcall;
    function OnAuthenticateFail(var pinfo: COMSVCSEVENTINFO; var guidActivity: TGUID; 
                                ObjectID: Largeuint; var guidIID: TGUID; iMeth: LongWord; 
                                cbByteOrig: LongWord; var pSidOriginalUser: Byte; 
                                cbByteCur: LongWord; var pSidCurrentUser: Byte; 
                                bCurrentUserInpersonatingInProc: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComObjectPoolEvents
// Flags:     (16) Hidden
// GUID:      {683130AD-2E50-11D2-98A5-00C04F8EE1C4}
// *********************************************************************//
  IComObjectPoolEvents = interface(IUnknown)
    ['{683130AD-2E50-11D2-98A5-00C04F8EE1C4}']
    function OnObjPoolPutObject(var pinfo: COMSVCSEVENTINFO; var guidObject: TGUID; 
                                nReason: SYSINT; dwAvailable: LongWord; oid: Largeuint): HResult; stdcall;
    function OnObjPoolGetObject(var pinfo: COMSVCSEVENTINFO; var guidActivity: TGUID; 
                                var guidObject: TGUID; dwAvailable: LongWord; oid: Largeuint): HResult; stdcall;
    function OnObjPoolRecycleToTx(var pinfo: COMSVCSEVENTINFO; var guidActivity: TGUID; 
                                  var guidObject: TGUID; var guidTx: TGUID; objid: Largeuint): HResult; stdcall;
    function OnObjPoolGetFromTx(var pinfo: COMSVCSEVENTINFO; var guidActivity: TGUID; 
                                var guidObject: TGUID; var guidTx: TGUID; objid: Largeuint): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComObjectPoolEvents2
// Flags:     (16) Hidden
// GUID:      {683130AE-2E50-11D2-98A5-00C04F8EE1C4}
// *********************************************************************//
  IComObjectPoolEvents2 = interface(IUnknown)
    ['{683130AE-2E50-11D2-98A5-00C04F8EE1C4}']
    function OnObjPoolCreateObject(var pinfo: COMSVCSEVENTINFO; var guidObject: TGUID; 
                                   dwObjsCreated: LongWord; oid: Largeuint): HResult; stdcall;
    function OnObjPoolDestroyObject(var pinfo: COMSVCSEVENTINFO; var guidObject: TGUID; 
                                    dwObjsCreated: LongWord; oid: Largeuint): HResult; stdcall;
    function OnObjPoolCreateDecision(var pinfo: COMSVCSEVENTINFO; dwThreadsWaiting: LongWord; 
                                     dwAvail: LongWord; dwCreated: LongWord; dwMin: LongWord; 
                                     dwMax: LongWord): HResult; stdcall;
    function OnObjPoolTimeout(var pinfo: COMSVCSEVENTINFO; var guidObject: TGUID; 
                              var guidActivity: TGUID; dwTimeout: LongWord): HResult; stdcall;
    function OnObjPoolCreatePool(var pinfo: COMSVCSEVENTINFO; var guidObject: TGUID; 
                                 dwMin: LongWord; dwMax: LongWord; dwTimeout: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComObjectConstructionEvents
// Flags:     (16) Hidden
// GUID:      {683130AF-2E50-11D2-98A5-00C04F8EE1C4}
// *********************************************************************//
  IComObjectConstructionEvents = interface(IUnknown)
    ['{683130AF-2E50-11D2-98A5-00C04F8EE1C4}']
    function OnObjectConstruct(var pinfo: COMSVCSEVENTINFO; var guidObject: TGUID; 
                               sConstructString: PWideChar; oid: Largeuint): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComActivityEvents
// Flags:     (16) Hidden
// GUID:      {683130B0-2E50-11D2-98A5-00C04F8EE1C4}
// *********************************************************************//
  IComActivityEvents = interface(IUnknown)
    ['{683130B0-2E50-11D2-98A5-00C04F8EE1C4}']
    function OnActivityCreate(var pinfo: COMSVCSEVENTINFO; var guidActivity: TGUID): HResult; stdcall;
    function OnActivityDestroy(var pinfo: COMSVCSEVENTINFO; var guidActivity: TGUID): HResult; stdcall;
    function OnActivityEnter(var pinfo: COMSVCSEVENTINFO; var guidCurrent: TGUID; 
                             var guidEntered: TGUID; dwThread: LongWord): HResult; stdcall;
    function OnActivityTimeout(var pinfo: COMSVCSEVENTINFO; var guidCurrent: TGUID; 
                               var guidEntered: TGUID; dwThread: LongWord; dwTimeout: LongWord): HResult; stdcall;
    function OnActivityReenter(var pinfo: COMSVCSEVENTINFO; var guidCurrent: TGUID; 
                               dwThread: LongWord; dwCallDepth: LongWord): HResult; stdcall;
    function OnActivityLeave(var pinfo: COMSVCSEVENTINFO; var guidCurrent: TGUID; 
                             var guidLeft: TGUID): HResult; stdcall;
    function OnActivityLeaveSame(var pinfo: COMSVCSEVENTINFO; var guidCurrent: TGUID; 
                                 dwCallDepth: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComIdentityEvents
// Flags:     (16) Hidden
// GUID:      {683130B1-2E50-11D2-98A5-00C04F8EE1C4}
// *********************************************************************//
  IComIdentityEvents = interface(IUnknown)
    ['{683130B1-2E50-11D2-98A5-00C04F8EE1C4}']
    function OnIISRequestInfo(var pinfo: COMSVCSEVENTINFO; objid: Largeuint; 
                              pszClientIP: PWideChar; pszServerIP: PWideChar; pszURL: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComQCEvents
// Flags:     (16) Hidden
// GUID:      {683130B2-2E50-11D2-98A5-00C04F8EE1C4}
// *********************************************************************//
  IComQCEvents = interface(IUnknown)
    ['{683130B2-2E50-11D2-98A5-00C04F8EE1C4}']
    function OnQCRecord(var pinfo: COMSVCSEVENTINFO; objid: Largeuint; szQueue: PrivateAlias1; 
                        var guidMsgId: TGUID; var guidWorkFlowId: TGUID; msmqhr: HResult): HResult; stdcall;
    function OnQCQueueOpen(var pinfo: COMSVCSEVENTINFO; szQueue: PrivateAlias1; QueueID: Largeuint; 
                           hr: HResult): HResult; stdcall;
    function OnQCReceive(var pinfo: COMSVCSEVENTINFO; QueueID: Largeuint; var guidMsgId: TGUID; 
                         var guidWorkFlowId: TGUID; hr: HResult): HResult; stdcall;
    function OnQCReceiveFail(var pinfo: COMSVCSEVENTINFO; QueueID: Largeuint; msmqhr: HResult): HResult; stdcall;
    function OnQCMoveToReTryQueue(var pinfo: COMSVCSEVENTINFO; var guidMsgId: TGUID; 
                                  var guidWorkFlowId: TGUID; RetryIndex: LongWord): HResult; stdcall;
    function OnQCMoveToDeadQueue(var pinfo: COMSVCSEVENTINFO; var guidMsgId: TGUID; 
                                 var guidWorkFlowId: TGUID): HResult; stdcall;
    function OnQCPlayback(var pinfo: COMSVCSEVENTINFO; objid: Largeuint; var guidMsgId: TGUID; 
                          var guidWorkFlowId: TGUID; hr: HResult): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComExceptionEvents
// Flags:     (16) Hidden
// GUID:      {683130B3-2E50-11D2-98A5-00C04F8EE1C4}
// *********************************************************************//
  IComExceptionEvents = interface(IUnknown)
    ['{683130B3-2E50-11D2-98A5-00C04F8EE1C4}']
    function OnExceptionUser(var pinfo: COMSVCSEVENTINFO; code: LongWord; address: Largeuint; 
                             pszStackTrace: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComCRMEvents
// Flags:     (16) Hidden
// GUID:      {683130B5-2E50-11D2-98A5-00C04F8EE1C4}
// *********************************************************************//
  IComCRMEvents = interface(IUnknown)
    ['{683130B5-2E50-11D2-98A5-00C04F8EE1C4}']
    function OnCRMRecoveryStart(var pinfo: COMSVCSEVENTINFO; guidApp: TGUID): HResult; stdcall;
    function OnCRMRecoveryDone(var pinfo: COMSVCSEVENTINFO; guidApp: TGUID): HResult; stdcall;
    function OnCRMCheckpoint(var pinfo: COMSVCSEVENTINFO; guidApp: TGUID): HResult; stdcall;
    function OnCRMBegin(var pinfo: COMSVCSEVENTINFO; guidClerkCLSID: TGUID; guidActivity: TGUID; 
                        guidTx: TGUID; szProgIdCompensator: PrivateAlias1; 
                        szDescription: PrivateAlias1): HResult; stdcall;
    function OnCRMPrepare(var pinfo: COMSVCSEVENTINFO; guidClerkCLSID: TGUID): HResult; stdcall;
    function OnCRMCommit(var pinfo: COMSVCSEVENTINFO; guidClerkCLSID: TGUID): HResult; stdcall;
    function OnCRMAbort(var pinfo: COMSVCSEVENTINFO; guidClerkCLSID: TGUID): HResult; stdcall;
    function OnCRMIndoubt(var pinfo: COMSVCSEVENTINFO; guidClerkCLSID: TGUID): HResult; stdcall;
    function OnCRMDone(var pinfo: COMSVCSEVENTINFO; guidClerkCLSID: TGUID): HResult; stdcall;
    function OnCRMRelease(var pinfo: COMSVCSEVENTINFO; guidClerkCLSID: TGUID): HResult; stdcall;
    function OnCRMAnalyze(var pinfo: COMSVCSEVENTINFO; guidClerkCLSID: TGUID; 
                          dwCrmRecordType: LongWord; dwRecordSize: LongWord): HResult; stdcall;
    function OnCRMWrite(var pinfo: COMSVCSEVENTINFO; guidClerkCLSID: TGUID; fVariants: Integer; 
                        dwRecordSize: LongWord): HResult; stdcall;
    function OnCRMForget(var pinfo: COMSVCSEVENTINFO; guidClerkCLSID: TGUID): HResult; stdcall;
    function OnCRMForce(var pinfo: COMSVCSEVENTINFO; guidClerkCLSID: TGUID): HResult; stdcall;
    function OnCRMDeliver(var pinfo: COMSVCSEVENTINFO; guidClerkCLSID: TGUID; fVariants: Integer; 
                          dwRecordSize: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComMethod2Events
// Flags:     (16) Hidden
// GUID:      {FB388AAA-567D-4024-AF8E-6E93EE748573}
// *********************************************************************//
  IComMethod2Events = interface(IUnknown)
    ['{FB388AAA-567D-4024-AF8E-6E93EE748573}']
    function OnMethodCall2(var pinfo: COMSVCSEVENTINFO; oid: Largeuint; var guidCid: TGUID; 
                           var guidRid: TGUID; dwThread: LongWord; iMeth: LongWord): HResult; stdcall;
    function OnMethodReturn2(var pinfo: COMSVCSEVENTINFO; oid: Largeuint; var guidCid: TGUID; 
                             var guidRid: TGUID; dwThread: LongWord; iMeth: LongWord; 
                             hresult: HResult): HResult; stdcall;
    function OnMethodException2(var pinfo: COMSVCSEVENTINFO; oid: Largeuint; var guidCid: TGUID; 
                                var guidRid: TGUID; dwThread: LongWord; iMeth: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComTrackingInfoEvents
// Flags:     (16) Hidden
// GUID:      {4E6CDCC9-FB25-4FD5-9CC5-C9F4B6559CEC}
// *********************************************************************//
  IComTrackingInfoEvents = interface(IUnknown)
    ['{4E6CDCC9-FB25-4FD5-9CC5-C9F4B6559CEC}']
    function OnNewTrackingInfo(const pToplevelCollection: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComApp2Events
// Flags:     (16) Hidden
// GUID:      {1290BC1A-B219-418D-B078-5934DED08242}
// *********************************************************************//
  IComApp2Events = interface(IUnknown)
    ['{1290BC1A-B219-418D-B078-5934DED08242}']
    function OnAppActivation2(var pinfo: COMSVCSEVENTINFO; guidApp: TGUID; guidProcess: TGUID): HResult; stdcall;
    function OnAppShutdown2(var pinfo: COMSVCSEVENTINFO; guidApp: TGUID): HResult; stdcall;
    function OnAppForceShutdown2(var pinfo: COMSVCSEVENTINFO; guidApp: TGUID): HResult; stdcall;
    function OnAppPaused2(var pinfo: COMSVCSEVENTINFO; guidApp: TGUID; bPaused: Integer): HResult; stdcall;
    function OnAppRecycle2(var pinfo: COMSVCSEVENTINFO; guidApp: TGUID; guidProcess: TGUID; 
                           lReason: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComTransaction2Events
// Flags:     (16) Hidden
// GUID:      {A136F62A-2F94-4288-86E0-D8A1FA4C0299}
// *********************************************************************//
  IComTransaction2Events = interface(IUnknown)
    ['{A136F62A-2F94-4288-86E0-D8A1FA4C0299}']
    function OnTransactionStart2(var pinfo: COMSVCSEVENTINFO; var guidTx: TGUID; var tsid: TGUID; 
                                 fRoot: Integer; nIsolationLevel: SYSINT): HResult; stdcall;
    function OnTransactionPrepare2(var pinfo: COMSVCSEVENTINFO; var guidTx: TGUID; fVoteYes: Integer): HResult; stdcall;
    function OnTransactionAbort2(var pinfo: COMSVCSEVENTINFO; var guidTx: TGUID): HResult; stdcall;
    function OnTransactionCommit2(var pinfo: COMSVCSEVENTINFO; var guidTx: TGUID): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComInstance2Events
// Flags:     (16) Hidden
// GUID:      {20E3BF07-B506-4AD5-A50C-D2CA5B9C158E}
// *********************************************************************//
  IComInstance2Events = interface(IUnknown)
    ['{20E3BF07-B506-4AD5-A50C-D2CA5B9C158E}']
    function OnObjectCreate2(var pinfo: COMSVCSEVENTINFO; var guidActivity: TGUID; 
                             var clsid: TGUID; var tsid: TGUID; CtxtID: Largeuint; 
                             ObjectID: Largeuint; var guidPartition: TGUID): HResult; stdcall;
    function OnObjectDestroy2(var pinfo: COMSVCSEVENTINFO; CtxtID: Largeuint): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComObjectPool2Events
// Flags:     (16) Hidden
// GUID:      {65BF6534-85EA-4F64-8CF4-3D974B2AB1CF}
// *********************************************************************//
  IComObjectPool2Events = interface(IUnknown)
    ['{65BF6534-85EA-4F64-8CF4-3D974B2AB1CF}']
    function OnObjPoolPutObject2(var pinfo: COMSVCSEVENTINFO; var guidObject: TGUID; 
                                 nReason: SYSINT; dwAvailable: LongWord; oid: Largeuint): HResult; stdcall;
    function OnObjPoolGetObject2(var pinfo: COMSVCSEVENTINFO; var guidActivity: TGUID; 
                                 var guidObject: TGUID; dwAvailable: LongWord; oid: Largeuint; 
                                 var guidPartition: TGUID): HResult; stdcall;
    function OnObjPoolRecycleToTx2(var pinfo: COMSVCSEVENTINFO; var guidActivity: TGUID; 
                                   var guidObject: TGUID; var guidTx: TGUID; objid: Largeuint): HResult; stdcall;
    function OnObjPoolGetFromTx2(var pinfo: COMSVCSEVENTINFO; var guidActivity: TGUID; 
                                 var guidObject: TGUID; var guidTx: TGUID; objid: Largeuint; 
                                 var guidPartition: TGUID): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComObjectConstruction2Events
// Flags:     (16) Hidden
// GUID:      {4B5A7827-8DF2-45C0-8F6F-57EA1F856A9F}
// *********************************************************************//
  IComObjectConstruction2Events = interface(IUnknown)
    ['{4B5A7827-8DF2-45C0-8F6F-57EA1F856A9F}']
    function OnObjectConstruct2(var pinfo: COMSVCSEVENTINFO; var guidObject: TGUID; 
                                sConstructString: PWideChar; oid: Largeuint; 
                                var guidPartition: TGUID): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComLTxEvents
// Flags:     (16) Hidden
// GUID:      {605CF82C-578E-4298-975D-82BABCD9E053}
// *********************************************************************//
  IComLTxEvents = interface(IUnknown)
    ['{605CF82C-578E-4298-975D-82BABCD9E053}']
    function OnLtxTransactionStart(var pinfo: COMSVCSEVENTINFO; guidLtx: TGUID; tsid: TGUID; 
                                   fRoot: Integer; nIsolationLevel: SYSINT): HResult; stdcall;
    function OnLtxTransactionPrepare(var pinfo: COMSVCSEVENTINFO; guidLtx: TGUID; fVote: Integer): HResult; stdcall;
    function OnLtxTransactionAbort(var pinfo: COMSVCSEVENTINFO; guidLtx: TGUID): HResult; stdcall;
    function OnLtxTransactionCommit(var pinfo: COMSVCSEVENTINFO; guidLtx: TGUID): HResult; stdcall;
    function OnLtxTransactionPromote(var pinfo: COMSVCSEVENTINFO; guidLtx: TGUID; txnId: TGUID): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISystemAppEventData
// Flags:     (16) Hidden
// GUID:      {D6D48A3C-D5C5-49E7-8C74-99E4889ED52F}
// *********************************************************************//
  ISystemAppEventData = interface(IUnknown)
    ['{D6D48A3C-D5C5-49E7-8C74-99E4889ED52F}']
    function Startup: HResult; stdcall;
    function OnDataChanged(dwPid: LongWord; dwMask: LongWord; dwNumberSinks: LongWord; 
                           const bstrDwMethodMask: WideString; dwReason: LongWord; 
                           u64TraceHandle: Largeuint): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICrmMonitorLogRecords
// Flags:     (0)
// GUID:      {70C8E441-C7ED-11D1-82FB-00A0C91EEDE9}
// *********************************************************************//
  ICrmMonitorLogRecords = interface(IUnknown)
    ['{70C8E441-C7ED-11D1-82FB-00A0C91EEDE9}']
    function Get_Count(out pVal: Integer): HResult; stdcall;
    function Get_TransactionState(out pVal: tagCrmTransactionState): HResult; stdcall;
    function Get_StructuredRecords(out pVal: WordBool): HResult; stdcall;
    function GetLogRecord(dwIndex: LongWord; var pCrmLogRec: tagCrmLogRecordRead): HResult; stdcall;
    function GetLogRecordVariants(IndexNumber: OleVariant; out pLogRecord: OleVariant): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICrmMonitor
// Flags:     (0)
// GUID:      {70C8E443-C7ED-11D1-82FB-00A0C91EEDE9}
// *********************************************************************//
  ICrmMonitor = interface(IUnknown)
    ['{70C8E443-C7ED-11D1-82FB-00A0C91EEDE9}']
    function GetClerks(out pClerks: ICrmMonitorClerks): HResult; stdcall;
    function HoldClerk(Index: OleVariant; out pItem: OleVariant): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICrmMonitorClerks
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {70C8E442-C7ED-11D1-82FB-00A0C91EEDE9}
// *********************************************************************//
  ICrmMonitorClerks = interface(IDispatch)
    ['{70C8E442-C7ED-11D1-82FB-00A0C91EEDE9}']
    function Item(Index: OleVariant): OleVariant; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_Count: Integer; safecall;
    function ProgIdCompensator(Index: OleVariant): OleVariant; safecall;
    function Description(Index: OleVariant): OleVariant; safecall;
    function TransactionUOW(Index: OleVariant): OleVariant; safecall;
    function ActivityId(Index: OleVariant): OleVariant; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  ICrmMonitorClerksDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {70C8E442-C7ED-11D1-82FB-00A0C91EEDE9}
// *********************************************************************//
  ICrmMonitorClerksDisp = dispinterface
    ['{70C8E442-C7ED-11D1-82FB-00A0C91EEDE9}']
    function Item(Index: OleVariant): OleVariant; dispid 0;
    property _NewEnum: IUnknown readonly dispid -4;
    property Count: Integer readonly dispid 1;
    function ProgIdCompensator(Index: OleVariant): OleVariant; dispid 2;
    function Description(Index: OleVariant): OleVariant; dispid 3;
    function TransactionUOW(Index: OleVariant): OleVariant; dispid 4;
    function ActivityId(Index: OleVariant): OleVariant; dispid 5;
  end;

// *********************************************************************//
// Interface: IMessageMover
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {588A085A-B795-11D1-8054-00C04FC340EE}
// *********************************************************************//
  IMessageMover = interface(IDispatch)
    ['{588A085A-B795-11D1-8054-00C04FC340EE}']
    function Get_SourcePath: WideString; safecall;
    procedure Set_SourcePath(const pVal: WideString); safecall;
    function Get_DestPath: WideString; safecall;
    procedure Set_DestPath(const pVal: WideString); safecall;
    function Get_CommitBatchSize: Integer; safecall;
    procedure Set_CommitBatchSize(pVal: Integer); safecall;
    function MoveMessages: Integer; safecall;
    property SourcePath: WideString read Get_SourcePath write Set_SourcePath;
    property DestPath: WideString read Get_DestPath write Set_DestPath;
    property CommitBatchSize: Integer read Get_CommitBatchSize write Set_CommitBatchSize;
  end;

// *********************************************************************//
// DispIntf:  IMessageMoverDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {588A085A-B795-11D1-8054-00C04FC340EE}
// *********************************************************************//
  IMessageMoverDisp = dispinterface
    ['{588A085A-B795-11D1-8054-00C04FC340EE}']
    property SourcePath: WideString dispid 1;
    property DestPath: WideString dispid 2;
    property CommitBatchSize: Integer dispid 3;
    function MoveMessages: Integer; dispid 4;
  end;

// *********************************************************************//
// Interface: IDispenserManager
// Flags:     (16) Hidden
// GUID:      {5CB31E10-2B5F-11CF-BE10-00AA00A2FA25}
// *********************************************************************//
  IDispenserManager = interface(IUnknown)
    ['{5CB31E10-2B5F-11CF-BE10-00AA00A2FA25}']
    function RegisterDispenser(const __MIDL__IDispenserManager0000: IDispenserDriver; 
                               szDispenserName: PWideChar; 
                               out __MIDL__IDispenserManager0001: IHolder): HResult; stdcall;
    function GetContext(out __MIDL__IDispenserManager0002: ULONG_PTR; 
                        out __MIDL__IDispenserManager0003: ULONG_PTR): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDispenserManagerShutdownGuarantee
// Flags:     (16) Hidden
// GUID:      {5CB31E11-2B5F-11CF-BE10-00AA00A2FA25}
// *********************************************************************//
  IDispenserManagerShutdownGuarantee = interface(IUnknown)
    ['{5CB31E11-2B5F-11CF-BE10-00AA00A2FA25}']
    function IsComPlusApp(out pfIsComPlusApp: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDispenserDriver
// Flags:     (16) Hidden
// GUID:      {208B3651-2B48-11CF-BE10-00AA00A2FA25}
// *********************************************************************//
  IDispenserDriver = interface(IUnknown)
    ['{208B3651-2B48-11CF-BE10-00AA00A2FA25}']
    function CreateResource(ResTypId: ULONG_PTR; out pResId: ULONG_PTR; 
                            out pSecsFreeBeforeDestroy: Integer): HResult; stdcall;
    function RateResource(ResTypId: ULONG_PTR; resId: ULONG_PTR; 
                          fRequiresTransactionEnlistment: Integer; out pRating: LongWord): HResult; stdcall;
    function EnlistResource(resId: ULONG_PTR; TransId: ULONG_PTR): HResult; stdcall;
    function ResetResource(resId: ULONG_PTR): HResult; stdcall;
    function DestroyResource(resId: ULONG_PTR): HResult; stdcall;
    function DestroyResourceS(resId: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IHolder
// Flags:     (16) Hidden
// GUID:      {BF6A1850-2B45-11CF-BE10-00AA00A2FA25}
// *********************************************************************//
  IHolder = interface(IUnknown)
    ['{BF6A1850-2B45-11CF-BE10-00AA00A2FA25}']
    function AllocResource(__MIDL__IHolder0000: ULONG_PTR; out __MIDL__IHolder0001: ULONG_PTR): HResult; stdcall;
    function FreeResource(__MIDL__IHolder0002: ULONG_PTR): HResult; stdcall;
    function TrackResource(__MIDL__IHolder0003: ULONG_PTR): HResult; stdcall;
    function TrackResourceS(__MIDL__IHolder0004: PWideChar): HResult; stdcall;
    function UntrackResource(__MIDL__IHolder0005: ULONG_PTR; __MIDL__IHolder0006: Integer): HResult; stdcall;
    function UntrackResourceS(__MIDL__IHolder0007: PWideChar; __MIDL__IHolder0008: Integer): HResult; stdcall;
    function Close: HResult; stdcall;
    function RequestDestroyResource(__MIDL__IHolder0009: ULONG_PTR): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IPoolManager
// Flags:     (4096) Dispatchable
// GUID:      {0A469861-5A91-43A0-99B6-D5E179BB0631}
// *********************************************************************//
  IPoolManager = interface(IDispatch)
    ['{0A469861-5A91-43A0-99B6-D5E179BB0631}']
    function ShutdownPool(const CLSIDOrProgID: WideString): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEventServerTrace
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9A9F12B8-80AF-47AB-A579-35EA57725370}
// *********************************************************************//
  IEventServerTrace = interface(IDispatch)
    ['{9A9F12B8-80AF-47AB-A579-35EA57725370}']
    procedure StartTraceGuid(const bstrguidEvent: WideString; const bstrguidFilter: WideString; 
                             lPidFilter: Integer); safecall;
    procedure StopTraceGuid(const bstrguidEvent: WideString; const bstrguidFilter: WideString; 
                            lPidFilter: Integer); safecall;
    procedure EnumTraceGuid(out plCntGuids: Integer; out pbstrGuidList: WideString); safecall;
  end;

// *********************************************************************//
// DispIntf:  IEventServerTraceDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9A9F12B8-80AF-47AB-A579-35EA57725370}
// *********************************************************************//
  IEventServerTraceDisp = dispinterface
    ['{9A9F12B8-80AF-47AB-A579-35EA57725370}']
    procedure StartTraceGuid(const bstrguidEvent: WideString; const bstrguidFilter: WideString; 
                             lPidFilter: Integer); dispid 1;
    procedure StopTraceGuid(const bstrguidEvent: WideString; const bstrguidFilter: WideString; 
                            lPidFilter: Integer); dispid 2;
    procedure EnumTraceGuid(out plCntGuids: Integer; out pbstrGuidList: WideString); dispid 3;
  end;

// *********************************************************************//
// Interface: IEventServer
// Flags:     (16) Hidden
// GUID:      {F1CB0608-EC04-11D1-93AE-00AA00BA3258}
// *********************************************************************//
  IEventServer = interface(IUnknown)
    ['{F1CB0608-EC04-11D1-93AE-00AA00BA3258}']
    function DispatchManyEvents(dwPid: LongWord; var appGuid: TGUID; dwEvents: LongWord; 
                                cbSize: LongWord; var pBuf: Byte): HResult; stdcall;
    function DispatchOneEvent(dwPid: LongWord; var appGuid: TGUID; cbSize: LongWord; var pBuf: Byte): HResult; stdcall;
    function AddProcess(dwPid: LongWord; var guidApp: TGUID; out pdwInitialEventMask: LongWord; 
                        out ppUnkCallfactory: IUnknown): HResult; stdcall;
    function RemoveProcess(dwPid: LongWord): HResult; stdcall;
    function GetCallFactoryObject(dwPid: LongWord; out ppUnkCallfactory: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEventServer2
// Flags:     (16) Hidden
// GUID:      {378F3CA7-BD24-481C-8DC3-5E5ECE1BCAD7}
// *********************************************************************//
  IEventServer2 = interface(IUnknown)
    ['{378F3CA7-BD24-481C-8DC3-5E5ECE1BCAD7}']
    function AddProcess2(dwPid: LongWord; var guidApp: TGUID; var guidAppInstance: TGUID; 
                         var guidPartition: TGUID; dwNumberSinks: LongWord; 
                         out pdwInitialEventMask: LongWord; var pdwInitialMetMasks: LongWord; 
                         out ppUnkCallfactory: IUnknown; out pTraceHandle: Largeuint): HResult; stdcall;
    function RemoveProcess2(dwPid: LongWord): HResult; stdcall;
    function UpdateEventMasks(dwPid: LongWord; dwNumberSinks: LongWord; dwEventMasksIn: LongWord; 
                              out pdwEventMaskOut: LongWord; var pdwMetMasksInOut: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IReceiveAppData
// Flags:     (16) Hidden
// GUID:      {413DAFB0-BCF4-11D1-861D-0080C729264D}
// *********************************************************************//
  IReceiveAppData = interface(IUnknown)
    ['{413DAFB0-BCF4-11D1-861D-0080C729264D}']
    function Register(var pApplId: TGUID; var pguidApplPartitionId: TGUID; 
                      var pApplInstanceId: TGUID; eAppType: tagCOMPLUS_APPTYPE; 
                      pwszProcessName: PWideChar; const pUnkPackageObject: IUnknown; 
                      dwAppProcessId: LongWord; dwState: LongWord; var pRecycleInfo: RECYCLE_INFO; 
                      out pidApp: LongWord; out pPushRate: LongWord): HResult; stdcall;
    function PushAppData(idApp: LongWord; appData: APPSTATISTICS; dwAppState: LongWord; 
                         nCLSIDs: LongWord; var clsids: CLSIDDATA2): HResult; stdcall;
    function Unregister(idApp: LongWord): HResult; stdcall;
    function RecycleCallingProcess(idApp: LongWord; lReasonCode: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IGetAppData
// Flags:     (16) Hidden
// GUID:      {B60040E0-BCF3-11D1-861D-0080C729264D}
// *********************************************************************//
  IGetAppData = interface(IUnknown)
    ['{B60040E0-BCF3-11D1-861D-0080C729264D}']
    function SetPushRate(dwPushRate: LongWord): HResult; stdcall;
    function GetApps(out nApps: LongWord; out aAppData: PUserType7): HResult; stdcall;
    function GetAppData(idApp: LongWord; out nCLSIDs: LongWord; out aAppData: PUserType8): HResult; stdcall;
    function GetCLSIDData(idApp: LongWord; clsid: TGUID; out ppAppData: PUserType8): HResult; stdcall;
    function Shutdown: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IProcessDump
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {23C9DD26-2355-4FE2-84DE-F779A238ADBD}
// *********************************************************************//
  IProcessDump = interface(IDispatch)
    ['{23C9DD26-2355-4FE2-84DE-F779A238ADBD}']
    procedure IsSupported; safecall;
    function DumpProcess(const bstrInstanceID: WideString; const bstrDirectory: WideString; 
                         dwMaxImages: LongWord): WideString; safecall;
  end;

// *********************************************************************//
// DispIntf:  IProcessDumpDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {23C9DD26-2355-4FE2-84DE-F779A238ADBD}
// *********************************************************************//
  IProcessDumpDisp = dispinterface
    ['{23C9DD26-2355-4FE2-84DE-F779A238ADBD}']
    procedure IsSupported; dispid 1;
    function DumpProcess(const bstrInstanceID: WideString; const bstrDirectory: WideString; 
                         dwMaxImages: LongWord): WideString; dispid 2;
  end;

// *********************************************************************//
// Interface: IPersist
// Flags:     (0)
// GUID:      {0000010C-0000-0000-C000-000000000046}
// *********************************************************************//
  IPersist = interface(IUnknown)
    ['{0000010C-0000-0000-C000-000000000046}']
    function GetClassID(out pClassID: TGUID): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IPersistStream
// Flags:     (0)
// GUID:      {00000109-0000-0000-C000-000000000046}
// *********************************************************************//
  IPersistStream = interface(IPersist)
    ['{00000109-0000-0000-C000-000000000046}']
    function IsDirty: HResult; stdcall;
    function Load(const pstm: IStream): HResult; stdcall;
    function Save(const pstm: IStream; fClearDirty: Integer): HResult; stdcall;
    function GetSizeMax(out pcbSize: _ULARGE_INTEGER): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMoniker
// Flags:     (0)
// GUID:      {0000000F-0000-0000-C000-000000000046}
// *********************************************************************//
  IMoniker = interface(IPersistStream)
    ['{0000000F-0000-0000-C000-000000000046}']
    function RemoteBindToObject(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                                var riidResult: TGUID; out ppvResult: IUnknown): HResult; stdcall;
    function RemoteBindToStorage(const pbc: IBindCtx; const pmkToLeft: IMoniker; var riid: TGUID; 
                                 out ppvObj: IUnknown): HResult; stdcall;
    function Reduce(const pbc: IBindCtx; dwReduceHowFar: LongWord; var ppmkToLeft: IMoniker; 
                    out ppmkReduced: IMoniker): HResult; stdcall;
    function ComposeWith(const pmkRight: IMoniker; fOnlyIfNotGeneric: Integer; 
                         out ppmkComposite: IMoniker): HResult; stdcall;
    function Enum(fForward: Integer; out ppenumMoniker: IEnumMoniker): HResult; stdcall;
    function IsEqual(const pmkOtherMoniker: IMoniker): HResult; stdcall;
    function Hash(out pdwHash: LongWord): HResult; stdcall;
    function IsRunning(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                       const pmkNewlyRunning: IMoniker): HResult; stdcall;
    function GetTimeOfLastChange(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                                 out pfiletime: _FILETIME): HResult; stdcall;
    function Inverse(out ppmk: IMoniker): HResult; stdcall;
    function CommonPrefixWith(const pmkOther: IMoniker; out ppmkPrefix: IMoniker): HResult; stdcall;
    function RelativePathTo(const pmkOther: IMoniker; out ppmkRelPath: IMoniker): HResult; stdcall;
    function GetDisplayName(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                            out ppszDisplayName: PWideChar): HResult; stdcall;
    function ParseDisplayName(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                              pszDisplayName: PWideChar; out pchEaten: LongWord; 
                              out ppmkOut: IMoniker): HResult; stdcall;
    function IsSystemMoniker(out pdwMksys: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISequentialStream
// Flags:     (0)
// GUID:      {0C733A30-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  ISequentialStream = interface(IUnknown)
    ['{0C733A30-2A1C-11CE-ADE5-00AA0044773D}']
    function RemoteRead(out pv: Byte; cb: LongWord; out pcbRead: LongWord): HResult; stdcall;
    function RemoteWrite(var pv: Byte; cb: LongWord; out pcbWritten: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IStream
// Flags:     (0)
// GUID:      {0000000C-0000-0000-C000-000000000046}
// *********************************************************************//
  IStream = interface(ISequentialStream)
    ['{0000000C-0000-0000-C000-000000000046}']
    function RemoteSeek(dlibMove: _LARGE_INTEGER; dwOrigin: LongWord; 
                        out plibNewPosition: _ULARGE_INTEGER): HResult; stdcall;
    function SetSize(libNewSize: _ULARGE_INTEGER): HResult; stdcall;
    function RemoteCopyTo(const pstm: IStream; cb: _ULARGE_INTEGER; out pcbRead: _ULARGE_INTEGER; 
                          out pcbWritten: _ULARGE_INTEGER): HResult; stdcall;
    function Commit(grfCommitFlags: LongWord): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset: _ULARGE_INTEGER; cb: _ULARGE_INTEGER; dwLockType: LongWord): HResult; stdcall;
    function UnlockRegion(libOffset: _ULARGE_INTEGER; cb: _ULARGE_INTEGER; dwLockType: LongWord): HResult; stdcall;
    function Stat(out pstatstg: tagSTATSTG; grfStatFlag: LongWord): HResult; stdcall;
    function Clone(out ppstm: IStream): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IBindCtx
// Flags:     (0)
// GUID:      {0000000E-0000-0000-C000-000000000046}
// *********************************************************************//
  IBindCtx = interface(IUnknown)
    ['{0000000E-0000-0000-C000-000000000046}']
    function RegisterObjectBound(const pUnk: IUnknown): HResult; stdcall;
    function RevokeObjectBound(const pUnk: IUnknown): HResult; stdcall;
    function ReleaseBoundObjects: HResult; stdcall;
    function RemoteSetBindOptions(var pbindopts: tagBIND_OPTS2): HResult; stdcall;
    function RemoteGetBindOptions(var pbindopts: tagBIND_OPTS2): HResult; stdcall;
    function GetRunningObjectTable(out pprot: IRunningObjectTable): HResult; stdcall;
    function RegisterObjectParam(pszKey: PWideChar; const pUnk: IUnknown): HResult; stdcall;
    function GetObjectParam(pszKey: PWideChar; out ppunk: IUnknown): HResult; stdcall;
    function EnumObjectParam(out ppEnum: IEnumString): HResult; stdcall;
    function RevokeObjectParam(pszKey: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IRunningObjectTable
// Flags:     (0)
// GUID:      {00000010-0000-0000-C000-000000000046}
// *********************************************************************//
  IRunningObjectTable = interface(IUnknown)
    ['{00000010-0000-0000-C000-000000000046}']
    function Register(grfFlags: LongWord; const punkObject: IUnknown; 
                      const pmkObjectName: IMoniker; out pdwRegister: LongWord): HResult; stdcall;
    function Revoke(dwRegister: LongWord): HResult; stdcall;
    function IsRunning(const pmkObjectName: IMoniker): HResult; stdcall;
    function GetObject(const pmkObjectName: IMoniker; out ppunkObject: IUnknown): HResult; stdcall;
    function NoteChangeTime(dwRegister: LongWord; var pfiletime: _FILETIME): HResult; stdcall;
    function GetTimeOfLastChange(const pmkObjectName: IMoniker; out pfiletime: _FILETIME): HResult; stdcall;
    function EnumRunning(out ppenumMoniker: IEnumMoniker): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumMoniker
// Flags:     (0)
// GUID:      {00000102-0000-0000-C000-000000000046}
// *********************************************************************//
  IEnumMoniker = interface(IUnknown)
    ['{00000102-0000-0000-C000-000000000046}']
    function RemoteNext(celt: LongWord; out rgelt: IMoniker; out pceltFetched: LongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppEnum: IEnumMoniker): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumString
// Flags:     (0)
// GUID:      {00000101-0000-0000-C000-000000000046}
// *********************************************************************//
  IEnumString = interface(IUnknown)
    ['{00000101-0000-0000-C000-000000000046}']
    function RemoteNext(celt: LongWord; out rgelt: PWideChar; out pceltFetched: LongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppEnum: IEnumString): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IAppDomainHelper
// Flags:     (4096) Dispatchable
// GUID:      {C7B67079-8255-42C6-9EC0-6994A3548780}
// *********************************************************************//
  IAppDomainHelper = interface(IDispatch)
    ['{C7B67079-8255-42C6-9EC0-6994A3548780}']
    function pfnShutdownCB(var pv: Pointer): HResult; stdcall;
    function Initialize(const pUnkAD: IUnknown; 
                        const __MIDL__IAppDomainHelper0000: IAppDomainHelper; var pPool: Pointer): HResult; stdcall;
    function pfnCallbackCB(var pv: Pointer): HResult; stdcall;
    function DoCallback(const pUnkAD: IUnknown; 
                        const __MIDL__IAppDomainHelper0001: IAppDomainHelper; var pPool: Pointer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IAssemblyLocator
// Flags:     (4352) OleAutomation Dispatchable
// GUID:      {391FFBB9-A8EE-432A-ABC8-BAA238DAB90F}
// *********************************************************************//
  IAssemblyLocator = interface(IDispatch)
    ['{391FFBB9-A8EE-432A-ABC8-BAA238DAB90F}']
    function GetModules(const applicationDir: WideString; const applicationName: WideString; 
                        const assemblyName: WideString; out pModules: PSafeArray): HResult; stdcall;
  end;

// *********************************************************************//
// The Class CoSecurityCertificate provides a Create and CreateRemote method to          
// create instances of the default interface ISecurityCertificateColl exposed by              
// the CoClass SecurityCertificate. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSecurityCertificate = class
    class function Create: ISecurityCertificateColl;
    class function CreateRemote(const MachineName: string): ISecurityCertificateColl;
  end;

// *********************************************************************//
// The Class CoSecurityIdentity provides a Create and CreateRemote method to          
// create instances of the default interface ISecurityIdentityColl exposed by              
// the CoClass SecurityIdentity. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSecurityIdentity = class
    class function Create: ISecurityIdentityColl;
    class function CreateRemote(const MachineName: string): ISecurityIdentityColl;
  end;

// *********************************************************************//
// The Class CoSecurityCallers provides a Create and CreateRemote method to          
// create instances of the default interface ISecurityCallersColl exposed by              
// the CoClass SecurityCallers. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSecurityCallers = class
    class function Create: ISecurityCallersColl;
    class function CreateRemote(const MachineName: string): ISecurityCallersColl;
  end;

// *********************************************************************//
// The Class CoSecurityCallContext provides a Create and CreateRemote method to          
// create instances of the default interface ISecurityCallContext exposed by              
// the CoClass SecurityCallContext. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSecurityCallContext = class
    class function Create: ISecurityCallContext;
    class function CreateRemote(const MachineName: string): ISecurityCallContext;
  end;

// *********************************************************************//
// The Class CoGetSecurityCallContextAppObject provides a Create and CreateRemote method to          
// create instances of the default interface IGetSecurityCallContext exposed by              
// the CoClass GetSecurityCallContextAppObject. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoGetSecurityCallContextAppObject = class
    class function Create: IGetSecurityCallContext;
    class function CreateRemote(const MachineName: string): IGetSecurityCallContext;
  end;

// *********************************************************************//
// The Class CoDummy30040732 provides a Create and CreateRemote method to          
// create instances of the default interface IObjectContext exposed by              
// the CoClass Dummy30040732. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoDummy30040732 = class
    class function Create: IObjectContext;
    class function CreateRemote(const MachineName: string): IObjectContext;
  end;

// *********************************************************************//
// The Class CoAppServer provides a Create and CreateRemote method to          
// create instances of the default interface IMTxAS exposed by              
// the CoClass AppServer. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoAppServer = class
    class function Create: IMTxAS;
    class function CreateRemote(const MachineName: string): IMTxAS;
  end;

// *********************************************************************//
// The Class CoTransactionContext provides a Create and CreateRemote method to          
// create instances of the default interface ITransactionContext exposed by              
// the CoClass TransactionContext. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoTransactionContext = class
    class function Create: ITransactionContext;
    class function CreateRemote(const MachineName: string): ITransactionContext;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TTransactionContext
// Help String      : Transaction Context Class
// Default Interface: ITransactionContext
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TTransactionContextProperties= class;
{$ENDIF}
  TTransactionContext = class(TOleServer)
  private
    FIntf: ITransactionContext;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TTransactionContextProperties;
    function GetServerProperties: TTransactionContextProperties;
{$ENDIF}
    function GetDefaultInterface: ITransactionContext;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ITransactionContext);
    procedure Disconnect; override;
    function CreateInstance(const pszProgId: WideString): OleVariant;
    procedure Commit;
    procedure Abort;
    property DefaultInterface: ITransactionContext read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TTransactionContextProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TTransactionContext
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TTransactionContextProperties = class(TPersistent)
  private
    FServer:    TTransactionContext;
    function    GetDefaultInterface: ITransactionContext;
    constructor Create(AServer: TTransactionContext);
  protected
  public
    property DefaultInterface: ITransactionContext read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoTransactionContextEx provides a Create and CreateRemote method to          
// create instances of the default interface ITransactionContextEx exposed by              
// the CoClass TransactionContextEx. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoTransactionContextEx = class
    class function Create: ITransactionContextEx;
    class function CreateRemote(const MachineName: string): ITransactionContextEx;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TTransactionContextEx
// Help String      : Transaction Context Extended Class
// Default Interface: ITransactionContextEx
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TTransactionContextExProperties= class;
{$ENDIF}
  TTransactionContextEx = class(TOleServer)
  private
    FIntf: ITransactionContextEx;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TTransactionContextExProperties;
    function GetServerProperties: TTransactionContextExProperties;
{$ENDIF}
    function GetDefaultInterface: ITransactionContextEx;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ITransactionContextEx);
    procedure Disconnect; override;
    function CreateInstance(var rclsid: TGUID; var riid: TGUID; out pObject: Pointer): HResult;
    function Commit: HResult;
    function Abort: HResult;
    property DefaultInterface: ITransactionContextEx read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TTransactionContextExProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TTransactionContextEx
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TTransactionContextExProperties = class(TPersistent)
  private
    FServer:    TTransactionContextEx;
    function    GetDefaultInterface: ITransactionContextEx;
    constructor Create(AServer: TTransactionContextEx);
  protected
  public
    property DefaultInterface: ITransactionContextEx read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoByotServerEx provides a Create and CreateRemote method to          
// create instances of the default interface ICreateWithTipTransactionEx exposed by              
// the CoClass ByotServerEx. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoByotServerEx = class
    class function Create: ICreateWithTipTransactionEx;
    class function CreateRemote(const MachineName: string): ICreateWithTipTransactionEx;
  end;

// *********************************************************************//
// The Class CoCServiceConfig provides a Create and CreateRemote method to          
// create instances of the default interface IServiceInheritanceConfig exposed by              
// the CoClass CServiceConfig. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCServiceConfig = class
    class function Create: IServiceInheritanceConfig;
    class function CreateRemote(const MachineName: string): IServiceInheritanceConfig;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCServiceConfig
// Help String      : CServiceConfig Class
// Default Interface: IServiceInheritanceConfig
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TCServiceConfigProperties= class;
{$ENDIF}
  TCServiceConfig = class(TOleServer)
  private
    FIntf: IServiceInheritanceConfig;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TCServiceConfigProperties;
    function GetServerProperties: TCServiceConfigProperties;
{$ENDIF}
    function GetDefaultInterface: IServiceInheritanceConfig;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IServiceInheritanceConfig);
    procedure Disconnect; override;
    function ContainingContextTreatment(inheritanceConfig: tagCSC_InheritanceConfig): HResult;
    property DefaultInterface: IServiceInheritanceConfig read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TCServiceConfigProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TCServiceConfig
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TCServiceConfigProperties = class(TPersistent)
  private
    FServer:    TCServiceConfig;
    function    GetDefaultInterface: IServiceInheritanceConfig;
    constructor Create(AServer: TCServiceConfig);
  protected
  public
    property DefaultInterface: IServiceInheritanceConfig read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoServicePool provides a Create and CreateRemote method to          
// create instances of the default interface IServicePool exposed by              
// the CoClass ServicePool. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoServicePool = class
    class function Create: IServicePool;
    class function CreateRemote(const MachineName: string): IServicePool;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TServicePool
// Help String      : ServicePool Class
// Default Interface: IServicePool
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TServicePoolProperties= class;
{$ENDIF}
  TServicePool = class(TOleServer)
  private
    FIntf: IServicePool;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TServicePoolProperties;
    function GetServerProperties: TServicePoolProperties;
{$ENDIF}
    function GetDefaultInterface: IServicePool;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IServicePool);
    procedure Disconnect; override;
    function Initialize(const pPoolConfig: IUnknown): HResult;
    function GetObject(var riid: TGUID; out ppv: Pointer): HResult;
    function Shutdown: HResult;
    property DefaultInterface: IServicePool read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TServicePoolProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TServicePool
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TServicePoolProperties = class(TPersistent)
  private
    FServer:    TServicePool;
    function    GetDefaultInterface: IServicePool;
    constructor Create(AServer: TServicePool);
  protected
  public
    property DefaultInterface: IServicePool read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoServicePoolConfig provides a Create and CreateRemote method to          
// create instances of the default interface IServicePoolConfig exposed by              
// the CoClass ServicePoolConfig. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoServicePoolConfig = class
    class function Create: IServicePoolConfig;
    class function CreateRemote(const MachineName: string): IServicePoolConfig;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TServicePoolConfig
// Help String      : ServicePoolConfig Class
// Default Interface: IServicePoolConfig
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TServicePoolConfigProperties= class;
{$ENDIF}
  TServicePoolConfig = class(TOleServer)
  private
    FIntf: IServicePoolConfig;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TServicePoolConfigProperties;
    function GetServerProperties: TServicePoolConfigProperties;
{$ENDIF}
    function GetDefaultInterface: IServicePoolConfig;
  protected
    procedure InitServerData; override;
    function Set_MaxPoolSize(pdwMaxPool: LongWord): HResult;
    function Get_MaxPoolSize(out pdwMaxPool: LongWord): HResult;
    function Set_MinPoolSize(pdwMinPool: LongWord): HResult;
    function Get_MinPoolSize(out pdwMinPool: LongWord): HResult;
    function Set_CreationTimeout(pdwCreationTimeout: LongWord): HResult;
    function Get_CreationTimeout(out pdwCreationTimeout: LongWord): HResult;
    function Set_TransactionAffinity(pfTxAffinity: Integer): HResult;
    function Get_TransactionAffinity(out pfTxAffinity: Integer): HResult;
    function Set_ClassFactory(const pFactory: IClassFactory): HResult;
    function Get_ClassFactory(out pFactory: IClassFactory): HResult;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IServicePoolConfig);
    procedure Disconnect; override;
    property DefaultInterface: IServicePoolConfig read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TServicePoolConfigProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TServicePoolConfig
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TServicePoolConfigProperties = class(TPersistent)
  private
    FServer:    TServicePoolConfig;
    function    GetDefaultInterface: IServicePoolConfig;
    constructor Create(AServer: TServicePoolConfig);
  protected
    function Set_MaxPoolSize(pdwMaxPool: LongWord): HResult;
    function Get_MaxPoolSize(out pdwMaxPool: LongWord): HResult;
    function Set_MinPoolSize(pdwMinPool: LongWord): HResult;
    function Get_MinPoolSize(out pdwMinPool: LongWord): HResult;
    function Set_CreationTimeout(pdwCreationTimeout: LongWord): HResult;
    function Get_CreationTimeout(out pdwCreationTimeout: LongWord): HResult;
    function Set_TransactionAffinity(pfTxAffinity: Integer): HResult;
    function Get_TransactionAffinity(out pfTxAffinity: Integer): HResult;
    function Set_ClassFactory(const pFactory: IClassFactory): HResult;
    function Get_ClassFactory(out pFactory: IClassFactory): HResult;
  public
    property DefaultInterface: IServicePoolConfig read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoSharedProperty provides a Create and CreateRemote method to          
// create instances of the default interface ISharedProperty exposed by              
// the CoClass SharedProperty. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSharedProperty = class
    class function Create: ISharedProperty;
    class function CreateRemote(const MachineName: string): ISharedProperty;
  end;

// *********************************************************************//
// The Class CoSharedPropertyGroup provides a Create and CreateRemote method to          
// create instances of the default interface ISharedPropertyGroup exposed by              
// the CoClass SharedPropertyGroup. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSharedPropertyGroup = class
    class function Create: ISharedPropertyGroup;
    class function CreateRemote(const MachineName: string): ISharedPropertyGroup;
  end;

// *********************************************************************//
// The Class CoSharedPropertyGroupManager provides a Create and CreateRemote method to          
// create instances of the default interface ISharedPropertyGroupManager exposed by              
// the CoClass SharedPropertyGroupManager. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSharedPropertyGroupManager = class
    class function Create: ISharedPropertyGroupManager;
    class function CreateRemote(const MachineName: string): ISharedPropertyGroupManager;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TSharedPropertyGroupManager
// Help String      : SharedPropertyGroupManager Class
// Default Interface: ISharedPropertyGroupManager
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TSharedPropertyGroupManagerProperties= class;
{$ENDIF}
  TSharedPropertyGroupManager = class(TOleServer)
  private
    FIntf: ISharedPropertyGroupManager;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TSharedPropertyGroupManagerProperties;
    function GetServerProperties: TSharedPropertyGroupManagerProperties;
{$ENDIF}
    function GetDefaultInterface: ISharedPropertyGroupManager;
  protected
    procedure InitServerData; override;
    function Get_Group(const name: WideString): ISharedPropertyGroup;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ISharedPropertyGroupManager);
    procedure Disconnect; override;
    function CreatePropertyGroup(const name: WideString; var dwIsoMode: Integer; 
                                 var dwRelMode: Integer; out fExists: WordBool): ISharedPropertyGroup;
    property DefaultInterface: ISharedPropertyGroupManager read GetDefaultInterface;
    property Group[const name: WideString]: ISharedPropertyGroup read Get_Group;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TSharedPropertyGroupManagerProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TSharedPropertyGroupManager
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TSharedPropertyGroupManagerProperties = class(TPersistent)
  private
    FServer:    TSharedPropertyGroupManager;
    function    GetDefaultInterface: ISharedPropertyGroupManager;
    constructor Create(AServer: TSharedPropertyGroupManager);
  protected
    function Get_Group(const name: WideString): ISharedPropertyGroup;
  public
    property DefaultInterface: ISharedPropertyGroupManager read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoCOMEvents provides a Create and CreateRemote method to          
// create instances of the default interface IMtsEvents exposed by              
// the CoClass COMEvents. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCOMEvents = class
    class function Create: IMtsEvents;
    class function CreateRemote(const MachineName: string): IMtsEvents;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCOMEvents
// Help String      : MTSEvents Class
// Default Interface: IMtsEvents
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TCOMEventsProperties= class;
{$ENDIF}
  TCOMEvents = class(TOleServer)
  private
    FIntf: IMtsEvents;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TCOMEventsProperties;
    function GetServerProperties: TCOMEventsProperties;
{$ENDIF}
    function GetDefaultInterface: IMtsEvents;
  protected
    procedure InitServerData; override;
    function Get_PackageName: WideString;
    function Get_PackageGuid: WideString;
    function Get_FireEvents: WordBool;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IMtsEvents);
    procedure Disconnect; override;
    procedure PostEvent(var vEvent: OleVariant);
    function GetProcessID: Integer;
    property DefaultInterface: IMtsEvents read GetDefaultInterface;
    property PackageName: WideString read Get_PackageName;
    property PackageGuid: WideString read Get_PackageGuid;
    property FireEvents: WordBool read Get_FireEvents;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TCOMEventsProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TCOMEvents
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TCOMEventsProperties = class(TPersistent)
  private
    FServer:    TCOMEvents;
    function    GetDefaultInterface: IMtsEvents;
    constructor Create(AServer: TCOMEvents);
  protected
    function Get_PackageName: WideString;
    function Get_PackageGuid: WideString;
    function Get_FireEvents: WordBool;
  public
    property DefaultInterface: IMtsEvents read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoCoMTSLocator provides a Create and CreateRemote method to          
// create instances of the default interface IMTSLocator exposed by              
// the CoClass CoMTSLocator. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCoMTSLocator = class
    class function Create: IMTSLocator;
    class function CreateRemote(const MachineName: string): IMTSLocator;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCoMTSLocator
// Help String      : MTSLocator Class
// Default Interface: IMTSLocator
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TCoMTSLocatorProperties= class;
{$ENDIF}
  TCoMTSLocator = class(TOleServer)
  private
    FIntf: IMTSLocator;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TCoMTSLocatorProperties;
    function GetServerProperties: TCoMTSLocatorProperties;
{$ENDIF}
    function GetDefaultInterface: IMTSLocator;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IMTSLocator);
    procedure Disconnect; override;
    function GetEventDispatcher: IUnknown;
    property DefaultInterface: IMTSLocator read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TCoMTSLocatorProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TCoMTSLocator
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TCoMTSLocatorProperties = class(TPersistent)
  private
    FServer:    TCoMTSLocator;
    function    GetDefaultInterface: IMTSLocator;
    constructor Create(AServer: TCoMTSLocator);
  protected
  public
    property DefaultInterface: IMTSLocator read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoMtsGrp provides a Create and CreateRemote method to          
// create instances of the default interface IMtsGrp exposed by              
// the CoClass MtsGrp. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoMtsGrp = class
    class function Create: IMtsGrp;
    class function CreateRemote(const MachineName: string): IMtsGrp;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TMtsGrp
// Help String      : MtxGrp Class
// Default Interface: IMtsGrp
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TMtsGrpProperties= class;
{$ENDIF}
  TMtsGrp = class(TOleServer)
  private
    FIntf: IMtsGrp;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TMtsGrpProperties;
    function GetServerProperties: TMtsGrpProperties;
{$ENDIF}
    function GetDefaultInterface: IMtsGrp;
  protected
    procedure InitServerData; override;
    function Get_Count: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IMtsGrp);
    procedure Disconnect; override;
    procedure Item(lIndex: Integer; out ppUnkDispatcher: IUnknown);
    procedure Refresh;
    property DefaultInterface: IMtsGrp read GetDefaultInterface;
    property Count: Integer read Get_Count;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TMtsGrpProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TMtsGrp
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TMtsGrpProperties = class(TPersistent)
  private
    FServer:    TMtsGrp;
    function    GetDefaultInterface: IMtsGrp;
    constructor Create(AServer: TMtsGrp);
  protected
    function Get_Count: Integer;
  public
    property DefaultInterface: IMtsGrp read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoComServiceEvents provides a Create and CreateRemote method to          
// create instances of the default interface IComThreadEvents exposed by              
// the CoClass ComServiceEvents. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoComServiceEvents = class
    class function Create: IComThreadEvents;
    class function CreateRemote(const MachineName: string): IComThreadEvents;
  end;

// *********************************************************************//
// The Class CoComSystemAppEventData provides a Create and CreateRemote method to          
// create instances of the default interface ISystemAppEventData exposed by              
// the CoClass ComSystemAppEventData. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoComSystemAppEventData = class
    class function Create: ISystemAppEventData;
    class function CreateRemote(const MachineName: string): ISystemAppEventData;
  end;

// *********************************************************************//
// The Class CoCRMClerk provides a Create and CreateRemote method to          
// create instances of the default interface ICrmLogControl exposed by              
// the CoClass CRMClerk. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCRMClerk = class
    class function Create: ICrmLogControl;
    class function CreateRemote(const MachineName: string): ICrmLogControl;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCRMClerk
// Help String      : CRMClerk Class
// Default Interface: ICrmLogControl
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TCRMClerkProperties= class;
{$ENDIF}
  TCRMClerk = class(TOleServer)
  private
    FIntf: ICrmLogControl;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TCRMClerkProperties;
    function GetServerProperties: TCRMClerkProperties;
{$ENDIF}
    function GetDefaultInterface: ICrmLogControl;
  protected
    procedure InitServerData; override;
    function Get_TransactionUOW(out pVal: WideString): HResult;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ICrmLogControl);
    procedure Disconnect; override;
    function RegisterCompensator(lpcwstrProgIdCompensator: PWideChar; 
                                 lpcwstrDescription: PWideChar; lCrmRegFlags: Integer): HResult;
    function WriteLogRecordVariants(var pLogRecord: OleVariant): HResult;
    function ForceLog: HResult;
    function ForgetLogRecord: HResult;
    function ForceTransactionToAbort: HResult;
    function WriteLogRecord(var rgBlob: tagBLOB; cBlob: LongWord): HResult;
    property DefaultInterface: ICrmLogControl read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TCRMClerkProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TCRMClerk
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TCRMClerkProperties = class(TPersistent)
  private
    FServer:    TCRMClerk;
    function    GetDefaultInterface: ICrmLogControl;
    constructor Create(AServer: TCRMClerk);
  protected
    function Get_TransactionUOW(out pVal: WideString): HResult;
  public
    property DefaultInterface: ICrmLogControl read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoCRMRecoveryClerk provides a Create and CreateRemote method to          
// create instances of the default interface ICrmMonitor exposed by              
// the CoClass CRMRecoveryClerk. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCRMRecoveryClerk = class
    class function Create: ICrmMonitor;
    class function CreateRemote(const MachineName: string): ICrmMonitor;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCRMRecoveryClerk
// Help String      : CRMRecoveryClerk Class
// Default Interface: ICrmMonitor
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TCRMRecoveryClerkProperties= class;
{$ENDIF}
  TCRMRecoveryClerk = class(TOleServer)
  private
    FIntf: ICrmMonitor;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TCRMRecoveryClerkProperties;
    function GetServerProperties: TCRMRecoveryClerkProperties;
{$ENDIF}
    function GetDefaultInterface: ICrmMonitor;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ICrmMonitor);
    procedure Disconnect; override;
    function GetClerks(out pClerks: ICrmMonitorClerks): HResult;
    function HoldClerk(Index: OleVariant; out pItem: OleVariant): HResult;
    property DefaultInterface: ICrmMonitor read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TCRMRecoveryClerkProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TCRMRecoveryClerk
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TCRMRecoveryClerkProperties = class(TPersistent)
  private
    FServer:    TCRMRecoveryClerk;
    function    GetDefaultInterface: ICrmMonitor;
    constructor Create(AServer: TCRMRecoveryClerk);
  protected
  public
    property DefaultInterface: ICrmMonitor read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoMessageMover provides a Create and CreateRemote method to          
// create instances of the default interface IMessageMover exposed by              
// the CoClass MessageMover. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoMessageMover = class
    class function Create: IMessageMover;
    class function CreateRemote(const MachineName: string): IMessageMover;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TMessageMover
// Help String      : QC MessageMover Class
// Default Interface: IMessageMover
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TMessageMoverProperties= class;
{$ENDIF}
  TMessageMover = class(TOleServer)
  private
    FIntf: IMessageMover;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TMessageMoverProperties;
    function GetServerProperties: TMessageMoverProperties;
{$ENDIF}
    function GetDefaultInterface: IMessageMover;
  protected
    procedure InitServerData; override;
    function Get_SourcePath: WideString;
    procedure Set_SourcePath(const pVal: WideString);
    function Get_DestPath: WideString;
    procedure Set_DestPath(const pVal: WideString);
    function Get_CommitBatchSize: Integer;
    procedure Set_CommitBatchSize(pVal: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IMessageMover);
    procedure Disconnect; override;
    function MoveMessages: Integer;
    property DefaultInterface: IMessageMover read GetDefaultInterface;
    property SourcePath: WideString read Get_SourcePath write Set_SourcePath;
    property DestPath: WideString read Get_DestPath write Set_DestPath;
    property CommitBatchSize: Integer read Get_CommitBatchSize write Set_CommitBatchSize;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TMessageMoverProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TMessageMover
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TMessageMoverProperties = class(TPersistent)
  private
    FServer:    TMessageMover;
    function    GetDefaultInterface: IMessageMover;
    constructor Create(AServer: TMessageMover);
  protected
    function Get_SourcePath: WideString;
    procedure Set_SourcePath(const pVal: WideString);
    function Get_DestPath: WideString;
    procedure Set_DestPath(const pVal: WideString);
    function Get_CommitBatchSize: Integer;
    procedure Set_CommitBatchSize(pVal: Integer);
  public
    property DefaultInterface: IMessageMover read GetDefaultInterface;
  published
    property SourcePath: WideString read Get_SourcePath write Set_SourcePath;
    property DestPath: WideString read Get_DestPath write Set_DestPath;
    property CommitBatchSize: Integer read Get_CommitBatchSize write Set_CommitBatchSize;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoDispenserManager provides a Create and CreateRemote method to          
// create instances of the default interface IDispenserManager exposed by              
// the CoClass DispenserManager. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoDispenserManager = class
    class function Create: IDispenserManager;
    class function CreateRemote(const MachineName: string): IDispenserManager;
  end;

// *********************************************************************//
// The Class CoPoolMgr provides a Create and CreateRemote method to          
// create instances of the default interface IPoolManager exposed by              
// the CoClass PoolMgr. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoPoolMgr = class
    class function Create: IPoolManager;
    class function CreateRemote(const MachineName: string): IPoolManager;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TPoolMgr
// Help String      : 
// Default Interface: IPoolManager
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TPoolMgrProperties= class;
{$ENDIF}
  TPoolMgr = class(TOleServer)
  private
    FIntf: IPoolManager;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TPoolMgrProperties;
    function GetServerProperties: TPoolMgrProperties;
{$ENDIF}
    function GetDefaultInterface: IPoolManager;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IPoolManager);
    procedure Disconnect; override;
    function ShutdownPool(const CLSIDOrProgID: WideString): HResult;
    property DefaultInterface: IPoolManager read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TPoolMgrProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TPoolMgr
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TPoolMgrProperties = class(TPersistent)
  private
    FServer:    TPoolMgr;
    function    GetDefaultInterface: IPoolManager;
    constructor Create(AServer: TPoolMgr);
  protected
  public
    property DefaultInterface: IPoolManager read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoEventServer provides a Create and CreateRemote method to          
// create instances of the default interface IEventServerTrace exposed by              
// the CoClass EventServer. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoEventServer = class
    class function Create: IEventServerTrace;
    class function CreateRemote(const MachineName: string): IEventServerTrace;
  end;

// *********************************************************************//
// The Class CoTrackerServer provides a Create and CreateRemote method to          
// create instances of the default interface IReceiveAppData exposed by              
// the CoClass TrackerServer. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoTrackerServer = class
    class function Create: IReceiveAppData;
    class function CreateRemote(const MachineName: string): IReceiveAppData;
  end;

// *********************************************************************//
// The Class CoProcessDump provides a Create and CreateRemote method to          
// create instances of the default interface IProcessDump exposed by              
// the CoClass ProcessDump. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoProcessDump = class
    class function Create: IProcessDump;
    class function CreateRemote(const MachineName: string): IProcessDump;
  end;

// *********************************************************************//
// The Class CoPartitionMoniker provides a Create and CreateRemote method to          
// create instances of the default interface IMoniker exposed by              
// the CoClass PartitionMoniker. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoPartitionMoniker = class
    class function Create: IMoniker;
    class function CreateRemote(const MachineName: string): IMoniker;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TPartitionMoniker
// Help String      : 
// Default Interface: IMoniker
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TPartitionMonikerProperties= class;
{$ENDIF}
  TPartitionMoniker = class(TOleServer)
  private
    FIntf: IMoniker;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TPartitionMonikerProperties;
    function GetServerProperties: TPartitionMonikerProperties;
{$ENDIF}
    function GetDefaultInterface: IMoniker;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IMoniker);
    procedure Disconnect; override;
    function GetClassID(out pClassID: TGUID): HResult;
    function IsDirty: HResult;
    function Load(const pstm: IStream): HResult;
    function Save(const pstm: IStream; fClearDirty: Integer): HResult;
    function GetSizeMax(out pcbSize: _ULARGE_INTEGER): HResult;
    function RemoteBindToObject(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                                var riidResult: TGUID; out ppvResult: IUnknown): HResult;
    function RemoteBindToStorage(const pbc: IBindCtx; const pmkToLeft: IMoniker; var riid: TGUID; 
                                 out ppvObj: IUnknown): HResult;
    function Reduce(const pbc: IBindCtx; dwReduceHowFar: LongWord; var ppmkToLeft: IMoniker; 
                    out ppmkReduced: IMoniker): HResult;
    function ComposeWith(const pmkRight: IMoniker; fOnlyIfNotGeneric: Integer; 
                         out ppmkComposite: IMoniker): HResult;
    function Enum(fForward: Integer; out ppenumMoniker: IEnumMoniker): HResult;
    function IsEqual(const pmkOtherMoniker: IMoniker): HResult;
    function Hash(out pdwHash: LongWord): HResult;
    function IsRunning(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                       const pmkNewlyRunning: IMoniker): HResult;
    function GetTimeOfLastChange(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                                 out pfiletime: _FILETIME): HResult;
    function Inverse(out ppmk: IMoniker): HResult;
    function CommonPrefixWith(const pmkOther: IMoniker; out ppmkPrefix: IMoniker): HResult;
    function RelativePathTo(const pmkOther: IMoniker; out ppmkRelPath: IMoniker): HResult;
    function GetDisplayName(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                            out ppszDisplayName: PWideChar): HResult;
    function ParseDisplayName(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                              pszDisplayName: PWideChar; out pchEaten: LongWord; 
                              out ppmkOut: IMoniker): HResult;
    function IsSystemMoniker(out pdwMksys: LongWord): HResult;
    property DefaultInterface: IMoniker read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TPartitionMonikerProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TPartitionMoniker
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TPartitionMonikerProperties = class(TPersistent)
  private
    FServer:    TPartitionMoniker;
    function    GetDefaultInterface: IMoniker;
    constructor Create(AServer: TPartitionMoniker);
  protected
  public
    property DefaultInterface: IMoniker read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoSoapMoniker provides a Create and CreateRemote method to          
// create instances of the default interface IMoniker exposed by              
// the CoClass SoapMoniker. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSoapMoniker = class
    class function Create: IMoniker;
    class function CreateRemote(const MachineName: string): IMoniker;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TSoapMoniker
// Help String      : 
// Default Interface: IMoniker
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TSoapMonikerProperties= class;
{$ENDIF}
  TSoapMoniker = class(TOleServer)
  private
    FIntf: IMoniker;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TSoapMonikerProperties;
    function GetServerProperties: TSoapMonikerProperties;
{$ENDIF}
    function GetDefaultInterface: IMoniker;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IMoniker);
    procedure Disconnect; override;
    function GetClassID(out pClassID: TGUID): HResult;
    function IsDirty: HResult;
    function Load(const pstm: IStream): HResult;
    function Save(const pstm: IStream; fClearDirty: Integer): HResult;
    function GetSizeMax(out pcbSize: _ULARGE_INTEGER): HResult;
    function RemoteBindToObject(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                                var riidResult: TGUID; out ppvResult: IUnknown): HResult;
    function RemoteBindToStorage(const pbc: IBindCtx; const pmkToLeft: IMoniker; var riid: TGUID; 
                                 out ppvObj: IUnknown): HResult;
    function Reduce(const pbc: IBindCtx; dwReduceHowFar: LongWord; var ppmkToLeft: IMoniker; 
                    out ppmkReduced: IMoniker): HResult;
    function ComposeWith(const pmkRight: IMoniker; fOnlyIfNotGeneric: Integer; 
                         out ppmkComposite: IMoniker): HResult;
    function Enum(fForward: Integer; out ppenumMoniker: IEnumMoniker): HResult;
    function IsEqual(const pmkOtherMoniker: IMoniker): HResult;
    function Hash(out pdwHash: LongWord): HResult;
    function IsRunning(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                       const pmkNewlyRunning: IMoniker): HResult;
    function GetTimeOfLastChange(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                                 out pfiletime: _FILETIME): HResult;
    function Inverse(out ppmk: IMoniker): HResult;
    function CommonPrefixWith(const pmkOther: IMoniker; out ppmkPrefix: IMoniker): HResult;
    function RelativePathTo(const pmkOther: IMoniker; out ppmkRelPath: IMoniker): HResult;
    function GetDisplayName(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                            out ppszDisplayName: PWideChar): HResult;
    function ParseDisplayName(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                              pszDisplayName: PWideChar; out pchEaten: LongWord; 
                              out ppmkOut: IMoniker): HResult;
    function IsSystemMoniker(out pdwMksys: LongWord): HResult;
    property DefaultInterface: IMoniker read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TSoapMonikerProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TSoapMoniker
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TSoapMonikerProperties = class(TPersistent)
  private
    FServer:    TSoapMoniker;
    function    GetDefaultInterface: IMoniker;
    constructor Create(AServer: TSoapMoniker);
  protected
  public
    property DefaultInterface: IMoniker read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoAppDomainHelper provides a Create and CreateRemote method to          
// create instances of the default interface IAppDomainHelper exposed by              
// the CoClass AppDomainHelper. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoAppDomainHelper = class
    class function Create: IAppDomainHelper;
    class function CreateRemote(const MachineName: string): IAppDomainHelper;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TAppDomainHelper
// Help String      : 
// Default Interface: IAppDomainHelper
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TAppDomainHelperProperties= class;
{$ENDIF}
  TAppDomainHelper = class(TOleServer)
  private
    FIntf: IAppDomainHelper;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TAppDomainHelperProperties;
    function GetServerProperties: TAppDomainHelperProperties;
{$ENDIF}
    function GetDefaultInterface: IAppDomainHelper;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IAppDomainHelper);
    procedure Disconnect; override;
    function pfnShutdownCB(var pv: Pointer): HResult;
    function Initialize(const pUnkAD: IUnknown; 
                        const __MIDL__IAppDomainHelper0000: IAppDomainHelper; var pPool: Pointer): HResult;
    function pfnCallbackCB(var pv: Pointer): HResult;
    function DoCallback(const pUnkAD: IUnknown; 
                        const __MIDL__IAppDomainHelper0001: IAppDomainHelper; var pPool: Pointer): HResult;
    property DefaultInterface: IAppDomainHelper read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TAppDomainHelperProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TAppDomainHelper
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TAppDomainHelperProperties = class(TPersistent)
  private
    FServer:    TAppDomainHelper;
    function    GetDefaultInterface: IAppDomainHelper;
    constructor Create(AServer: TAppDomainHelper);
  protected
  public
    property DefaultInterface: IAppDomainHelper read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoClrAssemblyLocator provides a Create and CreateRemote method to          
// create instances of the default interface IAssemblyLocator exposed by              
// the CoClass ClrAssemblyLocator. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoClrAssemblyLocator = class
    class function Create: IAssemblyLocator;
    class function CreateRemote(const MachineName: string): IAssemblyLocator;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TClrAssemblyLocator
// Help String      : 
// Default Interface: IAssemblyLocator
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TClrAssemblyLocatorProperties= class;
{$ENDIF}
  TClrAssemblyLocator = class(TOleServer)
  private
    FIntf: IAssemblyLocator;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TClrAssemblyLocatorProperties;
    function GetServerProperties: TClrAssemblyLocatorProperties;
{$ENDIF}
    function GetDefaultInterface: IAssemblyLocator;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IAssemblyLocator);
    procedure Disconnect; override;
    function GetModules(const applicationDir: WideString; const applicationName: WideString; 
                        const assemblyName: WideString; out pModules: PSafeArray): HResult;
    property DefaultInterface: IAssemblyLocator read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TClrAssemblyLocatorProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TClrAssemblyLocator
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TClrAssemblyLocatorProperties = class(TPersistent)
  private
    FServer:    TClrAssemblyLocator;
    function    GetDefaultInterface: IAssemblyLocator;
    constructor Create(AServer: TClrAssemblyLocator);
  protected
  public
    property DefaultInterface: IAssemblyLocator read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

resourcestring
  dtlServerPage = '(none)';

  dtlOcxPage = '(none)';

implementation

uses ComObj;

class function CoSecurityCertificate.Create: ISecurityCertificateColl;
begin
  Result := CreateComObject(CLASS_SecurityCertificate) as ISecurityCertificateColl;
end;

class function CoSecurityCertificate.CreateRemote(const MachineName: string): ISecurityCertificateColl;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SecurityCertificate) as ISecurityCertificateColl;
end;

class function CoSecurityIdentity.Create: ISecurityIdentityColl;
begin
  Result := CreateComObject(CLASS_SecurityIdentity) as ISecurityIdentityColl;
end;

class function CoSecurityIdentity.CreateRemote(const MachineName: string): ISecurityIdentityColl;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SecurityIdentity) as ISecurityIdentityColl;
end;

class function CoSecurityCallers.Create: ISecurityCallersColl;
begin
  Result := CreateComObject(CLASS_SecurityCallers) as ISecurityCallersColl;
end;

class function CoSecurityCallers.CreateRemote(const MachineName: string): ISecurityCallersColl;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SecurityCallers) as ISecurityCallersColl;
end;

class function CoSecurityCallContext.Create: ISecurityCallContext;
begin
  Result := CreateComObject(CLASS_SecurityCallContext) as ISecurityCallContext;
end;

class function CoSecurityCallContext.CreateRemote(const MachineName: string): ISecurityCallContext;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SecurityCallContext) as ISecurityCallContext;
end;

class function CoGetSecurityCallContextAppObject.Create: IGetSecurityCallContext;
begin
  Result := CreateComObject(CLASS_GetSecurityCallContextAppObject) as IGetSecurityCallContext;
end;

class function CoGetSecurityCallContextAppObject.CreateRemote(const MachineName: string): IGetSecurityCallContext;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_GetSecurityCallContextAppObject) as IGetSecurityCallContext;
end;

class function CoDummy30040732.Create: IObjectContext;
begin
  Result := CreateComObject(CLASS_Dummy30040732) as IObjectContext;
end;

class function CoDummy30040732.CreateRemote(const MachineName: string): IObjectContext;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Dummy30040732) as IObjectContext;
end;

class function CoAppServer.Create: IMTxAS;
begin
  Result := CreateComObject(CLASS_AppServer) as IMTxAS;
end;

class function CoAppServer.CreateRemote(const MachineName: string): IMTxAS;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_AppServer) as IMTxAS;
end;

class function CoTransactionContext.Create: ITransactionContext;
begin
  Result := CreateComObject(CLASS_TransactionContext) as ITransactionContext;
end;

class function CoTransactionContext.CreateRemote(const MachineName: string): ITransactionContext;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_TransactionContext) as ITransactionContext;
end;

procedure TTransactionContext.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{7999FC25-D3C6-11CF-ACAB-00A024A55AEF}';
    IntfIID:   '{7999FC21-D3C6-11CF-ACAB-00A024A55AEF}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TTransactionContext.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ITransactionContext;
  end;
end;

procedure TTransactionContext.ConnectTo(svrIntf: ITransactionContext);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TTransactionContext.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TTransactionContext.GetDefaultInterface: ITransactionContext;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TTransactionContext.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TTransactionContextProperties.Create(Self);
{$ENDIF}
end;

destructor TTransactionContext.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TTransactionContext.GetServerProperties: TTransactionContextProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TTransactionContext.CreateInstance(const pszProgId: WideString): OleVariant;
begin
  Result := DefaultInterface.CreateInstance(pszProgId);
end;

procedure TTransactionContext.Commit;
begin
  DefaultInterface.Commit;
end;

procedure TTransactionContext.Abort;
begin
  DefaultInterface.Abort;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TTransactionContextProperties.Create(AServer: TTransactionContext);
begin
  inherited Create;
  FServer := AServer;
end;

function TTransactionContextProperties.GetDefaultInterface: ITransactionContext;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoTransactionContextEx.Create: ITransactionContextEx;
begin
  Result := CreateComObject(CLASS_TransactionContextEx) as ITransactionContextEx;
end;

class function CoTransactionContextEx.CreateRemote(const MachineName: string): ITransactionContextEx;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_TransactionContextEx) as ITransactionContextEx;
end;

procedure TTransactionContextEx.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{5CB66670-D3D4-11CF-ACAB-00A024A55AEF}';
    IntfIID:   '{7999FC22-D3C6-11CF-ACAB-00A024A55AEF}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TTransactionContextEx.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ITransactionContextEx;
  end;
end;

procedure TTransactionContextEx.ConnectTo(svrIntf: ITransactionContextEx);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TTransactionContextEx.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TTransactionContextEx.GetDefaultInterface: ITransactionContextEx;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TTransactionContextEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TTransactionContextExProperties.Create(Self);
{$ENDIF}
end;

destructor TTransactionContextEx.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TTransactionContextEx.GetServerProperties: TTransactionContextExProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TTransactionContextEx.CreateInstance(var rclsid: TGUID; var riid: TGUID; 
                                              out pObject: Pointer): HResult;
begin
  Result := DefaultInterface.CreateInstance(rclsid, riid, pObject);
end;

function TTransactionContextEx.Commit: HResult;
begin
  Result := DefaultInterface.Commit;
end;

function TTransactionContextEx.Abort: HResult;
begin
  Result := DefaultInterface.Abort;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TTransactionContextExProperties.Create(AServer: TTransactionContextEx);
begin
  inherited Create;
  FServer := AServer;
end;

function TTransactionContextExProperties.GetDefaultInterface: ITransactionContextEx;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoByotServerEx.Create: ICreateWithTipTransactionEx;
begin
  Result := CreateComObject(CLASS_ByotServerEx) as ICreateWithTipTransactionEx;
end;

class function CoByotServerEx.CreateRemote(const MachineName: string): ICreateWithTipTransactionEx;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ByotServerEx) as ICreateWithTipTransactionEx;
end;

class function CoCServiceConfig.Create: IServiceInheritanceConfig;
begin
  Result := CreateComObject(CLASS_CServiceConfig) as IServiceInheritanceConfig;
end;

class function CoCServiceConfig.CreateRemote(const MachineName: string): IServiceInheritanceConfig;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CServiceConfig) as IServiceInheritanceConfig;
end;

procedure TCServiceConfig.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{ECABB0C8-7F19-11D2-978E-0000F8757E2A}';
    IntfIID:   '{92186771-D3B4-4D77-A8EA-EE842D586F35}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCServiceConfig.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IServiceInheritanceConfig;
  end;
end;

procedure TCServiceConfig.ConnectTo(svrIntf: IServiceInheritanceConfig);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCServiceConfig.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCServiceConfig.GetDefaultInterface: IServiceInheritanceConfig;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCServiceConfig.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TCServiceConfigProperties.Create(Self);
{$ENDIF}
end;

destructor TCServiceConfig.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TCServiceConfig.GetServerProperties: TCServiceConfigProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TCServiceConfig.ContainingContextTreatment(inheritanceConfig: tagCSC_InheritanceConfig): HResult;
begin
  Result := DefaultInterface.ContainingContextTreatment(inheritanceConfig);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TCServiceConfigProperties.Create(AServer: TCServiceConfig);
begin
  inherited Create;
  FServer := AServer;
end;

function TCServiceConfigProperties.GetDefaultInterface: IServiceInheritanceConfig;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoServicePool.Create: IServicePool;
begin
  Result := CreateComObject(CLASS_ServicePool) as IServicePool;
end;

class function CoServicePool.CreateRemote(const MachineName: string): IServicePool;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ServicePool) as IServicePool;
end;

procedure TServicePool.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{ECABB0C9-7F19-11D2-978E-0000F8757E2A}';
    IntfIID:   '{B302DF81-EA45-451E-99A2-09F9FD1B1E13}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TServicePool.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IServicePool;
  end;
end;

procedure TServicePool.ConnectTo(svrIntf: IServicePool);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TServicePool.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TServicePool.GetDefaultInterface: IServicePool;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TServicePool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TServicePoolProperties.Create(Self);
{$ENDIF}
end;

destructor TServicePool.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TServicePool.GetServerProperties: TServicePoolProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TServicePool.Initialize(const pPoolConfig: IUnknown): HResult;
begin
  Result := DefaultInterface.Initialize(pPoolConfig);
end;

function TServicePool.GetObject(var riid: TGUID; out ppv: Pointer): HResult;
begin
  Result := DefaultInterface.GetObject(riid, ppv);
end;

function TServicePool.Shutdown: HResult;
begin
  Result := DefaultInterface.Shutdown;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TServicePoolProperties.Create(AServer: TServicePool);
begin
  inherited Create;
  FServer := AServer;
end;

function TServicePoolProperties.GetDefaultInterface: IServicePool;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoServicePoolConfig.Create: IServicePoolConfig;
begin
  Result := CreateComObject(CLASS_ServicePoolConfig) as IServicePoolConfig;
end;

class function CoServicePoolConfig.CreateRemote(const MachineName: string): IServicePoolConfig;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ServicePoolConfig) as IServicePoolConfig;
end;

procedure TServicePoolConfig.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{ECABB0CA-7F19-11D2-978E-0000F8757E2A}';
    IntfIID:   '{A9690656-5BCA-470C-8451-250C1F43A33E}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TServicePoolConfig.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IServicePoolConfig;
  end;
end;

procedure TServicePoolConfig.ConnectTo(svrIntf: IServicePoolConfig);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TServicePoolConfig.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TServicePoolConfig.GetDefaultInterface: IServicePoolConfig;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TServicePoolConfig.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TServicePoolConfigProperties.Create(Self);
{$ENDIF}
end;

destructor TServicePoolConfig.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TServicePoolConfig.GetServerProperties: TServicePoolConfigProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TServicePoolConfig.Set_MaxPoolSize(pdwMaxPool: LongWord): HResult;
  { Warning: The property MaxPoolSize has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.MaxPoolSize := pdwMaxPool;
  Result := S_OK;
end;

function TServicePoolConfig.Get_MaxPoolSize(out pdwMaxPool: LongWord): HResult;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.MaxPoolSize;
end;

function TServicePoolConfig.Set_MinPoolSize(pdwMinPool: LongWord): HResult;
  { Warning: The property MinPoolSize has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.MinPoolSize := pdwMinPool;
  Result := S_OK;
end;

function TServicePoolConfig.Get_MinPoolSize(out pdwMinPool: LongWord): HResult;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.MinPoolSize;
end;

function TServicePoolConfig.Set_CreationTimeout(pdwCreationTimeout: LongWord): HResult;
  { Warning: The property CreationTimeout has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.CreationTimeout := pdwCreationTimeout;
  Result := S_OK;
end;

function TServicePoolConfig.Get_CreationTimeout(out pdwCreationTimeout: LongWord): HResult;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.CreationTimeout;
end;

function TServicePoolConfig.Set_TransactionAffinity(pfTxAffinity: Integer): HResult;
  { Warning: The property TransactionAffinity has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.TransactionAffinity := pfTxAffinity;
  Result := S_OK;
end;

function TServicePoolConfig.Get_TransactionAffinity(out pfTxAffinity: Integer): HResult;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.TransactionAffinity;
end;

function TServicePoolConfig.Set_ClassFactory(const pFactory: IClassFactory): HResult;
  { Warning: The property ClassFactory has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ClassFactory := pFactory;
  Result := S_OK;
end;

function TServicePoolConfig.Get_ClassFactory(out pFactory: IClassFactory): HResult;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.ClassFactory;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TServicePoolConfigProperties.Create(AServer: TServicePoolConfig);
begin
  inherited Create;
  FServer := AServer;
end;

function TServicePoolConfigProperties.GetDefaultInterface: IServicePoolConfig;
begin
  Result := FServer.DefaultInterface;
end;

function TServicePoolConfigProperties.Set_MaxPoolSize(pdwMaxPool: LongWord): HResult;
  { Warning: The property MaxPoolSize has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.MaxPoolSize := pdwMaxPool;
  Result := S_OK;
end;

function TServicePoolConfigProperties.Get_MaxPoolSize(out pdwMaxPool: LongWord): HResult;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.MaxPoolSize;
end;

function TServicePoolConfigProperties.Set_MinPoolSize(pdwMinPool: LongWord): HResult;
  { Warning: The property MinPoolSize has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.MinPoolSize := pdwMinPool;
  Result := S_OK;
end;

function TServicePoolConfigProperties.Get_MinPoolSize(out pdwMinPool: LongWord): HResult;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.MinPoolSize;
end;

function TServicePoolConfigProperties.Set_CreationTimeout(pdwCreationTimeout: LongWord): HResult;
  { Warning: The property CreationTimeout has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.CreationTimeout := pdwCreationTimeout;
  Result := S_OK;
end;

function TServicePoolConfigProperties.Get_CreationTimeout(out pdwCreationTimeout: LongWord): HResult;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.CreationTimeout;
end;

function TServicePoolConfigProperties.Set_TransactionAffinity(pfTxAffinity: Integer): HResult;
  { Warning: The property TransactionAffinity has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.TransactionAffinity := pfTxAffinity;
  Result := S_OK;
end;

function TServicePoolConfigProperties.Get_TransactionAffinity(out pfTxAffinity: Integer): HResult;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.TransactionAffinity;
end;

function TServicePoolConfigProperties.Set_ClassFactory(const pFactory: IClassFactory): HResult;
  { Warning: The property ClassFactory has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ClassFactory := pFactory;
  Result := S_OK;
end;

function TServicePoolConfigProperties.Get_ClassFactory(out pFactory: IClassFactory): HResult;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.ClassFactory;
end;

{$ENDIF}

class function CoSharedProperty.Create: ISharedProperty;
begin
  Result := CreateComObject(CLASS_SharedProperty) as ISharedProperty;
end;

class function CoSharedProperty.CreateRemote(const MachineName: string): ISharedProperty;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SharedProperty) as ISharedProperty;
end;

class function CoSharedPropertyGroup.Create: ISharedPropertyGroup;
begin
  Result := CreateComObject(CLASS_SharedPropertyGroup) as ISharedPropertyGroup;
end;

class function CoSharedPropertyGroup.CreateRemote(const MachineName: string): ISharedPropertyGroup;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SharedPropertyGroup) as ISharedPropertyGroup;
end;

class function CoSharedPropertyGroupManager.Create: ISharedPropertyGroupManager;
begin
  Result := CreateComObject(CLASS_SharedPropertyGroupManager) as ISharedPropertyGroupManager;
end;

class function CoSharedPropertyGroupManager.CreateRemote(const MachineName: string): ISharedPropertyGroupManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SharedPropertyGroupManager) as ISharedPropertyGroupManager;
end;

procedure TSharedPropertyGroupManager.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{2A005C11-A5DE-11CF-9E66-00AA00A3F464}';
    IntfIID:   '{2A005C0D-A5DE-11CF-9E66-00AA00A3F464}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TSharedPropertyGroupManager.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ISharedPropertyGroupManager;
  end;
end;

procedure TSharedPropertyGroupManager.ConnectTo(svrIntf: ISharedPropertyGroupManager);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TSharedPropertyGroupManager.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TSharedPropertyGroupManager.GetDefaultInterface: ISharedPropertyGroupManager;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TSharedPropertyGroupManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TSharedPropertyGroupManagerProperties.Create(Self);
{$ENDIF}
end;

destructor TSharedPropertyGroupManager.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TSharedPropertyGroupManager.GetServerProperties: TSharedPropertyGroupManagerProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TSharedPropertyGroupManager.Get_Group(const name: WideString): ISharedPropertyGroup;
begin
    Result := DefaultInterface.Group[name];
end;

function TSharedPropertyGroupManager.CreatePropertyGroup(const name: WideString; 
                                                         var dwIsoMode: Integer; 
                                                         var dwRelMode: Integer; 
                                                         out fExists: WordBool): ISharedPropertyGroup;
begin
  Result := DefaultInterface.CreatePropertyGroup(name, dwIsoMode, dwRelMode, fExists);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TSharedPropertyGroupManagerProperties.Create(AServer: TSharedPropertyGroupManager);
begin
  inherited Create;
  FServer := AServer;
end;

function TSharedPropertyGroupManagerProperties.GetDefaultInterface: ISharedPropertyGroupManager;
begin
  Result := FServer.DefaultInterface;
end;

function TSharedPropertyGroupManagerProperties.Get_Group(const name: WideString): ISharedPropertyGroup;
begin
    Result := DefaultInterface.Group[name];
end;

{$ENDIF}

class function CoCOMEvents.Create: IMtsEvents;
begin
  Result := CreateComObject(CLASS_COMEvents) as IMtsEvents;
end;

class function CoCOMEvents.CreateRemote(const MachineName: string): IMtsEvents;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_COMEvents) as IMtsEvents;
end;

procedure TCOMEvents.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{ECABB0AB-7F19-11D2-978E-0000F8757E2A}';
    IntfIID:   '{BACEDF4D-74AB-11D0-B162-00AA00BA3258}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCOMEvents.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IMtsEvents;
  end;
end;

procedure TCOMEvents.ConnectTo(svrIntf: IMtsEvents);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCOMEvents.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCOMEvents.GetDefaultInterface: IMtsEvents;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCOMEvents.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TCOMEventsProperties.Create(Self);
{$ENDIF}
end;

destructor TCOMEvents.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TCOMEvents.GetServerProperties: TCOMEventsProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TCOMEvents.Get_PackageName: WideString;
begin
    Result := DefaultInterface.PackageName;
end;

function TCOMEvents.Get_PackageGuid: WideString;
begin
    Result := DefaultInterface.PackageGuid;
end;

function TCOMEvents.Get_FireEvents: WordBool;
begin
    Result := DefaultInterface.FireEvents;
end;

procedure TCOMEvents.PostEvent(var vEvent: OleVariant);
begin
  DefaultInterface.PostEvent(vEvent);
end;

function TCOMEvents.GetProcessID: Integer;
begin
  Result := DefaultInterface.GetProcessID;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TCOMEventsProperties.Create(AServer: TCOMEvents);
begin
  inherited Create;
  FServer := AServer;
end;

function TCOMEventsProperties.GetDefaultInterface: IMtsEvents;
begin
  Result := FServer.DefaultInterface;
end;

function TCOMEventsProperties.Get_PackageName: WideString;
begin
    Result := DefaultInterface.PackageName;
end;

function TCOMEventsProperties.Get_PackageGuid: WideString;
begin
    Result := DefaultInterface.PackageGuid;
end;

function TCOMEventsProperties.Get_FireEvents: WordBool;
begin
    Result := DefaultInterface.FireEvents;
end;

{$ENDIF}

class function CoCoMTSLocator.Create: IMTSLocator;
begin
  Result := CreateComObject(CLASS_CoMTSLocator) as IMTSLocator;
end;

class function CoCoMTSLocator.CreateRemote(const MachineName: string): IMTSLocator;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CoMTSLocator) as IMTSLocator;
end;

procedure TCoMTSLocator.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{ECABB0AC-7F19-11D2-978E-0000F8757E2A}';
    IntfIID:   '{D19B8BFD-7F88-11D0-B16E-00AA00BA3258}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCoMTSLocator.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IMTSLocator;
  end;
end;

procedure TCoMTSLocator.ConnectTo(svrIntf: IMTSLocator);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCoMTSLocator.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCoMTSLocator.GetDefaultInterface: IMTSLocator;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCoMTSLocator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TCoMTSLocatorProperties.Create(Self);
{$ENDIF}
end;

destructor TCoMTSLocator.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TCoMTSLocator.GetServerProperties: TCoMTSLocatorProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TCoMTSLocator.GetEventDispatcher: IUnknown;
begin
  Result := DefaultInterface.GetEventDispatcher;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TCoMTSLocatorProperties.Create(AServer: TCoMTSLocator);
begin
  inherited Create;
  FServer := AServer;
end;

function TCoMTSLocatorProperties.GetDefaultInterface: IMTSLocator;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoMtsGrp.Create: IMtsGrp;
begin
  Result := CreateComObject(CLASS_MtsGrp) as IMtsGrp;
end;

class function CoMtsGrp.CreateRemote(const MachineName: string): IMtsGrp;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_MtsGrp) as IMtsGrp;
end;

procedure TMtsGrp.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{4B2E958D-0393-11D1-B1AB-00AA00BA3258}';
    IntfIID:   '{4B2E958C-0393-11D1-B1AB-00AA00BA3258}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TMtsGrp.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IMtsGrp;
  end;
end;

procedure TMtsGrp.ConnectTo(svrIntf: IMtsGrp);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TMtsGrp.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TMtsGrp.GetDefaultInterface: IMtsGrp;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TMtsGrp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TMtsGrpProperties.Create(Self);
{$ENDIF}
end;

destructor TMtsGrp.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TMtsGrp.GetServerProperties: TMtsGrpProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TMtsGrp.Get_Count: Integer;
begin
    Result := DefaultInterface.Count;
end;

procedure TMtsGrp.Item(lIndex: Integer; out ppUnkDispatcher: IUnknown);
begin
  DefaultInterface.Item(lIndex, ppUnkDispatcher);
end;

procedure TMtsGrp.Refresh;
begin
  DefaultInterface.Refresh;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TMtsGrpProperties.Create(AServer: TMtsGrp);
begin
  inherited Create;
  FServer := AServer;
end;

function TMtsGrpProperties.GetDefaultInterface: IMtsGrp;
begin
  Result := FServer.DefaultInterface;
end;

function TMtsGrpProperties.Get_Count: Integer;
begin
    Result := DefaultInterface.Count;
end;

{$ENDIF}

class function CoComServiceEvents.Create: IComThreadEvents;
begin
  Result := CreateComObject(CLASS_ComServiceEvents) as IComThreadEvents;
end;

class function CoComServiceEvents.CreateRemote(const MachineName: string): IComThreadEvents;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ComServiceEvents) as IComThreadEvents;
end;

class function CoComSystemAppEventData.Create: ISystemAppEventData;
begin
  Result := CreateComObject(CLASS_ComSystemAppEventData) as ISystemAppEventData;
end;

class function CoComSystemAppEventData.CreateRemote(const MachineName: string): ISystemAppEventData;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ComSystemAppEventData) as ISystemAppEventData;
end;

class function CoCRMClerk.Create: ICrmLogControl;
begin
  Result := CreateComObject(CLASS_CRMClerk) as ICrmLogControl;
end;

class function CoCRMClerk.CreateRemote(const MachineName: string): ICrmLogControl;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CRMClerk) as ICrmLogControl;
end;

procedure TCRMClerk.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{ECABB0BD-7F19-11D2-978E-0000F8757E2A}';
    IntfIID:   '{A0E174B3-D26E-11D2-8F84-00805FC7BCD9}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCRMClerk.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ICrmLogControl;
  end;
end;

procedure TCRMClerk.ConnectTo(svrIntf: ICrmLogControl);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCRMClerk.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCRMClerk.GetDefaultInterface: ICrmLogControl;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCRMClerk.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TCRMClerkProperties.Create(Self);
{$ENDIF}
end;

destructor TCRMClerk.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TCRMClerk.GetServerProperties: TCRMClerkProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TCRMClerk.Get_TransactionUOW(out pVal: WideString): HResult;
begin
    Result := DefaultInterface.Get_TransactionUOW(pVal);
end;

function TCRMClerk.RegisterCompensator(lpcwstrProgIdCompensator: PWideChar; 
                                       lpcwstrDescription: PWideChar; lCrmRegFlags: Integer): HResult;
begin
  Result := DefaultInterface.RegisterCompensator(lpcwstrProgIdCompensator, lpcwstrDescription, 
                                                 lCrmRegFlags);
end;

function TCRMClerk.WriteLogRecordVariants(var pLogRecord: OleVariant): HResult;
begin
  Result := DefaultInterface.WriteLogRecordVariants(pLogRecord);
end;

function TCRMClerk.ForceLog: HResult;
begin
  Result := DefaultInterface.ForceLog;
end;

function TCRMClerk.ForgetLogRecord: HResult;
begin
  Result := DefaultInterface.ForgetLogRecord;
end;

function TCRMClerk.ForceTransactionToAbort: HResult;
begin
  Result := DefaultInterface.ForceTransactionToAbort;
end;

function TCRMClerk.WriteLogRecord(var rgBlob: tagBLOB; cBlob: LongWord): HResult;
begin
  Result := DefaultInterface.WriteLogRecord(rgBlob, cBlob);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TCRMClerkProperties.Create(AServer: TCRMClerk);
begin
  inherited Create;
  FServer := AServer;
end;

function TCRMClerkProperties.GetDefaultInterface: ICrmLogControl;
begin
  Result := FServer.DefaultInterface;
end;

function TCRMClerkProperties.Get_TransactionUOW(out pVal: WideString): HResult;
begin
    Result := DefaultInterface.Get_TransactionUOW(pVal);
end;

{$ENDIF}

class function CoCRMRecoveryClerk.Create: ICrmMonitor;
begin
  Result := CreateComObject(CLASS_CRMRecoveryClerk) as ICrmMonitor;
end;

class function CoCRMRecoveryClerk.CreateRemote(const MachineName: string): ICrmMonitor;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CRMRecoveryClerk) as ICrmMonitor;
end;

procedure TCRMRecoveryClerk.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{ECABB0BE-7F19-11D2-978E-0000F8757E2A}';
    IntfIID:   '{70C8E443-C7ED-11D1-82FB-00A0C91EEDE9}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCRMRecoveryClerk.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ICrmMonitor;
  end;
end;

procedure TCRMRecoveryClerk.ConnectTo(svrIntf: ICrmMonitor);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCRMRecoveryClerk.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCRMRecoveryClerk.GetDefaultInterface: ICrmMonitor;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCRMRecoveryClerk.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TCRMRecoveryClerkProperties.Create(Self);
{$ENDIF}
end;

destructor TCRMRecoveryClerk.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TCRMRecoveryClerk.GetServerProperties: TCRMRecoveryClerkProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TCRMRecoveryClerk.GetClerks(out pClerks: ICrmMonitorClerks): HResult;
begin
  Result := DefaultInterface.GetClerks(pClerks);
end;

function TCRMRecoveryClerk.HoldClerk(Index: OleVariant; out pItem: OleVariant): HResult;
begin
  Result := DefaultInterface.HoldClerk(Index, pItem);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TCRMRecoveryClerkProperties.Create(AServer: TCRMRecoveryClerk);
begin
  inherited Create;
  FServer := AServer;
end;

function TCRMRecoveryClerkProperties.GetDefaultInterface: ICrmMonitor;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoMessageMover.Create: IMessageMover;
begin
  Result := CreateComObject(CLASS_MessageMover) as IMessageMover;
end;

class function CoMessageMover.CreateRemote(const MachineName: string): IMessageMover;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_MessageMover) as IMessageMover;
end;

procedure TMessageMover.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{ECABB0BF-7F19-11D2-978E-0000F8757E2A}';
    IntfIID:   '{588A085A-B795-11D1-8054-00C04FC340EE}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TMessageMover.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IMessageMover;
  end;
end;

procedure TMessageMover.ConnectTo(svrIntf: IMessageMover);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TMessageMover.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TMessageMover.GetDefaultInterface: IMessageMover;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TMessageMover.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TMessageMoverProperties.Create(Self);
{$ENDIF}
end;

destructor TMessageMover.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TMessageMover.GetServerProperties: TMessageMoverProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TMessageMover.Get_SourcePath: WideString;
begin
    Result := DefaultInterface.SourcePath;
end;

procedure TMessageMover.Set_SourcePath(const pVal: WideString);
  { Warning: The property SourcePath has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.SourcePath := pVal;
end;

function TMessageMover.Get_DestPath: WideString;
begin
    Result := DefaultInterface.DestPath;
end;

procedure TMessageMover.Set_DestPath(const pVal: WideString);
  { Warning: The property DestPath has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.DestPath := pVal;
end;

function TMessageMover.Get_CommitBatchSize: Integer;
begin
    Result := DefaultInterface.CommitBatchSize;
end;

procedure TMessageMover.Set_CommitBatchSize(pVal: Integer);
begin
  DefaultInterface.Set_CommitBatchSize(pVal);
end;

function TMessageMover.MoveMessages: Integer;
begin
  Result := DefaultInterface.MoveMessages;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TMessageMoverProperties.Create(AServer: TMessageMover);
begin
  inherited Create;
  FServer := AServer;
end;

function TMessageMoverProperties.GetDefaultInterface: IMessageMover;
begin
  Result := FServer.DefaultInterface;
end;

function TMessageMoverProperties.Get_SourcePath: WideString;
begin
    Result := DefaultInterface.SourcePath;
end;

procedure TMessageMoverProperties.Set_SourcePath(const pVal: WideString);
  { Warning: The property SourcePath has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.SourcePath := pVal;
end;

function TMessageMoverProperties.Get_DestPath: WideString;
begin
    Result := DefaultInterface.DestPath;
end;

procedure TMessageMoverProperties.Set_DestPath(const pVal: WideString);
  { Warning: The property DestPath has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.DestPath := pVal;
end;

function TMessageMoverProperties.Get_CommitBatchSize: Integer;
begin
    Result := DefaultInterface.CommitBatchSize;
end;

procedure TMessageMoverProperties.Set_CommitBatchSize(pVal: Integer);
begin
  DefaultInterface.Set_CommitBatchSize(pVal);
end;

{$ENDIF}

class function CoDispenserManager.Create: IDispenserManager;
begin
  Result := CreateComObject(CLASS_DispenserManager) as IDispenserManager;
end;

class function CoDispenserManager.CreateRemote(const MachineName: string): IDispenserManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DispenserManager) as IDispenserManager;
end;

class function CoPoolMgr.Create: IPoolManager;
begin
  Result := CreateComObject(CLASS_PoolMgr) as IPoolManager;
end;

class function CoPoolMgr.CreateRemote(const MachineName: string): IPoolManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_PoolMgr) as IPoolManager;
end;

procedure TPoolMgr.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{ECABAFB5-7F19-11D2-978E-0000F8757E2A}';
    IntfIID:   '{0A469861-5A91-43A0-99B6-D5E179BB0631}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TPoolMgr.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IPoolManager;
  end;
end;

procedure TPoolMgr.ConnectTo(svrIntf: IPoolManager);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TPoolMgr.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TPoolMgr.GetDefaultInterface: IPoolManager;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TPoolMgr.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TPoolMgrProperties.Create(Self);
{$ENDIF}
end;

destructor TPoolMgr.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TPoolMgr.GetServerProperties: TPoolMgrProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TPoolMgr.ShutdownPool(const CLSIDOrProgID: WideString): HResult;
begin
  Result := DefaultInterface.ShutdownPool(CLSIDOrProgID);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TPoolMgrProperties.Create(AServer: TPoolMgr);
begin
  inherited Create;
  FServer := AServer;
end;

function TPoolMgrProperties.GetDefaultInterface: IPoolManager;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoEventServer.Create: IEventServerTrace;
begin
  Result := CreateComObject(CLASS_EventServer) as IEventServerTrace;
end;

class function CoEventServer.CreateRemote(const MachineName: string): IEventServerTrace;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EventServer) as IEventServerTrace;
end;

class function CoTrackerServer.Create: IReceiveAppData;
begin
  Result := CreateComObject(CLASS_TrackerServer) as IReceiveAppData;
end;

class function CoTrackerServer.CreateRemote(const MachineName: string): IReceiveAppData;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_TrackerServer) as IReceiveAppData;
end;

class function CoProcessDump.Create: IProcessDump;
begin
  Result := CreateComObject(CLASS_ProcessDump) as IProcessDump;
end;

class function CoProcessDump.CreateRemote(const MachineName: string): IProcessDump;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ProcessDump) as IProcessDump;
end;

class function CoPartitionMoniker.Create: IMoniker;
begin
  Result := CreateComObject(CLASS_PartitionMoniker) as IMoniker;
end;

class function CoPartitionMoniker.CreateRemote(const MachineName: string): IMoniker;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_PartitionMoniker) as IMoniker;
end;

procedure TPartitionMoniker.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{ECABB0C5-7F19-11D2-978E-0000F8757E2A}';
    IntfIID:   '{0000000F-0000-0000-C000-000000000046}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TPartitionMoniker.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IMoniker;
  end;
end;

procedure TPartitionMoniker.ConnectTo(svrIntf: IMoniker);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TPartitionMoniker.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TPartitionMoniker.GetDefaultInterface: IMoniker;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TPartitionMoniker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TPartitionMonikerProperties.Create(Self);
{$ENDIF}
end;

destructor TPartitionMoniker.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TPartitionMoniker.GetServerProperties: TPartitionMonikerProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TPartitionMoniker.GetClassID(out pClassID: TGUID): HResult;
begin
  Result := DefaultInterface.GetClassID(pClassID);
end;

function TPartitionMoniker.IsDirty: HResult;
begin
  Result := DefaultInterface.IsDirty;
end;

function TPartitionMoniker.Load(const pstm: IStream): HResult;
begin
  Result := DefaultInterface.Load(pstm);
end;

function TPartitionMoniker.Save(const pstm: IStream; fClearDirty: Integer): HResult;
begin
  Result := DefaultInterface.Save(pstm, fClearDirty);
end;

function TPartitionMoniker.GetSizeMax(out pcbSize: _ULARGE_INTEGER): HResult;
begin
  Result := DefaultInterface.GetSizeMax(pcbSize);
end;

function TPartitionMoniker.RemoteBindToObject(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                                              var riidResult: TGUID; out ppvResult: IUnknown): HResult;
begin
  Result := DefaultInterface.RemoteBindToObject(pbc, pmkToLeft, riidResult, ppvResult);
end;

function TPartitionMoniker.RemoteBindToStorage(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                                               var riid: TGUID; out ppvObj: IUnknown): HResult;
begin
  Result := DefaultInterface.RemoteBindToStorage(pbc, pmkToLeft, riid, ppvObj);
end;

function TPartitionMoniker.Reduce(const pbc: IBindCtx; dwReduceHowFar: LongWord; 
                                  var ppmkToLeft: IMoniker; out ppmkReduced: IMoniker): HResult;
begin
  Result := DefaultInterface.Reduce(pbc, dwReduceHowFar, ppmkToLeft, ppmkReduced);
end;

function TPartitionMoniker.ComposeWith(const pmkRight: IMoniker; fOnlyIfNotGeneric: Integer; 
                                       out ppmkComposite: IMoniker): HResult;
begin
  Result := DefaultInterface.ComposeWith(pmkRight, fOnlyIfNotGeneric, ppmkComposite);
end;

function TPartitionMoniker.Enum(fForward: Integer; out ppenumMoniker: IEnumMoniker): HResult;
begin
  Result := DefaultInterface.Enum(fForward, ppenumMoniker);
end;

function TPartitionMoniker.IsEqual(const pmkOtherMoniker: IMoniker): HResult;
begin
  Result := DefaultInterface.IsEqual(pmkOtherMoniker);
end;

function TPartitionMoniker.Hash(out pdwHash: LongWord): HResult;
begin
  Result := DefaultInterface.Hash(pdwHash);
end;

function TPartitionMoniker.IsRunning(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                                     const pmkNewlyRunning: IMoniker): HResult;
begin
  Result := DefaultInterface.IsRunning(pbc, pmkToLeft, pmkNewlyRunning);
end;

function TPartitionMoniker.GetTimeOfLastChange(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                                               out pfiletime: _FILETIME): HResult;
begin
  Result := DefaultInterface.GetTimeOfLastChange(pbc, pmkToLeft, pfiletime);
end;

function TPartitionMoniker.Inverse(out ppmk: IMoniker): HResult;
begin
  Result := DefaultInterface.Inverse(ppmk);
end;

function TPartitionMoniker.CommonPrefixWith(const pmkOther: IMoniker; out ppmkPrefix: IMoniker): HResult;
begin
  Result := DefaultInterface.CommonPrefixWith(pmkOther, ppmkPrefix);
end;

function TPartitionMoniker.RelativePathTo(const pmkOther: IMoniker; out ppmkRelPath: IMoniker): HResult;
begin
  Result := DefaultInterface.RelativePathTo(pmkOther, ppmkRelPath);
end;

function TPartitionMoniker.GetDisplayName(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                                          out ppszDisplayName: PWideChar): HResult;
begin
  Result := DefaultInterface.GetDisplayName(pbc, pmkToLeft, ppszDisplayName);
end;

function TPartitionMoniker.ParseDisplayName(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                                            pszDisplayName: PWideChar; out pchEaten: LongWord; 
                                            out ppmkOut: IMoniker): HResult;
begin
  Result := DefaultInterface.ParseDisplayName(pbc, pmkToLeft, pszDisplayName, pchEaten, ppmkOut);
end;

function TPartitionMoniker.IsSystemMoniker(out pdwMksys: LongWord): HResult;
begin
  Result := DefaultInterface.IsSystemMoniker(pdwMksys);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TPartitionMonikerProperties.Create(AServer: TPartitionMoniker);
begin
  inherited Create;
  FServer := AServer;
end;

function TPartitionMonikerProperties.GetDefaultInterface: IMoniker;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoSoapMoniker.Create: IMoniker;
begin
  Result := CreateComObject(CLASS_SoapMoniker) as IMoniker;
end;

class function CoSoapMoniker.CreateRemote(const MachineName: string): IMoniker;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SoapMoniker) as IMoniker;
end;

procedure TSoapMoniker.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{ECABB0C7-7F19-11D2-978E-0000F8757E2A}';
    IntfIID:   '{0000000F-0000-0000-C000-000000000046}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TSoapMoniker.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IMoniker;
  end;
end;

procedure TSoapMoniker.ConnectTo(svrIntf: IMoniker);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TSoapMoniker.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TSoapMoniker.GetDefaultInterface: IMoniker;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TSoapMoniker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TSoapMonikerProperties.Create(Self);
{$ENDIF}
end;

destructor TSoapMoniker.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TSoapMoniker.GetServerProperties: TSoapMonikerProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TSoapMoniker.GetClassID(out pClassID: TGUID): HResult;
begin
  Result := DefaultInterface.GetClassID(pClassID);
end;

function TSoapMoniker.IsDirty: HResult;
begin
  Result := DefaultInterface.IsDirty;
end;

function TSoapMoniker.Load(const pstm: IStream): HResult;
begin
  Result := DefaultInterface.Load(pstm);
end;

function TSoapMoniker.Save(const pstm: IStream; fClearDirty: Integer): HResult;
begin
  Result := DefaultInterface.Save(pstm, fClearDirty);
end;

function TSoapMoniker.GetSizeMax(out pcbSize: _ULARGE_INTEGER): HResult;
begin
  Result := DefaultInterface.GetSizeMax(pcbSize);
end;

function TSoapMoniker.RemoteBindToObject(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                                         var riidResult: TGUID; out ppvResult: IUnknown): HResult;
begin
  Result := DefaultInterface.RemoteBindToObject(pbc, pmkToLeft, riidResult, ppvResult);
end;

function TSoapMoniker.RemoteBindToStorage(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                                          var riid: TGUID; out ppvObj: IUnknown): HResult;
begin
  Result := DefaultInterface.RemoteBindToStorage(pbc, pmkToLeft, riid, ppvObj);
end;

function TSoapMoniker.Reduce(const pbc: IBindCtx; dwReduceHowFar: LongWord; 
                             var ppmkToLeft: IMoniker; out ppmkReduced: IMoniker): HResult;
begin
  Result := DefaultInterface.Reduce(pbc, dwReduceHowFar, ppmkToLeft, ppmkReduced);
end;

function TSoapMoniker.ComposeWith(const pmkRight: IMoniker; fOnlyIfNotGeneric: Integer; 
                                  out ppmkComposite: IMoniker): HResult;
begin
  Result := DefaultInterface.ComposeWith(pmkRight, fOnlyIfNotGeneric, ppmkComposite);
end;

function TSoapMoniker.Enum(fForward: Integer; out ppenumMoniker: IEnumMoniker): HResult;
begin
  Result := DefaultInterface.Enum(fForward, ppenumMoniker);
end;

function TSoapMoniker.IsEqual(const pmkOtherMoniker: IMoniker): HResult;
begin
  Result := DefaultInterface.IsEqual(pmkOtherMoniker);
end;

function TSoapMoniker.Hash(out pdwHash: LongWord): HResult;
begin
  Result := DefaultInterface.Hash(pdwHash);
end;

function TSoapMoniker.IsRunning(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                                const pmkNewlyRunning: IMoniker): HResult;
begin
  Result := DefaultInterface.IsRunning(pbc, pmkToLeft, pmkNewlyRunning);
end;

function TSoapMoniker.GetTimeOfLastChange(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                                          out pfiletime: _FILETIME): HResult;
begin
  Result := DefaultInterface.GetTimeOfLastChange(pbc, pmkToLeft, pfiletime);
end;

function TSoapMoniker.Inverse(out ppmk: IMoniker): HResult;
begin
  Result := DefaultInterface.Inverse(ppmk);
end;

function TSoapMoniker.CommonPrefixWith(const pmkOther: IMoniker; out ppmkPrefix: IMoniker): HResult;
begin
  Result := DefaultInterface.CommonPrefixWith(pmkOther, ppmkPrefix);
end;

function TSoapMoniker.RelativePathTo(const pmkOther: IMoniker; out ppmkRelPath: IMoniker): HResult;
begin
  Result := DefaultInterface.RelativePathTo(pmkOther, ppmkRelPath);
end;

function TSoapMoniker.GetDisplayName(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                                     out ppszDisplayName: PWideChar): HResult;
begin
  Result := DefaultInterface.GetDisplayName(pbc, pmkToLeft, ppszDisplayName);
end;

function TSoapMoniker.ParseDisplayName(const pbc: IBindCtx; const pmkToLeft: IMoniker; 
                                       pszDisplayName: PWideChar; out pchEaten: LongWord; 
                                       out ppmkOut: IMoniker): HResult;
begin
  Result := DefaultInterface.ParseDisplayName(pbc, pmkToLeft, pszDisplayName, pchEaten, ppmkOut);
end;

function TSoapMoniker.IsSystemMoniker(out pdwMksys: LongWord): HResult;
begin
  Result := DefaultInterface.IsSystemMoniker(pdwMksys);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TSoapMonikerProperties.Create(AServer: TSoapMoniker);
begin
  inherited Create;
  FServer := AServer;
end;

function TSoapMonikerProperties.GetDefaultInterface: IMoniker;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoAppDomainHelper.Create: IAppDomainHelper;
begin
  Result := CreateComObject(CLASS_AppDomainHelper) as IAppDomainHelper;
end;

class function CoAppDomainHelper.CreateRemote(const MachineName: string): IAppDomainHelper;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_AppDomainHelper) as IAppDomainHelper;
end;

procedure TAppDomainHelper.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{EF24F689-14F8-4D92-B4AF-D7B1F0E70FD4}';
    IntfIID:   '{C7B67079-8255-42C6-9EC0-6994A3548780}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TAppDomainHelper.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IAppDomainHelper;
  end;
end;

procedure TAppDomainHelper.ConnectTo(svrIntf: IAppDomainHelper);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TAppDomainHelper.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TAppDomainHelper.GetDefaultInterface: IAppDomainHelper;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TAppDomainHelper.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TAppDomainHelperProperties.Create(Self);
{$ENDIF}
end;

destructor TAppDomainHelper.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TAppDomainHelper.GetServerProperties: TAppDomainHelperProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TAppDomainHelper.pfnShutdownCB(var pv: Pointer): HResult;
begin
  Result := DefaultInterface.pfnShutdownCB(pv);
end;

function TAppDomainHelper.Initialize(const pUnkAD: IUnknown; 
                                     const __MIDL__IAppDomainHelper0000: IAppDomainHelper; 
                                     var pPool: Pointer): HResult;
begin
  Result := DefaultInterface.Initialize(pUnkAD, __MIDL__IAppDomainHelper0000, pPool);
end;

function TAppDomainHelper.pfnCallbackCB(var pv: Pointer): HResult;
begin
  Result := DefaultInterface.pfnCallbackCB(pv);
end;

function TAppDomainHelper.DoCallback(const pUnkAD: IUnknown; 
                                     const __MIDL__IAppDomainHelper0001: IAppDomainHelper; 
                                     var pPool: Pointer): HResult;
begin
  Result := DefaultInterface.DoCallback(pUnkAD, __MIDL__IAppDomainHelper0001, pPool);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TAppDomainHelperProperties.Create(AServer: TAppDomainHelper);
begin
  inherited Create;
  FServer := AServer;
end;

function TAppDomainHelperProperties.GetDefaultInterface: IAppDomainHelper;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoClrAssemblyLocator.Create: IAssemblyLocator;
begin
  Result := CreateComObject(CLASS_ClrAssemblyLocator) as IAssemblyLocator;
end;

class function CoClrAssemblyLocator.CreateRemote(const MachineName: string): IAssemblyLocator;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ClrAssemblyLocator) as IAssemblyLocator;
end;

procedure TClrAssemblyLocator.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{458AA3B5-265A-4B75-BC05-9BEA4630CF18}';
    IntfIID:   '{391FFBB9-A8EE-432A-ABC8-BAA238DAB90F}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TClrAssemblyLocator.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IAssemblyLocator;
  end;
end;

procedure TClrAssemblyLocator.ConnectTo(svrIntf: IAssemblyLocator);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TClrAssemblyLocator.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TClrAssemblyLocator.GetDefaultInterface: IAssemblyLocator;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TClrAssemblyLocator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TClrAssemblyLocatorProperties.Create(Self);
{$ENDIF}
end;

destructor TClrAssemblyLocator.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TClrAssemblyLocator.GetServerProperties: TClrAssemblyLocatorProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TClrAssemblyLocator.GetModules(const applicationDir: WideString; 
                                        const applicationName: WideString; 
                                        const assemblyName: WideString; out pModules: PSafeArray): HResult;
begin
  Result := DefaultInterface.GetModules(applicationDir, applicationName, assemblyName, pModules);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TClrAssemblyLocatorProperties.Create(AServer: TClrAssemblyLocator);
begin
  inherited Create;
  FServer := AServer;
end;

function TClrAssemblyLocatorProperties.GetDefaultInterface: IAssemblyLocator;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents(dtlServerPage, [TTransactionContext, TTransactionContextEx, TCServiceConfig, TServicePool, 
    TServicePoolConfig, TSharedPropertyGroupManager, TCOMEvents, TCoMTSLocator, TMtsGrp, 
    TCRMClerk, TCRMRecoveryClerk, TMessageMover, TPoolMgr, TPartitionMoniker, 
    TSoapMoniker, TAppDomainHelper, TClrAssemblyLocator]);
end;

end.
