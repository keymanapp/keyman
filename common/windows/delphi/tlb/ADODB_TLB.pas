unit ADODB_TLB;

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

// PASTLWTR : 1.2
// File generated on 25/09/2006 3:57:52 PM from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Program Files\Common Files\System\ado\msado15.dll (1)
// LIBID: {2A75196C-D9EB-4129-B803-931327F72D5C}
// LCID: 0
// Helpfile: C:\WINDOWS\HELP\ado270.chm
// HelpString: Microsoft ActiveX Data Objects 2.8 Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// Parent TypeLibrary:
//   (0) v2.6 JRO, (C:\Program Files\Common Files\System\ado\msjro.dll)
// Errors:
//   Hint: TypeInfo 'Property' changed to 'Property_'
//   Hint: TypeInfo 'Record' changed to 'Record_'
//   Hint: Parameter 'Object' of _DynaCollection.Append changed to 'Object_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Parameter 'Type' of Command15.CreateParameter changed to 'Type_'
//   Hint: Parameter 'Type' of Fields.Append changed to 'Type_'
//   Hint: Parameter 'Type' of Fields20._Append changed to 'Type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
// ************************************************************************ //
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
  ADODBMajorVersion = 2;
  ADODBMinorVersion = 8;

  LIBID_ADODB: TGUID = '{2A75196C-D9EB-4129-B803-931327F72D5C}';

  IID__Collection: TGUID = '{00000512-0000-0010-8000-00AA006D2EA4}';
  IID__DynaCollection: TGUID = '{00000513-0000-0010-8000-00AA006D2EA4}';
  IID__ADO: TGUID = '{00000534-0000-0010-8000-00AA006D2EA4}';
  IID_Properties: TGUID = '{00000504-0000-0010-8000-00AA006D2EA4}';
  IID_Property_: TGUID = '{00000503-0000-0010-8000-00AA006D2EA4}';
  IID_Error: TGUID = '{00000500-0000-0010-8000-00AA006D2EA4}';
  IID_Errors: TGUID = '{00000501-0000-0010-8000-00AA006D2EA4}';
  IID_Command15: TGUID = '{00000508-0000-0010-8000-00AA006D2EA4}';
  IID_Connection15: TGUID = '{00000515-0000-0010-8000-00AA006D2EA4}';
  IID__Connection: TGUID = '{00000550-0000-0010-8000-00AA006D2EA4}';
  IID_Recordset15: TGUID = '{0000050E-0000-0010-8000-00AA006D2EA4}';
  IID_Recordset20: TGUID = '{0000054F-0000-0010-8000-00AA006D2EA4}';
  IID_Recordset21: TGUID = '{00000555-0000-0010-8000-00AA006D2EA4}';
  IID__Recordset: TGUID = '{00000556-0000-0010-8000-00AA006D2EA4}';
  IID_Fields15: TGUID = '{00000506-0000-0010-8000-00AA006D2EA4}';
  IID_Fields20: TGUID = '{0000054D-0000-0010-8000-00AA006D2EA4}';
  IID_Fields: TGUID = '{00000564-0000-0010-8000-00AA006D2EA4}';
  IID_Field20: TGUID = '{0000054C-0000-0010-8000-00AA006D2EA4}';
  IID_Field: TGUID = '{00000569-0000-0010-8000-00AA006D2EA4}';
  IID__Parameter: TGUID = '{0000050C-0000-0010-8000-00AA006D2EA4}';
  IID_Parameters: TGUID = '{0000050D-0000-0010-8000-00AA006D2EA4}';
  IID_Command25: TGUID = '{0000054E-0000-0010-8000-00AA006D2EA4}';
  IID__Command: TGUID = '{B08400BD-F9D1-4D02-B856-71D5DBA123E9}';
  IID_ConnectionEventsVt: TGUID = '{00000402-0000-0010-8000-00AA006D2EA4}';
  IID_RecordsetEventsVt: TGUID = '{00000403-0000-0010-8000-00AA006D2EA4}';
  DIID_ConnectionEvents: TGUID = '{00000400-0000-0010-8000-00AA006D2EA4}';
  DIID_RecordsetEvents: TGUID = '{00000266-0000-0010-8000-00AA006D2EA4}';
  IID_ADOConnectionConstruction15: TGUID = '{00000516-0000-0010-8000-00AA006D2EA4}';
  IID_ADOConnectionConstruction: TGUID = '{00000551-0000-0010-8000-00AA006D2EA4}';
  CLASS_Connection: TGUID = '{00000514-0000-0010-8000-00AA006D2EA4}';
  IID__Record: TGUID = '{00000562-0000-0010-8000-00AA006D2EA4}';
  CLASS_Record_: TGUID = '{00000560-0000-0010-8000-00AA006D2EA4}';
  IID__Stream: TGUID = '{00000565-0000-0010-8000-00AA006D2EA4}';
  CLASS_Stream: TGUID = '{00000566-0000-0010-8000-00AA006D2EA4}';
  IID_ADORecordConstruction: TGUID = '{00000567-0000-0010-8000-00AA006D2EA4}';
  IID_ADOStreamConstruction: TGUID = '{00000568-0000-0010-8000-00AA006D2EA4}';
  IID_ADOCommandConstruction: TGUID = '{00000517-0000-0010-8000-00AA006D2EA4}';
  CLASS_Command: TGUID = '{00000507-0000-0010-8000-00AA006D2EA4}';
  CLASS_Recordset: TGUID = '{00000535-0000-0010-8000-00AA006D2EA4}';
  IID_ADORecordsetConstruction: TGUID = '{00000283-0000-0010-8000-00AA006D2EA4}';
  IID_Field15: TGUID = '{00000505-0000-0010-8000-00AA006D2EA4}';
  CLASS_Parameter: TGUID = '{0000050B-0000-0010-8000-00AA006D2EA4}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum CursorTypeEnum
type
  CursorTypeEnum = TOleEnum;
const
  adOpenUnspecified = $FFFFFFFF;
  adOpenForwardOnly = $00000000;
  adOpenKeyset = $00000001;
  adOpenDynamic = $00000002;
  adOpenStatic = $00000003;

// Constants for enum CursorOptionEnum
type
  CursorOptionEnum = TOleEnum;
const
  adHoldRecords = $00000100;
  adMovePrevious = $00000200;
  adAddNew = $01000400;
  adDelete = $01000800;
  adUpdate = $01008000;
  adBookmark = $00002000;
  adApproxPosition = $00004000;
  adUpdateBatch = $00010000;
  adResync = $00020000;
  adNotify = $00040000;
  adFind = $00080000;
  adSeek = $00400000;
  adIndex = $00800000;

// Constants for enum LockTypeEnum
type
  LockTypeEnum = TOleEnum;
const
  adLockUnspecified = $FFFFFFFF;
  adLockReadOnly = $00000001;
  adLockPessimistic = $00000002;
  adLockOptimistic = $00000003;
  adLockBatchOptimistic = $00000004;

// Constants for enum ExecuteOptionEnum
type
  ExecuteOptionEnum = TOleEnum;
const
  adOptionUnspecified = $FFFFFFFF;
  adAsyncExecute = $00000010;
  adAsyncFetch = $00000020;
  adAsyncFetchNonBlocking = $00000040;
  adExecuteNoRecords = $00000080;
  adExecuteStream = $00000400;
  adExecuteRecord = $00000800;

// Constants for enum ConnectOptionEnum
type
  ConnectOptionEnum = TOleEnum;
const
  adConnectUnspecified = $FFFFFFFF;
  adAsyncConnect = $00000010;

// Constants for enum ObjectStateEnum
type
  ObjectStateEnum = TOleEnum;
const
  adStateClosed = $00000000;
  adStateOpen = $00000001;
  adStateConnecting = $00000002;
  adStateExecuting = $00000004;
  adStateFetching = $00000008;

// Constants for enum CursorLocationEnum
type
  CursorLocationEnum = TOleEnum;
const
  adUseNone = $00000001;
  adUseServer = $00000002;
  adUseClient = $00000003;
  adUseClientBatch = $00000003;

// Constants for enum DataTypeEnum
type
  DataTypeEnum = TOleEnum;
const
  adEmpty = $00000000;
  adTinyInt = $00000010;
  adSmallInt = $00000002;
  adInteger = $00000003;
  adBigInt = $00000014;
  adUnsignedTinyInt = $00000011;
  adUnsignedSmallInt = $00000012;
  adUnsignedInt = $00000013;
  adUnsignedBigInt = $00000015;
  adSingle = $00000004;
  adDouble = $00000005;
  adCurrency = $00000006;
  adDecimal = $0000000E;
  adNumeric = $00000083;
  adBoolean = $0000000B;
  adError = $0000000A;
  adUserDefined = $00000084;
  adVariant = $0000000C;
  adIDispatch = $00000009;
  adIUnknown = $0000000D;
  adGUID = $00000048;
  adDate = $00000007;
  adDBDate = $00000085;
  adDBTime = $00000086;
  adDBTimeStamp = $00000087;
  adBSTR = $00000008;
  adChar = $00000081;
  adVarChar = $000000C8;
  adLongVarChar = $000000C9;
  adWChar = $00000082;
  adVarWChar = $000000CA;
  adLongVarWChar = $000000CB;
  adBinary = $00000080;
  adVarBinary = $000000CC;
  adLongVarBinary = $000000CD;
  adChapter = $00000088;
  adFileTime = $00000040;
  adPropVariant = $0000008A;
  adVarNumeric = $0000008B;
  adArray = $00002000;

// Constants for enum FieldAttributeEnum
type
  FieldAttributeEnum = TOleEnum;
const
  adFldUnspecified = $FFFFFFFF;
  adFldMayDefer = $00000002;
  adFldUpdatable = $00000004;
  adFldUnknownUpdatable = $00000008;
  adFldFixed = $00000010;
  adFldIsNullable = $00000020;
  adFldMayBeNull = $00000040;
  adFldLong = $00000080;
  adFldRowID = $00000100;
  adFldRowVersion = $00000200;
  adFldCacheDeferred = $00001000;
  adFldIsChapter = $00002000;
  adFldNegativeScale = $00004000;
  adFldKeyColumn = $00008000;
  adFldIsRowURL = $00010000;
  adFldIsDefaultStream = $00020000;
  adFldIsCollection = $00040000;

// Constants for enum EditModeEnum
type
  EditModeEnum = TOleEnum;
const
  adEditNone = $00000000;
  adEditInProgress = $00000001;
  adEditAdd = $00000002;
  adEditDelete = $00000004;

// Constants for enum RecordStatusEnum
type
  RecordStatusEnum = TOleEnum;
const
  adRecOK = $00000000;
  adRecNew = $00000001;
  adRecModified = $00000002;
  adRecDeleted = $00000004;
  adRecUnmodified = $00000008;
  adRecInvalid = $00000010;
  adRecMultipleChanges = $00000040;
  adRecPendingChanges = $00000080;
  adRecCanceled = $00000100;
  adRecCantRelease = $00000400;
  adRecConcurrencyViolation = $00000800;
  adRecIntegrityViolation = $00001000;
  adRecMaxChangesExceeded = $00002000;
  adRecObjectOpen = $00004000;
  adRecOutOfMemory = $00008000;
  adRecPermissionDenied = $00010000;
  adRecSchemaViolation = $00020000;
  adRecDBDeleted = $00040000;

// Constants for enum GetRowsOptionEnum
type
  GetRowsOptionEnum = TOleEnum;
const
  adGetRowsRest = $FFFFFFFF;

// Constants for enum PositionEnum
type
  PositionEnum = TOleEnum;
const
  adPosUnknown = $FFFFFFFF;
  adPosBOF = $FFFFFFFE;
  adPosEOF = $FFFFFFFD;

// Constants for enum BookmarkEnum
type
  BookmarkEnum = TOleEnum;
const
  adBookmarkCurrent = $00000000;
  adBookmarkFirst = $00000001;
  adBookmarkLast = $00000002;

// Constants for enum MarshalOptionsEnum
type
  MarshalOptionsEnum = TOleEnum;
const
  adMarshalAll = $00000000;
  adMarshalModifiedOnly = $00000001;

// Constants for enum AffectEnum
type
  AffectEnum = TOleEnum;
const
  adAffectCurrent = $00000001;
  adAffectGroup = $00000002;
  adAffectAll = $00000003;
  adAffectAllChapters = $00000004;

// Constants for enum ResyncEnum
type
  ResyncEnum = TOleEnum;
const
  adResyncUnderlyingValues = $00000001;
  adResyncAllValues = $00000002;

// Constants for enum CompareEnum
type
  CompareEnum = TOleEnum;
const
  adCompareLessThan = $00000000;
  adCompareEqual = $00000001;
  adCompareGreaterThan = $00000002;
  adCompareNotEqual = $00000003;
  adCompareNotComparable = $00000004;

// Constants for enum FilterGroupEnum
type
  FilterGroupEnum = TOleEnum;
const
  adFilterNone = $00000000;
  adFilterPendingRecords = $00000001;
  adFilterAffectedRecords = $00000002;
  adFilterFetchedRecords = $00000003;
  adFilterPredicate = $00000004;
  adFilterConflictingRecords = $00000005;

// Constants for enum SearchDirectionEnum
type
  SearchDirectionEnum = TOleEnum;
const
  adSearchForward = $00000001;
  adSearchBackward = $FFFFFFFF;

// Constants for enum PersistFormatEnum
type
  PersistFormatEnum = TOleEnum;
const
  adPersistADTG = $00000000;
  adPersistXML = $00000001;

// Constants for enum StringFormatEnum
type
  StringFormatEnum = TOleEnum;
const
  adClipString = $00000002;

// Constants for enum ConnectPromptEnum
type
  ConnectPromptEnum = TOleEnum;
const
  adPromptAlways = $00000001;
  adPromptComplete = $00000002;
  adPromptCompleteRequired = $00000003;
  adPromptNever = $00000004;

// Constants for enum ConnectModeEnum
type
  ConnectModeEnum = TOleEnum;
const
  adModeUnknown = $00000000;
  adModeRead = $00000001;
  adModeWrite = $00000002;
  adModeReadWrite = $00000003;
  adModeShareDenyRead = $00000004;
  adModeShareDenyWrite = $00000008;
  adModeShareExclusive = $0000000C;
  adModeShareDenyNone = $00000010;
  adModeRecursive = $00400000;

// Constants for enum RecordCreateOptionsEnum
type
  RecordCreateOptionsEnum = TOleEnum;
const
  adCreateCollection = $00002000;
  adCreateStructDoc = $80000000;
  adCreateNonCollection = $00000000;
  adOpenIfExists = $02000000;
  adCreateOverwrite = $04000000;
  adFailIfNotExists = $FFFFFFFF;

// Constants for enum RecordOpenOptionsEnum
type
  RecordOpenOptionsEnum = TOleEnum;
const
  adOpenRecordUnspecified = $FFFFFFFF;
  adOpenSource = $00800000;
  adOpenOutput = $00800000;
  adOpenAsync = $00001000;
  adDelayFetchStream = $00004000;
  adDelayFetchFields = $00008000;
  adOpenExecuteCommand = $00010000;

// Constants for enum IsolationLevelEnum
type
  IsolationLevelEnum = TOleEnum;
const
  adXactUnspecified = $FFFFFFFF;
  adXactChaos = $00000010;
  adXactReadUncommitted = $00000100;
  adXactBrowse = $00000100;
  adXactCursorStability = $00001000;
  adXactReadCommitted = $00001000;
  adXactRepeatableRead = $00010000;
  adXactSerializable = $00100000;
  adXactIsolated = $00100000;

// Constants for enum XactAttributeEnum
type
  XactAttributeEnum = TOleEnum;
const
  adXactCommitRetaining = $00020000;
  adXactAbortRetaining = $00040000;
  adXactAsyncPhaseOne = $00080000;
  adXactSyncPhaseOne = $00100000;

// Constants for enum PropertyAttributesEnum
type
  PropertyAttributesEnum = TOleEnum;
const
  adPropNotSupported = $00000000;
  adPropRequired = $00000001;
  adPropOptional = $00000002;
  adPropRead = $00000200;
  adPropWrite = $00000400;

// Constants for enum ErrorValueEnum
type
  ErrorValueEnum = TOleEnum;
const
  adErrProviderFailed = $00000BB8;
  adErrInvalidArgument = $00000BB9;
  adErrOpeningFile = $00000BBA;
  adErrReadFile = $00000BBB;
  adErrWriteFile = $00000BBC;
  adErrNoCurrentRecord = $00000BCD;
  adErrIllegalOperation = $00000C93;
  adErrCantChangeProvider = $00000C94;
  adErrInTransaction = $00000CAE;
  adErrFeatureNotAvailable = $00000CB3;
  adErrItemNotFound = $00000CC1;
  adErrObjectInCollection = $00000D27;
  adErrObjectNotSet = $00000D5C;
  adErrDataConversion = $00000D5D;
  adErrObjectClosed = $00000E78;
  adErrObjectOpen = $00000E79;
  adErrProviderNotFound = $00000E7A;
  adErrBoundToCommand = $00000E7B;
  adErrInvalidParamInfo = $00000E7C;
  adErrInvalidConnection = $00000E7D;
  adErrNotReentrant = $00000E7E;
  adErrStillExecuting = $00000E7F;
  adErrOperationCancelled = $00000E80;
  adErrStillConnecting = $00000E81;
  adErrInvalidTransaction = $00000E82;
  adErrNotExecuting = $00000E83;
  adErrUnsafeOperation = $00000E84;
  adwrnSecurityDialog = $00000E85;
  adwrnSecurityDialogHeader = $00000E86;
  adErrIntegrityViolation = $00000E87;
  adErrPermissionDenied = $00000E88;
  adErrDataOverflow = $00000E89;
  adErrSchemaViolation = $00000E8A;
  adErrSignMismatch = $00000E8B;
  adErrCantConvertvalue = $00000E8C;
  adErrCantCreate = $00000E8D;
  adErrColumnNotOnThisRow = $00000E8E;
  adErrURLDoesNotExist = $00000E8F;
  adErrTreePermissionDenied = $00000E90;
  adErrInvalidURL = $00000E91;
  adErrResourceLocked = $00000E92;
  adErrResourceExists = $00000E93;
  adErrCannotComplete = $00000E94;
  adErrVolumeNotFound = $00000E95;
  adErrOutOfSpace = $00000E96;
  adErrResourceOutOfScope = $00000E97;
  adErrUnavailable = $00000E98;
  adErrURLNamedRowDoesNotExist = $00000E99;
  adErrDelResOutOfScope = $00000E9A;
  adErrPropInvalidColumn = $00000E9B;
  adErrPropInvalidOption = $00000E9C;
  adErrPropInvalidValue = $00000E9D;
  adErrPropConflicting = $00000E9E;
  adErrPropNotAllSettable = $00000E9F;
  adErrPropNotSet = $00000EA0;
  adErrPropNotSettable = $00000EA1;
  adErrPropNotSupported = $00000EA2;
  adErrCatalogNotSet = $00000EA3;
  adErrCantChangeConnection = $00000EA4;
  adErrFieldsUpdateFailed = $00000EA5;
  adErrDenyNotSupported = $00000EA6;
  adErrDenyTypeNotSupported = $00000EA7;
  adErrProviderNotSpecified = $00000EA9;
  adErrConnectionStringTooLong = $00000EAA;

// Constants for enum ParameterAttributesEnum
type
  ParameterAttributesEnum = TOleEnum;
const
  adParamSigned = $00000010;
  adParamNullable = $00000040;
  adParamLong = $00000080;

// Constants for enum ParameterDirectionEnum
type
  ParameterDirectionEnum = TOleEnum;
const
  adParamUnknown = $00000000;
  adParamInput = $00000001;
  adParamOutput = $00000002;
  adParamInputOutput = $00000003;
  adParamReturnValue = $00000004;

// Constants for enum CommandTypeEnum
type
  CommandTypeEnum = TOleEnum;
const
  adCmdUnspecified = $FFFFFFFF;
  adCmdUnknown = $00000008;
  adCmdText = $00000001;
  adCmdTable = $00000002;
  adCmdStoredProc = $00000004;
  adCmdFile = $00000100;
  adCmdTableDirect = $00000200;

// Constants for enum EventStatusEnum
type
  EventStatusEnum = TOleEnum;
const
  adStatusOK = $00000001;
  adStatusErrorsOccurred = $00000002;
  adStatusCantDeny = $00000003;
  adStatusCancel = $00000004;
  adStatusUnwantedEvent = $00000005;

// Constants for enum EventReasonEnum
type
  EventReasonEnum = TOleEnum;
const
  adRsnAddNew = $00000001;
  adRsnDelete = $00000002;
  adRsnUpdate = $00000003;
  adRsnUndoUpdate = $00000004;
  adRsnUndoAddNew = $00000005;
  adRsnUndoDelete = $00000006;
  adRsnRequery = $00000007;
  adRsnResynch = $00000008;
  adRsnClose = $00000009;
  adRsnMove = $0000000A;
  adRsnFirstChange = $0000000B;
  adRsnMoveFirst = $0000000C;
  adRsnMoveNext = $0000000D;
  adRsnMovePrevious = $0000000E;
  adRsnMoveLast = $0000000F;

// Constants for enum SchemaEnum
type
  SchemaEnum = TOleEnum;
const
  adSchemaProviderSpecific = $FFFFFFFF;
  adSchemaAsserts = $00000000;
  adSchemaCatalogs = $00000001;
  adSchemaCharacterSets = $00000002;
  adSchemaCollations = $00000003;
  adSchemaColumns = $00000004;
  adSchemaCheckConstraints = $00000005;
  adSchemaConstraintColumnUsage = $00000006;
  adSchemaConstraintTableUsage = $00000007;
  adSchemaKeyColumnUsage = $00000008;
  adSchemaReferentialContraints = $00000009;
  adSchemaReferentialConstraints = $00000009;
  adSchemaTableConstraints = $0000000A;
  adSchemaColumnsDomainUsage = $0000000B;
  adSchemaIndexes = $0000000C;
  adSchemaColumnPrivileges = $0000000D;
  adSchemaTablePrivileges = $0000000E;
  adSchemaUsagePrivileges = $0000000F;
  adSchemaProcedures = $00000010;
  adSchemaSchemata = $00000011;
  adSchemaSQLLanguages = $00000012;
  adSchemaStatistics = $00000013;
  adSchemaTables = $00000014;
  adSchemaTranslations = $00000015;
  adSchemaProviderTypes = $00000016;
  adSchemaViews = $00000017;
  adSchemaViewColumnUsage = $00000018;
  adSchemaViewTableUsage = $00000019;
  adSchemaProcedureParameters = $0000001A;
  adSchemaForeignKeys = $0000001B;
  adSchemaPrimaryKeys = $0000001C;
  adSchemaProcedureColumns = $0000001D;
  adSchemaDBInfoKeywords = $0000001E;
  adSchemaDBInfoLiterals = $0000001F;
  adSchemaCubes = $00000020;
  adSchemaDimensions = $00000021;
  adSchemaHierarchies = $00000022;
  adSchemaLevels = $00000023;
  adSchemaMeasures = $00000024;
  adSchemaProperties = $00000025;
  adSchemaMembers = $00000026;
  adSchemaTrustees = $00000027;
  adSchemaFunctions = $00000028;
  adSchemaActions = $00000029;
  adSchemaCommands = $0000002A;
  adSchemaSets = $0000002B;

// Constants for enum FieldStatusEnum
type
  FieldStatusEnum = TOleEnum;
const
  adFieldOK = $00000000;
  adFieldCantConvertValue = $00000002;
  adFieldIsNull = $00000003;
  adFieldTruncated = $00000004;
  adFieldSignMismatch = $00000005;
  adFieldDataOverflow = $00000006;
  adFieldCantCreate = $00000007;
  adFieldUnavailable = $00000008;
  adFieldPermissionDenied = $00000009;
  adFieldIntegrityViolation = $0000000A;
  adFieldSchemaViolation = $0000000B;
  adFieldBadStatus = $0000000C;
  adFieldDefault = $0000000D;
  adFieldIgnore = $0000000F;
  adFieldDoesNotExist = $00000010;
  adFieldInvalidURL = $00000011;
  adFieldResourceLocked = $00000012;
  adFieldResourceExists = $00000013;
  adFieldCannotComplete = $00000014;
  adFieldVolumeNotFound = $00000015;
  adFieldOutOfSpace = $00000016;
  adFieldCannotDeleteSource = $00000017;
  adFieldReadOnly = $00000018;
  adFieldResourceOutOfScope = $00000019;
  adFieldAlreadyExists = $0000001A;
  adFieldPendingInsert = $00010000;
  adFieldPendingDelete = $00020000;
  adFieldPendingChange = $00040000;
  adFieldPendingUnknown = $00080000;
  adFieldPendingUnknownDelete = $00100000;

// Constants for enum SeekEnum
type
  SeekEnum = TOleEnum;
const
  adSeekFirstEQ = $00000001;
  adSeekLastEQ = $00000002;
  adSeekAfterEQ = $00000004;
  adSeekAfter = $00000008;
  adSeekBeforeEQ = $00000010;
  adSeekBefore = $00000020;

// Constants for enum ADCPROP_UPDATECRITERIA_ENUM
type
  ADCPROP_UPDATECRITERIA_ENUM = TOleEnum;
const
  adCriteriaKey = $00000000;
  adCriteriaAllCols = $00000001;
  adCriteriaUpdCols = $00000002;
  adCriteriaTimeStamp = $00000003;

// Constants for enum ADCPROP_ASYNCTHREADPRIORITY_ENUM
type
  ADCPROP_ASYNCTHREADPRIORITY_ENUM = TOleEnum;
const
  adPriorityLowest = $00000001;
  adPriorityBelowNormal = $00000002;
  adPriorityNormal = $00000003;
  adPriorityAboveNormal = $00000004;
  adPriorityHighest = $00000005;

// Constants for enum ADCPROP_AUTORECALC_ENUM
type
  ADCPROP_AUTORECALC_ENUM = TOleEnum;
const
  adRecalcUpFront = $00000000;
  adRecalcAlways = $00000001;

// Constants for enum ADCPROP_UPDATERESYNC_ENUM
type
  ADCPROP_UPDATERESYNC_ENUM = TOleEnum;
const
  adResyncNone = $00000000;
  adResyncAutoIncrement = $00000001;
  adResyncConflicts = $00000002;
  adResyncUpdates = $00000004;
  adResyncInserts = $00000008;
  adResyncAll = $0000000F;

// Constants for enum MoveRecordOptionsEnum
type
  MoveRecordOptionsEnum = TOleEnum;
const
  adMoveUnspecified = $FFFFFFFF;
  adMoveOverWrite = $00000001;
  adMoveDontUpdateLinks = $00000002;
  adMoveAllowEmulation = $00000004;

// Constants for enum CopyRecordOptionsEnum
type
  CopyRecordOptionsEnum = TOleEnum;
const
  adCopyUnspecified = $FFFFFFFF;
  adCopyOverWrite = $00000001;
  adCopyAllowEmulation = $00000004;
  adCopyNonRecursive = $00000002;

// Constants for enum StreamTypeEnum
type
  StreamTypeEnum = TOleEnum;
const
  adTypeBinary = $00000001;
  adTypeText = $00000002;

// Constants for enum LineSeparatorEnum
type
  LineSeparatorEnum = TOleEnum;
const
  adLF = $0000000A;
  adCR = $0000000D;
  adCRLF = $FFFFFFFF;

// Constants for enum StreamOpenOptionsEnum
type
  StreamOpenOptionsEnum = TOleEnum;
const
  adOpenStreamUnspecified = $FFFFFFFF;
  adOpenStreamAsync = $00000001;
  adOpenStreamFromRecord = $00000004;

// Constants for enum StreamWriteEnum
type
  StreamWriteEnum = TOleEnum;
const
  adWriteChar = $00000000;
  adWriteLine = $00000001;
  stWriteChar = $00000000;
  stWriteLine = $00000001;

// Constants for enum SaveOptionsEnum
type
  SaveOptionsEnum = TOleEnum;
const
  adSaveCreateNotExist = $00000001;
  adSaveCreateOverWrite = $00000002;

// Constants for enum FieldEnum
type
  FieldEnum = TOleEnum;
const
  adDefaultStream = $FFFFFFFF;
  adRecordURL = $FFFFFFFE;

// Constants for enum StreamReadEnum
type
  StreamReadEnum = TOleEnum;
const
  adReadAll = $FFFFFFFF;
  adReadLine = $FFFFFFFE;

// Constants for enum RecordTypeEnum
type
  RecordTypeEnum = TOleEnum;
const
  adSimpleRecord = $00000000;
  adCollectionRecord = $00000001;
  adStructDoc = $00000002;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  _Collection = interface;
  _CollectionDisp = dispinterface;
  _DynaCollection = interface;
  _DynaCollectionDisp = dispinterface;
  _ADO = interface;
  _ADODisp = dispinterface;
  Properties = interface;
  PropertiesDisp = dispinterface;
  Property_ = interface;
  Property_Disp = dispinterface;
  Error = interface;
  ErrorDisp = dispinterface;
  Errors = interface;
  ErrorsDisp = dispinterface;
  Command15 = interface;
  Command15Disp = dispinterface;
  Connection15 = interface;
  Connection15Disp = dispinterface;
  _Connection = interface;
  _ConnectionDisp = dispinterface;
  Recordset15 = interface;
  Recordset15Disp = dispinterface;
  Recordset20 = interface;
  Recordset20Disp = dispinterface;
  Recordset21 = interface;
  Recordset21Disp = dispinterface;
  _Recordset = interface;
  _RecordsetDisp = dispinterface;
  Fields15 = interface;
  Fields15Disp = dispinterface;
  Fields20 = interface;
  Fields20Disp = dispinterface;
  Fields = interface;
  FieldsDisp = dispinterface;
  Field20 = interface;
  Field20Disp = dispinterface;
  Field = interface;
  FieldDisp = dispinterface;
  _Parameter = interface;
  _ParameterDisp = dispinterface;
  Parameters = interface;
  ParametersDisp = dispinterface;
  Command25 = interface;
  Command25Disp = dispinterface;
  _Command = interface;
  _CommandDisp = dispinterface;
  ConnectionEventsVt = interface;
  RecordsetEventsVt = interface;
  ConnectionEvents = dispinterface;
  RecordsetEvents = dispinterface;
  ADOConnectionConstruction15 = interface;
  ADOConnectionConstruction = interface;
  _Record = interface;
  _RecordDisp = dispinterface;
  _Stream = interface;
  _StreamDisp = dispinterface;
  ADORecordConstruction = interface;
  ADOStreamConstruction = interface;
  ADOCommandConstruction = interface;
  ADORecordsetConstruction = interface;
  Field15 = interface;
  Field15Disp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  Connection = _Connection;
  Record_ = _Record;
  Stream = _Stream;
  Command = _Command;
  Recordset = _Recordset;
  Parameter = _Parameter;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  POleVariant1 = ^OleVariant; {*}

  PositionEnum_Param = PositionEnum; 
  SearchDirection = SearchDirectionEnum; 
  ADO_LONGPTR = Integer; 

// *********************************************************************//
// Interface: _Collection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000512-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _Collection = interface(IDispatch)
    ['{00000512-0000-0010-8000-00AA006D2EA4}']
    function Get_Count: Integer; safecall;
    function _NewEnum: IUnknown; safecall;
    procedure Refresh; safecall;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  _CollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000512-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _CollectionDisp = dispinterface
    ['{00000512-0000-0010-8000-00AA006D2EA4}']
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: _DynaCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000513-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _DynaCollection = interface(_Collection)
    ['{00000513-0000-0010-8000-00AA006D2EA4}']
    procedure Append(const Object_: IDispatch); safecall;
    procedure Delete(Index: OleVariant); safecall;
  end;

// *********************************************************************//
// DispIntf:  _DynaCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000513-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _DynaCollectionDisp = dispinterface
    ['{00000513-0000-0010-8000-00AA006D2EA4}']
    procedure Append(const Object_: IDispatch); dispid 1610809344;
    procedure Delete(Index: OleVariant); dispid 1610809345;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: _ADO
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000534-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _ADO = interface(IDispatch)
    ['{00000534-0000-0010-8000-00AA006D2EA4}']
    function Get_Properties: Properties; safecall;
    property Properties: Properties read Get_Properties;
  end;

// *********************************************************************//
// DispIntf:  _ADODisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000534-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _ADODisp = dispinterface
    ['{00000534-0000-0010-8000-00AA006D2EA4}']
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: Properties
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000504-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Properties = interface(_Collection)
    ['{00000504-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): Property_; safecall;
    property Item[Index: OleVariant]: Property_ read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  PropertiesDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000504-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  PropertiesDisp = dispinterface
    ['{00000504-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: Property_ readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: Property_
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000503-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Property_ = interface(IDispatch)
    ['{00000503-0000-0010-8000-00AA006D2EA4}']
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(pval: OleVariant); safecall;
    function Get_Name: WideString; safecall;
    function Get_type_: DataTypeEnum; safecall;
    function Get_Attributes: Integer; safecall;
    procedure Set_Attributes(plAttributes: Integer); safecall;
    property Value: OleVariant read Get_Value write Set_Value;
    property Name: WideString read Get_Name;
    property type_: DataTypeEnum read Get_type_;
    property Attributes: Integer read Get_Attributes write Set_Attributes;
  end;

// *********************************************************************//
// DispIntf:  Property_Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000503-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Property_Disp = dispinterface
    ['{00000503-0000-0010-8000-00AA006D2EA4}']
    property Value: OleVariant dispid 0;
    property Name: WideString readonly dispid 1610743810;
    property type_: DataTypeEnum readonly dispid 1610743811;
    property Attributes: Integer dispid 1610743812;
  end;

// *********************************************************************//
// Interface: Error
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000500-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Error = interface(IDispatch)
    ['{00000500-0000-0010-8000-00AA006D2EA4}']
    function Get_Number: Integer; safecall;
    function Get_Source: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_HelpContext: Integer; safecall;
    function Get_SQLState: WideString; safecall;
    function Get_NativeError: Integer; safecall;
    property Number: Integer read Get_Number;
    property Source: WideString read Get_Source;
    property Description: WideString read Get_Description;
    property HelpFile: WideString read Get_HelpFile;
    property HelpContext: Integer read Get_HelpContext;
    property SQLState: WideString read Get_SQLState;
    property NativeError: Integer read Get_NativeError;
  end;

// *********************************************************************//
// DispIntf:  ErrorDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000500-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ErrorDisp = dispinterface
    ['{00000500-0000-0010-8000-00AA006D2EA4}']
    property Number: Integer readonly dispid 1;
    property Source: WideString readonly dispid 2;
    property Description: WideString readonly dispid 0;
    property HelpFile: WideString readonly dispid 3;
    property HelpContext: Integer readonly dispid 4;
    property SQLState: WideString readonly dispid 5;
    property NativeError: Integer readonly dispid 6;
  end;

// *********************************************************************//
// Interface: Errors
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000501-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Errors = interface(_Collection)
    ['{00000501-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): Error; safecall;
    procedure Clear; safecall;
    property Item[Index: OleVariant]: Error read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  ErrorsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000501-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ErrorsDisp = dispinterface
    ['{00000501-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: Error readonly dispid 0; default;
    procedure Clear; dispid 1610809345;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: Command15
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000508-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Command15 = interface(_ADO)
    ['{00000508-0000-0010-8000-00AA006D2EA4}']
    function Get_ActiveConnection: _Connection; safecall;
    procedure _Set_ActiveConnection(const ppvObject: _Connection); safecall;
    procedure Set_ActiveConnection(ppvObject: OleVariant); safecall;
    function Get_CommandText: WideString; safecall;
    procedure Set_CommandText(const pbstr: WideString); safecall;
    function Get_CommandTimeout: Integer; safecall;
    procedure Set_CommandTimeout(pl: Integer); safecall;
    function Get_Prepared: WordBool; safecall;
    procedure Set_Prepared(pfPrepared: WordBool); safecall;
    function Execute(out RecordsAffected: OleVariant; var Parameters: OleVariant; Options: Integer): _Recordset; safecall;
    function CreateParameter(const Name: WideString; Type_: DataTypeEnum; 
                             Direction: ParameterDirectionEnum; Size: ADO_LONGPTR; Value: OleVariant): _Parameter; safecall;
    function Get_Parameters: Parameters; safecall;
    procedure Set_CommandType(plCmdType: CommandTypeEnum); safecall;
    function Get_CommandType: CommandTypeEnum; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const pbstrName: WideString); safecall;
    property CommandText: WideString read Get_CommandText write Set_CommandText;
    property CommandTimeout: Integer read Get_CommandTimeout write Set_CommandTimeout;
    property Prepared: WordBool read Get_Prepared write Set_Prepared;
    property Parameters: Parameters read Get_Parameters;
    property CommandType: CommandTypeEnum read Get_CommandType write Set_CommandType;
    property Name: WideString read Get_Name write Set_Name;
  end;

// *********************************************************************//
// DispIntf:  Command15Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000508-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Command15Disp = dispinterface
    ['{00000508-0000-0010-8000-00AA006D2EA4}']
    function ActiveConnection: _Connection; dispid 1;
    property CommandText: WideString dispid 2;
    property CommandTimeout: Integer dispid 3;
    property Prepared: WordBool dispid 4;
    function Execute(out RecordsAffected: OleVariant; var Parameters: OleVariant; Options: Integer): _Recordset; dispid 5;
    function CreateParameter(const Name: WideString; Type_: DataTypeEnum; 
                             Direction: ParameterDirectionEnum; Size: ADO_LONGPTR; Value: OleVariant): _Parameter; dispid 6;
    property Parameters: Parameters readonly dispid 0;
    property CommandType: CommandTypeEnum dispid 7;
    property Name: WideString dispid 8;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: Connection15
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {00000515-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Connection15 = interface(_ADO)
    ['{00000515-0000-0010-8000-00AA006D2EA4}']
    function Get_ConnectionString: WideString; safecall;
    procedure Set_ConnectionString(const pbstr: WideString); safecall;
    function Get_CommandTimeout: Integer; safecall;
    procedure Set_CommandTimeout(plTimeout: Integer); safecall;
    function Get_ConnectionTimeout: Integer; safecall;
    procedure Set_ConnectionTimeout(plTimeout: Integer); safecall;
    function Get_Version: WideString; safecall;
    procedure Close; safecall;
    function Execute(const CommandText: WideString; out RecordsAffected: OleVariant; 
                     Options: Integer): _Recordset; safecall;
    function BeginTrans: Integer; safecall;
    procedure CommitTrans; safecall;
    procedure RollbackTrans; safecall;
    procedure Open(const ConnectionString: WideString; const UserID: WideString; 
                   const Password: WideString; Options: Integer); safecall;
    function Get_Errors: Errors; safecall;
    function Get_DefaultDatabase: WideString; safecall;
    procedure Set_DefaultDatabase(const pbstr: WideString); safecall;
    function Get_IsolationLevel: IsolationLevelEnum; safecall;
    procedure Set_IsolationLevel(Level: IsolationLevelEnum); safecall;
    function Get_Attributes: Integer; safecall;
    procedure Set_Attributes(plAttr: Integer); safecall;
    function Get_CursorLocation: CursorLocationEnum; safecall;
    procedure Set_CursorLocation(plCursorLoc: CursorLocationEnum); safecall;
    function Get_Mode: ConnectModeEnum; safecall;
    procedure Set_Mode(plMode: ConnectModeEnum); safecall;
    function Get_Provider: WideString; safecall;
    procedure Set_Provider(const pbstr: WideString); safecall;
    function Get_State: Integer; safecall;
    function OpenSchema(Schema: SchemaEnum; Restrictions: OleVariant; SchemaID: OleVariant): _Recordset; safecall;
    property ConnectionString: WideString read Get_ConnectionString write Set_ConnectionString;
    property CommandTimeout: Integer read Get_CommandTimeout write Set_CommandTimeout;
    property ConnectionTimeout: Integer read Get_ConnectionTimeout write Set_ConnectionTimeout;
    property Version: WideString read Get_Version;
    property Errors: Errors read Get_Errors;
    property DefaultDatabase: WideString read Get_DefaultDatabase write Set_DefaultDatabase;
    property IsolationLevel: IsolationLevelEnum read Get_IsolationLevel write Set_IsolationLevel;
    property Attributes: Integer read Get_Attributes write Set_Attributes;
    property CursorLocation: CursorLocationEnum read Get_CursorLocation write Set_CursorLocation;
    property Mode: ConnectModeEnum read Get_Mode write Set_Mode;
    property Provider: WideString read Get_Provider write Set_Provider;
    property State: Integer read Get_State;
  end;

// *********************************************************************//
// DispIntf:  Connection15Disp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {00000515-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Connection15Disp = dispinterface
    ['{00000515-0000-0010-8000-00AA006D2EA4}']
    property ConnectionString: WideString dispid 0;
    property CommandTimeout: Integer dispid 2;
    property ConnectionTimeout: Integer dispid 3;
    property Version: WideString readonly dispid 4;
    procedure Close; dispid 5;
    function Execute(const CommandText: WideString; out RecordsAffected: OleVariant; 
                     Options: Integer): _Recordset; dispid 6;
    function BeginTrans: Integer; dispid 7;
    procedure CommitTrans; dispid 8;
    procedure RollbackTrans; dispid 9;
    procedure Open(const ConnectionString: WideString; const UserID: WideString; 
                   const Password: WideString; Options: Integer); dispid 10;
    property Errors: Errors readonly dispid 11;
    property DefaultDatabase: WideString dispid 12;
    property IsolationLevel: IsolationLevelEnum dispid 13;
    property Attributes: Integer dispid 14;
    property CursorLocation: CursorLocationEnum dispid 15;
    property Mode: ConnectModeEnum dispid 16;
    property Provider: WideString dispid 17;
    property State: Integer readonly dispid 18;
    function OpenSchema(Schema: SchemaEnum; Restrictions: OleVariant; SchemaID: OleVariant): _Recordset; dispid 19;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: _Connection
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {00000550-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _Connection = interface(Connection15)
    ['{00000550-0000-0010-8000-00AA006D2EA4}']
    procedure Cancel; safecall;
  end;

// *********************************************************************//
// DispIntf:  _ConnectionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {00000550-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _ConnectionDisp = dispinterface
    ['{00000550-0000-0010-8000-00AA006D2EA4}']
    procedure Cancel; dispid 21;
    property ConnectionString: WideString dispid 0;
    property CommandTimeout: Integer dispid 2;
    property ConnectionTimeout: Integer dispid 3;
    property Version: WideString readonly dispid 4;
    procedure Close; dispid 5;
    function Execute(const CommandText: WideString; out RecordsAffected: OleVariant; 
                     Options: Integer): _Recordset; dispid 6;
    function BeginTrans: Integer; dispid 7;
    procedure CommitTrans; dispid 8;
    procedure RollbackTrans; dispid 9;
    procedure Open(const ConnectionString: WideString; const UserID: WideString; 
                   const Password: WideString; Options: Integer); dispid 10;
    property Errors: Errors readonly dispid 11;
    property DefaultDatabase: WideString dispid 12;
    property IsolationLevel: IsolationLevelEnum dispid 13;
    property Attributes: Integer dispid 14;
    property CursorLocation: CursorLocationEnum dispid 15;
    property Mode: ConnectModeEnum dispid 16;
    property Provider: WideString dispid 17;
    property State: Integer readonly dispid 18;
    function OpenSchema(Schema: SchemaEnum; Restrictions: OleVariant; SchemaID: OleVariant): _Recordset; dispid 19;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: Recordset15
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000050E-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Recordset15 = interface(_ADO)
    ['{0000050E-0000-0010-8000-00AA006D2EA4}']
    function Get_AbsolutePosition: PositionEnum_Param; safecall;
    procedure Set_AbsolutePosition(pl: PositionEnum_Param); safecall;
    procedure _Set_ActiveConnection(const pvar: IDispatch); safecall;
    procedure Set_ActiveConnection(pvar: OleVariant); safecall;
    function Get_ActiveConnection: OleVariant; safecall;
    function Get_BOF: WordBool; safecall;
    function Get_Bookmark: OleVariant; safecall;
    procedure Set_Bookmark(pvBookmark: OleVariant); safecall;
    function Get_CacheSize: Integer; safecall;
    procedure Set_CacheSize(pl: Integer); safecall;
    function Get_CursorType: CursorTypeEnum; safecall;
    procedure Set_CursorType(plCursorType: CursorTypeEnum); safecall;
    function Get_EOF: WordBool; safecall;
    function Get_Fields: Fields; safecall;
    function Get_LockType: LockTypeEnum; safecall;
    procedure Set_LockType(plLockType: LockTypeEnum); safecall;
    function Get_MaxRecords: ADO_LONGPTR; safecall;
    procedure Set_MaxRecords(plMaxRecords: ADO_LONGPTR); safecall;
    function Get_RecordCount: ADO_LONGPTR; safecall;
    procedure _Set_Source(const pvSource: IDispatch); safecall;
    procedure Set_Source(const pvSource: WideString); safecall;
    function Get_Source: OleVariant; safecall;
    procedure AddNew(FieldList: OleVariant; Values: OleVariant); safecall;
    procedure CancelUpdate; safecall;
    procedure Close; safecall;
    procedure Delete(AffectRecords: AffectEnum); safecall;
    function GetRows(Rows: Integer; Start: OleVariant; Fields: OleVariant): OleVariant; safecall;
    procedure Move(NumRecords: ADO_LONGPTR; Start: OleVariant); safecall;
    procedure MoveNext; safecall;
    procedure MovePrevious; safecall;
    procedure MoveFirst; safecall;
    procedure MoveLast; safecall;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; CursorType: CursorTypeEnum; 
                   LockType: LockTypeEnum; Options: Integer); safecall;
    procedure Requery(Options: Integer); safecall;
    procedure _xResync(AffectRecords: AffectEnum); safecall;
    procedure Update(Fields: OleVariant; Values: OleVariant); safecall;
    function Get_AbsolutePage: PositionEnum_Param; safecall;
    procedure Set_AbsolutePage(pl: PositionEnum_Param); safecall;
    function Get_EditMode: EditModeEnum; safecall;
    function Get_Filter: OleVariant; safecall;
    procedure Set_Filter(Criteria: OleVariant); safecall;
    function Get_PageCount: ADO_LONGPTR; safecall;
    function Get_PageSize: Integer; safecall;
    procedure Set_PageSize(pl: Integer); safecall;
    function Get_Sort: WideString; safecall;
    procedure Set_Sort(const Criteria: WideString); safecall;
    function Get_Status: Integer; safecall;
    function Get_State: Integer; safecall;
    function _xClone: _Recordset; safecall;
    procedure UpdateBatch(AffectRecords: AffectEnum); safecall;
    procedure CancelBatch(AffectRecords: AffectEnum); safecall;
    function Get_CursorLocation: CursorLocationEnum; safecall;
    procedure Set_CursorLocation(plCursorLoc: CursorLocationEnum); safecall;
    function NextRecordset(out RecordsAffected: OleVariant): _Recordset; safecall;
    function Supports(CursorOptions: CursorOptionEnum): WordBool; safecall;
    function Get_Collect(Index: OleVariant): OleVariant; safecall;
    procedure Set_Collect(Index: OleVariant; pvar: OleVariant); safecall;
    function Get_MarshalOptions: MarshalOptionsEnum; safecall;
    procedure Set_MarshalOptions(peMarshal: MarshalOptionsEnum); safecall;
    procedure Find(const Criteria: WideString; SkipRecords: ADO_LONGPTR; 
                   SearchDirection: SearchDirectionEnum; Start: OleVariant); safecall;
    property AbsolutePosition: PositionEnum_Param read Get_AbsolutePosition write Set_AbsolutePosition;
    property BOF: WordBool read Get_BOF;
    property Bookmark: OleVariant read Get_Bookmark write Set_Bookmark;
    property CacheSize: Integer read Get_CacheSize write Set_CacheSize;
    property CursorType: CursorTypeEnum read Get_CursorType write Set_CursorType;
    property EOF: WordBool read Get_EOF;
    property Fields: Fields read Get_Fields;
    property LockType: LockTypeEnum read Get_LockType write Set_LockType;
    property MaxRecords: ADO_LONGPTR read Get_MaxRecords write Set_MaxRecords;
    property RecordCount: ADO_LONGPTR read Get_RecordCount;
    property AbsolutePage: PositionEnum_Param read Get_AbsolutePage write Set_AbsolutePage;
    property EditMode: EditModeEnum read Get_EditMode;
    property Filter: OleVariant read Get_Filter write Set_Filter;
    property PageCount: ADO_LONGPTR read Get_PageCount;
    property PageSize: Integer read Get_PageSize write Set_PageSize;
    property Sort: WideString read Get_Sort write Set_Sort;
    property Status: Integer read Get_Status;
    property State: Integer read Get_State;
    property CursorLocation: CursorLocationEnum read Get_CursorLocation write Set_CursorLocation;
    property Collect[Index: OleVariant]: OleVariant read Get_Collect write Set_Collect;
    property MarshalOptions: MarshalOptionsEnum read Get_MarshalOptions write Set_MarshalOptions;
  end;

// *********************************************************************//
// DispIntf:  Recordset15Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000050E-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Recordset15Disp = dispinterface
    ['{0000050E-0000-0010-8000-00AA006D2EA4}']
    property AbsolutePosition: PositionEnum_Param dispid 1000;
    function ActiveConnection: IDispatch; dispid 1001;
    property BOF: WordBool readonly dispid 1002;
    property Bookmark: OleVariant dispid 1003;
    property CacheSize: Integer dispid 1004;
    property CursorType: CursorTypeEnum dispid 1005;
    property EOF: WordBool readonly dispid 1006;
    property Fields: Fields readonly dispid 0;
    property LockType: LockTypeEnum dispid 1008;
    property MaxRecords: ADO_LONGPTR dispid 1009;
    property RecordCount: ADO_LONGPTR readonly dispid 1010;
    function Source: IDispatch; dispid 1011;
    procedure AddNew(FieldList: OleVariant; Values: OleVariant); dispid 1012;
    procedure CancelUpdate; dispid 1013;
    procedure Close; dispid 1014;
    procedure Delete(AffectRecords: AffectEnum); dispid 1015;
    function GetRows(Rows: Integer; Start: OleVariant; Fields: OleVariant): OleVariant; dispid 1016;
    procedure Move(NumRecords: ADO_LONGPTR; Start: OleVariant); dispid 1017;
    procedure MoveNext; dispid 1018;
    procedure MovePrevious; dispid 1019;
    procedure MoveFirst; dispid 1020;
    procedure MoveLast; dispid 1021;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; CursorType: CursorTypeEnum; 
                   LockType: LockTypeEnum; Options: Integer); dispid 1022;
    procedure Requery(Options: Integer); dispid 1023;
    procedure _xResync(AffectRecords: AffectEnum); dispid 1610809378;
    procedure Update(Fields: OleVariant; Values: OleVariant); dispid 1025;
    property AbsolutePage: PositionEnum_Param dispid 1047;
    property EditMode: EditModeEnum readonly dispid 1026;
    property Filter: OleVariant dispid 1030;
    property PageCount: ADO_LONGPTR readonly dispid 1050;
    property PageSize: Integer dispid 1048;
    property Sort: WideString dispid 1031;
    property Status: Integer readonly dispid 1029;
    property State: Integer readonly dispid 1054;
    function _xClone: _Recordset; dispid 1610809392;
    procedure UpdateBatch(AffectRecords: AffectEnum); dispid 1035;
    procedure CancelBatch(AffectRecords: AffectEnum); dispid 1049;
    property CursorLocation: CursorLocationEnum dispid 1051;
    function NextRecordset(out RecordsAffected: OleVariant): _Recordset; dispid 1052;
    function Supports(CursorOptions: CursorOptionEnum): WordBool; dispid 1036;
    property Collect[Index: OleVariant]: OleVariant dispid -8;
    property MarshalOptions: MarshalOptionsEnum dispid 1053;
    procedure Find(const Criteria: WideString; SkipRecords: ADO_LONGPTR; 
                   SearchDirection: SearchDirectionEnum; Start: OleVariant); dispid 1058;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: Recordset20
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054F-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Recordset20 = interface(Recordset15)
    ['{0000054F-0000-0010-8000-00AA006D2EA4}']
    procedure Cancel; safecall;
    function Get_DataSource: IUnknown; safecall;
    procedure _Set_DataSource(const ppunkDataSource: IUnknown); safecall;
    procedure _xSave(const FileName: WideString; PersistFormat: PersistFormatEnum); safecall;
    function Get_ActiveCommand: IDispatch; safecall;
    procedure Set_StayInSync(pbStayInSync: WordBool); safecall;
    function Get_StayInSync: WordBool; safecall;
    function GetString(StringFormat: StringFormatEnum; NumRows: Integer; 
                       const ColumnDelimeter: WideString; const RowDelimeter: WideString; 
                       const NullExpr: WideString): WideString; safecall;
    function Get_DataMember: WideString; safecall;
    procedure Set_DataMember(const pbstrDataMember: WideString); safecall;
    function CompareBookmarks(Bookmark1: OleVariant; Bookmark2: OleVariant): CompareEnum; safecall;
    function Clone(LockType: LockTypeEnum): _Recordset; safecall;
    procedure Resync(AffectRecords: AffectEnum; ResyncValues: ResyncEnum); safecall;
    property DataSource: IUnknown read Get_DataSource write _Set_DataSource;
    property ActiveCommand: IDispatch read Get_ActiveCommand;
    property StayInSync: WordBool read Get_StayInSync write Set_StayInSync;
    property DataMember: WideString read Get_DataMember write Set_DataMember;
  end;

// *********************************************************************//
// DispIntf:  Recordset20Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054F-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Recordset20Disp = dispinterface
    ['{0000054F-0000-0010-8000-00AA006D2EA4}']
    procedure Cancel; dispid 1055;
    property DataSource: IUnknown dispid 1056;
    procedure _xSave(const FileName: WideString; PersistFormat: PersistFormatEnum); dispid 1610874883;
    property ActiveCommand: IDispatch readonly dispid 1061;
    property StayInSync: WordBool dispid 1063;
    function GetString(StringFormat: StringFormatEnum; NumRows: Integer; 
                       const ColumnDelimeter: WideString; const RowDelimeter: WideString; 
                       const NullExpr: WideString): WideString; dispid 1062;
    property DataMember: WideString dispid 1064;
    function CompareBookmarks(Bookmark1: OleVariant; Bookmark2: OleVariant): CompareEnum; dispid 1065;
    function Clone(LockType: LockTypeEnum): _Recordset; dispid 1034;
    procedure Resync(AffectRecords: AffectEnum; ResyncValues: ResyncEnum); dispid 1024;
    property AbsolutePosition: PositionEnum_Param dispid 1000;
    function ActiveConnection: IDispatch; dispid 1001;
    property BOF: WordBool readonly dispid 1002;
    property Bookmark: OleVariant dispid 1003;
    property CacheSize: Integer dispid 1004;
    property CursorType: CursorTypeEnum dispid 1005;
    property EOF: WordBool readonly dispid 1006;
    property Fields: Fields readonly dispid 0;
    property LockType: LockTypeEnum dispid 1008;
    property MaxRecords: ADO_LONGPTR dispid 1009;
    property RecordCount: ADO_LONGPTR readonly dispid 1010;
    function Source: IDispatch; dispid 1011;
    procedure AddNew(FieldList: OleVariant; Values: OleVariant); dispid 1012;
    procedure CancelUpdate; dispid 1013;
    procedure Close; dispid 1014;
    procedure Delete(AffectRecords: AffectEnum); dispid 1015;
    function GetRows(Rows: Integer; Start: OleVariant; Fields: OleVariant): OleVariant; dispid 1016;
    procedure Move(NumRecords: ADO_LONGPTR; Start: OleVariant); dispid 1017;
    procedure MoveNext; dispid 1018;
    procedure MovePrevious; dispid 1019;
    procedure MoveFirst; dispid 1020;
    procedure MoveLast; dispid 1021;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; CursorType: CursorTypeEnum; 
                   LockType: LockTypeEnum; Options: Integer); dispid 1022;
    procedure Requery(Options: Integer); dispid 1023;
    procedure _xResync(AffectRecords: AffectEnum); dispid 1610809378;
    procedure Update(Fields: OleVariant; Values: OleVariant); dispid 1025;
    property AbsolutePage: PositionEnum_Param dispid 1047;
    property EditMode: EditModeEnum readonly dispid 1026;
    property Filter: OleVariant dispid 1030;
    property PageCount: ADO_LONGPTR readonly dispid 1050;
    property PageSize: Integer dispid 1048;
    property Sort: WideString dispid 1031;
    property Status: Integer readonly dispid 1029;
    property State: Integer readonly dispid 1054;
    function _xClone: _Recordset; dispid 1610809392;
    procedure UpdateBatch(AffectRecords: AffectEnum); dispid 1035;
    procedure CancelBatch(AffectRecords: AffectEnum); dispid 1049;
    property CursorLocation: CursorLocationEnum dispid 1051;
    function NextRecordset(out RecordsAffected: OleVariant): _Recordset; dispid 1052;
    function Supports(CursorOptions: CursorOptionEnum): WordBool; dispid 1036;
    property Collect[Index: OleVariant]: OleVariant dispid -8;
    property MarshalOptions: MarshalOptionsEnum dispid 1053;
    procedure Find(const Criteria: WideString; SkipRecords: ADO_LONGPTR; 
                   SearchDirection: SearchDirectionEnum; Start: OleVariant); dispid 1058;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: Recordset21
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000555-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Recordset21 = interface(Recordset20)
    ['{00000555-0000-0010-8000-00AA006D2EA4}']
    procedure Seek(KeyValues: OleVariant; SeekOption: SeekEnum); safecall;
    procedure Set_Index(const pbstrIndex: WideString); safecall;
    function Get_Index: WideString; safecall;
    property Index: WideString read Get_Index write Set_Index;
  end;

// *********************************************************************//
// DispIntf:  Recordset21Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000555-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Recordset21Disp = dispinterface
    ['{00000555-0000-0010-8000-00AA006D2EA4}']
    procedure Seek(KeyValues: OleVariant; SeekOption: SeekEnum); dispid 1066;
    property Index: WideString dispid 1067;
    procedure Cancel; dispid 1055;
    property DataSource: IUnknown dispid 1056;
    procedure _xSave(const FileName: WideString; PersistFormat: PersistFormatEnum); dispid 1610874883;
    property ActiveCommand: IDispatch readonly dispid 1061;
    property StayInSync: WordBool dispid 1063;
    function GetString(StringFormat: StringFormatEnum; NumRows: Integer; 
                       const ColumnDelimeter: WideString; const RowDelimeter: WideString; 
                       const NullExpr: WideString): WideString; dispid 1062;
    property DataMember: WideString dispid 1064;
    function CompareBookmarks(Bookmark1: OleVariant; Bookmark2: OleVariant): CompareEnum; dispid 1065;
    function Clone(LockType: LockTypeEnum): _Recordset; dispid 1034;
    procedure Resync(AffectRecords: AffectEnum; ResyncValues: ResyncEnum); dispid 1024;
    property AbsolutePosition: PositionEnum_Param dispid 1000;
    function ActiveConnection: IDispatch; dispid 1001;
    property BOF: WordBool readonly dispid 1002;
    property Bookmark: OleVariant dispid 1003;
    property CacheSize: Integer dispid 1004;
    property CursorType: CursorTypeEnum dispid 1005;
    property EOF: WordBool readonly dispid 1006;
    property Fields: Fields readonly dispid 0;
    property LockType: LockTypeEnum dispid 1008;
    property MaxRecords: ADO_LONGPTR dispid 1009;
    property RecordCount: ADO_LONGPTR readonly dispid 1010;
    function Source: IDispatch; dispid 1011;
    procedure AddNew(FieldList: OleVariant; Values: OleVariant); dispid 1012;
    procedure CancelUpdate; dispid 1013;
    procedure Close; dispid 1014;
    procedure Delete(AffectRecords: AffectEnum); dispid 1015;
    function GetRows(Rows: Integer; Start: OleVariant; Fields: OleVariant): OleVariant; dispid 1016;
    procedure Move(NumRecords: ADO_LONGPTR; Start: OleVariant); dispid 1017;
    procedure MoveNext; dispid 1018;
    procedure MovePrevious; dispid 1019;
    procedure MoveFirst; dispid 1020;
    procedure MoveLast; dispid 1021;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; CursorType: CursorTypeEnum; 
                   LockType: LockTypeEnum; Options: Integer); dispid 1022;
    procedure Requery(Options: Integer); dispid 1023;
    procedure _xResync(AffectRecords: AffectEnum); dispid 1610809378;
    procedure Update(Fields: OleVariant; Values: OleVariant); dispid 1025;
    property AbsolutePage: PositionEnum_Param dispid 1047;
    property EditMode: EditModeEnum readonly dispid 1026;
    property Filter: OleVariant dispid 1030;
    property PageCount: ADO_LONGPTR readonly dispid 1050;
    property PageSize: Integer dispid 1048;
    property Sort: WideString dispid 1031;
    property Status: Integer readonly dispid 1029;
    property State: Integer readonly dispid 1054;
    function _xClone: _Recordset; dispid 1610809392;
    procedure UpdateBatch(AffectRecords: AffectEnum); dispid 1035;
    procedure CancelBatch(AffectRecords: AffectEnum); dispid 1049;
    property CursorLocation: CursorLocationEnum dispid 1051;
    function NextRecordset(out RecordsAffected: OleVariant): _Recordset; dispid 1052;
    function Supports(CursorOptions: CursorOptionEnum): WordBool; dispid 1036;
    property Collect[Index: OleVariant]: OleVariant dispid -8;
    property MarshalOptions: MarshalOptionsEnum dispid 1053;
    procedure Find(const Criteria: WideString; SkipRecords: ADO_LONGPTR; 
                   SearchDirection: SearchDirectionEnum; Start: OleVariant); dispid 1058;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: _Recordset
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000556-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _Recordset = interface(Recordset21)
    ['{00000556-0000-0010-8000-00AA006D2EA4}']
    procedure Save(Destination: OleVariant; PersistFormat: PersistFormatEnum); safecall;
  end;

// *********************************************************************//
// DispIntf:  _RecordsetDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000556-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _RecordsetDisp = dispinterface
    ['{00000556-0000-0010-8000-00AA006D2EA4}']
    procedure Save(Destination: OleVariant; PersistFormat: PersistFormatEnum); dispid 1057;
    procedure Seek(KeyValues: OleVariant; SeekOption: SeekEnum); dispid 1066;
    property Index: WideString dispid 1067;
    procedure Cancel; dispid 1055;
    property DataSource: IUnknown dispid 1056;
    procedure _xSave(const FileName: WideString; PersistFormat: PersistFormatEnum); dispid 1610874883;
    property ActiveCommand: IDispatch readonly dispid 1061;
    property StayInSync: WordBool dispid 1063;
    function GetString(StringFormat: StringFormatEnum; NumRows: Integer; 
                       const ColumnDelimeter: WideString; const RowDelimeter: WideString; 
                       const NullExpr: WideString): WideString; dispid 1062;
    property DataMember: WideString dispid 1064;
    function CompareBookmarks(Bookmark1: OleVariant; Bookmark2: OleVariant): CompareEnum; dispid 1065;
    function Clone(LockType: LockTypeEnum): _Recordset; dispid 1034;
    procedure Resync(AffectRecords: AffectEnum; ResyncValues: ResyncEnum); dispid 1024;
    property AbsolutePosition: PositionEnum_Param dispid 1000;
    function ActiveConnection: IDispatch; dispid 1001;
    property BOF: WordBool readonly dispid 1002;
    property Bookmark: OleVariant dispid 1003;
    property CacheSize: Integer dispid 1004;
    property CursorType: CursorTypeEnum dispid 1005;
    property EOF: WordBool readonly dispid 1006;
    property Fields: Fields readonly dispid 0;
    property LockType: LockTypeEnum dispid 1008;
    property MaxRecords: ADO_LONGPTR dispid 1009;
    property RecordCount: ADO_LONGPTR readonly dispid 1010;
    function Source: IDispatch; dispid 1011;
    procedure AddNew(FieldList: OleVariant; Values: OleVariant); dispid 1012;
    procedure CancelUpdate; dispid 1013;
    procedure Close; dispid 1014;
    procedure Delete(AffectRecords: AffectEnum); dispid 1015;
    function GetRows(Rows: Integer; Start: OleVariant; Fields: OleVariant): OleVariant; dispid 1016;
    procedure Move(NumRecords: ADO_LONGPTR; Start: OleVariant); dispid 1017;
    procedure MoveNext; dispid 1018;
    procedure MovePrevious; dispid 1019;
    procedure MoveFirst; dispid 1020;
    procedure MoveLast; dispid 1021;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; CursorType: CursorTypeEnum; 
                   LockType: LockTypeEnum; Options: Integer); dispid 1022;
    procedure Requery(Options: Integer); dispid 1023;
    procedure _xResync(AffectRecords: AffectEnum); dispid 1610809378;
    procedure Update(Fields: OleVariant; Values: OleVariant); dispid 1025;
    property AbsolutePage: PositionEnum_Param dispid 1047;
    property EditMode: EditModeEnum readonly dispid 1026;
    property Filter: OleVariant dispid 1030;
    property PageCount: ADO_LONGPTR readonly dispid 1050;
    property PageSize: Integer dispid 1048;
    property Sort: WideString dispid 1031;
    property Status: Integer readonly dispid 1029;
    property State: Integer readonly dispid 1054;
    function _xClone: _Recordset; dispid 1610809392;
    procedure UpdateBatch(AffectRecords: AffectEnum); dispid 1035;
    procedure CancelBatch(AffectRecords: AffectEnum); dispid 1049;
    property CursorLocation: CursorLocationEnum dispid 1051;
    function NextRecordset(out RecordsAffected: OleVariant): _Recordset; dispid 1052;
    function Supports(CursorOptions: CursorOptionEnum): WordBool; dispid 1036;
    property Collect[Index: OleVariant]: OleVariant dispid -8;
    property MarshalOptions: MarshalOptionsEnum dispid 1053;
    procedure Find(const Criteria: WideString; SkipRecords: ADO_LONGPTR; 
                   SearchDirection: SearchDirectionEnum; Start: OleVariant); dispid 1058;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: Fields15
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000506-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Fields15 = interface(_Collection)
    ['{00000506-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): Field; safecall;
    property Item[Index: OleVariant]: Field read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  Fields15Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000506-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Fields15Disp = dispinterface
    ['{00000506-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: Field readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: Fields20
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054D-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Fields20 = interface(Fields15)
    ['{0000054D-0000-0010-8000-00AA006D2EA4}']
    procedure _Append(const Name: WideString; Type_: DataTypeEnum; DefinedSize: ADO_LONGPTR; 
                      Attrib: FieldAttributeEnum); safecall;
    procedure Delete(Index: OleVariant); safecall;
  end;

// *********************************************************************//
// DispIntf:  Fields20Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054D-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Fields20Disp = dispinterface
    ['{0000054D-0000-0010-8000-00AA006D2EA4}']
    procedure _Append(const Name: WideString; Type_: DataTypeEnum; DefinedSize: ADO_LONGPTR; 
                      Attrib: FieldAttributeEnum); dispid 1610874880;
    procedure Delete(Index: OleVariant); dispid 4;
    property Item[Index: OleVariant]: Field readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: Fields
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000564-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Fields = interface(Fields20)
    ['{00000564-0000-0010-8000-00AA006D2EA4}']
    procedure Append(const Name: WideString; Type_: DataTypeEnum; DefinedSize: ADO_LONGPTR; 
                     Attrib: FieldAttributeEnum; FieldValue: OleVariant); safecall;
    procedure Update; safecall;
    procedure Resync(ResyncValues: ResyncEnum); safecall;
    procedure CancelUpdate; safecall;
  end;

// *********************************************************************//
// DispIntf:  FieldsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000564-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  FieldsDisp = dispinterface
    ['{00000564-0000-0010-8000-00AA006D2EA4}']
    procedure Append(const Name: WideString; Type_: DataTypeEnum; DefinedSize: ADO_LONGPTR; 
                     Attrib: FieldAttributeEnum; FieldValue: OleVariant); dispid 3;
    procedure Update; dispid 5;
    procedure Resync(ResyncValues: ResyncEnum); dispid 6;
    procedure CancelUpdate; dispid 7;
    procedure _Append(const Name: WideString; Type_: DataTypeEnum; DefinedSize: ADO_LONGPTR; 
                      Attrib: FieldAttributeEnum); dispid 1610874880;
    procedure Delete(Index: OleVariant); dispid 4;
    property Item[Index: OleVariant]: Field readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: Field20
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054C-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Field20 = interface(_ADO)
    ['{0000054C-0000-0010-8000-00AA006D2EA4}']
    function Get_ActualSize: ADO_LONGPTR; safecall;
    function Get_Attributes: Integer; safecall;
    function Get_DefinedSize: ADO_LONGPTR; safecall;
    function Get_Name: WideString; safecall;
    function Get_type_: DataTypeEnum; safecall;
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(pvar: OleVariant); safecall;
    function Get_Precision: Byte; safecall;
    function Get_NumericScale: Byte; safecall;
    procedure AppendChunk(Data: OleVariant); safecall;
    function GetChunk(Length: Integer): OleVariant; safecall;
    function Get_OriginalValue: OleVariant; safecall;
    function Get_UnderlyingValue: OleVariant; safecall;
    function Get_DataFormat: IUnknown; safecall;
    procedure _Set_DataFormat(const ppiDF: IUnknown); safecall;
    procedure Set_Precision(pbPrecision: Byte); safecall;
    procedure Set_NumericScale(pbNumericScale: Byte); safecall;
    procedure Set_type_(pDataType: DataTypeEnum); safecall;
    procedure Set_DefinedSize(pl: ADO_LONGPTR); safecall;
    procedure Set_Attributes(pl: Integer); safecall;
    property ActualSize: ADO_LONGPTR read Get_ActualSize;
    property Attributes: Integer read Get_Attributes write Set_Attributes;
    property DefinedSize: ADO_LONGPTR read Get_DefinedSize write Set_DefinedSize;
    property Name: WideString read Get_Name;
    property type_: DataTypeEnum read Get_type_ write Set_type_;
    property Value: OleVariant read Get_Value write Set_Value;
    property Precision: Byte read Get_Precision write Set_Precision;
    property NumericScale: Byte read Get_NumericScale write Set_NumericScale;
    property OriginalValue: OleVariant read Get_OriginalValue;
    property UnderlyingValue: OleVariant read Get_UnderlyingValue;
    property DataFormat: IUnknown read Get_DataFormat write _Set_DataFormat;
  end;

// *********************************************************************//
// DispIntf:  Field20Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054C-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Field20Disp = dispinterface
    ['{0000054C-0000-0010-8000-00AA006D2EA4}']
    property ActualSize: ADO_LONGPTR readonly dispid 1109;
    property Attributes: Integer dispid 1114;
    property DefinedSize: ADO_LONGPTR dispid 1103;
    property Name: WideString readonly dispid 1100;
    property type_: DataTypeEnum dispid 1102;
    property Value: OleVariant dispid 0;
    property Precision: Byte dispid 1112;
    property NumericScale: Byte dispid 1113;
    procedure AppendChunk(Data: OleVariant); dispid 1107;
    function GetChunk(Length: Integer): OleVariant; dispid 1108;
    property OriginalValue: OleVariant readonly dispid 1104;
    property UnderlyingValue: OleVariant readonly dispid 1105;
    property DataFormat: IUnknown dispid 1115;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: Field
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000569-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Field = interface(Field20)
    ['{00000569-0000-0010-8000-00AA006D2EA4}']
    function Get_Status: Integer; safecall;
    property Status: Integer read Get_Status;
  end;

// *********************************************************************//
// DispIntf:  FieldDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000569-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  FieldDisp = dispinterface
    ['{00000569-0000-0010-8000-00AA006D2EA4}']
    property Status: Integer readonly dispid 1116;
    property ActualSize: ADO_LONGPTR readonly dispid 1109;
    property Attributes: Integer dispid 1114;
    property DefinedSize: ADO_LONGPTR dispid 1103;
    property Name: WideString readonly dispid 1100;
    property type_: DataTypeEnum dispid 1102;
    property Value: OleVariant dispid 0;
    property Precision: Byte dispid 1112;
    property NumericScale: Byte dispid 1113;
    procedure AppendChunk(Data: OleVariant); dispid 1107;
    function GetChunk(Length: Integer): OleVariant; dispid 1108;
    property OriginalValue: OleVariant readonly dispid 1104;
    property UnderlyingValue: OleVariant readonly dispid 1105;
    property DataFormat: IUnknown dispid 1115;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: _Parameter
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000050C-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _Parameter = interface(_ADO)
    ['{0000050C-0000-0010-8000-00AA006D2EA4}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const pbstr: WideString); safecall;
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(pvar: OleVariant); safecall;
    function Get_type_: DataTypeEnum; safecall;
    procedure Set_type_(psDataType: DataTypeEnum); safecall;
    procedure Set_Direction(plParmDirection: ParameterDirectionEnum); safecall;
    function Get_Direction: ParameterDirectionEnum; safecall;
    procedure Set_Precision(pbPrecision: Byte); safecall;
    function Get_Precision: Byte; safecall;
    procedure Set_NumericScale(pbScale: Byte); safecall;
    function Get_NumericScale: Byte; safecall;
    procedure Set_Size(pl: ADO_LONGPTR); safecall;
    function Get_Size: ADO_LONGPTR; safecall;
    procedure AppendChunk(Val: OleVariant); safecall;
    function Get_Attributes: Integer; safecall;
    procedure Set_Attributes(plParmAttribs: Integer); safecall;
    property Name: WideString read Get_Name write Set_Name;
    property Value: OleVariant read Get_Value write Set_Value;
    property type_: DataTypeEnum read Get_type_ write Set_type_;
    property Direction: ParameterDirectionEnum read Get_Direction write Set_Direction;
    property Precision: Byte read Get_Precision write Set_Precision;
    property NumericScale: Byte read Get_NumericScale write Set_NumericScale;
    property Size: ADO_LONGPTR read Get_Size write Set_Size;
    property Attributes: Integer read Get_Attributes write Set_Attributes;
  end;

// *********************************************************************//
// DispIntf:  _ParameterDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000050C-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _ParameterDisp = dispinterface
    ['{0000050C-0000-0010-8000-00AA006D2EA4}']
    property Name: WideString dispid 1;
    property Value: OleVariant dispid 0;
    property type_: DataTypeEnum dispid 2;
    property Direction: ParameterDirectionEnum dispid 3;
    property Precision: Byte dispid 4;
    property NumericScale: Byte dispid 5;
    property Size: ADO_LONGPTR dispid 6;
    procedure AppendChunk(Val: OleVariant); dispid 7;
    property Attributes: Integer dispid 8;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: Parameters
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000050D-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Parameters = interface(_DynaCollection)
    ['{0000050D-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): _Parameter; safecall;
    property Item[Index: OleVariant]: _Parameter read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  ParametersDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000050D-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ParametersDisp = dispinterface
    ['{0000050D-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: _Parameter readonly dispid 0; default;
    procedure Append(const Object_: IDispatch); dispid 1610809344;
    procedure Delete(Index: OleVariant); dispid 1610809345;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: Command25
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054E-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Command25 = interface(Command15)
    ['{0000054E-0000-0010-8000-00AA006D2EA4}']
    function Get_State: Integer; safecall;
    procedure Cancel; safecall;
    property State: Integer read Get_State;
  end;

// *********************************************************************//
// DispIntf:  Command25Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054E-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Command25Disp = dispinterface
    ['{0000054E-0000-0010-8000-00AA006D2EA4}']
    property State: Integer readonly dispid 9;
    procedure Cancel; dispid 10;
    function ActiveConnection: _Connection; dispid 1;
    property CommandText: WideString dispid 2;
    property CommandTimeout: Integer dispid 3;
    property Prepared: WordBool dispid 4;
    function Execute(out RecordsAffected: OleVariant; var Parameters: OleVariant; Options: Integer): _Recordset; dispid 5;
    function CreateParameter(const Name: WideString; Type_: DataTypeEnum; 
                             Direction: ParameterDirectionEnum; Size: ADO_LONGPTR; Value: OleVariant): _Parameter; dispid 6;
    property Parameters: Parameters readonly dispid 0;
    property CommandType: CommandTypeEnum dispid 7;
    property Name: WideString dispid 8;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: _Command
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B08400BD-F9D1-4D02-B856-71D5DBA123E9}
// *********************************************************************//
  _Command = interface(Command25)
    ['{B08400BD-F9D1-4D02-B856-71D5DBA123E9}']
    procedure _Set_CommandStream(const pvStream: IUnknown); safecall;
    function Get_CommandStream: OleVariant; safecall;
    procedure Set_Dialect(const pbstrDialect: WideString); safecall;
    function Get_Dialect: WideString; safecall;
    procedure Set_NamedParameters(pfNamedParameters: WordBool); safecall;
    function Get_NamedParameters: WordBool; safecall;
    property Dialect: WideString read Get_Dialect write Set_Dialect;
    property NamedParameters: WordBool read Get_NamedParameters write Set_NamedParameters;
  end;

// *********************************************************************//
// DispIntf:  _CommandDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B08400BD-F9D1-4D02-B856-71D5DBA123E9}
// *********************************************************************//
  _CommandDisp = dispinterface
    ['{B08400BD-F9D1-4D02-B856-71D5DBA123E9}']
    function CommandStream: IUnknown; dispid 11;
    property Dialect: WideString dispid 12;
    property NamedParameters: WordBool dispid 13;
    property State: Integer readonly dispid 9;
    procedure Cancel; dispid 10;
    function ActiveConnection: _Connection; dispid 1;
    property CommandText: WideString dispid 2;
    property CommandTimeout: Integer dispid 3;
    property Prepared: WordBool dispid 4;
    function Execute(out RecordsAffected: OleVariant; var Parameters: OleVariant; Options: Integer): _Recordset; dispid 5;
    function CreateParameter(const Name: WideString; Type_: DataTypeEnum; 
                             Direction: ParameterDirectionEnum; Size: ADO_LONGPTR; Value: OleVariant): _Parameter; dispid 6;
    property Parameters: Parameters readonly dispid 0;
    property CommandType: CommandTypeEnum dispid 7;
    property Name: WideString dispid 8;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: ConnectionEventsVt
// Flags:     (16) Hidden
// GUID:      {00000402-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ConnectionEventsVt = interface(IUnknown)
    ['{00000402-0000-0010-8000-00AA006D2EA4}']
    function InfoMessage(const pError: Error; var adStatus: EventStatusEnum; 
                         const pConnection: _Connection): HResult; stdcall;
    function BeginTransComplete(TransactionLevel: Integer; const pError: Error; 
                                var adStatus: EventStatusEnum; const pConnection: _Connection): HResult; stdcall;
    function CommitTransComplete(const pError: Error; var adStatus: EventStatusEnum; 
                                 const pConnection: _Connection): HResult; stdcall;
    function RollbackTransComplete(const pError: Error; var adStatus: EventStatusEnum; 
                                   const pConnection: _Connection): HResult; stdcall;
    function WillExecute(var Source: WideString; var CursorType: CursorTypeEnum; 
                         var LockType: LockTypeEnum; var Options: Integer; 
                         var adStatus: EventStatusEnum; const pCommand: _Command; 
                         const pRecordset: _Recordset; const pConnection: _Connection): HResult; stdcall;
    function ExecuteComplete(RecordsAffected: Integer; const pError: Error; 
                             var adStatus: EventStatusEnum; const pCommand: _Command; 
                             const pRecordset: _Recordset; const pConnection: _Connection): HResult; stdcall;
    function WillConnect(var ConnectionString: WideString; var UserID: WideString; 
                         var Password: WideString; var Options: Integer; 
                         var adStatus: EventStatusEnum; const pConnection: _Connection): HResult; stdcall;
    function ConnectComplete(const pError: Error; var adStatus: EventStatusEnum; 
                             const pConnection: _Connection): HResult; stdcall;
    function Disconnect(var adStatus: EventStatusEnum; const pConnection: _Connection): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: RecordsetEventsVt
// Flags:     (16) Hidden
// GUID:      {00000403-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  RecordsetEventsVt = interface(IUnknown)
    ['{00000403-0000-0010-8000-00AA006D2EA4}']
    function WillChangeField(cFields: Integer; Fields: OleVariant; var adStatus: EventStatusEnum; 
                             const pRecordset: _Recordset): HResult; stdcall;
    function FieldChangeComplete(cFields: Integer; Fields: OleVariant; const pError: Error; 
                                 var adStatus: EventStatusEnum; const pRecordset: _Recordset): HResult; stdcall;
    function WillChangeRecord(adReason: EventReasonEnum; cRecords: Integer; 
                              var adStatus: EventStatusEnum; const pRecordset: _Recordset): HResult; stdcall;
    function RecordChangeComplete(adReason: EventReasonEnum; cRecords: Integer; 
                                  const pError: Error; var adStatus: EventStatusEnum; 
                                  const pRecordset: _Recordset): HResult; stdcall;
    function WillChangeRecordset(adReason: EventReasonEnum; var adStatus: EventStatusEnum; 
                                 const pRecordset: _Recordset): HResult; stdcall;
    function RecordsetChangeComplete(adReason: EventReasonEnum; const pError: Error; 
                                     var adStatus: EventStatusEnum; const pRecordset: _Recordset): HResult; stdcall;
    function WillMove(adReason: EventReasonEnum; var adStatus: EventStatusEnum; 
                      const pRecordset: _Recordset): HResult; stdcall;
    function MoveComplete(adReason: EventReasonEnum; const pError: Error; 
                          var adStatus: EventStatusEnum; const pRecordset: _Recordset): HResult; stdcall;
    function EndOfRecordset(var fMoreData: WordBool; var adStatus: EventStatusEnum; 
                            const pRecordset: _Recordset): HResult; stdcall;
    function FetchProgress(Progress: Integer; MaxProgress: Integer; var adStatus: EventStatusEnum; 
                           const pRecordset: _Recordset): HResult; stdcall;
    function FetchComplete(const pError: Error; var adStatus: EventStatusEnum; 
                           const pRecordset: _Recordset): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  ConnectionEvents
// Flags:     (4096) Dispatchable
// GUID:      {00000400-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ConnectionEvents = dispinterface
    ['{00000400-0000-0010-8000-00AA006D2EA4}']
    procedure InfoMessage(const pError: Error; var adStatus: EventStatusEnum; 
                          const pConnection: _Connection); dispid 0;
    procedure BeginTransComplete(TransactionLevel: Integer; const pError: Error; 
                                 var adStatus: EventStatusEnum; const pConnection: _Connection); dispid 1;
    procedure CommitTransComplete(const pError: Error; var adStatus: EventStatusEnum; 
                                  const pConnection: _Connection); dispid 3;
    procedure RollbackTransComplete(const pError: Error; var adStatus: EventStatusEnum; 
                                    const pConnection: _Connection); dispid 2;
    procedure WillExecute(var Source: WideString; var CursorType: CursorTypeEnum; 
                          var LockType: LockTypeEnum; var Options: Integer; 
                          var adStatus: EventStatusEnum; const pCommand: _Command; 
                          const pRecordset: _Recordset; const pConnection: _Connection); dispid 4;
    procedure ExecuteComplete(RecordsAffected: Integer; const pError: Error; 
                              var adStatus: EventStatusEnum; const pCommand: _Command; 
                              const pRecordset: _Recordset; const pConnection: _Connection); dispid 5;
    procedure WillConnect(var ConnectionString: WideString; var UserID: WideString; 
                          var Password: WideString; var Options: Integer; 
                          var adStatus: EventStatusEnum; const pConnection: _Connection); dispid 6;
    procedure ConnectComplete(const pError: Error; var adStatus: EventStatusEnum; 
                              const pConnection: _Connection); dispid 7;
    procedure Disconnect(var adStatus: EventStatusEnum; const pConnection: _Connection); dispid 8;
  end;

// *********************************************************************//
// DispIntf:  RecordsetEvents
// Flags:     (4096) Dispatchable
// GUID:      {00000266-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  RecordsetEvents = dispinterface
    ['{00000266-0000-0010-8000-00AA006D2EA4}']
    procedure WillChangeField(cFields: Integer; Fields: OleVariant; var adStatus: EventStatusEnum; 
                              const pRecordset: _Recordset); dispid 9;
    procedure FieldChangeComplete(cFields: Integer; Fields: OleVariant; const pError: Error; 
                                  var adStatus: EventStatusEnum; const pRecordset: _Recordset); dispid 10;
    procedure WillChangeRecord(adReason: EventReasonEnum; cRecords: Integer; 
                               var adStatus: EventStatusEnum; const pRecordset: _Recordset); dispid 11;
    procedure RecordChangeComplete(adReason: EventReasonEnum; cRecords: Integer; 
                                   const pError: Error; var adStatus: EventStatusEnum; 
                                   const pRecordset: _Recordset); dispid 12;
    procedure WillChangeRecordset(adReason: EventReasonEnum; var adStatus: EventStatusEnum; 
                                  const pRecordset: _Recordset); dispid 13;
    procedure RecordsetChangeComplete(adReason: EventReasonEnum; const pError: Error; 
                                      var adStatus: EventStatusEnum; const pRecordset: _Recordset); dispid 14;
    procedure WillMove(adReason: EventReasonEnum; var adStatus: EventStatusEnum; 
                       const pRecordset: _Recordset); dispid 15;
    procedure MoveComplete(adReason: EventReasonEnum; const pError: Error; 
                           var adStatus: EventStatusEnum; const pRecordset: _Recordset); dispid 16;
    procedure EndOfRecordset(var fMoreData: WordBool; var adStatus: EventStatusEnum; 
                             const pRecordset: _Recordset); dispid 17;
    procedure FetchProgress(Progress: Integer; MaxProgress: Integer; var adStatus: EventStatusEnum; 
                            const pRecordset: _Recordset); dispid 18;
    procedure FetchComplete(const pError: Error; var adStatus: EventStatusEnum; 
                            const pRecordset: _Recordset); dispid 19;
  end;

// *********************************************************************//
// Interface: ADOConnectionConstruction15
// Flags:     (512) Restricted
// GUID:      {00000516-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ADOConnectionConstruction15 = interface(IUnknown)
    ['{00000516-0000-0010-8000-00AA006D2EA4}']
    function Get_DSO(out ppDSO: IUnknown): HResult; stdcall;
    function Get_Session(out ppSession: IUnknown): HResult; stdcall;
    function WrapDSOandSession(const pDSO: IUnknown; const pSession: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ADOConnectionConstruction
// Flags:     (512) Restricted
// GUID:      {00000551-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ADOConnectionConstruction = interface(ADOConnectionConstruction15)
    ['{00000551-0000-0010-8000-00AA006D2EA4}']
  end;

// *********************************************************************//
// Interface: _Record
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {00000562-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _Record = interface(_ADO)
    ['{00000562-0000-0010-8000-00AA006D2EA4}']
    function Get_ActiveConnection: OleVariant; safecall;
    procedure Set_ActiveConnection(const pvar: WideString); safecall;
    procedure _Set_ActiveConnection(const pvar: _Connection); safecall;
    function Get_State: ObjectStateEnum; safecall;
    function Get_Source: OleVariant; safecall;
    procedure Set_Source(const pvar: WideString); safecall;
    procedure _Set_Source(const pvar: IDispatch); safecall;
    function Get_Mode: ConnectModeEnum; safecall;
    procedure Set_Mode(pMode: ConnectModeEnum); safecall;
    function Get_ParentURL: WideString; safecall;
    function MoveRecord(const Source: WideString; const Destination: WideString; 
                        const UserName: WideString; const Password: WideString; 
                        Options: MoveRecordOptionsEnum; Async: WordBool): WideString; safecall;
    function CopyRecord(const Source: WideString; const Destination: WideString; 
                        const UserName: WideString; const Password: WideString; 
                        Options: CopyRecordOptionsEnum; Async: WordBool): WideString; safecall;
    procedure DeleteRecord(const Source: WideString; Async: WordBool); safecall;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; Mode: ConnectModeEnum; 
                   CreateOptions: RecordCreateOptionsEnum; Options: RecordOpenOptionsEnum; 
                   const UserName: WideString; const Password: WideString); safecall;
    procedure Close; safecall;
    function Get_Fields: Fields; safecall;
    function Get_RecordType: RecordTypeEnum; safecall;
    function GetChildren: _Recordset; safecall;
    procedure Cancel; safecall;
    property State: ObjectStateEnum read Get_State;
    property Mode: ConnectModeEnum read Get_Mode write Set_Mode;
    property ParentURL: WideString read Get_ParentURL;
    property Fields: Fields read Get_Fields;
    property RecordType: RecordTypeEnum read Get_RecordType;
  end;

// *********************************************************************//
// DispIntf:  _RecordDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {00000562-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _RecordDisp = dispinterface
    ['{00000562-0000-0010-8000-00AA006D2EA4}']
    function ActiveConnection: OleVariant; dispid 1;
    property State: ObjectStateEnum readonly dispid 2;
    function Source: OleVariant; dispid 3;
    property Mode: ConnectModeEnum dispid 4;
    property ParentURL: WideString readonly dispid 5;
    function MoveRecord(const Source: WideString; const Destination: WideString; 
                        const UserName: WideString; const Password: WideString; 
                        Options: MoveRecordOptionsEnum; Async: WordBool): WideString; dispid 6;
    function CopyRecord(const Source: WideString; const Destination: WideString; 
                        const UserName: WideString; const Password: WideString; 
                        Options: CopyRecordOptionsEnum; Async: WordBool): WideString; dispid 7;
    procedure DeleteRecord(const Source: WideString; Async: WordBool); dispid 8;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; Mode: ConnectModeEnum; 
                   CreateOptions: RecordCreateOptionsEnum; Options: RecordOpenOptionsEnum; 
                   const UserName: WideString; const Password: WideString); dispid 9;
    procedure Close; dispid 10;
    property Fields: Fields readonly dispid 0;
    property RecordType: RecordTypeEnum readonly dispid 11;
    function GetChildren: _Recordset; dispid 12;
    procedure Cancel; dispid 13;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: _Stream
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {00000565-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _Stream = interface(IDispatch)
    ['{00000565-0000-0010-8000-00AA006D2EA4}']
    function Get_Size: ADO_LONGPTR; safecall;
    function Get_EOS: WordBool; safecall;
    function Get_Position: ADO_LONGPTR; safecall;
    procedure Set_Position(pPos: ADO_LONGPTR); safecall;
    function Get_type_: StreamTypeEnum; safecall;
    procedure Set_type_(ptype: StreamTypeEnum); safecall;
    function Get_LineSeparator: LineSeparatorEnum; safecall;
    procedure Set_LineSeparator(pLS: LineSeparatorEnum); safecall;
    function Get_State: ObjectStateEnum; safecall;
    function Get_Mode: ConnectModeEnum; safecall;
    procedure Set_Mode(pMode: ConnectModeEnum); safecall;
    function Get_Charset: WideString; safecall;
    procedure Set_Charset(const pbstrCharset: WideString); safecall;
    function Read(NumBytes: Integer): OleVariant; safecall;
    procedure Open(Source: OleVariant; Mode: ConnectModeEnum; Options: StreamOpenOptionsEnum; 
                   const UserName: WideString; const Password: WideString); safecall;
    procedure Close; safecall;
    procedure SkipLine; safecall;
    procedure Write(Buffer: OleVariant); safecall;
    procedure SetEOS; safecall;
    procedure CopyTo(const DestStream: _Stream; CharNumber: ADO_LONGPTR); safecall;
    procedure Flush; safecall;
    procedure SaveToFile(const FileName: WideString; Options: SaveOptionsEnum); safecall;
    procedure LoadFromFile(const FileName: WideString); safecall;
    function ReadText(NumChars: Integer): WideString; safecall;
    procedure WriteText(const Data: WideString; Options: StreamWriteEnum); safecall;
    procedure Cancel; safecall;
    property Size: ADO_LONGPTR read Get_Size;
    property EOS: WordBool read Get_EOS;
    property Position: ADO_LONGPTR read Get_Position write Set_Position;
    property type_: StreamTypeEnum read Get_type_ write Set_type_;
    property LineSeparator: LineSeparatorEnum read Get_LineSeparator write Set_LineSeparator;
    property State: ObjectStateEnum read Get_State;
    property Mode: ConnectModeEnum read Get_Mode write Set_Mode;
    property Charset: WideString read Get_Charset write Set_Charset;
  end;

// *********************************************************************//
// DispIntf:  _StreamDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {00000565-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _StreamDisp = dispinterface
    ['{00000565-0000-0010-8000-00AA006D2EA4}']
    property Size: ADO_LONGPTR readonly dispid 1;
    property EOS: WordBool readonly dispid 2;
    property Position: ADO_LONGPTR dispid 3;
    property type_: StreamTypeEnum dispid 4;
    property LineSeparator: LineSeparatorEnum dispid 5;
    property State: ObjectStateEnum readonly dispid 6;
    property Mode: ConnectModeEnum dispid 7;
    property Charset: WideString dispid 8;
    function Read(NumBytes: Integer): OleVariant; dispid 9;
    procedure Open(Source: OleVariant; Mode: ConnectModeEnum; Options: StreamOpenOptionsEnum; 
                   const UserName: WideString; const Password: WideString); dispid 10;
    procedure Close; dispid 11;
    procedure SkipLine; dispid 12;
    procedure Write(Buffer: OleVariant); dispid 13;
    procedure SetEOS; dispid 14;
    procedure CopyTo(const DestStream: _Stream; CharNumber: ADO_LONGPTR); dispid 15;
    procedure Flush; dispid 16;
    procedure SaveToFile(const FileName: WideString; Options: SaveOptionsEnum); dispid 17;
    procedure LoadFromFile(const FileName: WideString); dispid 18;
    function ReadText(NumChars: Integer): WideString; dispid 19;
    procedure WriteText(const Data: WideString; Options: StreamWriteEnum); dispid 20;
    procedure Cancel; dispid 21;
  end;

// *********************************************************************//
// Interface: ADORecordConstruction
// Flags:     (4608) Restricted Dispatchable
// GUID:      {00000567-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ADORecordConstruction = interface(IDispatch)
    ['{00000567-0000-0010-8000-00AA006D2EA4}']
    function Get_Row(out ppRow: IUnknown): HResult; stdcall;
    function Set_Row(const ppRow: IUnknown): HResult; stdcall;
    function Set_ParentRow(const Param1: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ADOStreamConstruction
// Flags:     (4608) Restricted Dispatchable
// GUID:      {00000568-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ADOStreamConstruction = interface(IDispatch)
    ['{00000568-0000-0010-8000-00AA006D2EA4}']
    function Get_Stream(out ppStm: IUnknown): HResult; stdcall;
    function Set_Stream(const ppStm: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ADOCommandConstruction
// Flags:     (512) Restricted
// GUID:      {00000517-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ADOCommandConstruction = interface(IUnknown)
    ['{00000517-0000-0010-8000-00AA006D2EA4}']
    function Get_OLEDBCommand(out ppOLEDBCommand: IUnknown): HResult; stdcall;
    function Set_OLEDBCommand(const ppOLEDBCommand: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ADORecordsetConstruction
// Flags:     (4608) Restricted Dispatchable
// GUID:      {00000283-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ADORecordsetConstruction = interface(IDispatch)
    ['{00000283-0000-0010-8000-00AA006D2EA4}']
    function Get_Rowset(out ppRowset: IUnknown): HResult; stdcall;
    function Set_Rowset(const ppRowset: IUnknown): HResult; stdcall;
    function Get_Chapter(out plChapter: ADO_LONGPTR): HResult; stdcall;
    function Set_Chapter(plChapter: ADO_LONGPTR): HResult; stdcall;
    function Get_RowPosition(out ppRowPos: IUnknown): HResult; stdcall;
    function Set_RowPosition(const ppRowPos: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: Field15
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000505-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Field15 = interface(_ADO)
    ['{00000505-0000-0010-8000-00AA006D2EA4}']
    function Get_ActualSize: ADO_LONGPTR; safecall;
    function Get_Attributes: Integer; safecall;
    function Get_DefinedSize: ADO_LONGPTR; safecall;
    function Get_Name: WideString; safecall;
    function Get_type_: DataTypeEnum; safecall;
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(pvar: OleVariant); safecall;
    function Get_Precision: Byte; safecall;
    function Get_NumericScale: Byte; safecall;
    procedure AppendChunk(Data: OleVariant); safecall;
    function GetChunk(Length: Integer): OleVariant; safecall;
    function Get_OriginalValue: OleVariant; safecall;
    function Get_UnderlyingValue: OleVariant; safecall;
    property ActualSize: ADO_LONGPTR read Get_ActualSize;
    property Attributes: Integer read Get_Attributes;
    property DefinedSize: ADO_LONGPTR read Get_DefinedSize;
    property Name: WideString read Get_Name;
    property type_: DataTypeEnum read Get_type_;
    property Value: OleVariant read Get_Value write Set_Value;
    property Precision: Byte read Get_Precision;
    property NumericScale: Byte read Get_NumericScale;
    property OriginalValue: OleVariant read Get_OriginalValue;
    property UnderlyingValue: OleVariant read Get_UnderlyingValue;
  end;

// *********************************************************************//
// DispIntf:  Field15Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000505-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Field15Disp = dispinterface
    ['{00000505-0000-0010-8000-00AA006D2EA4}']
    property ActualSize: ADO_LONGPTR readonly dispid 1109;
    property Attributes: Integer readonly dispid 1114;
    property DefinedSize: ADO_LONGPTR readonly dispid 1103;
    property Name: WideString readonly dispid 1100;
    property type_: DataTypeEnum readonly dispid 1102;
    property Value: OleVariant dispid 0;
    property Precision: Byte readonly dispid 1112;
    property NumericScale: Byte readonly dispid 1113;
    procedure AppendChunk(Data: OleVariant); dispid 1107;
    function GetChunk(Length: Integer): OleVariant; dispid 1108;
    property OriginalValue: OleVariant readonly dispid 1104;
    property UnderlyingValue: OleVariant readonly dispid 1105;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// The Class CoConnection provides a Create and CreateRemote method to          
// create instances of the default interface _Connection exposed by              
// the CoClass Connection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoConnection = class
    class function Create: _Connection;
    class function CreateRemote(const MachineName: string): _Connection;
  end;

// *********************************************************************//
// The Class CoRecord_ provides a Create and CreateRemote method to          
// create instances of the default interface _Record exposed by              
// the CoClass Record_. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRecord_ = class
    class function Create: _Record;
    class function CreateRemote(const MachineName: string): _Record;
  end;

// *********************************************************************//
// The Class CoStream provides a Create and CreateRemote method to          
// create instances of the default interface _Stream exposed by              
// the CoClass Stream. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStream = class
    class function Create: _Stream;
    class function CreateRemote(const MachineName: string): _Stream;
  end;

// *********************************************************************//
// The Class CoCommand provides a Create and CreateRemote method to          
// create instances of the default interface _Command exposed by              
// the CoClass Command. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCommand = class
    class function Create: _Command;
    class function CreateRemote(const MachineName: string): _Command;
  end;

// *********************************************************************//
// The Class CoRecordset provides a Create and CreateRemote method to          
// create instances of the default interface _Recordset exposed by              
// the CoClass Recordset. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRecordset = class
    class function Create: _Recordset;
    class function CreateRemote(const MachineName: string): _Recordset;
  end;

// *********************************************************************//
// The Class CoParameter provides a Create and CreateRemote method to          
// create instances of the default interface _Parameter exposed by              
// the CoClass Parameter. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoParameter = class
    class function Create: _Parameter;
    class function CreateRemote(const MachineName: string): _Parameter;
  end;

implementation

uses ComObj;

class function CoConnection.Create: _Connection;
begin
  Result := CreateComObject(CLASS_Connection) as _Connection;
end;

class function CoConnection.CreateRemote(const MachineName: string): _Connection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Connection) as _Connection;
end;

class function CoRecord_.Create: _Record;
begin
  Result := CreateComObject(CLASS_Record_) as _Record;
end;

class function CoRecord_.CreateRemote(const MachineName: string): _Record;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Record_) as _Record;
end;

class function CoStream.Create: _Stream;
begin
  Result := CreateComObject(CLASS_Stream) as _Stream;
end;

class function CoStream.CreateRemote(const MachineName: string): _Stream;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Stream) as _Stream;
end;

class function CoCommand.Create: _Command;
begin
  Result := CreateComObject(CLASS_Command) as _Command;
end;

class function CoCommand.CreateRemote(const MachineName: string): _Command;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Command) as _Command;
end;

class function CoRecordset.Create: _Recordset;
begin
  Result := CreateComObject(CLASS_Recordset) as _Recordset;
end;

class function CoRecordset.CreateRemote(const MachineName: string): _Recordset;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Recordset) as _Recordset;
end;

class function CoParameter.Create: _Parameter;
begin
  Result := CreateComObject(CLASS_Parameter) as _Parameter;
end;

class function CoParameter.CreateRemote(const MachineName: string): _Parameter;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Parameter) as _Parameter;
end;

end.
