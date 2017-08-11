unit SearchAPILib_TLB;

// ************************************************************************ //
// WARNUNG                                                                    
// -------                                                                    
// Die in dieser Datei deklarierten Typen wurden aus Daten einer Typbibliothek
// generiert. Wenn diese Typbibliothek explizit oder indirekt (über eine     
// andere Typbibliothek) reimportiert wird oder wenn der Befehl            
// 'Aktualisieren' im Typbibliotheks-Editor während des Bearbeitens der     
// Typbibliothek aktiviert ist, wird der Inhalt dieser Datei neu generiert und 
// alle manuell vorgenommenen Änderungen gehen verloren.                                        
// ************************************************************************ //

// $Rev: 17252 $
// Datei am 11.02.2009 01:10:49 erzeugt aus der unten beschriebenen Typbibliothek.

// ************************************************************************  //
// Typbib.: E:\Proggen\Microsoft SDKs\Windows\v6.0\Lib\SearchAPI.tlb (1)
// LIBID: {00000000-0000-0000-0000-000000000000}
// LCID: 0
// Hilfedatei: 
// Hilfe-String: 
// Liste der Abhäng.: 
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
// Fehler//   Hinweis: Symbol 'type' umbenannt in 'type_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit muss ohne Typüberprüfung für Zeiger compiliert werden. 

{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;
  
// *********************************************************************//
// In der Typbibliothek deklarierte GUIDS. Die folgenden Präfixe werden verwendet:        
//   Typbibliotheken      : LIBID_xxxx                                      
//   CoClasses            : CLASS_xxxx                                      
//   DISPInterfaces       : DIID_xxxx                                       
//   Nicht-DISP-Interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // Haupt- und Nebenversionen der Typbibliothek  SearchAPILibMajorVersion = 1;
  SearchAPILibMinorVersion = 0;

  LIBID_SearchAPILib: TGUID = '{00000000-0000-0000-0000-000000000000}';

  IID_ISearchManager: TGUID = '{AB310581-AC80-11D1-8DF3-00C04FB6EF69}';
  CLASS_CSearchManager: TGUID = '{7D096C5F-AC08-4F1F-BEB7-5C22C517CE39}';
  IID_ISequentialStream: TGUID = '{0C733A30-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IStream: TGUID = '{0000000C-0000-0000-C000-000000000046}';
  IID_IStorage: TGUID = '{0000000B-0000-0000-C000-000000000046}';
  IID_IEnumSTATSTG: TGUID = '{0000000D-0000-0000-C000-000000000046}';
  IID_IRecordInfo: TGUID = '{0000002F-0000-0000-C000-000000000046}';
  IID_ITypeInfo: TGUID = '{00020401-0000-0000-C000-000000000046}';
  IID_ITypeComp: TGUID = '{00020403-0000-0000-C000-000000000046}';
  IID_ITypeLib: TGUID = '{00020402-0000-0000-C000-000000000046}';
  IID_ISearchCatalogManager: TGUID = '{AB310581-AC80-11D1-8DF3-00C04FB6EF50}';
  IID_ISearchPersistentItemsChangedSink: TGUID = '{A2FFDF9B-4758-4F84-B729-DF81A1A0612F}';
  IID_ISearchViewChangedSink: TGUID = '{AB310581-AC80-11D1-8DF3-00C04FB6EF65}';
  IID_ISearchNotifyInlineSite: TGUID = '{B5702E61-E75C-4B64-82A1-6CB4F832FCCF}';
  IID_IEnumString: TGUID = '{00000101-0000-0000-C000-000000000046}';
  IID_ISearchQueryHelper: TGUID = '{AB310581-AC80-11D1-8DF3-00C04FB6EF63}';
  IID_ISearchCrawlScopeManager: TGUID = '{AB310581-AC80-11D1-8DF3-00C04FB6EF55}';
  IID_ISearchRoot: TGUID = '{04C18CCF-1F57-4CBD-88CC-3900F5195CE3}';
  IID_IEnumSearchRoots: TGUID = '{AB310581-AC80-11D1-8DF3-00C04FB6EF52}';
  IID_IEnumSearchScopeRules: TGUID = '{AB310581-AC80-11D1-8DF3-00C04FB6EF54}';
  IID_ISearchScopeRule: TGUID = '{AB310581-AC80-11D1-8DF3-00C04FB6EF53}';
  CLASS_CSearchCatalogManager: TGUID = '{AAB49DD5-AD0B-40AE-B654-AE8976BF6BD2}';
  CLASS_CSearchQueryHelper: TGUID = '{B271E955-09E1-42E1-9B95-5994A534B613}';
  IID_ISearchItemsChangedSink: TGUID = '{AB310581-AC80-11D1-8DF3-00C04FB6EF58}';
  CLASS_CSearchItemsChangedSink: TGUID = '{A5270F6C-19EC-4E17-9EA1-A7074276B9B9}';
  CLASS_CSearchCrawlScopeManager: TGUID = '{321069D2-2D7A-4AA1-9DC3-BA97CDF9AFB4}';
  CLASS_CEnumSearchRoots: TGUID = '{C188CDBA-53E5-4767-9FFB-FAFBD8017DF0}';
  CLASS_CEnumSearchScopeRules: TGUID = '{3379F9F7-F805-4827-9CF6-03DB41F297DA}';
  CLASS_CSearchRoot: TGUID = '{30766BD2-EA1C-4F28-BF27-0B44E2F68DB7}';
  CLASS_CSearchScopeRule: TGUID = '{E63DE750-3BD7-4BE5-9C84-6B4281988C44}';
  CLASS_CSearchPersistentItemsChangedSink: TGUID = '{D0F268B5-EA7A-4B35-BF2F-E1A091B80D51}';

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten  Aufzählungen                    
// *********************************************************************//

// Konstanten für enum tagTYPEKIND
type
  tagTYPEKIND = TOleEnum;
const
  TKIND_ENUM = $00000000;
  TKIND_RECORD = $00000001;
  TKIND_MODULE = $00000002;
  TKIND_INTERFACE = $00000003;
  TKIND_DISPATCH = $00000004;
  TKIND_COCLASS = $00000005;
  TKIND_ALIAS = $00000006;
  TKIND_UNION = $00000007;
  TKIND_MAX = $00000008;

// Konstanten für enum tagDESCKIND
type
  tagDESCKIND = TOleEnum;
const
  DESCKIND_NONE = $00000000;
  DESCKIND_FUNCDESC = $00000001;
  DESCKIND_VARDESC = $00000002;
  DESCKIND_TYPECOMP = $00000003;
  DESCKIND_IMPLICITAPPOBJ = $00000004;
  DESCKIND_MAX = $00000005;

// Konstanten für enum tagFUNCKIND
type
  tagFUNCKIND = TOleEnum;
const
  FUNC_VIRTUAL = $00000000;
  FUNC_PUREVIRTUAL = $00000001;
  FUNC_NONVIRTUAL = $00000002;
  FUNC_STATIC = $00000003;
  FUNC_DISPATCH = $00000004;

// Konstanten für enum tagINVOKEKIND
type
  tagINVOKEKIND = TOleEnum;
const
  INVOKE_FUNC = $00000001;
  INVOKE_PROPERTYGET = $00000002;
  INVOKE_PROPERTYPUT = $00000004;
  INVOKE_PROPERTYPUTREF = $00000008;

// Konstanten für enum tagCALLCONV
type
  tagCALLCONV = TOleEnum;
const
  CC_FASTCALL = $00000000;
  CC_CDECL = $00000001;
  CC_MSCPASCAL = $00000002;
  CC_PASCAL = $00000002;
  CC_MACPASCAL = $00000003;
  CC_STDCALL = $00000004;
  CC_FPFASTCALL = $00000005;
  CC_SYSCALL = $00000006;
  CC_MPWCDECL = $00000007;
  CC_MPWPASCAL = $00000008;
  CC_MAX = $00000009;

// Konstanten für enum tagVARKIND
type
  tagVARKIND = TOleEnum;
const
  VAR_PERINSTANCE = $00000000;
  VAR_STATIC = $00000001;
  VAR_CONST = $00000002;
  VAR_DISPATCH = $00000003;

// Konstanten für enum tagSYSKIND
type
  tagSYSKIND = TOleEnum;
const
  SYS_WIN16 = $00000000;
  SYS_WIN32 = $00000001;
  SYS_MAC = $00000002;
  SYS_WIN64 = $00000003;

// Konstanten für enum _PROXY_ACCESS
type
  _PROXY_ACCESS = TOleEnum;
const
  PROXY_ACCESS_PRECONFIG = $00000000;
  PROXY_ACCESS_DIRECT = $00000001;
  PROXY_ACCESS_PROXY = $00000002;

// Konstanten für enum _CatalogStatus
type
  _CatalogStatus = TOleEnum;
const
  CATALOG_STATUS_IDLE = $00000000;
  CATALOG_STATUS_PAUSED = $00000001;
  CATALOG_STATUS_RECOVERING = $00000002;
  CATALOG_STATUS_FULL_CRAWL = $00000003;
  CATALOG_STATUS_INCREMENTAL_CRAWL = $00000004;
  CATALOG_STATUS_PROCESSING_NOTIFICATIONS = $00000005;
  CATALOG_STATUS_SHUTTING_DOWN = $00000006;

// Konstanten für enum _CatalogPausedReason
type
  _CatalogPausedReason = TOleEnum;
const
  CATALOG_PAUSED_REASON_NONE = $00000000;
  CATALOG_PAUSED_REASON_HIGH_IO = $00000001;
  CATALOG_PAUSED_REASON_HIGH_CPU = $00000002;
  CATALOG_PAUSED_REASON_HIGH_NTF_RATE = $00000003;
  CATALOG_PAUSED_REASON_LOW_BATTERY = $00000004;
  CATALOG_PAUSED_REASON_LOW_MEMORY = $00000005;
  CATALOG_PAUSED_REASON_LOW_DISK = $00000006;
  CATALOG_PAUSED_REASON_DELAYED_RECOVERY = $00000007;
  CATALOG_PAUSED_REASON_USER_ACTIVE = $00000008;
  CATALOG_PAUSED_REASON_EXTERNAL = $00000009;
  CATALOG_PAUSED_REASON_UPGRADING = $0000000A;

// Konstanten für enum _SEARCH_KIND_OF_CHANGE
type
  _SEARCH_KIND_OF_CHANGE = TOleEnum;
const
  SEARCH_CHANGE_ADD = $00000000;
  SEARCH_CHANGE_DELETE = $00000001;
  SEARCH_CHANGE_MODIFY = $00000002;
  SEARCH_CHANGE_MOVE_RENAME = $00000003;
  SEARCH_CHANGE_SEMANTICS_DIRECTORY = $00040000;
  SEARCH_CHANGE_SEMANTICS_SHALLOW = $00080000;
  SEARCH_CHANGE_SEMANTICS_UPDATE_SECURITY = $00400000;

// Konstanten für enum _SEARCH_NOTIFICATION_PRIORITY
type
  _SEARCH_NOTIFICATION_PRIORITY = TOleEnum;
const
  SEARCH_NORMAL_PRIORITY = $00000000;
  SEARCH_HIGH_PRIORITY = $00000001;

// Konstanten für enum _SEARCH_INDEXING_PHASE
type
  _SEARCH_INDEXING_PHASE = TOleEnum;
const
  SEARCH_INDEXING_PHASE_GATHERER = $00000000;
  SEARCH_INDEXING_PHASE_QUERYABLE = $00000001;
  SEARCH_INDEXING_PHASE_PERSISTED = $00000002;

// Konstanten für enum _SEARCH_TERM_EXPANSION
type
  _SEARCH_TERM_EXPANSION = TOleEnum;
const
  SEARCH_TERM_NO_EXPANSION = $00000000;
  SEARCH_TERM_PREFIX_ALL = $00000001;
  SEARCH_TERM_STEM_ALL = $00000002;

// Konstanten für enum _SEARCH_QUERY_SYNTAX
type
  _SEARCH_QUERY_SYNTAX = TOleEnum;
const
  SEARCH_NO_QUERY_SYNTAX = $00000000;
  SEARCH_ADVANCED_QUERY_SYNTAX = $00000001;
  SEARCH_NATURAL_QUERY_SYNTAX = $00000002;

// Konstanten für enum _AUTH_TYPE
type
  _AUTH_TYPE = TOleEnum;
const
  eAUTH_TYPE_ANONYMOUS = $00000000;
  eAUTH_TYPE_NTLM = $00000001;
  eAUTH_TYPE_BASIC = $00000002;

// Konstanten für enum __MIDL___MIDL_itf_searchapi_0000_0012_0001
type
  __MIDL___MIDL_itf_searchapi_0000_0012_0001 = TOleEnum;
const
  CLUSIONREASON_UNKNOWNSCOPE = $00000000;
  CLUSIONREASON_DEFAULT = $00000001;
  CLUSIONREASON_USER = $00000002;
  CLUSIONREASON_GROUPPOLICY = $00000003;

type
// *********************************************************************//
// Forward-Deklaration von in der Typbibliothek definierten Typen                    
// *********************************************************************//

  ISearchManager = interface;
  ISequentialStream = interface;
  IStream = interface;
  IStorage = interface;
  IEnumSTATSTG = interface;
  IRecordInfo = interface;
  ITypeInfo = interface;
  ITypeComp = interface;
  ITypeLib = interface;
  ISearchCatalogManager = interface;
  ISearchPersistentItemsChangedSink = interface;
  ISearchViewChangedSink = interface;
  ISearchNotifyInlineSite = interface;
  IEnumString = interface;
  ISearchQueryHelper = interface;
  ISearchCrawlScopeManager = interface;
  ISearchRoot = interface;
  IEnumSearchRoots = interface;
  IEnumSearchScopeRules = interface;
  ISearchScopeRule = interface;
  ISearchItemsChangedSink = interface;

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten CoClasses             
// (HINWEIS: Hier wird jede CoClass ihrem Standard-Interface zugewiesen)              
// *********************************************************************//


  CSearchManager = ISearchManager;
  CSearchCatalogManager = ISearchCatalogManager;
  CSearchQueryHelper = ISearchQueryHelper;
  CSearchItemsChangedSink = ISearchItemsChangedSink;
  CSearchCrawlScopeManager = ISearchCrawlScopeManager;
  CEnumSearchRoots = IEnumSearchRoots;
  CEnumSearchScopeRules = IEnumSearchScopeRules;
  CSearchRoot = ISearchRoot;
  CSearchScopeRule = ISearchScopeRule;
  CSearchPersistentItemsChangedSink = ISearchPersistentItemsChangedSink;


// *********************************************************************//
// Deklaration von  Strukturen, Unions und Aliasen.                         
// *********************************************************************//
  wirePSAFEARRAY = ^PUserType4; 
  wireSNB = ^tagRemSNB; 
  PUserType5 = ^_FLAGGED_WORD_BLOB; {*}
  PUserType6 = ^_wireVARIANT; {*}
  PUserType13 = ^_wireBRECORD; {*}
  PUserType4 = ^_wireSAFEARRAY; {*}
  PPUserType1 = ^PUserType4; {*}
  PUserType10 = ^tagTYPEDESC; {*}
  PUserType11 = ^tagARRAYDESC; {*}
  PUserType1 = ^tag_inner_PROPVARIANT; {*}
  PByte1 = ^Byte; {*}
  PUserType2 = ^TGUID; {*}
  PUserType3 = ^_FILETIME; {*}
  POleVariant1 = ^OleVariant; {*}
  PUserType7 = ^tagTYPEATTR; {*}
  PUserType8 = ^tagFUNCDESC; {*}
  PUserType9 = ^tagVARDESC; {*}
  PUserType12 = ^tagTLIBATTR; {*}
  PUserType14 = ^_SEARCH_ITEM_PERSISTENT_CHANGE; {*}
  PInteger1 = ^Integer; {*}
  PUserType15 = ^_SEARCH_ITEM_CHANGE; {*}
  PUserType16 = ^_SEARCH_ITEM_INDEXING_STATUS; {*}
  PUserType17 = ^_tagpropertykey; {*}
  PUserType18 = ^_SEARCH_COLUMN_PROPERTIES; {*}

{$ALIGN 8}
  _LARGE_INTEGER = record
    QuadPart: Int64;
  end;

{$ALIGN 8}
  _ULARGE_INTEGER = record
    QuadPart: Largeuint;
  end;

  _FILETIME = record
    dwLowDateTime: LongWord;
    dwHighDateTime: LongWord;
  end;

  tagCLIPDATA = record
    cbSize: LongWord;
    ulClipFmt: Integer;
    pClipData: ^Byte;
  end;

  tagBSTRBLOB = record
    cbSize: LongWord;
    pData: ^Byte;
  end;

  tagBLOB = record
    cbSize: LongWord;
    pBlobData: ^Byte;
  end;

  tagVersionedStream = record
    guidVersion: TGUID;
    pStream: IStream;
  end;


{$ALIGN 8}
  tagSTATSTG = record
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


  tagRemSNB = record
    ulCntStr: LongWord;
    ulCntChar: LongWord;
    rgString: ^Word;
  end;

  tagCAC = record
    cElems: LongWord;
    pElems: ^Byte;
  end;

  tagCAUB = record
    cElems: LongWord;
    pElems: ^Byte;
  end;


  _wireSAFEARR_BSTR = record
    Size: LongWord;
    aBstr: ^PUserType5;
  end;

  _wireSAFEARR_UNKNOWN = record
    Size: LongWord;
    apUnknown: ^IUnknown;
  end;

  _wireSAFEARR_DISPATCH = record
    Size: LongWord;
    apDispatch: ^IDispatch;
  end;

  _FLAGGED_WORD_BLOB = record
    fFlags: LongWord;
    clSize: LongWord;
    asData: ^Word;
  end;


  _wireSAFEARR_VARIANT = record
    Size: LongWord;
    aVariant: ^PUserType6;
  end;


  _wireBRECORD = record
    fFlags: LongWord;
    clSize: LongWord;
    pRecInfo: IRecordInfo;
    pRecord: ^Byte;
  end;


  __MIDL_IOleAutomationTypes_0005 = record
    case Integer of
      0: (lptdesc: PUserType10);
      1: (lpadesc: PUserType11);
      2: (hreftype: LongWord);
  end;

  tagTYPEDESC = record
    __MIDL__IOleAutomationTypes0004: __MIDL_IOleAutomationTypes_0005;
    vt: Word;
  end;

  tagSAFEARRAYBOUND = record
    cElements: LongWord;
    lLbound: Integer;
  end;

  ULONG_PTR = LongWord; 

  tagIDLDESC = record
    dwReserved: ULONG_PTR;
    wIDLFlags: Word;
  end;

  DWORD = LongWord; 

{$ALIGN 8}
  tagPARAMDESCEX = record
    cBytes: LongWord;
    varDefaultValue: OleVariant;
  end;

  tagPARAMDESC = record
    pparamdescex: ^tagPARAMDESCEX;
    wParamFlags: Word;
  end;

  tagELEMDESC = record
    tdesc: tagTYPEDESC;
    paramdesc: tagPARAMDESC;
  end;

  tagFUNCDESC = record
    memid: Integer;
    lprgscode: ^SCODE;
    lprgelemdescParam: ^tagELEMDESC;
    funckind: tagFUNCKIND;
    invkind: tagINVOKEKIND;
    callconv: tagCALLCONV;
    cParams: Smallint;
    cParamsOpt: Smallint;
    oVft: Smallint;
    cScodes: Smallint;
    elemdescFunc: tagELEMDESC;
    wFuncFlags: Word;
  end;

  __MIDL_IOleAutomationTypes_0006 = record
    case Integer of
      0: (oInst: LongWord);
      1: (lpvarValue: ^OleVariant);
  end;

  tagVARDESC = record
    memid: Integer;
    lpstrSchema: PWideChar;
    __MIDL__IOleAutomationTypes0005: __MIDL_IOleAutomationTypes_0006;
    elemdescVar: tagELEMDESC;
    wVarFlags: Word;
    varkind: tagVARKIND;
  end;

  tagTLIBATTR = record
    guid: TGUID;
    lcid: LongWord;
    syskind: tagSYSKIND;
    wMajorVerNum: Word;
    wMinorVerNum: Word;
    wLibFlags: Word;
  end;

  _wireSAFEARR_BRECORD = record
    Size: LongWord;
    aRecord: ^PUserType13;
  end;

  _wireSAFEARR_HAVEIID = record
    Size: LongWord;
    apUnknown: ^IUnknown;
    iid: TGUID;
  end;

  _BYTE_SIZEDARR = record
    clSize: LongWord;
    pData: ^Byte;
  end;

  _SHORT_SIZEDARR = record
    clSize: LongWord;
    pData: ^Word;
  end;

  _LONG_SIZEDARR = record
    clSize: LongWord;
    pData: ^LongWord;
  end;

  _HYPER_SIZEDARR = record
    clSize: LongWord;
    pData: ^Int64;
  end;

  tagCAI = record
    cElems: LongWord;
    pElems: ^Smallint;
  end;

  tagCAUI = record
    cElems: LongWord;
    pElems: ^Word;
  end;

  tagCAL = record
    cElems: LongWord;
    pElems: ^Integer;
  end;

  tagCAUL = record
    cElems: LongWord;
    pElems: ^LongWord;
  end;

  tagCAH = record
    cElems: LongWord;
    pElems: ^_LARGE_INTEGER;
  end;

  tagCAUH = record
    cElems: LongWord;
    pElems: ^_ULARGE_INTEGER;
  end;

  tagCAFLT = record
    cElems: LongWord;
    pElems: ^Single;
  end;

  tagCADBL = record
    cElems: LongWord;
    pElems: ^Double;
  end;

  tagCABOOL = record
    cElems: LongWord;
    pElems: ^WordBool;
  end;

  tagCASCODE = record
    cElems: LongWord;
    pElems: ^SCODE;
  end;

  tagCACY = record
    cElems: LongWord;
    pElems: ^Currency;
  end;

  tagCADATE = record
    cElems: LongWord;
    pElems: ^TDateTime;
  end;

  tagCAFILETIME = record
    cElems: LongWord;
    pElems: ^_FILETIME;
  end;

  tagCACLSID = record
    cElems: LongWord;
    pElems: ^TGUID;
  end;

  tagCACLIPDATA = record
    cElems: LongWord;
    pElems: ^tagCLIPDATA;
  end;

  tagCABSTR = record
    cElems: LongWord;
    pElems: ^WideString;
  end;

  tagCABSTRBLOB = record
    cElems: LongWord;
    pElems: ^tagBSTRBLOB;
  end;

  tagCALPSTR = record
    cElems: LongWord;
    pElems: ^PChar;
  end;

  tagCALPWSTR = record
    cElems: LongWord;
    pElems: ^PWideChar;
  end;


  tagCAPROPVARIANT = record
    cElems: LongWord;
    pElems: PUserType1;
  end;

{$ALIGN 8}
  __MIDL___MIDL_itf_searchapi_0001_0129_0001 = record
    case Integer of
      0: (cVal: Byte);
      1: (bVal: Byte);
      2: (iVal: Smallint);
      3: (uiVal: Word);
      4: (lVal: Integer);
      5: (ulVal: LongWord);
      6: (intVal: SYSINT);
      7: (uintVal: SYSUINT);
      8: (hVal: _LARGE_INTEGER);
      9: (uhVal: _ULARGE_INTEGER);
      10: (fltVal: Single);
      11: (dblVal: Double);
      12: (boolVal: WordBool);
      13: (bool: WordBool);
      14: (scode: SCODE);
      15: (cyVal: Currency);
      16: (date: TDateTime);
      17: (filetime: _FILETIME);
      18: (puuid: ^TGUID);
      19: (pClipData: ^tagCLIPDATA);
      20: (bstrVal: {!!WideString}Pointer);
      21: (bstrblobVal: tagBSTRBLOB);
      22: (blob: tagBLOB);
      23: (pszVal: PChar);
      24: (pwszVal: PWideChar);
      25: (punkVal: {!!IUnknown}Pointer);
      26: (pdispVal: {!!IDispatch}Pointer);
      27: (pStream: {!!IStream}Pointer);
      28: (pStorage: {!!IStorage}Pointer);
      29: (pVersionedStream: ^tagVersionedStream);
      30: (parray: wirePSAFEARRAY);
      31: (cac: tagCAC);
      32: (caub: tagCAUB);
      33: (cai: tagCAI);
      34: (caui: tagCAUI);
      35: (cal: tagCAL);
      36: (caul: tagCAUL);
      37: (cah: tagCAH);
      38: (cauh: tagCAUH);
      39: (caflt: tagCAFLT);
      40: (cadbl: tagCADBL);
      41: (cabool: tagCABOOL);
      42: (cascode: tagCASCODE);
      43: (cacy: tagCACY);
      44: (cadate: tagCADATE);
      45: (cafiletime: tagCAFILETIME);
      46: (cauuid: tagCACLSID);
      47: (caclipdata: tagCACLIPDATA);
      48: (cabstr: tagCABSTR);
      49: (cabstrblob: tagCABSTRBLOB);
      50: (calpstr: tagCALPSTR);
      51: (calpwstr: tagCALPWSTR);
      52: (capropvar: tagCAPROPVARIANT);
      53: (pcVal: ^Byte);
      54: (pbVal: ^Byte);
      55: (piVal: ^Smallint);
      56: (puiVal: ^Word);
      57: (plVal: ^Integer);
      58: (pulVal: ^LongWord);
      59: (pintVal: ^SYSINT);
      60: (puintVal: ^SYSUINT);
      61: (pfltVal: ^Single);
      62: (pdblVal: ^Double);
      63: (pboolVal: ^WordBool);
      64: (pdecVal: ^TDecimal);
      65: (pscode: ^SCODE);
      66: (pcyVal: ^Currency);
      67: (pdate: ^TDateTime);
      68: (pbstrVal: ^WideString);
      69: (ppunkVal: {!!^IUnknown}Pointer);
      70: (ppdispVal: {!!^IDispatch}Pointer);
      71: (pparray: ^wirePSAFEARRAY);
      72: (pvarVal: PUserType1);
  end;

{$ALIGN 4}
  _SEARCH_ITEM_PERSISTENT_CHANGE = record
    Change: _SEARCH_KIND_OF_CHANGE;
    URL: PWideChar;
    OldURL: PWideChar;
    Priority: _SEARCH_NOTIFICATION_PRIORITY;
  end;

  _SEARCH_ITEM_CHANGE = record
    Change: _SEARCH_KIND_OF_CHANGE;
    Priority: _SEARCH_NOTIFICATION_PRIORITY;
    pUserData: ^tagBLOB;
    lpwszURL: PWideChar;
    lpwszOldURL: PWideChar;
  end;

  _SEARCH_ITEM_INDEXING_STATUS = record
    dwDocID: LongWord;
    hrIndexingStatus: HResult;
  end;

  _tagpropertykey = record
    fmtid: TGUID;
    pid: LongWord;
  end;

{$ALIGN 8}
  tag_inner_PROPVARIANT = record
    vt: Word;
    wReserved1: Byte;
    wReserved2: Byte;
    wReserved3: LongWord;
    __MIDL____MIDL_itf_searchapi_0001_01290001: __MIDL___MIDL_itf_searchapi_0001_0129_0001;
  end;

  CLUSION_REASON = __MIDL___MIDL_itf_searchapi_0000_0012_0001; 

{$ALIGN 8}
  _SEARCH_COLUMN_PROPERTIES = record
    Value: tag_inner_PROPVARIANT;
    lcid: LongWord;
  end;


{$ALIGN 8}
  __MIDL_IOleAutomationTypes_0004 = record
    case Integer of
      0: (llVal: Int64);
      1: (lVal: Integer);
      2: (bVal: Byte);
      3: (iVal: Smallint);
      4: (fltVal: Single);
      5: (dblVal: Double);
      6: (boolVal: WordBool);
      7: (scode: SCODE);
      8: (cyVal: Currency);
      9: (date: TDateTime);
      10: (bstrVal: ^_FLAGGED_WORD_BLOB);
      11: (punkVal: {!!IUnknown}Pointer);
      12: (pdispVal: {!!IDispatch}Pointer);
      13: (parray: ^PUserType4);
      14: (brecVal: ^_wireBRECORD);
      15: (pbVal: ^Byte);
      16: (piVal: ^Smallint);
      17: (plVal: ^Integer);
      18: (pllVal: ^Int64);
      19: (pfltVal: ^Single);
      20: (pdblVal: ^Double);
      21: (pboolVal: ^WordBool);
      22: (pscode: ^SCODE);
      23: (pcyVal: ^Currency);
      24: (pdate: ^TDateTime);
      25: (pbstrVal: ^PUserType5);
      26: (ppunkVal: {!!^IUnknown}Pointer);
      27: (ppdispVal: {!!^IDispatch}Pointer);
      28: (pparray: ^PPUserType1);
      29: (pvarVal: ^PUserType6);
      30: (cVal: Byte);
      31: (uiVal: Word);
      32: (ulVal: LongWord);
      33: (ullVal: Largeuint);
      34: (intVal: SYSINT);
      35: (uintVal: SYSUINT);
      36: (decVal: TDecimal);
      37: (pdecVal: ^TDecimal);
      38: (pcVal: ^Byte);
      39: (puiVal: ^Word);
      40: (pulVal: ^LongWord);
      41: (pullVal: ^Largeuint);
      42: (pintVal: ^SYSINT);
      43: (puintVal: ^SYSUINT);
  end;

{$ALIGN 4}
  __MIDL_IOleAutomationTypes_0001 = record
    case Integer of
      0: (BstrStr: _wireSAFEARR_BSTR);
      1: (UnknownStr: _wireSAFEARR_UNKNOWN);
      2: (DispatchStr: _wireSAFEARR_DISPATCH);
      3: (VariantStr: _wireSAFEARR_VARIANT);
      4: (RecordStr: _wireSAFEARR_BRECORD);
      5: (HaveIidStr: _wireSAFEARR_HAVEIID);
      6: (ByteStr: _BYTE_SIZEDARR);
      7: (WordStr: _SHORT_SIZEDARR);
      8: (LongStr: _LONG_SIZEDARR);
      9: (HyperStr: _HYPER_SIZEDARR);
  end;

  _wireSAFEARRAY_UNION = record
    sfType: LongWord;
    u: __MIDL_IOleAutomationTypes_0001;
  end;

{$ALIGN 8}
  _wireVARIANT = record
    clSize: LongWord;
    rpcReserved: LongWord;
    vt: Word;
    wReserved1: Word;
    wReserved2: Word;
    wReserved3: Word;
    __MIDL__IOleAutomationTypes0002: __MIDL_IOleAutomationTypes_0004;
  end;


  tagTYPEATTR = record
    guid: TGUID;
    lcid: LongWord;
    dwReserved: LongWord;
    memidConstructor: Integer;
    memidDestructor: Integer;
    lpstrSchema: PWideChar;
    cbSizeInstance: LongWord;
    typekind: tagTYPEKIND;
    cFuncs: Word;
    cVars: Word;
    cImplTypes: Word;
    cbSizeVft: Word;
    cbAlignment: Word;
    wTypeFlags: Word;
    wMajorVerNum: Word;
    wMinorVerNum: Word;
    tdescAlias: tagTYPEDESC;
    idldescType: tagIDLDESC;
  end;

  tagARRAYDESC = record
    tdescElem: tagTYPEDESC;
    cDims: Word;
    rgbounds: ^tagSAFEARRAYBOUND;
  end;


  _wireSAFEARRAY = record
    cDims: Word;
    fFeatures: Word;
    cbElements: LongWord;
    cLocks: LongWord;
    uArrayStructs: _wireSAFEARRAY_UNION;
    rgsabound: ^tagSAFEARRAYBOUND;
  end;


// *********************************************************************//
// Interface: ISearchManager
// Flags:     (0)
// GUID:      {AB310581-AC80-11D1-8DF3-00C04FB6EF69}
// *********************************************************************//
  ISearchManager = interface(IUnknown)
    ['{AB310581-AC80-11D1-8DF3-00C04FB6EF69}']
    function GetIndexerVersionStr(out ppszVersionString: PWideChar): HResult; stdcall;
    function GetIndexerVersion(out pdwMajor: LongWord; out pdwMinor: LongWord): HResult; stdcall;
    function GetParameter(pszName: PWideChar; out ppValue: PUserType1): HResult; stdcall;
    function SetParameter(pszName: PWideChar; var pValue: tag_inner_PROPVARIANT): HResult; stdcall;
    function Get_ProxyName(out ppszProxyName: PWideChar): HResult; stdcall;
    function Get_BypassList(out ppszBypassList: PWideChar): HResult; stdcall;
    function SetProxy(sUseProxy: _PROXY_ACCESS; fLocalByPassProxy: Integer; dwPortNumber: LongWord; 
                      pszProxyName: PWideChar; pszByPassList: PWideChar): HResult; stdcall;
    function GetCatalog(pszCatalog: PWideChar; out ppCatalogManager: ISearchCatalogManager): HResult; stdcall;
    function Get_UserAgent(out ppszUserAgent: PWideChar): HResult; stdcall;
    function Set_UserAgent(ppszUserAgent: PWideChar): HResult; stdcall;
    function Get_UseProxy(out pUseProxy: _PROXY_ACCESS): HResult; stdcall;
    function Get_LocalBypass(out pfLocalBypass: Integer): HResult; stdcall;
    function Get_PortNumber(out pdwPortNumber: LongWord): HResult; stdcall;
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
// Interface: IStorage
// Flags:     (0)
// GUID:      {0000000B-0000-0000-C000-000000000046}
// *********************************************************************//
  IStorage = interface(IUnknown)
    ['{0000000B-0000-0000-C000-000000000046}']
    function CreateStream(pwcsName: PWideChar; grfMode: LongWord; reserved1: LongWord; 
                          reserved2: LongWord; out ppstm: IStream): HResult; stdcall;
    function RemoteOpenStream(pwcsName: PWideChar; cbReserved1: LongWord; var reserved1: Byte; 
                              grfMode: LongWord; reserved2: LongWord; out ppstm: IStream): HResult; stdcall;
    function CreateStorage(pwcsName: PWideChar; grfMode: LongWord; reserved1: LongWord; 
                           reserved2: LongWord; out ppstg: IStorage): HResult; stdcall;
    function OpenStorage(pwcsName: PWideChar; const pstgPriority: IStorage; grfMode: LongWord; 
                         var snbExclude: tagRemSNB; reserved: LongWord; out ppstg: IStorage): HResult; stdcall;
    function RemoteCopyTo(ciidExclude: LongWord; var rgiidExclude: TGUID; 
                          var snbExclude: tagRemSNB; const pstgDest: IStorage): HResult; stdcall;
    function MoveElementTo(pwcsName: PWideChar; const pstgDest: IStorage; pwcsNewName: PWideChar; 
                           grfFlags: LongWord): HResult; stdcall;
    function Commit(grfCommitFlags: LongWord): HResult; stdcall;
    function Revert: HResult; stdcall;
    function RemoteEnumElements(reserved1: LongWord; cbReserved2: LongWord; var reserved2: Byte; 
                                reserved3: LongWord; out ppenum: IEnumSTATSTG): HResult; stdcall;
    function DestroyElement(pwcsName: PWideChar): HResult; stdcall;
    function RenameElement(pwcsOldName: PWideChar; pwcsNewName: PWideChar): HResult; stdcall;
    function SetElementTimes(pwcsName: PWideChar; var pctime: _FILETIME; var patime: _FILETIME; 
                             var pmtime: _FILETIME): HResult; stdcall;
    function SetClass(var clsid: TGUID): HResult; stdcall;
    function SetStateBits(grfStateBits: LongWord; grfMask: LongWord): HResult; stdcall;
    function Stat(out pstatstg: tagSTATSTG; grfStatFlag: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumSTATSTG
// Flags:     (0)
// GUID:      {0000000D-0000-0000-C000-000000000046}
// *********************************************************************//
  IEnumSTATSTG = interface(IUnknown)
    ['{0000000D-0000-0000-C000-000000000046}']
    function RemoteNext(celt: LongWord; out rgelt: tagSTATSTG; out pceltFetched: LongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppenum: IEnumSTATSTG): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IRecordInfo
// Flags:     (0)
// GUID:      {0000002F-0000-0000-C000-000000000046}
// *********************************************************************//
  IRecordInfo = interface(IUnknown)
    ['{0000002F-0000-0000-C000-000000000046}']
    function RecordInit(out pvNew: Pointer): HResult; stdcall;
    function RecordClear(var pvExisting: Pointer): HResult; stdcall;
    function RecordCopy(var pvExisting: Pointer; out pvNew: Pointer): HResult; stdcall;
    function GetGuid(out pguid: TGUID): HResult; stdcall;
    function GetName(out pbstrName: WideString): HResult; stdcall;
    function GetSize(out pcbSize: LongWord): HResult; stdcall;
    function GetTypeInfo(out ppTypeInfo: ITypeInfo): HResult; stdcall;
    function GetField(var pvData: Pointer; szFieldName: PWideChar; out pvarField: OleVariant): HResult; stdcall;
    function GetFieldNoCopy(var pvData: Pointer; szFieldName: PWideChar; out pvarField: OleVariant; 
                            out ppvDataCArray: Pointer): HResult; stdcall;
    function PutField(wFlags: LongWord; var pvData: Pointer; szFieldName: PWideChar; 
                      var pvarField: OleVariant): HResult; stdcall;
    function PutFieldNoCopy(wFlags: LongWord; var pvData: Pointer; szFieldName: PWideChar; 
                            var pvarField: OleVariant): HResult; stdcall;
    function GetFieldNames(var pcNames: LongWord; out rgBstrNames: WideString): HResult; stdcall;
    function IsMatchingType(const pRecordInfo: IRecordInfo): Integer; stdcall;
    function RecordCreate: Pointer; stdcall;
    function RecordCreateCopy(var pvSource: Pointer; out ppvDest: Pointer): HResult; stdcall;
    function RecordDestroy(var pvRecord: Pointer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITypeInfo
// Flags:     (0)
// GUID:      {00020401-0000-0000-C000-000000000046}
// *********************************************************************//
  ITypeInfo = interface(IUnknown)
    ['{00020401-0000-0000-C000-000000000046}']
    function RemoteGetTypeAttr(out ppTypeAttr: PUserType7; out pDummy: DWORD): HResult; stdcall;
    function GetTypeComp(out ppTComp: ITypeComp): HResult; stdcall;
    function RemoteGetFuncDesc(index: SYSUINT; out ppFuncDesc: PUserType8; out pDummy: DWORD): HResult; stdcall;
    function RemoteGetVarDesc(index: SYSUINT; out ppVarDesc: PUserType9; out pDummy: DWORD): HResult; stdcall;
    function RemoteGetNames(memid: Integer; out rgBstrNames: WideString; cMaxNames: SYSUINT; 
                            out pcNames: SYSUINT): HResult; stdcall;
    function GetRefTypeOfImplType(index: SYSUINT; out pRefType: LongWord): HResult; stdcall;
    function GetImplTypeFlags(index: SYSUINT; out pImplTypeFlags: SYSINT): HResult; stdcall;
    function LocalGetIDsOfNames: HResult; stdcall;
    function LocalInvoke: HResult; stdcall;
    function RemoteGetDocumentation(memid: Integer; refPtrFlags: LongWord; 
                                    out pbstrName: WideString; out pBstrDocString: WideString; 
                                    out pdwHelpContext: LongWord; out pBstrHelpFile: WideString): HResult; stdcall;
    function RemoteGetDllEntry(memid: Integer; invkind: tagINVOKEKIND; refPtrFlags: LongWord; 
                               out pBstrDllName: WideString; out pbstrName: WideString; 
                               out pwOrdinal: Word): HResult; stdcall;
    function GetRefTypeInfo(hreftype: LongWord; out ppTInfo: ITypeInfo): HResult; stdcall;
    function LocalAddressOfMember: HResult; stdcall;
    function RemoteCreateInstance(var riid: TGUID; out ppvObj: IUnknown): HResult; stdcall;
    function GetMops(memid: Integer; out pBstrMops: WideString): HResult; stdcall;
    function RemoteGetContainingTypeLib(out ppTLib: ITypeLib; out pIndex: SYSUINT): HResult; stdcall;
    function LocalReleaseTypeAttr: HResult; stdcall;
    function LocalReleaseFuncDesc: HResult; stdcall;
    function LocalReleaseVarDesc: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITypeComp
// Flags:     (0)
// GUID:      {00020403-0000-0000-C000-000000000046}
// *********************************************************************//
  ITypeComp = interface(IUnknown)
    ['{00020403-0000-0000-C000-000000000046}']
    function RemoteBind(szName: PWideChar; lHashVal: LongWord; wFlags: Word; 
                        out ppTInfo: ITypeInfo; out pDescKind: tagDESCKIND; 
                        out ppFuncDesc: PUserType8; out ppVarDesc: PUserType9; 
                        out ppTypeComp: ITypeComp; out pDummy: DWORD): HResult; stdcall;
    function RemoteBindType(szName: PWideChar; lHashVal: LongWord; out ppTInfo: ITypeInfo): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITypeLib
// Flags:     (0)
// GUID:      {00020402-0000-0000-C000-000000000046}
// *********************************************************************//
  ITypeLib = interface(IUnknown)
    ['{00020402-0000-0000-C000-000000000046}']
    function RemoteGetTypeInfoCount(out pcTInfo: SYSUINT): HResult; stdcall;
    function GetTypeInfo(index: SYSUINT; out ppTInfo: ITypeInfo): HResult; stdcall;
    function GetTypeInfoType(index: SYSUINT; out pTKind: tagTYPEKIND): HResult; stdcall;
    function GetTypeInfoOfGuid(var guid: TGUID; out ppTInfo: ITypeInfo): HResult; stdcall;
    function RemoteGetLibAttr(out ppTLibAttr: PUserType12; out pDummy: DWORD): HResult; stdcall;
    function GetTypeComp(out ppTComp: ITypeComp): HResult; stdcall;
    function RemoteGetDocumentation(index: SYSINT; refPtrFlags: LongWord; 
                                    out pbstrName: WideString; out pBstrDocString: WideString; 
                                    out pdwHelpContext: LongWord; out pBstrHelpFile: WideString): HResult; stdcall;
    function RemoteIsName(szNameBuf: PWideChar; lHashVal: LongWord; out pfName: Integer; 
                          out pBstrLibName: WideString): HResult; stdcall;
    function RemoteFindName(szNameBuf: PWideChar; lHashVal: LongWord; out ppTInfo: ITypeInfo; 
                            out rgMemId: Integer; var pcFound: Word; out pBstrLibName: WideString): HResult; stdcall;
    function LocalReleaseTLibAttr: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISearchCatalogManager
// Flags:     (0)
// GUID:      {AB310581-AC80-11D1-8DF3-00C04FB6EF50}
// *********************************************************************//
  ISearchCatalogManager = interface(IUnknown)
    ['{AB310581-AC80-11D1-8DF3-00C04FB6EF50}']
    function Get_Name(out pszName: PWideChar): HResult; stdcall;
    function GetParameter(pszName: PWideChar; out ppValue: PUserType1): HResult; stdcall;
    function SetParameter(pszName: PWideChar; var pValue: tag_inner_PROPVARIANT): HResult; stdcall;
    function GetCatalogStatus(out pStatus: _CatalogStatus; out pPausedReason: _CatalogPausedReason): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Reindex: HResult; stdcall;
    function ReindexMatchingURLs(pszPattern: PWideChar): HResult; stdcall;
    function ReindexSearchRoot(pszRootURL: PWideChar): HResult; stdcall;
    function Set_ConnectTimeout(pdwConnectTimeout: LongWord): HResult; stdcall;
    function Get_ConnectTimeout(out pdwConnectTimeout: LongWord): HResult; stdcall;
    function Set_DataTimeout(pdwDataTimeout: LongWord): HResult; stdcall;
    function Get_DataTimeout(out pdwDataTimeout: LongWord): HResult; stdcall;
    function NumberOfItems(out plCount: Integer): HResult; stdcall;
    function NumberOfItemsToIndex(out plIncrementalCount: Integer; 
                                  out plNotificationQueue: Integer; out plHighPriorityQueue: Integer): HResult; stdcall;
    function URLBeingIndexed(out pszUrl: PWideChar): HResult; stdcall;
    function GetURLIndexingState(pszUrl: PWideChar; out pdwState: LongWord): HResult; stdcall;
    function GetPersistentItemsChangedSink(out ppISearchPersistentItemsChangedSink: ISearchPersistentItemsChangedSink): HResult; stdcall;
    function RegisterViewForNotification(pszView: PWideChar; 
                                         const pViewChangedSink: ISearchViewChangedSink; 
                                         out pdwCookie: LongWord): HResult; stdcall;
    function GetItemsChangedSink(const pISearchNotifyInlineSite: ISearchNotifyInlineSite; 
                                 var riid: TGUID; out ppv: Pointer; 
                                 out pGUIDCatalogResetSignature: TGUID; 
                                 out pGUIDCheckPointSignature: TGUID; 
                                 out pdwLastCheckPointNumber: LongWord): HResult; stdcall;
    function UnregisterViewForNotification(dwCookie: LongWord): HResult; stdcall;
    function SetExtensionClusion(pszExtension: PWideChar; fExclude: Integer): HResult; stdcall;
    function EnumerateExcludedExtensions(out ppExtensions: IEnumString): HResult; stdcall;
    function GetQueryHelper(out ppSearchQueryHelper: ISearchQueryHelper): HResult; stdcall;
    function Set_DiacriticSensitivity(pfDiacriticSensitive: Integer): HResult; stdcall;
    function Get_DiacriticSensitivity(out pfDiacriticSensitive: Integer): HResult; stdcall;
    function GetCrawlScopeManager(out ppCrawlScopeManager: ISearchCrawlScopeManager): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISearchPersistentItemsChangedSink
// Flags:     (0)
// GUID:      {A2FFDF9B-4758-4F84-B729-DF81A1A0612F}
// *********************************************************************//
  ISearchPersistentItemsChangedSink = interface(IUnknown)
    ['{A2FFDF9B-4758-4F84-B729-DF81A1A0612F}']
    function StartedMonitoringScope(pszUrl: PWideChar): HResult; stdcall;
    function StoppedMonitoringScope(pszUrl: PWideChar): HResult; stdcall;
    function OnItemsChanged(dwNumberOfChanges: LongWord; 
                            var DataChangeEntries: _SEARCH_ITEM_PERSISTENT_CHANGE; 
                            out hrCompletionCodes: HResult): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISearchViewChangedSink
// Flags:     (0)
// GUID:      {AB310581-AC80-11D1-8DF3-00C04FB6EF65}
// *********************************************************************//
  ISearchViewChangedSink = interface(IUnknown)
    ['{AB310581-AC80-11D1-8DF3-00C04FB6EF65}']
    function OnChange(var pdwDocID: Integer; var pChange: _SEARCH_ITEM_CHANGE; var pfInView: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISearchNotifyInlineSite
// Flags:     (0)
// GUID:      {B5702E61-E75C-4B64-82A1-6CB4F832FCCF}
// *********************************************************************//
  ISearchNotifyInlineSite = interface(IUnknown)
    ['{B5702E61-E75C-4B64-82A1-6CB4F832FCCF}']
    function OnItemIndexedStatusChange(sipStatus: _SEARCH_INDEXING_PHASE; dwNumEntries: LongWord; 
                                       var rgItemStatusEntries: _SEARCH_ITEM_INDEXING_STATUS): HResult; stdcall;
    function OnCatalogStatusChange(var guidCatalogResetSignature: TGUID; 
                                   var guidCheckPointSignature: TGUID; 
                                   dwLastCheckPointNumber: LongWord): HResult; stdcall;
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
    function Clone(out ppenum: IEnumString): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISearchQueryHelper
// Flags:     (0)
// GUID:      {AB310581-AC80-11D1-8DF3-00C04FB6EF63}
// *********************************************************************//
  ISearchQueryHelper = interface(IUnknown)
    ['{AB310581-AC80-11D1-8DF3-00C04FB6EF63}']
    function Get_ConnectionString(out pszConnectionString: PWideChar): HResult; stdcall;
    function Set_QueryContentLocale(plcid: LongWord): HResult; stdcall;
    function Get_QueryContentLocale(out plcid: LongWord): HResult; stdcall;
    function Set_QueryKeywordLocale(plcid: LongWord): HResult; stdcall;
    function Get_QueryKeywordLocale(out plcid: LongWord): HResult; stdcall;
    function Set_QueryTermExpansion(pExpandTerms: _SEARCH_TERM_EXPANSION): HResult; stdcall;
    function Get_QueryTermExpansion(out pExpandTerms: _SEARCH_TERM_EXPANSION): HResult; stdcall;
    function Set_QuerySyntax(pQuerySyntax: _SEARCH_QUERY_SYNTAX): HResult; stdcall;
    function Get_QuerySyntax(out pQuerySyntax: _SEARCH_QUERY_SYNTAX): HResult; stdcall;
    function Set_QueryContentProperties(ppszContentProperties: PWideChar): HResult; stdcall;
    function Get_QueryContentProperties(out ppszContentProperties: PWideChar): HResult; stdcall;
    function Set_QuerySelectColumns(ppszSelectColumns: PWideChar): HResult; stdcall;
    function Get_QuerySelectColumns(out ppszSelectColumns: PWideChar): HResult; stdcall;
    function Set_QueryWhereRestrictions(ppszRestrictions: PWideChar): HResult; stdcall;
    function Get_QueryWhereRestrictions(out ppszRestrictions: PWideChar): HResult; stdcall;
    function Set_QuerySorting(ppszSorting: PWideChar): HResult; stdcall;
    function Get_QuerySorting(out ppszSorting: PWideChar): HResult; stdcall;
    function GenerateSQLFromUserQuery(pszQuery: PWideChar; out ppszSQL: PWideChar): HResult; stdcall;
    function WriteProperties(itemID: Integer; dwNumberOfColumns: LongWord; 
                             var pColumns: _tagpropertykey; var pValues: _SEARCH_COLUMN_PROPERTIES; 
                             var pftGatherModifiedTime: _FILETIME): HResult; stdcall;
    function Set_QueryMaxResults(pcMaxResults: Integer): HResult; stdcall;
    function Get_QueryMaxResults(out pcMaxResults: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISearchCrawlScopeManager
// Flags:     (0)
// GUID:      {AB310581-AC80-11D1-8DF3-00C04FB6EF55}
// *********************************************************************//
  ISearchCrawlScopeManager = interface(IUnknown)
    ['{AB310581-AC80-11D1-8DF3-00C04FB6EF55}']
    function AddDefaultScopeRule(pszUrl: PWideChar; fInclude: Integer; fFollowFlags: LongWord): HResult; stdcall;
    function AddRoot(const pSearchRoot: ISearchRoot): HResult; stdcall;
    function RemoveRoot(pszUrl: PWideChar): HResult; stdcall;
    function EnumerateRoots(out ppSearchRoots: IEnumSearchRoots): HResult; stdcall;
    function AddHierarchicalScope(pszUrl: PWideChar; fInclude: Integer; fDefault: Integer; 
                                  fOverrideChildren: Integer): HResult; stdcall;
    function AddUserScopeRule(pszUrl: PWideChar; fInclude: Integer; fOverrideChildren: Integer; 
                              fFollowFlags: LongWord): HResult; stdcall;
    function RemoveScopeRule(pszRule: PWideChar): HResult; stdcall;
    function EnumerateScopeRules(out ppSearchScopeRules: IEnumSearchScopeRules): HResult; stdcall;
    function HasParentScopeRule(pszUrl: PWideChar; out pfHasParentRule: Integer): HResult; stdcall;
    function HasChildScopeRule(pszUrl: PWideChar; out pfHasChildRule: Integer): HResult; stdcall;
    function IncludedInCrawlScope(pszUrl: PWideChar; out pfIsIncluded: Integer): HResult; stdcall;
    function IncludedInCrawlScopeEx(pszUrl: PWideChar; out pfIsIncluded: Integer; 
                                    out pReason: CLUSION_REASON): HResult; stdcall;
    function RevertToDefaultScopes: HResult; stdcall;
    function SaveAll: HResult; stdcall;
    function GetParentScopeVersionId(pszUrl: PWideChar; out plScopeId: Integer): HResult; stdcall;
    function RemoveDefaultScopeRule(pszUrl: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISearchRoot
// Flags:     (0)
// GUID:      {04C18CCF-1F57-4CBD-88CC-3900F5195CE3}
// *********************************************************************//
  ISearchRoot = interface(IUnknown)
    ['{04C18CCF-1F57-4CBD-88CC-3900F5195CE3}']
    function Set_Schedule(ppszTaskArg: PWideChar): HResult; stdcall;
    function Get_Schedule(out ppszTaskArg: PWideChar): HResult; stdcall;
    function Set_RootURL(ppszURL: PWideChar): HResult; stdcall;
    function Get_RootURL(out ppszURL: PWideChar): HResult; stdcall;
    function Set_IsHierarchical(pfIsHierarchical: Integer): HResult; stdcall;
    function Get_IsHierarchical(out pfIsHierarchical: Integer): HResult; stdcall;
    function Set_ProvidesNotifications(pfProvidesNotifications: Integer): HResult; stdcall;
    function Get_ProvidesNotifications(out pfProvidesNotifications: Integer): HResult; stdcall;
    function Set_UseNotificationsOnly(pfUseNotificationsOnly: Integer): HResult; stdcall;
    function Get_UseNotificationsOnly(out pfUseNotificationsOnly: Integer): HResult; stdcall;
    function Set_EnumerationDepth(pdwDepth: LongWord): HResult; stdcall;
    function Get_EnumerationDepth(out pdwDepth: LongWord): HResult; stdcall;
    function Set_HostDepth(pdwDepth: LongWord): HResult; stdcall;
    function Get_HostDepth(out pdwDepth: LongWord): HResult; stdcall;
    function Set_FollowDirectories(pfFollowDirectories: Integer): HResult; stdcall;
    function Get_FollowDirectories(out pfFollowDirectories: Integer): HResult; stdcall;
    function Set_AuthenticationType(pAuthType: _AUTH_TYPE): HResult; stdcall;
    function Get_AuthenticationType(out pAuthType: _AUTH_TYPE): HResult; stdcall;
    function Set_User(ppszUser: PWideChar): HResult; stdcall;
    function Get_User(out ppszUser: PWideChar): HResult; stdcall;
    function Set_Password(ppszPassword: PWideChar): HResult; stdcall;
    function Get_Password(out ppszPassword: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumSearchRoots
// Flags:     (0)
// GUID:      {AB310581-AC80-11D1-8DF3-00C04FB6EF52}
// *********************************************************************//
  IEnumSearchRoots = interface(IUnknown)
    ['{AB310581-AC80-11D1-8DF3-00C04FB6EF52}']
    function Next(celt: LongWord; out rgelt: ISearchRoot; var pceltFetched: LongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppenum: IEnumSearchRoots): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumSearchScopeRules
// Flags:     (0)
// GUID:      {AB310581-AC80-11D1-8DF3-00C04FB6EF54}
// *********************************************************************//
  IEnumSearchScopeRules = interface(IUnknown)
    ['{AB310581-AC80-11D1-8DF3-00C04FB6EF54}']
    function Next(celt: LongWord; out pprgelt: ISearchScopeRule; var pceltFetched: LongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppenum: IEnumSearchScopeRules): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISearchScopeRule
// Flags:     (0)
// GUID:      {AB310581-AC80-11D1-8DF3-00C04FB6EF53}
// *********************************************************************//
  ISearchScopeRule = interface(IUnknown)
    ['{AB310581-AC80-11D1-8DF3-00C04FB6EF53}']
    function Get_PatternOrURL(out ppszPatternOrURL: PWideChar): HResult; stdcall;
    function Get_IsIncluded(out pfIsIncluded: Integer): HResult; stdcall;
    function Get_IsDefault(out pfIsDefault: Integer): HResult; stdcall;
    function Get_FollowFlags(out pFollowFlags: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISearchItemsChangedSink
// Flags:     (0)
// GUID:      {AB310581-AC80-11D1-8DF3-00C04FB6EF58}
// *********************************************************************//
  ISearchItemsChangedSink = interface(IUnknown)
    ['{AB310581-AC80-11D1-8DF3-00C04FB6EF58}']
    function StartedMonitoringScope(pszUrl: PWideChar): HResult; stdcall;
    function StoppedMonitoringScope(pszUrl: PWideChar): HResult; stdcall;
    function OnItemsChanged(dwNumberOfChanges: LongWord; 
                            var rgDataChangeEntries: _SEARCH_ITEM_CHANGE; out rgdwDocIds: LongWord; 
                            out rghrCompletionCodes: HResult): HResult; stdcall;
  end;

// *********************************************************************//
// Die Klasse CoCSearchManager stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen des Standard-Interface ISearchManager, dargestellt 
// von CoClass CSearchManager, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// will, die von dieser Typbibliothek dargestellt werden.                                            
// *********************************************************************//

  CoCSearchManager = class
    class function Create: ISearchManager;
    class function CreateRemote(const MachineName: string): ISearchManager;
  end;

// *********************************************************************//
// Die Klasse CoCSearchCatalogManager stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen des Standard-Interface ISearchCatalogManager, dargestellt 
// von CoClass CSearchCatalogManager, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// will, die von dieser Typbibliothek dargestellt werden.                                            
// *********************************************************************//

  CoCSearchCatalogManager = class
    class function Create: ISearchCatalogManager;
    class function CreateRemote(const MachineName: string): ISearchCatalogManager;
  end;

// *********************************************************************//
// Die Klasse CoCSearchQueryHelper stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen des Standard-Interface ISearchQueryHelper, dargestellt 
// von CoClass CSearchQueryHelper, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// will, die von dieser Typbibliothek dargestellt werden.                                            
// *********************************************************************//

  CoCSearchQueryHelper = class
    class function Create: ISearchQueryHelper;
    class function CreateRemote(const MachineName: string): ISearchQueryHelper;
  end;

// *********************************************************************//
// Die Klasse CoCSearchItemsChangedSink stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen des Standard-Interface ISearchItemsChangedSink, dargestellt 
// von CoClass CSearchItemsChangedSink, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// will, die von dieser Typbibliothek dargestellt werden.                                            
// *********************************************************************//

  CoCSearchItemsChangedSink = class
    class function Create: ISearchItemsChangedSink;
    class function CreateRemote(const MachineName: string): ISearchItemsChangedSink;
  end;

// *********************************************************************//
// Die Klasse CoCSearchCrawlScopeManager stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen des Standard-Interface ISearchCrawlScopeManager, dargestellt 
// von CoClass CSearchCrawlScopeManager, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// will, die von dieser Typbibliothek dargestellt werden.                                            
// *********************************************************************//

  CoCSearchCrawlScopeManager = class
    class function Create: ISearchCrawlScopeManager;
    class function CreateRemote(const MachineName: string): ISearchCrawlScopeManager;
  end;

// *********************************************************************//
// Die Klasse CoCEnumSearchRoots stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen des Standard-Interface IEnumSearchRoots, dargestellt 
// von CoClass CEnumSearchRoots, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// will, die von dieser Typbibliothek dargestellt werden.                                            
// *********************************************************************//

  CoCEnumSearchRoots = class
    class function Create: IEnumSearchRoots;
    class function CreateRemote(const MachineName: string): IEnumSearchRoots;
  end;

// *********************************************************************//
// Die Klasse CoCEnumSearchScopeRules stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen des Standard-Interface IEnumSearchScopeRules, dargestellt 
// von CoClass CEnumSearchScopeRules, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// will, die von dieser Typbibliothek dargestellt werden.                                            
// *********************************************************************//

  CoCEnumSearchScopeRules = class
    class function Create: IEnumSearchScopeRules;
    class function CreateRemote(const MachineName: string): IEnumSearchScopeRules;
  end;

// *********************************************************************//
// Die Klasse CoCSearchRoot stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen des Standard-Interface ISearchRoot, dargestellt 
// von CoClass CSearchRoot, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// will, die von dieser Typbibliothek dargestellt werden.                                            
// *********************************************************************//

  CoCSearchRoot = class
    class function Create: ISearchRoot;
    class function CreateRemote(const MachineName: string): ISearchRoot;
  end;

// *********************************************************************//
// Die Klasse CoCSearchScopeRule stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen des Standard-Interface ISearchScopeRule, dargestellt 
// von CoClass CSearchScopeRule, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// will, die von dieser Typbibliothek dargestellt werden.                                            
// *********************************************************************//

  CoCSearchScopeRule = class
    class function Create: ISearchScopeRule;
    class function CreateRemote(const MachineName: string): ISearchScopeRule;
  end;

// *********************************************************************//
// Die Klasse CoCSearchPersistentItemsChangedSink stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen des Standard-Interface ISearchPersistentItemsChangedSink, dargestellt 
// von CoClass CSearchPersistentItemsChangedSink, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// will, die von dieser Typbibliothek dargestellt werden.                                            
// *********************************************************************//

  CoCSearchPersistentItemsChangedSink = class
    class function Create: ISearchPersistentItemsChangedSink;
    class function CreateRemote(const MachineName: string): ISearchPersistentItemsChangedSink;
  end;

implementation

uses ComObj;

class function CoCSearchManager.Create: ISearchManager;
begin
  Result := CreateComObject(CLASS_CSearchManager) as ISearchManager;
end;

class function CoCSearchManager.CreateRemote(const MachineName: string): ISearchManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CSearchManager) as ISearchManager;
end;

class function CoCSearchCatalogManager.Create: ISearchCatalogManager;
begin
  Result := CreateComObject(CLASS_CSearchCatalogManager) as ISearchCatalogManager;
end;

class function CoCSearchCatalogManager.CreateRemote(const MachineName: string): ISearchCatalogManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CSearchCatalogManager) as ISearchCatalogManager;
end;

class function CoCSearchQueryHelper.Create: ISearchQueryHelper;
begin
  Result := CreateComObject(CLASS_CSearchQueryHelper) as ISearchQueryHelper;
end;

class function CoCSearchQueryHelper.CreateRemote(const MachineName: string): ISearchQueryHelper;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CSearchQueryHelper) as ISearchQueryHelper;
end;

class function CoCSearchItemsChangedSink.Create: ISearchItemsChangedSink;
begin
  Result := CreateComObject(CLASS_CSearchItemsChangedSink) as ISearchItemsChangedSink;
end;

class function CoCSearchItemsChangedSink.CreateRemote(const MachineName: string): ISearchItemsChangedSink;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CSearchItemsChangedSink) as ISearchItemsChangedSink;
end;

class function CoCSearchCrawlScopeManager.Create: ISearchCrawlScopeManager;
begin
  Result := CreateComObject(CLASS_CSearchCrawlScopeManager) as ISearchCrawlScopeManager;
end;

class function CoCSearchCrawlScopeManager.CreateRemote(const MachineName: string): ISearchCrawlScopeManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CSearchCrawlScopeManager) as ISearchCrawlScopeManager;
end;

class function CoCEnumSearchRoots.Create: IEnumSearchRoots;
begin
  Result := CreateComObject(CLASS_CEnumSearchRoots) as IEnumSearchRoots;
end;

class function CoCEnumSearchRoots.CreateRemote(const MachineName: string): IEnumSearchRoots;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CEnumSearchRoots) as IEnumSearchRoots;
end;

class function CoCEnumSearchScopeRules.Create: IEnumSearchScopeRules;
begin
  Result := CreateComObject(CLASS_CEnumSearchScopeRules) as IEnumSearchScopeRules;
end;

class function CoCEnumSearchScopeRules.CreateRemote(const MachineName: string): IEnumSearchScopeRules;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CEnumSearchScopeRules) as IEnumSearchScopeRules;
end;

class function CoCSearchRoot.Create: ISearchRoot;
begin
  Result := CreateComObject(CLASS_CSearchRoot) as ISearchRoot;
end;

class function CoCSearchRoot.CreateRemote(const MachineName: string): ISearchRoot;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CSearchRoot) as ISearchRoot;
end;

class function CoCSearchScopeRule.Create: ISearchScopeRule;
begin
  Result := CreateComObject(CLASS_CSearchScopeRule) as ISearchScopeRule;
end;

class function CoCSearchScopeRule.CreateRemote(const MachineName: string): ISearchScopeRule;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CSearchScopeRule) as ISearchScopeRule;
end;

class function CoCSearchPersistentItemsChangedSink.Create: ISearchPersistentItemsChangedSink;
begin
  Result := CreateComObject(CLASS_CSearchPersistentItemsChangedSink) as ISearchPersistentItemsChangedSink;
end;

class function CoCSearchPersistentItemsChangedSink.CreateRemote(const MachineName: string): ISearchPersistentItemsChangedSink;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CSearchPersistentItemsChangedSink) as ISearchPersistentItemsChangedSink;
end;

end.
