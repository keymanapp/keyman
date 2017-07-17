unit JRO_TLB;

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
// Type Lib: C:\Program Files\Common Files\System\ado\msjro.dll (1)
// LIBID: {AC3B8B4C-B6CA-11D1-9F31-00C04FC29D52}
// LCID: 0
// Helpfile: C:\Program Files\Common Files\System\ado\msjro.chm
// HelpString: Microsoft Jet and Replication Objects 2.6 Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
//   (2) v2.8 ADODB, (C:\Program Files\Common Files\System\ado\msado15.dll)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, ADODB_TLB, Classes, Graphics, OleServer, StdVCL, Variants;
  


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  JROMajorVersion = 2;
  JROMinorVersion = 6;

  LIBID_JRO: TGUID = '{AC3B8B4C-B6CA-11D1-9F31-00C04FC29D52}';

  IID_IReplica: TGUID = '{D2D139E0-B6CA-11D1-9F31-00C04FC29D52}';
  IID_Filters: TGUID = '{D2D139E2-B6CA-11D1-9F31-00C04FC29D52}';
  IID_Filter: TGUID = '{D2D139E1-B6CA-11D1-9F31-00C04FC29D52}';
  IID_IJetEngine: TGUID = '{9F63D980-FF25-11D1-BB6F-00C04FAE22DA}';
  CLASS_Replica: TGUID = '{D2D139E3-B6CA-11D1-9F31-00C04FC29D52}';
  CLASS_JetEngine: TGUID = '{DE88C160-FF2C-11D1-BB6F-00C04FAE22DA}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum ReplicaTypeEnum
type
  ReplicaTypeEnum = TOleEnum;
const
  jrRepTypeNotReplicable = $00000000;
  jrRepTypeDesignMaster = $00000001;
  jrRepTypeFull = $00000002;
  jrRepTypePartial = $00000003;

// Constants for enum VisibilityEnum
type
  VisibilityEnum = TOleEnum;
const
  jrRepVisibilityGlobal = $00000001;
  jrRepVisibilityLocal = $00000002;
  jrRepVisibilityAnon = $00000004;

// Constants for enum UpdatabilityEnum
type
  UpdatabilityEnum = TOleEnum;
const
  jrRepUpdFull = $00000000;
  jrRepUpdReadOnly = $00000002;

// Constants for enum SyncTypeEnum
type
  SyncTypeEnum = TOleEnum;
const
  jrSyncTypeExport = $00000001;
  jrSyncTypeImport = $00000002;
  jrSyncTypeImpExp = $00000003;

// Constants for enum SyncModeEnum
type
  SyncModeEnum = TOleEnum;
const
  jrSyncModeIndirect = $00000001;
  jrSyncModeDirect = $00000002;
  jrSyncModeInternet = $00000003;

// Constants for enum FilterTypeEnum
type
  FilterTypeEnum = TOleEnum;
const
  jrFilterTypeTable = $00000001;
  jrFilterTypeRelationship = $00000002;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IReplica = interface;
  IReplicaDisp = dispinterface;
  Filters = interface;
  FiltersDisp = dispinterface;
  Filter = interface;
  FilterDisp = dispinterface;
  IJetEngine = interface;
  IJetEngineDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  Replica = IReplica;
  JetEngine = IJetEngine;


// *********************************************************************//
// Interface: IReplica
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D2D139E0-B6CA-11D1-9F31-00C04FC29D52}
// *********************************************************************//
  IReplica = interface(IDispatch)
    ['{D2D139E0-B6CA-11D1-9F31-00C04FC29D52}']
    procedure _Set_ActiveConnection(const ppconn: IDispatch); safecall;
    procedure Set_ActiveConnection(ppconn: OleVariant); safecall;
    function Get_ActiveConnection: IDispatch; safecall;
    function Get_ConflictFunction: WideString; safecall;
    procedure Set_ConflictFunction(const pbstr: WideString); safecall;
    function Get_ConflictTables: _Recordset; safecall;
    function Get_DesignMasterId: OleVariant; safecall;
    procedure Set_DesignMasterId(pvar: OleVariant); safecall;
    function Get_Priority: Integer; safecall;
    function Get_ReplicaId: OleVariant; safecall;
    function Get_ReplicaType: ReplicaTypeEnum; safecall;
    function Get_RetentionPeriod: Integer; safecall;
    procedure Set_RetentionPeriod(pl: Integer); safecall;
    function Get_Visibility: VisibilityEnum; safecall;
    procedure CreateReplica(const replicaName: WideString; const description: WideString; 
                            ReplicaType: ReplicaTypeEnum; Visibility: VisibilityEnum; 
                            Priority: Integer; updatability: UpdatabilityEnum); safecall;
    function GetObjectReplicability(const objectName: WideString; const objectType: WideString): WordBool; safecall;
    procedure SetObjectReplicability(const objectName: WideString; const objectType: WideString; 
                                     replicability: WordBool); safecall;
    procedure MakeReplicable(const connectionString: WideString; columnTracking: WordBool); safecall;
    procedure PopulatePartial(const FullReplica: WideString); safecall;
    procedure Synchronize(const target: WideString; syncType: SyncTypeEnum; syncMode: SyncModeEnum); safecall;
    function Get_Filters: Filters; safecall;
    property ConflictFunction: WideString read Get_ConflictFunction write Set_ConflictFunction;
    property ConflictTables: _Recordset read Get_ConflictTables;
    property DesignMasterId: OleVariant read Get_DesignMasterId write Set_DesignMasterId;
    property Priority: Integer read Get_Priority;
    property ReplicaId: OleVariant read Get_ReplicaId;
    property ReplicaType: ReplicaTypeEnum read Get_ReplicaType;
    property RetentionPeriod: Integer read Get_RetentionPeriod write Set_RetentionPeriod;
    property Visibility: VisibilityEnum read Get_Visibility;
    property Filters: Filters read Get_Filters;
  end;

// *********************************************************************//
// DispIntf:  IReplicaDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D2D139E0-B6CA-11D1-9F31-00C04FC29D52}
// *********************************************************************//
  IReplicaDisp = dispinterface
    ['{D2D139E0-B6CA-11D1-9F31-00C04FC29D52}']
    function ActiveConnection: IDispatch; dispid 1610743808;
    property ConflictFunction: WideString dispid 1610743811;
    property ConflictTables: _Recordset readonly dispid 1610743813;
    property DesignMasterId: OleVariant dispid 1610743814;
    property Priority: Integer readonly dispid 1610743816;
    property ReplicaId: OleVariant readonly dispid 1610743817;
    property ReplicaType: ReplicaTypeEnum readonly dispid 1610743818;
    property RetentionPeriod: Integer dispid 1610743819;
    property Visibility: VisibilityEnum readonly dispid 1610743821;
    procedure CreateReplica(const replicaName: WideString; const description: WideString; 
                            ReplicaType: ReplicaTypeEnum; Visibility: VisibilityEnum; 
                            Priority: Integer; updatability: UpdatabilityEnum); dispid 1610743822;
    function GetObjectReplicability(const objectName: WideString; const objectType: WideString): WordBool; dispid 1610743823;
    procedure SetObjectReplicability(const objectName: WideString; const objectType: WideString; 
                                     replicability: WordBool); dispid 1610743824;
    procedure MakeReplicable(const connectionString: WideString; columnTracking: WordBool); dispid 1610743825;
    procedure PopulatePartial(const FullReplica: WideString); dispid 1610743826;
    procedure Synchronize(const target: WideString; syncType: SyncTypeEnum; syncMode: SyncModeEnum); dispid 1610743827;
    property Filters: Filters readonly dispid 1610743828;
  end;

// *********************************************************************//
// Interface: Filters
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D2D139E2-B6CA-11D1-9F31-00C04FC29D52}
// *********************************************************************//
  Filters = interface(IDispatch)
    ['{D2D139E2-B6CA-11D1-9F31-00C04FC29D52}']
    procedure Refresh; safecall;
    function _NewEnum: IUnknown; safecall;
    function Get_Count: Integer; safecall;
    function Get_Item(Index: OleVariant): Filter; safecall;
    procedure Append(const TableName: WideString; FilterType: FilterTypeEnum; 
                     const FilterCriteria: WideString); safecall;
    procedure Delete(Index: OleVariant); safecall;
    property Count: Integer read Get_Count;
    property Item[Index: OleVariant]: Filter read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  FiltersDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D2D139E2-B6CA-11D1-9F31-00C04FC29D52}
// *********************************************************************//
  FiltersDisp = dispinterface
    ['{D2D139E2-B6CA-11D1-9F31-00C04FC29D52}']
    procedure Refresh; dispid 1610743808;
    function _NewEnum: IUnknown; dispid -4;
    property Count: Integer readonly dispid 1610743810;
    property Item[Index: OleVariant]: Filter readonly dispid 0; default;
    procedure Append(const TableName: WideString; FilterType: FilterTypeEnum; 
                     const FilterCriteria: WideString); dispid 1610743812;
    procedure Delete(Index: OleVariant); dispid 1610743813;
  end;

// *********************************************************************//
// Interface: Filter
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D2D139E1-B6CA-11D1-9F31-00C04FC29D52}
// *********************************************************************//
  Filter = interface(IDispatch)
    ['{D2D139E1-B6CA-11D1-9F31-00C04FC29D52}']
    function Get_TableName: WideString; safecall;
    function Get_FilterType: FilterTypeEnum; safecall;
    function Get_FilterCriteria: WideString; safecall;
    property TableName: WideString read Get_TableName;
    property FilterType: FilterTypeEnum read Get_FilterType;
    property FilterCriteria: WideString read Get_FilterCriteria;
  end;

// *********************************************************************//
// DispIntf:  FilterDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D2D139E1-B6CA-11D1-9F31-00C04FC29D52}
// *********************************************************************//
  FilterDisp = dispinterface
    ['{D2D139E1-B6CA-11D1-9F31-00C04FC29D52}']
    property TableName: WideString readonly dispid 1610743808;
    property FilterType: FilterTypeEnum readonly dispid 1610743809;
    property FilterCriteria: WideString readonly dispid 1610743810;
  end;

// *********************************************************************//
// Interface: IJetEngine
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9F63D980-FF25-11D1-BB6F-00C04FAE22DA}
// *********************************************************************//
  IJetEngine = interface(IDispatch)
    ['{9F63D980-FF25-11D1-BB6F-00C04FAE22DA}']
    procedure CompactDatabase(const SourceConnection: WideString; const Destconnection: WideString); safecall;
    procedure RefreshCache(const Connection: _Connection); safecall;
  end;

// *********************************************************************//
// DispIntf:  IJetEngineDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9F63D980-FF25-11D1-BB6F-00C04FAE22DA}
// *********************************************************************//
  IJetEngineDisp = dispinterface
    ['{9F63D980-FF25-11D1-BB6F-00C04FAE22DA}']
    procedure CompactDatabase(const SourceConnection: WideString; const Destconnection: WideString); dispid 1610743808;
    procedure RefreshCache(const Connection: _Connection); dispid 1610743809;
  end;

// *********************************************************************//
// The Class CoReplica provides a Create and CreateRemote method to          
// create instances of the default interface IReplica exposed by              
// the CoClass Replica. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoReplica = class
    class function Create: IReplica;
    class function CreateRemote(const MachineName: string): IReplica;
  end;

// *********************************************************************//
// The Class CoJetEngine provides a Create and CreateRemote method to          
// create instances of the default interface IJetEngine exposed by              
// the CoClass JetEngine. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoJetEngine = class
    class function Create: IJetEngine;
    class function CreateRemote(const MachineName: string): IJetEngine;
  end;

implementation

uses ComObj;

class function CoReplica.Create: IReplica;
begin
  Result := CreateComObject(CLASS_Replica) as IReplica;
end;

class function CoReplica.CreateRemote(const MachineName: string): IReplica;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Replica) as IReplica;
end;

class function CoJetEngine.Create: IJetEngine;
begin
  Result := CreateComObject(CLASS_JetEngine) as IJetEngine;
end;

class function CoJetEngine.CreateRemote(const MachineName: string): IJetEngine;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JetEngine) as IJetEngine;
end;

end.
