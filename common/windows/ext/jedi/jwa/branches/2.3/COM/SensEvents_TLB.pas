unit SensEvents_TLB;

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

// ************************************************************************  //
// Typbib: SensEvts.Tlb (1)
// LIBID: {D597DEED-5B9F-11D1-8DD2-00AA004ABD5E}
// LCID: 0
// Helpfile:
// HelpString: SENS Events Type Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
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
{$TYPEDADDRESS OFF} 
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
  SensEventsMajorVersion = 2;
  SensEventsMinorVersion = 0;

  LIBID_SensEvents: TGUID = '{D597DEED-5B9F-11D1-8DD2-00AA004ABD5E}';

  IID_ISensNetwork: TGUID = '{D597BAB1-5B9F-11D1-8DD2-00AA004ABD5E}';
  IID_ISensOnNow: TGUID = '{D597BAB2-5B9F-11D1-8DD2-00AA004ABD5E}';
  IID_ISensLogon: TGUID = '{D597BAB3-5B9F-11D1-8DD2-00AA004ABD5E}';
  IID_ISensLogon2: TGUID = '{D597BAB4-5B9F-11D1-8DD2-00AA004ABD5E}';
  CLASS_SENS: TGUID = '{D597CAFE-5B9F-11D1-8DD2-00AA004ABD5E}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ISensNetwork = interface;
  ISensNetworkDisp = dispinterface;
  ISensOnNow = interface;
  ISensOnNowDisp = dispinterface;
  ISensLogon = interface;
  ISensLogonDisp = dispinterface;
  ISensLogon2 = interface;
  ISensLogon2Disp = dispinterface;

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten CoClasses             
// (HINWEIS: Hier wird jede CoClass zu ihrer Standardschnittstelle        
// zugewiesen)                                                            
// *********************************************************************//


// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  PUserType1 = ^SENS_QOCINFO; {*}

  SENS_QOCINFO = packed record
    dwSize: LongWord;
    dwFlags: LongWord;
    dwOutSpeed: LongWord;
    dwInSpeed: LongWord;
  end;


// *********************************************************************//
// Interface: ISensNetwork
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D597BAB1-5B9F-11D1-8DD2-00AA004ABD5E}
// *********************************************************************//
  ISensNetwork = interface(IDispatch)
    ['{D597BAB1-5B9F-11D1-8DD2-00AA004ABD5E}']
    procedure ConnectionMade(const bstrConnection: WideString; ulType: LongWord; 
                             var lpQOCInfo: SENS_QOCINFO); safecall;
    procedure ConnectionMadeNoQOCInfo(const bstrConnection: WideString; ulType: LongWord); safecall;
    procedure ConnectionLost(const bstrConnection: WideString; ulType: LongWord); safecall;
    procedure DestinationReachable(const bstrDestination: WideString; 
                                   const bstrConnection: WideString; ulType: LongWord; 
                                   var lpQOCInfo: SENS_QOCINFO); safecall;
    procedure DestinationReachableNoQOCInfo(const bstrDestination: WideString; 
                                            const bstrConnection: WideString; ulType: LongWord); safecall;
  end;

// *********************************************************************//
// DispIntf:  ISensNetworkDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D597BAB1-5B9F-11D1-8DD2-00AA004ABD5E}
// *********************************************************************//
  ISensNetworkDisp = dispinterface
    ['{D597BAB1-5B9F-11D1-8DD2-00AA004ABD5E}']
    procedure ConnectionMade(const bstrConnection: WideString; ulType: LongWord; 
                             var lpQOCInfo: {??SENS_QOCINFO}OleVariant); dispid 1;
    procedure ConnectionMadeNoQOCInfo(const bstrConnection: WideString; ulType: LongWord); dispid 2;
    procedure ConnectionLost(const bstrConnection: WideString; ulType: LongWord); dispid 3;
    procedure DestinationReachable(const bstrDestination: WideString; 
                                   const bstrConnection: WideString; ulType: LongWord; 
                                   var lpQOCInfo: {??SENS_QOCINFO}OleVariant); dispid 4;
    procedure DestinationReachableNoQOCInfo(const bstrDestination: WideString; 
                                            const bstrConnection: WideString; ulType: LongWord); dispid 5;
  end;

// *********************************************************************//
// Interface: ISensOnNow
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D597BAB2-5B9F-11D1-8DD2-00AA004ABD5E}
// *********************************************************************//
  ISensOnNow = interface(IDispatch)
    ['{D597BAB2-5B9F-11D1-8DD2-00AA004ABD5E}']
    procedure OnACPower; safecall;
    procedure OnBatteryPower(dwBatteryLifePercent: LongWord); safecall;
    procedure BatteryLow(dwBatteryLifePercent: LongWord); safecall;
  end;

// *********************************************************************//
// DispIntf:  ISensOnNowDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D597BAB2-5B9F-11D1-8DD2-00AA004ABD5E}
// *********************************************************************//
  ISensOnNowDisp = dispinterface
    ['{D597BAB2-5B9F-11D1-8DD2-00AA004ABD5E}']
    procedure OnACPower; dispid 1;
    procedure OnBatteryPower(dwBatteryLifePercent: LongWord); dispid 2;
    procedure BatteryLow(dwBatteryLifePercent: LongWord); dispid 3;
  end;

// *********************************************************************//
// Interface: ISensLogon
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D597BAB3-5B9F-11D1-8DD2-00AA004ABD5E}
// *********************************************************************//
  ISensLogon = interface(IDispatch)
    ['{D597BAB3-5B9F-11D1-8DD2-00AA004ABD5E}']
    procedure Logon(const bstrUserName: WideString); safecall;
    procedure Logoff(const bstrUserName: WideString); safecall;
    procedure StartShell(const bstrUserName: WideString); safecall;
    procedure DisplayLock(const bstrUserName: WideString); safecall;
    procedure DisplayUnlock(const bstrUserName: WideString); safecall;
    procedure StartScreenSaver(const bstrUserName: WideString); safecall;
    procedure StopScreenSaver(const bstrUserName: WideString); safecall;
  end;

// *********************************************************************//
// DispIntf:  ISensLogonDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D597BAB3-5B9F-11D1-8DD2-00AA004ABD5E}
// *********************************************************************//
  ISensLogonDisp = dispinterface
    ['{D597BAB3-5B9F-11D1-8DD2-00AA004ABD5E}']
    procedure Logon(const bstrUserName: WideString); dispid 1;
    procedure Logoff(const bstrUserName: WideString); dispid 2;
    procedure StartShell(const bstrUserName: WideString); dispid 3;
    procedure DisplayLock(const bstrUserName: WideString); dispid 4;
    procedure DisplayUnlock(const bstrUserName: WideString); dispid 5;
    procedure StartScreenSaver(const bstrUserName: WideString); dispid 6;
    procedure StopScreenSaver(const bstrUserName: WideString); dispid 7;
  end;

// *********************************************************************//
// Interface: ISensLogon2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D597BAB4-5B9F-11D1-8DD2-00AA004ABD5E}
// *********************************************************************//
  ISensLogon2 = interface(IDispatch)
    ['{D597BAB4-5B9F-11D1-8DD2-00AA004ABD5E}']
    procedure Logon(const bstrUserName: WideString; dwSessionId: LongWord); safecall;
    procedure Logoff(const bstrUserName: WideString; dwSessionId: LongWord); safecall;
    procedure SessionDisconnect(const bstrUserName: WideString; dwSessionId: LongWord); safecall;
    procedure SessionReconnect(const bstrUserName: WideString; dwSessionId: LongWord); safecall;
    procedure PostShell(const bstrUserName: WideString; dwSessionId: LongWord); safecall;
  end;

// *********************************************************************//
// DispIntf:  ISensLogon2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D597BAB4-5B9F-11D1-8DD2-00AA004ABD5E}
// *********************************************************************//
  ISensLogon2Disp = dispinterface
    ['{D597BAB4-5B9F-11D1-8DD2-00AA004ABD5E}']
    procedure Logon(const bstrUserName: WideString; dwSessionId: LongWord); dispid 1;
    procedure Logoff(const bstrUserName: WideString; dwSessionId: LongWord); dispid 2;
    procedure SessionDisconnect(const bstrUserName: WideString; dwSessionId: LongWord); dispid 3;
    procedure SessionReconnect(const bstrUserName: WideString; dwSessionId: LongWord); dispid 4;
    procedure PostShell(const bstrUserName: WideString; dwSessionId: LongWord); dispid 5;
  end;

implementation

uses ComObj;

end.
