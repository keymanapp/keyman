unit WinLogonLogoService_TLB;

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
// File generated on 30.06.2008 02:28:18 from Type Library described below.

// ************************************************************************  //
// Type Lib: P:\Eigene Dateien\Dezipaitor\Projekte\Delphi\7\jedi-api-lib\jwapi\trunk\Examples\SENS\WinLogonLogo\WinLogonLogoService.tlb (1)
// LIBID: {6C10DA55-47AC-49CA-8793-E0982A7F97D0}
// LCID: 0
// Helpfile: 
// HelpString: SensLogonClient Library
// DepndLst: 
//   (1) v2.0 SensEvents, (P:\Eigene Dateien\Dezipaitor\Projekte\Delphi\7\jedi-api-lib\jwapi\trunk\COM\SensEvts.Tlb)
//   (2) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, SensEvents_TLB, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  WinLogonLogoServiceMajorVersion = 1;
  WinLogonLogoServiceMinorVersion = 0;

  LIBID_WinLogonLogoService: TGUID = '{6C10DA55-47AC-49CA-8793-E0982A7F97D0}';

  CLASS_SENSLogonProxy: TGUID = '{F83127E9-CA8B-452D-AF8C-5647ADCF7DD0}';
type

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  SENSLogonProxy = ISensLogon2;


// *********************************************************************//
// The Class CoSENSLogonProxy provides a Create and CreateRemote method to          
// create instances of the default interface ISensLogon2 exposed by              
// the CoClass SENSLogonProxy. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSENSLogonProxy = class
    class function Create: ISensLogon2;
    class function CreateRemote(const MachineName: string): ISensLogon2;
  end;

implementation

uses ComObj;

class function CoSENSLogonProxy.Create: ISensLogon2;
begin
  Result := CreateComObject(CLASS_SENSLogonProxy) as ISensLogon2;
end;

class function CoSENSLogonProxy.CreateRemote(const MachineName: string): ISensLogon2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SENSLogonProxy) as ISensLogon2;
end;

end.
