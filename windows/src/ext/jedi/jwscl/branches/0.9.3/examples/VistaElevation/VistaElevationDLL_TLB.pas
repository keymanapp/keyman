unit VistaElevationDLL_TLB;

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
// File generated on 31.07.2008 22:26:59 from Type Library described below.

// ************************************************************************  //
// Type Lib: P:\Eigene Dateien\Dezipaitor\Projekte\Delphi\7\jedi-api-lib\jwscl\trunk\examples\VistaElevation\VistaElevationDLL.tlb (1)
// LIBID: {C822DA82-0CA4-436C-B451-04A4AA57E7E3}
// LCID: 0
// Helpfile: 
// HelpString: VistaElevationDLL Bibliothek
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  VistaElevationDLLMajorVersion = 1;
  VistaElevationDLLMinorVersion = 0;

  LIBID_VistaElevationDLL: TGUID = '{C822DA82-0CA4-436C-B451-04A4AA57E7E3}';

  IID_IElevationDemoObject: TGUID = '{0CB0FB98-5AD7-4F1E-97E4-693CAF04AC9B}';
  CLASS_ElevationDemoObject: TGUID = '{E1859C9A-20EA-49E9-AEB0-DDA70CDFB7B7}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IElevationDemoObject = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  ElevationDemoObject = IElevationDemoObject;


// *********************************************************************//
// Interface: IElevationDemoObject
// Flags:     (256) OleAutomation
// GUID:      {0CB0FB98-5AD7-4F1E-97E4-693CAF04AC9B}
// *********************************************************************//
  IElevationDemoObject = interface(IUnknown)
    ['{0CB0FB98-5AD7-4F1E-97E4-693CAF04AC9B}']
    procedure DoSomething(const Str: WideString); stdcall;
    function DoTest: HResult; stdcall;
  end;

// *********************************************************************//
// The Class CoElevationDemoObject provides a Create and CreateRemote method to          
// create instances of the default interface IElevationDemoObject exposed by              
// the CoClass ElevationDemoObject. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoElevationDemoObject = class
    class function Create: IElevationDemoObject;
    class function CreateRemote(const MachineName: string): IElevationDemoObject;
  end;

implementation

uses ComObj;

class function CoElevationDemoObject.Create: IElevationDemoObject;
begin
  Result := CreateComObject(CLASS_ElevationDemoObject) as IElevationDemoObject;
end;

class function CoElevationDemoObject.CreateRemote(const MachineName: string): IElevationDemoObject;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ElevationDemoObject) as IElevationDemoObject;
end;

end.
