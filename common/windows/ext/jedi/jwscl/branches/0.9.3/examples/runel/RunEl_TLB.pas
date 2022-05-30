unit RunEl_TLB;

// ************************************************************************ //
// WARNUNG                                                                    
// -------                                                                    
// Die in dieser Datei deklarierten Typen wurden aus Daten einer Typbibliothek
// generiert. Wenn diese Typbibliothek explizit oder indirekt (über eine     
// andere Typbibliothek) reimportiert wird oder wenn die Anweisung            
// 'Aktualisieren' im Typbibliotheks-Editor während des Bearbeitens der     
// Typbibliothek aktiviert ist, wird der Inhalt dieser Datei neu generiert und 
// alle manuell vorgenommenen Änderungen gehen verloren.                           
// ************************************************************************ //

// PASTLWTR : 1.2
// Datei generiert am 11.03.2008 23:55:24 aus der unten beschriebenen Typbibliothek.

// ************************************************************************  //
// Typbib: P:\Eigene Dateien\Dezipaitor\Projekte\Delphi\7\jedi-api-lib\jwscl\trunk\examples\runel\RunEl.tlb (1)
// LIBID: {BA51F2D7-F01B-4D09-99B7-2129BE8F1430}
// LCID: 0
// Hilfedatei: 
// Hilfe-String: RunEl Bibliothek
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit muß ohne Typüberprüfung für Zeiger compiliert werden. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// In dieser Typbibliothek deklarierte GUIDS . Es werden folgende         
// Präfixe verwendet:                                                     
//   Typbibliotheken     : LIBID_xxxx                                     
//   CoClasses           : CLASS_xxxx                                     
//   DISPInterfaces      : DIID_xxxx                                      
//   Nicht-DISP-Schnittstellen: IID_xxxx                                       
// *********************************************************************//
const
  // Haupt- und Nebenversionen der Typbibliothek
  RunElMajorVersion = 1;
  RunElMinorVersion = 0;

  LIBID_RunEl: TGUID = '{BA51F2D7-F01B-4D09-99B7-2129BE8F1430}';

  IID_IJwRunElevated: TGUID = '{BD6978D1-4CC4-4893-A0BE-4A1485DE24C8}';
  CLASS_JwRunElevated: TGUID = '{5F13F2F3-B4C9-43F9-A1CE-584ABCC75CB9}';
type

// *********************************************************************//
// Forward-Deklaration von in der Typbibliothek definierten Typen         
// *********************************************************************//
  IJwRunElevated = interface;

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten CoClasses             
// (HINWEIS: Hier wird jede CoClass zu ihrer Standardschnittstelle        
// zugewiesen)                                                            
// *********************************************************************//
  JwRunElevated = IJwRunElevated;


// *********************************************************************//
// Schnittstelle: IJwRunElevated
// Flags:     (256) OleAutomation
// GUID:      {BD6978D1-4CC4-4893-A0BE-4A1485DE24C8}
// *********************************************************************//
  IJwRunElevated = interface(IUnknown)
    ['{BD6978D1-4CC4-4893-A0BE-4A1485DE24C8}']
    function RunAppElevated(AppName: PWideChar; Parameter: PWideChar; Dir: PWideChar; 
                            TargetHandle: LongWord; out DestHandle: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Die Klasse CoJwRunElevated stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwRunElevated, dargestellt von
// CoClass JwRunElevated, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwRunElevated = class
    class function Create: IJwRunElevated;
    class function CreateRemote(const MachineName: string): IJwRunElevated;
  end;

implementation

uses ComObj;

class function CoJwRunElevated.Create: IJwRunElevated;
begin
  Result := CreateComObject(CLASS_JwRunElevated) as IJwRunElevated;
end;

class function CoJwRunElevated.CreateRemote(const MachineName: string): IJwRunElevated;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwRunElevated) as IJwRunElevated;
end;

end.
