unit RunElCOM_TLB;

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
// Datei generiert am 12.03.2008 19:51:41 aus der unten beschriebenen Typbibliothek.

// ************************************************************************  //
// Typbib: P:\Eigene Dateien\Dezipaitor\Projekte\Delphi\7\jedi-api-lib\jwscl\trunk\examples\runel\RunElCOM.tlb (1)
// LIBID: {623BD86A-B280-4727-8C48-F030AB87097C}
// LCID: 0
// Hilfedatei: 
// Hilfe-String: RunElCOM Bibliothek
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
  RunElCOMMajorVersion = 1;
  RunElCOMMinorVersion = 0;

  LIBID_RunElCOM: TGUID = '{623BD86A-B280-4727-8C48-F030AB87097C}';

  IID_IJwRunElevated: TGUID = '{F8B12C3A-2076-4262-B512-008D55DEC603}';
  CLASS_JwRunElevated: TGUID = '{A71AD7FF-2015-4085-AC36-1B9EF17BE21D}';
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
// GUID:      {F8B12C3A-2076-4262-B512-008D55DEC603}
// *********************************************************************//
  IJwRunElevated = interface(IUnknown)
    ['{F8B12C3A-2076-4262-B512-008D55DEC603}']
    function RunAppElevated(AppName: PWideChar; Parameter: PWideChar; Dir: PWideChar; 
                            ClientProcessID: LongWord; out NewThreadHandle: LongWord; 
                            out NewProcessHandle: LongWord; out ResultValue: LongWord): HResult; stdcall;
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
