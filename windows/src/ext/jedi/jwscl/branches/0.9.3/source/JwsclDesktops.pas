{
Description
Project JEDI Windows Security Code Library (JWSCL)

Contains desktop functions

Author
Christian Wimmer

License
The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the  
GNU Lesser General Public License (the  "LGPL License"), in which case the   
provisions of the LGPL License are applicable instead of those above.        
If you wish to allow use of your version of this file only under the terms   
of the LGPL License and not to allow others to use your version of this file 
under the MPL, indicate your decision by deleting  the provisions above and  
replace  them with the notice and other provisions required by the LGPL      
License.  If you do not delete the provisions above, a recipient may use     
your version of this file under either the MPL or the LGPL License.          
                                                                             
For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html 

The Original Code is JwsclDesktop.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.



This unit is about to be changed.


Link List
Secure object types:
  http://msdn2.microsoft.com/en-us/library/aa379593.aspx

}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclDesktops;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $

interface

uses SysUtils, Classes, Contnrs,
  Registry,
  jwaWindows,
  JwsclResource,
  JwsclTypes, JwsclExceptions, JwsclSid, JwsclAcl, JwsclToken,
  JwsclMapping, JwsclKnownSid, JwsclSecureObjects,
  JwsclVersion, JwsclConstants, JwsclDescriptor,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type




  {Forward declaration of TDesktops}
  TJwSecurityDesktops = class;

  {Forward declaration of TJwSecurityDesktop}
  TJwSecurityDesktop = class;

   {<B>TJwOnDestroyDesktop</B> is called if a desktop is about to be destroyed. 
    You cannot stop this process!
    @param Sender Contains the desktop instance that is beeing destroyed. }
  TJwOnDestroyDesktop = procedure(Sender: TJwSecurityDesktop) of object;

   {<B>TJwSecurityDesktop</B> is the main class that provides methods to create, open
    and manipulate desktops.
  Be aware of following things: 
   1. nearly every function can create an exception.
    If you don't check for them it can happen that a switch back to default desktop does not work.
    This especially happens if Delphi is active and shows the exception source on the default desktop.
   2. This instance does not dynamically react on changes done by direct WinAPI calls.
   }
  TJwSecurityDesktop = class
  private
    { private declarations }
    fParent:        TJwSecurityDesktops;
    { private declarations }
    fDesiredAccess: ACCESS_MASK;

    fHandle: HDESK;
    fOnDestroyDesktop: TJwOnDestroyDesktop;
    fIsInputDesktop: boolean;
    fName: TJwString;

    fCloseOnDestroy: boolean;
    fOpened:         boolean;

    fLastThreadDesktop,
    fLastSwitchDesktop: HDESK;
    fUserSID:           TJwSecurityId;

  protected
    { protected declarations }

  {<B>OpenDesktop</B> opens an existing Desktop.
    It calls the WinAPI function OpenDesktop. See more information in MSDN.
    @param aName Contains the name of an existing Desktop. A desktop name is case sensitive. 
    @param DesktopFlags Contains flags for desktop opening. See TJwSecurityDesktopFlags  for more information  
    @param doInherit  
    @param aDesirecAccess Contains flags that defines the type of opening. You can use the following flags
       concatenated by OR-Operator :
         READ_CONTROL, WRITE_DAC, WRITE_OWNER, STANDARD_RIGHTS_READ, STANDARD_RIGHTS_WRITE, STANDARD_RIGHTS_EXECUTE,
       STANDARD_RIGHTS_ALL, SPECIFIC_RIGHTS_ALL, ACCESS_SYSTEM_SECURITY, MAXIMUM_ALLOWED, GENERIC_READ,
       GENERIC_WRITE, GENERIC_EXECUTE, GENERIC_ALL,
       // Desktop-specific access flags 
       DESKTOP_READOBJECTS, DESKTOP_CREATEWINDOW, DESKTOP_CREATEMENU, DESKTOP_HOOKCONTROL, DESKTOP_JOURNALRECORD
       DESKTOP_JOURNALPLAYBACK, DESKTOP_ENUMERATE, DESKTOP_WRITEOBJECTS
     You can look up the meanings in MSDN.
     @return OpenDesktop returns a handle to the desktop 
     raises
 OpenDesktop:  raises EJwsclOpenDesktopException with an error description if the desktop could not be opened
             OpenDesktop raises EJwsclDesktopException if the desktop is already opened by this instance.

  }
    function OpenDesktop(const aName: TJwString;
      const DesktopFlags: TJwSecurityDesktopFlags;
      const doInherit: boolean;
      const aDesiredAccess: ACCESS_MASK): HDESK;
      overload; virtual;

  {<B>CreateDesktop</B> creates a new Desktop.
    It calls the WinAPI function CreateDesktop. See more information in MSDN.
    @param aName Contains the name of an existing Desktop. A desktop name is case sensitive. 
    @param DesktopFlags Contains flags for desktop opening. See TJwSecurityDesktopFlags  for more information 
    @param doInherit  
    @param aDesirecAccess Contains flags that defines the type of opening. You can use the following flags
       concatenated by OR-Operator :
         READ_CONTROL, WRITE_DAC, WRITE_OWNER, STANDARD_RIGHTS_READ, STANDARD_RIGHTS_WRITE, STANDARD_RIGHTS_EXECUTE,
       STANDARD_RIGHTS_ALL, SPECIFIC_RIGHTS_ALL, ACCESS_SYSTEM_SECURITY, MAXIMUM_ALLOWED, GENERIC_READ,
       GENERIC_WRITE, GENERIC_EXECUTE, GENERIC_ALL,
       // Desktop-specific access flags 
       DESKTOP_READOBJECTS, DESKTOP_CREATEWINDOW, DESKTOP_CREATEMENU, DESKTOP_HOOKCONTROL, DESKTOP_JOURNALRECORD
       DESKTOP_JOURNALPLAYBACK, DESKTOP_ENUMERATE, DESKTOP_WRITEOBJECTS
     You can look up the meanings in MSDN.
     You must at least use the flag DESKTOP_CREATEWINDOW. 
     @param aSecurityDescriptor Contains security aspects to be assigned to the new desktop 
     @return CreateDesktop returns a handle to the desktop 
     raises
 CreateDesktop:  raises EJwsclCreateDesktopException with an error description if the desktop could not be created. 

  }
    function CreateDesktop(const aName: TJwString;
      const DesktopFlags: TJwSecurityDesktopFlags;
      const aDesiredAccess: ACCESS_MASK;
      const aSecurityDescriptor:
      TJwSecurityDescriptor): HDESK; overload; virtual;

   {<B>RetrieveSD</B> gets security attributes of the already opened desktop
    If the desktop is not valid (e.g. not opened) the structure contains only zero information.
   }
    function RetrieveSD: TSecurityAttributes; virtual;

   {<B>SetInherit</B> sets the inherit flag of the already opened desktop.
    If the desktop is not valid (e.g. not opened) the flag is not changed.
    If an error occurs while changing flag the exception EJwsclDesktopException is raised.
    @param Value Contains the inherit flag value }
    procedure SetInherit(const Value: boolean);

   {<B>SetDesktopFlags</B> sets the Desktops flags of the already opened desktop.
    If the desktop is not valid (e.g. not opened) the flag is not changed.
    If an error occurs while changing flag the exception EJwsclDesktopException is raised.
   }
    procedure SetDesktopFlags(const DesktopFlags: TJwSecurityDesktopFlags);

     {<B>GetDesktopFlags</B> returns the current flags of the opened desktop.
    @return GeTSecurityDesktopFlags returns the flag of the opened desktop or a empty set if desktop is not opened/valid }
    function GetDesktopFlags: TJwSecurityDesktopFlags; virtual;

     {GetName returns the name of the desktop.
    @return GetName returns the name of the opened desktop or if the desktop is not opened
            GetName returns the name that was supplied on creation of instance. }
    function GetName: TJwString;

   {<B>GetOpened</B> gets the open status of the desktop.
    @return GetOpened returns if the desktop handle is valid (true) or not (false).    }
    function GetOpened: boolean;

    function GetUserSID: TJwSecurityId; virtual;
    function GetObjectType: TJwString; virtual;

  protected
    fSD: TJwSecurityDescriptor;

    function GetSD(Info: TJwSecurityInformationFlagSet): TJwSecurityDescriptor;
    procedure PutSD(Info: TJwSecurityInformationFlagSet;
      aSD: TJwSecurityDescriptor);
  public
    { public declarations }
   {Constructor <B>Create</B> creates a new instance of TJwSecurityDesktop.
    This constructor can create and open a desktop object.

    @param aParent Contains the parent instance that administer the desktop.
             You can set it to nil if you want to administer the desktop yourself. In that case you
             must free it by yourself.
       In the other case you must not call Free because TJwSecurityDesktops(aParent) calls it if it freed. 

    @param aFlag Contains the type of work you want to do : Create or Open a desktop. See TJwSecurityDesktopFlags  for more information. 
    @param aCloseOnDestroy Contains a flag that determines if the desktop handle shall be closed if the desktop instance is freed.
            That case is used if aCloseOnDestroy is TRUE otherwise the handle will exists until you close it by yourself or
         the application is closed. 
    @param aName Contains the name of an existing Desktop. A desktop name is case sensitive. 
    @param DesktopFlags Contains flags for desktop opening. See TJwSecurityDesktopFlags  for more information 
    @param doInherit TBD 
    @param aDesirecAccess Contains flags that defines the type of opening. You can use the following flags
       concatenated by OR-Operator :
         READ_CONTROL, WRITE_DAC, WRITE_OWNER, STANDARD_RIGHTS_READ, STANDARD_RIGHTS_WRITE, STANDARD_RIGHTS_EXECUTE,
       STANDARD_RIGHTS_ALL, SPECIFIC_RIGHTS_ALL, ACCESS_SYSTEM_SECURITY, MAXIMUM_ALLOWED, GENERIC_READ,
       GENERIC_WRITE, GENERIC_EXECUTE, GENERIC_ALL,
       // Desktop-specific access flags 
       DESKTOP_READOBJECTS, DESKTOP_CREATEWINDOW, DESKTOP_CREATEMENU, DESKTOP_HOOKCONTROL, DESKTOP_JOURNALRECORD
       DESKTOP_JOURNALPLAYBACK, DESKTOP_ENUMERATE, DESKTOP_WRITEOBJECTS
     You can look up the meanings in MSDN. 
    @param aSecurityDescriptor Contains security aspects to be assigned to the new desktop. Set to nil if the security attributes
           of the application shall be used.   
    raises
 EJwsclCreateDesktopException:  is raised if the desktop could not be created. 
     EJwsclOpenDesktopException: is raised if the desktop could not be opened. 
     EJwsclDesktopException: is raised if the desktop is already opened by this instance. 

   }
    constructor Create(const aParent: TJwSecurityDesktops;
      const aFlag: TJwDesktopCreationFlag;
      const aCloseOnDestroy: boolean;
      const aName: TJwString;
      const DesktopFlags: TJwSecurityDesktopFlags;
      const doInherit: boolean;
      const aDesiredAccess: ACCESS_MASK;
      const aSecurityDescriptor: TJwSecurityDescriptor);
      overload;

     {<B>CreateDesktop</B> creates a new desktop using the current window station.
      See Create  for more information.
     }
    constructor CreateDesktop(const aParent: TJwSecurityDesktops;
      const aCloseOnDestroy: boolean;
      const aName: TJwString;
      const DesktopFlags: TJwSecurityDesktopFlags;
      const doInherit: boolean;
      const aDesiredAccess: ACCESS_MASK;
      const aSecurityDescriptor: TJwSecurityDescriptor);
      overload;
     {<B>OpenDesktop</B> opens a desktop using the current window station.
      See Create  for more information.
     }
    constructor OpenDesktop(const aParent: TJwSecurityDesktops;
      const aCloseOnDestroy: boolean;
      const aName: TJwString;
      const DesktopFlags: TJwSecurityDesktopFlags;
      const doInherit: boolean;
      const aDesiredAccess: ACCESS_MASK); overload;

    {<B>CreateByHandle</B> creates an instance using an existing handle.
     The handle can be automatically destroyed on freeing.}
    constructor CreateByHandle(const Handle : HDESK; const CloseOnDestroy: boolean);

    {<B>CreateAndGetInputDesktop</B> creates an instance using the current input desktop.
    The input desktop is the one which can receive user inputs.
    }
    constructor CreateAndGetInputDesktop(const DesktopFlags: TJwSecurityDesktopFlags;
          const InheritHandles: boolean;
          const DesiredAccess: ACCESS_MASK);

     {CreateUnInitialized creates a desktop instance without creating or opening a desktop.
    This function is used for internal purposes and should not be used.
   }
    constructor CreateUnInitialized(const aParent: TJwSecurityDesktops;
      const aName: TJwString);

     {Destroy frees the desktop instance and closes the desktop handle if any and if the
      property OnDestroyDesktop is true.
      If OnDestroyDesktop is false the desktop instance is freed but the desktop handle is retained. }
    destructor Destroy; override;


   {OpenDesktopByName opens a desktop with the name given in the property Name.
    This function does not check for an already opened desktop in this instance.
     It simply overrides the current handle of this desktop instance.
    For more information about parameters see Create .
    
    Do not use this function, it is used for internal purposes.}
    function OpenDesktopByName(const DesktopFlags: TJwSecurityDesktopFlags;
      const doInherit: boolean;
      const aDesiredAccess: ACCESS_MASK): HDESK;
      overload; virtual;


     {<B>DesktopFlagsToInt</B> converts the enum type TJwSecurityDesktopFlags to the corresponding
    integer type. 
    @param DesktopFlags See TJwSecurityDesktopFlags  for more Information. 
    @return Returns Windows.dfAllowOtherAccountHook (as integer) if [dfAllowOtherAccountHook] was supplied
      otherise it returns zero. }
    function DesktopFlagsToInt(DesktopFlags: TJwSecurityDesktopFlags)
      : Cardinal;

     {GeTSecurityDesktopWindowHandles creates a list of available desktop window handles.
    All TList pointers are desktop handles. You can convert them to HDESK by type cast :
        HDESK(List.Item[i])
     @return Returns a list of desktop handles. The return value can never be nil and therfore
         must be freed by the caller.  }
    function GetSecurityDesktopWindowHandles: TList; virtual;

     {<B>OpenInputDesktop</B> opens the desktop that currently holds the input.
    This function does check for an already opened desktop in this instance and raises an EOpenDesktop Error
     It simply overrides the current handle of this desktop instance.
    For more information about parameters see Create .
    
    Do not use this function, it is used for internal purposes.
    
    raises
 EJwsclOpenDesktopException:  with an error description if the desktop could not be opened 
	 EJwsclDesktopException: if the desktop is already opened by this instance. }
    function OpenInputDesktop(const DesktopFlags: TJwSecurityDesktopFlags;
      const doInherit: boolean;
      const aDesiredAccess: ACCESS_MASK): HDESK;
      virtual;

     {<B>Close</B> frees a desktop handle. The instance cannot be used for more desktop manipulation.
    raises
 EJwsclCloseDesktopException:  can be raised with furhter information 
        if the desktop is already closed or an error occurred. }
    procedure Close;

   {<B>SetThreadDesktop</B> assigns the desktop to the thread that calls this function.
    This only works if the thread has no already opened windows or hooks.
    If the desktop is not opened this function does nothing.
    raises
 EJwsclDesktopException:  can be raised if an error occured }
    procedure SetThreadDesktop;
    {<B>SetLastThreadDesktop</B> tries to return to the thread desktop that
     was used before a call to SetThreadDesktop. !
     raises
     EJwsclDesktopException:  can be raised if an error occured
     }
    procedure SetLastThreadDesktop;

   {<B>SwitchDesktop</B> switches input to the desktop of this instance.
    If no desktop is opened this function does nothing.
    
   <B>SwitchDesktop</B> does save a handle to the desktop that was active before it is called so
    it can be changed back in SwitchDesktopBack.
      
     raises
 EJwsclDesktopException:  can be raised if an error occured while switching desktop }
    procedure SwitchDesktop;

   {<B>SwitchDesktopBack</B> switches the input to the desktop used before SwitchDesktop  was called.
    raises
 EJwsclDesktopException:  can be raised if an error occured while switching desktop }
    procedure SwitchDesktopBack;


  public
    {The readonly property<B>Name</B> contains the name of the Desktops. Be aware that desktop names are case sensitive.}
    property Name: TJwString Read GetName;
    {The readonly property <B>DesiredAccess</B> contains the access mask specified by the parameter aDesiredAccess in Create }
    property DesiredAccess: ACCESS_MASK Read fDesiredAccess;
    {The readonly property <B>SecurityDescriptorPtr</B> contains the SecurityDescriptor specified by the parameter aSecurityDescriptor in Create }
    property SecurityDescriptorPtr: TSECURITYATTRIBUTES Read RetrieveSD;
   {The readonly property <B>Handle</B> contains the handle to the desktop. It can be 0 if the desktop is not opened or invald.
    Be aware that an access to this handle can result to unexpected results. }
    property Handle: HDESK Read fHandle;
   {The property <B>DesktopFlags</B> gets or sets the desktop flag.
    If no desktop is opened a change of this flag is ignored.
    EJwsclDesktopException can be raised if an error occures while setting flag.}
    property DesktopFlags: TJwSecurityDesktopFlags
      Read GetDesktopFlags Write SetDesktopFlags;

    {The property <B>IsInputDesktop</B> returns true if this desktop instance was created by OpenInputDesktop; otherwise false.}
    property IsInputDesktop: boolean Read fIsInputDesktop;

    {The property <B>Parent</B> returns the parameter aParent in in Create .}
    property Parent: TJwSecurityDesktops Read fParent;

   {The property <B>CloseOnDestroy</B> is a flag that determines if the desktop handle shall be closed if the desktop instance is freed.
     That case is used if aCloseOnDestroy is TRUE otherwise the handle will exists until you close it by yourself or
     the application is closed.}
    property CloseOnDestroy: boolean Read fCloseOnDestroy
      Write fCloseOnDestroy;


     {The readonly property <B>Opened</B> gets the status of the desktop handle.
      If the desktop handle is not zero it returns true.
    Opened cannot determine a open status of a desktop that was closed by a winapi call.
     }
    property Opened: boolean Read GetOpened;

   {The event property <B>OnDestroyDesktop</B> is called if the desktop instance is destroyed.
      See TJwOnDestroyDesktop  for more information. }
    property OnDestroyDesktop: TJwOnDestroyDesktop
      Read fOnDestroyDesktop Write fOnDestroyDesktop;

     
    property UserSID: TJwSecurityId Read GetUserSID;
    property ObjectType: TJwString Read GetObjectType;
  public

     {<B>SecurityDescriptor[Info</B> sets or gets the security descriptor of the current window station.
      This property uses a parameter Info to set which information is to be set or get.
       ex. SecurityDescriptor[[sif_XXXX,sif_XXXX]]

      If used for getting the SD the caller is responsible for freeing the instance.

      Getting the security parameter uses TJwSecureGeneralObject.GetSecurityInfo ,
      setting the security parameter uses TJwSecureGeneralObject.SetSecurityInfo ,
      see the methods for more informations and errors.
     }
    property SecurityDescriptor[Info: TJwSecurityInformationFlagSet]
      : TJwSecurityDescriptor Read GetSD Write PutSD;

    property LastThreadDesktop : HDESK  read fLastThreadDesktop write fLastThreadDesktop;
    property LastSwitchDesktop : HDESK read fLastSwitchDesktop write fLastSwitchDesktop;
  end;




   {TJwSecurityDesktops is a administrative class that contains a list of desktops and
    methods to manipulate desktops.
  All TJwSecurityDesktop instances that are created and assigned to this class are
  automatically freed if this instance is freed.}
  TJwSecurityDesktops = class(TComponent)
  private
    fDesktops:         TObjectList;
    fOnDestroyDesktop: TJwOnDestroyDesktop;
  protected
    { protected declarations }

  public
    { public declarations }
   {Create creates a new instance of TJwSecurityDesktop.
    @param AOwner contains the parent of this TJwSecurityDesktops component. If you set it to another component
            the TJwSecurityDesktops instance and all its sub desktops are freed automatically. }
    constructor Create(AOwner: TComponent); override;

    {Destroy frees the TDektops instance.}
    destructor Destroy; override;

    {Clear removes all desktops that are administered by this instance.}
    procedure Clear; virtual;


     {FindDesktopByName searches for a desktop with the given name.
    The search is case sensitive.
    @param Name contains the desktop name 
    @return Returns the instance of the desktop with the given name.
           If this TJwSecurityDesktops instance does not hold a desktop instance with the given name
        it returns nil. That does not mean there is no desktop with that name. }
    function FindDesktopByName(const Name: TJwString): TJwSecurityDesktop;
      virtual;

   {FindDesktopByHandle searches for a desktop in the TJwSecurityDesktops list that has the same handle.
    This function even searches desktops administerd by this instance even if they are closed.
    In this case it temporarily opens the desktop and closes it again.
   @param Handle contains the handle to be searched for 
   @return Returns the instance of desktop to be found; otherwise found if the desktop is not in list of
          TJwSecurityDesktops }
    function FindDesktopByHandle(const Handle: HDESK): TJwSecurityDesktop;
      virtual;

   {FindInpuTSecurityDesktop searches for the desktop that was created with OpenInputDesktop.
    
    Developer warning: 
      Currently this function returns the first desktop that was created/opened by OpenInputDesktop 
    and not the current real input desktop.
    TODO: change behaviour  
   }
    function FindInputDesktop: TJwSecurityDesktop; virtual;

     {Update updates the internal desktop list.
    Some or all pointers to the desktops (TJwSecurityDesktop) can be invalid!
    
    This function removes all desktop instances that are no longer valid on the window station.
    If there are added new desktops new TJwSecurityDesktop instanced are added to the list.
    }
    procedure Update;



     {CreateDesktop creates a new desktop and adds it to the list (if successfull).
     
    @param aName Contains the name of an existing Desktop. A desktop name is case sensitive. 
    @param DesktopFlags Contains flags for desktop opening. See TJwSecurityDesktopFlags  for more information  
    @param doInherit  
    @param aDesirecAccess Contains flags that defines the type of opening. You can use the following flags
       concatenated by OR-Operator :
         READ_CONTROL, WRITE_DAC, WRITE_OWNER, STANDARD_RIGHTS_READ, STANDARD_RIGHTS_WRITE, STANDARD_RIGHTS_EXECUTE,
       STANDARD_RIGHTS_ALL, SPECIFIC_RIGHTS_ALL, ACCESS_SYSTEM_SECURITY, MAXIMUM_ALLOWED, GENERIC_READ,
       GENERIC_WRITE, GENERIC_EXECUTE, GENERIC_ALL,
       // Desktop-specific access flags
       DESKTOP_READOBJECTS, DESKTOP_CREATEWINDOW, DESKTOP_CREATEMENU, DESKTOP_HOOKCONTROL, DESKTOP_JOURNALRECORD
       DESKTOP_JOURNALPLAYBACK, DESKTOP_ENUMERATE, DESKTOP_WRITEOBJECTS
     You can look up the meanings in MSDN.
     You must at least use the flag DESKTOP_CREATEWINDOW. 
     @param aSecurityDescriptor Contains security aspects to be assigned to the new desktop 
     @return CreateDesktop returns a handle to the desktop 
     raises
 CreateDesktop:  raises EJwsclCreateDesktopException with an error description if the desktop could not be created. 
          
   }
    function CreateDesktop(const Name: TJwString;
      const DesktopFlags: TJwSecurityDesktopFlags;
      const aDesiredAccess: ACCESS_MASK;
      const aSecurityDescriptor:
      TJwSecurityDescriptor): TJwSecurityDesktop; virtual;



  { <B>OpenDesktop</B> opens an existing desktop and adds it to the list (if successfull).
    
    It calls the WinAPI function OpenDesktop. See more information in MSDN.
    @param aName Contains the name of an existing Desktop. A desktop name is case sensitive. 
    @param DesktopFlags Contains flags for desktop opening. See TJwSecurityDesktopFlags  for more information  
    @param doInherit  
    @param aDesirecAccess Contains flags that defines the type of opening. You can use the following flags
       concatenated by OR-Operator :
         READ_CONTROL, WRITE_DAC, WRITE_OWNER, STANDARD_RIGHTS_READ, STANDARD_RIGHTS_WRITE, STANDARD_RIGHTS_EXECUTE,
       STANDARD_RIGHTS_ALL, SPECIFIC_RIGHTS_ALL, ACCESS_SYSTEM_SECURITY, MAXIMUM_ALLOWED, GENERIC_READ,
       GENERIC_WRITE, GENERIC_EXECUTE, GENERIC_ALL,
       // Desktop-specific access flags 
       DESKTOP_READOBJECTS, DESKTOP_CREATEWINDOW, DESKTOP_CREATEMENU, DESKTOP_HOOKCONTROL, DESKTOP_JOURNALRECORD
       DESKTOP_JOURNALPLAYBACK, DESKTOP_ENUMERATE, DESKTOP_WRITEOBJECTS
     You can look up the meanings in MSDN. 
     @return OpenDesktop returns a handle to the desktop 
     raises
 OpenDesktop:  raises EJwsclOpenDesktopException with an error description if the desktop could not be opened
             OpenDesktop raises EJwsclDesktopException if the desktop is already opened by this instance. 

  }
    function OpenDesktop(const Name: TJwString;
      const DesktopFlags: TJwSecurityDesktopFlags;
      const aDesiredAccess: ACCESS_MASK;
      const doInherit: boolean)
      : TJwSecurityDesktop; virtual;


     {<B>OpenInputDesktop</B> opens the desktop that currently holds the input and adds it to the list (if successfull)
    This function does check for an already opened desktop in this instance and raises an EOpenDesktop Error
     It simply overrides the current handle of this desktop instance.
    For more information about parameters see Create .

    Do not use this function, it is used for internal purposes.
    
    raises
 EJwsclOpenDesktopException:  is raised if the desktop could not be opened 
     EJwsclDesktopException: is raised if the desktop is already opened by this instance. }
    function OpenInputDesktop(const DesktopFlags: TJwSecurityDesktopFlags;
      const aDesiredAccess: ACCESS_MASK;
      const doInherit: boolean): TJwSecurityDesktop;
      virtual;


   {<B>GetThreadDesktop</B> returns the desktop that is assigned to the thread given in ThreadID.
    @param ThreadID contains the thread ID that is used to determine which desktop is assigned to the Thread.
         You can set ThreadID to zero if the current thread shall be used. 
    @return Returns the desktop instance that is assigned to the given thread or 
             nil if no desktop could be found in TJwSecurityDesktops list. 
         
    raises
 GetThreadDesktop:  raises EJwsclDesktopException if the given thread does not have a desktop 
    }
    function GetThreadDesktop(ThreadID: Cardinal = 0): TJwSecurityDesktop;

     {The class function GeTSecurityDesktopName returns the name of the desktop identified by its handle
    @param deskt contains the handle value to the desktop 
    @return Returns the name of the desktop as a unicode string. The return value is a zero string
            if no desktop name could be retrieved. 
   }
    class function GetDesktopName(const desk: HDESK): TJwString;

     {<B>GetDesktops</B> creates a unicode stringlist that contains all desktop names.
    @param WindowStation contains the window station that shall be used to get the desktop names.
      You can set it to zero if you want to use the window station of the process. 
    @return Returns always a none nil Stringlist. 
    raises
 EJwsclDesktopException:  can be raised if an error occurs 
   }
    class function GetDesktops(WindowStation: HWINSTA): TJwTJwStringArray;
      overload; virtual;

    {<B>IsStationLocked</B> tries to determine if the winlogon desktop receives input from
    the console session. This function returns true if the workstation is locked.
    It returns false if user desktop or screensaver is active.
    }
    class function IsStationLocked : Boolean;

{$IFDEF DELPHI7_UP}
     {<B>GetDesktopHeapSize</B> returns the maximum heap size of the desktop heap.
      This function is only available on Delphi 7 or higher.
	  @return Returns the heap size in bytes 
	  raises
 If:  an error occurs EJwsclDesktopException will be raised }
     class function GetDesktopHeapSize : Cardinal; virtual;
{$ENDIF}

   {The readonly property Desktops contains all desktops that are administered by this instance.
    Be aware that pointers to the instances can be made invalid.}
    property Desktops: TObjectList Read fDesktops;

   {The event property <B>OnDestroyDesktop</B> replaces OnDestroyDesktop of TJwSecurityDesktop if both 
     (TJwSecurityDesktop and TJwSecurityDesktops) events are not nil.
    This happens in Clear  method.}
    property OnDestroyDesktop: TJwOnDestroyDesktop
      Read fOnDestroyDesktop Write fOnDestroyDesktop;


  end;



{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation
uses JwsclUtils;


{$ENDIF SL_OMIT_SECTIONS}


{$IFNDEF SL_INTERFACE_SECTION}

function EnumDesktopProc(lpszDesktop: TJwPChar; lParam: LPARAM): boolean;
  stdcall;
  
begin
  if lParam <> 0 then
  begin
    TJwString(Pointer(lParam)^) :=
      TJwString(Pointer(lParam)^) + #1 + TJwString(lpszDesktop);
  end;
  Result := True;
end;

function EnumWindowsProc(hwnd: HWND; lParam: LPARAM): boolean; stdcall;
begin
  if lParam <> 0 then
    try
      TList(lParam).Add(Pointer(hwnd));
    except

    end;
  Result := True;
end;

{ TJwSecurityDesktops }

procedure TJwSecurityDesktops.Clear;
var
  i: integer;
begin
  for i := 0 to fDesktops.Count - 1 do
  begin
    try
      if not Assigned(TJwSecurityDesktop(fDesktops.Items[i]).OnDestroyDesktop) then
        TJwSecurityDesktop(fDesktops.Items[i]).OnDestroyDesktop :=
          OnDestroyDesktop;

      //do not free here - clear does it for us
    finally
    end;
  end;
  fDesktops.Clear;
end;

constructor TJwSecurityDesktops.Create(AOwner: TComponent);
begin
  inherited;

  fDesktops := TObjectList.Create;
  fOnDestroyDesktop := nil;
  //  fWindowStation := nil;
end;

function TJwSecurityDesktops.CreateDesktop(const Name: TJwString;
  const DesktopFlags: TJwSecurityDesktopFlags;
  const aDesiredAccess: ACCESS_MASK;
  const aSecurityDescriptor: TJwSecurityDescriptor): TJwSecurityDesktop;
begin
  Result := FindDesktopByName(Name);

  if Assigned(Result) then
    exit;

  Result := TJwSecurityDesktop.Create(Self, dcfCreate, True, Name,
    DesktopFlags, False, aDesiredAccess, aSecurityDescriptor);

  if Assigned(Result) then
    fDesktops.add(Result);
end;

destructor TJwSecurityDesktops.Destroy;
begin
  Clear;
  FreeAndNil(fDesktops);
  inherited Destroy;
end;

function TJwSecurityDesktops.FindDesktopByHandle(
  const Handle: HDESK): TJwSecurityDesktop;
var
  i: integer;
  DesktopOpened: boolean;
begin
  Result := nil;
  for I := 0 to fDesktops.Count - 1 do
  begin
    DesktopOpened := False;
    if TJwSecurityDesktop(fDesktops[i]).Handle = 0 then
    begin
      DesktopOpened := True;
      try
        TJwSecurityDesktop(fDesktops[i]).OpenDesktopByName(
          [], True, DESKTOP_SWITCHDESKTOP);
      except
        on E: EJwsclOpenDesktopException do
          DesktopOpened := False;
      end;
    end;
    if TJwSecurityDesktop(fDesktops[i]).Handle = Handle then
    begin
      Result := TJwSecurityDesktop(fDesktops[i]);
      //   OutputDebugStringA(PAnsiChar('FindDesktopByHandle found : ' + Result.Name));
      exit;
    end;
    if DesktopOpened then
    begin
      try
        TJwSecurityDesktop(fDesktops[i]).Close;
      except
      end;
    end;
  end;
end;

function TJwSecurityDesktops.FindDesktopByName(
  const Name: TJwString): TJwSecurityDesktop;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to fDesktops.Count - 1 do
  begin
    //if WideCompareText(TJwSecurityDesktop(fDesktops[i]).Name,Name) = 0 then
    //if TecurityDesktop(fDesktops[i]).Name = Name then
    if JwCompareString(TJwSecurityDesktop(fDesktops[i]).Name, Name, True) = 0 then
    begin
      Result := TJwSecurityDesktop(fDesktops[i]);
      //     OutputDebugStringA(PAnsiChar('FindDesktopByName found : ' + Result.Name));
      exit;
    end;
  end;
end;

function TJwSecurityDesktops.FindInputDesktop: TJwSecurityDesktop;
var
  i: integer;
begin
  Result := nil;
  for I := 0 to fDesktops.Count - 1 do
  begin
    if TJwSecurityDesktop(fDesktops[i]).IsInputDesktop then
    begin
      Result := TJwSecurityDesktop(fDesktops[i]);
      //      OutputDebugStringA(PAnsiChar('FindInpuTSecurityDesktop found : ' + Result.Name));
      exit;
    end;
  end;
end;




{$IFDEF DELPHI7_UP}
class function TJwSecurityDesktops.GetDesktopHeapSize: Cardinal;
var Reg : TRegistry;
    Value, sSize : String; //this depends on TRegistry string type
    Strings : TStringList;
    p : Integer;
begin
  result := 0;
  
  Reg := TRegistry.Create;

  Reg.RootKey := HKEY_LOCAL_MACHINE;
  
  if Reg.OpenKeyReadOnly('SYSTEM\CurrentControlSet\Control\Session Manager\SubSystems\') then
  begin
    Value := Reg.ReadString('Windows');
    Strings := TStringList.Create;
    Strings.Delimiter := ' ';
    Strings.DelimitedText := Value;
    sSize := Strings.Values['SharedSection'];
    p := pos(',',sSize);
    System.Delete(sSize,p,Length(sSize));

    result := StrToIntDef(sSize,0);
  end;

  if result = 0 then
   raise EJwsclDesktopException.Create(RsDesktopFailedGetHeapSize);
end;

{$ENDIF}

class function TJwSecurityDesktops.GetDesktopName(const desk: HDESK): TJwString;
var
  Name: PWideChar;
  len:  Cardinal;
  L : DWORD;
begin
  L := GetLastError; //preserve the last error value
  try
    Result := '';
    len := 0;
    GetUserObjectInformationW(desk, UOI_NAME, nil, 0, len);

    if GetLastError = ERROR_INSUFFICIENT_BUFFER then
    begin
      GetMem(Name, len + 1);
      FillChar(Name^, len + 1, 0);
      if GetUserObjectInformationW(desk, UOI_NAME, Name, len, len) then
      begin
        Result := Name;
        FreeMem(Name);
      end;
    end;
  finally
    SetLastError(L);
  end;
end;


class function TJwSecurityDesktops.GetDesktops(WindowStation: HWINSTA):
  TJwTJwStringArray;
var
  p2: integer;
  str: TJwString;
begin
  //result := TJwTJwStringArray.Create;
  SetLength(Result, 0);

  if WindowStation = 0 then
    WindowStation := GetProcessWindowStation;


  //P := TList.Create;
  //SetLength(Str, 1000);
  Str := '';

  SetLastError(0);
  {$IFDEF UNICODE}
  if not EnumDesktopsW(WindowStation,@EnumDesktopProc,LPARAM(@Str)) then
  {$ELSE}
  if not EnumDesktopsA(WindowStation, @EnumDesktopProc, LPARAM(@Str)) then
  {$ENDIF}
  begin

    raise EJwsclDesktopException.CreateFmt(
      RsDesktopFailedEnumDesktops,
      [WindowStation]);
  end;

  if (Length(Str) > 0) then
  begin
    System.Delete(Str, 1, 1);

    p2 := pos(#1, Str);
    while p2 > 0 do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[high(Result)] := Copy(Str, 1, p2 - 1);
      System.Delete(Str, 1, p2);

      p2 := pos(#1, Str);
    end;
    if (Length(Str) > 0) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[high(Result)] := Copy(Str, 1, Length(Str));
    end;

  end;
  {
  SetLength(result, P.Count);
  for i := 0 to P.count-1 do
  begin
    l := Length(TJwString(P.Items[i]));
    SetLength(result[i], l);
    result[i] := TJwString(P[i]);
    FreeMem(P[i]);
  end;    }



  //  OutputDebugStringW(PWideChar('GeTSecurityDesktops returns : ' + result.Text));
end;



function TJwSecurityDesktops.GetThreadDesktop(ThreadID: Cardinal = 0):
TJwSecurityDesktop;

var
  desk: HDESK;
  Name: TJwString;
begin
  Result := nil;

  if ThreadID = 0 then
    ThreadID := GetCurrentThreadId;

  desk := jwaWindows.GetThreadDesktop(ThreadID);

  if Desk > 0 then
  begin
    Name := GetDesktopName(desk);

    if Length(Name) > 0 then
      Result := FindDesktopByName(Name);
  end
  else
    raise EJwsclDesktopException.CreateFmt(
      RsDesktopFailedGetThreadDesktop,
      [ThreadID]);

end;



function TJwSecurityDesktops.OpenDesktop(const Name: TJwString;
  const DesktopFlags: TJwSecurityDesktopFlags;
  const aDesiredAccess: ACCESS_MASK;
  const doInherit: boolean): TJwSecurityDesktop;
begin
  Result := FindDesktopByName(Name);

  if Assigned(Result) then
  begin
    if not Result.Opened then
      Result.OpenDesktop(Result.Name, DesktopFlags, doInherit, aDesiredAccess);
    exit;
  end;

  Result := TJwSecurityDesktop.Create(Self, dcfOpen, False, Name,
    DesktopFlags, doInherit, aDesiredAccess, nil);
  fDesktops.add(Result);
end;

function TJwSecurityDesktops.OpenInputDesktop(
  const DesktopFlags: TJwSecurityDesktopFlags; const aDesiredAccess: ACCESS_MASK;
  const doInherit: boolean): TJwSecurityDesktop;
begin
  Result := FindInputDesktop;

  if Assigned(Result) then
  begin
    if not Result.Opened then
      Result.OpenInputDesktop(DesktopFlags, doInherit, aDesiredAccess);
    exit;
  end;

  Result := TJwSecurityDesktop.CreateUnInitialized(Self, '');
  Result.OpenInputDesktop(DesktopFlags, doInherit, aDesiredAccess);

  fDesktops.Add(Result);
end;




procedure TJwSecurityDesktops.Update;

var
  Stations: TJwTJwStringArray;
  i, idx: integer;

begin
  //  if Assigned(WindowStation) then
  //    Stations := GeTSecurityDesktops(WindowStation)
  //  else
  Stations := GetDesktops(jwaWindows.GetProcessWindowStation);


  try
    if Assigned(Stations) then
    begin
      {TODO: search case insensetive in delphi 5}
{$IFDEF DELPHI7_UP}
     // Stations.CaseSensitive := FALSE;
{$ENDIF}
      for i := fDesktops.Count - 1 downto 0 do
      begin
        if Assigned(fDesktops[i]) then
        begin
          idx := JwStringArrayIndexOf(Stations, TJwSecurityDesktop(fDesktops[i]).Name);
          if idx < 0 then
          begin
            try
              //              OutputDebugStringW(PWideChar('Update frees : ' + TJwSecurityDesktop(fDesktops[i]).Name));
              TJwSecurityDesktop(fDesktops[i]).CloseOnDestroy := False;
              TJwSecurityDesktop(fDesktops[i]).Free;
            finally
              fDesktops.Delete(i);
            end;
          end;
        end;
      end;

      for I := 0 to Length(Stations) - 1 do
      begin
        if not Assigned(FindDesktopByName(Stations[i])) then
        begin
          fDesktops.Add(TJwSecurityDesktop.CreateUnInitialized(
            Self, Stations[i]));
          //          OutputDebugStringW(PWideChar('Update adds : ' + Stations[i]));
        end;
      end;

    end;
  finally
  end;
end;

class function TJwSecurityDesktops.IsStationLocked : Boolean;
var Desk : TJwSecurityDesktops;
begin
  result := true;
  Desk := TJwSecurityDesktops.Create(nil);
  try
    try
      Desk.OpenInputDesktop([], DESKTOP_READOBJECTS, false);
    except
      On e : EJwsclOpenDesktopException do
      begin
        FreeAndNil(Desk);
        exit;
      end;
    end;
    result := JwCompareString(Desk.Name, 'winlogon', true) = 0;
  finally
    FreeAndNil(Desk);
  end;
end;



{ TJwSecurityDesktop }


procedure TJwSecurityDesktop.Close;
begin
  if not Opened then
    raise EJwsclCloseDesktopException.CreateFmt(
      RsDesktopInvalidClosedDesktop, [Name]);

  SetLastError(0);
  if Handle > 0 then
    if not jwaWindows.CloseDesktop(Handle) then
      raise EJwsclCloseDesktopException.CreateFmt(
        RsDesktopCloseFailed,
        [Name,GetLastError, EJwsclSecurityException.GetLastErrorMessage]);

  fHandle := 0;
end;


constructor TJwSecurityDesktop.Create(const aParent: TJwSecurityDesktops;
  const aFlag: TJwDesktopCreationFlag; const aCloseOnDestroy: boolean;
  const aName: TJwString; const DesktopFlags: TJwSecurityDesktopFlags;
  const doInherit: boolean; const aDesiredAccess: ACCESS_MASK;
  const aSecurityDescriptor: TJwSecurityDescriptor);
begin
  inherited Create;
  fParent := aParent;
  fSD := nil;
  fName := aName;
  fDesiredAccess := aDesiredAccess;
  fOpened := False;
  fHandle := 0;
  fOnDestroyDesktop := nil;
  fCloseOnDestroy := aCloseOnDestroy;
  fIsInputDesktop := False;
  fLastSwitchDesktop := jwaWindows.OpenInputDesktop(0, false, DESKTOP_SWITCHDESKTOP);
  fLastThreadDesktop := jwaWindows.GetThreadDesktop(GetCurrentThreadId);

  fUserSID := nil;



  SetLastError(0);
  case aFlag of
    dcfCreate: fHandle :=
        HDESK(Self.CreateDesktop(aName, DesktopFlags, aDesiredAccess, aSecurityDescriptor));
    dcfOpen: fHandle :=
        HDESK(Self.OpenDesktop(aName, DesktopFlags, doInherit, aDesiredAccess));
  end;

end;

constructor TJwSecurityDesktop.CreateAndGetInputDesktop(
  const DesktopFlags: TJwSecurityDesktopFlags; const InheritHandles: boolean;
  const DesiredAccess: ACCESS_MASK);
begin
  self.Create;
  fHandle := OpenInputDesktop(DesktopFlags, InheritHandles, DesiredAccess)
end;

constructor TJwSecurityDesktop.CreateByHandle(const Handle: HDESK;
  const CloseOnDestroy: boolean);
begin
  fParent := nil;
  fSD := nil;
  fName := '';
  fDesiredAccess := 0;
  fOpened := true;
  fHandle := Handle;
  fOnDestroyDesktop := nil;
  fCloseOnDestroy := CloseOnDestroy;
  fIsInputDesktop := False;
  fLastSwitchDesktop := jwaWindows.OpenInputDesktop(0, false, DESKTOP_SWITCHDESKTOP);
  fLastThreadDesktop := jwaWindows.GetThreadDesktop(GetCurrentThreadId);

  fUserSID := nil;

end;

constructor TJwSecurityDesktop.CreateDesktop(const aParent: TJwSecurityDesktops;
  const aCloseOnDestroy: boolean;
  const aName: TJwString;
  const DesktopFlags: TJwSecurityDesktopFlags;
  const doInherit: boolean;
  const aDesiredAccess: ACCESS_MASK;
  const aSecurityDescriptor: TJwSecurityDescriptor);
begin
  Self.Create(
    aParent,//const aParent: TJwSecurityDesktops;
    dcfCreate,//const aFlag: TJwDesktopCreationFlag;
    aCloseOnDestroy,//const aCloseOnDestroy: Boolean;
    aName,//const aName: TJwString;
    DesktopFlags,//const DesktopFlags: TJwSecurityDesktopFlags;
    doInherit,//const doInherit: Boolean;
    aDesiredAccess,//const aDesiredAccess: ACCESS_MASK;
    aSecurityDescriptor//const aSecurityDescriptor: TJwSecurityDescriptor
    );
end;

constructor TJwSecurityDesktop.OpenDesktop(const aParent: TJwSecurityDesktops;
  const aCloseOnDestroy: boolean;
  const aName: TJwString;
  const DesktopFlags: TJwSecurityDesktopFlags;
  const doInherit: boolean;
  const aDesiredAccess: ACCESS_MASK);
begin
  Self.Create(
    aParent,//const aParent: TJwSecurityDesktops;
    dcfOpen,//const aFlag: TJwDesktopCreationFlag;
    aCloseOnDestroy,//const aCloseOnDestroy: Boolean;
    aName,//const aName: TJwString;
    DesktopFlags,//const DesktopFlags: TJwSecurityDesktopFlags;
    doInherit,//const doInherit: Boolean;
    aDesiredAccess,//const aDesiredAccess: ACCESS_MASK;
    nil
    );
end;

function TJwSecurityDesktop.CreateDesktop(const aName: TJwString;
  const DesktopFlags: TJwSecurityDesktopFlags;
  const aDesiredAccess: ACCESS_MASK;
  const aSecurityDescriptor: TJwSecurityDescriptor): HDESK;
var
  pSec: PSECURITYATTRIBUTES;
  aSecurityInfo: TJwSecurityInformationFlagSet;
  Access : DWORD;
begin
  pSec := nil;

  if Assigned(aSecurityDescriptor) then
  begin
  //  MessageBoxW(0,PWideChar(aSecurityDescriptor.Text),'', MB_OK);
  //  pSec := aSecurityDescriptor.Create_SA(True);
  end;

  Access := 0;
  if Assigned(aSecurityDescriptor) then
  begin
    aSecurityInfo := [];
    if Assigned(aSecurityDescriptor.Owner) or Assigned(aSecurityDescriptor.PrimaryGroup) then
       Access := Access or WRITE_OWNER;
    if Assigned(aSecurityDescriptor.DACL) or (sdcDaclPresent in aSecurityDescriptor.Control) then
      Access := Access or WRITE_DAC;
    if Assigned(aSecurityDescriptor.SACL) and (sdcSaclPresent in aSecurityDescriptor.Control) then
      Access := Access or ACCESS_SYSTEM_SECURITY;
  end;

  {$IFDEF UNICODE}
  result := jwaWindows.CreateDesktopW(PWideChar(aName),nil,nil,
                        DesktopFlagsToInt(DesktopFlags),
                        aDesiredAccess or Access, LPSECURITY_ATTRIBUTES(pSec));
  {$ELSE}
  Result := jwaWindows.CreateDesktopA(PAnsiChar(aName), nil, nil,
    DesktopFlagsToInt(DesktopFlags),
    aDesiredAccess or Access,
    LPSECURITY_ATTRIBUTES(pSec));
  {$ENDIF}

  //if Assigned(aSecurityDescriptor) then
  //  aSecurityDescriptor.Free_SA(pSec);

  if Result = 0 then
  begin
    raise EJwsclCreateDesktopException.CreateFmtWinCall(RsDesktopCreateFailed,
       'CreateDesktop', ClassName, 'JWsclDesktops.pas',0,
       true, 'CreateDesktop', [Name]);
    {raise EJwsclCreateDesktopException.CreateFmt(
      RsDesktopCreateFailed,
      [Name]);}
    exit;
  end;

  //the lpsec descriptor does not work - it always used default DACL!!!
  //so we set it here
  if Assigned(aSecurityDescriptor) and (Result <> 0) then
  begin
    aSecurityInfo := [];
    if Assigned(aSecurityDescriptor.Owner) then
      Include(aSecurityInfo,siOwnerSecurityInformation);
    if Assigned(aSecurityDescriptor.PrimaryGroup) then
      Include(aSecurityInfo,siGroupSecurityInformation);
    if Assigned(aSecurityDescriptor.DACL) or (sdcDaclPresent in aSecurityDescriptor.Control) then
      Include(aSecurityInfo,siDaclSecurityInformation);
    if Assigned(aSecurityDescriptor.SACL) and (sdcSaclPresent in aSecurityDescriptor.Control) then
      Include(aSecurityInfo,siSaclSecurityInformation);

    try
      TJwSecureGeneralObject.SetSecurityInfo(Result,SE_KERNEL_OBJECT,
        aSecurityInfo, aSecurityDescriptor);
    except
      on E : Exception do
      begin
        CloseDesktop(Result);
        raise;
      end;
    end;
  end;


  fOpened  := Result > 0;
  fHandle  := Result;
  fUserSID := nil;
end;

constructor TJwSecurityDesktop.CreateUnInitialized(
  const aParent: TJwSecurityDesktops; const aName: TJwString);
begin
  inherited Create;
  fParent := aParent;
  fName := aName;
  fDesiredAccess := 0;
  fHandle := 0;
  fOnDestroyDesktop := nil;
  fCloseOnDestroy := False;
  fIsInputDesktop := False;
  fOpened := False;
  //fLastSwitchDesktop := jwaWindows.OpenInputDesktop(0, false, DESKTOP_SWITCHDESKTOP);
  //fLastThreadDesktop := jwaWindows.GetThreadDesktop(GetCurrentThreadId);
  fLastThreadDesktop := 0;
  fLastSwitchDesktop := 0;


  fSD := nil;
  fUserSID := nil;

  //OutputDebugStringA(PAnsiChar('CreateUnInitialized called '));
end;

function TJwSecurityDesktop.DesktopFlagsToInt(DesktopFlags:
  TJwSecurityDesktopFlags): Cardinal;
begin
  Result := 0;
  if dfAllowOtherAccountHook in DesktopFlags then
    Inc(Result, jwaWindows.DF_ALLOWOTHERACCOUNTHOOK);
end;

destructor TJwSecurityDesktop.Destroy;
begin
  if Assigned(OnDestroyDesktop) then
    OnDestroyDesktop(Self);

  FreeAndNil(fSD);
  FreeAndNil(fUserSID);

  if CloseOnDestroy then
    try
      Close;
    except
      on E: EJwsclCloseDesktopException do ; //we do not care
    end;

  CloseDesktop(fLastSwitchDesktop);
  CloseDesktop(fLastThreadDesktop);

  inherited;
end;

function TJwSecurityDesktop.GetDesktopFlags: TJwSecurityDesktopFlags;
var
  Len: Cardinal;
  Flags: USEROBJECTFLAGS;
begin
  FillChar(Flags, sizeof(Flags), 0);

  if Handle = 0 then
    exit;

  if GetUserObjectInformation(Handle, UOI_FLAGS,
    @Flags, sizeof(Flags), len) then
  begin
    Result := [TJwSecurityDesktopFlag(Flags.dwFlags)];
  end;
end;

function TJwSecurityDesktop.GeTSecurityDesktopWindowHandles: TList;
begin
  Result := TList.Create;

  if Handle > 0 then
    EnumDesktopWindows(Handle, @EnumWindowsProc, LPARAM(Result));
end;




function TJwSecurityDesktop.GetName: TJwString;
var LastError : DWORD;
begin
  LastError := GetLastError; //Save last error value since we use this method everywhere
  if (Handle = 0) or not Opened then
    Result := fName
  else
    try
      Result := TJwSecurityDesktops.GetDesktopName(Handle);
      //    if Length(fName) = 0 then
      fName  := Result;
    except
      Result := fName;
    end;
  SetLastError(LastError);
end;

function TJwSecurityDesktop.GetOpened: boolean;
begin
  if Handle = 0 then
  begin
    fOpened := False;
    Result  := False;
  end
  else
    Result := fOpened;
end;

function TJwSecurityDesktop.OpenDesktop(
  const aName: TJwString;
  const DesktopFlags: TJwSecurityDesktopFlags;
  const doInherit: boolean;
  const aDesiredAccess: ACCESS_MASK): HDESK;
begin
  if Opened then
    raise EJwsclDesktopException.CreateFmt(
      RsDesktopAlreadyOpened, [Name]);

  {$IFDEF UNICODE}
  result := jwaWindows.OpenDesktopW(PWideChar(aName),
                        DesktopFlagsToInt(DesktopFlags),doInherit,aDesiredAccess);
  {$ELSE}
  Result := jwaWindows.OpenDesktopA(PAnsiChar(aName),
    DesktopFlagsToInt(DesktopFlags),
    doInherit, aDesiredAccess);
  {$ENDIF}


  if Result = 0 then
    {raise EJwsclOpenDesktopException.CreateFmt(
      RsDesktopOpenFailed,
      [Name]);}
    raise EJwsclDesktopException.CreateFmtWinCall(RsDesktopOpenFailed,
       'OpenDesktop', ClassName, 'JwsclDesktops.pas',0,
       true, 'OpenDesktop', [Name]);

  fOpened := Result > 0;
  fHandle := Result;
end;




function TJwSecurityDesktop.OpenDesktopByName(
  const DesktopFlags: TJwSecurityDesktopFlags; const doInherit: boolean;
  const aDesiredAccess: ACCESS_MASK): HDESK;
begin
  fHandle := HDESK(Self.OpenDesktop(Name, DesktopFlags, doInherit, aDesiredAccess));
  Result  := fHandle;
end;

procedure TJwSecurityDesktop.SetDesktopFlags(
  const DesktopFlags: TJwSecurityDesktopFlags);
var
  Flags: USEROBJECTFLAGS;
begin
  FillChar(Flags, sizeof(USEROBJECTFLAGS), 0);

  Flags.dwFlags := DesktopFlagsToInt(DesktopFlags);
  if Handle > 0 then
    if not SetUserObjectInformation(Handle, UOI_FLAGS, @Flags,
      sizeof(Flags)) then
      raise EJwsclDesktopException.CreateFmt(
        RsDesktopFailedSetFlags,
        [Name]);
end;

procedure TJwSecurityDesktop.SetInherit(const Value: boolean);
var
  Flags: USEROBJECTFLAGS;
begin
  FillChar(Flags, sizeof(USEROBJECTFLAGS), 0);

  Flags.dwFlags := DesktopFlagsToInt(DesktopFlags);
  if Handle > 0 then
    if not SetUserObjectInformation(Handle, UOI_FLAGS, @Flags,
      sizeof(Flags)) then
      raise EJwsclDesktopException.CreateFmt(
        RsDesktopFailedSetInheritFlag,
        [Name]);
end;


procedure TJwSecurityDesktop.SetThreadDesktop;
var
  L: integer;
  tempLastDesktop : DWORD;
begin
  tempLastDesktop := jwaWindows.GetThreadDesktop(GetCurrentThreadId);

  SetLastError(0);
  if Handle > 0 then
    if not jwaWindows.SetThreadDesktop(Handle) then
    begin
      L := GetLastError;
      if L <> 0 then
        {raise EJwsclDesktopException.CreateFmt(
          RsDesktopFailedSetThreadDesktop,
          [Name]);}
        raise EJwsclDesktopException.CreateFmtWinCall(RsDesktopFailedSetThreadDesktop,
       'SetThreadDesktop', ClassName, 'JWsclDesktops.pas',0,
       true, 'SetThreadDesktop', [Name]);
    end;
  fLastThreadDesktop := tempLastDesktop;
end;

procedure TJwSecurityDesktop.SetLastThreadDesktop;
var
  L: integer;
begin
  SetLastError(0);
  if JwIsHandleValid(fLastThreadDesktop) then
  begin
    if not jwaWindows.SetThreadDesktop(fLastThreadDesktop) then
    begin
      L := GetLastError;
      if L <> 0 then
        raise EJwsclDesktopException.CreateFmtWinCall(RsDesktopFailedSetThreadDesktop,
       'SetLastThreadDesktop', ClassName, 'JWsclDesktops.pas',0,
       true, 'SetLastThreadDesktop', [Name]);

    end;
  end;
end;


procedure TJwSecurityDesktop.SwitchDesktop;
var tempLastDesktop : DWORD;
begin
  tempLastDesktop := jwaWindows.OpenInputDesktop(0, false, DESKTOP_SWITCHDESKTOP);
  if Handle > 0 then
    if not jwaWindows.SwitchDesktop(Handle) then
      raise EJwsclDesktopException.CreateFmt(
        RsDesktopSwitchFailed,
        [Name]);

  fLastSwitchDesktop := tempLastDesktop;
end;


procedure TJwSecurityDesktop.SwitchDesktopBack;
begin
  if JwIsHandleValid(fLastSwitchDesktop) then
  begin
    if not jwaWindows.SwitchDesktop(fLastSwitchDesktop) then
     raise EJwsclDesktopException.CreateFmt(
         RsDesktopFailedSwitchBack, [Name]);

  end;
end;

function TJwSecurityDesktop.OpenInputDesktop(
  const DesktopFlags: TJwSecurityDesktopFlags; const doInherit: boolean;
  const aDesiredAccess: ACCESS_MASK): HDESK;
begin
  if Opened then
    raise EJwsclOpenDesktopException.CreateFmt(
      RsDesktopAlreadyOpened, [Name]);

  fIsInputDesktop := True;

  Result := jwaWindows.OpenInputDesktop(DesktopFlagsToInt(
    DesktopFlags), doInherit, aDesiredAccess);

  if Result = 0 then
    raise EJwsclOpenDesktopException.CreateFmtWinCall(RsDesktopFailedOpenDesktop,
       'OpenInputDesktop', ClassName, 'JWsclDesktops.pas',0,
       true, 'OpenInputDesktop', [Name]);

{    raise EJwsclOpenDesktopException.CreateFmt(
      RsDesktopFailedOpenDesktop,
      [Name]);}

  fHandle := Result;
  fDesiredAccess := aDesiredAccess;
  fOpened := Result > 0;
end;


function TJwSecurityDesktop.GetObjectType: TJwString;
var
  len: Cardinal;
begin
{$IFDEF UNICODE}
  GetUserObjectInformationW(
{$ELSE}
  GetUserObjectInformationA(
{$ENDIF UNICODE}
    fHandle,//HANDLE hObj,
    UOI_TYPE,//int nIndex,
    nil,//PVOID pvInfo,
    0, //DWORD nLength,
    len//LPDWORD lpnLengthNeeded
    );


  SetLength(Result, len div TJwCharSize );
  FillChar(Result[1], len div TJwCharSize , 0);
  if not
{$IFDEF UNICODE}
  GetUserObjectInformationW(
{$ELSE}
  GetUserObjectInformationA(
{$ENDIF UNICODE}
     fHandle,//HANDLE hObj,
    UOI_TYPE,//int nIndex,
    @Result[1],//PVOID pvInfo,
    len,//DWORD nLength,
    len //LPDWORD lpnLengthNeeded
    ) then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'Open',                                //sSourceProc
      ClassName,                                //sSourceClass
      'JwsclWinStations.pas',                          //sSourceFile
      0,                                           //iSourceLine
      True,                                  //bShowLastError
      'OpenWindowStation',                   //sWinCall
      ['OpenWindowStation']);                                  //const Args: array of const

  SetLength(Result, len div TJwCharSize - 1);  
  if Result = '' then;
end;

function TJwSecurityDesktop.GetUserSID: TJwSecurityId;
var
  len: Cardinal;
  apSID: PSID;
begin
  Result := nil;
  len := 0;
{$IFDEF UNICODE}
  GetUserObjectInformationW  (
{$ELSE}
  GetUserObjectInformationA(
{$ENDIF UNICODE}
    fHandle,//HANDLE hObj,
    UOI_USER_SID,//int nIndex,
    nil,//PVOID pvInfo,
    0, //DWORD nLength,
    len//LPDWORD lpnLengthNeeded
    );


  if len = 0 then
    exit;

  GetMem(apSid, len);


  if not
{$IFDEF UNICODE}
  GetUserObjectInformationW(
{$ELSE}
  GetUserObjectInformationA(
{$ENDIF UNICODE}
    fHandle,//HANDLE hObj,
    UOI_USER_SID,//int nIndex,
    apSID,//PVOID pvInfo,
    len,//DWORD nLength,
    len //LPDWORD lpnLengthNeeded
    ) then
  begin
    FreeMem(apSid);
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'Open',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNDesktops,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'GetUserSID',                  //sWinCall
      ['GetUserObjectInformation']);                                  //const Args: array of const
  end;

  FreeAndNil(fUserSID);


  fUserSID := TJwSecurityId.Create(apSid);
  Result := fUserSID;

  FreeMem(apSid);
end;


function TJwSecurityDesktop.RetrieveSD: TSecurityAttributes;
var
  Len: Cardinal;
begin
  FillChar(Result, sizeof(Result), 0);

  if Handle = 0 then
    exit;

  if GetUserObjectInformation(Handle, UOI_USER_SID, nil, 0, len) then
  begin
    if GetUserObjectInformation(Handle, UOI_USER_SID, @Result, len, len) then
    begin
      //OutputDebugStringA(PAnsiChar('RetrieveSD retrieved SD : ok'));
    end
    else
    begin
      //OutputDebugStringA(PAnsiChar('RetrieveSD retrieved SD : failure'));
    end;
  end;
end;

function TJwSecurityDesktop.GetSD(Info: TJwSecurityInformationFlagSet)
: TJwSecurityDescriptor;
begin
  FreeAndNil(fSD);
  Result := TJwSecureGeneralObject.GetSecurityInfo(
    Handle,//const aHandle : THandle;
    SE_WINDOW_OBJECT,//const aObjectType : TSeObjectType;
    Info//aSecurityInfo: TJwSecurityInformationFlagSet;
    );
  fSD := Result;
end;

procedure TJwSecurityDesktop.PutSD(Info: TJwSecurityInformationFlagSet;
  aSD: TJwSecurityDescriptor);
begin
  FreeAndNil(fSD);
  TJwSecureGeneralObject.SetSecurityInfo(
    Handle,//const aHandle : THandle;
    SE_WINDOW_OBJECT,//const aObjectType : TSeObjectType;
    Info,//aSecurityInfo: TJwSecurityInformationFlagSet;
    aSD//const aSecurityDescriptor: TJwSecurityDescriptor);
    );
end;

{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
initialization
{$ENDIF SL_OMIT_SECTIONS}
{$IFNDEF SL_INITIALIZATION_SECTION}

{$ENDIF SL_INITIALIZATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
finalization
{$ENDIF SL_OMIT_SECTIONS}
{$IFNDEF SL_FINALIZATION_SECTION}

{$ENDIF SL_FINALIZATION_SECTION}



{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}
