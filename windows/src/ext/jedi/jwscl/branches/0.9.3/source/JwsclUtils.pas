{
Description
Project JEDI Windows Security Code Library (JWSCL)

This unit hosts utilty functions.

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

Note

The Original Code is JwsclUtils.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

}
unit JwsclUtils;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $


//Check for FastMM4
{$IFDEF FASTMM4}
  //and also activate debug mode (leak detection for Local-/GlobalAlloc)
  {$DEFINE FullDebugMode}
{$ENDIF FASTMM4}
{.$UNDEF FullDebugMode}

//check for Eurekalog
{$IFDEF EUREKALOG}
  {$DEFINE FullDebugMode}
{to see if this memory manager catches Local/Global leaks}
  {.$UNDEF FASTMM4}
  {.$UNDEF MEMCHECK}
  {.$UNDEF FullDebugMode}
{$ENDIF EUREKALOG}

interface

uses
  Classes,
  JwaWindows,
{$IFDEF JCL}
  JclWideFormat,
  JclWideStrings,
{$ENDIF}
  JwsclTypes,
  JwsclExceptions,
  JwsclResource,
  //JwsclDescriptor, //do not set!
  JwsclStrings;


type
  {<B>TJwThread</B> defines a thread base class
  which offers a name for the thread.
  Override Execute and call it at first
  to have any effect.
  }
  TJwThread = class(TThread)
  private
    { Private declarations }
    FName: TJwString;

    procedure SetName(const Name: TJwString);
  protected
    FTerminatedEvent: THandle;
  public
    {<B>Execute</B> is the main execution procedure of thread.
     Override it and call it at first.
    }
    procedure Execute; override;
  public
    {<B>Create</B> create a thread instance.
     @param CreateSuspended defines whether the thread is created
      and commenced immediately (false) or suspended (true). 
     @param Name defines the thread's name 
     }
    constructor Create(const CreateSuspended: Boolean; const Name: TJwString);
    destructor Destroy; override;

    function GetReturnValue : Integer;

    {<B>WaitWithTimeOut</B> waits for the thread to finish or
     until a timeout occurs.
    @param TimeOut Define a timeout. If the value is 0 or INFINITE
      the function behaves like WaitFor.
    @param MsgLoop Defines whether the method supports window messages
      to avoid a GUI dead lock.
    @return Returns WAIT_TIMEOUT if a timeout occured or zero (0) if the thread finished.

    Remarks
      The method does not support waiting with message loop support in the case
       when parameter MsgLoop is true and the current thread is not the main thread.

      Delphi 5 does not support SyncEvent. This event is signaled every time
      a thread wishes to synchronize with the main thread.
     }
    function WaitWithTimeOut(const TimeOut: DWORD;
      const MsgLoop : Boolean = true) : LongWord;


    {<B>Name</B> sets or gets the threads name.
     The name is retrieved from internal variable. Changing the thread's name
     using foreign code does not affect this property.
    }
    property Name: TJwString read FName write SetName;

  end;

  {<B>TJwIntTupleList</B> defines an integer tuple list.
  Despite its name the list manages index with pointers (integer,pointer).

  todo
    make this bijective
      index <-> pointer

  }
  TJwIntTupleList = class
  protected
    fList : TList;
    function GetItem(Index : DWORD) : Pointer;
    procedure SetItem(Index : DWORD; Value : Pointer);
    function GetCount : Cardinal;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Index : DWORD; Value : Pointer);
    procedure DeleteIndex(Index : DWORD);

    procedure Clear;

    property Items[Index : DWORD] : Pointer read GetItem write SetItem; default;
    property Count : Cardinal read GetCount;
  end;

{<B>JwGlobalFreeAndNil</B> calls GlobalFree on parameter hMem and sets it to zero (0).}
procedure JwGlobalFreeAndNil(var hMem: HGLOBAL);

{<B>JwLocalAllocMem</B> creates a managed memory handle by LocalAlloc.
Some memory leak managers do not recognize leaks created by
LocalAlloc and GlobalAlloc. Thus we create for them a GetMem
memory block.
Replace each call to LocalAlloc/GlobalAlloc with JwLocalAllocMem/JwGlobalAllocMem
and their counter parts JwLocalFreeMem/JwGlobalFreeMem.
If a counter part is not called and the program halts the memory manager
will (hopefully) show the stack trace to the GetMemPointer created by
JwLocalAllocMem/JwGlobalAllocMem.

Warning: Do not call JwLocalAllocMem/JwGlobalAllocMem for API functions
that will free the handle. GetMemPointer will remain
whatsoever. Instead use LocalAlloc/GlobalAlloc.
This behavior is rare but the API documentation will (mostly) say it.
Refer to MSDN documentation for more information.}
function JwLocalAllocMem(uFlags: UINT; uBytes: SIZE_T): HLOCAL;

{<B>JwLocalFreeMem</B> frees a managed LocalAlloc handle created by JwLocalAllocMem.
The given handle will be set to 0.
Refer to MSDN documentation for more information.
raises
 EInvalidPointer:  if the given handle was not created by JwLocalAllocMem .}
function JwLocalFreeMem(var hMem: HLOCAL): HLOCAL;

{<B>JwGlobalAllocMem</B> creates a managed memory handle by LocalAlloc.
Some memory leak managers do not recognize leaks created by
LocalAlloc and GlobalAlloc. Thus we create for them a GetMem
memory block.
Replace each call to LocalAlloc/GlobalAlloc with JwLocalAllocMem/JwGlobalAllocMem
and their counter parts JwLocalFreeMem/JwGlobalFreeMem.
If a counter part is not called and the program halts the memory manager
will (hopefully) show the stack trace to the GetMemPointer created by
JwLocalAllocMem/JwGlobalAllocMem.

Warning: Do not call JwLocalAllocMem/JwGlobalAllocMem for API functions
that will free the handle. GetMemPointer will remain
whatsoever. Instead use LocalAlloc/GlobalAlloc.
This behavior is rare but the API documentation will (mostly) say it.
Refer to MSDN documentation for more information.}
function JwGlobalAllocMem(uFlags: UINT; uBytes: SIZE_T): HGLOBAL;

{<B>JwGlobalFreeMem</B> frees a managed GlobalAlloc handle created by JwGlobalAllocMem.
The given handle will be set to 0.
Refer to MSDN documentation for more information.
raises
 EInvalidPointer:  if the given handle was not created by JwGlobalAllocMem.}
function JwGlobalFreeMem(var hMem: HGLOBAL): HGLOBAL;





{<B>LocalizeMapping</B> loads the resource strings of a TJwRightsMapping record array
defined in JwsclTypes.pas.
To convert a rights mapping record array define a start resource string
index, say 4000. This is the starting point of the resource strings, but
it does not define a string. It simply contains a number that defines the count
of array elements, say 4.
So the record array must look like this :
<code lang="Delphi">
  MyMapping: array[1..4] of TJwRightsMapping =
    (
    (Right: STANDARD_RIGHTS_ALL; Name: 'STANDARD_RIGHTS_ALL';
    Flags: 0; StringId : 5008),
    (Right: STANDARD_RIGHTS_READ; Name: 'STANDARD_RIGHTS_READ';
    Flags: 0),
    (Right: STANDARD_RIGHTS_WRITE; Name: 'STANDARD_RIGHTS_WRITE';
    Flags: 0),
    (Right: STANDARD_RIGHTS_EXECUTE; Name: 'STANDARD_RIGHTS_EXECUTE';
    Flags: 0));
</code>
Each element is linked to the resource string. e.g.
 MyMapping[1].Name is read from string resource with index [4001]
 MyMapping[2].Name is read from string resource with index [4002] and so on.
So the last index of the array (here 4) is resource index [4004].

There is the possibility to use exceptional indexes. To do so set StringId
member of the TJwRightsMapping to an index which starts at "StartStringId".
The positive number will be increased by the parameter StartStringId to
get the resource string index.
E.g. set StringId to 20 to load the resource string from <pre>index [4020] (=
<StartStringId> + 20)</pre>
It is also possible to use absolute values - like 4020. To use them
simply negate the StringId. e.g. StringID: "-4020" will load index [4020].
It is discouraged to use absolute values because they do not depend on the
parameter StartStringId. Changing this value and the resource strings
will lead to EJwsclResourceNotFound exception.

@param MappingRecord @italic([in,out]) receives an array of TJwRightsMapping which
  string member Name is replaced by the resource string. 

@param StartStringId defines the starting position of the index counting.
 It must be an absolute resource string index, which context contains a number
 that defines the count of array elements. 

@param PrimaryLanguageId defines the primary language id.
use PRIMARYLANGID(GetUserDefaultUILanguage), SUBLANGID(GetUserDefaultUILanguage)
to get user language. 

@param SubLanguageId defines the sub language id. 

@param UseDefaultOnError defines whether EJwsclResourceNotFound is thrown if a
resource index is invalid (could not be found in resource) (false) or not (true).
If UseDefaultOnError is true the function does the following.

1. Try to load resource string at index given by member StringId 
2. if fails : try to load resource string ignoring member StringId 
3. if fails : leave the text member Name to default value 
 

@param ModuleInstance defines where the resource strings can be found. It is simply
put through to LoadString function. It can be an instance of a dll file which
contains localized versions of the strings. 

raises
 EJwsclResourceInitFailed:  is raised if the given value in parameter
StartStringId could not be found in the string resource 

 EJwsclResourceUnequalCount: is raised if the count of the members of
the given array (MappingRecord) is not equal to the number given in the resource
string at index StartStringId. 

 EJwsclResourceNotFound: is raised if UseDefaultOnError is false and
a given resource index of a member of the array of TJwRightsMapping could not
be found }
procedure LocalizeMapping(var MappingRecord : array of TJwRightsMapping;
  const StartStringId : Cardinal;
  const PrimaryLanguageId, SubLanguageId : Word;
  const UseDefaultOnError : Boolean = true;
  ModuleInstance : HINST = 0
  );

{<B>JwCheckArray</B> checks whether a object type list array is correctly formed.
The array must be in a post fix order. This sequence describes the
Level structure.

<pre>
Objs[i .Level = a_i
        { a_i +1        | a_i - a_(i-1) = 1 AND a_i < 4
a_i+1 = { a_i - t       | a_i - t AND t >= 0
        { ERROR_INVALID_PARAMETER | else
</pre> 

sequence start: a_0 = 0

@param Objs contains the object list 
@return Returns true if the object type list is correct; otherwise false.
      It returns false if Objs is nil or does not contain any element.
      It also returns false if any GUID member is nil. 
}
function JwCheckArray(const Objs : TJwObjectTypeArray) : Boolean; overload;

{<B>JwCheckArray</B> checks whether a object type list array is correctly formed.
The array must be in a post fix order. This sequence describes the
Level structure.

<pre>
Objs[i .Level = a_i
        { a_i +1        | a_i - a_(i-1) = 1 AND a_i < 4
a_i+1 = { a_i - t       | a_i - t AND t >= 0
        { ERROR_INVALID_PARAMETER | else
</pre> 

sequence start: a_0 = 0

@param Objs contains the object list 
@param Index returns the index where an error occured. 
@return Returns true if the object type list is correct; otherwise false.
      It returns false if Objs is nil or does not contain any element.
      It also returns false if any GUID member is nil. 
}
function JwCheckArray(const Objs : TJwObjectTypeArray; out Index : Integer) : Boolean; overload;

{<B>JwUNIMPLEMENTED_DEBUG</B> raises exception EJwsclUnimplemented if compiler directive DEBUG
was used to compile the source}
procedure JwUNIMPLEMENTED_DEBUG;

{<B>JwUNIMPLEMENTED</B> raises exception EJwsclUnimplemented}
procedure JwUNIMPLEMENTED;

{<B>JwCheckInitKnownSid</B> checks for the call of JwInitWellKnownSIDs
and raises EJwsclInitWellKnownException if it was not called.

There is a more detailed procedure JwsclKnownSid.JwCheckInitKnownSid
which can be used to check for special well known SID variables from
unit JwsclKnownSid.

@param MethodName defines the name of the method this parameter belongs to
@param ClassName defines the name of the class the method belongs to. Can be
  empty if the method does not belong to a class
@param FileName defines the source file of the call to this procedure.
raises
 EJwsclInitWellKnownException This exception will be raised if JwInitWellKnownSIDs
  was not called.
}

procedure JwCheckInitKnownSid(const MethodName, ClassName, FileName : TJwString);


{$IFDEF JW_TYPEINFO}
{<B>GetUnitName</B> returns the name of unit where the given objects was defined in source code.
}
function GetUnitName(argObject: TObject): AnsiString;
{$ENDIF JW_TYPEINFO}

{<B>JwSetThreadName</B> names a thread. A debugger can use this name to display a human readably
identifier for a thread.
<B>JwSetThreadName</B> must be called without using parameter ThreadID
 as a precondition to use JwGetThreadName .

@param Name defines an name for the thread. This Name is converted to ansicode internally.
@param ThreadID defines which thread is named. A value of Cardinal(-1)  uses
  the current thread 
}
procedure JwSetThreadName(const Name: TJwString; const ThreadID : Cardinal = Cardinal(-1));

{<b>JwFreeThreadName</b> frees the thread variable allocated by JwSetThreadName.
This procedure must be call at the end of thread to avoid a memory leak in
some situation.

Remarks
This procedure is not always necessary. Although it is told otherwise, it seems that
Delphi sometimes cleans up the memory. However if you use TJwThread the
cleanup is done automatically.
}
procedure JwFreeThreadName;

{<B>JwGetThreadName</B> returns the name of a thread set by JwSetThreadName.
 This function only returns the name of the current thread. It cannot be used
 with different threads than the current one.

<B>Precondition:</B> 
 JwSetThreadName must be called with a value of Cardinal(-1) for parameter ThreadID.
}
function JwGetThreadName : WideString;

{<B>IsHandleValid</B> returns true if Handle is neither zero (0) nor INVALID_HANDLE_VALUE; otherwise false.}
function JwIsHandleValid(const Handle : THandle) : Boolean;

{<B>JwCheckBitMask</B> Checks if (Bitmask and Check) = Check}
function JwCheckBitMask(const Bitmask: Integer; const Check: Integer): Boolean; 

{<B>JwMsgWaitForMultipleObjects</B> encapsulates MsgWaitForMultipleObjects using an open array
parameter. The function should be used to make sure that window messages are processed. In this way
windows are responsible. This function returns if such a message is received.

@param Handles This parameter receives an array of Handles. You can be either create an array type of THandle
  or use set operators "[" and "]" containing a comma separated list of handle variables.
@param bWaitAll Set to true to let the function wait for all given handles until it returns; otherwise it returns
  as soon as at least one handle state is signaled.
@param dwMilliseconds Defines a timeout interval that exits the function when elapsed. Set to constant INFINITE (-1) 
  to ignore timeouts.
@param dwWakeMask See MsgWaitForMultipleObjects (http://msdn.microsoft.com/en-us/library/ms684242.aspx) in MSDN for more information.

@return Returns a status code. See MsgWaitForMultipleObjects (http://msdn.microsoft.com/en-us/library/ms684242.aspx) in MSDN for more information.
}
function JwMsgWaitForMultipleObjects(const Handles: array of THandle; bWaitAll: LongBool;
           dwMilliseconds: DWord; dwWakeMask: DWord): DWord; overload;

function JwMsgWaitForMultipleObjects(const Handles: TJwHandles; bWaitAll: LongBool;
           dwMilliseconds: DWord; dwWakeMask: DWord): DWord; overload;

{<B>JwWaitForMultipleObjects</B> encapsulates WaitForMultipleObjects using an open array
parameter.

@param Handles This parameter receives an array of Handles. You can be either create an array type of THandle
  or use set operators "[" and "]" containing a comma separated list of handle variables.
@param bWaitAll Set to true to let the function wait for all given handles until it returns; otherwise it returns
  as soon as at least one handle state is signaled.
@param dwMilliseconds Defines a timeout interval that exits the function when elapsed. Set to constant INFINITE (-1) 
  to ignore timeouts.

@return Returns a status code. See WaitForMultipleObjects (http://msdn.microsoft.com/en-us/library/aa931008.aspx) in MSDN for more information.
}
function JwWaitForMultipleObjects(const Handles: array of THandle; bWaitAll: LongBool;
           dwMilliseconds: DWord): DWord; overload;



function JwWaitForMultipleObjects(const Handles: TJwHandles; bWaitAll: LongBool;
           dwMilliseconds: DWord): DWord; overload;

function JwHandlesArray(const Handles: array of THandle) : TJwHandles;
procedure JwAddHandleToArray(var TargetHandles: TJwHandles; const Handles: array of THandle);

{<B>JwCreateWaitableTimer</B> creates a waitable timer handle.

@param TimeOut defines a signal interval in miliseconds (1sec = 1000msec)
@param SecurityAttributes defines security attributes for the timer. The class type
  must be TJwSecurityDescriptor or a derivation.
@return Returns a handle to the new timer object. Must be closed by CloseHandle.

raise
  EJwsclClassTypeMismatch: If parameter SecurityAttributes is not nil and also
    not of the type TJwSecurityDescriptor, an exception EJwsclClassTypeMismatch is raised.
  EOSError: If any winapi calls fail, an exception EJwsclWinCallFailedException is raised.
}
function JwCreateWaitableTimer(
      const TimeOut: DWORD;
      const SecurityAttributes : TObject = nil) : THandle; overload;

{<B>JwCreateWaitableTimerAbs</B> is not implemented yet.}
function JwCreateWaitableTimerAbs(
      const DateTime : TDateTime;
      const SecurityAttributes : TObject = nil) : THandle; overload;


{<B>JwCreateWaitableTimer</B> creates a waitable timer handle.

For more information about the undocumented parameters, see MSDN
  http://msdn.microsoft.com/en-us/library/ms686289(VS.85).aspx

This function does not support absolute time like the original winapi function.
It means that you cannot specify a point in time.

@param TimeOut defines a signal interval in miliseconds (1sec = 1000msec)
@param Name defines a name for the timer. If Name is empty, the timer will be unnamed.
@param SecurityAttributes defines security attributes for the timer. The class type
  must be TJwSecurityDescriptor or a derivation.

@return Returns a handle to the new timer object. Must be closed by CloseHandle.

raise
  EJwsclClassTypeMismatch: If parameter SecurityAttributes is not nil and also
    not of the type TJwSecurityDescriptor, an exception EJwsclClassTypeMismatch is raised.
  EOSError: If any winapi calls fail, an exception EJwsclWinCallFailedException is raised.
}
function JwCreateWaitableTimer(
      const TimeOut: DWORD;
      const ManualReset : Boolean;
      const Name : TJwString;
      const Period : Integer = 0;
      const CompletitionRoutine : PTIMERAPCROUTINE = nil;
      const CompletitionRoutineArgs : Pointer = nil;
      const SuspendResume : Boolean = false;
      const SecurityAttributes : TObject = nil) : THandle; overload;

{<B>JwCreateWaitableTimerAbs</B> is not implemented yet.
It is intended to support absolute time.
}
function JwCreateWaitableTimerAbs(
      const DateTime : TDateTime;
      const ManualReset : Boolean;
      const Name : TJwString;
      const Period : Integer = 0;
      const CompletitionRoutine : PTIMERAPCROUTINE = nil;
      const CompletitionRoutineArgs : Pointer = nil;
      const SuspendResume : Boolean = false;
      const SecurityAttributes : TObject = nil) : THandle; overload;


{<B>JwCompareFileHash</B> creates a hash from a given file and compares it to
an already calculated hash.

@param FilePath Defines a path to a file that is used to calculate a hash.
@param OriginalHash Defines a hash that is compared to the hash of the file.
@return Returns true if the hash of file and the given one is identical; otherwise false. If
  the parameter OriginalHash has a nil pointer the return value is also false.

raise
  EJwsclFileMappingException see TJwFileStreamEx.Create for more information.
  EJwsclSecurityException There can be other exceptions raised by TJwHash.Create, TJwHash.HashData
    and TJwHash.RetrieveHash.
  
remarks
  * This function uses TJwFileStreamEx for getting hash in the fastest way possible.
  * This function uses SHA hashing.

}
function JwCompareFileHash(const FilePath : WideString;
  const OriginalHash : TJwFileHashData) : Boolean;
  
{<B>JwCreateFileHash</B> creates a hash from a given file and returns 
the hash. 

@param FilePath Defines a path to a file that is used to calculate a hash.
@return The return value is a record that holds the hash data. The returned pointer member "hash" in
TJwFileHashData must be freed by TJwHash.FreeBuffer (unit JwsclCryptProvider.pas).

raise
  EJwsclFileMappingException see TJwFileStreamEx.Create for more information.
  EJwsclSecurityException There can be other exceptions raised by TJwHash.Create, TJwHash.HashData
    and TJwHash.RetrieveHash.
	
remarks
  * This function uses TJwFileStreamEx for getting hash in the fastest way possible.
  * This function uses SHA hashing.

}
function JwCreateFileHash(const FilePath : WideString) : TJwFileHashData;

{<B>JwSaveHashToRegistry</B> saves a hash record (TJwFileHashData) to registry.

@param Hive Defines a registry hive like HKEY_LOCAL_MACHINE.
@param Key Defines a registry path to a key like "Software\JEDI".
@param HashName Defines the registry value name that receives the hash data.
@param SizeName Defines the registry value name that receives the size of hash data.
@param FileHashData receives the hashdata.

raise
  Exception This procedure may raise exception coming from TRegistry methods.

}
procedure JwSaveHashToRegistry(const Hive: Cardinal;
   const Key, HashName, SizeName : String;
   const FileHashData : TJwFileHashData);

{<B>JwLoadHashFromRegistry</B> loads a hash record (TJwFileHashData) to registry
previously saved by JwSaveHashToRegistry.

@param Hive Defines a registry hive like HKEY_LOCAL_MACHINE.
@param Key Defines a registry path to a key like "Software\JEDI".
@param HashName Defines the registry value name that receives the hash data.
@param SizeName Defines the registry value name that receives the size of hash data.
@return The return value is a record that holds the hash data. The returned pointer member "hash" in
TJwFileHashData must be freed by TJwHash.FreeBuffer (unit JwsclCryptProvider.pas).

raise
  ERegistryException If the given key was not found.
  Exception This procedure may raise exception coming from TRegistry methods.


remarks
  The procedure has some characteristics :
  * It does not check for a correct hash value. However the key type of "HashName" must be binary though.
  * It only returns a valid structure if the value from key "SizeName" is between 1 than 1023 bytes (1 <= SizeName <= 1023)

}
function JwLoadHashFromRegistry(const Hive: Cardinal;
   const Key, HashName, SizeName : String) : TJwFileHashData;


function JwAccessMaskToBits(const Access : DWORD) : TJwString;

{<B>JwCheckVISTACompilerSwitch</B> raises an exception EJwsclVistaFeaturesDisabled
if the current code isn't compiled with the VISTA compiler directive.
Otherwise it does nothing.
}
procedure JwCheckVISTACompilerSwitch(MethodName, ClassName, FileName : TJwString);

{JwCreateClassHash calculates an integer hash value from a memory structure.}
function JwCreateClassHash(const Data : Pointer; const Size : Cardinal) : Integer;

{JwBeginCreateHash begins a hash calculation and returns an hash handle
that is used with other hash functions like:
JwStringHash, JwIntegerHash, JwDataHash, JwObjectHash

@return
  The untyped returned value must be freed by JwEndCreateHash.
}
function JwBeginCreateHash : Pointer;

{JwStringHash hashes a string and adds it to the given hash handle.}
procedure JwStringHash(const Hash: Pointer; const S : string);

{JwIntegerHash hashes an integer and adds it to the given hash handle.}
procedure JwIntegerHash(const Hash: Pointer; const I : Integer);

{JwDataHash hashes a data structure and adds it to the given hash handle.}
procedure JwDataHash(const Hash: Pointer; const P : Pointer; const Size : Cardinal);

{JwObjectHash hashes an object and adds it to the given hash handle.}
procedure JwObjectHash(const Hash: Pointer; const Obj : IJwBase);

{JwEndCreateHash frees a hash value created by JwBeginCreateHash
and returns the hash value over all calculated hashes.

@param Hash Receives a hash handle created by JwBeginCreateHash and frees it. The parameter value
  will be nil afterwards.
@return
  Returns a hash value.
}
function JwEndCreateHash(var Hash : Pointer) : Integer;

{JwCreateToString creates a comma separated string (compatible to TStringList.CommaText)
with names and values. This string is used in toString() methods of JWSCL
classes for property output.

@param Values Contains a single value or tuples for the output.
  Each value in this parameter can be an Integer, Boolean, Int64, Ansi-&WideString and ShortString;
  other types are output to '??'. All entries must have a second entry (a so called tuple)


Remarks
  The following example shows the usage
  <code lang="Delphi">
   result := JwCreateToString(
    ['aString','', //simple string
     'Hash',123, //name='Hash', value=GetHashCode
  </code>
  The output looks like:
  <pre>aString,Hash=123</pre>
}
function JwCreateToString(const Values : array of const) : String;

{ <b>JwZeroPassword</b> erases securely a UNICODE or ANSICODE string.
  
  
  

  Parameters
  S :  Defines the string to be erased securely. The returned string will have a
       length of 0.
  
  
  
  Remarks
  JwZeroPassword writes random data over all characters of the string. In a second
  step it zeroes all characters and sets the length of the string to zero (0).
  
  This function works with UNICODE and ANSICODE.
  
  
                                                                                   }
procedure JwZeroPassword(var S : TJwString);

implementation
uses SysUtils, Registry, Math, D5Impl, JwsclToken, JwsclKnownSid, JwsclDescriptor, JwsclAcl,
     JwsclSecureObjects, JwsclMapping, JwsclStreams, JwsclCryptProvider,
     JwsclConstants
{$IFDEF JW_TYPEINFO}
     ,TypInfo
{$ENDIF JW_TYPEINFO}
      ;


type
   TLStr = array[0..1] of AnsiChar;

procedure JwZeroPassword(var S : TJwString);
{$IFOPT O+}
 {$DEFINE OPT_ON}
{$ELSE}
 {$UNDEF OPT_ON}
{$ENDIF}
{$O-}
var i : Integer;
      Data : ^TLStr;
      len : Integer;
begin
  if Length(S) = 0 then
    exit;

  randomize;
  len := Length(S) * sizeof(S[1]); //also get widechar size
  Data := Pointer(@S[1]); //get first char

  for i := 0 to len-1 do //goes through all chars (even both widechar)
  begin
    {$IFOPT O+}
     Error //: this procedure must not be compiled with optimization
    {$ENDIF}
    Data^[i] := AnsiChar(Random(255));
  end;
  ZeroMemory(Data, len);
  SetLength(S, 0);
end;
{$IFDEF OPT_ON}
 {$O+}
{$ENDIF}

function JwCreateToString(const Values : array of const) : String;
  function GetValue(I : Integer; const Values : array of const) : String;
  const B : Array[boolean] of ShortString = ('true','false');
  begin    
    case Values[i].VType of
      vtInteger : result := IntToStr(Values[i].VInteger);
      vtBoolean : result := String(B[Values[i].VBoolean]);
      vtInt64 : result := IntToStr(Values[i].VInt64^);

      vtAnsiString : result := String(AnsiString(Values[i].VAnsiString));
      vtWideString : result := WideString(Values[i].VWideString);
      vtString : result := String(Values[i].VString^);
{$IFDEF DELPHI2009_UP}
      vtUnicodeString : result := UnicodeString(Values[i].VUnicodeString);
{$ENDIF DELPHI2009_UP}
    else
      Result := '<error>';
    end;
  end;

var
  i : Integer;
  List : TStringList;
begin
  List := TStringList.Create;

  result := '';
  i := low(Values);
  {i = Vl}
  while i < High(Values) do
  begin
    {i < Vh}
    if Values[i+1].VString = nil then
      result := GetValue(i, Values)
    else
      result := GetValue(i, Values)+'='+GetValue(i+1, Values);
    List.Add(result);
    Inc(i,2);
    {i = i + 2}
  end;
  {i = Vh}

  result := List.CommaText;
  List.Free;
end;


function JwCreateClassHash(const Data : Pointer; const Size : Cardinal) : Integer;
{var
  i: Integer;
  P : PByte;
  Hash : TJwHash;
begin
  P := Data;
  result := 0;

  for i := 0 to Size-1 do
  begin
    result := result shl 1;
    result := result xor P^;

    Inc(P);
  end;
end; }
var
  i, x: Integer;
  P : PByte;
begin
  Result := 0;
  P := Data;

  //http://www.scalabium.com/faq/dct0136.htm
  for i := 1 to Size do
  begin
    Result := (Result shl 4) + Ord(P^);
    x := Result and $F0000000;
    if (x <> 0) then
      Result := Result xor (x shr 24);
    Result := Result and (not x);

    Inc(P);
  end;
end;


function JwAccessMaskToBits(const Access : DWORD) : TJwString;
var i : byte;
begin
  result := '';
  for I := 1 to (sizeof(Access)*8) do
  begin
    if (i in  [17, 25,26,29]) then
      result := ' ' + result;


    if Access and Powers2[I] = Powers2[I] then
      result := '1' + result
    else
      result := '0' + result;
  end;
end;


function JwBeginCreateHash : Pointer;
begin
  result := TMemoryStream.Create;
end;

procedure JwStringHash(const Hash: Pointer; const S : string);
begin
  TMemoryStream(Hash).Write(S[1], Length(S) * SizeOf(S[1]));
end;

procedure JwIntegerHash(const Hash: Pointer; const I : Integer);
begin
  TMemoryStream(Hash).Write(I, SizeOf(I));
end;

procedure JwDataHash(const Hash: Pointer; const P : Pointer; const Size : Cardinal);
begin
  TMemoryStream(Hash).Write(P^, Size);
end;

procedure JwObjectHash(const Hash: Pointer; const Obj : IJwBase);
begin
  if Assigned(Obj) then
    JwIntegerHash(Hash, Obj.GetHashCode);
end;

function JwEndCreateHash(var Hash : Pointer) : Integer;
begin
  result := JwCreateClassHash(TMemoryStream(Hash).Memory, TMemoryStream(Hash).Size);

  TMemoryStream(Hash).Free;
  Hash := nil;
end;




procedure JwCheckVISTACompilerSwitch(MethodName, ClassName, FileName : TJwString);
begin
{$IFNDEF VISTA}
  raise EJwsclVistaFeaturesDisabled.CreateFmtEx(
      RsVistaFeaturesDisabled, MethodName,
      ClassName, FileName, 0, False, []);
{$ENDIF VISTA}      
end;


function JwCompareFileHash(
  const FilePath : WideString;
  const OriginalHash : TJwFileHashData) : Boolean;
var
  Stream : TJwFileStreamEx;
  //M : TMemoryStream;
  Size : Cardinal;
  fAppHash : TJwHash;
  NewHash : TJwFileHashData;
begin
  Stream := TJwFileStreamEx.Create(FilePath, fmOpenRead);
  try
    if Stream.Size > high(Size) then
      Size := high(Size)-1  //big file huh?
    else
      Size := Stream.Size;

    fAppHash := TJwHash.Create(haSHA);
    try
      fAppHash.HashData(Stream.Memory,Size);

      NewHash.Hash := fAppHash.RetrieveHash(NewHash.Size);

      try
        result := (OriginalHash.Size = NewHash.Size) and
           (NewHash.Hash <> nil) and (OriginalHash.Hash <> nil) and
           (CompareMem(OriginalHash.Hash,NewHash.Hash, OriginalHash.Size));
      finally
        TJwHash.FreeBuffer(NewHash.Hash);
      end;
    finally
      fAppHash.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function JwCreateFileHash(const FilePath : WideString) : TJwFileHashData;
var
  Stream : TJwFileStreamEx;
  Hash : TJwHash;
  Size : Cardinal;
begin
  Stream := TJwFileStreamEx.Create(FilePath, fmOpenRead);
  try
    if Stream.Size > high(Size) then
      Size := high(Size)-1  //big file huh?
    else
      Size := Stream.Size;

    Hash := TJwHash.Create(haSHA);
    try
      Hash.HashData(Stream.Memory,Size);

      result.Hash := Hash.RetrieveHash(result.Size);
    finally
      Hash.Free;
    end;
  finally
    Stream.Free;
  end;
end;


function JwLoadHashFromRegistry(const Hive: Cardinal;
   const Key, HashName, SizeName : String) : TJwFileHashData;
var
  Reg: TRegistry;
begin
  result.Size := 0;
  result.Hash := nil;

  Reg := TRegistry.Create(KEY_QUERY_VALUE or KEY_READ);
  try
    Reg.RootKey := Hive;
    if Reg.OpenKey(Key, false)
   //don't check for these value since we need an exception to notify the caller
     { and Reg.ValueExists(SizeName)
      and Reg.ValueExists(HashName)}
      then
    begin
      try
        result.Size := Reg.ReadInteger(SizeName);
        if (result.Size > 0) and (result.Size < 1024) then
        begin
      // TJwHash uses GetMem; the returned record
      // TJwFileHashData is freed by TJwHash.FreeBuffer
      // Change this memory manager when TJwHash is changed.
          GetMem(result.Hash, result.Size);
		  
          ZeroMemory(result.Hash, result.Size);
          try
            result.Size := Reg.ReadBinaryData(HashName,result.Hash^,result.Size);
          except
            FreeMem(Result.Hash);
          end;
        end;
      finally
        Reg.CloseKey;
      end;
    end
    else
      raise ERegistryException.CreateFmt('Key %s not found/accessible.',[Key]);
  finally
    Reg.Free;
  end;
end;

procedure JwSaveHashToRegistry(const Hive: Cardinal;
   const Key, HashName, SizeName : String;
   const FileHashData : TJwFileHashData);
var
  Reg: TRegistry;
begin
  Reg:=TRegistry.Create(KEY_SET_VALUE or KEY_CREATE_SUB_KEY);
  try
    Reg.RootKey := Hive;
    if Reg.OpenKey(Key, true) then
    try
      Reg.WriteInteger(SizeName, FileHashData.Size);
      Reg.WriteBinaryData(HashName,FileHashData.Hash^,FileHashData.Size);
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

{$IFDEF JW_TYPEINFO}
function GetUnitName(argObject: TObject): AnsiString;
var
  ptrTypeData: PTypeData;
begin
  if (argObject.ClassInfo <> nil) then
  begin
    ptrTypeData := GetTypeData(argObject.ClassInfo);
    Result := ptrTypeData.UnitName;
  end;
end;
{$ENDIF JW_TYPEINFO}


function JwIsHandleValid(const Handle : THandle) : Boolean;
begin
  result := (Handle <> 0) and (Handle <> INVALID_HANDLE_VALUE);
end;

function JwCheckBitMask(const Bitmask: Integer; const Check: Integer): Boolean;
begin
  Result := BitMask and Check = Check;
end;

threadvar
  InternalThreadName : WideString;

type
  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PAnsiChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;


procedure TJwThread.SetName(const Name: TJwString);
begin
  FName := Name;
  JwSetThreadName(Name, ThreadID);
end;

constructor TJwThread.Create(const CreateSuspended: Boolean; const Name: TJwString);
begin
  inherited Create(CreateSuspended);

  SetName(Name);

  FTerminatedEvent := CreateEvent(nil, False, False, nil);
end;

destructor TJwThread.Destroy;
begin
  JwFreeThreadName;
  CloseHandle(FTerminatedEvent);
  inherited;
end;


procedure TJwThread.Execute;
begin
  SetName(Name);
end;

function TJwThread.GetReturnValue: Integer;
begin
  result := ReturnValue;
end;

function JwGetThreadName : WideString;
begin
  result := InternalThreadName;
end;

procedure JwFreeThreadName;
begin
  InternalThreadName := '';
end;

//source http://msdn2.microsoft.com/en-us/library/xcb2z8hs(vs.71).aspx
procedure JwSetThreadName(const Name: TJwString; const ThreadID : Cardinal = Cardinal(-1));
{$IFDEF MSWINDOWS}
var
  ThreadNameInfo: TThreadNameInfo;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ThreadNameInfo.FType := $1000;
  //CW : imho, only ansicode is supported. So we cast it here.
  ThreadNameInfo.FName := PAnsiChar(AnsiString(Name));
  if (ThreadID = Cardinal(-1)) or (ThreadID = GetCurrentThreadID) then
    InternalThreadName := WideString(Name);

  ThreadNameInfo.FThreadID := ThreadID; //$FFFFFFFF;
  ThreadNameInfo.FFlags := 0;

  try
    RaiseException( $406D1388, 0, SizeOf(ThreadNameInfo) div SizeOf(LongWord),
      @ThreadNameInfo );
  except
  end;
{$ENDIF}
end;




procedure JwCheckInitKnownSid(const MethodName, ClassName, FileName : TJwString);
begin
  if not JwInitWellKnownSidStatus then
    raise EJwsclInitWellKnownException.CreateFmtEx(
      RsInitWellKnownNotCalled,
      MethodName, ClassName, FileName, 0, false, []);
end;


function JwCheckArray(const Objs : TJwObjectTypeArray; out Index : Integer) : Boolean;
var
    LastLevel : Cardinal;
begin
  Index := 0;

  result := Assigned(Objs);
  if not result then exit;

  result := Length(Objs) > 0;
  if not result then exit;

  result := Objs[0].Level = 0;
  if not result then exit;

  LastLevel := 0;
  if Length(Objs) > 1 then
  begin
    Index := 1;
    while Index <= high(Objs) do
    begin
      result := (Objs[Index].Level > 0) and (Objs[Index].Level < 5);
      if not result then exit;

      if Objs[Index].Level > LastLevel then
      begin
        result := (Objs[Index].Level - LastLevel) = 1;
        if not result then exit;
      end;

      if Objs[Index].ObjectType = nil then
        exit;

      LastLevel := Objs[Index].Level;
      Inc(Index);
    end;
  end;
end;

function JwCheckArray(const Objs : TJwObjectTypeArray) : Boolean;
var Index : Integer;
begin
  result := JwCheckArray(Objs, Index);
end;

procedure JwUNIMPLEMENTED;
begin
  raise EJwsclUnimplemented.CreateFmtEx(
    'This function is not implemented.',
    '', '', '', 0, false, []);
end;

procedure JwUNIMPLEMENTED_DEBUG;
begin
{$IFNDEF DEBUG}
  raise EJwsclUnimplemented.CreateFmtEx(
    'This function is not implemented.',
    '', '', '', 0, false, []);
{$ENDIF DEBUG}    
end;

procedure LocalizeMapping(var MappingRecord : array of TJwRightsMapping;
  const StartStringId : Cardinal;
  const PrimaryLanguageId, SubLanguageId : Word;
  const UseDefaultOnError : Boolean = true;
  ModuleInstance : HINST = 0
  );
var ArrayHi,ArrayLo : Cardinal;
    LHi : Cardinal;
    i,
    Id : Integer;
    bSuccess : Boolean;
    S : TJwString;

  function GetNumber(S : TJwString) : Cardinal;
  var i : Integer;
  begin
    for i := 1 to Length(S) do
    begin
{$IFDEF UNICODE}
//in this way Delphi 2009 does not generate a warning
      if not ((S[i] >= '0') and (S[i] <= '9')) then
{$ELSE}
      if not (S[i] in [TJwChar('0')..TJwChar('9')]) then
{$ENDIF UNICODE}
      begin
        SetLength(S, i-1);
        break;
      end;
    end;
    result := StrToIntDef(S,0);
  end;
begin
  ArrayHi := high(MappingRecord);
  ArrayLo := low(MappingRecord);

  if ModuleInstance = 0 then
    ModuleInstance := HInstance;


  try
    S := LoadLocalizedString(StartStringId, PrimaryLanguageId, SubLanguageId, ModuleInstance);
  except
     on E : EJwsclOSError do
       raise EJwsclResourceInitFailed.CreateFmtEx(
               RsResourceInitFailed,
               'LocalizeMapping', '', RsUNUtils, 0, true,
                [StartStringID]);
  end;

  {Load last string number and compare it to the highest one
   of the given array. If unequal we have a problem.}
  LHi := GetNumber(S);
  if (LHi < ArrayHi+1) then
    raise EJwsclResourceUnequalCount.CreateFmtEx(
            RsResourceUnequalCount,
            'LocalizeMapping', '', RsUNUtils, 0, false, [LHi,StartStringID,ArrayHi]);


  for i := ArrayLo to ArrayHi do
  begin
    bSuccess := true;
    
    if MappingRecord[i].StringId > 0 then
    begin
      Id := MappingRecord[i].StringId;
      Inc(Id, StartStringID);
    end
    else
    if MappingRecord[i].StringId < 0 then
      Id := (-MappingRecord[i].StringId)
    else
    begin
      Id := i;
      Inc(Id);
      Inc(Id, StartStringID);
    end;


    try
      S := LoadLocalizedString(Id, PrimaryLanguageId, SubLanguageId, ModuleInstance);
    except
      on E : EJwsclOSError do
      begin
        if UseDefaultOnError then
        begin
          Id := i;
          Inc(Id);
          Inc(Id, StartStringID);
        end;

        try
          S := LoadLocalizedString(Id, PrimaryLanguageId, SubLanguageId, ModuleInstance);
        except
          on E : EJwsclOSError do
          begin
            if UseDefaultOnError then
              bSuccess := false
            else
              raise EJwsclResourceNotFound.CreateFmtEx(
                RsResourceNotFound,
                'LocalizeMapping', '', RsUNUtils, 0, true, [Id]);
          end;
        end; //try except
      end;
    end; //try except

    if bSuccess then
      MappingRecord[i].Name := S;
  end;
end;

function JwMsgWaitForMultipleObjects(const Handles: array of THandle; bWaitAll: LongBool;
           dwMilliseconds: DWord; dwWakeMask: DWord): DWord;
begin
  Result := MsgWaitForMultipleObjects(Length(Handles), @Handles[0], bWaitAll, dwMilliseconds, dwWakeMask);
end;

function JwMsgWaitForMultipleObjects(const Handles: TJwHandles; bWaitAll: LongBool;
           dwMilliseconds: DWord; dwWakeMask: DWord): DWord;
begin
  Result := MsgWaitForMultipleObjects(Length(Handles), @Handles[0], bWaitAll, dwMilliseconds, dwWakeMask);
end;

function JwWaitForMultipleObjects(const Handles: array of THandle; bWaitAll: LongBool;
           dwMilliseconds: DWord): DWord;
begin
  Result := WaitForMultipleObjects(Length(Handles), @Handles[0], bWaitAll, dwMilliseconds);
end;

function JwHandlesArray(const Handles: array of THandle) : TJwHandles;
var i : Integer;
begin
  if Length(Handles) = 0 then
    result := nil
  else
    SetLength(result, Length(Handles));
  for I := low(Handles) to High(Handles) do
  begin
    result[i] := Handles[i];
  end;
end;

procedure JwAddHandleToArray(var TargetHandles: TJwHandles; const Handles: array of THandle);
var i, len, StartI : Integer;
begin
  len := Length(TargetHandles);
  if len < 0 then
    len := 0;

  StartI := Len;
  if Length(Handles) > 0 then
    SetLength(TargetHandles, len+Length(Handles));
  for I := low(Handles) to High(Handles) do
  begin
    TargetHandles[i+StartI] := Handles[i];
  end;
end;


function JwWaitForMultipleObjects(const Handles: TJwHandles; bWaitAll: LongBool;
           dwMilliseconds: DWord): DWord;
begin
  Result := WaitForMultipleObjects(Length(Handles), @Handles[0], bWaitAll, dwMilliseconds);
end;


{$IFDEF FullDebugMode}
type
     PMemTuple = ^TMemTuple;
     TMemTuple = record
       GetMemPointer : Pointer;
       case MemType : Boolean of
         true : (LocalData : HLOCAL);              
         false: (GlobalData : HGLOBAL);
      end;
var InternalMemArray : TList {=nil};
{$ENDIF}


function JwLocalAllocMem(uFlags: UINT; uBytes: SIZE_T): HLOCAL;
{$IFDEF FullDebugMode}
var MemTuple : PMemTuple;
{$ENDIF FullDebugMode}
begin
  result := LocalAlloc(uFlags,uBytes);
{$IFDEF FullDebugMode}
  if result <> 0 then
  begin
    New(MemTuple);
    GetMem(MemTuple.GetMemPointer,uBytes);
    MemTuple.MemType := true;
    MemTuple.LocalData := result;
    InternalMemArray.Add(MemTuple);
  end;
{$ENDIF}
end;

function JwLocalFreeMem(var hMem: HLOCAL): HLOCAL;
{$IFDEF FullDebugMode}
  function Find : Integer;
  var i : Integer;
  begin
    result := -1;
    for I := 0 to InternalMemArray.Count - 1 do
    begin
      if PMemTuple(InternalMemArray[i]).MemType and
         (PMemTuple(InternalMemArray[i]).LocalData = hMem) then
      begin
        result := i;
        exit;
      end;
    end;
  end;
{$ENDIF}

{$IFDEF FullDebugMode}
var Index : Integer;
{$ENDIF FullDebugMode}

begin
{$IFDEF FullDebugMode}
  result := 0;
  if LocalLock(hMem) <> nil then
  begin
    Index := Find;
    if Index < 0 then
    begin
      LocalUnlock(hMem);
      raise EInvalidPointer.Create(RsInvalidLocalPointer);
    end;

    FreeMem(PMemTuple(InternalMemArray[Index]).GetMemPointer);
    FreeMem(PMemTuple(InternalMemArray[Index]));
    InternalMemArray.Delete(Index);

    LocalUnlock(hMem);
    result := LocalFree(hMem);
  end;
{$ELSE}
  result := LocalFree(hMem);
{$ENDIF FullDebugMode}
  hMem := 0;
end;




function JwGlobalAllocMem(uFlags: UINT; uBytes: SIZE_T): HGLOBAL;
{$IFDEF FullDebugMode}
var MemTuple : PMemTuple;
{$ENDIF FullDebugMode}
begin
  result := GlobalAlloc(uFlags,uBytes);
{$IFDEF FullDebugMode}
  if result <> 0 then
  begin
    New(MemTuple);
    GetMem(MemTuple.GetMemPointer,uBytes);
    MemTuple.MemType := false;
    MemTuple.GlobalData := result;
    InternalMemArray.Add(MemTuple);
  end;
{$ENDIF FullDebugMode}
end;

function JwGlobalFreeMem(var hMem: HGLOBAL): HGLOBAL;
{$IFDEF FullDebugMode}
  function Find : Integer;
  var i : Integer;
  begin
    result := -1;
    for I := 0 to InternalMemArray.Count - 1 do
    begin
      if not PMemTuple(InternalMemArray[i]).MemType and
         (PMemTuple(InternalMemArray[i]).GlobalData = hMem) then
      begin
        result := i;
        exit;
      end;
    end;
  end;
{$ENDIF FullDebugMode}

{$IFDEF FullDebugMode}
var Index : Integer;
{$ENDIF FullDebugMode}
begin
{$IFDEF FullDebugMode}
  result := 0;
  if GlobalLock(hMem) <> nil then
  begin
    Index := Find;
    if Index < 0 then
    begin
      GlobalUnlock(hMem);
      raise EInvalidPointer.Create(RsInvalidGlobalPointer);
    end;

    FreeMem(PMemTuple(InternalMemArray[Index]).GetMemPointer);
    FreeMem(PMemTuple(InternalMemArray[Index]));
    InternalMemArray.Delete(Index);

    GlobalUnlock(hMem);                
    result := GlobalFree(hMem);
  end;
{$ELSE}
  result := GlobalFree(hMem);          
{$ENDIF FullDebugMode}
  hMem := 0;
end;


procedure JwGlobalFreeAndNil(var hMem: HGLOBAL);
begin
  if hMem <> 0 then
    GlobalFree(hMem);
  hMem := 0;
end;


{$IFDEF FullDebugMode}
procedure DeleteInternalMemArray;
var i : Integer;
begin
  //we do not attempt to free the remaining TMemTuple.GetMemPointer blocks
  //instead we only remove PMemTuple memory
  for i := 0 to InternalMemArray.Count-1 do
  begin
    FreeMem(PMemTuple(InternalMemArray[i]));
    InternalMemArray[i] := nil;
  end;
  FreeAndNil(InternalMemArray);
end;
{$ENDIF}


{var S : TJwString;
    SA : TResourceTStringArray;
    Indexes : TResourceIndexArray;
    i : Integer;     }



{ TJwIntTupleList }

type
  PIntTuple = ^TIntTuple;
  TIntTuple = record
    Index : DWORD;
    Value : Pointer;
  end;

procedure TJwIntTupleList.Add(Index : DWORD; Value: Pointer);
var P : PIntTuple;
begin
  new(P);
  P^.Index := Index;
  P^.Value := Value;
  fList.Add(P);
end;

constructor TJwIntTupleList.Create;
begin
  fList := TList.Create;
end;


procedure TJwIntTupleList.Clear;
var i : Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    Dispose(PIntTuple(fList[i]));
  end;
  fList.Clear;
end;

procedure TJwIntTupleList.DeleteIndex(Index: DWORD);
var i : Integer;
begin
  for i := 0 to fList.Count - 1 do
  begin
    if PIntTuple(fList[i])^.Index = Index then
    begin
      dispose(PIntTuple(fList[i]));
      fList.Delete(i);
      exit;
    end;
  end;

  raise ERangeError.CreateFmt('Value %d not found',[Index]);
end;


destructor TJwIntTupleList.Destroy;
begin
  Clear;
  FreeAndNil(fList);
  inherited;
end;

function TJwIntTupleList.GetCount: Cardinal;
begin
  result := fList.Count;
end;

function TJwIntTupleList.GetItem(Index: DWORD): Pointer;
var i : Integer;
begin
  for i := 0 to fList.Count - 1 do
  begin
    if PIntTuple(fList[i])^.Index = Index then
    begin
      result := PIntTuple(fList[i])^.Value;
      exit;
    end;
  end;

  raise ERangeError.CreateFmt('Value %d not found',[Index]);
end;

procedure TJwIntTupleList.SetItem(Index : DWORD; Value: Pointer);
var i : Integer;
begin
  for i := 0 to fList.Count - 1 do
  begin
    if PIntTuple(fList[i])^.Index = Index then
    begin
      PIntTuple(fList[i])^.Value := Value;
      exit;
    end;
  end;

  raise ERangeError.CreateFmt('Value %d not found',[Index]);
end;

function JwCreateWaitableTimer(
      const TimeOut: DWORD;
      const SecurityAttributes : TObject = nil) : THandle;
begin
  result := JwCreateWaitableTimer(TimeOut, false, '',0,nil,nil,false,SecurityAttributes);
end;

function JwCreateWaitableTimerAbs(
      const DateTime : TDateTime;
      const SecurityAttributes : TObject = nil) : THandle; overload;
begin
  result := JwCreateWaitableTimerAbs(DateTime, false, '',0,nil,nil,false,SecurityAttributes);
end;





function JwCreateWaitableTimer(
      const TimeOut: DWORD;
      const ManualReset : Boolean;
      const Name : TJwString;
      const Period : Integer = 0;
      const CompletitionRoutine : PTIMERAPCROUTINE = nil;
      const CompletitionRoutineArgs : Pointer = nil;
      const SuspendResume : Boolean = false;
      const SecurityAttributes : TObject = nil) : THandle;
var
  TimeOutInt64 : LARGE_INTEGER;
  SA : PSecurityAttributes;
  pName : TJwPChar;
begin
  JwRaiseOnClassTypeMisMatch(SecurityAttributes, TJwSecurityDescriptor,
    'JwCreateWaitableTimer','',RsUNUtils);
  try
    SA := nil;
    if Assigned(SecurityAttributes) then
      SA := TJwSecurityDescriptor(SecurityAttributes).Create_SA();

    if Length(Name) > 0 then
      pName := TJwPchar(Name)
    else
      pName := nil;

    Result := {$IFDEF UNICODE}CreateWaitableTimerW{$ELSE}CreateWaitableTimerA{$ENDIF}
      (LPSECURITY_ATTRIBUTES(SA), ManualReset, pName);
    if (Result = 0) or (Result = INVALID_HANDLE_VALUE) then
      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed,
         'JwCreateWaitableTimer', '', RsUNUtils, 0, True, ['CreateWaitableTimer']);
  finally
    if SA <> nil then
      TJwSecurityDescriptor.Free_SA(SA);
  end;

  ZeroMemory(@TimeOutInt64,sizeof(TimeOutInt64));
  TimeOutInt64.HighPart := -1;
  TimeOutInt64.LowPart := - TimeOut * 10000;


  if not SetWaitableTimer(Result, TimeOutInt64, Period, CompletitionRoutine, CompletitionRoutineArgs, SuspendResume) then
  begin
    CloseHandle(Result);
    raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed,
         'JwCreateWaitableTimer', '', RsUNUtils, 0, True, ['SetWaitableTimer']);
  end;
end;

function JwCreateWaitableTimerAbs(
      const DateTime : TDateTime;
      const ManualReset : Boolean;
      const Name : TJwString;
      const Period : Integer = 0;
      const CompletitionRoutine : PTIMERAPCROUTINE = nil;
      const CompletitionRoutineArgs : Pointer = nil;
      const SuspendResume : Boolean = false;
      const SecurityAttributes : TObject = nil) : THandle; overload;
begin
  raise EJwsclUnimplemented.Create('JwCreateWaitableTimer is not implemented.');
  (*
  // Declare our local variables.
HANDLE hTimer;
SYSTEMTIME st;
FILETIME ftLocal, ftUTC;
LARGE_INTEGER liUTC;

// Create an auto-reset timer.
hTimer = CreateWaitableTimer(NULL, FALSE, NULL);

// First signaling is at January 1, 2002, at 1:00 P.M. (local time).
st.wYear         = 2002; // Year
st.wMonth        = 1;    // January
st.wDayOfWeek    = 0;    // Ignored
st.wDay          = 1;    // The first of the month
st.wHour         = 13;   // 1PM
st.wMinute       = 0;    // 0 minutes into the hour
st.wSecond       = 0;    // 0 seconds into the minute
st.wMilliseconds = 0;    // 0 milliseconds into the second

SystemTimeToFileTime(&st, &ftLocal);

// Convert local time to UTC time.
LocalFileTimeToFileTime(&ftLocal, &ftUTC);
// Convert FILETIME to LARGE_INTEGER because of different alignment.
liUTC.LowPart  = ftUTC.dwLowDateTime;
liUTC.HighPart = ftUTC.dwHighDateTime;

// Set the timer.
SetWaitableTimer(hTimer, &liUTC, 6 * 60 * 60 * 1000,
   NULL, NULL, FALSE);

  *)
end;



function TJwThread.WaitWithTimeOut(const TimeOut: DWORD;
  const MsgLoop : Boolean = true) : LongWord;
var
  WaitResult: Cardinal;
  Msg: TMsg;
  hTimer : THandle;
begin
  if (TimeOut = 0) or (TimeOut = INFINITE) then
    result := WaitFor
  else
  if GetCurrentThreadID = MainThreadID then
  begin
    WaitResult := 0;

    hTimer := JwCreateWaitableTimer(TimeOut, true, '');
    try
      repeat
        { This prevents a potential deadlock if the background thread
          does a SendMessage to the foreground thread }
        if (MsgLoop) and (WaitResult = WAIT_OBJECT_0 + 2) then
          PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE);

        ResetEvent(hTimer);

        {Okay, we could just have used the builtin timeout support.
        But that's too easy
        }
        if MsgLoop then
          WaitResult := JwMsgWaitForMultipleObjects([Handle, {$IFDEF DELPHI7_UP}SyncEvent,{$ENDIF} hTimer], False, INFINITE, QS_SENDMESSAGE)
        else
          WaitResult := JwWaitForMultipleObjects([Handle, {$IFDEF DELPHI7_UP}SyncEvent,{$ENDIF} hTimer], False, INFINITE);


        CheckThreadError(WaitResult <> WAIT_FAILED);
        if WaitResult = WAIT_OBJECT_0 + 1 then
{$IFDEF DELPHI7_UP}
          CheckSynchronize;         //not supported in D5

        if WaitResult = WAIT_OBJECT_0 + 2 then
{$ENDIF DELPHI7_UP}
        begin
          result := WAIT_TIMEOUT;
          exit;
        end;
      until WaitResult = WAIT_OBJECT_0;
    finally
      if hTimer <> INVALID_HANDLE_VALUE then
        CloseHandle(hTimer);
    end;
  end
  else
    WaitForSingleObject(Handle, TimeOut);


  CheckThreadError(GetExitCodeThread(Handle, Result));
end;


initialization

  {
  S := LoadLocalizedString(50005, LANG_NEUTRAL, SUBLANG_NEUTRAL);
  S := LoadLocalizedString(50005, LANG_ENGLISH, SUBLANG_NEUTRAL, 0);
  S := LoadLocalizedString(50005, LANG_NEUTRAL, SUBLANG_SYS_DEFAULT);
  if s = '' then;
                       }
{  SetLength(Indexes,20);
  for i := 1 to 20 do
    Indexes[i-1] := 50000+i;
  SA := LoadLocalizedStringArray(Indexes,MAKELANGID(LANG_NEUTRAL,  SUBLANG_SYS_DEFAULT),0);
 }

{$IFDEF FullDebugMode}
  InternalMemArray := TList.Create;
{$ENDIF}

finalization
{$IFDEF FullDebugMode}
   DeleteInternalMemArray;
{$ENDIF}

end.
