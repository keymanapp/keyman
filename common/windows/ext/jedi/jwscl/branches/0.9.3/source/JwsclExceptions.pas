{
Description
Project JEDI Windows Security Code Library (JWSCL)

Contains exceptions that are used by the units of the JWSCL

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

The Original Code is JwsclExceptions.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.


Remarks
* Install JCL and compile this unit with compiler directive SM_JCLDEBUG and TD32 Debug Info.
* The GUIDs of each JWSCL exception are unique and won't be changed. If you add a new exception also
  add a new and unique GUID to it. They will be parsed to create JwsclExceptionMappings.inc that contains the
  JwExceptionMapping array. It will be used to de-/serialize exceptions to and from streams.

Warning: Only use Delphi 7 syntax!

}

{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclExceptions;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $

interface

uses SysUtils, Classes,
  jwaWindows, D5impl,
  JwsclResource,
  JwsclTypes, JwsclStrings;

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type
  EJwsclSecurityException = class;

  //<B>EJwsclExceptionClass</B> is the class of EJwsclSecurityException
  EJwsclExceptionClass = class of EJwsclSecurityException;
  EJwsclSecurityExceptionClass = class of EJwsclSecurityException;


  TJwExceptionConstructorType = (ctNone, ctIgnore, ctCreateMsg, ctCreateWithException,
      ctCreateFmtEx, ctCreateFmtWinCall);


  {<b>TJwOnFormatExceptionMessage</b> is used by JwOnFormatExceptionMessage.

   @param Sender Defines the exception. Use it for formatting the exception.
   @param ConstructorType Defines the constructor type which calls this function.
   @param Msg Contains the original message which can be replaced.

   Remarks
     Do not raise an exception in this function.
   }
  TJwOnFormatExceptionMessage = procedure(Sender : EJwsclSecurityException;
        ConstructorType : TJwExceptionConstructorType; Var Msg : TJwString);

  {<B>EJwsclSecurityException</B> is the main exception class that is used if an error occurs in any
        Security Library unit.

  Remarks
    You can define JwOnFormatExceptionMessage to format the exception message.
  @GUID(138EDC0B-B10B-4FA3-BB5D-DFDABBEBDDA4)
  }
  EJwsclSecurityException = class(Exception)
  {[2B2288BC-7905-46F2-0001-A0183067E63D]}
  protected
    fLastError:   Cardinal;
    fSourceProc, fsSourceClass,
    fsSourceFile: TJwString;
    fiSourceLine: Cardinal;
    fWinCallName: TJwString;
    fComSource : TJwString;
    fLog : TJwString;
    fUnsupportedProperties,
    fSimpleMessage : TJwString;
    fGuid : TGuid;

    fCurrentExceptionConstructorType : TJwExceptionConstructorType;


    fStackTrace : TJwString;

    procedure DoFormatExceptionMessage(ConstructorType : TJwExceptionConstructorType);
  public
    constructor Create(const Msg: String); overload;
           {<B>CreateFmtEx</B> creates an instance of the @classname exception.
            @param sMsg contains a description of the exception. 
            @param sSourceProc contains the caller method name 
            @param sSourceClass contains the caller method class name 
            @param iSourceLine contains the caller source position 
            @param bShowLastError defines if windows GetLastError information is used 
            @param Args contains string formatting information for sMsg. 
            }
    constructor CreateFmtEx(const MessageString: TJwString;
      sSourceProc, sSourceClass,
      sSourceFile: TJwString; iSourceLine:
      Cardinal; bShowLastError: boolean;
      const Args: array of const);
      overload; virtual;
    constructor CreateFmtEx(const MessageString: TJwString;
      sSourceProc, sSourceClass,
      sSourceFile: TJwString; iSourceLine:
      Cardinal; iLastError: Cardinal;
      const Args: array of const);
      overload; virtual;
    constructor CreateFmtWinCall(const sMsg: TJwString;
      sSourceProc, sSourceClass,
      sSourceFile: TJwString; iSourceLine:
      Cardinal; bShowLastError: boolean;
      sWinCall: TJwString;
      const Args: array of const); virtual;
    constructor Create(const anException: EJwsclSecurityException);
      overload;

           {<B>GetErrorMessage</B> creates a windows error string from a LastError.
           @param errNumber contains a GetLastError error number. }
    class function GetErrorMessage(errNumber: TJwLastError): TJwString;
      virtual;

           {<B>GetLastErrorMessage</B> creates a windows error string from the last call to GetLastError or a defined error message.
            @param iGetLastError defines the error ID to be translated into a string. If set to high(Cardinal)
              the GetLastError() call is used.}
    class function GetLastErrorMessage(
      const iGetLastError: Cardinal = Cardinal(-1)): TJwString; virtual;
  public
    class function CreateExceptionFromStream(const Stream: TStream;
      const DefaultExceptionClass : EJwsclExceptionClass = nil) : Exception; virtual;
  public //do not use
    procedure SaveToStream(const Stream : TStream); virtual;
    procedure LoadFromStream(const Stream : TStream); virtual;
    //http://www.blong.com/Conferences/BorConUK98/DelphiRTTI/CB140.htm
  public
  //published
    {<B>LastError</B> contains the LastError error code provided the the CreateFmtEx constructor}
    property LastError: Cardinal Read fLastError write fLastError;

    property SourceProc: TJwString Read fSourceProc Write fSourceProc;
    property SourceClass: TJwString
      Read fsSourceClass Write fsSourceClass;
    property SourceFile: TJwString
      Read fsSourceFile Write fsSourceFile;
    property SourceLine: Cardinal
      Read fiSourceLine Write fiSourceLine;

    {<b>SimpleMessage</b> contains the original text that was inteded for this exception.
     It does not contain any other (for debugging purposes) information.}
    property SimpleMessage : TJwString read fSimpleMessage write fSimpleMessage;

    {<B>WinCallName</B> defines the winapi function name of the failed call.}
    property WinCallName: TJwString Read fWinCallName Write fWinCallName;

    property Log : TJwString read fLog write fLog;
    property ComSource: TJwString read fComSource write fComSource;
    property StackTrace : TJwString read fStackTrace write fStackTrace;

    {<b>UnsupportedProperties</b> contains a comma separated list of
     property names that could not be written/read from stream
     by SaveToStream/LoadFromStream.}
    property UnsupportedProperties : TJwString read fUnsupportedProperties;

    property Guid : TGuid read fGuid;
  end;

var
  {<b>JwOnFormatExceptionMessage</b> is called every time a JWSCL exception is
   raised and the message should be formatted. }
  JwOnFormatExceptionMessage : TJwOnFormatExceptionMessage = nil;


type
  //<B>EJwsclOpenThreadTokenException</B> is raised if the thread token could not be opened
  EJwsclOpenThreadTokenException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0002-A0183067E63D]};

  //<B>EJwsclOpenProcessTokenException</B> is raised if the process token could not be opened
  EJwsclOpenProcessTokenException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0003-A0183067E63D]};

  //<B>EJwsclSharedTokenException</B> is raised is not used.
  EJwsclSharedTokenException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F5-0003-A0183067E63D]};

  //<B>EJwsclTokenInformationException</B> is raised if token information could not be retrieved.
  EJwsclTokenInformationException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0004-A0183067E63D]};

  //<B>EJwsclTokenImpersonationException</B> is raised if the token could not be converted to an impersonated token.
  EJwsclTokenImpersonationException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0005-A0183067E63D]};

  //<B>EJwsclTokenPrimaryException</B> is raised if the requested primary token could not be retrieved. For more information see LastError.
  EJwsclTokenPrimaryException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0006-A0183067E63D]};

  EJwsclInvalidPrimaryToken = class(EJwsclTokenPrimaryException)
    {[2B2288BC-7905-46F2-0007-A0183067E63D]};


  //<B>EJwsclInvalidOwnerException</B> is raised if the given owner is invalid say nil.
  EJwsclInvalidOwnerException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0008-A0183067E63D]};

  //<B>EJwsclDuplicateTokenException</B> is raised if a call to DuplicateTokenEx failed
  EJwsclDuplicateTokenException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0009-A0183067E63D]};

  //<B>EJwsclNoThreadTokenAvailable</B> is raised if the requested impersonated token could not be retrieved
  EJwsclNoThreadTokenAvailable = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0010-A0183067E63D]};

  //<B>EJwsclInvalidTokenHandle</B> is raised if the handle of the token is invalid
  EJwsclInvalidTokenHandle = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0011-A0183067E63D]};

  //<B>EJwsclNotEnoughMemory</B> is raised if a allocation function could not allocate a buffer in memory because of not enough memory
  EJwsclNotEnoughMemory = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0012-A0183067E63D]};

  //<B>EJwsclPrivilegeException</B> is raised if an errors occurs that includes a problem with a privilege
  EJwsclPrivilegeException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0013-A0183067E63D]};

  //<B>EJwsclInvalidIndexPrivilegeException</B> is raised if the given index is out of bounds of the privileges list
    EJwsclInvalidIndexPrivilegeException = class(EJwsclPrivilegeException)
    {[2B2288BC-7905-46F2-0014-A0183067E63D]};

  //<B>EJwsclPrivilegeNotFoundException</B> is raised if a given privilege was not found
  EJwsclPrivilegeNotFoundException = class(EJwsclPrivilegeException)
    {[2B2288BC-7905-46F2-0015-A0183067E63D]};

  //<B>EJwsclPrivilegeCheckException</B> is raised if a given privilege was not found in the list of privileges of the token
  EJwsclPrivilegeCheckException = class(EJwsclPrivilegeException)
    {[2B2288BC-7905-46F2-0016-A0183067E63D]};

  //<B>EJwsclAdjustPrivilegeException</B> is raised if the privileges of a token could not be changed
  EJwsclAdjustPrivilegeException = class(EJwsclPrivilegeException)
    {[2B2288BC-7905-46F2-0017-A0183067E63D]};

  //<B>EJwsclAccessTypeException</B> is raised if the desired access mask is not included in the token access mask!
  EJwsclAccessTypeException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0018-A0183067E63D]};

  //<B>EJwsclNotImplementedException</B> is raised if the called method is not implemented yet.
  EJwsclNotImplementedException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0019-A0183067E63D]};
  //<B>EJwsclUnsupportedWindowsVersionException</B> is raised if the called function is not supported under the running windows version
  EJwsclUnsupportedWindowsVersionException =  class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0020-A0183067E63D]};




  //<B>EJwsclWinCallFailedException</B> is raised if a call to a windows API function failed. For more information see the LastError property
  EJwsclWinCallFailedException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-1020-A0183067E63D]};

  EJwsclProcessIdNotAvailable = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0021-A0183067E63D]};

  EJwsclInvalidObjectArrayException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0022-A0183067E63D]};

  EJwsclInheritanceSourceNotSupportedException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0023-A0183067E63D]};

  //<B>EJwsclNILParameterException</B> is raised if a given parameter is nil which is invalid.
  EJwsclNILParameterException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0024-A0183067E63D]};

  EJwsclEmptyACLException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0025-A0183067E63D]};

  EJwsclInvalidMandatoryLevelException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0026-A0183067E63D]};

  EJwsclInvalidSecurityListException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0027-A0183067E63D]};

  //<B>EJwsclInvalidSIDException</B> is raised if a SID has an invalid structure
  EJwsclInvalidSIDException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0028-A0183067E63D]};

    EJwsclInvalidOwnerSIDException = class(EJwsclInvalidSIDException)
      {[2B2288BC-7905-46F2-0029-A0183067E63D]};

    EJwsclInvalidGroupSIDException = class(EJwsclInvalidSIDException)
      {[2B2288BC-7905-46F2-0030-A0183067E63D]};

  EJwsclInvalidComputer = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0031-A0183067E63D]};

  {<B>EJwsclInvalidKnownSIDException</B> is raised if TJwSecurityId.CreateWellKnownSid fails}
  EJwsclInvalidKnownSIDException = class(EJwsclInvalidSIDException)
    {[2B2288BC-7905-46F2-0032-A0183067E63D]}
  protected
    fSidType : TWellKnownSidType;
  public
    {<B>SidType</B> contains information which sid type was used
     but could not be found}
    property SidType : TWellKnownSidType read fSidType write fSidType;
  end;

  EJwsclInvalidSidAuthorityValue = class(EJwsclInvalidSIDException)
    {[2B2288BC-7905-46F2-0033-A0183067E63D]};


  //<B>EJwsclIndexOutOfBoundsException</B> is raised if an given index is not within the bounds of a list
  EJwsclIndexOutOfBoundsException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0034-A0183067E63D]};

  //<B>EJwsclDuplicateListEntryException</B> is raised if a SID was already added to a list
  EJwsclDuplicateListEntryException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0035-A0183067E63D]};

  EJwsclReadOnlyPropertyException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0036-A0183067E63D]};

  EJwsclInvalidACEException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0037-A0183067E63D]};

  EJwsclRevisionMismatchException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0038-A0183067E63D]};

  EJwsclInvalidAceMismatch = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0039-A0183067E63D]};

  EJwsclInvalidRevision = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0040-A0183067E63D]};

  EJwsclInvalidSecurityDescriptor = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0041-A0183067E63D]};

  EJwsclInvalidPathException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0042-A0183067E63D]};

  EJwsclInvalidParameterException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0043-A0183067E63D]};

  EJwsclProcessNotFound = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0044-A0183067E63D]};

  EJwsclInvalidFlagsException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0045-A0183067E63D]};

  EJwsclNoSuchLogonSession = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0046-A0183067E63D]};

  EJwsclStreamException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0047-A0183067E63D]};

    EJwsclStreamSizeException = class(EJwsclStreamException)
      {[2B2288BC-7905-46F2-0048-A0183067E63D]};

    EJwsclStreamInvalidMagicException = class(EJwsclStreamException)
      {[2B2288BC-7905-46F2-0049-A0183067E63D]};

    EJwsclStreamHashException = class(EJwsclStreamException)
      {[2B2288BC-7905-46F2-0050-A0183067E63D]};

  //EHashMismatch is raised in case of unequal hash data
  EJwsclHashMismatch = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0051-A0183067E63D]};


  EJwsclSecurityObjectException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0052-A0183067E63D]};

  EJwsclInvalidObjectException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0053-A0183067E63D]};

  EJwsclThreadException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0054-A0183067E63D]};

  EJwsclAdaptSecurityInfoException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0055-A0183067E63D]};

  EJwsclInvalidGenericAccessMask = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0056-A0183067E63D]};

  EJwsclInvalidKeyPath = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0057-A0183067E63D]};

  EJwsclInvalidParentDescriptor = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0058-A0183067E63D]};

  ESetSecurityException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0059-A0183067E63D]};

  ESetOwnerException = class(ESetSecurityException)
    {[2B2288BC-7905-46F2-0060-A0183067E63D]};

  EJwsclLSAException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0061-A0183067E63D]};

  EJwsclAccessDenied = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0062-A0183067E63D]};

    EJwsclSACLAccessDenied = class(EJwsclAccessDenied)
      {[2B2288BC-7905-46F2-0063-A0183067E63D]};

          {
          EDesktopException is the general exception that is raised if an error occurred
          during desktop manipulation.
          }
    EJwsclDesktopException = class(EJwsclSecurityException)
      {[2B2288BC-7905-46F2-0064-A0183067E63D]};

          {EOpenDesktopException is raised if there was an error during opening a desktops.
          Possible cases are :
          1. Desktop does not exists
          }
    EJwsclOpenDesktopException = class(EJwsclDesktopException)
      {[2B2288BC-7905-46F2-0065-A0183067E63D]};

          {ECreateDesktopException is raised if there was an error during creating a new desktop.
          Possible cases are :
          1. Desktop already exists
          2. Not enough rights}
    EJwsclCreateDesktopException = class(EJwsclDesktopException)
      {[2B2288BC-7905-46F2-1065-A0183067E63D]};

          {ECloseDesktopException is raised if there was an error during closing a desktop.
          Possible cases are :
          1. Desktop handle is not valid
          2. not enough rights
          }

    EJwsclCloseDesktopException = class(EJwsclDesktopException)
      {[2B2288BC-7905-46F2-0066-A0183067E63D]};


  EJwsclWindowStationException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0067-A0183067E63D]};

  EJwsclOpenWindowStationException = class(EJwsclWindowStationException)
    {[2B2288BC-7905-46F2-0068-A0183067E63D]};

  EJwsclUnsupportedACE = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0069-A0183067E63D]};

  EJwsclFailedAddACE = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0070-A0183067E63D]};

  EJwsclResourceException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0071-A0183067E63D]};

  EJwsclResourceNotFound = class(EJwsclResourceException)
    {[2B2288BC-7905-46F2-0072-A0183067E63D]};

  EJwsclResourceUnequalCount = class(EJwsclResourceException)
    {[2B2288BC-7905-46F2-0073-A0183067E63D]};

  EJwsclResourceInitFailed = class(EJwsclResourceException)
    {[2B2288BC-7905-46F2-0074-A0183067E63D]};


  EJwsclOSError = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0075-A0183067E63D]};

  EJwsclCryptException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0076-A0183067E63D]};

  EJwsclCryptApiException = class(EjwsclCryptException)
    {[2B2288BC-7905-46F2-0077-A0183067E63D]};

  EJwsclCryptUnsupportedException = class(EjwsclCryptException)
    {[2B2288BC-7905-46F2-0078-A0183067E63D]};

  //general exception for terminal server methods
  EJwsclTerminalServerException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0079-A0183067E63D]};

  // Terminal Server Connection Exception
  EJwsclTerminalServerConnectException = class(EJwsclTerminalServerException)
    {[2B2288BC-7905-46F2-0080-A0183067E63D]};

  EJwsclTerminalServiceException =  class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0081-A0183067E63D]};

  EJwsclTerminalServiceNecessary =  class(EJwsclTerminalServiceException)
    {[2B2288BC-7905-46F2-0082-A0183067E63D]};

  //general exception for terminal session methods

  //general exception for terminal session methods
  EJwsclTerminalSessionException = class(EJwsclTerminalServerException)
    {[2B2288BC-7905-46F2-1082-A0183067E63D]};

  EJwsclCSPException = class(EJwsclCryptException)
    {[2B2288BC-7905-46F2-0083-A0183067E63D]};

  EJwsclCSPApiException = class(EJwsclCSPException)
    {[2B2288BC-7905-46F2-1013-A0183067E63D]};

  EJwsclHashException = class(EJwsclCryptException)
    {[2B2288BC-7905-46F2-0084-A0183067E63D]};

  EJwsclHashApiException = class (EJwsclHashException)
    {[2B2288BC-7905-46F2-0085-A0183067E63D]};

  EJwsclKeyException = class(EJwsclCryptException)
    {[2B2288BC-7905-46F2-0086-A0183067E63D]};

  EJwsclKeyApiException = class(EJwsclKeyException)
    {[2B2288BC-7905-46F2-0087-A0183067E63D]};

  {<B>EJwsclInitWellKnownException</B> is raised if JwInitWellKnownSIDs was not called.}
  EJwsclInitWellKnownException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0088-A0183067E63D]};

  {<b>EJwsclUnimplemented</b>
  The called function isn't implemented yet.}
  EJwsclUnimplemented = class(EJwsclSecurityException)
    {[2B2288BC-7905-FFFF-0013-A0183067E63D]};

  {<b>EJwsclNilPointer</b>
  A given parameter or variable is nil but must not be nil.}
  EJwsclNilPointer = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0089-A0183067E63D]};

  EJwsclCreateProcessFailed = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0090-A0183067E63D]};

  EJwsclInvalidPointerType = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0091-A0183067E63D]};

  EJwsclMissingEvent = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0092-A0183067E63D]};

  EJwsclInvalidSession = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0093-A0183067E63D]};

  EJwsclInvalidIndex = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0094-A0183067E63D]};

  EJwsclInvalidHandle = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0095-A0183067E63D]};

  EJwsclClassTypeMismatch = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0096-A0183067E63D]};

  EJwsclEndOfStream = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0097-A0183067E63D]};

  EJwsclInvalidRegistryPath = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0098-A0183067E63D]};

  {}
  EJwsclEnumerateProcessFailed = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0099-A0183067E63D]};

  EJwsclGenericFirewallException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0101-A0183067E63D]};

	  EJwsclFirewallInitException = class(EJwsclGenericFirewallException)
      {[2B2288BC-7905-46F2-0102-A0183067E63D]};

	  EJwsclFirewallProfileInitException = class(EJwsclGenericFirewallException)
      {[2B2288BC-7905-46F2-0103-A0183067E63D]};

	  EJwsclSetFWStateException = class(EJwsclGenericFirewallException)
      {[2B2288BC-7905-46F2-0104-A0183067E63D]};

	  EJwsclGetFWStateException = class(EJwsclGenericFirewallException)
      {[2B2288BC-7905-46F2-0105-A0183067E63D]};

	  EJwsclGetFWExceptionsAllowedException = class(EJwsclGenericFirewallException)
      {[2B2288BC-7905-46F2-0106-A0183067E63D]};

	  EJwsclSetFWExceptionsAllowedException = class(EJwsclGenericFirewallException)
      {[2B2288BC-7905-46F2-0107-A0183067E63D]};

	  EJwsclGetIncomingPingAllowedException = class(EJwsclGenericFirewallException)
      {[2B2288BC-7905-46F2-0108-A0183067E63D]};

	  EJwsclSetIncomingPingAllowedException = class(EJwsclGenericFirewallException)
      {[2B2288BC-7905-46F2-0109-A0183067E63D]};

	  EJwsclGetRemoteAdminAllowedException = class(EJwsclGenericFirewallException)
      {[2B2288BC-7905-46F2-0110-A0183067E63D]};

	  EJwsclSetRemoteAdminAllowedException = class(EJwsclGenericFirewallException)
      {[2B2288BC-7905-46F2-0111-A0183067E63D]};

	  EJwsclGetRemoteAdminAdressException = class(EJwsclGenericFirewallException)
      {[2B2288BC-7905-46F2-0112-A0183067E63D]};

	  EJwsclSetRemoteAdminAdressException = class(EJwsclGenericFirewallException)
      {[2B2288BC-7905-46F2-0113-A0183067E63D]};

	  EJwsclFirewallAddRuleException = class(EJwsclGenericFirewallException)
      {[2B2288BC-7905-46F2-0114-A0183067E63D]};

	  EJwsclAddTcpPortToFirewallException = class(EJwsclGenericFirewallException)
      {[2B2288BC-7905-46F2-0115-A0183067E63D]};

	  EJwsclAddUdpPortToFirewallException = class(EJwsclGenericFirewallException)
      {[2B2288BC-7905-46F2-0116-A0183067E63D]};

	  EJwsclFirewallDelRuleException = class(EJwsclGenericFirewallException)
      {[2B2288BC-7905-46F2-0117-A0183067E63D]};

	  EJwsclFirewallInactiveException = class(EJwsclGenericFirewallException)
      {[2B2288BC-7905-46F2-0118-A0183067E63D]};

	  EJwsclFirewallNoExceptionsException = class(EJwsclGenericFirewallException)
      {[2B2288BC-7905-46F2-0119-A0183067E63D]};



  EJwsclInvalidStartupInfo = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0120-A0183067E63D]};

  {<B>EJwsclVistaFeaturesDisabled</B> is raised if the JWSCL library
  was compiled with the compiler directive VISTA deactivated.
  To use vista features you must activate the directive in file
  includes\Jwscl.inc and make sure that you also compiled JwaWindows
  with at least WINVISTA or WIN2008 to enable Vista features.
  }
  EJwsclVistaFeaturesDisabled = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0121-A0183067E63D]};

  EJwsclCertApiException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0122-A0183067E63D]};

  EJwsclElevateProcessException = class(EJwsclSecurityException)
    {[2B2288BC-7905-46F2-0123-A0183067E63D]};

  EJwsclSuRunErrorException = class(EJwsclElevateProcessException)
    {[2B2288BC-7905-46F2-0124-A0183067E63D]};

  EJwsclAbortException = class(EJwsclElevateProcessException)
    {[2B2288BC-7905-46F2-0125-A0183067E63D]};

  EJwsclElevationException = class(EJwsclElevateProcessException)
    {[2B2288BC-7905-46F2-0126-A0183067E63D]};

  EJwsclShellExecuteException = class(EJwsclElevateProcessException)
    {[2B2288BC-7905-46F2-0127-A0183067E63D]};

  EJwsclJwShellExecuteException = class(EJwsclElevateProcessException)
    {[2B2288BC-7905-46F2-0128-A0183067E63D]};

  EJwsclPIDException = class(EJwsclElevateProcessException)
    {[2B2288BC-7905-46F2-0129-A0183067E63D]};

  {EJwsclUnsupportedException This exception is raised if an exception could
    not be interpreted as an JWSCL exception.}
  EJwsclUnsupportedException = class(EJwsclElevateProcessException)
    {[2B2288BC-7905-46F2-0130-A0183067E63D]};


  JwGeneralExceptionClass = class of Exception;


type
   TJwExceptionMapping = record
     Name : WideString;
     ID : TGUID;
     ExcPtr : JwGeneralExceptionClass;
   end;

procedure JwRaiseLastOSError(const FailedWin32FunctionName,
  Method, ClassName, FileName : TJwString);


{<B>JwRaiseOnNilParameter</B> raises an exception EJwsclNILParameterException if parameter P
 is nil; otherwise nothing happens.
This function is like Assert but it will not be removed in a release build.

@param P defines a pointer to be validated
@param ParameterName defines the name of the parameter which is validated and
 belongs to this pointer
@param MethodName defines the name of the method this parameter belongs to
@param ClassName defines the name of the class the method belongs to. Can be
  empty if the method does not belong to a class
@param FileName defines the source file of the call to this procedure.

raises
 EJwsclNILParameterException:  will be raised if P is nil
}
procedure JwRaiseOnNilParameter(const P : Pointer;
  const ParameterName, MethodName, ClassName, FileName : TJwString);

{<B>JwRaiseOnClassTypeMisMatch</B> raises an exception EJwsclClassTypeMismatch if parameter Instance
 is not of type ExpectedClass.
This function is like Assert but it will not be removed in a release build.

@param Instance defines the class to be tested. If this parameter is nil, the procedure exists without harm.
@param ExpectedClass defines the class type to be checked for.
@param MethodName defines the name of the method this parameter belongs to
@param ClassName defines the name of the class the method belongs to. Can be
  empty if the method does not belong to a class
@param FileName defines the source file of the call to this procedure.

raises
 EJwsclNILParameterException:  will be raised if P is nil
}
procedure JwRaiseOnClassTypeMisMatch(const Instance : TObject;
  const ExpectedClass : TClass;
  const MethodName, ClassName, FileName : TJwString);


{<B>JwRaiseOnNilMemoryBlock</B> raises an exception EJwsclNilPointer if parameter P
 is nil; otherwise nothing happens.
This function is like Assert but it will not be removed in a release build.

@param P defines a pointer to be validated
@param ParameterName defines the name of the parameter which is validated and
 belongs to this pointer
@param MethodName defines the name of the method this parameter belongs to
@param ClassName defines the name of the class the method belongs to. Can be
  empty if the method does not belong to a class
@param FileName defines the source file of the call to this procedure.

raises
 EJwsclNilPointer:  will be raised if P is nil
}
procedure JwRaiseOnNilMemoryBlock(const P : Pointer;
  const MethodName, ClassName, FileName : TJwString);


//Mappings for JWSCL exceptions so we can
//stream and restore them
{$DEFINE JWSCL_EXCEPTION_MAPPINGS}
{$I ..\includes\JwsclExceptionMappings.inc}

function JwCreateException(const Name : WideString) : JwGeneralExceptionClass;
function JwMapException(Const Id : TGuid) : WideString; overload;
function JwMapException(Const Name : WideString) : TGuid; overload;

{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

uses JwsclConstants, JwsclDescriptor
{$IFDEF SM_JCLDEBUG}
  ,jclDebug, JwsclStrings
{$ENDIF}
;


{$ENDIF SL_OMIT_SECTIONS}


procedure JwRaiseLastOSError(const FailedWin32FunctionName,
  Method, ClassName, FileName : TJwString);
begin
  raise EJwsclWinCallFailedException.CreateFmtEx(
     RsWinCallFailed,
      Method, ClassName, FileName, 0, false, [FailedWin32FunctionName]);
end;

procedure JwRaiseOnNilMemoryBlock(const P : Pointer; const MethodName, ClassName, FileName : TJwString);
begin
  if P = nil then
    raise EJwsclNilPointer.CreateFmtEx(
     RsNilPointer,
      MethodName, ClassName, FileName, 0, false, []);
end;

procedure JwRaiseOnClassTypeMisMatch(const Instance : TObject;
  const ExpectedClass : TClass;
  const MethodName, ClassName, FileName : TJwString);
begin
  if Assigned(Instance) and
    not (Instance is TJwSecurityDescriptor) then
      raise EJwsclClassTypeMismatch.CreateFmtEx(
               RsInvalidClassType,
               MethodName, ClassName, FileName, 0, false,
                [Instance.ClassName, ExpectedClass.ClassName]);
end;
procedure JwRaiseOnNilParameter(const P : Pointer; const ParameterName, MethodName, ClassName, FileName : TJwString);
begin
  if not Assigned(P) then
  raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter, MethodName,
      ClassName, FileName, 0, False, [ParameterName]);
end;


function JwMapException(Const Id : TGuid) : WideString;

  function CompareGUID(const G1, G2: TGUID): boolean;
  begin
    Result := CompareMem(@G1, @G2, Sizeof(TGUID));
  end;

var i : Integer;
begin
  result := '';
  for i := low(JwExceptionMapping) to high(JwExceptionMapping) do
  begin
    if CompareGUID(JwExceptionMapping[i].ID, Id) then
    begin
      result := JwExceptionMapping[i].Name;
      exit;
    end;
  end;
end;

function JwMapException(Const Name : WideString) : TGuid;
var i : Integer;
begin
  result := NULL_GUID;
  for i := low(JwExceptionMapping) to high(JwExceptionMapping) do
  begin
    if WideCompareText(Name, JwExceptionMapping[i].Name) = 0 then
    begin
      result := JwExceptionMapping[i].Id;
      exit;
    end;
  end;
end;

function JwCreateException(const Name : WideString) : JwGeneralExceptionClass;
var i : Integer;
begin
  result := Exception;
  for i := low(JwExceptionMapping) to high(JwExceptionMapping) do
  begin
    if WideCompareText(Name, JwExceptionMapping[i].Name) = 0 then
    begin
      result := JwExceptionMapping[i].ExcPtr;
      exit;
    end;
  end;
end;


{$IFNDEF SL_INTERFACE_SECTION}

procedure EJwsclSecurityException.DoFormatExceptionMessage(
  ConstructorType : TJwExceptionConstructorType);
var Msg : TJwString;
begin
  if Assigned(JwOnFormatExceptionMessage)
    and (fCurrentExceptionConstructorType <> ctIgnore) then
  begin
    Msg := Message;
    JwOnFormatExceptionMessage(Self, ConstructorType, Msg);
    Message := Msg;
  end;
end;

constructor EJwsclSecurityException.Create(const Msg: String);
begin
  fLastError := GetLastError;

  inherited Create(Msg);
  fSimpleMessage := Msg;
  ZeroMemory(@fGuid, sizeof(fGuid));

  DoFormatExceptionMessage(ctCreateMsg);
end;

constructor EJwsclSecurityException.CreateFmtWinCall(const sMsg: TJwString;
  sSourceProc, sSourceClass,
  sSourceFile: TJwString; iSourceLine:
  Cardinal; bShowLastError: boolean;
  sWinCall: TJwString;
  const Args: array of const);
begin
  //ignore call of JwOnFormatExceptionMessage in the next function
  fCurrentExceptionConstructorType := ctIgnore;

  CreateFmtEx(sMsg, sSourceProc, sSourceClass, sSourceFile,
    iSourceLine, bShowLastError, Args);
  fWinCallName := sWinCall;

  fSimpleMessage := sMsg;

  ZeroMemory(@fGuid, sizeof(fGuid));

  DoFormatExceptionMessage(ctCreateFmtWinCall);
end;

constructor EJwsclSecurityException.Create(
  const anException: EJwsclSecurityException);
begin
  inherited Create(anException.Message);

  fSimpleMessage := anException.Message;

  fLastError := anException.fLastError;
  fSourceProc := anException.fSourceProc;
  fsSourceClass := anException.fsSourceClass;
  fsSourceFile := anException.fsSourceFile;
  fiSourceLine := anException.fiSourceLine;
  fWinCallName := anException.fWinCallName;

  ZeroMemory(@fGuid, sizeof(fGuid));

  DoFormatExceptionMessage(ctCreateWithException);
end;

constructor EJwsclSecurityException.CreateFmtEx(const MessageString: TJwString;
  sSourceProc, sSourceClass,
  sSourceFile: TJwString;
  iSourceLine: Cardinal;
  iLastError: Cardinal;
  const Args: array of const);
var
  aMessage, sData, sLastError,sJCLText : TJwString;
{$IFDEF SM_JCLDEBUG}
    CallStackStrings: TStringList;
    pCaller : Pointer;
    sModule: TJwString;
    sI : TJclStackInfoList;
{$ENDIF SM_JCLDEBUG}

begin
  fLastError := iLastError;
  fSourceProc := sSourceProc;
  fsSourceClass := sSourceClass;
  fsSourceFile := sSourceFile;
  fiSourceLine := fiSourceLine;
  ZeroMemory(@fGuid, sizeof(fGuid));

  fSimpleMessage := MessageString;

  if Length(MessageString) > 0 then
  begin
    sData := JwFormatString(RsExceptionMessage, [MessageString]);
    sData := JwFormatString(sData,Args);
  end
  else
    sData := '';

  if Length(sSourceProc) = 0 then
    sSourceProc := RsExceptionNoProc;

  if Length(sSourceClass) = 0 then
    sSourceClass := RsExceptionNoClass;

  if Length(sSourceFile) = 0 then
    sSourceFile := RsExceptionNoFile;


  sLastError := JwFormatString(RsExceptionErrors,
    [iLastError, IntToHex(iLastError, 1),
    GetLastErrorMessage(iLastError)]);

  sJCLText := '';
{$IFDEF SM_JCLDEBUG}

  if DebugInfoAvailable(HInstance) then
  begin
    pCaller := Caller(1,false);
    if (MapOfAddr(pCaller,  sSourceFile, sModule, sSourceProc, Integer(iSourceLine))) then
    begin
      sSourceFile := JwFormatString(RsExceptionJCLText1,[sModule,sSourceFile]);

      CallStackStrings := TStringList.Create;

       sI := JclCreateStackList(false, 1, pCaller);
       if Assigned(sI) then
       begin
         SI.AddToStrings(CallStackStrings,
                        false,//IncludeModuleName: Boolean = False;
                        false,//IncludeAddressOffset: Boolean = False;
                        false,//IncludeStartProcLineOffset: Boolean = False;
                        false//IncludeVAdress: Boolean = False): Boolean;
                      ) ;
         sJCLText := TJwString(CallStackStrings.Text);

         fStackTrace := TJwString(CallStackStrings.Text);
       end;
       CallStackStrings.Free;
    end;

  end;
{$ENDIF}

  aMessage := JwFormatString(RsExceptionMainMessage,
   [ClassName, sSourceProc, sSourceClass,
   sSourceFile, iSourceLine, sLastError, sData,sJCLText]);


  inherited Create(aMessage);

  DoFormatExceptionMessage(ctCreateFmtEx);
end;




constructor EJwsclSecurityException.CreateFmtEx(const MessageString: TJwString;
  sSourceProc, sSourceClass,
  sSourceFile: TJwString;
  iSourceLine:  Cardinal;
  bShowLastError: boolean;
  const Args: array of const);
var
  aMessage, sData, sLastError,
  sJCLText: TJwString;
{$IFDEF SM_JCLDEBUG}
    CallStackStrings: TStringList;
    pCaller : Pointer;
    sModule: TJwString;
    sI : TJclStackInfoList;
{$ENDIF SM_JCLDEBUG}
begin
  fLastError := GetLastError;
  fSourceProc := sSourceProc;
  fsSourceClass := sSourceClass;
  fsSourceFile := sSourceFile;
  fiSourceLine := fiSourceLine;

  ZeroMemory(@fGuid, sizeof(fGuid));

  fSimpleMessage := MessageString;


  if Length(MessageString) > 0 then
  begin
    sData := JwFormatString(RsExceptionMessage, [MessageString]);
    sData := JwFormatString(sData,Args);
  end
  else
    sData := '';

  if Length(sSourceProc) = 0 then
    sSourceProc := RsExceptionNoProc;

  if Length(sSourceClass) = 0 then
    sSourceClass := RsExceptionNoClass;

  if Length(sSourceFile) = 0 then
    sSourceFile := RsExceptionNoFile;


  if bShowLastError then
  begin
    sLastError := JwFormatString(RsExceptionErrors,
    [fLastError, IntToHex(fLastError, 1),
    GetLastErrorMessage(fLastError)]);
  end;

  aMessage := RsExceptionMainMessage;

  sJCLText := '';
{$IFDEF SM_JCLDEBUG}

  if DebugInfoAvailable(HInstance) then
  begin
    pCaller := Caller(1,false);
    if (MapOfAddr(pCaller,  sSourceFile, sModule, sSourceProc, Integer(iSourceLine))) then
    begin
      sSourceFile := JwFormatString(RsExceptionJCLText1,[sModule,sSourceFile]);

      CallStackStrings := TStringList.Create;

       sI := JclCreateStackList(false, 1, pCaller);
       if Assigned(sI) then
       begin
         SI.AddToStrings(CallStackStrings,
                        false,//IncludeModuleName: Boolean = False;
                        false,//IncludeAddressOffset: Boolean = False;
                        false,//IncludeStartProcLineOffset: Boolean = False;
                        false//IncludeVAdress: Boolean = False): Boolean;
                      ) ;
         sJCLText := TJwString(CallStackStrings.Text);
         fStackTrace := TJwString(CallStackStrings.Text);
       end;
       CallStackStrings.Free;
    end;
  end;
{$ENDIF}


  aMessage := JwFormatString(RsExceptionMainMessage,
   [ClassName, sSourceProc, sSourceClass,
   sSourceFile, iSourceLine, sLastError, sData, sJCLText]);


  inherited Create(aMessage);

  DoFormatExceptionMessage(ctCreateFmtEx);
end;


class function EJwsclSecurityException.GetLastErrorMessage(
  const iGetLastError: Cardinal = Cardinal(-1)): TJwString;
begin
  if iGetLastError = Cardinal(-1) then
    Result := GetErrorMessage(GetLastError)
  else
    Result := GetErrorMessage(iGetLastError);
end;

procedure WriteStringToStream(const Stream : TStream; const Value : WideString);
var v : DWORD;
begin
  v := Length(Value);
  Stream.Write(v, sizeof(v));
  Stream.Write(Value[1], Length(Value) * sizeof(WideChar));
end;

procedure ReadStringFromStream(const Stream : TStream; out Value : TJwString);
var v : DWORD;
  WideS : WideString;
begin
  Stream.Read(v, sizeof(v));

  SetLength(WideS, v);
  Stream.Read(WideS[1], V * sizeof(WideChar));
  Value := WideS;
end;

class function EJwsclSecurityException.CreateExceptionFromStream(const Stream: TStream;
    const DefaultExceptionClass : EJwsclExceptionClass = nil) : Exception;

  //load jwscl properties if this is a jwscl exception
  procedure LoadFromStream(var E : Exception; Mem : TStream);
  begin
    if E is EJwsclSecurityException then
    begin
      try
        (E as EJwsclSecurityException).LoadFromStream(Mem);
      except
        E.Free;
        E := nil;
      end;
    end;
  end;
var
  E : EJwsclSecurityException;
  i : Integer;
  Mem : TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  try
    E := EJwsclSecurityException.Create('');
    try
      //make a copy of the stream data into mem
      E.LoadFromStream(Stream);
      E.SaveToStream(Mem);
      Mem.Position := 0;

      result := nil;
      //lower indexes are more generic classes so start from the ending
      //find the exception class from the guid
      for i := High(JwExceptionMapping) downto Low(JwExceptionMapping) do
      begin
        if CompareMem(@E.Guid, @JwExceptionMapping[i].ID, sizeof(TGuid)) then
        begin
          result := JwExceptionMapping[i].ExcPtr.Create(E.Message);
          LoadFromStream(result, Mem);
          break;
        end;
      end;

      //could not find a mapping
      if not Assigned(result) then
      begin
        //use supplied exception class
        if DefaultExceptionClass <> nil then
        begin
          result := DefaultExceptionClass.Create(E.Message);
          LoadFromStream(result, Mem);
        end
        else
          //otherwise create error
          raise EJwsclUnsupportedException.CreateFmt('An exception could not be '+
             'created because the guid could not be mapped to the exception class (ID:%s) ',[GUIDToString(E.Guid)]);
      end;
    finally
      E.Free;
    end;
  finally
    Mem.Free;
  end;
end;

procedure EJwsclSecurityException.LoadFromStream(const Stream: TStream);
begin
  Stream.Read(fGuid, sizeof(fGuid));
  Stream.Read(fLastError, sizeof(fLastError));
  Stream.Read(fSourceProc, sizeof(fSourceProc));
  Stream.Read(fsSourceClass, sizeof(fsSourceClass));
  Stream.Read(fsSourceFile, sizeof(fsSourceFile));
  Stream.Read(fiSourceLine, sizeof(fiSourceLine));
  ReadStringFromStream(Stream, fWinCallName);
  ReadStringFromStream(Stream, fComSource);
  ReadStringFromStream(Stream, fLog);
  Stream.Read(fUnsupportedProperties, sizeof(fUnsupportedProperties));
  ReadStringFromStream(Stream, fSimpleMessage);
  ReadStringFromStream(Stream, fStackTrace);
end;

procedure EJwsclSecurityException.SaveToStream(const Stream: TStream);
var
  i : Integer;
  Guid : TGuid;
begin
  ZeroMemory(@Guid, sizeof(Guid));
  for i := High(JwExceptionMapping) downto low(JwExceptionMapping) do
  begin
    if Self is JwExceptionMapping[i].ExcPtr then
    begin
      Guid := JwExceptionMapping[i].ID;
      Break;
    end;
  end;

  Stream.Write(Guid, sizeof(Guid));
  Stream.Write(fLastError, sizeof(fLastError));
  Stream.Write(fSourceProc, sizeof(fSourceProc));
  Stream.Write(fsSourceClass, sizeof(fsSourceClass));
  Stream.Write(fsSourceFile, sizeof(fsSourceFile));
  Stream.Write(fiSourceLine, sizeof(fiSourceLine));
  WriteStringToStream(Stream, fWinCallName);
  WriteStringToStream(Stream, fComSource);
  WriteStringToStream(Stream, fLog);
  Stream.Write(fUnsupportedProperties, sizeof(fUnsupportedProperties));
  WriteStringToStream(Stream, fSimpleMessage);
  WriteStringToStream(Stream, fStackTrace);
end;

class function EJwsclSecurityException.GetErrorMessage(errNumber: TJwLastError)
: TJwString;
var
  s: TJwPChar;
begin
  if (
{$IFDEF UNICODE}
    FormatMessageW
{$ELSE}
    FormatMessageA
{$ENDIF}
    (FORMAT_MESSAGE_ALLOCATE_BUFFER or
    FORMAT_MESSAGE_FROM_SYSTEM or
    FORMAT_MESSAGE_IGNORE_INSERTS //see http://blogs.msdn.com/oldnewthing/archive/2007/11/28/6564257.aspx
    ,
    nil,
    errNumber, 0, TJwPChar(@s),
    0, nil) = 0) then
  begin
    Result := RsUnknownGetLastError;
    exit;
  end;
  Result := s;

  LocalFree(HLOCAL(s));
end;

{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}


end.
{$ENDIF SL_OMIT_SECTIONS}
