{
Description
Project JEDI Windows Security Code Library (JWSCL)

This unit contains a log mechanisms

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

The Original Code is JwsclLogging.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

}
unit JwsclLogging;

{$INCLUDE ..\includes\Jwscl.inc}
interface
uses ActiveX,
     ComServ,
     ComObj,
     Classes,
     SyncObjs,
     SysUtils,
     JwsclStrings;

type {<B>TJwLogType</B> defines log tag attribute types}
     TJwLogType = (
        lsNone,
        //logs a message. Information about ths.
        lsMessage,
        //logs a warning.
        lsWarning,
        //logs an error but not a show stopper
        lsError,
        //logs an showstopper
        lsStop);
     {<B>TJwEnterType</B> defines an enter/leave attribute type}
     TJwEnterType = (
        //unknown or unsupported enter/leave
        etNone,
        //enter or leave a function body
        etFunction,
        //enter or leave a method body
        etMethod,
        //enter or leave a thread main func. body
        etThread);

     {<B>TJwSignalType</B> defines a signal attribute type}
     TJwSignalType = (
        stNone,
        //log sent signal 
        stSend,
        //log received signal
        stReceived,
        //log wait for signal
        stWait);

     {<B>TJwMemoryType</B> defines a memory attribute type}   
     TJwMemoryType = (
        mtNone,
        //log memory allocation
        mtAlloc,
        //log memory deallocation
        mtDeAlloc);

    {<B>TJwXMLTag</B> defines known XML tag names}    
    TJwXMLTag = (
        xtLogFile,
        xtLogProcess,
        xtEnter,
        xtLeave,
        xtSignal,
        xtMemory,
        xtLog,
        xtException,
        xtType,
        xtGuid,
        xtGetLastError,
        xtWinApiFunction,
        xtLogString,
        xtComSource,
        xtStackTrace,
        xtMessage,
        xtLastErrorString,
        xtSourceProc);

    TJwXMLLogTag = (
        ltEnter, ltLeave, ltSignal, ltMemory, ltLog, ltException, ltDisabled, ltNone);

    {<B>TJwXMLAttrTag</B> defines known xml tag attributes}
    TJwXMLAttrTag = (
        atStart,
        atEnd,
        atType,
        atMethod,
        atClass,
        atFile,
        atThread,
        atSource,
        atTarget,
        atMemType
        );

     {
     <B>TJwXMLAttribute</B> defines an attribute.
      Attributes with empty name or value will be ignored
     }
     TJwXMLAttribute = record
       //<B>Value</B> defines the name of the attribute
       Name,
       //<B>Value</B> contains the content of the attribute
       Value : TJwString;
     end;

     TJwEventType = record
       {<B>TagName</B> defines an event that can be logged.}
       TagName   : TJwXMLLogTag;
       {<B>TypeValues</B> contains an or combined bit mask of enumeration constants
        from TJwLogType, TJwEnterType, TJwMemoryType or TJwSignalType.
        Set to -1 if all event type are to be logged. 
       }
       TypeValues : Integer;
     end;

     TJwEventTypes = array of TJwEventType;

     //<B>TJwXMLAttributes</B> defines an dynamic array of attributes
     TJwXMLAttributes = array of TJwXMLAttribute;


const IID_JwWriterCLASS : TGUID = '{02A5D97D-9017-45B1-A2E7-2217015536DE}';
type
      TJwXMLEventType = (
       //is called for every WriteSingleTag call
       etWriteSingleTag,
       //is called for every StartWriteMultipleTags call
       etStartWriteMultipleTags,
       //is called for every StartWriteMultipleTags call
       etEndWriteMultipleTags,
       {is called in TJwLogServerImpl.Done when the process end time is updated
       in the first xml line}
       etSetProcessEndTime);

      IJwWriterClass = interface;

     {<B>TJwOnXMLWriting</B> is called before code is processed in IJwWriterClass
      The function receives information about the xml line to be generated.
     }
     TJwOnXMLWriting = procedure(
        IJwWriterClass : IJwWriterClass;

        const EventType : TJwXMLEventType;
        var IndentLevel: Integer;
        var TagName: TJwString;
        var Value: TJwString;
        var Attributes: TJwXMLAttributes) of object;

     {<B>TJwOnXMLWrite</B> is called after the xml lines has been generated.
      Its value is stored into parameter Result. Set parameter
      result to empty if you don't want the xml line to be stored
      into the internal string list.
     }
     TJwOnXMLWrite = procedure(
        IJwWriterClass : IJwWriterClass;
        var Result : TJwString)  of object;



     IJwWriterClass = interface
     ['{02A5D97D-9017-45B1-A2E7-2217015536DE}']
        function WriteSingleTag(IndentLevel: Integer;  TagName: TJwString;
                                 Value: TJwString; Attributes: TJwXMLAttributes): TJwString; safecall;
        function StartWriteMultipleTags(IndentLevel: Integer;  TagName: TJwString;
                                        Attributes: TJwXMLAttributes): TJwString; safecall;
        function EndWriteMultipleTags: TJwString; safecall;

        procedure Done; safecall;

        function CreateObject : IJwWriterClass; safecall;

        function GetOnXMLWrite : TJwOnXMLWrite;
        procedure SetOnXMLWrite(Event : TJwOnXMLWrite);

        property OnXMLWrite : TJwOnXMLWrite read GetOnXMLWrite write SetOnXMLWrite;

        function GetOnXMLWriting : TJwOnXMLWriting;
        procedure SetOnXMLWriting(Event : TJwOnXMLWriting);

        property OnXMLWriting : TJwOnXMLWriting read GetOnXMLWriting write SetOnXMLWriting;
     end;



     {<B>TJwLogWriter</B> defines a default and base class for xml write operations
     and other util functions.
     Overwrite this class and specify its class type to CreateLogServer
     for changed behavior.
     }
     TJwLogWriter = class(TInterfacedObject, IJwWriterClass)
     protected
       fStartMTags,
       fMultipleTags : Boolean;
       fTagName : TJwString;
       fIndLevel : Integer;
       fOnXMLWrite : TJwOnXMLWrite;
       fOnXMLWriting : TJwOnXMLWriting;
     public
       {<B>WriteSingleTag</B> writes a single tag.
       @param IndLevel defines the indentation level. 
       @param TagName defines the name of the tag 
       @param Value defines the value of the tag. If empty the tag has no value 
       @param Attributes defines an array of attributes to be added to the tag 
       @return Returns the formatted xml tag. 
       }
       function WriteSingleTag(IndentLevel: Integer; TagName: TJwString;
         Value: TJwString; Attributes: TJwXMLAttributes): TJwString; safecall;

       {<B>StartWriteMultipleTags</B> starts a tag with sub tags. All subsequent calls to WriteSingleTag
        will create tag under this tag.
        Call EndWriteMultipleTags to end creating sub tags.
        Multiple sub tags is not supported directly. Instead the instance
        is created again.

        @param IndLevel defines the indentation level. 
        @param TagName defines the name of the tag 
        @param Attributes defines an array of attributes to be added to the tag 
        @return Returns the formatted xml tag. 
       }
       function StartWriteMultipleTags(IndentLevel: Integer; TagName: TJwString;
          Attributes: TJwXMLAttributes): TJwString;safecall;

       {<B>EndWriteMultipleTags</B> ends creating sub tags which was commenced by StartWriteMultipleTags.
        @return The return value is the last closing tag started by StartWriteMultipleTags 
       }
       function EndWriteMultipleTags : TJwString; safecall;

       procedure Done; safecall;

       function CreateObject : IJwWriterClass; safecall;

       {<B>AddAttribute</B> is a helper function that adds an attribute structure TJwXMLAttribute
        to an array TJwXMLAttributes.
        @param Attr receives an TJwXMLAttributes where the new attribute is added to the end 
        @param Name defines a name for the attribute. The function does not check for duplicates 
        @param Value defines the value of the attribute 
        }
       class procedure AddAttribute(var Attr : TJwXMLAttributes; const Name, Value : TJwString); virtual;

       {<B>AddAttributes</B> is a helper function that adds source code location attribute like
         classname, methodname and filename from where the log is made.
        @param Attr receives an TJwXMLAttributes where the new attribute is added to the end 
        @param ClassName defines the classname where the method is located. Can be empty
          if the method is not in a class. In this case the attribute is ignored. 
        @param Methodname defines the name of the method or function 
        @param Filename defines the filename where the method is located. The filename can contain
          a source line at the end of string succeeded by a colon. 
        }
       class procedure AddAttributes(var Attr : TJwXMLAttributes; const ClassName, MethodName, FileName : TJwString); virtual;

       {<B>FormatString</B> is a helper function that is used to format a tag value.
        It replaces line breaks with c style line breaks (\n).}
       class function FormatString(const Str : TJwString) : TJwString; virtual;

       {<B>GetThreadName</B> returns the value of the attribute thread which contains the name (or ID)
        of the thread which logged a message.
        @return Name and/or ID of thread  
         }
       class function GetThreadName : TJwString; virtual;

       {<B>CheckLogEventType</B> checks whether a log type and its type value should be logged to file.
        @param LogTag defines a log tag which is checked 
        @param LogTypeValue defines a or bitmask of types (a type like TJwLogType). 
        @param AllowedTypes defines a list of event types that can be logged 
       }
       class function CheckLogEventType(const LogTag : TJwXMLLogTag; const LogTypeValue : Integer;
            const AllowedTypes : TJwEventTypes) : Boolean; virtual;

       {<B>AddEventType</B> adds an event type to an events list.
        @param Event defines a list of events that can be logged 
        @param LogTag defines an event to be logged 
        @param TypeValues defines a or bitmask of types (like TJwLogType). 
        }
       class procedure AddEventType(var Events : TJwEventTypes; const LogTag : TJwXMLLogTag; const TypeValues : Integer); virtual;

       function GetOnXMLWrite : TJwOnXMLWrite;
       procedure SetOnXMLWrite(Event : TJwOnXMLWrite);

       function GetOnXMLWriting : TJwOnXMLWriting;
       procedure SetOnXMLWriting(Event : TJwOnXMLWriting);

     end;

     //TJwLogWriterClass = class of TJwLogWriter;

     {<B>IJwLogClient</B> defines an interface for a log client.
      Use IJwLogServer.Connect to create an instance of this interface.
      Each log function is multi thread safe. Different log calls in different threads
      will wait for each other.
     }
     IJwLogClient = interface {$IFDEF DELPHI6_UP}(IInterface){$ENDIF}
       ['{B7202309-4766-4D62-9E16-ECE5953C2AEA}']
        {<B>Log</B> creates an ordinary log entry.
         @param LogType defines the type of log entry. See TJwLogType for more information 
         @param LogMessage This parameter receives a message string that is shown in the log. 
         }
        procedure Log(const LogType : TJwLogType; const LogMessage : TJwString); overload; safecall;

         {<B>Log</B> creates an ordinary log entry.
         @param LogType defines the type of log entry. See TJwLogType for more information 
         @param LogMessage This parameter receives a message string that is shown in the log.
         }
        procedure Log(const LogMessage : TJwString; const LogType : TJwLogType = lsMessage); overload; safecall;

        {<B>Log</B> creates an ordinary log entry with log source information.
         @param LogType defines the type of log entry. See TJwLogType for more information 
         @param ClassName defines the classname where the method is located. Can be empty
          if the method is not in a class. In this case the attribute is ignored. 
         @param Methodname defines the name of the method or function 
         @param Filename defines the filename where the method is located. The filename can contain
          a source line at the end of string succeeded by a colon. 
         @param LogMessage This parameter receives a message string that is shown in the log.
         }
        procedure Log(const LogType : TJwLogType; const ClassName, MethodName, FileName, LogMessage : TJwString); overload; safecall;

        {<B>Signal</B> creates a signal log entry. A signal log entries is used for communcation logging.
         Threads or processes may use events to signal special events.
         @param SignalType defines the type of signal entry. See TJwSignalType for more information 
         @param Source defines the source of the signal, like another thread or process ID. 
         @param Target defines the target of the signal, like another thread or process ID. 
         @param LogMessage This parameter receives a message string that is shown in the log.
         }
        procedure Signal(const SignalType : TJwSignalType; const Source, Target, LogMessage : TJwString); overload; safecall;

        {<B>Signal</B> creates a signal log entry. A signal log entries is used for communcation logging.
         Threads or processes may use events to signal special events.
         @param LogType defines the type of log entry. See TJwLogType for more information 
         @param Source defines the source of the signal, like another thread or process ID. 
         @param Target defines the target of the signal, like another thread or process ID. 
         @param ClassName defines the classname where the method is located. Can be empty
          if the method is not in a class. In this case the attribute is ignored. 
         @param Methodname defines the name of the method or function 
         @param Filename defines the filename where the method is located. The filename can contain
          a source line at the end of string succeeded by a colon. 
         @param LogMessage This parameter receives a message string that is shown in the log.
         }
        procedure Signal(const SignalType : TJwSignalType; const Source, Target, ClassName, MethodName, FileName, LogMessage : TJwString); overload; safecall;

        {<B>Memory</B> creates a memory information log entry.
         Application can log allocations and deallocations of memory for leak search. 

         @param MemoryType defines the type of memory allocation. See TJwMemoryType for more information 
         @param MemType defines which type of (de-)allocation mechanism is used (like GetMem) 
         @param LogMessage defines the message to be shown. 
         }
        procedure Memory(const MemoryType : TJwMemoryType; const MemType, LogMessage : TJwString); overload; safecall;

        {<B>Memory</B> creates a memory information log entry with log source information.
         Application can log allocations and deallocations of memory for leak search. 

         @param MemoryType defines the type of memory allocation. See TJwMemoryType for more information 
         @param MemType defines which type of (de-)allocation mechanism is used (like GetMem) 
         @param ClassName defines the classname where the method is located. Can be empty
          if the method is not in a class. In this case the attribute is ignored. 
         @param Methodname defines the name of the method or function 
         @param Filename defines the filename where the method is located. The filename can contain
          a source line at the end of string succeeded by a colon. 
         @param LogMessage defines the message to be shown. 
         }
        procedure Memory(const MemoryType : TJwMemoryType; const MemType, ClassName, MethodName, FileName, LogMessage : TJwString); overload; safecall;

        {<B>Exception</B> creates an exception information log entry.

         @param E contains an exception object which content is logged.
            The object can be of class EJwsclSecurityException. In this case more information is available in the log. 
         @param LogMessage defines the message to be shown. 
         }
        procedure Exception(const E : Exception); overload; safecall;

        {<B>Exception</B> creates an exception information log entry with log source information.

         @param E contains an exception object which content is logged.
            The object can be of class EJwsclSecurityException. In this case more information is available in the log. 
         @param ClassName defines the classname where the method is located. Can be empty
          if the method is not in a class. In this case the attribute is ignored. 
         @param Methodname defines the name of the method or function 
         @param Filename defines the filename where the method is located. The filename can contain
          a source line at the end of string succeeded by a colon. 
         @param LogMessage defines the message to be shown. 
         }
        procedure Exception(const E : Exception; const ClassName, MethodName, FileName : TJwString); overload; safecall;

        {<B>SetEventTypes</B> sets the type of events which are logged of this log client}
        procedure SetEventTypes(const EventTypes : TJwEventTypes); safecall;
     end;

     IJwLogServer = interface {$IFDEF DELPHI6_UP}(IInterface){$ENDIF}
       ['{1B3EC217-2F6D-4FE2-A9DC-BF7E8C025D4F}']
       {<B>Connect</B> creates a new log client. A log client (IJwLogClient) provides access to logging mechanisms.
        Use this function at the beginning of a process, thread or function start. Obtain an instance
        of IJwLogClient and start to log information.

        Warning:
          Do not store the result into a global variable. The reason is that the global interface
          variable will be destroyed after the LogServer has been freed. This may lead to an assertion
          (in TJwLogServerImpl.EnterCriticalSection).

        @param EnterType define of which kind this log client consists. A log client automatically
         creates an enter tag using this parameter. Specify etNone to do not create an enter log.
         If the instance is destroyed the log client will automatically create
         a leave tag. 
        @param ClassName defines the classname where the method is located. Can be empty
          if the method is not in a class. In this case the attribute is ignored. 
        @param Methodname defines the name of the method or function 
        @param Filename defines the filename where the method is located. The filename can contain
          a source line at the end of string succeeded by a colon. 
        @param MessageText defines the message to be used for the enter log entry. 
       }
       function Connect(const EnterType : TJwEnterType; const ClassName, MethodName, FileName, MessageText : TJwString) : IJwLogClient; safecall;

       {<B>Disconnect</B> nils the log instance and frees it if reference count is zero.}
       procedure Disconnect(var Client : IJwLogClient); safecall;

       {<B>Done</B> stops the logging server and shut it down.}
       procedure Done; SafeCall;

       {<B>SetLogTypes</B> sets type of log events which are logged}
       procedure SetLogTypes(const LogTypes : TJwEventTypes); SafeCall;
       {<B>GetLogTypes</B> gets type of log events which are logged}
       function GetLogTypes : TJwEventTypes; SafeCall;
       {<B>SetWriterClass</B> changes the writer class.
        The writer class is called when an log event must be written to a database
         (file, xml, db)
       }
       procedure SetWriterClass(const Writer : IJwWriterClass); SafeCall;
       {<B>GetWriterClass</B> gets the writer class.}
       function GetWriterClass : IJwWriterClass; SafeCall;

       function GetOnXMLWrite : TJwOnXMLWrite;
       procedure SetOnXMLWrite(Event : TJwOnXMLWrite);

       property OnXMLWrite : TJwOnXMLWrite read GetOnXMLWrite write SetOnXMLWrite;

       function GetOnXMLWriting : TJwOnXMLWriting;
       procedure SetOnXMLWriting(Event : TJwOnXMLWriting);

       property OnXMLWriting : TJwOnXMLWriting read GetOnXMLWriting write SetOnXMLWriting;
     end;

{<B>SetEventTypesEnabled</B> enables or disables logging. It just does not write output.}
procedure SetEventTypesEnabled(const LogServer : IJwLogServer; const Enabled : Boolean);

function GetEventTypesEnabled(const LogServer : IJwLogServer) : Boolean;


{<B>CreateLogServer</B> creates a new log server that can hold several log clients (IJwLogClient).
A log sever creates a new logprocess tag and closes it if it is destroyed.
It saves each xml tag into a new string list item in parameter Elements. This
behavior can be overwritten by using a non default WriterClass (TJwLogWriterClass)
@param Elements receives the xml tags. Each item contains a whole line of an xml tag.
  Must not be nil if parameter WriterClass is left nil.
   
@param LogTypes receives a list of TJwEventType records that contains tags and its
attributes which ought to be logged.
If the array is empty all types of events are logged.
 

@param OnXMLWrite is a callback method that will be called when an xml
 line is going to be stored into the log file. The xml line can be adapted or even removed 

@param OnXMLWriting is a callback method that will be called when an xml
 line is going to be created. Some parameters can be changed. 

@param WriterClass defines a custom class that can be used to change the
default mechanism how xml is stored. By default (if nil) the TJwLogWriterClass
uses a string list implementation to store xml. 

@return Returns an instance of IJwLogServer for logging information 

}
function CreateLogServer(Elements : TStringList;
  const LogTypes : TJwEventTypes;
  const OnXMLWrite : TJwOnXMLWrite = nil;
  const OnXMLWriting : TJwOnXMLWriting = nil;
  const WriterClass : IJwWriterClass = nil) : IJwLogServer;


var //JwStrNewLine : AnsiString = #13#10;
    //identation string (default #9 = tabulator char)
    JwStrIdent : TJwString = #9;

    //format of start attribute value for processlog tag
    JwTimeOutputString : TJwString = 'dd.mm.yyyy hh:nn:ss:zzz';

    //strings for log tag value
    JwLogTypeStrings : array[TJwLogType] of TJwString = (
         '',
         'message',
         'warning',
         'error',
         'stop');

    //strings for enter/leave tag type value
    JwEnterTypeString : array[TJwEnterType] of TJwString = (
         '',
         'function',
         'method',
         'thread');

    //strings for signal tag type value
    JwSignalTypeString : array[TJwSignalType] of TJwString = (
         '',
         'send',
         'received',
         'wait');

    //strings for memory type value
    JwMemoryTypeString : array[TJwMemoryType] of TJwString = (
         '',
         'alloc',
         'dealloc');

    //strings for xml tags
    JwXMLTagsString : array[TJwXMLTag] of TJwString = (
        'logfile',
        'logprocess',
        'enter',
        'leave',
        'signal',
        'memory',
        'log',
        'exception',
        'type',
        'guid',
        'getlasterror',
        'winapifunction',
        'logstring',
        'comsource',
        'stacktrace',
        'message',
        'getlasterrorstring',
        'sourceproc');

    //strings type attribute names
    JwXMLAttributeString : array[TJwXMLAttrTag] of TJwString = (
        'start',
        'end',
        'type',
        'method',
        'class',
        'file',
        'thread',
        'source',
        'target',
        'memtype');

    JwXMLLogTagString : array[TJwXMLLogTag] of TJwString = (
       'ltEnter', 'ltLeave', 'ltSignal', 'ltMemory', 'ltLog', 'ltException', 'ltDisabled', 'ltNone');


implementation
uses JwaWindows, JwsclExceptions, JwsclUtils, D5impl;


type TJwLogServerImpl = class;

     TThreadMapRec = record
       //thread id
       ThreadID : DWORD;
       //identation 
       Ident : Integer;
     end;
     TThreadMapRecs = array of TThreadMapRec;



     TJwLogClientImpl = class(TInterfacedObject, IJwLogClient)
     protected
       fOwner : TJwLogServerImpl;
       fInd : Integer;
       fClassName,
       fMethodName, fFileName, fMessageText : TJwString;
       fEnterType : TJwEnterType;
       fWriter : IJwWriterClass;

       //ID connects enter and leave tag unambiguously
       fID : {Int64}Integer; //int64 with InterlockedIncrement64 is only supported in newer os
       fEventTypes : TJwEventTypes;

     public
       constructor Create(Owner : TJwLogServerImpl;
          const EnterType : TJwEnterType;
          const EventTypes : TJwEventTypes;
          const ClassName, MethodName, FileName, MessageText : TJwString);
       destructor Destroy; override;

       procedure Log(const LogType : TJwLogType; const LogMessage : TJwString); overload; safecall;
       procedure Log(const LogType : TJwLogType; const ClassName, MethodName, FileName, LogMessage : TJwString); overload; safecall;
       procedure Log(const LogMessage : TJwString; const LogType : TJwLogType = lsMessage); overload; safecall;

       procedure Signal(const SignalType : TJwSignalType; const Source, Target, LogMessage : TJwString); overload; safecall;
       procedure Signal(const SignalType : TJwSignalType; const Source, Target, ClassName, MethodName, FileName, LogMessage : TJwString); overload; safecall;

       procedure Memory(const MemoryType : TJwMemoryType; const MemType, LogMessage : TJwString); overload; safecall;
       procedure Memory(const MemoryType : TJwMemoryType; const MemType, ClassName, MethodName, FileName, LogMessage : TJwString); overload; safecall;

       procedure Exception(const E : Exception); overload; safecall;
       procedure Exception(const E : Exception; const ClassName, MethodName, FileName: TJwString); overload; safecall;

       procedure SetEventTypes(const EventTypes : TJwEventTypes); safecall;

       function AddToList(const S : TJwString) : Integer;
     end;

     TJwLogServerImpl = class(TInterfacedObject, IJwLogServer)
     protected
       fElements : TStringList;
//       fWriterClass : IJwWriterClass;
       fWriter : IJwWriterClass;
       fIndents : TThreadMapRecs;

       fEventTypes : TJwEventTypes;

       fCritical : SyncObjs.TCriticalSection;
       //ID connects enter and leave tag unambiguously
       fID : {Int64}Integer; //int64 with InterlockedIncrement64 is only supported in newer os

       fIdx : Integer;
       fOnXMLWrite : TJwOnXMLWrite;
       fOnXMLWriting : TJwOnXMLWriting;

       procedure EnterCriticalSection;
       procedure LeaveCriticalSection;

       //gets identation for a specific thread
       function GetIdentByThread : Integer;
       //sets or adds identation for a specific thread
       procedure SetIdent(Ident : Integer);

       //<B>GetID</B> creates a new unambiguous ID threadsafe.
       function GetID : Int64;

       function GetOnXMLWrite : TJwOnXMLWrite;
       procedure SetOnXMLWrite(Event : TJwOnXMLWrite);

       function GetOnXMLWriting : TJwOnXMLWriting;
       procedure SetOnXMLWriting(Event : TJwOnXMLWriting);


     public
       constructor Create(const Elements : TStringList;
         const EventTypes : TJwEventTypes;
         const OnXMLWrite : TJwOnXMLWrite;
         const OnXMLWriting : TJwOnXMLWriting;
         const WriterClass : IJwWriterClass);
       destructor Destroy; override;

       function Connect(const EnterType : TJwEnterType; const ClassName, MethodName, FileName, MessageText : TJwString) : IJwLogClient;safecall;
       procedure Disconnect(var Client : IJwLogClient); safecall;

       procedure Done; SafeCall;

       procedure SetLogTypes(const LogTypes : TJwEventTypes); SafeCall;
       function GetLogTypes : TJwEventTypes; SafeCall;
       procedure SetWriterClass(const Writer : IJwWriterClass); SafeCall;
       function GetWriterClass : IJwWriterClass; SafeCall;

     end;

function CreateLogServer(Elements : TStringList;
  const LogTypes : TJwEventTypes;
  const OnXMLWrite : TJwOnXMLWrite = nil;
  const OnXMLWriting : TJwOnXMLWriting = nil;
  const WriterClass : IJwWriterClass = nil) : IJwLogServer;
begin
  result := TJwLogServerImpl.Create(Elements,LogTypes, OnXMLWrite, OnXMLWriting, WriterClass);
end;


procedure SetEventTypesEnabled(const LogServer : IJwLogServer; const Enabled : Boolean);
var i : Integer;
    Events : TJwEventTypes;
begin
  //make multithread safe by copy events - GetLogTypes is thread safe
  Events := LogServer.GetLogTypes;

  for i := low(Events) to High(Events) do
  begin
    if Enabled and (Events[i].TagName = ltDisabled) then
    begin
      Events[i].TagName := ltNone;
      LogServer.SetLogTypes(Events);
      exit;
    end
    else
    if not Enabled and (Events[i].TagName = ltNone) then
    begin
      Events[i].TagName := ltDisabled;
      LogServer.SetLogTypes(Events);
      exit;
    end;
  end;

  if not Enabled then
  begin
    TJwLogWriter.AddEventType(Events, JwsclLogging.ltDisabled, -1);
    //make multithread safe by using copy - SetLogTypes is thread safe
    LogServer.SetLogTypes(Events);
  end;
end;

function GetEventTypesEnabled(const LogServer : IJwLogServer) : Boolean;
var i : Integer;
    Events : TJwEventTypes;
begin
  //make multithread safe by copy events - GetLogTypes is thread safe
  Events := LogServer.GetLogTypes;

  result := true;
  for i := low(Events) to High(Events) do
  begin
    if Events[i].TagName = ltDisabled then
    begin
      result := false;
      exit;
    end;
  end;
end;






{ TJwLogClientImpl }

constructor TJwLogClientImpl.Create(Owner : TJwLogServerImpl;
  const EnterType : TJwEnterType;
  const EventTypes : TJwEventTypes;
  const ClassName, MethodName, FileName, MessageText : TJwString);
var Attributes : TJwXMLAttributes;
begin
  fOwner := Owner;
  fClassName := ClassName;
  fMethodName := MethodName;
  fFileName := FileName;
  fMessageText := MessageText;
  fEnterType := EnterType;

  fEventTypes := EventTypes;

  fID := fOwner.GetID; //thread safe

  fWriter := fOwner.fWriter.CreateObject;

  fOwner.EnterCriticalSection;
  try
    fInd := fOwner.GetIdentByThread; //deep

    if (EnterType <> etNone) and
      TJwLogWriter.CheckLogEventType(ltEnter, Integer(0), fEventTypes) then
    begin
      TJwLogWriter.AddAttribute(Attributes, JwXMLAttributeString[atType], JwEnterTypeString[EnterType]);
      TJwLogWriter.AddAttribute(Attributes, JwXMLAttributeString[atSource], IntToStr(fID));
      TJwLogWriter.AddAttributes(Attributes, fClassName, fMethodName, fFileName);
      TJwLogWriter.AddAttribute(Attributes, JwXMLAttributeString[atThread], TJwLogWriter.GetThreadName);

      AddToList(fWriter.WriteSingleTag(fInd, JwXMLTagsString[xtEnter], MessageText, Attributes));

      Inc(fInd); //deeper identation for sub tags
      fOwner.SetIdent(fInd); //set parent identation for this thread
    end;

  finally
     fOwner.LeaveCriticalSection
  end;
end;

destructor TJwLogClientImpl.Destroy;
var Attributes : TJwXMLAttributes;
begin
  fOwner.EnterCriticalSection;
  try
    if (fEnterType <> etNone) and
      TJwLogWriter.CheckLogEventType(ltLeave, Integer(0), fEventTypes) then
    begin

      //get one step back of identation
      fOwner.SetIdent(fInd-1);

      TJwLogWriter.AddAttribute(Attributes, JwXMLAttributeString[atType], JwEnterTypeString[fEnterType]);
      TJwLogWriter.AddAttribute(Attributes, JwXMLAttributeString[atSource], IntToStr(fID));
      TJwLogWriter.AddAttributes(Attributes, fClassName, fMethodName, fFileName);
      TJwLogWriter.AddAttribute(Attributes, JwXMLAttributeString[atThread], TJwLogWriter.GetThreadName);
      try
        AddToList(fWriter.WriteSingleTag(fInd-1, JwXMLTagsString[xtLeave], fMessageText, Attributes));
      except
        raise;
      end;
    end;
  finally
    fOwner.LeaveCriticalSection;
  end;

  inherited;
end;

procedure TJwLogClientImpl.Log(const LogType: TJwLogType;
  const LogMessage: TJwString);
begin
  Log(LogType, fClassName, fMethodName, fFileName, LogMessage);
end;

procedure TJwLogClientImpl.Log(const LogType : TJwLogType; const ClassName, MethodName, FileName, LogMessage : TJwString); safecall;
var
Attributes : TJwXMLAttributes;
begin
  fOwner.EnterCriticalSection;
  try
    if not TJwLogWriter.CheckLogEventType(ltLog, Integer(LogType), fEventTypes) then
    begin
      {
      In XP SP2, calling LeaveCriticalSection twice results in an
       hang in EnterCriticalSection, next time executed.

      finally is executed here, so we don't call LeaveCriticalSection here
      }
      exit;
    end;    

    TJwLogWriter.AddAttribute(Attributes, JwXMLAttributeString[atType],JwLogTypeStrings[LogType]);
    TJwLogWriter.AddAttributes(Attributes, ClassName, MethodName, FileName);
    TJwLogWriter.AddAttribute(Attributes, JwXMLAttributeString[atThread], TJwLogWriter.GetThreadName);
    AddToList(fWriter.WriteSingleTag(fInd, JwXMLTagsString[xtLog], LogMessage, Attributes));
{$IFDEF DEBUG}
{$IFDEF UNICODE}OutputDebugStringW{$ELSE}OutputDebugStringA{$ENDIF}(
        TJwPChar(JwFormatString('[%0:s] %1:s::%2:s.%3:s: %4:s',
          [JwLogTypeStrings[LogType], //0
           FileName,   //1
           ClassName,  //2
           MethodName, //3
           LogMessage  //4
          ])));
{$ENDIF DEBUG}
  finally
    fOwner.LeaveCriticalSection;
  end;
end;

procedure TJwLogClientImpl.Log(const LogMessage: TJwString;
  const LogType: TJwLogType = lsMessage);
begin
  Log(LogType, LogMessage);
end;



procedure TJwLogClientImpl.Memory(const MemoryType: TJwMemoryType;
  const MemType, LogMessage: TJwString);
begin
  Memory(MemoryType, MemType, fClassName, fMethodName, fFileName, LogMessage);
end;

procedure TJwLogClientImpl.Memory(const MemoryType: TJwMemoryType;
  const MemType, ClassName, MethodName, FileName, LogMessage: TJwString);
var Attributes : TJwXMLAttributes;
begin
  fOwner.EnterCriticalSection;
  try
    if not TJwLogWriter.CheckLogEventType(ltMemory, Integer(MemoryType), fEventTypes) then
    begin
      {
      In XP SP2, calling LeaveCriticalSection twice results in an
       hang in EnterCriticalSection, next time executed.

      finally is executed here, so we don't call LeaveCriticalSection here
      }
      exit;
    end;

    TJwLogWriter.AddAttribute(Attributes, JwXMLAttributeString[atType], JwMemoryTypeString[MemoryType]);
    TJwLogWriter.AddAttribute(Attributes, JwXMLAttributeString[atMemType], MemType);
    TJwLogWriter.AddAttributes(Attributes, ClassName, MethodName, FileName);
    TJwLogWriter.AddAttribute(Attributes, JwXMLAttributeString[atThread], TJwLogWriter.GetThreadName);
    
    AddToList(fWriter.WriteSingleTag(fInd, JwXMLTagsString[xtMemory], LogMessage, Attributes));
  finally
    fOwner.LeaveCriticalSection;
  end;
end;

procedure TJwLogClientImpl.Signal(const SignalType: TJwSignalType;
  const Source, Target, LogMessage: TJwString);
begin
  Signal(SignalType, Source, Target, fClassName, fMethodName, fFileName, LogMessage);
end;

procedure TJwLogClientImpl.Signal(const SignalType: TJwSignalType;
  const Source, Target, ClassName, MethodName, FileName,
  LogMessage: TJwString);
var Attributes : TJwXMLAttributes;
begin
  fOwner.EnterCriticalSection;
  try
    if not TJwLogWriter.CheckLogEventType(ltSignal, Integer(SignalType), fEventTypes) then
    begin
      {
      In XP SP2, calling LeaveCriticalSection twice results in an
       hang in EnterCriticalSection, next time executed.

      finally is executed here, so we don't call LeaveCriticalSection here
      }
      exit;
    end;

    TJwLogWriter.AddAttribute(Attributes, JwXMLAttributeString[atType], JwSignalTypeString[SignalType]);
    TJwLogWriter.AddAttribute(Attributes, JwXMLAttributeString[atSource], Source);
    TJwLogWriter.AddAttribute(Attributes, JwXMLAttributeString[atTarget], Target);
    TJwLogWriter.AddAttributes(Attributes, ClassName, MethodName, FileName);
    TJwLogWriter.AddAttribute(Attributes, JwXMLAttributeString[atThread], TJwLogWriter.GetThreadName);
    AddToList(fWriter.WriteSingleTag(fInd, JwXMLTagsString[xtSignal], LogMessage, Attributes));
  finally
    fOwner.LeaveCriticalSection;
  end;
end;


procedure TJwLogClientImpl.Exception(const E: Exception);
begin
  Exception(E, fClassName, fMethodName, fFileName);
end;

procedure TJwLogClientImpl.Exception(const E: Exception; const ClassName,
  MethodName, FileName: TJwString);
var Attributes : TJwXMLAttributes;
    Writer : IJwWriterClass;
    JE : EJwsclSecurityException;
begin
  fOwner.EnterCriticalSection;
  try
    if not TJwLogWriter.CheckLogEventType(ltException, Integer(0), fEventTypes) then
    begin
      {
      In XP SP2, calling LeaveCriticalSection twice results in an
       hang in EnterCriticalSection, next time executed.

      finally is executed here, so we don't call LeaveCriticalSection here
      }
      exit;
    end;


    Writer := fWriter.CreateObject;



    TJwLogWriter.AddAttributes(Attributes, ClassName, MethodName, FileName);
    TJwLogWriter.AddAttribute(Attributes, JwXMLAttributeString[atThread], TJwLogWriter.GetThreadName);

    AddToList(fWriter.StartWriteMultipleTags(fInd, JwXMLTagsString[xtException], Attributes));

    AddToList(fWriter.WriteSingleTag(fInd,
        JwXMLTagsString[xtType], E.ClassName, Attributes));

    AddToList(fWriter.WriteSingleTag(fInd,
        JwXMLTagsString[xtGuid], GUIDToString(JwMapException(E.ClassName)) , Attributes));

    if E is EJwsclSecurityException then
    begin
      JE := E as EJwsclSecurityException;

      AddToList(fWriter.WriteSingleTag(fInd,
        JwXMLTagsString[xtGetLastError], IntToStr(JE.LastError), Attributes));

      AddToList(fWriter.WriteSingleTag(fInd,
        JwXMLTagsString[xtLastErrorString], JE.GetLastErrorMessage(JE.LastError), Attributes));

      if Length(Trim(JE.WinCallName)) > 0 then
        AddToList(fWriter.WriteSingleTag(fInd,
          JwXMLTagsString[xtWinApiFunction], JE.WinCallName, Attributes));

      if Length(Trim(JE.Log)) > 0 then
        AddToList(fWriter.WriteSingleTag(fInd,
          JwXMLTagsString[xtLogString], JE.Log , Attributes));

      if Length(Trim(JE.ComSource)) > 0 then
        AddToList(fWriter.WriteSingleTag(fInd,
          JwXMLTagsString[xtComSource], JE.ComSource, Attributes));

      if Length(Trim(JE.StackTrace)) > 0 then
        AddToList(fWriter.WriteSingleTag(fInd,
          JwXMLTagsString[xtStackTrace], JE.StackTrace , Attributes));

      if Length(Trim(JE.SourceProc)) > 0 then
        AddToList(fWriter.WriteSingleTag(fInd,
          JwXMLTagsString[xtSourceProc], JE.SourceProc , Attributes));

    end
    else
    if E is EOSError then
    begin
      AddToList(fWriter.WriteSingleTag(fInd,
        JwXMLTagsString[xtGetLastError], IntToStr((E as EOSError).ErrorCode), Attributes));
    end
    else
    begin

    end;
    OutputDebugStringW(PWideChar(WideString(E.Message)));

    AddToList(fWriter.WriteSingleTag(fInd,
        JwXMLTagsString[xtMessage], E.Message, Attributes));
    AddToList(fWriter.EndWriteMultipleTags);


    //AddToList(fOwner.fWriter.WriteSingleTag(fOwner.fInd,
  finally
    fOwner.LeaveCriticalSection;
  end;
end;


procedure TJwLogClientImpl.SetEventTypes(const EventTypes: TJwEventTypes);
begin
  fEventTypes := EventTypes;
end;

function TJwLogClientImpl.AddToList(const S: TJwString): Integer;
begin
  result := 0;
  if Assigned(fOwner.fElements) and (Length(S) > 0) then
    result := fOwner.fElements.Add(S);
end;

{ TJwLogServer }

function TJwLogServerImpl.Connect(const EnterType : TJwEnterType; const ClassName, MethodName, FileName, MessageText : TJwString): IJwLogClient;
begin
  result := TJwLogClientImpl.Create(Self, EnterType, fEventTypes, ClassName, MethodName, FileName, MessageText);
end;



constructor TJwLogServerImpl.Create;
var Attributes : TJwXMLAttributes;
    S : String;  //in Tiburon this String is WideString. no prob
begin                                               
  fCritical := SyncObjs.TCriticalSection.Create;
  fElements := Elements;

  fID := 0;
  SetIdent(2); //logprocess' sub tags ident 2 tabs

  fEventTypes := EventTypes;

  if Assigned(WriterClass) then
    fWriter := WriterClass
  else
    fWriter := TJwLogWriter.Create;

  fWriter.OnXMLWrite := OnXMLWrite;
  fWriter.OnXMLWriting := OnXMLWriting;
  fOnXMLWrite := OnXMLWrite;
  fOnXMLWriting := OnXMLWriting;


  DateTimeToString(S, String(JwTimeOutputString), Now);

  TJwLogWriter.AddAttribute(Attributes, JwXMLAttributeString[atStart], S);
  TJwLogWriter.AddAttribute(Attributes, JwXMLAttributeString[atThread], TJwLogWriter.GetThreadName);
  TJwLogWriter.AddAttribute(Attributes, JwXMLAttributeString[atEnd], '%s'); //save space for end date and time

  S := fWriter.StartWriteMultipleTags(1, JwXMLTagsString[xtLogProcess],Attributes);
  if Assigned(fElements) and (Length(S) > 0) then
    fIdx := fElements.Add(S);
end;

destructor TJwLogServerImpl.Destroy;
begin
  Done;
  inherited;
end;

procedure TJwLogServerImpl.Done;
var
  Attributes : TJwXMLAttributes;
  Str : String;
  //WideStr : WideString;
  S : TJwString;

  Value : TJwString;
  IndentLevel: Integer;
  TagName: TJwString;
begin
  if not Assigned(fCritical) then
    exit;

  DateTimeToString(Str, String(JwTimeOutputString), Now);
{$IFDEF UNICODE}
  S := WideString(Str);
{$ELSE}
  {WARNING: may lose information in Unicode Delphi}
  S := Str;
{$ENDIF}

  //Send end process time 
  if Assigned(fOnXMLWriting) then
    fOnXMLWriting(nil,etSetProcessEndTime,
        IndentLevel, TagName,
        S, Attributes);

  if (fIdx >= 0) and Assigned(fElements) then
  try
    //write end date and time into formatted string from .Create
    Value := JwFormatStringEx(fElements[fIdx], [S]);
    fElements[fIdx] := Value;
  except
  end;

  S := fWriter.EndWriteMultipleTags;

  if Assigned(fElements) and (Length(S) > 0) then
    fElements.Add(S);

  fWriter.Done;
  fWriter := nil;

  FreeAndNil(fCritical);
end;

procedure TJwLogServerImpl.Disconnect(var Client: IJwLogClient);
begin
  //TJwLogClientImpl(Client).Free;
  //Client._Release;
  Client := nil;
end;

procedure TJwLogServerImpl.EnterCriticalSection;
begin
  Assert(Assigned(fCritical), 'LogServer is no more active.');
  if Assigned(fCritical) then
    fCritical.Enter; 
end;

function TJwLogServerImpl.GetIdentByThread: Integer;
var i : Integer;
    ID : DWORD;
begin
  result := 0;
  ID := GetCurrentThreadId;
  for i := low(fIndents) to high(fIndents) do
   if fIndents[i].ThreadID = ID then
     result := fIndents[i].Ident;
end;

function TJwLogServerImpl.GetLogTypes: TJwEventTypes;
begin
  EnterCriticalSection;
  try
    result := fEventTypes;
  finally
   LeaveCriticalSection;
  end;
end;

function TJwLogServerImpl.GetWriterClass: IJwWriterClass;
begin
  EnterCriticalSection;
  try
    result := fWriter;
  finally
   LeaveCriticalSection;
  end;
end;

procedure TJwLogServerImpl.SetIdent(Ident: Integer);
var i : Integer;
    ID : DWORD;
begin
  ID := GetCurrentThreadId;
  for i := low(fIndents) to high(fIndents) do
    if fIndents[i].ThreadID = ID then
    begin
      fIndents[i].Ident := Ident;
      exit;
    end;

  SetLength(fIndents, Length(fIndents)+1);
  fIndents[high(fIndents)].ThreadID := ID;
  fIndents[high(fIndents)].Ident := Ident;
end;

procedure TJwLogServerImpl.SetLogTypes(const LogTypes: TJwEventTypes);
begin
  EnterCriticalSection;
  try
    fEventTypes := LogTypes;
  finally
   LeaveCriticalSection;
  end;
end;

procedure TJwLogServerImpl.SetWriterClass(const Writer: IJwWriterClass);
begin
  EnterCriticalSection;
  try
    fWriter := Writer;
  finally
   LeaveCriticalSection;
  end;
end;

function TJwLogServerImpl.GetOnXMLWrite : TJwOnXMLWrite;
begin
  result := fOnXMLWrite;
end;
procedure TJwLogServerImpl.SetOnXMLWrite(Event : TJwOnXMLWrite);
begin
  fOnXMLWrite := Event;
end;


function TJwLogServerImpl.GetID : Int64;
//var I : Integer;
begin
//  try
    //some OS does not support functions
    //which this function is calls
   // InterlockedIncrement64(fID);
   InterlockedIncrement(fID);
{  except
    InterlockedIncrement(fID);
  end;}
  result := fID;
end;


procedure TJwLogServerImpl.LeaveCriticalSection;
begin
  Assert(Assigned(fCritical), 'LogServer is no more active.');
  if Assigned(fCritical) then
    fCritical.Leave;
end;





{ TJwLogWriter }

class procedure TJwLogWriter.AddAttribute(var Attr: TJwXMLAttributes;
  const Name, Value: TJwString);
begin
  SetLength(Attr, Length(Attr)+1);
  Attr[High(Attr)].Name := Name;
  Attr[High(Attr)].Value := Value;
end;

class procedure TJwLogWriter.AddAttributes(var Attr : TJwXMLAttributes; const ClassName, MethodName, FileName : TJwString);
begin
  if Length(ClassName) > 0 then
  begin
    SetLength(Attr, Length(Attr)+1);
    Attr[High(Attr)].Name := JwXMLAttributeString[atClass];
    Attr[High(Attr)].Value := ClassName;
  end;

  if Length(MethodName) > 0 then
  begin
    SetLength(Attr, Length(Attr)+1);
    Attr[High(Attr)].Name := JwXMLAttributeString[atMethod];
    Attr[High(Attr)].Value := MethodName;
  end;

  if Length(FileName) > 0 then
  begin
    SetLength(Attr, Length(Attr)+1);
    Attr[High(Attr)].Name := JwXMLAttributeString[atFile];
    Attr[High(Attr)].Value := FileName;
  end;
end;

class procedure TJwLogWriter.AddEventType(var Events: TJwEventTypes;
  const LogTag: TJwXMLLogTag; const TypeValues: Integer);
begin
  SetLength(Events, Length(Events)+1);
  Events[high(Events)].TagName := LogTag;
  Events[high(Events)].TypeValues := TypeValues;
end;

function TJwLogWriter.GetOnXMLWrite : TJwOnXMLWrite;
begin
  result := fOnXMLWrite;
end;
procedure TJwLogWriter.SetOnXMLWrite(Event : TJwOnXMLWrite);
begin
  fOnXMLWrite := Event;
end;


class function TJwLogWriter.CheckLogEventType(const LogTag: TJwXMLLogTag;
  const LogTypeValue: Integer; const AllowedTypes: TJwEventTypes): Boolean;

//var i,i2 : Integer;
var i : Integer;
begin
  if Length(AllowedTypes) = 0 then
  begin
    result := true;
    exit;
  end;
  
  result := false;
  for i := low(AllowedTypes) to high(AllowedTypes) do
  begin
    if AllowedTypes[i].TagName = ltDisabled then
    begin
      exit;
    end;
    
    if AllowedTypes[i].TagName = LogTag then
    begin
      result := (AllowedTypes[i].TypeValues and LogTypeValue) = LogTypeValue;
      exit;
    end;
  end;
end;

function TJwLogWriter.CreateObject: IJwWriterClass;
begin
  result := TJwLogWriter.Create;
  result.OnXMLWrite := Self.fOnXMLWrite;
  result.OnXMLWriting := Self.fOnXMLWriting;

end;

procedure TJwLogWriter.Done;
begin

end;

function TJwLogWriter.EndWriteMultipleTags(): TJwString;
var i : Integer;
 IndentLevel: Integer;
 TagName: TJwString;
 Value: TJwString;
 Attributes: TJwXMLAttributes;
begin
  IndentLevel := fIndLevel;


  if Assigned(fOnXMLWriting) then
    fOnXMLWriting(Self,
        etEndWriteMultipleTags,//const EventType : TJwXMLEventType;
        IndentLevel,//var IndentLevel: Integer;
        TagName,//var TagName: TJwString;
        Value,//var Value: TJwString;
        Attributes//var Attributes: TJwXMLAttributes
        );

  result := '';
  for i := 1 to fIndLevel-1 do
    result := JwStrIdent + result;
  result := result + '</'+fTagName+'>';

  fMultipleTags := false;

  if Assigned(fOnXMLWrite) then
    fOnXMLWrite(Self, Result);
end;

class function TJwLogWriter.FormatString(const Str: TJwString): TJwString;
var i : Integer;
begin
  result := Str;
  for i := Length(Str) downto 0 do
  begin
    if result[i] = #10 then
      System.Delete(result, i,1)
    else
    if result[i] = #13 then
    begin
      result[i] := '\';
      System.Insert('n',result,i+1);
    end;
  end;
end;

class function TJwLogWriter.GetThreadName: TJwString;
begin
  result := JwGetThreadName + ' ('+IntToStr(GetCurrentThreadId)+')';
  if Length(Trim(JwGetThreadName)) = 0 then
    result := IntToStr(GetCurrentThreadId);
end;

function TJwLogWriter.StartWriteMultipleTags(IndentLevel: Integer; TagName: TJwString;
          Attributes: TJwXMLAttributes): TJwString;
var Value: TJwString;
begin
  if Assigned(fOnXMLWriting) then
    fOnXMLWriting(Self,
        etStartWriteMultipleTags,//const EventType : TJwXMLEventType;
        IndentLevel,//var IndentLevel: Integer;
        TagName,//var TagName: TJwString;
        Value,//var Value: TJwString;
        Attributes//var Attributes: TJwXMLAttributes
        );

  fIndLevel := IndentLevel+1;
  fTagName := TagName;
   fStartMTags := true;
   result := WriteSingleTag(IndentLevel, TagName, '', Attributes);
   fStartMTags := false;
  fMultipleTags := true;

  {
  Already done in WriteSingleTag
  if Assigned(fOnXMLWrite) then
    fOnXMLWrite(Self, Result);  }
end;

function TJwLogWriter.WriteSingleTag(IndentLevel: Integer; TagName: TJwString;
                                Value: TJwString; Attributes: TJwXMLAttributes): TJwString;
var S, AttributesLine : TJwString;
    i : Integer;

begin
  if Assigned(fOnXMLWriting) then
    fOnXMLWriting(Self,
        etWriteSingleTag,//const EventType : TJwXMLEventType;
        IndentLevel,//var IndentLevel: Integer;
        TagName,//var TagName: TJwString;
        Value,//var Value: TJwString;
        Attributes//var Attributes: TJwXMLAttributes
        );

  AttributesLine := '';
  for i := Low(Attributes) to high(Attributes) do
  begin
    //TODO: check for spaces in Name
    //TODO: check for #13#10 in Name and Value
    if (Length(Attributes[i].Name) > 0) and
       (Length(Attributes[i].Value) > 0) then
    AttributesLine := AttributesLine + Attributes[i].Name+'="'+Attributes[i].Value+'" ';
  end;

  if Length(AttributesLine) > 0 then
  begin
    System.Delete(AttributesLine, Length(AttributesLine), 1);
    AttributesLine := ' '+AttributesLine;
  end;

  if fStartMTags then
    result := JwFormatStringEx('<%0:s%1:s>',[TagName, AttributesLine])
  else
  if (Length(Value) > 0) then
  begin
    S := FormatString(Value);
    result := JwFormatStringEx('<%0:s%2:s>%1:s</%0:s>',[TagName, S, AttributesLine])
  end
  else
    result := JwFormatStringEx('<%0:s%1:s/>',[TagName, AttributesLine]);


  if fMultipleTags then
    IndentLevel := fIndLevel;
    //Inc(IndLevel);

  for i := 1 to IndentLevel do
    result := JwStrIdent + result;

  if Assigned(fOnXMLWrite) then
    fOnXMLWrite(Self, Result);


{  if fMultipleTags then
    result := result + JwStrNewLine;  }
end;

function TJwLogServerImpl.GetOnXMLWriting: TJwOnXMLWriting;
begin
  result := fOnXMLWriting;
end;

procedure TJwLogServerImpl.SetOnXMLWriting(Event: TJwOnXMLWriting);
begin
  fOnXMLWriting := Event;
  fWriter.OnXMLWriting := Event;
end;

function TJwLogWriter.GetOnXMLWriting: TJwOnXMLWriting;
begin
  result := fOnXMLWriting;
end;

procedure TJwLogWriter.SetOnXMLWriting(Event: TJwOnXMLWriting);
begin
  fOnXMLWriting := Event;
end;

initialization

{ TComObjectFactory.Create(ComServer, TJwLogWriter, IID_JwWriterCLASS,
    'JwLogWriter', '',ciMultiInstance,
             tmApartment);}


end.
