{
Description
Project JEDI Windows Security Code Library (JWSCL)

This unit provides access to winstation api functions

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

The Original Code is JwsclWinStations.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.
}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclWinStations;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $


interface

uses
  SysUtils,
  Contnrs {for TStack},
  JwsclUtils, JwsclResource,
  jwaWindows,
  JwsclTypes, JwsclExceptions, JwsclSid, JwsclAcl, JwsclToken,
  JwsclMapping, JwsclKnownSid, JwsclSecureObjects,
  JwsclVersion, JwsclConstants, JwsclDescriptor,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type

  TJwSecurityWindowStation  = class;
  TJwSecurityWindowStations = class;

  TJwSecurityWindowStationArray = array of TJwSecurityWindowStation;

  {<B>TJwSecurityWindowStation</B> provides access to window station api.}
  TJwSecurityWindowStation = class
  private
  protected
    fOldWinStations: TStack;
    fHandle: HWINSTA;
    fUserSID: TJwSecurityId;
    fSD: TJwSecurityDescriptor;

    fDestroyWinSta: boolean;

    function GetDesktopNames: TJwTJwStringArray; virtual;
    function GetName: TJwString; virtual;
    function GetFlags: TUserObjectFlags; virtual;
    procedure SetFlags(const Flags: TUserObjectFlags); virtual;
    function GetObjectType: TJwString; virtual;
    function GetUserSID: TJwSecurityId; virtual;

    constructor Create; overload;

    function GetSD(Info: TJwSecurityInformationFlagSet): TJwSecurityDescriptor;
      virtual;
    procedure PutSD(Info: TJwSecurityInformationFlagSet;
      aSD: TJwSecurityDescriptor); virtual;
  public
     {<B>Open</B> opens a window station using a name and desired access rights.
      The window station must be of the current session.
     @param sName defines the name of the window station.
     @param bInherit TBD
     @param cDesiredAccess defines winstation access righs

     raises
 EJwsclOpenWindowStationException:  if the window station could not be opened.
     }
    constructor Open(const sName: TJwString; bInherit: boolean;
      cDesiredAccess: TJwAccessMask);

     {<B>Create</B> creates a new window station. It exists until all handles to it are closed.
      @param sName
      @param bInherit
      @param cDesiredAccess defines winstation access righs
      @param bCreateOnly(Defines whether the winstation is opened if already exits (FALSE),
                or simply fails if already exists (true))

      @param SecurityDescriptor(Defines which security is set on the new window station.
                It can be nil. In that case everybody has full access)
      raises
 EJwsclWinCallFailedException:  if a call to CreateWindowStation failed.
     }
    constructor Create(const sName: TJwString; bInherit: boolean;
      cDesiredAccess: TJwAccessMask; bCreateOnly: boolean;
      SecurityDescriptor: TJwSecurityDescriptor); overload;

    {<B>CreateByHandle</B> creates a new instance and assigns an existing handle to it.
     By default the handle is destroyed on freeing.
     Set property DestroyWinSta manually to false.}
    constructor CreateByHandle(const Handle  :THandle);

    destructor Destroy; override;

     {<B>SetWindowStation</B> changes the current window station for the process to the given on in the current instance.
      raises
 EJwsclWinCallFailedException:  if a call to SetProcessWindowStation failed.
     }
    procedure SetWindowStation;

     {<B>RevertWindowStation</B> reverts to the saved window station from SetWindowStation.
     raises
 EJwsclWinCallFailedException:  if a call to SetProcessWindowStation failed. 
     }
    procedure RevertWindowStation;

     {<B>Handle</B> contains the handle of the window station. <B>Handle</B> is 0 if
      the window station was not opened or created.
      }
    property Handle: HWINSTA Read fHandle;

     {<B>DestroyWinSta</B> defines whether a call to CloseHandle is done on instance destroying (true)
      or not.
     }
    property DestroyWinSta: boolean
      Read fDestroyWinSta Write fDestroyWinSta;

     {<B>DesktopNames</B> returns an arrays of desktop names (TJwString)
      This method raises EJwsclWinCallFailedException if a call to EnumDesktops failed.
     }
    property DesktopNames: TJwTJwStringArray Read GetDesktopNames;

     {<B>Name</B> returns the name of the window station.
      This method raises EJwsclWinCallFailedException if a call to GetUserObjectInformation failed.
     }
    property Name: TJwString Read GetName;

     {<B>Flags</B> sets or gets window station flags.
      This method raises EJwsclWinCallFailedException if a call to SetUserObjectInformation or GetUserObjectInformation failed.}
    property Flags: TUserObjectFlags Read GetFlags Write SetFlags;

     {<B>ObjectType</B> returns a string of this object ("window station")
     This method raises EJwsclWinCallFailedException if a call to GetUserObjectInformation failed.}
    property ObjectType: TJwString Read GetObjectType;

     {<B>UserSID</B> returns the user of this window station.
      This method raises EJwsclWinCallFailedException if a call to GetUserObjectInformation failed.}
    property UserSID: TJwSecurityId Read GetUserSID;

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
  end;


  {<B>TJwSecurityWindowStations</B> provides access to window stations}
  TJwSecurityWindowStations = class
  private
  protected
    fWinstationList: TJwSecurityWindowStationArray;
    fSessionID:      TJwSessionID;

  public
    { public declarations }
     {<B>Create</B> creates an instance of @ClassName which can be used
      to get a list of window station instances.
     }
    constructor Create; overload;

    {Not implemented. Do not use!}
    constructor Create(const LogonId: TLuid); overload;

    destructor Destroy; override;


     {<B>GetWindowStationNames</B> returns a list of window station of the current session.
      raises
 EJwsclWinCallFailedException:  if a call to EnumWindowStationsW failed.}
    class function GetWindowStationNames: TJwTJwStringArray; virtual;
    //property WindowStationNames : TJwTJwStringArray read GetWindowStationNames;

    {<B>SessionID</B> returns the SessionID}
    property SessionID: TJwSessionID Read fSessionID;

     {<B>WindowStations</B> returns a list of window station instances.
      Do not free these instances - they are automatically freed on destruction.
      The window station list is made on creating the @ClassName instance.
     }
    property WindowStations: TJwSecurityWindowStationArray
      Read fWinstationList;
  end;

{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

{$ENDIF SL_OMIT_SECTIONS}


{$IFNDEF SL_INTERFACE_SECTION}


{ TJwSecurityWindowStation }


{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
{ TJwSecurityWindowStations }

(*
type


  PLogonID = ^TLogonID;
  TLogonID = record
    LogonID : Cardinal; //assumed
    State : Cardinal;
  end;
  TWinStationEnumerateW = function(
                    hServer : THandle;
                    var pEntries : Cardinal;
                    var pLogonId : TLogonID;
                    var pByteCount : Cardinal;
                    var pIndex : Cardinal
                    ) : Cardinal; stdcall;

  TWinStationQueryInformationW = function (hServer : THandle;
              LogonID: ULONG;
              WinStaInfoClass : _WINSTATIONINFOCLASS;
              var WinStaInformation : _WINSTATIONINFORMATIONW;
              WinStationInfoLength : ULONG;
              out ReturnLength : ULONG) : Boolean; stdcall;



var  _WinStationEnumerateW : TWinStationEnumerateW;
     _WinStationQueryInformationW : TWinStationQueryInformationW;
       *)
constructor TJwSecurityWindowStations.Create(const LogonId: TLuid);
(*var ByteCount,
    Index,
    pEntries : Cardinal;
    Logon : TLogonID;

    res : NTSTATUS;

    Module : TModuleLoader;
//    b : Boolean;
    ReturnLength : ULONG;
    WinStaInformation : _WINSTATIONINFORMATIONW;
    WinStationInfoLength : ULONG;*)
begin
{$IFNDEF DEBUG}
  raise EJwsclNotImplementedException.Create('TJwSecurityWindowStations.Create is not implemented.');
{$ENDIF DEBUG}
 (*
  _WinStationEnumerateW := nil;
  Module := TModuleLoader.Create('winsta.dll');
  Module.GetProcedure('WinStationEnumerateW',@_WinStationEnumerateW);
  Module.GetProcedure('WinStationQueryInformationW',@_WinStationQueryInformationW);

  //WARNING!  Does not work!!!!
  try
    pEntries := 0;
    Index := 0;

    SetLastError(0);

    ByteCount := sizeof(Logon);
 //   Logon := 0;

    repeat
      res := _WinStationEnumerateW(
                      0,//hServer : THandle;
                      pEntries,//var pEntries : Cardinal;
                      Logon,//var pLogonId : TLogonID;
                      ByteCount,//var pByteCount : Cardinal;
                      Index//var pIndex : Cardinal
                      );
      FillChar(WinStaInformation, sizeof(WinStaInformation), 0);
      WinStationInfoLength := sizeof(WinStaInformation);
      {b := }_WinStationQueryInformationW(
              0,//hServer : THandle;
              Logon.LogonID,//LogonID: ULONG;
              WinStationInformation ,//WinStaInfoClass : WINSTATIONINFOCLASS;
              WinStaInformation,//WinStaInformation : PWINSTATIONINFORMATION;
              WinStationInfoLength,//WinStationInfoLength : ULONG;
              ReturnLength//out ReturnLength : ULONG)
              );
     Inc(Index);
    until res = 0;

    //BUGBUG: res is always 0
    if res <> 0 then
    begin
      SetLastError(res);  //BUGBUG? correct return value?

     // ShowMessage(IntToStr(res));
       raise EJwsclWinCallFailedException.CreateFmtWinCall(
        RsWinCallFailed,
        'Create',       //sSourceProc
        ClassName,                                //sSourceClass
        RsUNWinStation,                          //sSourceFile
        0,                                           //iSourceLine
        True,                                 //bShowLastError
        '_WinStationQueryInformationW',                  //sWinCall
        ['_WinStationQueryInformationW']);
    end;
   finally
     Module.Free;
   end;

//  until res <> 0;       *)
end;

constructor TJwSecurityWindowStations.Create;
begin
  inherited Create;
  fSessionID := 0;

end;



destructor TJwSecurityWindowStations.Destroy;
begin

  inherited;
end;

function EnumWindowStationProc(lpszWindowStation: PWideChar;
  lParam: LPARAM): boolean; stdcall;
begin
  if lParam <> 0 then
    try
      SetLength(TJwTJwStringArray(Pointer(lParam)^), Length(
        TJwTJwStringArray(Pointer(lParam)^)) + 1);
      TJwTJwStringArray(Pointer(lParam)^)[high(TJwTJwStringArray(Pointer(lParam)^))] :=
        TJwString(lpszWindowStation);
    finally
    end;
  Result := True;
end;


class function TJwSecurityWindowStations.GetWindowStationNames: TJwTJwStringArray;
var
  list: TJwTJwStringArray;
begin
  if not jwaWindows.EnumWindowStationsW(@EnumWindowStationProc,
    LPARAM(@list)) then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'GetWindowStationNames',
      //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'EnumWindowStationsW',                  //sWinCall
      ['EnumWindowStations']);
  //const Args: array of const

  Result := list;
end;

{ TJwSecurityWindowStation }

constructor TJwSecurityWindowStation.Create;
begin
  inherited;
  fUserSid := nil;
  fHandle  := 0;

  DestroyWinSta := True;
  fOldWinStations := TStack.Create;
end;


constructor TJwSecurityWindowStation.Create(const sName: TJwString;
  bInherit: boolean; cDesiredAccess: TJwAccessMask; bCreateOnly: boolean;
  SecurityDescriptor: TJwSecurityDescriptor);
var
  pSD: PSecurityAttributes;
  flags: Cardinal;
begin
  Self.Create;


  flags := 0;
  if bCreateOnly then
    flags := CWF_CREATE_ONLY;

  pSD := nil;
  if Assigned(SecurityDescriptor) then
    pSD := SecurityDescriptor.Create_SA(True);

  fHandle := {$IFDEF UNICODE}CreateWindowStationW{$ELSE}CreateWindowStationA{$ENDIF}(TJwPChar(sName), //LPCTSTR lpwinsta,
    flags,//DWORD dwFlags,
    cDesiredAccess,//ACCESS_MASK dwDesiredAccess,
    LPSECURITY_ATTRIBUTES(
    pSD)//LPSECURITY_ATTRIBUTES lpsa
    );

  if Assigned(SecurityDescriptor) then
    SecurityDescriptor.Free_SA(pSD);

  if fHandle = 0 then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinStationCreateFailed,     //const sMsg: TJwString;
      'Create',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'Create',                  //sWinCall
      [sName]);
  //const Args: array of const

  DestroyWinSta := True;
end;


constructor TJwSecurityWindowStation.CreateByHandle(const Handle: THandle);
begin
  Self.Create;
  fHandle := HAndle;
end;

destructor TJwSecurityWindowStation.Destroy;
begin
  if fDestroyWinSta then
    CloseWindowStation(Handle);
  fHandle := 0;
  FreeAndNil(fOldWinStations);
  FreeAndNil(fUserSid);
  FreeAndNil(fSD);

  inherited;
end;


function EnumDesktopProc(lpszDesktop: TJwPChar; lParam: Pointer): boolean;
  stdcall;
begin
  if lParam <> nil then
    try
      SetLength(TJwTJwStringArray(Pointer(lParam)^), Length(
        TJwTJwStringArray(Pointer(lParam)^)) + 1);
      TJwTJwStringArray(Pointer(lParam)^)[high(TJwTJwStringArray(Pointer(lParam)^))] :=
        TJwString(lpszDesktop);
    finally
    end;
  Result := true;
end;

function TJwSecurityWindowStation.GetDesktopNames: TJwTJwStringArray;
var
  list: TJwTJwStringArray;
begin
  if not
{$IFDEF UNICODE}
  EnumDesktopsW(
{$ELSE}
  EnumDesktopsA(
{$ENDIF UNICODE}
    fHandle, @EnumDesktopProc, LPARAM(@list)) then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'GetDesktopNames',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'EnumDesktops',                   //sWinCall
      ['EnumDesktops']);
  Result := list;
end;

function TJwSecurityWindowStation.GetFlags: TUserObjectFlags;
var
  len: Cardinal;
begin
  if not GetUserObjectInformation(fHandle,//HANDLE hObj,
    UOI_FLAGS,//int nIndex,
    Pointer(@Result),//PVOID pvInfo,
    sizeof(Result),//DWORD nLength,
    len//LPDWORD lpnLengthNeeded
    ) then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'GetFlags',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'GetUserObjectInformation',                  //sWinCall
      ['GetUserObjectInformation']);                                  //const Args: array of const
end;

function TJwSecurityWindowStation.GetName: TJwString;
var
  len: Cardinal;
begin
{$IFDEF UNICODE}
  GetUserObjectInformationW(
{$ELSE}
  GetUserObjectInformationA(
{$ENDIF UNICODE}
    fHandle,//HANDLE hObj,
    UOI_NAME,//int nIndex,
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
    UOI_NAME,//int nIndex,
    @Result[1],//PVOID pvInfo,
    len,//DWORD nLength,
    len //LPDWORD lpnLengthNeeded
    ) then


    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'GetName',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'GetUserObjectInformation',                  //sWinCall
      ['GetUserObjectInformation']);                                  //const Args: array of const

  SetLength(Result, len div TJwCharSize - 1);
  if Result = '' then;
end;

function TJwSecurityWindowStation.GetObjectType: TJwString;
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
      'GetObjectType',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'GetUserObjectInformation',                  //sWinCall
      ['GetUserObjectInformation']);                                  //const Args: array of const

  SetLength(Result, len div TJwCharSize - 1);  
  if Result = '' then;
end;

function TJwSecurityWindowStation.GetSD(Info: TJwSecurityInformationFlagSet)
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



function TJwSecurityWindowStation.GetUserSID: TJwSecurityId;
var
  len: Cardinal;
  apSID: PSID;
begin
  Result := nil;
  len := 0;
  GetUserObjectInformation(fHandle,//HANDLE hObj,
    UOI_USER_SID,//int nIndex,
    nil,//PVOID pvInfo,
    0, //DWORD nLength,
    len//LPDWORD lpnLengthNeeded
    );

  if len = 0 then
    exit;

  GetMem(apSid, len);


  if not GetUserObjectInformation(fHandle,//HANDLE hObj,
    UOI_USER_SID,//int nIndex,
    apSID,//PVOID pvInfo,
    len,//DWORD nLength,
    len //LPDWORD lpnLengthNeeded
    ) then
  begin
    FreeMem(apSid);
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'GetUserSID',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'GetUserObjectInformation',                  //sWinCall
      ['GetUserObjectInformation']);                                  //const Args: array of const
  end;

  FreeAndNil(fUserSID);


  fUserSID := TJwSecurityId.Create(apSid);
  Result := fUserSID;

  FreeMem(apSid);
end;

constructor TJwSecurityWindowStation.Open(const sName: TJwString;
  bInherit: boolean; cDesiredAccess: TJwAccessMask);
begin
  Self.Create;


  fHandle :=
{$IFDEF UNICODE}
   OpenWindowStationW
{$ELSE}
    OpenWindowStationA
{$ENDIF}
    (TJwPChar(sName), bInherit, cDesiredAccess);
  if fHandle = 0 then
    raise EJwsclOpenWindowStationException.CreateFmtWinCall(
      RsWinStationOpenFailed,      //const sMsg: TJwString;
      'Open',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'OpenWindowStation',                  //sWinCall
      [sName]);
  //const Args: array of const
end;

procedure TJwSecurityWindowStation.PutSD(Info: TJwSecurityInformationFlagSet;
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


procedure TJwSecurityWindowStation.RevertWindowStation;
var
  h: HWINSTA;
begin
  if fOldWinStations.Count = 0 then
    exit;
  h := HWINSTA(fOldWinStations.Peek);

  if not SetProcessWindowStation(h) then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'SetWindowStation',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'RevertWindowStation',                  //sWinCall
      ['SetProcessWindowStation']);

  fOldWinStations.Pop;
end;

procedure TJwSecurityWindowStation.SetFlags(const Flags: TUserObjectFlags);
begin
  if not SetUserObjectInformation(fHandle, UOI_FLAGS, @Flags,
    sizeof(Flags)) then
  begin
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'SetFlags',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'SetUserObjectInformation',                  //sWinCall
      ['SetUserObjectInformation']);                                  //const Args: array of const
  end;
end;

procedure TJwSecurityWindowStation.SetWindowStation;
var
  tempWinst: HWINSTA;
begin
  tempWinst := GetProcessWindowStation;

  if not SetProcessWindowStation(fHandle) then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'SetWindowStation',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'SetProcessWindowStation',                  //sWinCall
      ['SetProcessWindowStation']);

  //only save pointer if no error occured
  fOldWinStations.Push(Pointer(tempWinst));
end;

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
