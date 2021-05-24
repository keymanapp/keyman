(*
  Name:             enumhandles
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      18 Feb 2011

  Modified Date:    18 Feb 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          18 Feb 2011 - mcdurdin - I2537 - Fix crash when Adobe Reader X is opened in sandbox mode
*)
unit enumhandles;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


const SystemHandleInformation = 16;

type
  _NtQuerySystemInformation = function (SystemInformationClass: ULONG; SystemInformation: Pointer; SystemInformationLength: ULONG; var ReturnLength: ULONG): ULONG; stdcall;
  _NtQueryObject = function (Handle: THandle; ObjectInformationClass: ULONG; ObjectInformation: Pointer; ObjectInformationLength: ULONG; var ReturnLength: ULONG): ULONG; stdcall;

{typedef NTSTATUS (NTAPI *_NtQuerySystemInformation)(
    ULONG SystemInformationClass,
    PVOID SystemInformation,
    ULONG SystemInformationLength,
    PULONG ReturnLength
    );
}
///* The following structure is actually called SYSTEM_HANDLE_TABLE_ENTRY_INFO, but SYSTEM_HANDLE is shorter. */

_SYSTEM_HANDLE = record
    ProcessId: ULONG;
    ObjectTypeNumber: BYTE;
    Flags: BYTE;
    Handle: Word;
    Object_: Pointer;
    GrantedAccess: ACCESS_MASK;
end;

SYSTEM_HANDLE = _SYSTEM_HANDLE;
PSYSTEM_HANDLE = ^_SYSTEM_HANDLE;

_SYSTEM_HANDLE_INFORMATION = record
  HandleCount: ULONG;
  Handles: array[0..1000000] of _SYSTEM_HANDLE;
end;

SYSTEM_HANDLE_INFORMATION = _SYSTEM_HANDLE_INFORMATION;
PSYSTEM_HANDLE_INFORMATION = ^_SYSTEM_HANDLE_INFORMATION;

const
  STATUS_INFO_LENGTH_MISMATCH = $c0000004;
  STATUS_SUCCESS = 0;

  ObjectTypeInformation = 2;

type
  UNICODE_STRING = record
    Length: WORD;
//    Pad: WORD;
    MaximumLength: WORD;
//    Pad2: Word;
    Buffer: PWIDECHAR;
  end;
  
  PUBLIC_OBJECT_TYPE_INFORMATION = record
    TypeName: UNICODE_STRING;
    TotalNumberOfObjects: ULONG;
    TotalNumberOfHandles: ULONG;
    TotalPagedPoolUsage: ULONG;
    TotalNonPagedPoolUsage: ULONG;
    TotalNamePoolUsage: ULONG;
    TotalHandleTableUsage: ULONG;
    HighWaterNumberOfObjects: ULONG;
    HighWaterNumberOfHandles: ULONG;
    HighWaterPagedPoolUsage: ULONG;
    HighWaterNonPagedPoolUsage: ULONG;
    HighWaterNamePoolUsage: ULONG;
    HighWaterHandleTableUsage: ULONG;
    InvalidAttributes: ULONG;
    GenericMapping: GENERIC_MAPPING;
    ValidAccessMask: ULONG;
    SecurityRequired: LONGBOOL; //BOOLEAN;
    MaintainHandleCount: LONGBOOL; //BOOLEAN;
    PoolType: ULONG;
    DefaultPagedPoolCharge: ULONG;
    DefaultNonPagedPoolCharge: ULONG;
    PADDING: ARRAY[0..100] of ULONG;
  end;

  _DuplicateHandle = function(SourceHandle: THandle; var DestinationHandle: THandle): ULONG; stdcall;

procedure TForm1.FormCreate(Sender: TObject);
var
  NtQuerySystemInformation: _NtQuerySystemInformation;
  NtQueryObject: _NtQueryObject;
  psysinfo: PSYSTEM_HANDLE_INFORMATION;
  lsysinfo: ULONG;
  nlsysinfo: ULONG;
  i, nn: Integer;
  res: ULONG;
  oti: PUBLIC_OBJECT_TYPE_INFORMATION;
  JobType: BYTE;
  JobTypeFound: Boolean;
  IsNotJobType: array[0..255] of Boolean;
  h: Cardinal;
  XDuplicateHandle: _DuplicateHandle;
  hProcess: Cardinal;
begin
  NtQuerySystemInformation := GetProcAddress(GetModuleHandle('ntdll.dll'), 'NtQuerySystemInformation');
  NtQueryObject := GetProcAddress(GetModuleHandle('ntdll.dll'), 'NtQueryObject');
  XDuplicateHandle := GetProcAddress(GetModuleHandle('kernel32.dll'), 'DuplicateHandle');
  nn := 1000;
  psysinfo := nil;
  repeat
    lsysinfo := sizeof(ULONG) + nn * sizeof(_SYSTEM_HANDLE);
    if Assigned(psysinfo) then ReallocMem(psysinfo, lsysinfo)
    else psysinfo := AllocMem(lsysinfo);
    res := NtQuerySystemInformation(SystemHandleInformation, psysinfo, lsysinfo, nlsysinfo);  // Need SystemExtendedHandleInformation on XP and higher for 65K+ process ids
    nn := nn * 2;
  until res <> STATUS_INFO_LENGTH_MISMATCH;

  if res <> STATUS_SUCCESS then
    RaiseLastOSError(res);

  JobTypeFound := False;
  JobType := 0;
  for i := 0 to 255 do
    IsNotJobType[i] := False;

  for i := 0 to psysinfo.HandleCount - 1 do
  begin
    if JobTypeFound then
    begin
      if psysinfo.Handles[i].ObjectTypeNumber = JobType then
      begin
        memo1.Lines.Add('found');
      end;
    end
    else if not IsNotJobType[psysinfo.Handles[i].ObjectTypeNumber] then
    begin
      hProcess := OpenProcess(PROCESS_DUP_HANDLE, False, psysinfo.Handles[i].ProcessId);
      if hProcess <> 0 then
      begin
        if DuplicateHandle(hProcess, psysinfo.Handles[i].Handle, GetCurrentProcess(), @h, $1F001F, False, 0) then
        begin
          res := NtQueryObject(h, ObjectTypeInformation, @oti, sizeof(oti), nlsysinfo);
          if res = STATUS_SUCCESS then
          begin
            if oti.TypeName.Buffer = 'Job' then
            begin
              JobType := psysinfo.Handles[i].ObjectTypeNumber;
              JobTypeFound := True;
              memo1.Lines.Add('Found');
            end
            else
              IsNotJobType[psysinfo.Handles[i].ObjectTypeNumber] := True;
          end;
          CloseHandle(h);
        end;
//        end
//        else raiselastoserror;
        CloseHandle(hProcess);
      end;
    end;
  end;
end;

end.
