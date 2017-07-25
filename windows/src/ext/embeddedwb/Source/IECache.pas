//**************************************************************
//                                                             *
//                          TIECache                           *                                            //                                                             *
//                     For Delphi 5 to XE                      *
//                     Freeware Component                      *
//                            by                               *
//                     Per Lindsø Larsen                       *
//                   per.lindsoe@larsen.dk                     *
//                                                             *
//  Contributions:                                             *
//  Christian Lovis for lib dynamic linking                    *
//            {christian.lovis@dim.hcuge.ch]                   *
//  Eran Bodankin (bsalsa) bsalsa@gmail.com                    *
//         -  D2005 update                                     *
//                                                             *
//  Updated versions:                                          *
//               http://www.bsalsa.com                         *
//**************************************************************

{*******************************************************************************}
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
4. Please consider donation in our web site!
{*******************************************************************************}

unit IECache;

interface

{$I EWB.inc}

uses
  WinInet, Windows, Messages, SysUtils, Classes, EwbIeConst;

type
  PInternetCacheTimeStamps = ^TInternetCacheTimeStamps;
  TInternetCacheTimeStamps = record
    ftExpires: TFileTime;
    ftLastModified: TFileTime;
  end;
  PInternetCacheGroupInfo = ^TInternetCacheGroupInfo;
  TInternetCacheGroupInfo = record
    dwGroupSize: DWORD;
    dwGroupFlags: DWORD;
    dwGroupType: DWORD;
    dwDiskUsage: DWORD;
    dwDiskQuota: DWORD;
    dwOwnerStorage: array[0..GROUP_OWNER_STORAGE_SIZE - 1] of DWORD;
    szGroupName: array[0..GROUPNAME_MAX_LENGTH - 1] of AnsiChar;
  end;
  TEntryInfo = record
    SourceUrlName: string;
    LocalFileName: string;
    EntryType: DWORD;
    UseCount: DWORD;
    HitRate: DWORD;
    FSize: DWORD;
    LastModifiedTime: TDateTime;
    ExpireTime: TDateTime;
    LastAccessTime: TDateTime;
    LastSyncTime: TDateTime;
    HeaderInfo: string;
    FileExtension: string;
    ExemptDelta: DWORD;
  end;
  TGroupInfo = record
    DiskUsage: DWORD;
    DiskQuota: DWORD;
    OwnerStorage: array[0..GROUP_OWNER_STORAGE_SIZE - 1] of DWORD;
    GroupName: string;
  end;
  TContent = record
    Buffer: Pointer;
    BufferLength: Integer;
  end;
  TFilterOption = (NORMAL_ENTRY,
    STABLE_ENTRY,
    STICKY_ENTRY,
    COOKIE_ENTRY,
    URLHISTORY_ENTRY,
    TRACK_OFFLINE_ENTRY,
    TRACK_ONLINE_ENTRY,
    SPARSE_ENTRY,
    OCX_ENTRY);
  TFilterOptions = set of TFilterOption;
  TOnEntryEvent = procedure(Sender: TObject; var Cancel: Boolean) of object;
  TOnGroupEvent = procedure(Sender: TObject; GroupID: GROUPID; var Cancel: Boolean) of object;
  TSearchPattern = (spAll, spCookies, spHistory, spUrl);

  TIECache = class(TComponent)
  private
    FSearchPattern: TSearchPattern;
    FOnEntry: TOnEntryEvent;
    FOnGroup: TOnGroupEvent;
    GrpHandle: THandle;
    H: THandle;
    FCancel: Boolean;
    FFilterOptions: TFilterOptions;
    FFilterOptionValue: Cardinal;
    procedure SetFilterOptions(const Value: TFilterOptions);
    procedure UpdateFilterOptionValue;
    procedure GetEntryValues(Info: PInternetCacheEntryInfo);
    procedure ClearEntryValues;
  protected { Protected declarations }
  public
    GroupInfo: TGroupInfo;
    EntryInfo: TEntryInfo;
    Content: TContent;
    constructor Create(AOwner: TComponent); override;
    function AddUrlToGroup(GroupID: INT64; URL: string): DWORD;
    function CloseFindEntry: BOOL;
    function CopyFileToCache(URL, FileName: string; CacheType: DWORD; Expire: TDateTime): DWORD;
    //  function CopyFileToCache(UrlName, FileName: Pchar): string;
    function CreateEntry(URL, FileExtension: string; ExpectedFileSize: DWORD; var FName: string): DWORD;
    function CreateGroup: INT64;
    function DeleteEntry(URL: string): DWORD;
    function DeleteGroup(GroupID: INT64): DWORD;
    function FindFirstEntry(GroupID: INT64): DWORD;
    function FindFirstGroup(var GroupID: Int64): DWORD;
    function FindNextEntry: DWORD;
    function FindNextGroup(var GroupID: Int64): BOOL;
    function GetEntryContent(URL: string): DWORD;
    function GetEntryInfo(URL: string): DWORD;
    function GetGroupInfo(GroupID: INT64): DWORD;
    function getLibraryFound: Boolean;
    function RemoveUrlFromGroup(GroupID: INT64; URL: string): DWORD;
    function RetrieveGroups: DWORD;
    function SetEntryInfo(URL: string): DWORD;
    function SetGroupInfo(GroupID: INT64): DWORD;
    procedure ClearAllEntries;
    procedure RetrieveEntries(GroupID: INT64);
    { Public declarations }
  published
    property FilterOptions: TFilterOptions read FFilterOptions write SetFilterOptions;
    property LibraryFound: Boolean read getLibraryFound;
    property OnEntry: TOnEntryEvent read FOnEntry write FOnEntry;
    property OnGroup: TOnGroupEvent read FOnGroup write FOnGroup;
    property SearchPattern: TSearchpattern read FSearchpattern write FSearchPattern;
    { Published declarations }
  end;

implementation

type

  TFindFirstUrlCacheGroup =
    function(dwFlags, dwFilter: DWORD;
    lpSearchCondition: Pointer; dwSearchCondition: DWORD;
    var Group: Int64; lpReserved: Pointer): THandle; stdcall;

  TFindNextUrlCacheGroup =
    function(hFind: THandle; var GroupID: Int64; lpReserved: Pointer): BOOL; stdcall;

  TSetUrlCacheGroupAttribute =
    function(gid: Int64; dwFlags, dwAttributes: DWORD; var lpGroupInfo: TInternetCacheGroupInfo;
    lpReserved: Pointer): BOOL; stdcall;

  TGetUrlCacheGroupAttribute =
    function(gid: Int64; dwFlags, dwAttributes: DWORD;
    var GroupInfo: TInternetCacheGroupInfo; var dwGroupInfo: DWORD; lpReserved: Pointer): BOOL; stdcall;

var
  FindFirstUrlCacheGroup: tFindFirstUrlCacheGroup;
  FindNextUrlCacheGroup: tFindNextUrlCacheGroup;
  GetUrlCacheGroupAttribute: tGetUrlCacheGroupAttribute;
  SetUrlCacheGroupAttribute: tSetUrlCacheGroupAttribute;
  winInetLibFound: Boolean;

const
  winetdll = 'wininet.dll';

//====Accessories===============================================================

function InitializeWinInet: Boolean;
var
  fPointer: tFarProc;
  hInst: tHandle;
begin
  if winInetLibFound then
    Result := true
  else
  begin
    Result := False;
    hInst := loadLibrary(winetdll);
    if hInst > 0 then
    try
      fPointer := getProcAddress(hInst, 'FindFirstUrlCacheGroup');
      if fPointer <> nil then
      begin
        FindFirstUrlCacheGroup := tFindFirstUrlCacheGroup(fPointer);
        fPointer := getProcAddress(hInst, 'FindNextUrlCacheGroup');
        if fPointer <> nil then
        begin
          FindNextUrlCacheGroup := tFindNextUrlCacheGroup(fPointer);
          fPointer := getProcAddress(hInst, 'GetUrlCacheGroupAttributeA');
          if fPointer <> nil then
          begin
            GetUrlCacheGroupAttribute := tGetUrlCacheGroupAttribute(fPointer);
            fPointer := getProcAddress(hInst, 'SetUrlCacheGroupAttributeA');
            if fPointer <> nil then
            begin
              SetUrlCacheGroupAttribute := tSetUrlCacheGroupAttribute(fPointer);
              fPointer := getProcAddress(hInst, 'FindFirstUrlCacheEntryExA');
              if fPointer <> nil then
                Result := true;
            end; // SetUrlCacheGroupAttribute
          end; // GetUrlCacheGroupAttribute
        end; // FindNextUrlCacheGroup
      end; // FindFirstUrlCacheGroup
    except
    end; // loadLib
    winInetLibFound := Result;
  end;
end;

function FileTimeToDateTime(Ft: TFileTime): TDateTime;
var
  St: TSystemTime;
  lft: TFileTime;
begin
  Result := 0;
  try
    if FileTimeToLocalFiletime(Ft, lft) then
      if FileTimeToSyStemTime(lft, st) then
        Result := SystemTimeTODateTime(st);
  except
  end;
end;

function DateTimeToFileTime(Dt: TDateTime): TFileTime;
var
  St: TSystemTime;
  lft: TFileTime;
begin
  try
    DateTimeToSystemTime(Dt, ST);
    if SystemTimeToFileTime(st, lft) then
      LocalFileTimeToFileTime(lft, Result);
  except
    Result.dwLowDateTime := 0;
    Result.dwHighDateTime := 0;
  end;
end;

//====IE Cache==================================================================

constructor TIECache.Create(AOwner: TComponent);
begin
  inherited;
  Content.Buffer := nil;
  ClearEntryValues;
    // Identical to URLCACHE_FIND_DEFAULT_FILTER
  FFilterOptions := [NORMAL_ENTRY, COOKIE_ENTRY, URLHISTORY_ENTRY,
    TRACK_OFFLINE_ENTRY, TRACK_ONLINE_ENTRY, STICKY_ENTRY];
end;

function TIECache.getLibraryFound: Boolean;
begin
  Result := InitializeWinInet;
end;

function TIECache.RemoveUrlFromGroup(GroupID: INT64; URL: string): DWORD;
begin
  Result := S_OK;
  if not InitializeWinInet then
  begin
    Result := ERROR_FILE_NOT_FOUND;
    Exit;
  end;
  if not SetUrlCacheEntryGroup(Pchar(URL), INTERNET_CACHE_GROUP_REMOVE, GroupID, nil, 0, nil)
    then
    Result := GetLastError;
end;

function TIECache.AddUrlToGroup(GroupID: INT64; URL: string): DWORD;
begin
  Result := S_OK;
  if not InitializeWinInet then
  begin
    Result := ERROR_FILE_NOT_FOUND;
    Exit;
  end;
  if not SetUrlCacheEntryGroup(Pchar(URL), INTERNET_CACHE_GROUP_ADD, GroupID, nil, 0, nil)
    then
    Result := GetLastError;
end;

function TIECache.CopyFileToCache(URL, FileName: string; CacheType: DWORD; Expire: TDateTime): DWORD;
var
  FName: string;
  Ext: string;
  F: file of Byte;
  Size: DWORD;
begin
  if not InitializeWinInet then
  begin
    Result := ERROR_FILE_NOT_FOUND;
    Exit;
  end;
  if not FileExists(FileName) then
  begin
    Result := ERROR_FILE_NOT_FOUND;
    Exit;
  end;
  AssignFile(F, FileName);
  Reset(F);
  Size := FileSize(F);
  CloseFile(F);
  Ext := ExtractFileExt(FileName);
  Ext := Copy(Ext, 2, Length(ext));
  Result := CreateEntry(URL, Ext, Size, FName);
  if Result <> S_OK then
    Exit;
  if not windows.copyfile(PChar(FileName), Pchar(FName), False) then
  begin
    Result := GetLastError;
    Exit;
  end;
  if not CommitUrlCacheEntry(Pchar(URL), Pchar(Fname), DateTimeToFileTime(Expire), DateTimeToFileTime(now), CacheType, nil, 0, Pchar(Ext), 0)
    then
    Result := GetLastError;
end;

function TIECache.CreateEntry(URL, FileExtension: string; ExpectedFileSize: DWORD; var FName: string): DWORD;
var
  PC: array[0..MAX_PATH] of Char;
begin
  PC := '';
  Result := S_OK;
  if not InitializeWinInet then
  begin
    Result := ERROR_FILE_NOT_FOUND;
    Exit;
  end;
  if not CreateUrlCacheEntry(Pchar(URL), ExpectedFileSize, Pchar(FileExtension), PC, 0) then
    Result := GetLastError
  else
    FName := StrPas(PC);
end;

function TIECache.GetGroupInfo(GroupID: INT64): DWORD;
var
  info: TInternetCacheGroupInfo;
  dw: DWORD;
begin
  Result := S_OK;
  if not InitializeWinInet then
  begin
    Result := ERROR_FILE_NOT_FOUND;
    Exit;
  end;
  dw := SizeOf(TInternetCacheGroupInfo);
  if not GetUrlCacheGroupAttribute(GroupID, 0, CACHEGROUP_ATTRIBUTE_GET_ALL, info, dw, nil)
    then
    Result := GetLastError
  else
    with GroupInfo do
    begin
      DiskUsage := info.dwDiskUsage;
      DiskQuota := info.dwDiskQuota;
      Move(info.dwOwnerStorage, OwnerStorage, Sizeof(OwnerStorage));
      GroupName := string(info.szGroupName);
    end;
end;

function TIECache.SetGroupInfo(GroupID: INT64): DWORD;
var
  info: TInternetCacheGroupInfo;
begin
  Result := S_OK;
  if not InitializeWinInet then
  begin
    Result := ERROR_FILE_NOT_FOUND;
    Exit;
  end;
  info.dwGroupSize := SizeOf(Info);
  info.dwGroupFlags := CACHEGROUP_FLAG_NONPURGEABLE;
  info.dwGroupType := CACHEGROUP_TYPE_INVALID;
  info.dwDiskQuota := GroupInfo.DiskQuota;
  move(GroupInfo.OwnerStorage, info.dwOwnerStorage, Sizeof(info.dwOwnerStorage));
  move(GroupInfo.Groupname[1], info.szGroupName[0], length(GroupInfo.Groupname));
  if not SetUrlCacheGroupAttribute(GroupID, 0, CACHEGROUP_READWRITE_MASK, info, nil) then
    Result := GetLastError;
end;

function TIECache.CreateGroup: INT64;
begin
  if not InitializeWinInet then
  begin
    Result := ERROR_FILE_NOT_FOUND;
    Exit;
  end;
  Result := CreateUrlCacheGroup(0, nil);
end;

function TIECache.DeleteGroup(GroupID: INT64): DWORD;
begin
  Result := S_OK;
  if not InitializeWinInet then
  begin
    Result := ERROR_FILE_NOT_FOUND;
    Exit;
  end;
  if not DeleteUrlCacheGroup(GroupID, CACHEGROUP_FLAG_FLUSHURL_ONDELETE, nil) then
    Result := GetLastError;
end;

function TIECache.SetEntryInfo(URL: string): DWORD;
var
  Info: TInternetCacheEntryInfo;
  FC: DWORD;
begin
  Result := S_OK;
  if not InitializeWinInet then
  begin
    Result := ERROR_FILE_NOT_FOUND;
    Exit;
  end;
  FC := CACHE_ENTRY_ATTRIBUTE_FC +
    CACHE_ENTRY_HITRATE_FC +
    CACHE_ENTRY_MODTIME_FC +
    CACHE_ENTRY_EXPTIME_FC +
    CACHE_ENTRY_ACCTIME_FC +
    CACHE_ENTRY_SYNCTIME_FC +
    CACHE_ENTRY_EXEMPT_DELTA_FC;
  with Info do
  begin
    CacheEntryType := EntryInfo.EntryType;
    dwHitRate := EntryInfo.HitRate;
    LastModifiedTime := DateTimeToFileTime(EntryInfo.LastModifiedTime);
    ExpireTime := DateTimeToFileTime(EntryInfo.ExpireTime);
    LastAccessTime := DateTimeToFileTime(EntryInfo.LastAccessTime);
    LastSyncTime := DateTimeToFileTime(EntryInfo.LastSyncTime);
    dwReserved := EntryInfo.ExemptDelta;
  end;
  if not SetUrlCacheEntryInfo(Pchar(URL), Info, FC) then
    Result := GetLastError;
end;

function TIECache.GetEntryInfo(URL: string): DWORD;
var
  D: DWORD;
  T: PInternetCacheEntryInfo;
begin
  Result := S_OK;
  if not InitializeWinInet then
  begin
    Result := ERROR_FILE_NOT_FOUND;
    Exit;
  end;
  if (D <= 0) or (not GetUrlCacheEntryInfoEx(Pchar(URL), nil, @D, nil, nil, nil, 0)) then
  begin
    // (PChar(SysErrorMessage(GetLastError)));
    // https objects are not stored in cache
    Result := GetLastError();
  end
  else
  begin
    GetMem(T, D);
    try
      if GetUrlCacheEntryInfoEx(Pchar(URL), T, @D, nil, nil, nil, 0)
        then
        GetEntryValues(t)
      else
        Result := GetLastError;
    finally
      FreeMem(T, D);
    end;
  end;
end;

function TIECache.GetEntryContent(URL: string): DWORD;
var
  HR: THandle;
  D: Cardinal;
  T: PInternetCacheEntryInfo;
begin
  Result := S_OK;
  if not InitializeWinInet then
  begin
    Result := ERROR_FILE_NOT_FOUND;
    Exit;
  end;
  D := 0;
  T := nil;
  RetrieveUrlCacheEntryStream(PChar(URL), T^, D, TRUE, 0);
  Getmem(T, D);
  try
    HR := THandle(RetrieveUrlCacheEntryStream(PChar(URL), T^, D, TRUE, 0));
    if HR <> 0 then
    begin
      Content.BufferLength := T^.dwSizeLow + 1;
      GetEntryValues(T);
      Getmem(Content.Buffer, Content.BufferLength);
      Fillchar(Content.Buffer^, Content.BufferLength, #0);
      if not ReadUrlCacheEntryStream(Hr, 0, Content.Buffer, T^.DwSizeLow, 0)
        then
        Result := GetLastError;
    end;
  finally
    FreeMem(T, D);
  end;
  UnLockUrlCacheEntryStream(HR, 0);
end;

function TIECache.FindNextGroup(var GroupID: Int64): BOOL;
begin
  if not InitializeWinInet then
  begin
    Result := False;
    Exit;
  end;
  Result := FindNextUrlCacheGroup(GrpHandle, GroupID, nil);
  GetGroupInfo(GroupID);
end;

function TIECache.FindFirstGroup(var GroupID: Int64): DWORD;
begin
  if not InitializeWinInet then
  begin
    Result := ERROR_FILE_NOT_FOUND;
    Exit;
  end;
  GrpHandle := FindFirstUrlCacheGroup(0, 0, nil, 0, GroupID, nil);
  if GrpHandle <> 0 then
    Result := S_OK
  else
    Result := GetLastError;
  if Result = S_OK then
    GetGroupInfo(GroupID);
end;

function TIECache.RetrieveGroups: DWORD;
var
  GroupID: INT64;
  Res: DWORD;
  NewGroup, Cancel: Boolean;
begin
  Result := S_OK;
  if not InitializeWinInet then
  begin
    Result := ERROR_FILE_NOT_FOUND;
    Exit;
  end;
  Cancel := False;
  NewGroup := True;
  Res := FindFirstGroup(GroupID);
  if Res = S_OK then
  begin
    GetGroupInfo(GroupID);
    if Assigned(FOngroup) then
      FOnGroup(self, GroupID, FCancel);
    while not Cancel and NewGroup do
    begin
      NewGroup := FindNextGroup(GroupID);
      GetGroupInfo(GroupID);
      if Assigned(FOngroup) and NewGroup then
        FOnGroup(self, GroupID, Cancel);
    end;
  end
  else
    Result := GetLastError;
end;

function TIECache.DeleteEntry(URL: string): DWORD;
begin
  Result := S_OK;
  if not InitializeWinInet then Exit;
  if not DeleteUrlCacheEntry(PChar(URL)) then
    Result := GetLastError
  else
    ClearEntryValues;
end;

procedure TIECache.ClearAllEntries;
var
  HR: DWord;
begin
  if not InitializeWinInet then Exit;
  if FindFirstEntry(0) = S_OK then
  begin
    repeat
      DeleteEntry(EntryInfo.SourceUrlName);
      HR := FindNextEntry;
    until HR = ERROR_NO_MORE_ITEMS;
  end;
  FindCloseUrlCache(H);
end;

procedure TIECache.ClearEntryValues;
begin
  if not InitializeWinInet then Exit;
  Content.Buffer := nil;
  Content.BufferLength := 0;
  with EntryInfo do
  begin
    SourceUrlName := '';
    LocalFileName := '';
    EntryType := 0;
    UseCount := 0;
    Hitrate := 0;
    LastModifiedTime := 0;
    ExpireTime := 0;
    LastAccessTime := 0;
    LastSyncTime := 0;
    FileExtension := '';
    FSize := 0;
    HeaderInfo := '';
    ExemptDelta := 0;
  end;
end;

procedure TIECache.GetEntryValues(Info: PInternetCacheEntryInfo);
begin
  if not InitializeWinInet then Exit;
  with entryInfo do
  begin
    SourceUrlName := info^.lpszSourceUrlName;
    LocalFileName := info^.lpszLocalFileName;
    EntryType := info^.CacheEntryType;
    UseCount := info^.dwUseCount;
    Hitrate := info^.dwHitRate;
    LastModifiedTime := FileTimeToDateTime(info^.LastModifiedTime);
    ExpireTime := FileTimeToDateTime(info^.ExpireTime);
    LastAccessTime := FileTimeToDateTime(info^.LastAccessTime);
    LastSyncTime := FileTimeToDateTime(info^.LastSyncTime);
    FileExtension := info^.lpszFileExtension;
    FSize := (info^.dwSizeHigh shl 32) + info^.dwSizeLow;
    HeaderInfo := StrPas(PChar(info^.lpHeaderInfo));
    ExemptDelta := info^.dwReserved;
  end;
end;

function TIECache.FindFirstEntry(GroupID: INT64): DWORD;
const
  Pattern: array[TSearchPattern] of PChar = (nil, 'Cookie:', 'Visited:', '');
var
  T: PInternetCacheEntryInfo;
  D: DWORD;
begin
  Result := S_OK;
  if not InitializeWinInet then
  begin
    Result := ERROR_FILE_NOT_FOUND;
    Exit;
  end;
  H := 0;
  D := 0;
  FindFirstUrlCacheEntryEx(Pattern[SearchPattern], 0, FFilterOptionValue, GroupID, nil, @D, nil, nil, nil);
  GetMem(T, D);
  try
    H := FindFirstUrlCacheEntryEx(Pattern[SearchPattern], 0, FFilterOptionValue, GroupID, T, @D, nil, nil, nil);
    if (H = 0) then
      Result := GetLastError
    else
      GetEntryValues(T);
  finally
    FreeMem(T, D)
  end;
end;

function TIECache.FindNextEntry: DWORD;
var
  T: PInternetCacheEntryInfo;
  D: DWORD;
begin
  Result := S_OK;
  if not InitializeWinInet then
  begin
    Result := ERROR_FILE_NOT_FOUND;
    Exit;
  end;
  D := 0;
  FindNextUrlCacheEntryEx(H, nil, @D, nil, nil, nil);
  GetMem(T, D);
  try
    if not FindNextUrlCacheEntryEx(H, T, @D, nil, nil, nil)
      then
      Result := GetLastError
    else
      GetEntryValues(T);
  finally
    FreeMem(T, D)
  end;
end;

procedure TIECache.RetrieveEntries(GroupID: INT64);
var
  HR: DWORD;
begin
  if not InitializeWinInet then Exit;
  FCancel := False;
  HR := FindFirstEntry(GroupID);
  if (HR = S_OK) then
  begin
    if Assigned(FOnEntry) then
      with EntryInfo do
        FOnEntry(self, FCancel);
    while (HR = S_OK) and not FCancel do
    begin
      HR := FindNextEntry;
      if (HR = S_OK) and Assigned(FOnEntry) then
        with EntryInfo do
          FOnEntry(self, FCancel);
    end;
  end;
  FindCloseUrlCache(H);
end;

function TIECache.CloseFindEntry: BOOL;
begin
  if not InitializeWinInet then
  begin
    Result := False;
    Exit;
  end;
  Result := FindCloseUrlCache(H);
end;

procedure TIECache.SetFilterOptions(const Value: TFilterOptions);
begin
  FFilterOptions := Value;
  UpdateFilterOptionValue;
end;

procedure TIECache.UpdateFilterOptionValue;
const
  AcardFilterOptionValues: array[TFilterOption] of Cardinal = (
    $00000001, $00000002, $00000004, $00100000, $00200000,
    $00000010, $00000020, $00010000, $00020000);
var
  i: TFilterOption;
begin
  FFilterOptionValue := 0;
  if (FFilterOptions <> []) then
    for i := Low(TFilterOption) to High(TFilterOption) do
      if (i in FFilterOptions) then
        Inc(FFilterOptionValue, AcardFilterOptionValues[i]);
end;

initialization
  wininetLibFound := InitializeWinInet;

end.
