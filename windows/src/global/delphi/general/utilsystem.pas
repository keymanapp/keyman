(*
  Name:             utilsystem
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    7 Feb 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Refactor util functions into multiple units
                    16 May 2007 - mcdurdin - Fix functions to widestring
                    30 May 2007 - mcdurdin - Fix OS version testing
                    27 Mar 2008 - mcdurdin - Add WaitForProgram
                    20 Jul 2008 - mcdurdin - I1546 - Use $a000 system shadow keyboard layout
                    28 Aug 2008 - mcdurdin - I1617 - Fix range check error
                    27 Jan 2009 - mcdurdin - I1814 - Avoid unnecessary runas
                    18 Mar 2011 - mcdurdin - I2793 - uninst.vbs does not work for Admin-installed keyboards
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    04 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    26 Jun 2012 - mcdurdin - I3379 - KM9 - Remove old Winapi references now in Delphi libraries
                    01 Dec 2012 - mcdurdin - I3613 - V9.0 - System shadow keyboards obsolete, strip out remaining code
                    24 Apr 2014 - mcdurdin - I4195 - V9.0 - Use TTempFileManager for all temporary files
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
                    28 May 2014 - mcdurdin - I4220 - V9.0 - Remove references to LoadKeyboardLayout, Preload, Substitutes, etc. and use only TSF
                    07 Feb 2015 - mcdurdin - I4590 - V9.0 - Keyman fails to install shortcuts for keyboard documentation correctly
*)
unit utilsystem;  // I3306   // I4220

interface

uses
  System.Classes,
  System.Win.Registry,
  Winapi.Windows;

function GetFolderPath(csidl: Integer): string;
function MAKELCID(wLanguageID, wSortID: WORD): DWORD;

function DeleteFileOnReboot(const FileName: string): Boolean; //deprecated 'use TTempFileManager';-->used only by onlineupdatecheck, hard to find alternate for now   // I4195   // I4181

function CreateLink(FProg, FArgs, LinkPath, Desc: string; FWorkingDirectory: string = ''; IconFile: string = ''; IconIndex: Integer = 0; RunAsAdmin: Boolean = False): Boolean;

function ExecuteProgram(const cmdline, path: string; var errmsg: string): Boolean;
function WaitForProgram(WindowHandle: HWND; const cmdline, path, parameters: WideString; var errmsg: WideString; DoSpin: TNotifyEvent; RunAs: Boolean): Cardinal;

function ExpandFileNameEx(RootPath, FileName: WideString): WideString;
function ExpandFileNameClean(const Root, FileName: WideString): WideString;

procedure OpenContainingFolder(const FileName: string);

function LoadIndirectString(const name: WideString): WideString;

type
  TRegistryHelper = class helper for TRegistry
    procedure ReadMultiString(const ValueName: string; Strings: TStrings);
  end;

implementation

uses
  System.SysUtils,
  Winapi.ActiveX,
  Winapi.ShellApi,
  Winapi.ShlObj,

  RegistryKeys,
  utilexecute,
  GetOsVersion;

function GetFolderPath(csidl: Integer): string;
var
  buf: array[0..260] of Char;
  idl: PItemIDList;
  mm: IMalloc;
begin
  Result := '';
  if SHGetMalloc(mm) = NOERROR then
  begin
    if SHGetSpecialFolderLocation(0, csidl, idl) = NOERROR then
    begin
      if SHGetPathFromIDList(idl, buf) then
      begin
        Result := Buf;
      end;
      mm.Free(idl);
    end;
    mm._Release;
  end;

  if (Result = '') and (csidl = CSIDL_PROGRAM_FILES) then
    with TRegistry.Create do  // I2890
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if not OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion') then  // I2890
        raise ERegistryException.Create(LastErrorMsg + ' ('+IntToHex(LastError,8)+')');
      Result := ReadString('ProgramFilesDir');
    finally
      Free;
    end;
  if Result <> '' then
    if Result[Length(Result)] <> '\' then Result := Result + '\';
end;

function MakeULong(A, B: Word): DWord;
begin
  Result := A or B shl 16;
end;

function MAKELCID(wLanguageID, wSortID: WORD): DWORD;
begin
    Result := (DWORD(wSortID) shl 16) or DWORD(wLanguageID);
end;

function DeleteFileOnReboot(const FileName: string): Boolean;
begin
  Result := True;
  MoveFileEx(PChar(FileName), nil, MOVEFILE_DELAY_UNTIL_REBOOT);
end;

type
{ IPersist interface }

  {$EXTERNALSYM IPersist }
  IPersist = interface(IUnknown)
    function GetClassID(var classID: TCLSID): HResult; stdcall;
  end;

{ IPersistFile interface }

  {$EXTERNALSYM IPersistFile }
  IPersistFile = interface(IPersist)
    function IsDirty: HResult; stdcall;
    function Load(pszFileName: POleStr; dwMode: Longint): HResult; stdcall;
    function Save(pszFileName: POleStr; fRemember: BOOL): HResult; stdcall;
    function SaveCompleted(pszFileName: POleStr): HResult; stdcall;
    function GetCurFile(var pszFileName: POleStr): HResult; stdcall;
  end;

  {$EXTERNALSYM IShellLinkDataList}
  IShellLinkDataList = interface(IUnknown)
    ['{45e2b4ae-b1c3-11d0-b92f-00a0c90312e1}']
    function AddDataBlock(pDataBlock: Pointer): HRESULT; stdcall;
    function CopyDataBlock(dwSig: DWORD; var ppDataBlock: Pointer): HRESULT; stdcall;
    function RemoveDataBlock(dwSig: DWORD): HRESULT; stdcall;
    function GetFlags(var pdwFlags: DWORD): HRESULT; stdcall;
    function SetFlags(dwFlags: DWORD): HRESULT; stdcall;
  end;

const
  {$EXTERNALSYM IID_IPersistFile}
  IID_IPersistFile: TGUID = (
    D1:$0000010B;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));

  SLDF_RUNAS_USER = $2000;

function CreateLink(FProg, FArgs, LinkPath, Desc: string; FWorkingDirectory: string = ''; IconFile: string = ''; IconIndex: Integer = 0; RunAsAdmin: Boolean = False): Boolean;  // I2793
var
    hres: HResult;
    psl: IShellLink;
    ppf: IPersistFile;
    pdl: IShellLinkDataList;
    wsz: WideString;
    buf: array[0..MAX_PATH] of char;
    pbuf: PChar;
  dwFlags: Cardinal;
begin
    // Get a pointer to the IShellLink interface.
    hres := CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IID_IShellLinkW, psl);   // I4590
    if SUCCEEDED(hres) then
    begin
        if Pos('\', FProg) = 0 then
          if SearchPath(nil, PChar(FProg), nil, MAX_PATH, buf, pbuf) <> 0 then
            FProg := buf;

        if FWorkingDirectory <> '' then  psl.SetWorkingDirectory(PChar(FWorkingDirectory));
        psl.SetPath(PChar(FProg));
        psl.SetArguments(PChar(FArgs));
        psl.SetDescription(PChar(Desc));
        if IconFile <> '' then
          psl.SetIconLocation(PChar(IconFile), IconIndex);

        if RunAsAdmin then
        begin
          hres := psl.QueryInterface(IShellLinkDataList, pdl);
          if SUCCEEDED(hres) then
          begin
            hres := pdl.GetFlags(dwFlags);
            if SUCCEEDED(hres) then
            begin
              pdl.SetFlags(dwFlags or SLDF_RUNAS_USER);
            end;
          end;
        end;

        if SUCCEEDED(hres) then
        begin
          // Query IShellLink for the IPersistFile interface for saving the
          // shortcut in persistent storage.
          hres := psl.QueryInterface(IID_IPersistFile, ppf);

          if SUCCEEDED(hres) then
          begin
              wsz := LinkPath;
              hres := ppf.Save(PWideChar(wsz), TRUE);

              if SUCCEEDED(hres) then
              begin
                hres := ppf.SaveCompleted(PWideChar(wsz));
              end;
          end;
        end;
    end;

    Result := SUCCEEDED(hres);
end;

function ExecuteProgram(const cmdline, path: string; var errmsg: string): Boolean;
var
  prog, params: string;
  len, n: Integer;
begin
  Result := False;
  prog := cmdline;
  if prog = '' then
  begin
    errmsg := 'Command line was empty';
    Exit;
  end;
  params := '';
  len := Length(prog);
  if prog[1] = '"' then
  begin
    Delete(prog,1,1);
    n := Pos('"', prog);
    if n > 0 then
    begin
      { Quoted filename -- text after closing quote is parameters }
      params := Trim(Copy(prog, n+1, len));
      Delete(prog, n, len);
    end;
      { Otherwise, no closing quote, so whole cmdline is prog }
  end
  else if Pos(' ', prog) > 0 then
  begin
    { First space indicates start of paramters }
    n := Pos(' ', prog);
    params := Trim(Copy(prog, n+1, len));
    Delete(prog, n, Length(prog));
  end;

  if not TUtilExecute.Shell(0, prog, path, params) then  // I3349
  begin
    errmsg := SysErrorMessage(GetLastError);
    Exit;
  end;

  Result := True;
end;

function Tnt_ShellExecuteExW(execinfo: PShellExecuteInfoW): Boolean;
begin
  Result := ShellExecuteExW{TNT-ALLOW ShellExecuteExW}(execinfo)
end;

//TODO: Leaks a thread handle
function WaitForProgram(WindowHandle: HWND; const cmdline, path, parameters: WideString; var errmsg: WideString; DoSpin: TNotifyEvent; RunAs: Boolean): Cardinal;
var
  execinfo: TShellExecuteInfoW;
begin
  FillChar(execinfo, sizeof(execinfo), 0);
  execinfo.cbSize := SizeOf(execinfo);
  execinfo.fMask := SEE_MASK_NOCLOSEPROCESS;
  execinfo.Wnd := WindowHandle;
  if RunAs then execinfo.lpVerb := 'runas' else execinfo.lpVerb := 'open';
  execinfo.lpFile := PWideChar(cmdline);
  execinfo.lpParameters := PWideChar(parameters);
  execinfo.lpDirectory := PWideChar(path);
  execinfo.nShow := SW_SHOWNORMAL;
  if Tnt_ShellExecuteExW(@execinfo) then
  begin
    repeat
      if not GetExitCodeProcess(execinfo.hProcess, Result) then
      begin
        Result := $FFFFFFFF;
        errmsg := SysErrorMessage(GetLastError);
        Break;
      end;
      DoSpin(nil);
    until Result <> STILL_ACTIVE;
    CloseHandle(execinfo.hProcess);
  end
  else
  begin
    errmsg := SysErrorMessage(GetLastError);
    Result := $FFFFFFFF;
  end;
end;


{-------------------------------------------------------------------------------
 - ExpandFileNameEx                                                            -
 ------------------------------------------------------------------------------}

function ExpandFileNameEx(RootPath, FileName: WideString): WideString;
begin
  RootPath := ExtractFilePath(RootPath);
  if (Copy(FileName,1,1) = '\') or (Copy(FileName,2,1) = ':') then Result := FileName
  else Result := RootPath + FileName;
end;

function ExpandFileNameClean(const Root, FileName: WideString): WideString;
var
  buf: array[0..260] of WideChar;
  p: PWideChar;
begin
  Result := ExpandFileNameEx(ExtractFilePath(Root), FileName);
  if GetFullPathNameW(PWideChar(Result), 260, buf, p) > 0 then Result := buf;
end;

procedure OpenContainingFolder(const FileName: string);
var
  sei: TShellExecuteInfo;
  s: string;
begin
  if not FileExists(FileName)
    then s := '"'+ExtractFilePath(FileName)+'"'
    else s := '/select,"'+FileName+'"';
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(TShellExecuteInfo);
  sei.lpVerb := 'open';
  sei.lpFile := 'explorer';
  sei.lpParameters := PChar(s);
  sei.nShow := SW_SHOWNORMAL;
  ShellExecuteEx(@sei);
end;

type
  TSHLoadIndirectString = function(const pszSource: PWideChar; pszOutBuf: PWideChar; cchOutBuf: UINT; ppvReserved: Pointer): HRESULT; stdcall;

var
  hSHLWApi: THandle = 0;
  FSHLoadIndirectString: TSHLoadIndirectString = nil;

function LoadIndirectString(const name: WideString): WideString;
var
  buf: array[0..260] of WideChar;
begin
  Result := '';

  if hSHLWApi = 0 then
  begin
    hSHLWApi := LoadLibrary('shlwapi.dll');
    if hSHLWApi = 0 then
      Exit;
    FSHLoadIndirectString := GetProcAddress(hSHLWApi, 'SHLoadIndirectString');
  end;

  if not Assigned(FSHLoadIndirectString) then Exit;
  if FSHLoadIndirectString(PWideChar(name), buf, Length(buf), nil) = S_OK then
    Result := buf;
end;

procedure TRegistryHelper.ReadMultiString(const ValueName: string; Strings: TStrings);
var
  valueType: DWORD;
  valueLen: DWORD;
  p: PChar;
  buffer: PByte;
begin
  Strings.Clear;

  if not CheckResult(RegQueryValueEx(CurrentKey, PChar(ValueName), nil, @valueType, nil, @valueLen)) then
    raise ERegistryException.Create('Unable to find value '+ValueName);

  if valueType <> REG_MULTI_SZ then
    raise ERegistryException.Create('Value '+ValueName+' does not have type REG_MULTI_SZ');

  if valueLen = 0 then
    // Empty
    Exit;

  GetMem(buffer, valueLen + sizeof(Char));
  try
    RegQueryValueEx(CurrentKey, PChar(ValueName), nil, nil, PBYTE(buffer), @valueLen);
    buffer[valueLen] := 0;
    buffer[valueLen+1] := 0;

    p := PChar(buffer);
    while p^ <> #0 do
    begin
      Strings.Add(p);
      Inc(p, lstrlen(p) + 1);
    end;
  finally
    FreeMem(buffer);
  end;
end;

initialization
finalization
  if hSHLWApi <> 0 then FreeLibrary(hSHLWApi);
  hSHLWApi := 0;
  FSHLoadIndirectString := nil;
end.

