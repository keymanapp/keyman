unit FileLocks;

interface

uses Windows, TlHelp32, psapi, SysUtils, Classes;

function GetFileLocks(const filename: string): string;

implementation

function enumfunc(hwnd: HWND; lParam: Integer): Boolean; stdcall;
var
  str: TStringList;
  s: string;
  outbuf: array[0..256] of char;
  i: Integer;
  dw: Integer;
begin
  Result := True;
  if not IsWindowVisible(hwnd) then Exit;
  str := TStringList(lParam);
  GetWindowThreadProcessID(hwnd, @dw);
  for i := 0 to str.Count - 1 do
  begin
    if Integer(str.Objects[i]) = dw then
    begin
      str.Objects[i] := Pointer($FFFFFFFF);
      if GetWindowText(hwnd, outbuf, 256) = 0 then Exit;
      s := outbuf;
      if s = '' then Exit;
      str[i] := s + ' ('+str[i]+')';
      Exit;
    end;
  end;
end;

function ModList(const filename: string; var pe: TPROCESSENTRY32): string;
var
  hModuleSnap: THandle;
  me32: TMODULEENTRY32;
  szArg: string;
begin
  Result := '';

  FillChar(me32, SizeOf(TMODULEENTRY32), 0);

  // Take a snapshot of all modules in the specified process.

  hModuleSnap := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, pe.th32ProcessID);
  if hModuleSnap = INVALID_HANDLE_VALUE then Exit;
  try
    // Fill the size of the structure before using it.

    me32.dwSize := sizeof(MODULEENTRY32);

    // Walk the module list of the process, and find the module of
    // interest. Then copy the information to the buffer pointed
    // to by lpMe32 so that it can be returned to the caller.

    szArg := ExtractFileName(filename);

    if Module32First(hModuleSnap, me32) then
    begin
      repeat
        if LowerCase(me32.szModule) = LowerCase(szArg) then
        begin
          //GetFullPathName(me32.szExePath, 256, szAll, p);
          Result := pe.szExeFile;
          Exit;
        end;
      until not Module32Next(hModuleSnap, me32);
    end;
  finally
    // Do not forget to clean up the snapshot object.
    CloseHandle(hModuleSnap);
  end;
end;

function GetFileLocks_95(const filename: string): string;
var
  hsnap: THandle;
  pe: TPROCESSENTRY32;
  str: TStringList;
  s: string;
  i: Integer;
begin
  Result := '';

  FillChar(pe, SizeOf(TPROCESSENTRY32), 0);
	pe.dwSize := SizeOf(pe);

	hsnap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  str := TStringList.Create;
  try
    if Process32First(hsnap, pe) then
    begin
      s := ModList(filename, pe);
      if s <> '' then str.Add(s);
      while Process32Next(hsnap, pe) do
      begin
        s := ModList(filename, pe);
        if s <> '' then str.AddObject(s, Pointer(pe.th32ProcessID));
      end;
    end;

    EnumWindows(@enumfunc, Integer(str));
    for i := 0 to str.Count - 1 do
      Result := Result + '   ' + str[i] + #13#10;
  finally
    CloseHandle(hsnap);
    str.Free;
  end;
end;

function ModList_NT(hProcess: THandle; const filename: string): string;
var
  dwIndex, dwSize, dwSize2: DWord;
  lpdwHMODs, pdw: PDWord;
  buf: array[0..256] of char;
begin
  Result := '';
  dwSize2 := 32 * sizeof(DWORD);
  lpdwHMODs := nil;
  repeat
    if Assigned(lpdwHMODs) then
    begin
       FreeMem(lpdwHMODs);
       dwSize2 := dwSize2 * 2;
    end;
    lpdwHMODs := PDWord(AllocMem(dwSize2));
    if not Assigned(lpdwHMODs) then Exit;

    if not EnumProcessModules(hProcess, lpdwHMODs, dwSize2, dwSize) then
    begin
       FreeMem(lpdwHMODs);
       Exit;
    end;
  until dwSize <> dwSize2;

  dwSize := dwSize div sizeof(DWORD);

  pdw := lpdwHMODs;

  for dwIndex := 0 to dwSize - 1 do
  begin
    if GetModuleFileNameEx(hProcess, pdw^, buf, 256) <> 0 then
      if LowerCase(ExtractFileName(buf)) = LowerCase(filename) then
      begin
        GetModuleFileNameEx(hProcess, lpdwHMODs^, buf, 256);
        Result := ExtractFileName(buf);
        FreeMem(lpdwHMODs);
        Exit;
      end;
    Inc(pdw);
  end;

  FreeMem(lpdwHMODs);
end;

function GetFileLocks_NT(const filename: string): string;
var
  str: TStringList;
  dwSize, dwSize2, dwIndex: DWord;
  lpdwPIDs, pdw: LPDWord;
  i: Integer;
  s: string;
  hProcess: THandle;
begin
  Result := '';
  str := TStringList.Create;
  try
    // Enumerate the running processes by doubling size of buffer until buffer is large enough for complete list
    dwSize2 := 256 * sizeof(DWORD);
    lpdwPIDs := nil;
    repeat
      if Assigned(lpdwPIDs) then
      begin
         FreeMem(lpdwPIDs);
         dwSize2 := dwSize2 * 2;
      end;
      lpdwPIDs := PDWord(AllocMem(dwSize2));
      if not Assigned(lpdwPIDs) then Exit;

      if not EnumProcesses(lpdwPIDs, dwSize2, dwSize) then
      begin
         FreeMem(lpdwPIDs);
         Exit;
      end;
    until dwSize <> dwSize2;

    // How many ProcID's did we get?
    dwSize := dwSize div sizeof(DWORD);

    // Loop through each ProcID.
    pdw := lpdwPIDs;
    for dwIndex := 0 to dwSize - 1 do
    begin
      // Open the process (if we can... security does not
      // permit every process in the system).
      hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, FALSE, pdw^);
      if hProcess <> 0 then
      begin
        s := ModList_NT(hProcess, filename);
        if s <> '' then
          str.AddObject(s, Pointer(pdw^));
        CloseHandle(hProcess);
      end;
      Inc(pdw);
    end;

    FreeMem(lpdwPIDs);

    EnumWindows(@enumfunc, Integer(str));
    for i := 0 to str.Count - 1 do
      Result := Result + '   ' + str[i] + #13#10;
  finally
    str.Free;
  end;
end;

function GetFileLocks(const filename: string): string;
begin
  if (GetVersion and $80000000) <> 0
    then Result := GetFileLocks_95(filename)
    else Result := GetFileLocks_NT(filename);
end;


end.
