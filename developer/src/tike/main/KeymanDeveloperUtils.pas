(*
  Name:             KeymanDeveloperUtils
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    25 Oct 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    02 Aug 2006 - mcdurdin - Timeout when Beta expires
                    23 Aug 2006 - mcdurdin - Add WM_USER_WEBCOMMAND and WM_USER_INPUTLANGCHANGE
                    14 Sep 2006 - mcdurdin - Implement install visual keyboard
                    14 Sep 2006 - mcdurdin - Fix crash when installing keyboard or package
                    28 Sep 2006 - mcdurdin - Add Uninstall package support
                    04 Dec 2006 - mcdurdin - Support uninstalling a keyboard when in a package
                    04 Jan 2007 - mcdurdin - Add WC_COMMAND and WC_HELP options for WM_USER_WEBCOMMAND
                    23 Aug 2007 - mcdurdin - I826 - Fix open file from explorer not working
                    12 Oct 2007 - mcdurdin - I654 - Handle crash when installing or uninstalling a keyboard or visual keyboard or package
                    16 Jan 2009 - mcdurdin - I1043, I1138 - Fix crash installing keyboards (and use KMShell to install so we get full UI)
                    27 Jan 2009 - mcdurdin - I1814 - Add 'runas' parameter
                    25 May 2010 - mcdurdin - I2392 - Activation Client integration
                    22 Oct 2010 - mcdurdin - I2519 - Keyman Developer crashes when trying to install a keyboard
                    10 Dec 2010 - mcdurdin - I2558 - Remove LazyWrite
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    17 Aug 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    19 Oct 2012 - mcdurdin - I3475 - V9.0 - Refactor legacy program execution functions
                    23 Mar 2012 - mcdurdin - I3283 - Debugger should start Keyman if not running
                    03 Nov 2012 - mcdurdin - I3503 - V9.0 - Merge of I3283 - Debugger should start Keyman if it isn't running
                    21 Aug 2013 - mcdurdin - I3893 - V9.0 - Keyman Developer crashes if Desktop not installed
                    04 Dec 2013 - mcdurdin - I3655 - V9.0 - Keyboard debugger does not appear to function in 9.0.419.0
                    12 Jun 2014 - mcdurdin - I4264 - V9.0 - If Keyman Developer is running, double clicking a file in Explorer doesn't open it
                    30 Apr 2015 - mcdurdin - I4682 - V9.0 - Installing a keyboard with an OSK from Developer fails to install the OSK
                    22 Jun 2015 - mcdurdin - I4749 - Opening a keyboard externally can open it multiple times
                    23 Jun 2015 - mcdurdin - I4749 - Opening a keyboard externally can open it multiple times
                    25 Oct 2016 - mcdurdin - I5135 - Remove product and licensing references from Developer projects
*)
unit KeymanDeveloperUtils;  // I3306

interface

uses
  System.UITypes,
  System.Win.ComObj,
  Vcl.Graphics,
  Winapi.Messages,
  Winapi.Windows,

  KeymanPaths,
  Sentry.Client,
  UserMessages;

const
  MAXMRU = 4;
  MAX_REG_FONT_ENTRIES = 16;

function GetFolderPath(csidl: Integer): string;
function CreateLink(const ObjPath, LinkPath, Desc: string): Boolean;
procedure SetFontFromString(f: TFont; s: string);
function FontAsString(f: TFont): string;

procedure InstallKeyboard(const nm: string; FCanInstallUnreg: Boolean);
procedure InstallPackage(const nm: string; FCanInstallUnreg: Boolean);
function UninstallKeyboard(const nm: string): Boolean;
function UninstallPackage(const nm: string): Boolean;

function GetKeymanInstallPath: string;

function IsBitmapLine(s: string): Boolean;
function GetBitmapNameFromLine(FFileName: string; s: string): string;

procedure RemoveOldestTikeEditFonts(FMaxLessOne: Boolean);
procedure RemoveOldestTikeTestFonts(FMaxLessOne: Boolean);
function GetCurrentDateTime: string;
function WaitForElevatedConfiguration(WindowHandle: THandle; const Parameters: WideString; FWait: Boolean): Cardinal;   // I3655

procedure TestSentry;

implementation

uses
  Winapi.ShellApi,
  Classes, SysUtils,
  ErrorControlledRegistry, ActiveX, shlobj, RegistryKeys, //Dialogs,
     utilsystem, Forms, kmxfile, Dialogs, utilexecute,
     KeymanVersion, VisualKeyboard, Controls;

function GetKMShellPath(var ps: string): Boolean; forward;  // I3655

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

const
  {$EXTERNALSYM IID_IPersistFile}
  IID_IPersistFile: TGUID = (
    D1:$0000010B;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));

function CreateLink(const ObjPath, LinkPath, Desc: string): Boolean;
var
    hres: HResult;
    psl: IShellLink;
    ppf: IPersistFile;
    i: Integer;
    InQuotes: Boolean;
    FArgs, FProg: string;
begin
    // Get a pointer to the IShellLink interface.
    hres := CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IID_IShellLinkA, psl);
    if SUCCEEDED(hres) then
    begin
        // Set the path to the shortcut target and add the description.
        i := 1; InQuotes := False;
        FArgs := ''; FProg := ObjPath;
        while i <= Length(ObjPath) do
        begin
            if (ObjPath[i] = ' ') and not InQuotes then
            begin
                FArgs := Copy(ObjPath, i+1, Length(ObjPath));
                FProg := Copy(ObjPath, 1, i-1);
                Break;
            end
            else if ObjPath[i] = '"' then InQuotes := not InQuotes;
            Inc(i);
        end;
        psl.SetPath(PChar(FProg));
        psl.SetArguments(PChar(FArgs));
        psl.SetDescription(PChar(Desc));
        //psl.SetIconLocation(PChar(FIcon), FIconIndex);

        // Query IShellLink for the IPersistFile interface for saving the
        // shortcut in persistent storage.
        hres := psl.QueryInterface(IID_IPersistFile, ppf);

        if SUCCEEDED(hres) then
        begin
          // Save the link by calling IPersistFile::Save.
          hres := ppf.Save(PChar(LinkPath), TRUE);
          //ppf.Release;
        end;
        //psl.Release;
    end;

    Result := SUCCEEDED(hres);
end;

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
                if Result <> '' then
                    if Result[Length(Result)] <> '\' then Result := Result + '\';
            end;
            mm.Free(idl);
        end;
    end;
end;

function FontAsString(f: TFont): string;
begin
  Result := f.Name + '|' + IntToStr(f.Size) + '|';
  if fsBold in f.Style then Result := Result + 'B' else Result := Result + ' ';
  if fsItalic in f.Style then Result := Result + 'I' else Result := Result + ' ';
  if fsUnderline in f.Style then Result := Result + 'U' else Result := Result + ' ';
  Result := Result + '|' + IntToStr(f.CharSet);
end;

procedure SetFontFromString(f: TFont; s: string);
var
  n: Integer;
  fn: string;
  fsz: Integer;
  fb, fi, fu: Boolean;
  fch: Integer;
  FontOnChange: TNotifyEvent;
begin
  if s = '' then
  begin
    // We set to Consolas, 12px as default
    f.Name := 'Consolas';
    f.Charset := ANSI_CHARSET;    // 02/02/2003 MCD: Added, charset not set when resetting to default font
    f.Height := 12;
    f.Style := [];
    f.Color := clWindowText;
    Exit;
  end;

  n := Pos('|', s); if n = 0 then Exit;
  fn := Copy(s, 1, n-1); Delete(s, 1, n);
  n := Pos('|', s); if n = 0 then Exit;
  fsz := StrToIntDef(Copy(s, 1, n-1), 0); if fsz = 0 then Exit;
  Delete(s, 1, n);
  if Length(s) < 5 then Exit;
  s := UpperCase(s);
  fb := s[1] = 'B';
  fi := s[2] = 'I';
  fu := s[3] = 'U';
  Delete(s, 1, 4);
  fch := StrToIntDef(s, DEFAULT_CHARSET);

  FontOnChange := f.OnChange;
  f.OnChange := nil;
  try
    f.Name := fn;
    f.Size := fsz;
    f.Style := [];
    if fb then f.Style := f.Style + [fsBold];
    if fi then f.Style := f.Style + [fsItalic];
    if fu then f.Style := f.Style + [fsUnderline];
    f.Charset := fch;
  finally
    f.OnChange := FontOnChange;
  end;
  if Assigned(f.OnChange) then f.OnChange(f);
end;

procedure InstallKeyboard(const nm: string; FCanInstallUnreg: Boolean);   // I4682
var
  kmshell: string;
begin
  if not GetKMShellPath(kmshell) then   // I3655
    Exit;

  if not TUtilExecute.WaitForProcess('"'+kmshell+'" -i "'+nm+'"', ExtractFilePath(nm)) then  // I3475
    ShowMessage('Failed to install keyboard');
end;

function IsKeymanDesktopInstalled: Boolean;
begin
  try
    Result := FileExists(TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_KMShell));
  except
    on E:EKeymanPath do
      Result := False;
  end;
end;

procedure InstallPackage(const nm: string; FCanInstallUnreg: Boolean);
var
  kmshell: string;
begin
  if not GetKmshellPath(kmshell) then
    Exit;

  if TUtilExecute.WaitForProcess('"'+kmshell+'" -i "'+nm+'"', ExtractFilePath(nm)) = False then  // I3475
    ShowMessage('Failed to install package');
end;

function UninstallKeyboard(const nm: string): Boolean;
var
  kmshell: string;
begin
  if not GetKmshellPath(kmshell) then
    Exit(False);

  Result := TUtilExecute.WaitForProcess('"'+kmshell+'" -uk "'+nm+'"', ExtractFilePath(kmshell));  // I3475
end;

function UninstallPackage(const nm: string): Boolean;
var
  kmshell: string;
begin
  if not GetKmshellPath(kmshell) then
    Exit(False);

  Result := TUtilExecute.WaitForProcess('"'+kmshell+'" -up "'+nm+'"', ExtractFilePath(kmshell));  // I3475
end;



function GetKeymanInstallPath: string;
var
  RootPath: string;
begin
  RootPath := ExtractFilePath(ParamStr(0));
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeymanEngine_LM) then
      if ValueExists(SRegValue_RootPath) then
        RootPath := ReadString(SRegValue_RootPath);
  finally
    Free;
  end;
  Result := IncludeTrailingPathDelimiter(RootPath);

  if not FileExists(Result + 'keyman32.dll') then
    ShowMessage('The executable keyman32.dll could not be found.  You should reinstall '+SKeymanDesktopName+'.');
end;


function IsBitmapLine(s: string): Boolean;
begin
  Result := True;

  s := UpperCase(Trim(s));
  if Copy(s, 1, 6) = 'BITMAP' then Exit;
  if Pos('&BITMAP', s) > 0 then Exit;

  Result := False;
end;

// FFileName = filename of .kmn file; s is line of file with BITMAP "..."
function GetBitmapNameFromLine(FFileName: string; s: string): string;
var
  n: Integer;
begin
  Result := '';

  s := Trim(s);
  if UpperCase(Copy(s, 1, 6)) = 'BITMAP' then
  begin
    Delete(s,1,6);
    if Copy(s,1,1) = 'S' then
    begin
      Delete(s,1,1);
    end;

    n := Pos(',', s);
    if n > 0 then Delete(s, n, Length(s));

    if s = '' then Exit;
    if s[1] <> ' ' then Exit;
    s := Trim(s); if s = '' then Exit;
    if s[1] = '"' then
    begin
      Delete(s,1,1);
      n := Pos('"', s);
    end
    else
      n := Pos(' ', s);
  end
  else if Pos('&BITMAP', UpperCase(s)) > 0 then
  begin
    n := Pos(')', s);
    if n = 0 then Exit;
    Delete(s, 1, n); s := Trim(s);
    if s = '' then Exit;

    if s[1] = '"' then
    begin
      Delete(s,1,1);
      n := Pos('"', s);
    end
    else if s[1] = '''' then
    begin
      Delete(s,1,1);
      n := Pos('''', s);
    end
    else
      Exit;
  end
  else
    Exit;

  if n > 0 then Delete(s,n,Length(s));

  if s = '' then Exit;
  if ExtractFileExt(s) = '' then s := s + '.bmp';

  if (s[1] <> '\') and (s[2] <> ':') then
    s := ExtractFilePath(FFileName) + s;

  Result := s;
end;

function GetCurrentDateTime: string;
var
  s: string;
begin
  //2147483647
  //0103021450
  s := FormatDateTime('yymmddhhnn', Now);
  while s[1] >= '2' do s[1] := Chr(Ord(s[1]) - 1); // stop integer overflow after 2020 ;->
  Result := s;
end;

procedure RemoveOldestTikeFonts(FMaxLessOne: Boolean; const key: string);
var
  str: TStringList;
  i, c, max: Integer;
  s, CurExp: string;
begin
  max := MAX_REG_FONT_ENTRIES;
  if FMaxLessOne then Dec(max);
  CurExp := GetCurrentDateTime;
  str := TStringList.Create;
  with TRegistryErrorControlled.Create do  // I2890
  try
    if not OpenKey(key, True) then  // I2890
      RaiseLastRegistryError;

    GetValueNames(str);

    for i := 0 to str.Count - 1 do
    begin
      s := ReadString(str[i]);
      c := Pos('|', s);
      if (c = 0) or (StrToIntDef(Copy(s,1,c-1), -1) = -1) then
      begin
        WriteString(str[i], CurExp+'|'+s);
        str[i] := Format('%s|%s', [CurExp, str[i]])
      end
      else
        str[i] := Format('%10.10d|%s', [StrToInt(Copy(s,1,c-1)), str[i]]);
    end;

    str.Sort;

    { Delete all old entries }
    for i := 0 to str.Count - max - 1 do
      DeleteValue(Copy(str[i], 16, 260));
  finally
    str.Free;
    Free;
  end;
end;

procedure RemoveOldestTikeEditFonts(FMaxLessOne: Boolean);
begin
  RemoveOldestTikeFonts(FMaxLessOne, SRegKey_IDEEditFonts_CU);
end;

procedure RemoveOldestTikeTestFonts(FMaxLessOne: Boolean);
begin
  RemoveOldestTikeFonts(FMaxLessOne, SRegKey_IDETestFonts_CU);
end;

function GetKMShellPath(var ps: string): Boolean;   // I3655
begin
  if not IsKeymanDesktopInstalled then
  begin
    ShowMessage('Keyman for Windows is not installed.  You must install Keyman to complete this action.');
    Exit(False);
  end;

  try
    ps := TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_KMShell);
  except
    on E:EKeymanPath do
    begin
      ShowMessage('Keyman for Windows is not installed.  You must install Keyman to complete this action.');
      Exit(False);
    end;
  end;
  Result := FileExists(ps);
end;

function WaitForElevatedConfiguration(WindowHandle: THandle; const Parameters: WideString; FWait: Boolean): Cardinal;
var
  execinfo: TShellExecuteInfoW;
  kmshell: string;
begin
  Result := $FFFFFFFF;

  if not GetKMShellPath(kmshell) then
    Exit;

  FillChar(execinfo, sizeof(execinfo), 0);
  execinfo.cbSize := SizeOf(execinfo);
  execinfo.fMask := SEE_MASK_NOCLOSEPROCESS;
  execinfo.Wnd := WindowHandle;
  execinfo.lpVerb := 'runas';
  execinfo.lpFile := PWideChar(kmshell);
  execinfo.lpParameters := PWideChar(Parameters);
  execinfo.lpDirectory := PWideChar(ExtractFileDir(ParamStr(0)));
  execinfo.nShow := SW_SHOWNORMAL;
  if ShellExecuteExW(@execinfo) then
  begin
    if FWait then
    begin
      ShowWindow(WindowHandle, SW_HIDE);
      repeat
        if not GetExitCodeProcess(execinfo.hProcess, Result) then
        begin
          Result := $FFFFFFFF;
          Break;
        end;
        Sleep(10);
        Application.ProcessMessages;
      until Result <> STILL_ACTIVE;
      ShowWindow(WindowHandle, SW_SHOW);
    end
    else
      Result := 0;
    CloseHandle(execinfo.hProcess);
  end
  else
    ShowMessage(SysErrorMessage(GetLastError));
end;

procedure TestSentry;
begin
  raise ESentryTest.Create('Just testing Sentry integration');
end;

end.

