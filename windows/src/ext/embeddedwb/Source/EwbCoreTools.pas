//*************************************************************
//                      EwbCoreTools                          *
//                                                            *
//                     Freeware Unit 			      *
//                       For Delphi                           *
//      Developing Team:                                      *
//          Serge Voloshenyuk (SergeV@bsalsa.com)             *
//          Eran Bodankin (bsalsa) -(bsalsa@gmail.com)       *
//                                                            *
//       Documentation and updated versions:                  *
//                                                            *
//               http://www.bsalsa.com                        *
//*************************************************************
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

You may use/ change/ modify the component under 3 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit  for the benefit
   of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}


unit EwbCoreTools;

{$I EWB.inc}

interface

uses
  Graphics, ActiveX, Mshtml_Ewb, Windows, SysUtils;

function IsWinXPSP2OrLater(): Boolean;
function ColorToHTML(const Color: TColor): string;
function WideStringToLPOLESTR(const Source: Widestring): POleStr;
function XPath4Node(node: IHTMLElement): string;
function TaskAllocWideString(const S: string): PWChar;
function AnsiIndexStr(const AText: string; const AValues: array of string): Integer;
function GetPos(const SubSt, Text: string; StartPos: Integer = -1): Integer;
function _CharPos(const C: Char; const S: string): Integer;
function CutString(var Text: string; const Delimiter: string = ' ';
  const Remove: Boolean = True): string;
procedure FormatPath(Path: string);
function GetWinText(WinHandle: THandle): string;
function GetWinClass(Handle: Hwnd): WideString;
function GetParentWinByClass(ChildHandle: HWND; const ClassName: string): HWND;
{$IFDEF DELPHI5}
function DirectoryExists(const Directory: string): Boolean;
function VarSupports(const V: Variant; const IID: TGUID; out Intf): Boolean;
{$ENDIF}
{$IFNDEF DELPHI12_UP}
function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
{$ENDIF}
function AddBackSlash(const S: string): string;

const
  WM_SETWBFOCUS = $0400 {WM_USER} + $44;

implementation

uses
  EwbIeConst, EwbAcc;

type
   {VerifyVersion}
  fn_VerifyVersionInfo = function(var VersionInformation: OSVERSIONINFOEX;
    dwTypeMask: DWORD; dwlConditionMask: LONGLONG): BOOL; stdcall;
  fn_VerSetConditionMask = function(ConditionMask: LONGLONG; TypeMask: DWORD;
    Condition: Byte): LONGLONG; stdcall;


function IsWinXPSP2OrLater(): Boolean;
var
  osvi: TOSVersionInfoEx;
  dwlConditionMask: LONGLONG;
  op: Integer;
  hlib: THandle;
  VerifyVersionInfo: fn_VerifyVersionInfo;
  VerSetConditionMask: fn_VerSetConditionMask;
begin
  Result := False;
  hLib := GetModuleHandle('kernel32.dll');
  if hLib = 0 then
    hLib := LoadLibrary('kernel32.dll');
  if (hLib <> 0) then
  begin
    @VerifyVersionInfo := GetProcAddress(hLib, 'VerifyVersionInfoA');
    @VerSetConditionMask := GetProcAddress(hLib, 'VerSetConditionMask');
    if ((@VerifyVersionInfo = nil) or (@VerSetConditionMask = nil)) then Exit;

    dwlConditionMask := 0;
    op := VER_GREATER_EQUAL;

    // Initialize the OSVERSIONINFOEX structure.
    ZeroMemory(@osvi, SizeOf(OSVERSIONINFOEX));
    osvi.dwOSVersionInfoSize := SizeOf(OSVERSIONINFOEX);
    osvi.dwMajorVersion := 5;
    osvi.dwMinorVersion := 1;
    osvi.wServicePackMajor := 2;
    osvi.wServicePackMinor := 0;

    // Initialize the condition mask.
    dwlConditionMask := VerSetConditionMask(dwlConditionMask, VER_MAJORVERSION, op);
    dwlConditionMask := VerSetConditionMask(dwlConditionMask, VER_MINORVERSION, op);
    dwlConditionMask := VerSetConditionMask(dwlConditionMask, VER_SERVICEPACKMAJOR, op);
    dwlConditionMask := VerSetConditionMask(dwlConditionMask, VER_SERVICEPACKMINOR, op);

    // Perform the test.
    Result := VerifyVersionInfo(osvi, VER_MAJORVERSION or VER_MINORVERSION or
      VER_SERVICEPACKMAJOR or VER_SERVICEPACKMINOR, dwlConditionMask);
  end;
end;

function GetParentWinByClass(ChildHandle: HWND; const ClassName: string): HWND;
var
  szClass: array[0..255] of Char;
begin
  Result := GetParent(ChildHandle);
  while IsWindow(Result) do
  begin
    if (GetClassName(Result, szClass, SizeOf(szClass)) > 0) and
      (AnsiStrComp(PChar(ClassName), szClass) = 0) then Exit;
    Result := GetParent(Result);
  end;
end;



{$IFNDEF DELPHI12_UP}

function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}

{$IFDEF DELPHI5}

function DirectoryExists(const Directory: string): Boolean;
var
  Code: Integer;
begin
{$RANGECHECKS OFF}
  Code := GetFileAttributes(PChar(Directory));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
{$RANGECHECKS ON}
end;

function VarSupports(const V: Variant; const IID: TGUID; out Intf): Boolean;
begin
  Result := Supports(V, IID, Intf);
end;
{$ENDIF}

function AddBackSlash(const S: string): string;
begin
{$IFDEF DELPHI5}
  Result := IncludeTrailingBackslash(S);
{$ELSE}
{$IFDEF DELPHI6UP}
  Result := IncludeTrailingPathDelimiter(S);
{$ELSE}
  if Copy(S, Length(S), 1) = '\' then
    Result := S
  else
    Result := S + '\';
{$ENDIF}
{$ENDIF}
end;

function CutString(var Text: string; const Delimiter: string = ' ';
  const Remove: Boolean = True): string;
var
  IdxPos: Integer;
begin
  if Delimiter = #0 then
    IdxPos := Pos(Delimiter, Text)
  else
    IdxPos := AnsiPos(Delimiter, Text);

  if (IdxPos = 0) then
  begin
    Result := Text;
    if Remove then
      Text := '';
  end
  else
  begin
    Result := Copy(Text, 1, IdxPos - 1);
    if Remove then
      Delete(Text, 1, IdxPos + Length(Delimiter) - 1);
  end;
end;


function GetPos(const SubSt, Text: string; StartPos: Integer = -1): Integer;
var
  i: Integer;
  LStartPos: Integer;
  LTokenLen: Integer;
begin
  result := 0;
  LTokenLen := Length(SubSt);
  if StartPos = -1 then
  begin
    StartPos := Length(Text);
  end;
  if StartPos < (Length(Text) - LTokenLen + 1) then
  begin
    LStartPos := StartPos;
  end
  else
  begin
    LStartPos := (Length(Text) - LTokenLen + 1);
  end;
  for i := LStartPos downto 1 do
  begin
    if AnsiSameText(Copy(Text, i, LTokenLen), SubSt) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function _CharPos(const C: Char; const S: string): Integer;
begin
  for Result := 1 to Length(S) do
    if S[Result] = C then Exit;
  Result := 0;
end;

procedure FormatPath(Path: string);
var
  i: Integer;
begin
  i := 1;
  while i <= Length(Path) do
  begin
    if CharInSet(Path[i], LeadBytes) then
      Inc(i, 2)
    else
      if Path[i] = '\' then
      begin
        Path[i] := '/';
        Inc(i, 1);
      end
      else
        Inc(i, 1);
  end;
end;

function AnsiIndexStr(const AText: string; const AValues: array of string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(AValues) to High(AValues) do
    if AnsiSameStr(AText, AValues[I]) then
    begin
      Result := I;
      Break;
    end;
end;


function TaskAllocWideString(const S: string): PWChar;
var
  WideLength: integer;
  Wide: PWideChar;
begin
  WideLength := Length(S) + 1;
  Wide := CoTaskMemAlloc(WideLength * SizeOf(WideChar));
  StringToWideChar(S, Wide, WideLength);
  Result := Wide;
end;

{
function TaskAllocWideString(const S: string): PWChar;
var
  Len: Integer;
begin
  Len := Length(S) + 1;
  Result := CoTaskMemAlloc(2 * Len);
  StringToWideChar(S, Result, Len);
end;
}

function WideStringToLPOLESTR(const Source: Widestring): POleStr;
var
  Len: Integer;
begin
  Len := Length(Source) * SizeOf(WideChar);
  Result := CoTaskMemAlloc(Len + 2);
  FillChar(Result^, Len + 2, 0);
  Move(Result^, PWideString(Source)^, Len);
end;

function ColorToHTML(const Color: TColor): string;
var
  ColorRGB: LongWord;
begin
  ColorRGB := ColorToRGB(Color);
  FmtStr(Result, '#%0.2X%0.2X%0.2X',
    [Byte(ColorRGB), Byte(ColorRGB shr 8), Byte(ColorRGB shr 16)]);
end;

function GetWinText(WinHandle: THandle): string;
var
  DlgName: string;
  TxtLength: Integer;
begin
  TxtLength := GetWindowTextLength(WinHandle);
  SetLength(DlgName, TxtLength + 1);
  GetWindowText(WinHandle, PChar(DlgName), TxtLength + 1);
  Result := DlgName;
end;


function GetWinClass(Handle: Hwnd): WideString;
var
  pwc: PWideChar;
const
  maxbufsize = 32767 * SizeOf(WideChar);
begin
  Result := '';
  if IsWindow(Handle) then
  begin
    pwc := GetMemory(maxbufsize);
    if Assigned(pwc) then
    try
      ZeroMemory(pwc, maxbufsize);
      if GetClassnameW(Handle, pwc, maxbufsize) > 0 then
        SetString(Result, pwc, lstrlenW(pwc));
    finally
      FreeMemory(pwc);
    end;
  end;
end;

{
function GetWinClass(WinHandle: THANDLE): string;
begin
  SetLength(Result, 80);
  SetLength(Result, GetClassName(WinHandle, PChar(Result), Length(Result)));
end;
}


function XPath4Node(node: IHTMLElement): string;

  function NodePosition(elem: IHTMLElement): string;
  var tag: Widestring;
    Idx: Integer;
    n: IHTMLElement;
    cl: IHTMLElementCollection;
    itm: IDispatch;
    I, C, mI: Integer;
  begin
    Result := '';
    if (elem.parentElement = nil) or
      not Supports(elem.parentElement.children, IHTMLElementCollection, cl) then Exit;

    Tag := elem.tagName;
    Idx := elem.sourceIndex;
    C := 0;
    mI := -1;

    for I := 0 to cl.length - 1 do
    begin
      itm := cl.item(I, I);
      if Supports(itm, IHTMLElement, n) then
      begin
        if n.tagName = Tag then
        begin
          if n.sourceIndex = Idx then mI := C;
          Inc(C);
        end;
      end;
    end;
    if (mI > 0) or (C > 1) then Result := Format('[%d]', [mI]);
  end;

var id: string;
begin
  if node <> nil then
  begin
    id := node.id;
    if id <> '' then
      Result := Format('%s[@id="%s"]', [node.tagName, id])
    else if node.parentElement = nil then
      Result := '/' + node.tagName
    else Result := Format('%s/%s%s',
        [XPath4Node(node.parentElement), node.tagName, NodePosition(node)]);
  end else Result := '';
end;

end.
