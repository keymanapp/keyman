//****************************************************
//                       IE-Guid                     *
//                      For Delphi                   *
//                Freeware Component                 *
//                   by                              *
//                                                   *
//        Per Lindsø Larsen                          *
//   http://www.euromind.com/iedelphi                *
//                                                   *
// Contributor:                                      *
// Eran Bodankin (bsalsa) - D2005 update and bug fix *
//  bsalsa@gmail.com                                *
//                                                   *
// Documentation and updated versions:               *
//               http://www.bsalsa.com               *
//****************************************************

{*******************************************************************************}
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DocUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SystemS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SystemS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 3 conditions:
1. In your website, add a Link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
4. You may consider donation in our web site!
{*******************************************************************************}
//$Id: IEGuid.pas,v 1.2 2006/11/15 21:01:42 sergev Exp $


unit IEGuid;

interface

uses
  Mshtml_Ewb, Clipbrd, Comobj, Activex, Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs;

type

  TIEGuid = class(TObject)
  private
    function LoadList(Fname: string): Integer;
  public
    Names: TStringlist;
    Guids: TStringlist;
    function NameFromGuid(Guid: TGUID): string;
    function NameFromGuidStr(GuidStr: string): string;
    function CopyToClipboard(GuidName: string): HResult;
    function GetInterfaces(Unk: IUnknown; const S: TStrings): HResult;
    function GetServices(Unk: IUnknown; rsid: string; const S: TStrings): HResult;
    function GetConnectionPoints(Unk: IUnknown; const S: TStrings; ShowIDispatch: Boolean): HResult;
    procedure GetPropertyList(const Obj: IDispatch; const S: TStrings);
    function GetDispatchFromName(const Disp: IDispatch; const PropertyName: WideString): OleVariant;
    function GetInterfacesEx(Unk: IUnknown;
      const S: TStrings; ShowIUnknown, ShowIDispatch, ShowIDispatchEx, ShowDispinterfaces: Boolean): HResult;
    destructor Destroy; override;
    constructor Create(const fname: string);
  end;

function CreateIEGuid(HeadersDir, GuidFile: string): Integer;
function CreateIEList(Guidfile, IEList: string): Integer;

implementation

{ TIEGuid }

function TIEGuid.GetDispatchFromName(const Disp: IDispatch; const PropertyName: WideString): OleVariant;
var
  PName: PWideChar;
  DispID: Integer;
  ExcepInfo: TExcepInfo;
  DispParams: TDispParams;
  Status: HResult;
begin
  Result := disp <> nil;
  if Result then
  begin
    PName := PWideChar(PropertyName);
    if PName <> 'parentDocument' then
    begin
      if PropertyName = '' then
        Result := DISPID_UNKNOWN
      else
        Disp.GetIDsOfNames(GUID_NULL, @PName, 1, GetThreadLocale, @DispID);
      FillChar(DispParams, SizeOf(DispParams), 0);
      Status := Disp.Invoke(DispID, GUID_NULL, 0, DISPATCH_PROPERTYGET, DispParams,
        @Result, @ExcepInfo, nil);
      if Status <> S_OK then
        DispatchInvokeError(Status, ExcepInfo);
    end;
  end;
end;

procedure TIEGuid.GetPropertyList(const Obj: IDispatch; const S: TStrings);
var
  i: Integer;
  TI: ITypeInfo;
  TA: PTypeAttr;
  FD: PFuncDesc;
  aName: WideString;
  vt: Integer;
begin
  OleCheck(Obj.GetTypeInfo(0, 0, TI));
  OleCheck(TI.GetTypeAttr(TA));
  for i := 0 to TA.cFuncs - 1 do
  begin
    OleCheck(TI.GetFuncDesc(i, FD));
    if (FD.invkind = INVOKE_PROPERTYGET) then
    begin
      TI.GetDocumentation(FD.memid, @aName, nil, nil, nil);
      vt := fd.elemdescFunc.tdesc.vt;
      if (vt = VT_DISPATCH) or (vt = VT_PTR) then
        S.add(aName);
    end;
    TI.ReleaseFuncDesc(FD);
  end;
  TI.ReleaseTypeAttr(TA);
end;

// Create list of .h-files in Headers directory

function GetFileList(const Path: string; var FileList:
  TStringList): Boolean;
var
  SearchRec: TSearchRec;
  ff: Integer;
begin
  GetFileList := False;
  ff := FindFirst(Path + '\*.*', faAnyFile, SearchRec);
  if ff = 0 then
  begin
    GetFileList := True;
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      if (SearchRec.Attr and $10 <> $10)
        then
        if Pos('.h', SearchRec.Name) > 0 then
          FileList.Add(Path + '\' + SearchRec.Name);
    end;
    repeat
      ff := FindNext(SearchRec);
      if ff = 0 then
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          if (SearchRec.Attr and $10 <> $10) then
            FileList.Add(Path + '\' + SearchRec.Name);
        end;
      end;
    until ff <> 0;
  end;
end;

function FindStr(S: string; I: Integer; Token: string): string;
var
  counter, t, t1, t2: Shortint;
begin
  S := Token + S + Token;
  counter := 1;
  t := 0;
  while t < I do
  begin
    if Copy(S, counter, 1) = Token then
      Inc(t);
    inc(counter);
  end;
  t1 := counter;
  if Copy(S, counter, 1) = token then
    Result := ''
  else
  begin
    Inc(Counter);
    while Copy(S, counter, 1) <> Token do
      Inc(counter);
    t2 := counter - t1;
    Result := copy(S, t1, t2);
  end;
end;

function ExtractDefineGuid(GuidStr: string): string;
var
  o, Temp, s, G: string;
  X: Integer;
begin
  GuidStr := Trim(StringReplace(GuidStr, ' ', '', [rfReplaceAll, rfIgnoreCase]));
  Guidstr := Stringreplace(GuidStr, Chr($09), '', [rfReplaceAll]);
  g := Copy(GuidStr, 13, Pos(',', GuidStr) - 13);
  S := Uppercase(Copy(GuidStr, Pos('0x', GuidStr), 255));
  S := Trim(StringReplace(S, ');', '', [rfReplaceAll, rfIgnoreCase]));
  S := StringReplace(S, '0x', '', [rfReplaceAll, rfIgnoreCase]);
  S := StringReplace(S, 'L', '', [rfReplaceAll, rfIgnoreCase]);
  X := 1;
  o := '';
  repeat
    Temp := Findstr(S, X, ',');
    if (Length(Temp) = 1) or (Length(temp) = 3) or (Length(temp) = 7) then
      o := o + '0' + temp
    else
      o := o + temp;
    Inc(x);
  until Temp = '';
  s := o;
  S := StringReplace(S, ',', '', [rfReplaceAll, rfIgnoreCase]);
  s := Trim('{' + Copy(s, 1, 8) + '-' + Copy(S, 9, 4) + '-' + Copy(s, 13, 4) + '-' + Copy(s, 17, 4) + '-' + Copy(s, 21, 12) + '}');
  if (Length(s) <> 38) or (Pos('name', g) > 0) or (Pos('DEFINE_', S) > 0) then
    Result := ''
  else
    Result := g + '=' + S;
end;

//Extract GUIDS Registry: HKEY_CLASSES_ROOT\Interfaces

procedure GetGuidsFromRegistry(Guids: TStringList);
var
  dwIndex: DWORD;
  cb: Integer;
  szIID: array[0..80] of char;
  szvalue: array[0..256] of char;
  hk: HKEY;
  Str: string;
begin
  cb := SizeOf(szValue);
  RegOpenKey(HKEY_CLASSES_ROOT, 'Interface', hk);
  dwIndex := 0;
  while RegEnumKey(hk, dwindex, szIID, SizeOf(szIID)) = ERROR_SUCCESS
    do
  begin
    szValue := '';
    RegQueryValue(hk, szIID, szvalue, cb);
    if (szValue <> '') then
    begin
      str := string(szvalue) + '=' + szIID;
      if Guids.IndexOf(str) = -1 then
        Guids.Add(str);
    end;
    inc(dwIndex);
  end;
end;

function CreateIEGuid(HeadersDir, GuidFile: string): Integer;
var
  files, lines, Guids: TStringlist;
  S, n, g: string;
  X, Fcounter, Lcounter: Integer;
begin
  Files := TStringlist.Create;
  Lines := TStringlist.Create;
  Guids := TStringlist.Create;
  Guids.Sorted := True;
  GetFileList(HeadersDir, Files);
  for FCounter := 0 to Files.Count - 1 do
  begin
    Lines.LoadFromFile(Files[FCounter]);
    for LCounter := 0 to Lines.Count - 1 do
    begin
      if Pos('MIDL_INTERFACE("', Lines[LCounter]) > 0 then
      //(1) Extract GUIDS from MIDL_INTERFACE("... lines in header files
      begin
        g := UpperCase('{' + Copy(Trim(Lines[LCounter]), 17, 36) + '}');
        n := Copy(Trim(Lines[LCounter + 1]), 1, 255);
        if n = '' then
          N := Copy(Trim(Lines[LCounter + 2]), 1, 255);
        n := Copy(n, 1, Pos(' ', n) - 1);
        S := n + '=' + g;
        if (Guids.IndexOf(S) < 0) and (n <> '') then
          Guids.Add(S);
      end
      else
        if Pos('DEFINE_GUID(', Trim(Lines[LCounter])) = 1 then
        begin
          //(2) Extract GUIDS from DEFINE_GUID("... lines in header files
          n := Lines[LCounter];
          x := LCounter;
          while (pos(');', n) = 0) and (x < lines.count) do
          begin
            Inc(x);
            n := n + Lines[x];
          end;
          S := ExtractDefineGuid(n);
          if s <> '' then
            if Guids.IndexOf(s) < 0 then
              Guids.Add(s);
        end
        else
          if (pos('__declspec(uuid("', Lines[LCounter]) > 0) and (Pos(';', Lines[LCounter]) > 0) then
          begin
            // (3) Extract GUIDS from __declspec(uuid("... lines in header files
            g := Copy(Lines[LCOunter], Pos('declspec(uuid("', Lines[LCounter]) + 15, 255);
            n := '{' + Copy(G, 1, Pos('"', G) - 1) + '}';
            g := Copy(g, Pos(' ', g), 255);
            g := Trim(StringReplace(g, ';', '', [rfReplaceAll]));
            S := g + '=' + Uppercase(n);
            if Guids.IndexOf(S) < 0 then
              Guids.Add(S);
          end;
    end;
  end;
  Files.Free;
  Lines.Free;
  GetGuidsFromRegistry(Guids);
  Result := Guids.Count;
  Guids.SaveToFile(GuidFile);
  Guids.Free;
end;

function CreateIEList(GuidFile, Ielist: string): Integer;
var
  S: string;
  i: Integer;
  Temp: TStringlist;
  Guids: TStringlist;
begin
  Temp := TStringlist.Create;
  Guids := TStringlist.Create;
  Guids.LoadFromFile(GuidFile);
  for I := 0 to Guids.Count - 1 do
  begin
    s := Uppercase(Guids[i]);

    if (pos('DISP', S) = 1) or (pos('HTML', S) = 1) or (pos('SID_', S) = 1) or
      (pos('DWEB', S) = 1) or (pos('CGID', S) = 1) or ((pos('I', S) = 1) and (pos('IID_', S) = 0))
      then
      Temp.Add(Guids[i])
    else
      if (pos('IID_I', S) = 1) and (Guids.IndexOf(Copy(S, 5, 255)) = -1) then
        Temp.add(copy(guids[i], 5, 255));

  end;
  if Temp.IndexOf('CGID_MSHTML={DE4BA900-59CA-11CF-9592-444553540000}') = -1 then
    Temp.Add('CGID_MSHTML={DE4BA900-59CA-11CF-9592-444553540000}');

  Temp.SaveToFile(IEList);
  Result := Temp.Count;
  Temp.Free;
  Guids.Free;
end;

function TIEGuid.LoadList(Fname: string): Integer;
var
  X: Integer;
  Temp: TStringlist;
begin
  Guids.Clear;
  Names.Clear;
  Temp := TStringlist.Create;
  try
    Temp.LoadFromFile(FName);
    for x := 0 to Temp.Count - 1 do
    begin
      Guids.Add(Copy(Temp[x], Pos('=', Temp[x]) + 1, 255));
      Names.Add(Copy(Temp[x], 1, Pos('=', Temp[x]) - 1));
    end;
    Result := Guids.Count;
  finally
    Temp.Free;
  end;
end;

destructor TIEGuid.Destroy;
begin
  if Names <> nil then
    Names.Free;
  if Guids <> nil then
    Guids.Free;
  inherited;
end;

constructor TIEGuid.Create(const fname: string);
begin
  inherited Create;
  if FileExists(fname) then
  begin
    Names := TStringlist.Create;
    Guids := TStringlist.Create;
    loadlist(Fname);
  end;
end;

function TIEGuid.NameFromGuidStr(GuidStr: string): string;
var
  i: Integer;
begin
  i := Guids.IndexOf(GuidStr);
  if i > -1 then
    Result := Names[i]
  else
    Result := GuidStr;
end;

function TIEGuid.CopyToClipboard(GuidName: string): HResult;
var
 //  s: string;
  x: Integer;
begin
  x := Names.IndexOf(guidname);
  if x > -1 then
  begin
    ClipBoard.SetTextBuf(Pchar(Names[x] + ' : TGUID = ''' + Guids[x] + ''';'));
    Result := S_OK;
  end
  else
    Result := S_FALSE;
end;

function TIEGuid.NameFromGuid(Guid: TGUID): string;
var
  s: string;
  i: Integer;
begin
  s := GuidToString(Guid);
  i := Guids.IndexOf(s);
  if i > -1 then
    Result := Names[i]
  else
    Result := S;
end;

function TIEGuid.GetServices(Unk: IUnknown; rsid: string; const S: TStrings): HResult;
var
  Isp: IServiceprovider;
  x: Integer;
  i: IUnknown;
  G, N: string;
begin
  Result := S_FALSE;
  x := Names.IndexOf(rsid);
  if ((rsid <> '') and (x = -1)) or not Assigned(unk) then
    Exit;
  if x > -1 then
  begin
    G := Guids[x];
    n := Names[x];
  end;
  if Succeeded(Unk.QueryInterface(IServiceprovider, isp))
    then
    for x := 0 to Guids.Count - 1 do
    begin
      if rsid = '' then
      begin
        G := Guids[x];
        N := Names[x];
      end;
      try
        if isp.QueryService(StringtoGuid(G), StringtoGuid(Guids[x]), i) = S_OK
          then
          S.Add(Names[x]);
      except
        ShowMessage('Invalid GUID: ' + Names[x]);
      end;
      Result := S_OK;
    end;
end;

function TIEGuid.GetInterfaces(Unk: IUnknown;
  const S: TStrings): HResult;
var
  i: IUnknown;
  x: Integer;
begin
  Result := S_OK;
  if not Assigned(unk) then
  begin
    Result := S_FALSE;
    Exit;
  end
  else
    for x := 0 to Guids.count - 1 do
    try
      if Succeeded(unk.QueryInterface(StringToGuid(Guids[x]), i)) then
        S.Add(Names[x]);
    except
      ShowMessage('Invalid GUID: ' + Names[x]);
    end;
end;

function TIEGuid.GetInterfacesEx(Unk: IUnknown;
  const S: TStrings; ShowIUnknown, ShowIDispatch, ShowIDispatchEx, ShowDispinterfaces: Boolean): HResult;
var
  I: IUnknown;
 //  Show: Boolean;
  x: Integer;
begin
  Result := S_OK;
  if not Assigned(unk) then
  begin
    Result := S_FALSE;
    Exit;
  end
  else
    for x := 0 to Guids.count - 1 do
    try
      if Succeeded(unk.QueryInterface(StringToGuid(Guids[x]), i)) then
        if ((not ShowIdispatch and (UpperCase(Names[x]) = 'IDISPATCH')) or
          (not ShowIdispatchEx and (UpperCase(Names[x]) = 'IDISPATCHEX')) or
          (not ShowIUnknown and (UpperCase(Names[x]) = 'IUNKNOWN'))) or
          (not ShowDispInterfaces and (Pos('DISP', UpperCase(Names[x])) = 1)) then
        else
          S.Add(Names[x]);
    except
      ShowMessage('Invalid GUID for: ' + Names[x]);
    end;
end;

function TIEGuid.GetConnectionPoints(Unk: IUnknown; const S: TStrings; ShowIDispatch: Boolean): HResult;
var
  IID: TGuid;
  CPC: IConnectionPointContainer;
  iecp: IEnumConnectionPoints;
  cp: IConnectionPoint;
  Fetched: Integer;
begin
  Result := S_FALSE;
  if Assigned(unk) then
  begin
    if Succeeded(Unk.QueryInterface(IConnectionPointContainer, CPC)) then
    begin
      CPC.EnumConnectionPoints(iecp);
      iecp.Next(1, cp, @Fetched);
      repeat
        cp.GetConnectionInterface(iid);
        if (Uppercase(NameFromGuid(IID)) = 'IDISPATCH') and not ShowIdispatch
        then else
          S.Add(NameFromGuid(IID));
        iecp.Next(1, cp, @Fetched);
      until fetched = 0;
      Result := S_OK
    end;
  end;
end;

end.

