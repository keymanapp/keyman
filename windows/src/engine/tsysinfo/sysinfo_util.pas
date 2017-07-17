(*
  Name:             sysinfo_util
  Copyright:        Copyright (C) SIL International.
  Documentation:    km4.1
  Description:      Utility functions for system information
  Create Date:      13 May 2005

  Modified Date:    31 Dec 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          13 May 2005 - mcdurdin - Integrated into kmshell from tsysinfo
                    09 Jun 2005 - mcdurdin - Use MSXML_TLB not MSXML2_TLB
                    20 Jul 2008 - mcdurdin - I1523 - Report additional items in tsysinfo
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    19 Jul 2011 - mcdurdin - I3000 - Tweak display of diagnostics using .xslt files
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    26 Jun 2012 - mcdurdin - I3383 - KM9 - ErrorControlledRegistry is obsolete
                    23 Dec 2011 - mcdurdin - I3180 - x64 process details for tsysinfo (move Wow64Process to util)
                    04 Nov 2012 - mcdurdin - I3542 - V9.0 - Merge of I3180 - x64 process details for tsysinfo (and hide irrelevant tabs)
                    31 Dec 2014 - mcdurdin - I4560 - V9.0 - Binary data in diagnostics is not streamed correctly
*)
unit sysinfo_util;

interface

uses Windows, Classes, Sysutils, Registry, ErrorControlledRegistry, msxml;

procedure AddRegistry(node: IXMLDOMNode; key: HKEY; path: string);
procedure AddFileVersion(node: IXMLDOMNode; const afilename: string);
procedure AddFiles(node: IXMLDOMNode; csidl: Integer; const PathName: string);
function TempFileName(ext: string = ''): string;
function GetFolderPath(csidl: Integer): string;
function GetTarget(const LinkFileName:String):String;
function XMLTextEncode(s: WideString): WideString;

function xmlAddChild(node: IXMLDOMNode; const name: WideString): IXMLDOMNode;
procedure xmlSetAttribute(node: IXMLDOMNode; const name, value: WideString);
function xmlGetAttribute(node: IXMLDOMNode; const name: WideString): WideString;
function xmlChildValue(node: IXMLDOMNode; const name: WideString): Variant;
function xmlChild(node: IXMLDOMNode; const name: WideString): IXMLDOMNode;

function StrToBoolDef(const str: string; res: Boolean): Boolean;
function VarToStrDef(v: Variant; def: string): string;

function FileSizeKB(sz: Int64): string;

function Wow64Process: Boolean;  // I2919  // I3180   // I3542

implementation

uses
  ActiveX,
  FileVersionInfo,
  kmxfile,
  ShlObj,
  TTInfo,
  Unicode,
  Variants;

const
  CSIDL_PROGRAM_FILES = 1;

function XMLTextEncode(s: WideString): WideString;
begin
  Result := s; //UTF16ToUtf8(s);
end;


function xmlAddChild(node: IXMLDOMNode; const name: WideString): IXMLDOMNode;
begin
  Result := node.ownerDocument.createNode(1, name, '');
  node.appendChild(Result);
end;

procedure xmlSetAttribute(node: IXMLDOMNode; const name, value: WideString);
var
  nattr: IXMLDOMNode;
begin
  nattr := node.ownerDocument.createAttribute(name);
  nattr.nodeValue := value;
  node.attributes.setNamedItem(nattr);
end;

function xmlGetAttribute(node: IXMLDOMNode; const name: WideString): WideString;
var
  nattr: IXMLDOMNode;
begin
  nattr := node.attributes.getNamedItem(name);
  if Assigned(nattr)
    then Result := nattr.nodeValue
    else Result := '';
end;

function xmlChild(node: IXMLDOMNode; const name: WideString): IXMLDOMNode;
var
  i: Integer;
begin
  for i := 0 to node.childNodes.Length - 1 do
    if node.childNodes.item[i].nodeName = name then
    begin
      Result := node.childNodes.item[i];
      Exit;
    end;
  Result := nil;
end;

function xmlChildValue(node: IXMLDOMNode; const name: WideString): Variant;
var
  i: Integer;
begin
  for i := 0 to node.childNodes.Length - 1 do
    if node.childNodes.item[i].nodeName = name then
    begin
      Result := node.childNodes.item[i].text;
      Exit;
    end;
  Result := Null;
end;

procedure AddRegistry(node: IXMLDOMNode; key: HKEY; path: string);
  procedure AddRegValue(node: IXMLDOMNode; const name, regtype, value: string);
  begin
    node := xmlAddChild(node, 'Value');
    xmlSetAttribute(node, 'Name', XMLTextEncode(name));
    xmlSetAttribute(node, 'Type', XMLTextEncode(regtype));
    xmlSetAttribute(node, 'Value', XMLTextEncode(value));
  end;

  function BinaryToHex(buf: PAnsiChar; sz: Integer): string;   // I4560
  var
    i: Integer;
  begin
    Result := '';
    for i := 1 to sz do
    begin
      Result := Result + IntToHex(Ord(buf^), 2);
      if (i mod 16) = 0 then Result := Result + ' ';
      Inc(buf);
    end;
  end;
  
var
  i: Integer;
  str: TStringList;
  sz: Integer;
  buf: PAnsiChar;
begin
  str := TStringList.Create;
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := key;
    if OpenKeyReadOnly(path) then
    begin
      node := xmlAddChild(node, 'Key');
      if key = HKEY_LOCAL_MACHINE then
        xmlSetAttribute(node, 'Path', XMLTextEncode('HKEY_LOCAL_MACHINE\'+path))
      else if key = HKEY_CURRENT_USER then
        xmlSetAttribute(node, 'Path', XMLTextEncode('HKEY_CURRENT_USER\'+path))
      else  if key = HKEY_CLASSES_ROOT then
        xmlSetAttribute(node, 'Path', XMLTextEncode('HKEY_CLASSES_ROOT\'+path))
      else
        xmlSetAttribute(node, 'Path', XMLTextEncode('?\'+path));
      GetValueNames(str);
      for i := 0 to str.Count - 1 do
      begin
        case GetDataType(str[i]) of
          rdUnknown: AddRegValue(node, str[i], 'Unknown', '?'); // node.AddChild('Value').
          rdString:  AddRegValue(node, str[i], 'String', ReadString(str[i]));
          rdExpandString: AddRegValue(node, str[i], 'ExpandString', ReadString(str[i]));
          rdInteger: AddRegValue(node, str[i], 'Integer', IntToStr(ReadInteger(str[i])));
          rdBinary:
            begin
              sz := GetDataSize(str[i]);
              buf := AllocMem(sz);
              if buf <> nil then
              begin
                ReadBinaryData(str[i], buf^, sz);
                AddRegValue(node, str[i], 'Binary', BinaryToHex(buf, sz));
                FreeMem(buf);
              end
              else
                AddRegValue(node, str[i], 'Binary', 'ERROR');
            end;
        end;
      end;
      str.Clear;
      GetKeyNames(str);
      for i := 0 to str.Count - 1 do
        AddRegistry(node, key, path + '\' + str[i]);
    end;
  finally
    Free;
    str.Free;
  end;
end;

procedure AddFileVersion(node: IXMLDOMNode; const afilename: string);
begin
  if FileExists( afilename) then
  begin
    with TFileVersionInfo.Create(afilename) do
    try
      node := xmlAddChild(node, 'VersionInformation');
      xmlAddChild(node, 'FileName').Text := XMLTextEncode(FileName);
      xmlAddChild(node, 'VersionCompanyName').Text := XMLTextEncode(StringValue('CompanyName'));
      xmlAddChild(node, 'VersionProductName').Text := XMLTextEncode(StringValue('ProductName'));
      xmlAddChild(node, 'VersionProductVersion').Text := XMLTextEncode(StringValue('ProductVersion'));
      xmlAddChild(node, 'VersionFileDescription').Text := XMLTextEncode(StringValue('FileDescription'));
    finally
      Free;
    end;
  end;
end;

function TempFileName(ext: string = ''): string;
var
  path, filename: array[0..260] of char;
begin
  GetTempPath(260, path);
  GetTempFileName(path, 'tkd', 0, filename);
  if ext <> '' then
  begin
    DeleteFile(filename);
    Result := ChangeFileExt(filename, ext);
  end
  else
    Result := filename;
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
      end;
      mm.Free(idl);
    end;
    mm._Release;
  end;

  if (Result = '') and (csidl = CSIDL_PROGRAM_FILES) then
    with TRegistryErrorControlled.Create do  // I2890
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if not OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion') then  // I2890
        RaiseLastRegistryError;
      Result := ReadString('ProgramFilesDir');
    finally
      Free;
    end;
  if Result <> '' then
    if Result[Length(Result)] <> '\' then Result := Result + '\';
end;

function GetTarget(const LinkFileName:String):String;
var
   psl  : IShellLink;
   ppf  : IPersistFile;
   Info      : Array[0..MAX_PATH] of Char;
   wfs       : TWin32FindData;
begin
 if UpperCase(ExtractFileExt(LinkFileName)) <> '.LNK' Then
 begin
   Result:='';
   Exit;
 end;

 CoCreateInstance(CLSID_ShellLink,
                  nil,
                  CLSCTX_INPROC_SERVER,
                  IShellLink,
                  psl);
 if psl.QueryInterface(IPersistFile, ppf) = 0 then
 begin
   ppf.Load(PWideChar(LinkFileName), STGM_READ);  // I3310
   psl.GetPath(@info,
               MAX_PATH,
               wfs,
               SLGP_UNCPRIORITY);
   Result := info;
 end
 else
   Result := '';
end;

function StrToBoolDef(const str: string; res: Boolean): Boolean;
begin
  if UpperCase(str) = 'TRUE' then Result := True
  else if StrToIntDef(str, 0) <> 0 then Result := True
  else if (str = 'y') or (str = 'Y') then Result := True
  else if UpperCase(str) = 'FALSE' then Result := False
  else if StrToIntDef(str,-1) = 0 then Result := False
  else if (str = 'n') or (str = 'N') then Result := False
  else Result := res;
end;

function VarToStrDef(v: Variant; def: string): string;
begin
  try
    Result := VarToStr(v);
  except
    Result := def;
  end;
end;

procedure AddFilePEInfo(node: IXMLDOMNode; const FileName: string);
begin
  try
    with TFileVersionInfo.Create(FileName) do
    try
      xmlSetAttribute(node, 'VersionCompanyName', XMLTextEncode(StringValue('CompanyName')));
      xmlSetAttribute(node, 'VersionProductName', XMLTextEncode(StringValue('ProductName')));
      xmlSetAttribute(node, 'VersionProductVersion', XMLTextEncode(StringValue('ProductVersion')));
      xmlSetAttribute(node, 'VersionFileDescription', XMLTextEncode(StringValue('FileDescription')));
    finally
      Free;
    end;
  except
    on E:Exception do
    begin
      try
        xmlSetAttribute(node, 'Error', E.ClassName+': '+E.Message);
      except
        ;
      end;
    end;
  end;
end;

procedure AddFileKMXInfo(node: IXMLDOMNode; const FileName: string);
var
  ki: TKeyboardInfo;
begin
  try
    GetKeyboardInfo(FileName, False, ki, False);
    xmlSetAttribute(node, 'KeyboardName', ki.KeyboardName);
    xmlSetAttribute(node, 'Copyright', ki.CopyrightString);
    xmlSetAttribute(node, 'Message', ki.MessageString);
  except
    on E:Exception do
    begin
      try
        xmlSetAttribute(node, 'Error', E.ClassName+': '+E.Message);
      except
        ;
      end;
    end;
  end;
end;

procedure AddFileTTFInfo(node: IXMLDOMNode; const FileName: string);
begin
  try
    with TTTInfo.Create(FileName, [tfNames]) do
    try
      xmlSetAttribute(node, 'FamilyName', FamilyName);
      xmlSetAttribute(node, 'Copyright', Copyright);
      xmlSetAttribute(node, 'Style', Style);
      xmlSetAttribute(node, 'FullName', FullName);
      xmlSetAttribute(node, 'FontVersion', FontVersion);
     finally
      Free;
    end;
  except
    on E:Exception do
    begin
      try
        xmlSetAttribute(node, 'Error', E.ClassName+': '+E.Message);
      except
        ;
      end;
    end;
  end;
end;

procedure AddFiles(node: IXMLDOMNode; csidl: Integer; const PathName: string);
var
  s: string;
  f: TSearchRec;
  subnode: IXMLDOMNode;
  IsDir: Boolean;
  ext: string;
begin
  if csidl <> 0 then
  begin
    s := GetFolderPath(csidl);
    if s = '' then
    begin
      xmlSetAttribute(node, 'Invalid', 'True');
      Exit;
    end;

    s := s + PathName + '\';
  end
  else
    s := PathName + '\';

  if FindFirst(s+'*', faAnyFile, f) = 0 then
  begin
    repeat
      IsDir := (f.Attr and faDirectory) = faDirectory;

      if IsDir and ((f.Name = '.') or (f.Name = '..')) then Continue;
      
      if IsDir
        then subnode := xmlAddChild(node, 'Directory')
        else subnode := xmlAddChild(node, 'File');
      xmlSetAttribute(subnode, 'Name', f.Name);
      xmlSetAttribute(subnode, 'Date', FormatDateTime('yyyy-mm-dd hh:nn:ss', f.TimeStamp));  // I3310
      if IsDir then
         AddFiles(subnode, 0, s + f.Name)
      else
      begin
        xmlSetAttribute(subnode, 'Size', IntToStr(f.Size));
        ext := ExtractFileExt(f.Name);
        if SameText(ext, '.kmx') or SameText(ext, '.kxx') then
          AddFileKMXInfo(subnode, s+f.Name)
        else if SameText(ext, '.dll') or SameText(ext, '.exe') or SameText(ext, '.kma') then
          AddFilePEInfo(subnode, s+f.Name)
        else if SameText(ext, '.ttf') or SameText(ext, '.otf') then
          AddFileTTFInfo(subnode, s+f.Name);
      end;
    until FindNext(f) <> 0;
    FindClose(f);
  end;
end;

function FileSizeKB(sz: Int64): string;
begin
  if sz < 1024 then
    Result := IntToStr(sz) + ' bytes'
  else //if sz < 1024 * 1024 then
    Result := IntToStr(sz div 1024) + ' KB'
//  else
  //  Result := IntToStr(sz div 1024 div 1024) + ' MB';
end;

function Wow64Process: Boolean;  // I2919 // I3180   // I3542
type
  TWow64Process = function(hProcess: THandle; var bWow64: BOOL): BOOL; stdcall;
var
  FWow64Process: TWow64Process;
  v: BOOL;
begin
  Result := False;
  FWow64Process := GetProcAddress(GetModuleHandle('kernel32'), 'IsWow64Process');
  if Assigned(FWow64Process) then
  begin
    if FWow64Process(GetCurrentProcess, v) then
      Result := v;
  end;
end;


end.
