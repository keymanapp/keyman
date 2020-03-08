(*
  Name:             si_startup
  Copyright:        Copyright (C) SIL International.
  Documentation:    km4.1
  Description:      get information about all apps that start with Windows
  Create Date:      13 May 2005

  Modified Date:    19 Jul 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          13 May 2005 - mcdurdin - Integrated into kmshell from tsysinfo
                    09 Jun 2005 - mcdurdin - Use MSXML_TLB not MSXML2_TLB
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    19 Jul 2011 - mcdurdin - I3000 - Tweak display of diagnostics using .xslt files
*)
{STARTUP APPLICATIONS:

HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\RunServicesOnce
HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\RunServicesOnce
HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\RunServices
HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\RunServices

HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\Run
HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Run
HKEY_CURRENT_USER\Software\Microsoft\Windows NT\CurrentVersion\Windows [Run]

HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\RunOnce
HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\RunOnce\Setup
HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\RunOnce
HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\RunOnce\Setup
HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\RunOnceEx
HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\Run
HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\Run
HKEY_LOCAL_MACHINE\Software\Microsoft\Windows NT\CurrentVersion\Winlogon\Userinit
HKEY_CURRENT_USER\Software\Microsoft\Windows NT\CurrentVersion\Windows\load
HKEY_CURRENT_USER\Software\Microsoft\Windows NT\CurrentVersion\Windows [AppInit_Dlls]

[C:\Documents and Settings\All Users\Start Menu\Programs\Startup]
[C:\Documents and Settings\Your user name\Start Menu\Programs\Startup]

[win.ini] [Windows] load=, run=
}

unit si_startup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, si_base, StdCtrls, ErrorControlledRegistry, inifiles, msxml;

type
  TSI_Startup = class(TSI_Base)
  private
    procedure AddStartup(node: IXMLDOMNode; appnum: Integer; name, value: string);
    procedure AddStartups(node: IXMLDOMNode; s: string);
    procedure RecordValue(rootnode: IXMLDOMNode; key: HKEY; path, value: string);
    procedure RecordValues(rootnode: IXMLDOMNode; key: HKEY; path: string);
    procedure RecordTargets(node: IXMLDOMNode; const Desc, Path: string);
  protected
    function DoCollect: Boolean; override;
    class function GetCaption: String; override;
  end;

implementation

uses fileversioninfo, sysinfo_util, ActiveX, ComObj;

{ TSI_Startup }

class function TSI_Startup.GetCaption: String;
begin
  Result := 'Startup Applications';
end;

procedure TSI_Startup.RecordValues(rootnode: IXMLDOMNode; key: HKEY; path: string);
var
  str: TStringList;
  i: Integer;
  s: string;
  node: IXMLDOMNode;
begin
  str := TStringList.Create;
  if key = HKEY_CURRENT_USER then s := 'HKEY_CURRENT_USER' else s := 'HKEY_LOCAL_MACHINE';
  node := xmlAddChild(rootnode,'Location');
  xmlAddChild(node,'Location').Text := XMLTextEncode(s+path);
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := key;
    if OpenKeyReadOnly(path) then
    begin
      GetValueNames(str);
      for i := 0 to str.Count - 1 do
        try
          AddStartup(node, i, str[i], ReadString(str[i]));
        except
          on E:Exception do
            xmlAddChild(node,str[i]).Text := XMLTextEncode('ERROR READING VALUE: '+E.Message);
        end;
    end;
  finally
    Free;
  end;
end;

procedure TSI_Startup.RecordValue(rootnode: IXMLDOMNode; key: HKEY; path, value: string);
var
  s: string;
  node: IXMLDOMNode;
begin
  if key = HKEY_CURRENT_USER then s := 'HKEY_CURRENT_USER' else s := 'HKEY_LOCAL_MACHINE';
  node := xmlAddChild(rootnode,'Location');
  xmlAddChild(node,'Location').Text := XMLTextEncode(s+path);
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := key;
    if OpenKeyReadOnly(path) and ValueExists(value) then
      AddStartups(node, ReadString(value));
  finally
    Free;
  end;
end;

procedure TSI_Startup.AddStartups(node: IXMLDOMNode; s: string);
var
  i, n: Integer;
begin
  n := Pos(',', s);
  i := 0;
  while n > 0 do
  begin
    AddStartup(node, i, 'App'+IntToStr(i), Trim(Copy(s, 1, n-1)));
    Delete(s,1,n); s := Trim(s);
    n := Pos(',', s);
    Inc(i);
  end;
  if s <> '' then
    AddStartup(node, i, 'App'+IntToStr(i), Trim(s));
end;

procedure TSI_Startup.RecordTargets(node: IXMLDOMNode; const Desc, Path: string);
var
  f: TSearchRec;
  n: Integer;
  s: string;
begin
  n := 0;
  node := xmlAddChild(node, 'Location');
  xmlAddChild(node,'Location').Text := XMLTextEncode(Desc);
  if FindFirst(Path+'*', 0, f) = 0 then
  begin
    repeat
      try
        s := GetTarget(Path+f.Name);
        if s <> '' then
          AddStartup(node, n, 'Link', s)
        else
          AddStartup(node, n, 'App', Path+f.Name);
      except
        AddStartup(node, n, 'AppErr', Path+f.Name);
      end;
      Inc(n);
    until FindNext(f) <> 0;
    FindClose(f);
  end;
end;

function TSI_Startup.DoCollect: Boolean;
var
  s, WinPath: string;
  buf: array[0..260] of char;
  n, node: IXMLDOMNode;
begin
  node := xmlAddChild(rootnode,'Startup');
  RecordValues(node, HKEY_CURRENT_USER, '\Software\Microsoft\Windows\CurrentVersion\RunServicesOnce');
  RecordValues(node, HKEY_CURRENT_USER, '\Software\Microsoft\Windows\CurrentVersion\RunServices');
  RecordValues(node, HKEY_CURRENT_USER, '\Software\Microsoft\Windows\CurrentVersion\Run');
  RecordValues(node, HKEY_CURRENT_USER, '\Software\Microsoft\Windows\CurrentVersion\RunOnce');
  RecordValues(node, HKEY_CURRENT_USER, '\Software\Microsoft\Windows\CurrentVersion\RunOnce\Setup');
  RecordValues(node, HKEY_CURRENT_USER, '\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\Run');

  RecordValues(node, HKEY_LOCAL_MACHINE, '\Software\Microsoft\Windows\CurrentVersion\RunServicesOnce');
  RecordValues(node, HKEY_LOCAL_MACHINE, '\Software\Microsoft\Windows\CurrentVersion\RunServices');
  RecordValues(node, HKEY_LOCAL_MACHINE, '\Software\Microsoft\Windows\CurrentVersion\Run');
  RecordValues(node, HKEY_LOCAL_MACHINE, '\Software\Microsoft\Windows\CurrentVersion\RunOnce');
  RecordValues(node, HKEY_LOCAL_MACHINE, '\Software\Microsoft\Windows\CurrentVersion\RunOnce\Setup');
  RecordValues(node, HKEY_LOCAL_MACHINE, '\Software\Microsoft\Windows\CurrentVersion\RunOnceEx');
  RecordValues(node, HKEY_LOCAL_MACHINE, '\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\Run');

  RecordValue(node, HKEY_LOCAL_MACHINE, '\Software\Microsoft\Windows NT\CurrentVersion\Winlogon', 'Userinit');
  RecordValue(node, HKEY_CURRENT_USER, '\Software\Microsoft\Windows NT\CurrentVersion\Windows', 'Run');
  RecordValue(node, HKEY_CURRENT_USER, '\Software\Microsoft\Windows NT\CurrentVersion\Windows', 'load');
  RecordValue(node, HKEY_LOCAL_MACHINE, '\Software\Microsoft\Windows NT\CurrentVersion\Windows', 'AppInit_Dlls');

  GetWindowsDirectory(buf, 260);
  WinPath := IncludeTrailingPathDelimiter(buf);
  with TIniFile.Create(WinPath + 'win.ini') do
  try
    s := Trim(ReadString('Windows', 'load', ''));
    if s <> '' then
    begin
      n := xmlAddChild(node,'Location');
      xmlAddChild(n,'Location').Text := 'win.ini-load';
      AddStartups(n, s);
    end;
    s := Trim(ReadString('Windows', 'run', ''));
    if s <> '' then
    begin
      n := xmlAddChild(node,'Location');
      xmlAddChild(n,'Location').Text := 'win.ini-run';
      AddStartups(n, s);
    end;
  finally
    Free;
  end;

  RecordTargets(node, 'UserStartup', GetFolderPath($7));
  RecordTargets(node, 'CommonStartup', GetFolderPath($18));

  Result := True;

//[C:\Documents and Settings\All Users\Start Menu\Programs\Startup]
//[C:\Documents and Settings\Your user name\Start Menu\Programs\Startup]
end;

procedure TSI_Startup.AddStartup(node: IXMLDOMNode; appnum: Integer; name, value: string);
var
  fvi: TFileVersionInfo;
    function GetAppFromCmd(s: string): string;
    var
      n: Integer;
    begin
      Result := '';
      if s = '' then Exit;
      if s[1] = '"' then
      begin
        Delete(s,1,1);
        n := Pos('"', s);
        if n = 0 then Result := s
        else Result := Copy(s, 1, n-1);
        Exit;
      end;

      repeat
        n := Pos(' ', s);
        if Result <> '' then Result := Result + ' ';
        if n = 0 then Result := Result + s
        else Result := Result + Copy(s, 1, n-1);
        if n > 0 then Delete(s, 1, n);
      until FileExists(Result) or (s='') or (n=0);
    end;
begin
  node := xmlAddChild(node,'App'+IntToStr(appnum));
  xmlAddChild(node,'Name').Text := XMLTextEncode(name);
  xmlAddChild(node,'FileName').Text := XMLTextEncode(value);
  try
    fvi := TFileVersionInfo.Create(GetAppFromCmd(value));
    try
      xmlAddChild(node,'VersionCompanyName').Text := XMLTextEncode(fvi.StringValue('CompanyName'));
      xmlAddChild(node,'VersionProductName').Text := XMLTextEncode(fvi.StringValue('ProductName'));
      xmlAddChild(node,'VersionProductVersion').Text := XMLTextEncode(fvi.StringValue('ProductVersion'));
      xmlAddChild(node,'VersionFileDescription').Text := XMLTextEncode(fvi.StringValue('FileDescription'));
    finally
      fvi.Free;
    end;
  except
    ; // ignore errors
  end;
end;

initialization
  TSI_Startup.Register;
end.
