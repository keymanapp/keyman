(*
  Name:             si_hookdlls
  Copyright:        Copyright (C) SIL International.
  Documentation:    km4.1
  Description:      check all dlls in memory for hook procs (heuristic)
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
                    19 Jul 2011 - mcdurdin - I3000 - Tweak display of diagnostics using .xslt files
*)
unit si_hookdlls;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, si_base, StdCtrls;

type
  TSI_HookDLLs = class(TSI_Base)
  protected
    function DoCollect: Boolean; override;
    class function GetCaption: String; override;
  end;

implementation

uses fileversioninfo, udumpfile, psapi, sysinfo_util, msxml, ComObj,
  sysinfo_main;

function BoolAsStr(b: Boolean): string;
begin
  if b then Result := 'Y' else Result := '';
end;

{ TSI_HookDLLs }

class function TSI_HookDLLs.GetCaption: String;
begin
  Result := 'Hook DLLs';
end;

function TSI_HookDLLs.DoCollect: Boolean;
var
  df: TDumpFile;
  hProcess: THandle;
  buf: array[0..260] of char;
  dw: DWord;
  hMods: array[0..1023] of THandle;
  str: TStringList;
  i: Integer;
  fvi: TFileVersionInfo;
  FSuspicious: Boolean;
  hooksnode, node: IXMLDOMNode;
begin
  hProcess := GetCurrentProcess;
  str := TStringList.Create;
  try
    if EnumProcessModules(hProcess, @hMods, sizeof(hMods), dw) then
      for i := 0 to dw div sizeof(THandle) - 1 do
        if GetModuleFileNameEx(hProcess, hMods[i], buf, 260) <> 0 then
          str.Add(buf);

    hooksnode := xmlAddchild(rootnode,'HookDLLs');

    for i := 0 to str.Count - 1 do
    begin
      node := xmlAddChild(hooksnode,'DLL');
      xmlSetAttribute(node,'ID',IntToStr(i));
      df := TDumpFile.Create(str[i]);
      fvi := TFileVersionInfo.Create(str[i]);
      try
        //vdf.Dump;
        FSuspicious := True;

        if AnsiCompareText(str[i], PAramStr(0)) = 0 then FSuspicious := False;
        if (df.Imports.IndexOf('user32.dll', 'SetWindowsHookExA') < 0) and
           (df.Imports.IndexOf('user32.dll', 'SetWindowsHookExW') < 0) then FSuspicious := False;
        if (fvi.StringValue('CompanyName') = 'Tavultesoft') or
           (fvi.StringValue('CompanyName') = 'Tavultesoft Pty Ltd') or
           (fvi.StringValue('CompanyName') = 'SIL International') or
           (fvi.StringValue('CompanyName') = 'Microsoft Corporation') then FSuspicious := False;

        xmlAddChild(node,'Suspicious').Text := BoolAsStr(FSuspicious);

        xmlAddChild(node,'FileName').Text := XMLTextEncode(ExtractFileName(str[i]));
        xmlAddChild(node,'Folder').Text := XMLTextEncode(ExtractFileDir(str[i]));

        xmlAddChild(node,'ImportsSetWindowsHookExA').Text := BoolAsStr(df.Imports.IndexOf('user32.dll', 'SetWindowsHookExA') >= 0);
        xmlAddChild(node,'ImportsSetWindowsHookExW').Text := BoolAsStr(df.Imports.IndexOf('user32.dll', 'SetWindowsHookExW') >= 0);
        xmlAddChild(node,'ImportsCallNextHookEx').Text := BoolAsStr(df.Imports.IndexOf('user32.dll', 'CallNextHookEx') >= 0);
        xmlAddChild(node,'ImportsUnhookWindowsHookEx').Text := BoolAsStr(df.Imports.IndexOf('user32.dll', 'UnhookWindowsHookEx') >= 0);

        xmlAddChild(node,'VersionCompanyName').Text := XMLTextEncode(fvi.StringValue('CompanyName'));
        xmlAddChild(node,'VersionProductName').Text := XMLTextEncode(fvi.StringValue('ProductName'));
        xmlAddChild(node,'VersionProductVersion').Text := XMLTextEncode(fvi.StringValue('ProductVersion'));
        xmlAddChild(node,'VersionFileDescription').Text := XMLTextEncode(fvi.StringValue('FileDescription'));
      finally
        df.Free;
        fvi.Free;
      end;
    end;
  finally
    str.Free;
  end;
  Result := True;
end;

initialization
  TSI_HookDLLs.Register;
end.
