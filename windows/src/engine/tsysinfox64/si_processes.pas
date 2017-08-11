(*
  Name:             si_processes
  Copyright:        Copyright (C) SIL International.
  Documentation:    km4.1
  Description:      get information on all loaded processes and modules
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
unit si_processes;

interface

uses
  WinApi.Windows, System.SysUtils, System.Classes, si_base;

type
  TSI_Processes = class(TSI_Base)
  protected
    function DoCollect: Boolean; override;
  end;

implementation

uses fileversioninfo, WinApi.msxml, WinApi.psapi, sysinfo_util;

{ TSI_Processes }

function EnumProcessModulesEx(
  hProcess: THandle;
  lphModule: PDWord;
  cb: DWord;
  var lpcbNeeded: DWord;
  dwFilterFlag: DWord
): BOOL; stdcall; external 'psapi.dll';

const
  LIST_MODULES_ALL = 3;


function TSI_Processes.DoCollect: Boolean;
var
  hProcess: THandle;
  buf: array[0..260] of char;
  cbNeeded, dw: DWord;
  aProcesses: array[0..1024] of DWord;
  hMods: array[0..1023] of THandle;
  smods, str: TStringList;
  n, i, j: Integer;
  fvi: TFileVersionInfo;
  modnode, procnode, node: IXMLDOMNode;
begin
  Result := False;
  procnode := xmlAddChild(rootnode,'Processes');

  str := TStringList.Create;
  try
    if not EnumProcesses(@aProcesses, sizeof(aProcesses), cbNeeded) then Exit;
    for i := 0 to cbNeeded div sizeof(DWord) - 1 do
    begin
      hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, aProcesses[i]);
      if hProcess <> 0 then
      begin
        if EnumProcessModulesEx(hProcess, PDWord(@hMods[0]), sizeof(hMods), dw, LIST_MODULES_ALL) then
        begin
          GetModuleFileNameEx(hProcess, hMods[0], buf, 260);
          n := str.Add(buf);
          smods := TStringList.Create;
          for j := 1 to dw div sizeof(dword) - 1 do
          begin
            GetModuleFileNameEx(hProcess, hMods[j], buf, 260);
            if smods.IndexOf(buf) < 0 then
              smods.Add(buf);
          end;
          str.Objects[n] := smods;
        end;
        CloseHandle(hProcess);
      end;
    end;

    for i := 0 to str.Count - 1 do
    begin
      node := xmlAddChild(procnode,'Process');
      xmlSetAttribute(node, 'ID', IntToStr(i));
      fvi := TFileVersionInfo.Create(str[i]);
      try
        xmlAddChild(node,'FileName').Text := XMLTextEncode(ExtractFileName(str[i]));
        xmlAddChild(node,'Folder').Text := XMLTextEncode(ExtractFileDir(str[i]));

        xmlAddChild(node,'VersionCompanyName').Text := XMLTextEncode(fvi.StringValue('CompanyName'));
        xmlAddChild(node,'VersionProductName').Text := XMLTextEncode(fvi.StringValue('ProductName'));
        xmlAddChild(node,'VersionProductVersion').Text := XMLTextEncode(fvi.StringValue('ProductVersion'));
        xmlAddChild(node,'VersionFileDescription').Text := XMLTextEncode(fvi.StringValue('FileDescription'));
      finally
        fvi.Free;
      end;
      smods := str.Objects[i] as TStringList;
      for j := 0 to smods.Count - 1 do
      begin
        modnode := xmlAddChild(node,'Module');
        xmlSetAttribute(modnode,'ID', IntToStr(j));
        fvi := TFileVersionInfo.Create(smods[j]);
        try
          xmlAddChild(modnode,'FileName').Text := XMLTextEncode(ExtractFileName(smods[j]));
          xmlAddChild(modnode,'Folder').Text := XMLTextEncode(ExtractFileDir(smods[j]));

          xmlAddChild(modnode,'VersionCompanyName').Text := XMLTextEncode(fvi.StringValue('CompanyName'));
          xmlAddChild(modnode,'VersionProductName').Text := XMLTextEncode(fvi.StringValue('ProductName'));
          xmlAddChild(modnode,'VersionProductVersion').Text := XMLTextEncode(fvi.StringValue('ProductVersion'));
          xmlAddChild(modnode,'VersionFileDescription').Text := XMLTextEncode(fvi.StringValue('FileDescription'));
        finally
          fvi.Free;
        end;
      end;
    end;
  finally
    str.Free;
  end;
  Result := True;
end;

initialization
  TSI_Processes.Register;
end.
