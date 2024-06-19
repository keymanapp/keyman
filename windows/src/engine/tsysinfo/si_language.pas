(*
  Name:             si_language
  Copyright:        Copyright (C) SIL International.
  Documentation:    km4.1
  Description:      Get language environment details from system
  Create Date:      13 May 2005

  Modified Date:    15 Apr 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          13 May 2005 - mcdurdin - Integrated into kmshell from tsysinfo
                    09 Jun 2005 - mcdurdin - Use MSXML_TLB not MSXML2_TLB
                    20 Jul 2008 - mcdurdin - I1532 - Report on additional items in tsysinfo
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    19 Jul 2011 - mcdurdin - I3000 - Tweak display of diagnostics using .xslt files
                    15 Apr 2015 - mcdurdin - I4659 - V9.0 - Add more detailed keyboard diagnostics
*)
unit si_language;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, si_base, StdCtrls, msxml;

type
  TGetLocalInfoEx = function(lpLocaleName: PWideChar; LCType: DWORD; lpLCData: PWideChar; cchData: Integer): Integer; stdcall;

  TSI_Language = class(TSI_Base)
  private
    FLocaleNode: IXMLDOMNode;
    FGetLocaleInfoEx: TGetLocalInfoEx;
    procedure AddLocale(localeName: WideString; dwFlags: DWORD);
    procedure AddLocaleEx(localeName: WideString; dwFlags: DWORD);
  protected

    function DoCollect: Boolean; override;
    class function GetCaption: String; override;
    function GetUseGlobalData: Boolean; override;
  end;

implementation

uses ErrorControlledRegistry, RegistryKeys, WideStrings, sysinfo_Util, ComObj, ShlObj;

{ TSI_Language }

class function TSI_Language.GetCaption: String;
begin
  Result := 'Language Environment';
end;

function TSI_Language.GetUseGlobalData: Boolean;
begin
  Result := True;
end;

var
  FSILanguage: TSI_Language = nil;

const
  LOCALE_WINDOWS = 1;
  LOCALE_SUPPLEMENTAL = 2;

function LocaleEnumProc(const localeName: PWideChar): DWORD; stdcall;
begin
  FSILanguage.AddLocale(localeName, LOCALE_WINDOWS);
  Result := 1;
end;

function LocaleEnumProcEx(const localeName: PWideChar; dwFlags: DWord; lParam: LPARAM): DWORD; stdcall;
begin
  TSI_Language(lParam).AddLocaleEx(localeName, dwFlags);
  Result := 1;
end;

procedure TSI_Language.AddLocale(localeName: WideString; dwFlags: DWORD);
var
  node: IXMLDOMNode;
  buf: array[0..255] of WideChar;
begin
  node := xmlAddChild(FLocaleNode,'Locale');
  xmlSetAttribute(node, 'ID', localeName);
  xmlSetAttribute(node, 'Flags', IntToHex(dwFlags,8));
  if GetLocaleInfoW(StrToIntDef('$'+localeName, 8), LOCALE_SENGLANGUAGE, buf, 255) > 0 then
    xmlSetAttribute(node, 'Name', buf);
  if GetLocaleInfoW(StrToIntDef('$'+localeName, 8), LOCALE_SENGCOUNTRY, buf, 255) > 0 then
    xmlSetAttribute(node, 'Country', buf);
end;

procedure TSI_Language.AddLocaleEx(localeName: WideString; dwFlags: DWORD);
var
  node: IXMLDOMNode;
  buf: array[0..255] of WideChar;
begin
  node := xmlAddChild(FLocaleNode,'Locale');
  if FGetLocaleInfoEx(PWideChar(localeName), LOCALE_ILANGUAGE, buf, 255) > 0 then
    xmlSetAttribute(node, 'ID', '0000'+buf);
  xmlSetAttribute(node, 'StringID', localeName);
  xmlSetAttribute(node, 'Flags', IntToHex(dwFlags,8));
  if FGetLocaleInfoEx(PWideChar(localeName), LOCALE_SENGLANGUAGE, buf, 255) > 0 then
    xmlSetAttribute(node, 'Name', buf);
  if FGetLocaleInfoEx(PWideChar(localeName), LOCALE_SENGCOUNTRY, buf, 255) > 0 then
    xmlSetAttribute(node, 'Country', buf);
end;

function TSI_Language.DoCollect: Boolean;
type
  TLocale_EnumProcExW = function(const lpLocaleString: PWideChar; dwFlags: DWord; lParam: LPARAM): DWORD; stdcall;
  TEnumSystemLocalesExW = function(lpLocaleEnumProcEx: TLocale_EnumProcExW; dwFlags: DWORD; lParam: LPARAM; lpReserved: Pointer): BOOL; stdcall;
const
  CSIDL_SYSTEM = $25;
var
  regnode, subnode, node: IXMLDOMNode;
  str: TStringList;
  I: Integer;
  FEnumSystemLocalesEx: TEnumSystemLocalesExW;
begin
  node := xmlAddChild(rootnode,'Languages');
  subnode := xmlAddChild(node,'Registry');
  regnode := xmlAddChild(subnode,'LocalMachine');
  AddRegistry(regnode, HKEY_LOCAL_MACHINE, SRegKey_KeyboardLayouts_LM);
  AddRegistry(regnode, HKEY_LOCAL_MACHINE, 'Software\Microsoft\CTF');
  AddRegistry(regnode, HKEY_LOCAL_MACHINE, 'System\CurrentControlSet\Control\Nls');

  FLocaleNode := xmlAddChild(rootnode, 'Locales');
  FEnumSystemLocalesEx := GetProcAddress(GetModuleHandle(Kernel32), 'EnumSystemLocalesEx');
  FGetLocaleInfoEx := GetProcAddress(GetModuleHandle(Kernel32), 'GetLocaleInfoEx');
  if Assigned(FEnumSystemLocalesEx) and Assigned(FGetLocaleInfoEx) then
  begin
    FEnumSystemLocalesEx(LocaleEnumProcEx, LOCALE_WINDOWS or LOCALE_SUPPLEMENTAL, LPARAM(Self), nil);
  end
  else
  begin
    FSILanguage := Self;
    EnumSystemLocalesW(@LocaleEnumProc, LCID_SUPPORTED);
    FSILanguage := nil;
  end;
  FLocaleNode := nil;

  regnode := xmlAddChild(subnode,'CurrentUser');
  AddRegistry(regnode, HKEY_CURRENT_USER, SRegKey_KeyboardLayout_CU);
  AddRegistry(regnode, HKEY_CURRENT_USER, 'Software\Microsoft\CTF');
  AddRegistry(regnode, HKEY_CURRENT_USER, SRegKey_ControlPanelInternationalUserProfile);
  subnode := xmlAddChild(node,'Uniscribe');
  AddFileVersion(subnode, GetFolderPath(CSIDL_SYSTEM) + 'usp10.dll');
  str := TStringList.Create;
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion\SharedDlls') then
    begin
      GetValueNames(str);
      for I := 0 to str.Count - 1 do
      begin
        if SameText(Copy(str[i], Length(str[i])-8, 9), 'usp10.dll') then
          AddFileVersion(subnode, str[i]);
      end;
    end;
  finally
    Free;
    str.Free;
  end;
  Result := True;
end;

initialization
  TSI_Language.Register;
end.
