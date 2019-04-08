(*
  Name:             si_keyman
  Copyright:        Copyright (C) SIL International.
  Documentation:    km4.1
  Description:      Get keyman specific details from system
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
                    20 Jul 2008 - mcdurdin - I1532 - Report files in tsysinfo
                    16 Jan 2009 - mcdurdin - Use a TE-mbeddedWB so copy and paste works
                    18 Feb 2011 - mcdurdin - I2721 - Override Javascript-disabled security for web controls
                    19 Jul 2011 - mcdurdin - I3000 - Tweak display of diagnostics using .xslt files
                    26 Jun 2012 - mcdurdin - I3379 - KM9 - Remove old Winapi references now in Delphi libraries
                    17 Jan 2013 - mcdurdin - I3765 - V9.0 - Diagnostics should report on installed language profiles
                    23 Jan 2013 - mcdurdin - I3772 - V9.0 - Diagnostics on installed profiles should report TIP filename in place of clsid, keyboard name and description, and not show 0000... strings.
                    15 Apr 2015 - mcdurdin - I4659 - V9.0 - Add more detailed keyboard diagnostics
*)
unit si_keyman;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, si_base, msxml;

type
  TSI_Keyman = class(TSI_Base)
  private
    function GetClassIDName(clsid: TGUID): string;   // I3772
    function GetClassIDFileName(clsid: TGUID): string;   // I3772
  protected
    function GetUseGlobalData: Boolean; override;
    function DoCollect: Boolean; override;
    class function GetCaption: String; override;
  end;

implementation

uses
  ActiveX,
  Registry,
  RegistryKeys, ComObj, sysinfo_Util, ShellApi, ShlObj,
  keyman_msctf,
  Winapi.MsCTF;

{ TSI_Keyman }

class function TSI_Keyman.GetCaption: String;
begin
  Result := 'Keyman';
end;

function TSI_Keyman.GetClassIDName(clsid: TGUID): string;   // I3772
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_CLASSES_ROOT;
    if OpenKeyReadOnly('CLSID\'+GUIDToString(clsid))
      then Result := ReadString('')
      else Result := '';
  finally
    Free;
  end;
end;

function TSI_Keyman.GetUseGlobalData: Boolean;
begin
  Result := True;
end;

function TSI_Keyman.GetClassIDFileName(clsid: TGUID): string;   // I3772
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_CLASSES_ROOT;
    if OpenKeyReadOnly('CLSID\'+GUIDToString(clsid)+'\InProcServer32')
      then Result := ReadString('')
      else Result := '';
  finally
    Free;
  end;
end;

function TSI_Keyman.DoCollect: Boolean;
var
  node2, regnode, subnode, node: IXMLDOMNode;
  pInputProcessorProfiles: ITfInputProcessorProfiles;
  pInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
  ulCount: Cardinal;
  pLangIDs: PWord;
  I: Integer;
  pLangID: PWord;
  nFetched: Cardinal;
  pEnumIPPs: IEnumTfInputProcessorProfiles;
  FProfile: TF_INPUTPROCESSORPROFILE;
  FDescription: WideString;
  FProfile2: TF_INPUTPROCESSORPROFILE;
  FClassFileName: string;   // I3772
begin
  node := xmlAddChild(rootnode,'Keyman');

  subnode := xmlAddChild(node,'Registry');
  regnode := xmlAddChild(subnode,'LocalMachine');
  AddRegistry(regnode, HKEY_LOCAL_MACHINE, SRegKey_KeymanRoot_LM);
  regnode := xmlAddChild(subnode,'CurrentUser');
  AddRegistry(regnode, HKEY_CURRENT_USER, SRegKey_KeymanRoot_CU);

  { Get file versions }

  subnode := xmlAddChild(node,'Files');
  regnode := xmlAddChild(subnode,'Program-Files-Keyman');
  AddFiles(regnode, CSIDL_PROGRAM_FILES, SFolderKeymanRoot);
  regnode := xmlAddChild(subnode,'Common-Files-Keyman');
  AddFiles(regnode, CSIDL_PROGRAM_FILES_COMMON, SFolderKeymanRoot);
  regnode := xmlAddChild(subnode,'AppData-Files-Keyman');
  AddFiles(regnode, CSIDL_APPDATA, SFolderKeymanRoot);
  regnode := xmlAddChild(subnode,'Local-AppData-Files-Keyman');
  AddFiles(regnode, CSIDL_LOCAL_APPDATA, SFolderKeymanRoot);
  regnode := xmlAddChild(subnode,'Common-AppData-Files-Keyman');
  AddFiles(regnode, CSIDL_COMMON_APPDATA, SFolderKeymanRoot);

  { Get TSF installed TIPs }

  subnode := xmlAddchild(node,'LanguageProfiles');   // I3765

  try
    OleCheck(CoCreateInstance(CLASS_TF_InputProcessorProfiles, nil, CLSCTX_INPROC_SERVER,
                            IID_ITfInputProcessorProfiles, pInputProcessorProfiles));
    OleCheck(pInputProcessorProfiles.QueryInterface(IID_ITfInputProcessorProfileMgr, pInputProcessorProfileMgr));

    OleCheck(pInputProcessorProfiles.GetLanguageList(@pLangIDs, ulCount));
    pLangID := pLangIDs;
    for I := 0 to ulCount - 1 do
    begin

      OleCheck(pInputProcessorProfileMgr.EnumProfiles(pLangID^, pEnumIPPs));
      while pEnumIPPs.Next(1, FProfile, nFetched) = S_OK do
      begin
        pInputProcessorProfileMgr.GetProfile(FProfile.dwProfileType,
          FProfile.langid, FProfile.clsid, FProfile.guidProfile,
          FProfile.HKL, FProfile2);
        regnode := xmlAddChild(subnode,'LanguageProfile');
        case FProfile.dwProfileType of
          TF_PROFILETYPE_INPUTPROCESSOR:  xmlSetAttribute(regnode,'Type', 'Input Processor');
          TF_PROFILETYPE_KEYBOARDLAYOUT: xmlSetAttribute(regnode,'Type', 'Keyboard Layout');
          else xmlSetAttribute(regnode,'Type', IntToStr(FProfile.dwProfileType));
        end;
        xmlSetAttribute(regnode,'LangID', IntToHex(FProfile.langid, 4));
        xmlSetAttribute(regnode,'ClassID', GUIDToString(FProfile.clsid));
        xmlSetAttribute(regnode,'ClassName', GetClassIDName(FProfile.clsid));   // I3772
        FClassFileName := GetClassIDFileName(FProfile.clsid);   // I3772
        xmlSetAttribute(regnode,'ClassFileName', ExtractFileName(FClassFileName));   // I3772
        xmlSetAttribute(regnode,'ClassFilePath', ExtractFilePath(FClassFileName));   // I3772
        xmlSetAttribute(regnode,'ProfileGUID', GUIDToString(FProfile.guidProfile));

        if pInputProcessorProfiles.GetLanguageProfileDescription(FProfile.clsid, FProfile.langid, FProfile.guidProfile, FDescription) = S_OK then
          xmlSetAttribute(regnode,'Description', FDescription);

        if IsEqualGUID(FProfile.catid, GUID_TFCAT_TIP_KEYBOARD) then
          xmlSetAttribute(regnode,'Category', 'Keyboard TIP')
        else if IsEqualGUID(FProfile.catid, GUID_TFCAT_TIP_SPEECH) then
          xmlSetAttribute(regnode,'Category', 'Speech TIP')
        else if IsEqualGUID(FProfile.catid, GUID_TFCAT_TIP_HANDWRITING) then
          xmlSetAttribute(regnode,'Category', 'Handwriting TIP')
        else
          xmlSetAttribute(regnode,'Category', GuidToString(FProfile.catid));
        xmlSetAttribute(regnode,'SubstituteHKL', IntToHex(FProfile.hklSubstitute, 8));
        node2 := xmlAddChild(regnode, 'Capabilities');
        if (FProfile.dwCaps and TF_IPP_CAPS_DISABLEONTRANSITORY) = TF_IPP_CAPS_DISABLEONTRANSITORY then
          xmlAddChild(node2, 'DisableOnTransitory');
        if (FProfile.dwCaps and TF_IPP_CAPS_SECUREMODESUPPORT) = TF_IPP_CAPS_SECUREMODESUPPORT then
          xmlAddChild(node2, 'SecureModeSupport');
        if (FProfile.dwCaps and TF_IPP_CAPS_UIELEMENTENABLED) = TF_IPP_CAPS_UIELEMENTENABLED then
          xmlAddChild(node2, 'UIElementEnabled');
        if (FProfile.dwCaps and TF_IPP_CAPS_COMLESSSUPPORT) = TF_IPP_CAPS_COMLESSSUPPORT then
          xmlAddChild(node2, 'COMLessSupport');
        if (FProfile.dwCaps and TF_IPP_CAPS_WOW16SUPPORT) = TF_IPP_CAPS_WOW16SUPPORT then
          xmlAddChild(node2, 'WOW16Support');
        if (FProfile.dwCaps and TF_IPP_CAPS_IMMERSIVESUPPORT) = TF_IPP_CAPS_IMMERSIVESUPPORT then
          xmlAddChild(node2, 'ImmersiveSupport');
        if (FProfile.dwCaps and TF_IPP_CAPS_SYSTRAYSUPPORT) = TF_IPP_CAPS_SYSTRAYSUPPORT then
          xmlAddChild(node2, 'SysTraySupport');
        xmlSetAttribute(regnode,'HKL', IntToHex(FProfile.HKL, 8));

        if (FProfile.dwFlags and TF_IPP_FLAG_ACTIVE) = TF_IPP_FLAG_ACTIVE then
          xmlSetAttribute(regnode,'IsActive', 'True');
        if (FProfile.dwFlags and TF_IPP_FLAG_Enabled) = TF_IPP_FLAG_ENABLED then
          xmlSetAttribute(regnode,'IsEnabled', 'True');
        if (FProfile.dwFlags and TF_IPP_FLAG_SUBSTITUTEDBYINPUTPROCESSOR) = TF_IPP_FLAG_SUBSTITUTEDBYINPUTPROCESSOR then
          xmlSetAttribute(regnode,'IsSubstituted', 'True');
      end;
      Inc(pLangID);
    end;
    CoTaskMemFree(pLangIDs);

    {OleCheck(pInputProcessorProfiles.EnumInputProcessorInfo(pEnum));
    while pEnum.RemoteNext(1, FGUID, nFetched) <> S_FALSE do
    begin

    end;}
  except
    on E:Exception do
      xmlSetAttribute(subnode, 'error', E.Message);
  end;
  Result := True;
end;

initialization
  TSI_Keyman.Register;
end.
