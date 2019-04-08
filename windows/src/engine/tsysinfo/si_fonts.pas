(*
  Name:             si_keyman
  Copyright:        Copyright (C) SIL International.
  Documentation:    km4.1
  Description:      Get keyman specific details from system
  Create Date:      13 May 2005

  Modified Date:    19 Jul 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          10 May 2010 - mcdurdin - I2042 - Initial version (from si_keyman)
                    18 Feb 2011 - mcdurdin - I2721 - Override Javascript-disabled security for web controls
                    19 Jul 2011 - mcdurdin - I3000 - Tweak display of diagnostics using .xslt files
*)
unit si_fonts;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, si_base, StdCtrls, msxml, Contnrs;

type
  TSI_Fonts = class(TSI_Base)
  protected
    function DoCollect: Boolean; override;
    class function GetCaption: String; override;
  end;

implementation

uses RegistryKeys, ComObj, sysinfo_Util, ShellApi, ShlObj;

{ TSI_Keyman }

class function TSI_Fonts.GetCaption: String;
begin
  Result := 'Fonts';
end;

type
  PSCRIPT_CACHE = ^SCRIPT_CACHE;
  SCRIPT_CACHE = Pointer;

  OPENTYPE_TAG = DWORD;
  POPENTYPE_TAG = ^OPENTYPE_TAG;

function ScriptGetFontScriptTags(hdc: HDC; var psc: SCRIPT_CACHE; psa: Pointer; cMaxTags: Integer; tags: POPENTYPE_TAG; var pcTags: Integer): HRESULT; stdcall; external 'usp10.dll';
function ScriptGetFontLanguageTags(hdc: HDC; var psc: SCRIPT_CACHE; psa: Pointer; tagScript: OPENTYPE_TAG; cMaxTags: Integer; tags: POPENTYPE_TAG; var pcTags: Integer): HRESULT; stdcall; external 'usp10.dll';
function ScriptFreeCache(var psc: SCRIPT_CACHE): HRESULT; stdcall; external 'usp10.dll';

function OpenTypeTagToString(tag: OPENTYPE_TAG): string;
begin
  Result := Chr(Lo(LoWord(tag))) + Chr(Hi(Loword(tag))) + Chr(Lo(HiWord(tag))) + Chr(Hi(HiWord(tag)));
end;

function FindFontEnum(lpelfe: PENUMLOGFONTEX; lpntme: PNewTextMetricEx; FontType: DWORD; lParam: LPARAM): integer; stdcall;
var
  hfont: THandle;
  I: Integer;
    temp_dc: THandle;
    old_font: THandle;

  pScriptTags: POPENTYPE_TAG;

  pcTags: Integer;
  psc: SCRIPT_CACHE;
  
  scriptnode, subnode, node: IXMLDOMNode;
  p: POPENTYPE_TAG;
  j: Integer;
  langnode: IXMLDOMNode;
  pcLangTags: Integer;
  pLangTags: POPENTYPE_TAG;
  q: POPENTYPE_TAG;
begin
  Result := 1;

  if FontType <> TRUETYPE_FONTTYPE then Exit;

  node := IXMLDOMNode(lParam);

  if lpelfe.elfLogFont.lfFaceName[0] = '@' then
    Exit; // Ignore Far-East font names

  for I := 0 to node.childNodes.length - 1 do
    if SameText(xmlGetAttribute(node.childNodes[I], 'FaceName'), lpelfe.elfLogFont.lfFaceName) then Exit;

  subnode := xmlAddChild(node, 'Font');
  xmlSetAttribute(subnode, 'FaceName', lpelfe.elfLogFont.lfFaceName);

  SetLastError(0);
  hfont := CreateFontIndirect(lpelfe.elfLogFont);
  if GetLastError <> 0 then
  begin
    xmlSetAttribute(subnode, 'Error', SysErrorMessage(GetLastError));
    Exit;
  end;

  psc := nil;
  try
    temp_dc := GetDC(0);
    old_font := SelectObject(temp_dc, hfont);
    pScriptTags := AllocMem(200 * sizeof(OPENTYPE_TAG));
    pLangTags := AllocMem(200 * sizeof(OPENTYPE_TAG));
    try
      if not Succeeded(ScriptGetFontScriptTags(temp_dc, psc, nil, 200, pScriptTags, pcTags)) then
      begin
        xmlSetAttribute(subnode, 'Error', 'Failed to get script tags');
        Exit;
      end;

      p := pScriptTags;
      for I := 0 to pcTags - 1 do
      begin
        scriptnode := xmlAddChild(subnode, 'Script');
        xmlSetAttribute(scriptnode, 'Tag', OpenTypeTagToString(p^));
        Inc(p);

        if not Succeeded(ScriptGetFontLanguageTags(temp_dc, psc, nil, p^, 200, pLangTags, pcLangTags)) then
        begin
          xmlSetAttribute(scriptnode, 'Error', 'Failed to get language tags');
          Continue;
        end;

        q := pLangTags;
        for j := 0 to pcLangTags - 1 do
        begin
          langnode := xmlAddChild(scriptnode, 'Language');
          xmlSetAttribute(langnode, 'Tag', OpenTypeTagToString(q^));
          Inc(q);
        end;
      end;

    finally
      SelectObject(temp_dc, old_font);
      ReleaseDC(0, temp_dc);
      FreeMem(pScriptTags);
      FreeMem(pLangTags);
    end;
  finally
    if psc <> nil then
      ScriptFreeCache(psc);
    DeleteObject(hfont);
  end;
end;

function TSI_Fonts.DoCollect: Boolean;
var
  node: IXMLDOMNode;
  lf: TLogFont;
  hdc: THandle;
begin
  node := xmlAddChild(rootnode,'Fonts');

  hdc := GetDC(0);
  try
    FillChar(lf, sizeof(lf), 0);
    lf.lfCharSet := DEFAULT_CHARSET;
    EnumFontFamiliesEx(hdc, lf, @FindFontEnum, NativeInt(Pointer(node)), 0);
  finally
    ReleaseDC(0, hdc);
  end;

  Result := True;
end;

initialization
  TSI_Fonts.Register;
end.
