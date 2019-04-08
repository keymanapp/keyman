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
  History:          10 May 2010 - mcdurdin - I1134 - Initial version (from si_keyman)
                    18 Feb 2011 - mcdurdin - I2721 - Override Javascript-disabled security for web controls
                    19 Jul 2011 - mcdurdin - I3000 - Tweak display of diagnostics using .xslt files
*)
unit si_office;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, si_base, StdCtrls, msxml;

type
  TSI_Office = class(TSI_Base)
  protected
    function DoCollect: Boolean; override;
    class function GetCaption: String; override;
  end;

implementation

uses RegistryKeys, ComObj, sysinfo_Util, ShellApi, ShlObj;

{ TSI_Office }

class function TSI_Office.GetCaption: String;
begin
  Result := 'Microsoft Office';
end;

const
  CSIDL_PROGRAM_FILES = $26;

function TSI_Office.DoCollect: Boolean;
var
  regnode2, regnode, subnode, node: IXMLDOMNode;
begin
  node := xmlAddChild(rootnode,'Office');

  subnode := xmlAddChild(node,'Registry');
  regnode := xmlAddChild(subnode,'Office2000');
  AddRegistry(regnode, HKEY_CURRENT_USER, 'Software\Microsoft\Office\9.0\Common\LanguageResources');

  regnode := xmlAddChild(subnode,'OfficeXP');
  AddRegistry(regnode, HKEY_CURRENT_USER, 'Software\Microsoft\Office\10.0\Common\LanguageResources');

  regnode := xmlAddChild(subnode,'Office2003');
  AddRegistry(regnode, HKEY_CURRENT_USER, 'Software\Microsoft\Office\11.0\Common\LanguageResources');

  regnode := xmlAddChild(subnode,'Office2007');
  AddRegistry(regnode, HKEY_CURRENT_USER, 'Software\Microsoft\Office\12.0\Common\LanguageResources');

  regnode := xmlAddChild(subnode,'Office2010');
  AddRegistry(regnode, HKEY_CURRENT_USER, 'Software\Microsoft\Office\13.0\Common\LanguageResources');
  //regnode := xmlAddChild(subnode,'CurrentUser');
  //AddRegistry(regnode, HKEY_CURRENT_USER, 'Software\Microsoft\Office');

  subnode := xmlAddChild(node,'Files');
  regnode := xmlAddChild(subnode,'Program-Files-MicrosoftOffice');
  regnode2 := xmlAddChild(regnode,'Office9');
  AddFiles(regnode2, CSIDL_PROGRAM_FILES, 'Microsoft Office\Office9');
  regnode2 := xmlAddChild(regnode,'Office10');
  AddFiles(regnode2, CSIDL_PROGRAM_FILES, 'Microsoft Office\Office10');
  regnode2 := xmlAddChild(regnode,'Office11');
  AddFiles(regnode2, CSIDL_PROGRAM_FILES, 'Microsoft Office\Office11');
  regnode2 := xmlAddChild(regnode,'Office12');
  AddFiles(regnode2, CSIDL_PROGRAM_FILES, 'Microsoft Office\Office12');
  regnode2 := xmlAddChild(regnode,'Office13');
  AddFiles(regnode2, CSIDL_PROGRAM_FILES, 'Microsoft Office\Office13');

  Result := True;
end;

initialization
  TSI_Office.Register;
end.
