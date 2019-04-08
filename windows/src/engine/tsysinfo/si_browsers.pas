(*
  Name:             si_keyman
  Copyright:        Copyright (C) SIL International.
  Documentation:    km4.1
  Description:      Get keyman specific details from system
  Create Date:      13 May 2005

  Modified Date:    26 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          10 May 2010 - mcdurdin - I2367 - Initial version (from si_keyman)
                    18 Feb 2011 - mcdurdin - I2721 - Override Javascript-disabled security for web controls
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    19 Jul 2011 - mcdurdin - I3000 - Tweak display of diagnostics using .xslt files
                    26 Jun 2012 - mcdurdin - I3379 - KM9 - Remove old Winapi references now in Delphi libraries
*)
unit si_browsers;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, si_base, StdCtrls, msxml;

type
  TSI_Browsers = class(TSI_Base)
  protected
    function DoCollect: Boolean; override;
    class function GetCaption: String; override;
  end;

implementation

uses ErrorControlledRegistry, RegistryKeys, ComObj, sysinfo_Util, ShellApi, ShlObj;

{ TSI_Browsers }

class function TSI_Browsers.GetCaption: String;
begin
  Result := 'Browsers';
end;

function TSI_Browsers.DoCollect: Boolean;
var
  regnode, subnode, node: IXMLDOMNode;
begin
  node := xmlAddChild(rootnode,'Browsers');
  node := xmlAddChild(node,'InternetExplorer');

  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly('Software\Microsoft\Internet Explorer') and ValueExists('Version') then
    begin
      xmlSetAttribute(node, 'Version', XMLTextEncode(ReadString('Version')));
    end
    else
      xmlSetAttribute(node, 'Version', XMLTextEncode('Unknown'));
  finally
    Free;
  end;

  subnode := xmlAddChild(node,'Registry');
  regnode := xmlAddChild(subnode,'CurrentUserScripts');
  AddRegistry(regnode, HKEY_CURRENT_USER, 'Software\Microsoft\Internet Explorer\International\Scripts');

  Result := True;
end;

initialization
  TSI_Browsers.Register;
end.
