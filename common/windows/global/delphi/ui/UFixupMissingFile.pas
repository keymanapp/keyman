(*
  Name:             UFixupMissingFile
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      28 Aug 2008

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          28 Aug 2008 - mcdurdin - I1607 - Fix crash if xsl file is missing
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit UFixupMissingFile;  // I3306

interface

uses
  SysUtils,
  jwamsi;

type
  EFixupMissingFile = class(Exception)
  end;

procedure FixupMissingFile(const path: WideString);

implementation

uses
  Windows;

{**

  FixupMissingFile: reinstalls the missing files for all products using Keyman Engine.

**}

procedure FixupMissingFile(const path: WideString);
const
  componentcode = '{58D31506-F964-47AF-B4A0-51107A74C2FE}';
var
  err: DWord;
  productcode: array[0..39] of char;
  i: Cardinal;
begin
  i := 0;
  err := MsiEnumClients(componentcode, i, productcode);
  while err = ERROR_SUCCESS do
  begin
    err := MsiReinstallProduct(productcode, REINSTALLMODE_FILEMISSING);
    if err <> ERROR_SUCCESS then
    begin
      raise EFixupMissingFile.Create('Keyman could not be repaired.  Please reinstall this product.'#13#10#13#10+
        'Error message was: MsiReinstallProduct: '+SysErrorMessage(err));
    end;
    Inc(i);
    err := MsiEnumClients(componentcode, i, productcode);
  end;
  if err <> ERROR_NO_MORE_ITEMS then
    raise EFixupMissingFile.Create('Keyman could not be repaired.  Please reinstall this product.'#13#10#13#10+
      'Error message was: MsiEnumClients: '+SysErrorMessage(err));

end;

end.
