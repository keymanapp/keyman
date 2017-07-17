(*
  Name:             FixupLocaleDoctype
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      11 Jan 2011

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          11 Jan 2011 - mcdurdin - I2605 - Fix crash starting Keyman Desktop due to invalid path in locale doctype after upgrade
                    18 Mar 2011 - mcdurdin - I2765 - Fix crash when locale file is in use when trying to correct DTD reference
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit FixupLocaleDoctype;  // I2605  // I3306

interface

uses
  Windows,
  keymanapi_TLB;

procedure UpdateLocaleDoctype(const path: WideString);
procedure UpdateAllLocaleDoctypes;

implementation

uses
  Classes,
  custinterfaces,
  Dialogs,
  KeymanPaths,
  kmint,
  SysUtils,
  Unicode;

procedure UpdateLocaleDoctype(const path: WideString);
var
  I: Integer;
  localedefpath: WideString;
  ss: string;
  n1: Integer;
  n2: Integer;
  l: Integer;
begin
  localedefpath := TKeymanPaths.KeymanDesktopInstallPath('xml\localedef.dtd');
  if not FileExists(localedefPath) then
    localedefpath := TKeymanPaths.KeymanDesktopInstallPath('localedef.dtd');

  with TStringList.Create do
  try
    LoadFromFile(path);
    ss := Text;   // I1320 - fix locale.xml doctype parsing and replacement

    n1 := Pos('<!DOCTYPE', ss);
    if n1 > 0 then
    begin
      i := 1; n2 := n1 + 8; l := Length(ss);
      while (i > 0) and (n2 <= l) do
      begin
        case ss[n2] of
          '<': Inc(i);
          '>': Dec(i);
        end;
        Inc(n2);
      end;

      if n2 <= l then
      begin
        Text := Copy(ss, 1, n1-1) + '<!DOCTYPE Locale SYSTEM '''+localedefpath+'''>' + Copy(ss, n2+1, l);
        SaveToFile(path, Encoding);
      end;
    end;
  finally
    Free;
  end;
end;

procedure UpdateAllLocaleDoctypes;  // I2605
var
  I: Integer;
  FLocalePath: WideString;
  hFile: THandle;
  J: Integer;
begin
  with kmint.KeymanCustomisation.CustMessages do
  begin
    with TStringList.Create do
    try
      Text := GetAvailableLanguages;
      for I := 0 to Count - 1 do
      begin
        FLocalePath := GetLocalePathForLocale(Strings[I]);

        if FileExists(FLocalePath) then
        begin
          hFile := CreateFileW(PWideChar(FLocalePath), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
          if hFile <> INVALID_HANDLE_VALUE then
          begin
            CloseHandle(hFile);
            for J := 0 to 5 do
            try
              UpdateLocaleDoctype(FLocalePath);
              Break;
            except
              on E:EFCreateError do // I2765 - File may be in use
              begin
                if J = 5
                  then ShowMessage('Unable to update translation file '+Strings[I]+'.  Please contact Keyman Support for assistance.')
                  else Sleep(500);
              end;
            end;
          end;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

end.
