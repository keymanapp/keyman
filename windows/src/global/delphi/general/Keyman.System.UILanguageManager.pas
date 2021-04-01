unit Keyman.System.UILanguageManager;

interface

uses
  System.Classes;

type
  TUILanguageManager = class
  public
    class function GetUserUILanguages: TStrings; static;
    class function Find(KeymanLanguages, UserLanguages: TStrings): string; static;
  end;

implementation

uses
  System.SysUtils,
  Winapi.Windows;

{ TUILanguageManager }

/// Returns list of BCP 47 tags (lower cased) of user's preferred UI languages
class function TUILanguageManager.GetUserUILanguages: TStrings;
var
  ulNumLanguages, cchLanguagesBuffer: ULONG;
  p, pwszLanguagesBuffer: PChar;
begin
  Result := TStringList.Create;

  cchLanguagesBuffer := 0;
  ulNumLanguages := 0;

  if GetUserPreferredUILanguages(MUI_LANGUAGE_NAME, @ulNumLanguages, nil, @cchLanguagesBuffer) then
  begin
    pwszLanguagesBuffer := AllocMem(cchLanguagesBuffer * sizeof(Char));
    try
      if GetUserPreferredUILanguages(MUI_LANGUAGE_NAME, @ulNumLanguages, pwszLanguagesBuffer, @cchLanguagesBuffer) then
      begin
        p := pwszLanguagesBuffer;
        // pwszLanguagesBuffer is a null delimited buffer terminated with two nulls
        while p^ <> #0 do
        begin
          Result.Add(LowerCase(p));
          p := StrEnd(p);
          Inc(p);
        end;
      end;
    finally
      FreeMem(pwszLanguagesBuffer);
    end;
  end;
end;

/// Look at the Windows UI language list and see if we have anything similar
/// in our list of available UI languages, and if so, returns it. Returns empty
/// string if nothing found.
class function TUILanguageManager.Find(KeymanLanguages, UserLanguages: TStrings): string;
var
  ktag, ktag2, utag, utag2: string;
  n: Integer;
begin

  for utag in UserLanguages do
  begin
    utag2 := utag;

    // Try and look for an exact (case-insensitive) match first
    n := KeymanLanguages.IndexOf(utag2);
    if n >= 0 then
      Exit(KeymanLanguages[n]);

    // Then try and set the region-neutral language
    if Pos('-', utag2) > 0 then
      Delete(utag2, Pos('-', utag2), MaxInt);

    for ktag in KeymanLanguages do
    begin
      ktag2 := ktag;
      if Pos('-', ktag2) > 0 then
        Delete(ktag2, Pos('-', ktag2), MaxInt);
      if ktag2 = utag2 then
        Exit(ktag); // we return the Keyman UI language BCP 47 tag which was closest
    end;
  end;

  Result := '';
end;

end.
