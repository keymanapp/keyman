// See discussion online at https://github.com/keymanapp/keyman/issues/1285
// Introduced in Keyman 11. Back ported to Keyman 10. If Microsoft introduces
// a fix in a later release, we will need to update this to turn the mitigation
// off for that version. In that situation, users will need to reinstall their
// language profile in order to get the corrected language.
unit Keyman.System.MitigateWin10_1803LanguageInstall;

interface

uses
  Winapi.Windows;

type
  TMitigateWin10_1803 = class
  type
    TLanguageReference = record
      Code: LANGID;
      BCP47: string;
      Name: string;
    end;

    TMitigatedLanguage = record
      OriginalLanguage: TLanguageReference;
      NewLanguage: TLanguageReference;
    end;

    class function IsMitigationRequired(Code: LANGID; var lang: TMitigateWin10_1803.TMitigatedLanguage): Boolean; overload;
    class function IsMitigationRequired(BCP47Tag: string; var lang: TMitigateWin10_1803.TMitigatedLanguage): Boolean; overload;
  end;

implementation

uses
  System.SysUtils;

const
  MitigatedLanguages: array[0..2] of TMitigateWin10_1803.TMitigatedLanguage = (
    (OriginalLanguage: (Code: $005E; BCP47: 'am'; Name: 'Amharic'); NewLanguage: (BCP47: 'gez-Ethi-ET'; Name: 'Geez')),
    (OriginalLanguage: (Code: $0073; BCP47: 'ti'; Name: 'Tigrinya'); NewLanguage: (BCP47: 'gez-Ethi-ET'; Name: 'Geez')),
    (OriginalLanguage: (Code: $005B; BCP47: 'si'; Name: 'Sinhala'); NewLanguage: (Code: $0409; Name: 'English (US)'))
  );

{ TMitigateWin10_1803 }

const
  WINDOWS_10_MAJORVERSION = 10;
  WINDOWS_10_MINORVERSION = 0;
  WINDOWS_10_BUILDNUMBER_WithFailure = 17134;
  WINDOWS_10_BUILDNUMBER_Fixed = 19597;

function IsWindows10_OrGreater_Build(Build: DWORD): BOOL; inline;
var
  osvi: OSVERSIONINFOEXW;
  dwlConditionMask: DWORDLONG;
begin
  // Code pattern comes from Keyman.Winapi.VersionHelpers, but testing also
  // build number so cannot use functions already there. This also makes back-porting
  // to Keyman 10 simpler.
  FillChar(osvi, SizeOf(osvi), 0);
  osvi.dwOSVersionInfoSize := SizeOf(osvi);
  dwlConditionMask := VerSetConditionMask(
    VerSetConditionMask(
    VerSetConditionMask(
      0, VER_MAJORVERSION, VER_GREATER_EQUAL),
         VER_MINORVERSION, VER_GREATER_EQUAL),
         VER_BUILDNUMBER, VER_GREATER_EQUAL);

  osvi.dwMajorVersion := WINDOWS_10_MAJORVERSION;
  osvi.dwMinorVersion := WINDOWS_10_MINORVERSION;
  osvi.dwBuildNumber := Build;

  Result := VerifyVersionInfoW(osvi, VER_MAJORVERSION or VER_MINORVERSION or VER_BUILDNUMBER, dwlConditionMask) <> False;
end;

class function TMitigateWin10_1803.IsMitigationRequired(Code: LANGID; var lang: TMitigateWin10_1803.TMitigatedLanguage): Boolean;
var
  I: Integer;
begin
  if not IsWindows10_OrGreater_Build(WINDOWS_10_BUILDNUMBER_WithFailure) then
    Exit(False);

  if IsWindows10_OrGreater_Build(WINDOWS_10_BUILDNUMBER_Fixed) then
    Exit(False);

  Code := PRIMARYLANGID(Code);
  for I := Low(MitigatedLanguages) to High(MitigatedLanguages) do
    if MitigatedLanguages[I].OriginalLanguage.Code = Code then
    begin
      lang := MitigatedLanguages[I];
      Exit(True);
    end;
  // lang value undefined.
  Result := False;
end;

class function TMitigateWin10_1803.IsMitigationRequired(BCP47Tag: string;
  var lang: TMitigateWin10_1803.TMitigatedLanguage): Boolean;
var
  I: Integer;
begin
  if not IsWindows10_OrGreater_Build(WINDOWS_10_BUILDNUMBER_WithFailure) then
    Exit(False);

  if IsWindows10_OrGreater_Build(WINDOWS_10_BUILDNUMBER_Fixed) then
    Exit(False);

  // We only want to look at the primary language tag for comparison
  // And we assume that the tag has been canonicalized from ISO639-3 first
  BCP47Tag := LowerCase(BCP47Tag);
  I := Pos('-', BCP47Tag);
  if I > 0 then
    BCP47Tag := Copy(BCP47Tag, 1, I-1);

  for I := Low(MitigatedLanguages) to High(MitigatedLanguages) do
    if MitigatedLanguages[I].OriginalLanguage.BCP47 = BCP47Tag then
    begin
      lang := MitigatedLanguages[I];
      Exit(True);
    end;
  // lang value undefined.
  Result := False;
end;

end.
