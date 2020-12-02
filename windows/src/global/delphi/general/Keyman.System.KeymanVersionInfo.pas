unit Keyman.System.KeymanVersionInfo;

interface

{$I keymanversioninfo.inc}

function BuildKeymanVersionInfo(const Version, Tier, Tag, Environment: string): TKeymanVersionInfo;

implementation

uses
  System.SysUtils,
  System.RegularExpressions;

type
  EKeymanVersionInfo = class(Exception);

function BuildKeymanVersionInfo(const Version, Tier, Tag, Environment: string): TKeymanVersionInfo;
const
  S_Regex_Version = '^(\d+)\.(\d+)\.(\d+)$';
  S_E_InvalidVersion = 'Invalid version tag %0:s (Tier=%1:s, Tag=%2:s)';
  S_E_InvalidTier = 'Invalid tier tag %1:s (Version=%0:s, Tag=%2:s)';
  S_Format_Release = '%d.%d';
  S_Format_Win = '%d.%d.%d.0';
  S_Format_Rc = '%d,%d,%d,0';
var
  m: TMatch;
  vmajor, vminor, vpatch: Integer;
begin
  m := TRegEx.Match(Version, S_Regex_Version);
  if not m.Success then
    raise EKeymanVersionInfo.CreateFmt(S_E_InvalidVersion, [Version, Tier, Tag]);

  if (Tier <> 'alpha') and (Tier <> 'beta') and (Tier <> 'stable') then
    raise EKeymanVersionInfo.CreateFmt(S_E_InvalidTier, [Version, Tier, Tag]);

  // Tag may be more variable, so we won't try and limit it here

  vmajor := StrToInt(m.Groups[1].Value);
  vminor := StrToInt(m.Groups[2].Value);
  vpatch := StrToInt(m.Groups[3].Value);


  Result.Version := Version;
  Result.VersionWin := Format(S_Format_Win, [vmajor, vminor, vpatch]);
  Result.VersionRelease := Format(S_Format_Release, [vmajor, vminor]);
  Result.VersionMajor := vmajor;
  Result.VersionMinor := vminor;
  Result.VersionPatch := vpatch;
  Result.Tier := Tier;
  Result.Tag := Tag;
  Result.VersionWithTag := Version + Tag;
  Result.VersionRc := Format(S_Format_Rc, [vmajor, vminor, vpatch]);
  Result.Environment := Environment;
end;

end.
