/*
  Version information from build-utils.sh:

  #   KEYMAN_VERSION:              Full current build version, e.g. "14.0.1"
  #   [KEYMAN_VERSION_WIN:         Full current build version for Windows, e.g. "14.0.1.0"]
  #   KEYMAN_VERSION_RELEASE:      Current release version, e.g. "14.0"
  #   KEYMAN_VERSION_MAJOR:        Major version, e.g. "14"
  #   KEYMAN_VERSION_MINOR:        Minor version, e.g. "0"
  #   KEYMAN_VERSION_PATCH:        Patch version, e.g. "1"
  #   KEYMAN_TIER:                 Current tier, one of "alpha", "beta" or "stable"
  #   KEYMAN_VERSION_TAG:          Tier + Pull Request + Location of build [-alpha|-beta][-test[-1234]][-local]
  #   KEYMAN_VERSION_WITH_TAG:     e.g. "14.0.1-alpha-test-1234" or "14.0.5-beta-local" or "14.0.1-alpha-test"
  #   [KEYMAN_ROOT:                fully resolved root path of Keyman repository]
  #   KEYMAN_VERSION_ENVIRONMENT:  One of: local, test, alpha, beta, stable
  #   KEYMAN_VERSION_GIT_TAG:      Git tag for the release, "release@$KEYMAN_VERSION_WITH_TAG", e.g. "release@14.0.1-alpha-test-1234"
*/

export interface Environment {
  version: string;
  versionRelease: string;
  versionMajor: string;
  versionMinor: string;
  versionPatch: string;
  tier: string;
  versionTag: string;
  versionWithTag: string;
  versionEnvironment: string;
  versionGitTag: string;
  // Pull Request Data
  pr: string;
}

export function extractVersionData(version: string): Environment {
  const versionData = /^(\d+)\.(\d+)\.(\d+)((?:-(alpha|beta))?(?:-(test|local))?(?:-(\d+))?)$/.exec(version);
  if(!versionData) return null;

  return {
    version: `${versionData[1]}.${versionData[2]}.${versionData[3]}`,
    versionRelease: `${versionData[1]}.${versionData[2]}`,
    versionMajor: versionData[1],
    versionMinor: versionData[2],
    versionPatch: versionData[3],
    tier: versionData[5] || 'stable',
    versionTag: versionData[4],
    versionWithTag: version,
    versionEnvironment: versionData[6] || versionData[5] || 'stable',
    versionGitTag: 'release@'+version,

    pr: versionData[7] || ''
  };
};

