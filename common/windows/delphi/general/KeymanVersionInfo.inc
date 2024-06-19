type
  TKeymanVersionInfo = record
    // These fields match fields in /resources/build/build-utils.sh
    Version: string;        // 14.0.2
    VersionWin: string;     // 14.0.2.0
    VersionRelease: string; // 14.0
    VersionMajor: Integer;  // 14
    VersionMinor: Integer;  // 0
    VersionPatch: Integer;  // 2
    Tier: string;           // alpha, beta or stable
    Tag: string;            // Tier + Pull Request + Location of build [-alpha|-beta][-test[-1234]][-local]
    VersionWithTag: string; // 14.0.2-alpha-test-1234 or 14.0.2-beta-local or 14.0.2
    VersionGitTag: string;  // release@14.0.2-alpha-test-1234
    Environment: string;    // alpha, beta, stable, test or local

    // These fields are useful only for Windows apps
    VersionRc: string;      // 14,0,2,0
  end;
