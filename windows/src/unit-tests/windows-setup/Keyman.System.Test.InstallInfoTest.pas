unit Keyman.System.Test.InstallInfoTest;

interface

uses
  System.Classes,
  System.SysUtils,
  DUnitX.TestFramework;

type
  [TestFixture]
  TInstallInfoTest = class(TObject)
  public
    [Test]
    procedure TestLocatePackagesAndTierFromFilename;

    [Test]
    procedure TestLocatePackagesFromParameter;
  end;

implementation

uses
  KeymanVersion,
  Keyman.Setup.System.InstallInfo;

{ TInstallInfoTest }

procedure TInstallInfoTest.TestLocatePackagesAndTierFromFilename;
var
  ii: TInstallInfo;
begin
  ii := TInstallInfo.Create('');
  try
    // It should match a standard pattern
    ii.LocatePackagesAndTierFromFilename('c:\foo\keyman-setup.khmer_angkor.km.exe');
    Assert.AreEqual(CKeymanVersionInfo.Tier, ii.Tier);
    Assert.AreEqual(1, ii.Packages.Count);
    Assert.AreEqual('khmer_angkor', ii.Packages[0].ID);
    Assert.AreEqual('km', ii.Packages[0].BCP47);
  finally
    ii.Free;
  end;

  ii := TInstallInfo.Create('');
  try
    // It should match a standard pattern with a tier
    ii.LocatePackagesAndTierFromFilename('c:\foo\keyman-setup-alpha.khmer_angkor.km.exe');
    Assert.AreEqual(TIER_ALPHA, ii.Tier);
    Assert.AreEqual(1, ii.Packages.Count);
    Assert.AreEqual('khmer_angkor', ii.Packages[0].ID);
    Assert.AreEqual('km', ii.Packages[0].BCP47);
  finally
    ii.Free;
  end;

  ii := TInstallInfo.Create('');
  try
    // It should match a standard pattern with a tier
    ii.LocatePackagesAndTierFromFilename('c:\foo\keyman-setup-beta.khmer_angkor.km.exe');
    Assert.AreEqual(TIER_BETA, ii.Tier);
    Assert.AreEqual(1, ii.Packages.Count);
    Assert.AreEqual('khmer_angkor', ii.Packages[0].ID);
    Assert.AreEqual('km', ii.Packages[0].BCP47);
  finally
    ii.Free;
  end;

  ii := TInstallInfo.Create('');
  try
    // It should match a standard pattern with a tier
    ii.LocatePackagesAndTierFromFilename('c:\foo\keyman-setup-stable.khmer_angkor.km.exe');
    Assert.AreEqual(TIER_STABLE, ii.Tier);
    Assert.AreEqual(1, ii.Packages.Count);
    Assert.AreEqual('khmer_angkor', ii.Packages[0].ID);
    Assert.AreEqual('km', ii.Packages[0].BCP47);
  finally
    ii.Free;
  end;

  ii := TInstallInfo.Create('');
  try
    // It should match a standard pattern
    ii.LocatePackagesAndTierFromFilename('c:\foo\keyman-setup.khmer_angkor.km.sil_euro_latin.fr.exe');
    Assert.AreEqual(2, ii.Packages.Count);
    Assert.AreEqual('khmer_angkor', ii.Packages[0].ID);
    Assert.AreEqual('km', ii.Packages[0].BCP47);
    Assert.AreEqual('sil_euro_latin', ii.Packages[1].ID);
    Assert.AreEqual('fr', ii.Packages[1].BCP47);
  finally
    ii.Free;
  end;

  ii := TInstallInfo.Create('');
  try
    // It should strip off " (1)" suffixes when these are added by web browser
    ii.LocatePackagesAndTierFromFilename('c:\foo\keyman-setup.khmer_angkor.km (1).exe');
    Assert.AreEqual(1, ii.Packages.Count);
    Assert.AreEqual('khmer_angkor', ii.Packages[0].ID);
    Assert.AreEqual('km', ii.Packages[0].BCP47);
  finally
    ii.Free;
  end;

  ii := TInstallInfo.Create('');
  try
    // It should give an empty BCP 47 tag if one is not provided
    ii.LocatePackagesAndTierFromFilename('c:\foo\keyman-setup.khmer_angkor.exe');
    Assert.AreEqual(1, ii.Packages.Count);
    Assert.AreEqual('khmer_angkor', ii.Packages[0].ID);
    Assert.IsEmpty(ii.Packages[0].BCP47);
  finally
    ii.Free;
  end;

  ii := TInstallInfo.Create('');
  try
    // It should only match on keyman-setup
    ii.LocatePackagesAndTierFromFilename('c:\foo\setup.khmer_angkor.km.exe');
    Assert.AreEqual(0, ii.Packages.Count, 'setup.khmer_angkor.km.exe');
  finally
    ii.Free;
  end;

  ii := TInstallInfo.Create('');
  try
    // It should match packages with less common characters in filename
    ii.LocatePackagesAndTierFromFilename('c:\foo\keyman-setup.khmer angkor.km.exe');
    Assert.AreEqual(1, ii.Packages.Count);
    Assert.AreEqual('khmer angkor', ii.Packages[0].ID);
    Assert.AreEqual('km', ii.Packages[0].BCP47);
  finally
    ii.Free;
  end;

  ii := TInstallInfo.Create('');
  try
    // It should match packages with less common characters in filename
    ii.LocatePackagesAndTierFromFilename('c:\foo\keyman-setup.khmer-angkor.km.exe');
    Assert.AreEqual(1, ii.Packages.Count);
    Assert.AreEqual('khmer-angkor', ii.Packages[0].ID);
    Assert.AreEqual('km', ii.Packages[0].BCP47);
  finally
    ii.Free;
  end;
end;

procedure TInstallInfoTest.TestLocatePackagesFromParameter;
var
  ii: TInstallInfo;
begin
  ii := TInstallInfo.Create('');
  try
    // It should match a standard pattern
    ii.LocatePackagesFromParameter('khmer_angkor=km');
    Assert.AreEqual(1, ii.Packages.Count);
    Assert.AreEqual('khmer_angkor', ii.Packages[0].ID);
    Assert.AreEqual('km', ii.Packages[0].BCP47);
  finally
    ii.Free;
  end;

  ii := TInstallInfo.Create('');
  try
    // It should match a standard pattern
    ii.LocatePackagesFromParameter('khmer_angkor=km,sil_euro_latin=fr');
    Assert.AreEqual(2, ii.Packages.Count);
    Assert.AreEqual('khmer_angkor', ii.Packages[0].ID);
    Assert.AreEqual('km', ii.Packages[0].BCP47);
    Assert.AreEqual('sil_euro_latin', ii.Packages[1].ID);
    Assert.AreEqual('fr', ii.Packages[1].BCP47);
  finally
    ii.Free;
  end;

  ii := TInstallInfo.Create('');
  try
    // It should give an empty BCP 47 tag if one is not provided
    ii.LocatePackagesFromParameter('khmer_angkor');
    Assert.AreEqual(1, ii.Packages.Count);
    Assert.AreEqual('khmer_angkor', ii.Packages[0].ID);
    Assert.IsEmpty(ii.Packages[0].BCP47);
  finally
    ii.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TInstallInfoTest);
end.
