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
    [Setup]
    procedure Setup;

    [Test]
    procedure TestLocatePackagesAndTierFromFilename;

    [Test]
    procedure TestLocatePackagesFromParameter;

    [Test]
    procedure TestSerialization;
  end;

implementation

uses
  System.JSON,

  KeymanVersion,
  Keyman.Setup.System.InstallInfo,
  utildir;

{ TInstallInfoTest }

procedure TInstallInfoTest.Setup;
begin
  Assert.IgnoreCaseDefault := False;
end;

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

procedure TInstallInfoTest.TestSerialization;
var
  ii: TInstallInfo;
//  o: TJSONObject;
  iifl: TInstallInfoFileLocation;
  iipflo: TInstallInfoPackageFileLocation;
  lang: TInstallInfoPackageLanguage;
  iip: TInstallInfoPackage;
  FTempFilename: string;
const
  ii_EditionTitle = 'Test App';
  ii_StartDisabled = True; // use non-default value
  ii_StartWithConfiguration = True; // use non-default value
  ii_ShouldInstallKeyman = False;  // use non-default value
  ii_Tier = 'stable';  // may not be the default tier
  ii_String = 'ssApplicationTitle=Test App';

  iifll_LocationType: TInstallInfoLocationType = iilLocal;
  iifll_Size = 1234;
  iifll_Path = 'c:\foo\keyman.msi';
  iifll_Url = '';
  iifll_ProductCode = 'D992D213-3F78-42D8-AC97-289059E6DC15';
  iifll_VersionWithTag = '14.0.111-beta-test';
  iifll_Version = '14.0.111';

  iiflo_LocationType: TInstallInfoLocationType = iilOnline;
  iiflo_Size = 2468;
  iiflo_Path = '';
  iiflo_Url = 'https://downloads.keyman.example/keyman.msi';
  iiflo_ProductCode = 'D992D213-3F78-42D8-AC97-289059E6DC15';
  iiflo_VersionWithTag = '14.0.112-beta-test';
  iiflo_Version = '14.0.112';

  iipflo_LocationType: TInstallInfoLocationType = iilOnline;
  iipflo_name = 'Khmer Angkor';
  iipflo_Size = 1111;
  iipflo_Path = '';
  iipflo_Url = 'https://downloads.keyman.example/khmer_angkor.kmp';
  iipflo_ProductCode = '';
  iipflo_VersionWithTag = '1.2.3';
  iipflo_Version = '1.2.3';

  lang_0_bcp47 = 'km';
  lang_0_name = 'Khmer';
  lang_1_bcp47 = 'cmo';
  lang_1_name = 'Mnong, Central';

  iip_ShouldInstall = False; // non-default
  iip_ID = 'khmer_angkor';
  iip_BCP47 = 'km';
begin
  FTempFilename := KGetTempFileName;
//  o := TJSONObject.Create;
  try
    ii := TInstallInfo.Create('');
    try
      // Add a common set of data.
      iip := TInstallInfoPackage.Create(iip_ID, iip_BCP47);
      iip.ShouldInstall := iip_ShouldInstall;

      iipflo := TInstallInfoPackageFileLocation.Create(iipflo_LocationType);
      iipflo.Name := iipflo_Name;

      lang := TInstallInfoPackageLanguage.Create(lang_0_bcp47, lang_0_name);
      iipflo.Languages.Add(lang);
      lang := TInstallInfoPackageLanguage.Create(lang_1_bcp47, lang_1_name);
      iipflo.Languages.Add(lang);
      iipflo.Size := iipflo_Size;
      iipflo.Path := iipflo_Path;
      iipflo.Url := iipflo_Url;
      iipflo.ProductCode := iipflo_ProductCode;
      iipflo.VersionWithTag := iipflo_VersionWithTag;
      iipflo.Version := iipflo_Version;
      iip.Locations.Add(iipflo);

      iip.InstallLocation := iipflo;
      ii.Packages.Add(iip);

      ii.EditionTitle := ii_EditionTitle;
      ii.StartDisabled := ii_StartDisabled;
      ii.StartWithConfiguration := ii_StartWithConfiguration;
      ii.ShouldInstallKeyman := ii_ShouldInstallKeyman;
      ii.Tier := ii_Tier;
      ii.Strings.Add(ii_String);

      iifl := TInstallInfoFileLocation.Create(iifll_LocationType);
      iifl.Size := iifll_Size;
      iifl.Path := iifll_Path;
      iifl.Url := iifll_Url;
      iifl.ProductCode := iifll_ProductCode;
      iifl.VersionWithTag := iifll_VersionWithTag;
      iifl.Version := iifll_Version;
      ii.MsiLocations.Add(iifl);

      iifl := TInstallInfoFileLocation.Create(iiflo_LocationType);
      iifl.Size := iiflo_Size;
      iifl.Path := iiflo_Path;
      iifl.Url := iiflo_Url;
      iifl.ProductCode := iiflo_ProductCode;
      iifl.VersionWithTag := iiflo_VersionWithTag;
      iifl.Version := iiflo_Version;
      ii.MsiLocations.Add(iifl);

      ii.CheckMsiUpgradeScenarios;
      ii.MsiInstallLocation := ii.MsiLocations[0];

      // We test with the temp filename because we'll know it exists when we
      // want to assert the result later -- it's not used to actually draw the
      // title as part of this test, so this is safe and avoids creating another
      // dummy file for the test.
      ii.TitleImageFilename := FTempFilename;

      ii.SaveToJSONFile(FTempFilename);
    finally
      ii.Free;
    end;

    ii := TInstallInfo.Create('');
    try
      // Validate the data
      ii.LoadFromJSONFile(FTempFilename);

      Assert.AreEqual(1, ii.Packages.Count);
      iip := ii.Packages[0];
      Assert.AreEqual(iip_ID, iip.ID);
      Assert.AreEqual(iip_BCP47, iip.BCP47);
      Assert.AreEqual(iip_ShouldInstall, iip.ShouldInstall);

      Assert.AreEqual(1, iip.Locations.Count);
      iipflo := iip.Locations[0];
      Assert.AreEqual(iilOnline, iipflo.LocationType);
      Assert.AreEqual(iipflo_Name, iipflo.Name);

      Assert.AreEqual(2, iipflo.Languages.Count);
      Assert.AreEqual(lang_0_bcp47, iipflo.Languages[0].BCP47);
      Assert.AreEqual(lang_0_name, iipflo.Languages[0].Name);
      Assert.AreEqual(lang_1_bcp47, iipflo.Languages[1].BCP47);
      Assert.AreEqual(lang_1_name, iipflo.Languages[1].Name);

      Assert.AreEqual(iipflo_Size, iipflo.Size);
      Assert.AreEqual(iipflo_Path, iipflo.Path);
      Assert.AreEqual(iipflo_Url, iipflo.Url);
      Assert.AreEqual(iipflo_ProductCode, iipflo.ProductCode);
      Assert.AreEqual(iipflo_VersionWithTag, iipflo.VersionWithTag);
      Assert.AreEqual(iipflo_Version, iipflo.Version);

      Assert.IsTrue(iip.InstallLocation = iip.Locations[0]);


      Assert.AreEqual(ii_EditionTitle, ii.EditionTitle);
      Assert.AreEqual(ii_StartDisabled, ii.StartDisabled);
      Assert.AreEqual(ii_StartWithConfiguration, ii.StartWithConfiguration);
      Assert.AreEqual(ii_ShouldInstallKeyman, ii.ShouldInstallKeyman);
      Assert.AreEqual(ii_Tier, ii.Tier);
      Assert.AreEqual(ii_String, ii.Strings[0]);

      Assert.AreEqual(2, ii.MsiLocations.Count);
      iifl := ii.MsiLocations[0];

      Assert.AreEqual(iifll_Size, iifl.Size);
      Assert.AreEqual(iifll_Path, iifl.Path);
      Assert.AreEqual(iifll_Url, iifl.Url);
      Assert.AreEqual(iifll_ProductCode, iifl.ProductCode);
      Assert.AreEqual(iifll_VersionWithTag, iifl.VersionWithTag);
      Assert.AreEqual(iifll_Version, iifl.Version);

      iifl := ii.MsiLocations[1];
      Assert.AreEqual(iiflo_Size, iifl.Size);
      Assert.AreEqual(iiflo_Path, iifl.Path);
      Assert.AreEqual(iiflo_Url, iifl.Url);
      Assert.AreEqual(iiflo_ProductCode, iifl.ProductCode);
      Assert.AreEqual(iiflo_VersionWithTag, iifl.VersionWithTag);
      Assert.AreEqual(iiflo_Version, iifl.Version);

      Assert.IsTrue(ii.MsiInstallLocation = ii.MsiLocations[0]);

      Assert.AreEqual(FTempFilename, ii.TitleImageFilename);
    finally
      ii.Free;
    end;
  finally
    DeleteFile(FTempFilename);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TInstallInfoTest);
end.
