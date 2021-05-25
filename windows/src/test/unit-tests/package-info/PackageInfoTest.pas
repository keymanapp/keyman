unit PackageInfoTest;

interface

uses
  DUnitX.TestFramework,

  PackageInfo,
  kpsfile;

type
  [TestFixture]
  TPackageInfoTest = class(TObject)
  private
    DataPath: string;
    procedure DoCompare(f1, f2: string);
    procedure DoCompareKpsFiles(f1, f2: string; k1, k2: TKpsFile);

  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestXMLInOut;

    [Test]
    procedure TestJSONInOut;

    [Test]
    procedure TestRoundTrip;

    [Test]
    procedure TestRoundTripInf;

    [Test]
    procedure TestInfInOut;

    [Test]
    procedure TestAssign;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Win.ComObj,
  Winapi.ActiveX,
  Xml.XmlDoc,
  Xml.XmlIntf;


procedure TPackageInfoTest.Setup;
begin
  Assert.IgnoreCaseDefault := False;
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  DataPath := ExtractFilePath(ParamStr(0)) + '..\..\test\';
end;

procedure TPackageInfoTest.TearDown;
begin
  CoUninitialize;
end;

procedure TPackageInfoTest.TestAssign;
var
  p1, p2: TPackage;
begin
  p1 := TKpsFile.Create;
  p2 := TKpsFile.Create;
  try
    p1.FileName := DataPath + 'test.xml';
    p1.LoadXML;
    p2.Assign(p1);
    p2.FileName := DataPath + 'testAssign.out.xml';
    p2.SaveXML;
  finally
    p1.Free;
    p2.Free;
  end;

  DoCompare(DataPath + 'test.xml', DataPath + 'testAssign.out.xml');
end;

procedure TPackageInfoTest.TestInfInOut;
var
  p1: TPackage;
begin
  p1 := TKpsFile.Create;
  try
    p1.FileName := DataPath + 'test.inf';
    p1.LoadIni;
    p1.FileName := DataPath + 'test1.out.inf';
    p1.SaveIni;
  finally
    p1.Free;
  end;

  DoCompare(DataPath + 'test.inf', DataPath + 'test1.out.inf');
end;

procedure TPackageInfoTest.TestRoundTrip;
var
  p1: TPackage;
begin
  p1 := TKpsFile.Create;
  try
    p1.FileName := DataPath + 'test.xml';
    p1.LoadXML;
    p1.FileName := DataPath + 'test2.out.inf';
    p1.SaveIni;
  finally
    p1.Free;
  end;

  p1 := TKpsFile.Create;
  try
    p1.FileName := DataPath + 'test2.out.inf';
    p1.LoadIni;
    p1.FileName := DataPath + 'test2.out.json';
    p1.SaveJSON;
  finally
    p1.Free;
  end;

  p1 := TKpsFile.Create;
  try
    p1.FileName := DataPath + 'test2.out.json';
    p1.LoadJSON;
    p1.FileName := DataPath + 'test2.out.xml';
    p1.SaveXML;
  finally
    p1.Free;
  end;

  DoCompare(DataPath + 'test.xml', DataPath + 'test2.out.xml');
end;

procedure TPackageInfoTest.TestRoundTripInf;
var
  p1: TPackage;
begin
  p1 := TKpsFile.Create;
  try
    p1.FileName := DataPath + 'test.inf';
    p1.LoadIni;
    p1.FileName := DataPath + 'test3.out.xml';
    p1.SaveXML;
  finally
    p1.Free;
  end;

  p1 := TKpsFile.Create;
  try
    p1.FileName := DataPath + 'test3.out.xml';
    p1.LoadXML;
    p1.FileName := DataPath + 'test3.out.json';
    p1.SaveJSON;
  finally
    p1.Free;
  end;

  p1 := TKpsFile.Create;
  try
    p1.FileName := DataPath + 'test3.out.json';
    p1.LoadJSON;
    p1.FileName := DataPath + 'test3.out.inf';
    p1.SaveIni;
  finally
    p1.Free;
  end;

  DoCompare(DataPath + 'test.inf', DataPath + 'test3.out.inf');
end;

procedure TPackageInfoTest.TestXMLInOut;
var
  p1: TPackage;
begin
  p1 := TKpsFile.Create;
  try
    p1.FileName := DataPath + 'test.xml';
    p1.LoadXML;
    p1.FileName := DataPath + 'test4.out.xml';
    p1.SaveXML;
  finally
    p1.Free;
  end;

  DoCompare(DataPath + 'test.xml', DataPath + 'test4.out.xml');
end;

procedure TPackageInfoTest.TestJSONInOut;
var
  p1: TPackage;
begin
  p1 := TKpsFile.Create;
  try
    p1.FileName := DataPath + 'test.json';
    p1.LoadJSON;
    p1.FileName := DataPath + 'test5.out.json';
    p1.SaveJSON;
  finally
    p1.Free;
  end;

  DoCompare(DataPath + 'test.json', DataPath + 'test5.out.json');
end;

procedure TPackageInfoTest.DoCompare(f1, f2: string);
var
  k1, k2: TKpsFile;
begin
  k1 := TKpsFile.Create;
  k2 := TKpsFile.Create;
  try
    k1.FileName := f1;
    if SameText(ExtractFileExt(f1), '.xml') then k1.LoadXml
    else if SameText(ExtractFileExt(f1), '.json') then k1.LoadJSON
    else k1.LoadIni;

    k2.FileName := f2;
    if SameText(ExtractFileExt(f2), '.xml') then k2.LoadXml
    else if SameText(ExtractFileExt(f2), '.json') then k2.LoadJSON
    else k2.LoadIni;

    DoCompareKpsFiles(f1, f2, k1, k2);
  finally
    k1.Free;
    k2.Free;
  end;
end;

procedure TPackageInfoTest.DoCompareKpsFiles(f1, f2: string; k1, k2: TKpsFile);
var
  i: Integer;
  t: TPackageInfoEntryType;
begin
  Assert.AreEqual(k1.Options.FileVersion, k2.Options.FileVersion);
  Assert.AreEqual(k1.Options.ExecuteProgram, k2.Options.ExecuteProgram);
  Assert.IsTrue(
    (
      Assigned(k1.Options.ReadmeFile) and
      Assigned(k2.Options.ReadmeFile) and
      (k1.Options.ReadmeFile.FileName = k2.Options.ReadmeFile.FileName)
    ) or (
      not Assigned(k1.Options.ReadmeFile) and
      not Assigned(k2.Options.ReadmeFile)
    ),
    'ReadmeFile does not match for '+f1+','+f2);
  Assert.IsTrue(
    (
      Assigned(k1.Options.GraphicFile) and
      Assigned(k2.Options.GraphicFile) and
      (k1.Options.GraphicFile.FileName = k2.Options.GraphicFile.FileName)
    ) or (
      not Assigned(k1.Options.GraphicFile) and
      not Assigned(k2.Options.GraphicFile)
    ),
    'GraphicFile does not match for '+f1+','+f2);

  Assert.AreEqual(k1.Files.Count, k2.Files.Count);
  for i := 0 to k1.Files.Count - 1 do
  begin
    Assert.AreEqual(k1.Files[i].FileName, k2.Files.FromFileNameEx(k1.Files[i].FileName).FileName);
    Assert.AreEqual(k1.Files[i].Description, k2.Files.FromFileNameEx(k1.Files[i].FileName).Description);
  end;

  for t := Low(TPackageInfoEntryType) to High(TPackageInfoEntryType) do
  begin
    Assert.AreEqual(k1.Info.Desc[PackageInfoEntryTypeNames[t]], k2.Info.Desc[PackageInfoEntryTypeNames[t]]);
    Assert.AreEqual(k1.Info.URL[PackageInfoEntryTypeNames[t]], k2.Info.URL[PackageInfoEntryTypeNames[t]]);
  end;

  Assert.AreEqual(k1.Keyboards.Count, k2.Keyboards.Count);
  for i := 0 to k1.Keyboards.Count - 1 do
  begin
    Assert.AreEqual(k1.Keyboards[i].Name, k2.Keyboards[i].Name);
    Assert.AreEqual(k1.Keyboards[i].ID, k2.Keyboards[i].ID);
    Assert.AreEqual(k1.Keyboards[i].Version, k2.Keyboards[i].Version);
    Assert.IsTrue(
      (
        Assigned(k1.Keyboards[i].OSKFont) and
        Assigned(k2.Keyboards[i].OSKFont) and
       (k1.Keyboards[i].OSKFont.FileName = k2.Keyboards[i].OSKFont.FileName)
      ) or (
        not Assigned(k1.Keyboards[i].OSKFont) and
        not Assigned(k2.Keyboards[i].OSKFont)
      )
    );
  end;

  Assert.AreEqual(k1.Strings.Count, k2.Strings.Count);
  for i := 0 to k1.Strings.Count - 1 do
    Assert.AreEqual(k1.Strings[i], k2.Strings[i]);

  Assert.AreEqual(k1.StartMenu.Path, k2.StartMenu.Path);
  Assert.AreEqual(k1.StartMenu.DoCreate, k2.StartMenu.DoCreate);
  Assert.AreEqual(k1.StartMenu.AddUninstallEntry, k2.StartMenu.AddUninstallEntry);
  Assert.AreEqual(k1.StartMenu.Entries.Count, k2.StartMenu.Entries.Count);
  for i := 0 to k1.StartMenu.Entries.Count - 1 do
  begin
    Assert.AreEqual(k1.StartMenu.Entries[i].Name, k2.StartMenu.Entries[i].Name);
    Assert.AreEqual(k1.StartMenu.Entries[i].Prog, k2.StartMenu.Entries[i].Prog);
    Assert.AreEqual(k1.StartMenu.Entries[i].Params, k2.StartMenu.Entries[i].Params);
    Assert.AreEqual(k1.StartMenu.Entries[i].Icon, k2.StartMenu.Entries[i].Icon);
    Assert.AreEqual(k1.StartMenu.Entries[i].Location, k2.StartMenu.Entries[i].Location);
  end;

  Assert.AreEqual(k1.KPSOptions.MSIFileName, k2.KPSOptions.MSIFileName);
  Assert.AreEqual(k1.KPSOptions.MSIOptions, k2.KPSOptions.MSIOptions);
end;


initialization
  TDUnitX.RegisterTestFixture(TPackageInfoTest);
end.
