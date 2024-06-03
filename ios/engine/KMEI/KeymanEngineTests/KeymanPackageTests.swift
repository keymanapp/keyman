//
//  KeymanPackageTests.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 6/3/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest
@testable import KeymanEngine

class KeymanPackageTests: XCTestCase {
  var mockedURLSession: TestUtils.Downloading.URLSessionMock!

  override func setUp() {
    mockedURLSession = TestUtils.Downloading.URLSessionMock()
  }

  override func tearDownWithError() throws {
    TestUtils.standardTearDown()

    let queueWasCleared = mockedURLSession!.queueIsEmpty
    mockedURLSession = nil

    if !queueWasCleared {
      throw NSError(domain: "Keyman",
                    code: 4,
                    userInfo: [NSLocalizedDescriptionKey: "A test did not fully utilize its queued mock results!"])
    }
  }

  func testKeyboardPackage_extractWithoutKmpExtension_succeeds() throws {
    let cacheDirectory = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
    let khmerPackageZip = cacheDirectory.appendingPathComponent("khmer_angkor.kmp")
    try ResourceFileManager.shared.copyWithOverwrite(from: TestUtils.Keyboards.khmerAngkorKMP, to: khmerPackageZip)

    let destinationFolderURL = cacheDirectory.appendingPathComponent("khmer_angkor")

    do {
      if let kmp = try KeymanPackage.extract(fileUrl: khmerPackageZip, destination: destinationFolderURL) {
        // Run assertions on the package's kmp.info.
        // Assumes the KMP used for testing here has the same kmp.info used for those tests.
        let kmp_json_testcase = KMPJSONTests()
        kmp_json_testcase.kmp_info_khmer_angkor_assertions(kmp.metadata)

        XCTAssertNotNil(kmp as? KeyboardKeymanPackage, "Keyboard KMP test extraction did not yield a keyboard package!")
        XCTAssertTrue(kmp.isKeyboard(), "Keyboard KMP test extraction did not yield a keyboard package!")
        XCTAssertEqual(kmp.id, "khmer_angkor", "Incorrect package ID")
        // extracted ok, test kmp
        XCTAssert(kmp.sourceFolder == destinationFolderURL,
                  "The KMP's reported 'source folder' should match the specified destination folder")
      } else {
        XCTAssert(false, "KeymanPackage.extract failed")
      }
    } catch {
      XCTFail("KeymanPackage.extract failed with error \(error)")
    }
  }

  func testKeyboardPackage_clearNonexistentDirectory_doesNothing() throws {
    let cacheDirectory = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
    let destinationDirectory = cacheDirectory.appendingPathComponent("doesnotexist")

    do {
      // clear directory
      try KeymanPackage.clearDirectory(destination: destinationDirectory)
    } catch {
      XCTFail("error clearing the nonexistent directory \(error)")
    }
  }

  func testKeyboardPackage_clearEmptyDirectory_throwsNoError() throws {
    let cacheDirectory = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
    let destinationDirectory = cacheDirectory.appendingPathComponent("destination")

    do {
      // create directory
      try FileManager.default.createDirectory(
        atPath: destinationDirectory.path,
        withIntermediateDirectories: false,
        attributes: nil
      )

      // clear directory
      try KeymanPackage.clearDirectory(destination: destinationDirectory)
    } catch {
      XCTFail("error clearing the empty directory \(error)")
    }
    
    let fileArray = try FileManager.default.contentsOfDirectory(atPath: destinationDirectory.path)
    XCTAssert(fileArray.count == 0, "directory still contains \(fileArray.count) items")
  }

  func testKeyboardPackage_clearNonEmptyDirectory_directoryIsEmpty() throws {
    let cacheDirectory = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
    let destinationDirectory = cacheDirectory.appendingPathComponent("destination")

    do {
      // create directory
      try FileManager.default.createDirectory(
        atPath: destinationDirectory.path,
        withIntermediateDirectories: false,
        attributes: nil
      )

      // add some files
      FileManager.default.createFile(atPath: destinationDirectory.appendingPathComponent("fileone").path, contents: nil)
      FileManager.default.createFile(atPath: destinationDirectory.appendingPathComponent("filetwo").path, contents: nil)
      FileManager.default.createFile(atPath: destinationDirectory.appendingPathComponent("filethree").path, contents: nil)

      // clear directory
      try KeymanPackage.clearDirectory(destination: destinationDirectory)
    } catch {
      XCTFail("error clearing the empty directory \(error)")
    }
    
    let fileArray = try FileManager.default.contentsOfDirectory(atPath: destinationDirectory.path)
    XCTAssert(fileArray.count == 0, "directory still contains \(fileArray.count) items")
  }

  func testKeyboardPackage_extractTwice_noDuplicateFileError() throws {
    let cacheDirectory = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
    let khmerPackageZip = cacheDirectory.appendingPathComponent("khmer_angkor.kmp")
    try ResourceFileManager.shared.copyWithOverwrite(from: TestUtils.Keyboards.khmerAngkorKMP, to: khmerPackageZip)

    let destinationFolderURL = cacheDirectory.appendingPathComponent("khmer_angkor")

    do {
      if let kmp = try KeymanPackage.extract(fileUrl: khmerPackageZip, destination: destinationFolderURL) {
        do {
          // clear directory before second extract
          try KeymanPackage.clearDirectory(destination: destinationFolderURL)
          
          if let secondKmp = try KeymanPackage.extract(fileUrl: khmerPackageZip, destination: destinationFolderURL) {
          } else {
            XCTAssert(false, "*** second unzip failed")
          }
        } catch {
          XCTFail("unzip 2 failure with error \(error)")
        }
      } else {
        XCTAssert(false, "*** first unzip failed")
      }
    } catch {
      XCTFail("unzip 1 failure with error \(error)")
    }
  }

  func testLexicalModelPackageExtraction() throws {
    let cacheDirectory = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
    let mtntZip = cacheDirectory.appendingPathComponent("nrc.en.mtnt.kmp")
    try ResourceFileManager.shared.copyWithOverwrite(from: TestUtils.LexicalModels.mtntKMP, to: mtntZip)

    let destinationFolderURL = cacheDirectory.appendingPathComponent("nrc.en.mtnt.model")

    do {
      if let kmp = try KeymanPackage.extract(fileUrl: mtntZip, destination: destinationFolderURL) {
        // Run assertions on the package's kmp.info.
        // Assumes the KMP used for testing here has the same kmp.info used for those tests.
        let kmp_json_testcase = KMPJSONTests()

        // As this test takes place after construction of the LexicalModelPackage,
        // the version will be set accordingly, unlike in the other JSON-related tests.
        kmp_json_testcase.kmp_info_nrc_en_mtnt_assertions(kmp.metadata, version: "0.1.4")

        XCTAssertNotNil(kmp as? LexicalModelKeymanPackage, "Lexical model KMP test extraction yielded a keyboard package!")
        XCTAssertTrue(!kmp.isKeyboard(), "Lexical model KMP test extraction yielded a keyboard package!")
        XCTAssertEqual(kmp.id, "nrc.en.mtnt")

        // extracted ok, test kmp
        XCTAssert(kmp.sourceFolder == destinationFolderURL,
                  "The KMP's reported 'source folder' should match the specified destination folder")
      } else {
        XCTAssert(false, "KeymanPackage.extract failed")
      }
    } catch {
      XCTFail("KeymanPackage.extract failed with error \(error)")
    }
  }

  func testPackageFindResourceMatch() throws {
    guard let kmp1 = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.khmerAngkorKMP) as? KeyboardKeymanPackage else {
      XCTFail("Incorrect package type loaded for test")
      return
    }
    let kbd = kmp1.findResource(withID: TestUtils.Keyboards.khmer_angkor.fullID)
    XCTAssertNotNil(kbd)
    XCTAssertEqual(kbd?.packageID, "khmer_angkor", "Keyboard package ID not properly set")
    // This keyboard's not in the specified testing package.
    XCTAssertNil(kmp1.findResource(withID: TestUtils.Keyboards.khmer10.fullID))

    // Thanks to our package typing hierarchy, it's impossible to even TRY finding
    // a FullLexicalModelID within a KeyboardKeymanPackage!

    guard let kmp2 = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.LexicalModels.mtntKMP) as? LexicalModelKeymanPackage else {
      XCTFail("Incorrect package type loaded for test")
      return
    }
    let lm = kmp2.findResource(withID: TestUtils.LexicalModels.mtnt.fullID)
    XCTAssertNotNil(lm)
    XCTAssertEqual(lm?.packageID, "nrc.en.mtnt", "Lexical model ID not properly set")

    // Thanks to our package typing hierarchy, it's impossible to even TRY finding
    // a FullKeyboardID within a LexicalModelKeymanPackage!
  }

  func testInstalledPackageDeinit() throws {
    let kmp = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.khmerAngkorKMP)
    XCTAssertNotNil(kmp, "Failed to prepare KMP for installation")
    XCTAssertNotNil(kmp as? KeyboardKeymanPackage, "KMP resource type improperly recognized - expected a keyboard package!")

    try ResourceFileManager.shared.finalizePackageInstall(kmp, isCustom: true)

    let installedDir = Storage.active.resourceDir(for: TestUtils.Keyboards.khmer_angkor)!
    let installedURL = Storage.active.resourceURL(for: TestUtils.Keyboards.khmer_angkor)!

    // Intentionally scopes the `package` variable
    do {
      let package = try KeymanPackage.parse(installedDir)!
      XCTAssertTrue(FileManager.default.fileExists(atPath: installedURL.path))
      XCTAssertEqual(package.id, "khmer_angkor")
    }

    // Deinit should have triggered - is everything still in place?
    XCTAssertTrue(FileManager.default.fileExists(atPath: installedURL.path))
    XCTAssertTrue(FileManager.default.fileExists(atPath: installedDir.path))
  }

  func testTempPackageDeinit() throws {
    var tempDir: URL

    // Intentionally scopes the `package` variable.
    do {
      let package = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.khmerAngkorKMP)
      XCTAssertNotNil(package, "Failed to prepare KMP for installation")
      XCTAssertNotNil(package as? KeyboardKeymanPackage, "KMP resource type improperly recognized - expected a keyboard package!")

      tempDir = package.sourceFolder

      XCTAssertTrue(FileManager.default.fileExists(atPath: tempDir.path))
    }

    // Deinit should have triggered - were the files automatically cleaned up?
    XCTAssertFalse(FileManager.default.fileExists(atPath: tempDir.path))
  }

  // Analogous to QueryPackageVersionTests.testMockedBatchFetchParse, but with more analysis applied
  // and more integration.
  func testQueryDistributionStates() throws {
    let expectation = XCTestExpectation(description: "The query completes as expected.")

    // Test setup

    let mockedResult = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_case_1, error: nil)
    mockedURLSession!.queueMockResult(.data(mockedResult))

    let badKbdKey = KeymanPackage.Key(id: "foo", type: .keyboard)
    let badLexKey = KeymanPackage.Key(id: "bar", type: .lexicalModel)
    let packageKeys = [KeymanPackage.Key(forResource: TestUtils.Keyboards.khmer_angkor),
                       KeymanPackage.Key(forResource: TestUtils.Keyboards.sil_euro_latin),
                       KeymanPackage.Key(id: "foo", type: .keyboard),
                       KeymanPackage.Key(forResource: TestUtils.LexicalModels.mtnt),
                       KeymanPackage.Key(id: "bar", type: .lexicalModel)]

    KeymanPackage.queryDistributionStates(for: packageKeys, withSession: mockedURLSession!) { results, error in
      guard error == nil, let results = results else {
        XCTFail()
        expectation.fulfill()
        return
      }

      let khmer_angkor = KeymanPackage.Key(forResource: TestUtils.Keyboards.khmer_angkor)
      let sil_euro_latin = KeymanPackage.Key(forResource: TestUtils.Keyboards.sil_euro_latin)
      let mtnt = KeymanPackage.Key(forResource: TestUtils.LexicalModels.mtnt)
      XCTAssertEqual(results[khmer_angkor]?.latestVersion, "1.0.6")
      XCTAssertEqual(results[khmer_angkor]?.distributionMethod, .cloud)

      XCTAssertEqual(results[sil_euro_latin]?.latestVersion, "1.9.1")
      XCTAssertEqual(results[sil_euro_latin]?.distributionMethod, .cloud)

      XCTAssertEqual(results[mtnt]?.latestVersion, "0.1.4")
      XCTAssertEqual(results[mtnt]?.distributionMethod, .cloud)

      XCTAssertNotNil(results[badKbdKey])
      XCTAssertEqual(results[badKbdKey]?.distributionMethod, .custom)

      XCTAssertNotNil(results[badLexKey])
      XCTAssertEqual(results[badLexKey]?.distributionMethod, .custom)

      // Test that this metadata is stored and persisted (cached) in UserDefaults.
      let userDefaults = Storage.active.userDefaults

      let cache_khmer_angkor = userDefaults.cachedPackageQueryResult(forPackageKey: khmer_angkor)
      XCTAssertNotNil(cache_khmer_angkor)
      XCTAssertEqual(cache_khmer_angkor?.distributionMethod, results[khmer_angkor]?.distributionMethod)
      XCTAssertEqual(cache_khmer_angkor?.latestVersion, "1.0.6")
      XCTAssertEqual(cache_khmer_angkor?.timestampForLastQuery, results[khmer_angkor]?.timestampForLastQuery)

      let cache_mtnt = userDefaults.cachedPackageQueryResult(forPackageKey: mtnt)
      XCTAssertNotNil(cache_mtnt)
      XCTAssertEqual(cache_mtnt?.distributionMethod, results[mtnt]?.distributionMethod)
      XCTAssertEqual(cache_mtnt?.latestVersion, "0.1.4")
      XCTAssertEqual(cache_mtnt?.timestampForLastQuery, results[mtnt]?.timestampForLastQuery)

      let cache_foo = userDefaults.cachedPackageQueryResult(forPackageKey: badKbdKey)
      XCTAssertNotNil(cache_foo)
      XCTAssertEqual(cache_foo?.distributionMethod, results[badKbdKey]?.distributionMethod)
      XCTAssertNil(cache_foo?.latestVersion)

      expectation.fulfill()
    }

    wait(for: [expectation], timeout: 5)
  }

  // Analogous to QueryPackageVersionTests.testMockedBatchFetchParse, but with more analysis applied
  // and more integration.
  func testQueryCurrentVersions() throws {
    let mockedURLSession = TestUtils.Downloading.URLSessionMock()

    let expectation = XCTestExpectation(description: "The query completes as expected.")

    // Test setup

    let mockedResult = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_case_1, error: nil)
    mockedURLSession.queueMockResult(.data(mockedResult))

    let badKbdKey = KeymanPackage.Key(id: "foo", type: .keyboard)
    let badLexKey = KeymanPackage.Key(id: "bar", type: .lexicalModel)
    let packageKeys = [KeymanPackage.Key(forResource: TestUtils.Keyboards.khmer_angkor),
                       KeymanPackage.Key(forResource: TestUtils.Keyboards.sil_euro_latin),
                       KeymanPackage.Key(id: "foo", type: .keyboard),
                       KeymanPackage.Key(forResource: TestUtils.LexicalModels.mtnt),
                       KeymanPackage.Key(id: "bar", type: .lexicalModel)]

    KeymanPackage.queryCurrentVersions(for: packageKeys, withSession: mockedURLSession) { results, error in
      guard error == nil, let results = results else {
        XCTFail()
        expectation.fulfill()
        return
      }

      let khmer_angkor = KeymanPackage.Key(forResource: TestUtils.Keyboards.khmer_angkor)
      let sil_euro_latin = KeymanPackage.Key(forResource: TestUtils.Keyboards.sil_euro_latin)
      let mtnt = KeymanPackage.Key(forResource: TestUtils.LexicalModels.mtnt)
      XCTAssertEqual(results[khmer_angkor], Version("1.0.6"))
      XCTAssertEqual(results[sil_euro_latin], Version("1.9.1"))
      XCTAssertEqual(results[mtnt], Version("0.1.4"))
      XCTAssertNil(results[badKbdKey])
      XCTAssertNil(results[badLexKey])

      expectation.fulfill()
    }

    wait(for: [expectation], timeout: 5)

    // Post-execution cleanup
    let queueWasCleared = mockedURLSession.queueIsEmpty

    if !queueWasCleared {
      throw NSError(domain: "Keyman",
                    code: 4,
                    userInfo: [NSLocalizedDescriptionKey: "A test did not fully utilize its queued mock results!"])
    }
  }

  func testVersionState() throws {
    // Step 1:  Install khmer_angkor.  Fixture:  version 1.0.6.
    guard let installPackage = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.khmerAngkorKMP) as? KeyboardKeymanPackage else {
      XCTFail("Could not load keyboard KMP for test")
      return
    }

    try ResourceFileManager.shared.install(resourceWithID: TestUtils.Keyboards.khmer_angkor.fullID, from: installPackage)

    // Step 2:  retrieve the installed version of the package.  We're currently using the temp version.
    guard let package = ResourceFileManager.shared.getInstalledPackage(withKey: installPackage.key) else {
      XCTFail("Could not load installed form of keyboard KMP for test")
      return
    }

    // No query has yet occurred - version state unknown.
    XCTAssertEqual(package.versionState, .unknown)

    // Step 3:  Prepare mocking for two separate query rounds.
    let mockedVersionIsCurrent = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_km, error: nil)  // For Step 4.
    let mockedVersionIsOutdated = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_km_updated, error: nil) // For Step 5.
    mockedURLSession.queueMockResult(.data(mockedVersionIsCurrent))
    mockedURLSession.queueMockResult(.data(mockedVersionIsOutdated))

    // Step 4:  Mocked package-version query returns the same version as we installed.
    let expectationCurrent = XCTestExpectation(description: "Query matching 'current version' completed successfully.")

    KeymanPackage.queryCurrentVersions(for: [package.key], withSession: mockedURLSession) { _, _ in
      expectationCurrent.fulfill()
    }

    wait(for: [expectationCurrent], timeout: 5)
    XCTAssertEqual(package.versionState, .upToDate)

    // Step 5:  Mocked package-version query returns an updated version compared to what is installed.
    let expectationUpdate = XCTestExpectation(description: "Query matching 'update available' completed successfully.")

    KeymanPackage.queryCurrentVersions(for: [package.key], withSession: mockedURLSession) { _, _ in
      expectationUpdate.fulfill()
    }

    wait(for: [expectationUpdate], timeout: 5)
    XCTAssertEqual(package.versionState, .needsUpdate)
  }

  func testInstallState() throws {
    // Step 1:  Install khmer_angkor.  Fixture:  version 1.0.6.
    guard let installPackage = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.khmerAngkorKMP) as? KeyboardKeymanPackage else {
      XCTFail("Could not load keyboard KMP for test")
      return
    }

    XCTAssertEqual(installPackage.installState, .pending)

    try ResourceFileManager.shared.install(resourceWithID: TestUtils.Keyboards.khmer_angkor.fullID, from: installPackage)

    // Step 2:  retrieve an instance for the installation.  We're currently using the temp version.
    guard let package = ResourceFileManager.shared.getInstalledPackage(withKey: installPackage.key) else {
      XCTFail("Could not load installed form of keyboard KMP for test")
      return
    }

    XCTAssertEqual(installPackage.installState, .pending) // Reflects that the instance is a temporary extraction.
    XCTAssertEqual(package.installState, .installed) // Reflects the actually-installed package
  }
}
