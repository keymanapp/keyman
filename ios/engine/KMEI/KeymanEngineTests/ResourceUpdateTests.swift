//
//  ResourceUpdateTests.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 7/15/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest
@testable import KeymanEngine

class ResourceUpdateTests: XCTestCase {
  var mockedURLSession: TestUtils.Downloading.URLSessionMock!
  var downloadManager: ResourceDownloadManager!

  override func setUp() {
    TestUtils.setupAndDeinitManager()

    mockedURLSession = TestUtils.Downloading.URLSessionMock()
    downloadManager = ResourceDownloadManager(session: mockedURLSession!, autoExecute: false)
  }

  override func tearDownWithError() throws {
    // Resets resource directories for a clean slate.
    TestUtils.standardTearDown()

    let queueWasCleared = mockedURLSession!.queueIsEmpty
    mockedURLSession = nil

    if !queueWasCleared {
      throw NSError(domain: "Keyman",
                    code: 4,
                    userInfo: [NSLocalizedDescriptionKey: "A test did not fully utilize its queued mock results!"])
    }
  }

  // Designed for use in a 'temporary' test to produce test fixtures for ResourceUpdateTests.
  // Will receive an XCTAttachment with the generated bundle in .zip form.
  func setupUpdateCacheValidityTest() throws {
    // Install a package
    guard let kh_package = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.khmerAngkorKMP) as? KeyboardKeymanPackage else {
      XCTFail()
      return
    }

    try ResourceFileManager.shared.install(resourceWithID: TestUtils.Keyboards.khmer_angkor.fullID, from: kh_package)

    // Set up package-version query mocking.
    let mockedResult = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_km, error: nil)
    mockedURLSession.queueMockResult(.data(mockedResult))

    let queryExpectation = XCTestExpectation(description: "Package-version query completed.")
    // Run a package-version query on it.
    KeymanPackage.queryCurrentVersions(for: [TestUtils.Keyboards.khmer_angkor.packageKey], withSession: mockedURLSession) { _, _ in
      queryExpectation.fulfill()
    }

    wait(for: [queryExpectation], timeout: 5)

    // Uses the XCTest 'attachment' system to retrieve the desired file.
    self.add(try TestUtils.EngineStateBundler.createBundle(withName: "khmer_angkor update base"))

    log.info("Bundle archived and attached to test's report.")
  }

  func testCacheCurrent() {
    // Load save-state:  khmer_angkor installed, cached query holds timestamp for 15 Jul 2020.
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Updates.km_base_state)

    let khmerAngkorKey = TestUtils.Keyboards.khmer_angkor.packageKey
    guard let timestamp = UserDefaults.standard.cachedPackageQueryResult(forPackageKey: khmerAngkorKey)?.timestampForLastQuery else {
      XCTFail()
      return
    }

    let originalDate = Date.init(timeIntervalSince1970: timestamp)

    downloadManager.unitTestCurrentDate = originalDate
    XCTAssertTrue(downloadManager.updateCacheIsCurrent)

    let timestampPastThreshold = timestamp + ResourceDownloadManager.DISTRIBUTION_CACHE_VALIDITY_THRESHOLD + 1
    let needsUpdateDate = Date.init(timeIntervalSince1970: timestampPastThreshold)

    downloadManager.unitTestCurrentDate = needsUpdateDate
    XCTAssertFalse(downloadManager.updateCacheIsCurrent)
  }
}
