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

  func testGetKeysForUpdatablePackagesSimple() {
    // Load save-state:  khmer_angkor installed, cached query holds timestamp for 15 Jul 2020.
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Updates.km_base_state)

    // Case 1:  up-to-date query, version is current.
    let khmerAngkorKey = TestUtils.Keyboards.khmer_angkor.packageKey
    guard let timestamp = UserDefaults.standard.cachedPackageQueryResult(forPackageKey: khmerAngkorKey)?.timestampForLastQuery else {
      XCTFail()
      return
    }

    let originalDate = Date.init(timeIntervalSince1970: timestamp)

    downloadManager.unitTestCurrentDate = originalDate
    XCTAssertEqual(downloadManager.getKeysForUpdatablePackages().count, 0)

    // Case 2:  query out of date, version still current.
    let timestampPastThreshold = timestamp + ResourceDownloadManager.DISTRIBUTION_CACHE_VALIDITY_THRESHOLD + 1
    let needsUpdateDate = Date.init(timeIntervalSince1970: timestampPastThreshold)

    downloadManager.unitTestCurrentDate = needsUpdateDate
    XCTAssertEqual(downloadManager.getKeysForUpdatablePackages().count, 0)

    // Now to run it under an actual "updatable" condition.
    // Case 3:  fresh query with available update.
    let mockedVersionIsOutdated = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_km_updated, error: nil) // For Step 5.
    mockedURLSession.queueMockResult(.data(mockedVersionIsOutdated))

    let queryExpectation = XCTestExpectation(description: "Version query should complete successfully.")

    KeymanPackage.queryCurrentVersions(for: [khmerAngkorKey], withSession: mockedURLSession) { _, _ in

      var keys = self.downloadManager.getKeysForUpdatablePackages()
      XCTAssertEqual(keys.count, 1)
      if keys.count > 0 {
        XCTAssertTrue(keys.contains(khmerAngkorKey))
      }

      // Case 4: we now make _this_ fetch look out-of-date.
      guard let package = ResourceFileManager.shared.getInstalledPackage(withKey: khmerAngkorKey),
            let timestamp = package.versionQueryCache.timestampForLastQuery else {
        XCTFail()
        queryExpectation.fulfill()
        return
      }

      let needsUpdateTimestamp = timestamp + ResourceDownloadManager.DISTRIBUTION_CACHE_VALIDITY_THRESHOLD + 1
      self.downloadManager.unitTestCurrentDate = Date(timeIntervalSince1970: needsUpdateTimestamp)

      keys = self.downloadManager.getKeysForUpdatablePackages()
      XCTAssertEqual(keys.count, 1)
      if keys.count > 0 {
        XCTAssertTrue(keys.contains(khmerAngkorKey))
      }

      queryExpectation.fulfill()
    }

    wait(for: [queryExpectation], timeout: 5)
  }

  func testQueryKeysForUpdatablePackagesSimple() {
    // Load save-state:  khmer_angkor installed, cached query holds timestamp for 15 Jul 2020.
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Updates.km_base_state)
    let khmerAngkorKey = TestUtils.Keyboards.khmer_angkor.packageKey

    // Case 1:  fresh query reports 'version is current'.
    let mockedVersionIsCurrent = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_km, error: nil)
    mockedURLSession.queueMockResult(.data(mockedVersionIsCurrent))

    let queryExpectation1 = XCTestExpectation(description: "Version query should complete successfully.")

    downloadManager.queryKeysForUpdatablePackages { results, error in
      guard error == nil, let results = results else {
        XCTFail()
        queryExpectation1.fulfill()
        return
      }

      XCTAssertEqual(results.count, 0)

      queryExpectation1.fulfill()
    }

    // Case 2:  fresh query reports 'version is outdated'.
    let mockedVersionIsOutdated = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_km_updated, error: nil)
    mockedURLSession.queueMockResult(.data(mockedVersionIsOutdated))

    let queryExpectation2 = XCTestExpectation(description: "Version query should complete successfully.")

    downloadManager.queryKeysForUpdatablePackages { results, error in
      guard error == nil, let results = results else {
        XCTFail()
        queryExpectation2.fulfill()
        return
      }
      XCTAssertEqual(results.count, 1)
      if results.count > 0 {
        XCTAssertTrue(results.contains(khmerAngkorKey))
      }

      queryExpectation2.fulfill()
    }

    // Since we're relying on the queries' local parameters, we can safely run them in parallel!
    wait(for: [queryExpectation1, queryExpectation2], timeout: 5)
  }

  func testQueryKeysForUpdatablePackagesPostMigration() throws {
    // Features 7 keyboards and 6 lexical models, most of which use autogenerated kmp.jsons.
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Migrations.cloud_to_kmp_13)
    try Migrations.migrateCloudResourcesToKMPFormat()

    let allPackages = ResourceFileManager.shared.installedPackages

    // Query generated from actual api.keyman.com query on 17 Jul 2020.
    /* Original request URL: https://api.keyman.com/package-version?keyboard=sil_sahu&keyboard=sil_euro_latin&keyboard=balochi_latin&keyboard=sil_cameroon_qwerty&keyboard=fv_sencoten&keyboard=indigenous_nt&model=nrc.en.mtnt&model=nrc.str.sencoten&model=sil.bcc-latn.upp_ptwl1&platform=ios
     */
    let mockedQuery = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_post_migration, error: nil)
    mockedURLSession.queueMockResult(.data(mockedQuery))

    let expectation = XCTestExpectation(description: "package-version query should return cleanly")

    func assertAutogeneratedPackagesWillUpdate(packageSet: Set<KeymanPackage.Key>) {
      // Any "package" with an autogenerated wrapper should ALWAYS be auto-updated if possible.
      // Note:  all installed resources were cloud-distributed at the time that the mocked
      //        query was created.
      allPackages.forEach { package in
        if package.metadata.isAutogeneratedWrapper {
          XCTAssertTrue(packageSet.contains(package.key), "Key for autogenerated package \(package.id) not returned for update")
        }
      }
    }

    XCTAssertFalse(downloadManager.updateCacheIsCurrent)
    downloadManager.queryKeysForUpdatablePackages { keys, error in
      guard error == nil, let keys = keys else {
        XCTFail()
        expectation.fulfill()
        return
      }

      assertAutogeneratedPackagesWillUpdate(packageSet: keys)
      expectation.fulfill()
    }

    wait(for: [expectation], timeout: 5)

    // Now, check to ensure we've cached the results.
    XCTAssertTrue(downloadManager.updateCacheIsCurrent)

    let set = downloadManager.getKeysForUpdatablePackages()
    assertAutogeneratedPackagesWillUpdate(packageSet: set)
  }

  @available(*, deprecated)
  func testGetAvailableUpdatesSimple() {
        // Load save-state:  khmer_angkor installed, cached query holds timestamp for 15 Jul 2020.
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Updates.km_base_state)

    // Case 1:  fresh query reports 'version is current'.
    let mockedVersionIsCurrent = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_km, error: nil)
    mockedURLSession.queueMockResult(.data(mockedVersionIsCurrent))

    let queryExpectation1 = XCTestExpectation(description: "Version query should complete successfully.")

    downloadManager.queryKeysForUpdatablePackages { _, _ in
      queryExpectation1.fulfill()
    }

    wait(for: [queryExpectation1], timeout: 5)

    var updatables = downloadManager.getAvailableUpdates()
    XCTAssertNil(updatables) // When the array would be empty, legacy behavior returned nil instead.

    // Case 2:  fresh query reports 'version is outdated'.
    let mockedVersionIsOutdated = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_km_updated, error: nil)
    mockedURLSession.queueMockResult(.data(mockedVersionIsOutdated))

    let queryExpectation2 = XCTestExpectation(description: "Version query should complete successfully.")

    downloadManager.queryKeysForUpdatablePackages { _, _ in
      queryExpectation2.fulfill()
    }

    // Since we're relying on the queries' local parameters, we can safely run them in parallel!
    wait(for: [queryExpectation2], timeout: 5)

    updatables = downloadManager.getAvailableUpdates()
    guard let array = updatables else {
      XCTAssertNotNil(updatables) // basically, XCTFail, but its documentation is better.
      return
    }

    XCTAssertEqual(array[0].fullID as? FullKeyboardID, TestUtils.Keyboards.khmer_angkor.fullID)
  }

  // TODO:  actual update tests.
}
