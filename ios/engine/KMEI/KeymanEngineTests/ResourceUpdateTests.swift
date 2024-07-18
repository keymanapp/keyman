//
//  ResourceUpdateTests.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 7/15/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest
import os.log
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

    os_log("Bundle archived and attached to test's report.", log:KeymanEngineLogger.resources, type: .info)
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

  func testPerformBatchUpdateSimple() {
    // Load save-state:  khmer_angkor installed, cached query holds timestamp for 15 Jul 2020.
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Updates.km_base_state)
    let khmerAngkorKey = TestUtils.Keyboards.khmer_angkor.packageKey

    // Establish the query's cache.
    let mockedVersionIsOutdated = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_km_updated, error: nil)
    let mockedKeyboardUpdate = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: nil)
    mockedURLSession.queueMockResult(.data(mockedVersionIsOutdated))
    mockedURLSession.queueMockResult(.download(mockedKeyboardUpdate))

    // For this test, it's easier to let the queue automatically run.
    downloadManager.downloader.autoExecute = true

    let queryExpectation = XCTestExpectation(description: "Version query should complete successfully.")
    let updateExpectation = XCTestExpectation(description: "The keyboard update should complete.")

    downloadManager.queryKeysForUpdatablePackages { results, error in
      guard error == nil, let results = results else {
        XCTFail()
        queryExpectation.fulfill()
        return
      }
      XCTAssertEqual(results.count, 1)
      if results.count > 0 {
        XCTAssertTrue(results.contains(khmerAngkorKey))
      }

      queryExpectation.fulfill()

      self.downloadManager.performBatchUpdate(forPackageKeys: results, withNotifications: false) { successes, failures in

        if successes.count == 1 {
          XCTAssertEqual(successes[0], khmerAngkorKey)
        } else {
          XCTAssertEqual(successes.count, 1)
        }

        updateExpectation.fulfill()
      }
    }

    // Since we're relying on the queries' local parameters, we can safely run them in parallel!
    wait(for: [queryExpectation, updateExpectation], timeout: 5)
  }

  func testPerformBatchUpdateSimpleWithDownloadError() {
    // Load save-state:  khmer_angkor installed, cached query holds timestamp for 15 Jul 2020.
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Updates.km_base_state)
    let khmerAngkorKey = TestUtils.Keyboards.khmer_angkor.packageKey

    // Establish the query's cache.
    let mockedVersionIsOutdated = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_km_updated, error: nil)
    let mockedKeyboardUpdate = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: NSError(domain: "Keyman", code: 0, userInfo: nil))
    mockedURLSession.queueMockResult(.data(mockedVersionIsOutdated))
    mockedURLSession.queueMockResult(.download(mockedKeyboardUpdate))

    // For this test, it's easier to let the queue automatically run.
    downloadManager.downloader.autoExecute = true

    let queryExpectation = XCTestExpectation(description: "Version query should complete successfully.")
    let updateExpectation = XCTestExpectation(description: "The keyboard update should complete.")

    downloadManager.queryKeysForUpdatablePackages { results, error in
      guard error == nil, let results = results else {
        XCTFail()
        queryExpectation.fulfill()
        return
      }
      XCTAssertEqual(results.count, 1)
      if results.count > 0 {
        XCTAssertTrue(results.contains(khmerAngkorKey))
      }

      queryExpectation.fulfill()

      self.downloadManager.performBatchUpdate(forPackageKeys: [khmerAngkorKey], withNotifications: false) { successes, failures in

        if failures.count == 1 {
          XCTAssertEqual(failures[0].0, khmerAngkorKey)
        } else {
          XCTAssertEqual(failures.count, 1)
        }

        updateExpectation.fulfill()
      }
    }

    // Since we're relying on the queries' local parameters, we can safely run them in parallel!
    wait(for: [queryExpectation, updateExpectation], timeout: 5)
  }

  func testPerformBatchUpdateSimpleWithQueryError() {
    // Load save-state:  khmer_angkor installed, cached query holds timestamp for 15 Jul 2020.
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Updates.km_base_state)
    let khmerAngkorKey = TestUtils.Keyboards.khmer_angkor.packageKey

    // Establish the query's cache.
    let mockedVersionIsOutdated = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_km_updated, error: NSError(domain: "Keyman", code: 0, userInfo: nil))
    mockedURLSession.queueMockResult(.data(mockedVersionIsOutdated))

    // For this test, it's easier to let the queue automatically run.
    downloadManager.downloader.autoExecute = true

    let queryExpectation = XCTestExpectation(description: "Version query should complete successfully.")
    let updateExpectation = XCTestExpectation(description: "The keyboard update should complete.")

    downloadManager.queryKeysForUpdatablePackages { results, error in
      guard let _ = error, results == nil else {
        XCTFail()
        queryExpectation.fulfill()
        return
      }

      queryExpectation.fulfill()

      self.downloadManager.performBatchUpdate(forPackageKeys: [khmerAngkorKey], withNotifications: false) { successes, failures in

        if failures.count == 1 {
          XCTAssertEqual(failures[0].0, khmerAngkorKey)
        } else {
          XCTAssertEqual(failures.count, 1)
        }

        updateExpectation.fulfill()
      }
    }

    // Since we're relying on the queries' local parameters, we can safely run them in parallel!
    wait(for: [queryExpectation, updateExpectation], timeout: 5)
  }

  func testPerformBatchUpdateInvalidKey() {
    // Load save-state:  khmer_angkor installed, cached query holds timestamp for 15 Jul 2020.
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Updates.km_base_state)
    let badKey = KeymanPackage.Key(id: "foo", type: .keyboard)

    let updateExpectation = XCTestExpectation(description: "The keyboard update should complete.")

    self.downloadManager.performBatchUpdate(forPackageKeys: [badKey], withNotifications: false) { successes, failures in

      if failures.count == 1 {
        XCTAssertEqual(failures[0].0, badKey)
      } else {
        XCTAssertEqual(failures.count, 1)
      }

      updateExpectation.fulfill()
    }

    // Since we're relying on the queries' local parameters, we can safely run them in parallel!
    wait(for: [updateExpectation], timeout: 5)
  }

  func testPerformBatchUpdatePostMigration() throws {
    // Features 7 keyboards and 6 lexical models, most of which use autogenerated kmp.jsons.
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Migrations.cloud_to_kmp_13)
    try Migrations.migrateCloudResourcesToKMPFormat()

    // Step 1 - get the package-version query results in place.

    // Query generated from actual api.keyman.com query on 17 Jul 2020.
    /* Original request URL: https://api.keyman.com/package-version?keyboard=sil_sahu&keyboard=sil_euro_latin&keyboard=balochi_latin&keyboard=sil_cameroon_qwerty&keyboard=fv_sencoten&keyboard=indigenous_nt&model=nrc.en.mtnt&model=nrc.str.sencoten&model=sil.bcc-latn.upp_ptwl1&platform=ios
     */
    let mockedQuery = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_post_migration, error: nil)
    mockedURLSession.queueMockResult(.data(mockedQuery))

    let queryExpectation = XCTestExpectation(description: "package-version query should return cleanly")

    XCTAssertFalse(downloadManager.updateCacheIsCurrent)
    downloadManager.queryKeysForUpdatablePackages { keys, error in
      guard error == nil, keys != nil else {
        XCTFail()
        queryExpectation.fulfill()
        return
      }

      queryExpectation.fulfill()
    }

    wait(for: [queryExpectation], timeout: 5)

    // Now, check to ensure we've cached the results.
    XCTAssertTrue(downloadManager.updateCacheIsCurrent)

    // Step 2:  prepare to run the batch update!
    let updateExpectation = XCTestExpectation(description: "performBatchUpdate should return cleanly")

    let updateSet = downloadManager.getKeysForUpdatablePackages()
    downloadManager.performBatchUpdate(forPackageKeys: updateSet, withNotifications: false) { successes, failures in
      XCTAssertEqual(failures.count, 0)
      XCTAssertEqual(successes.count, updateSet.count)

      updateExpectation.fulfill()
    }

    // Step 2.5:  ensure all mocking is properly established.

    // At this stage, the downloading queue is paused.  We need to inspect the batch ordering in order
    // to get the mocking right.
    //
    // Swift implements 'secure hashing' behavior, which means the order is technically
    // inconsistent between runs.  (In order to prevent "hash flooding" attacks.)
    // Reference: https://www.hackingwithswift.com/articles/115/swift-4-2-improves-hashable-with-a-new-hasher-struct
    guard case let .compositeBatch(updateBatch) = downloadManager.downloader.topLevelNodes()[0] else {
      XCTFail()
      return
    }

    // Now that we are examining the batch node, we can determine the order in which KeymanEngine wants to
    // download the updates and mock accordingly.
    updateBatch.batchQueue.forEach { entry in
      // Create the mock corresponding to this key and add it to the mocking queue.
      let packageLocation = TestUtils.Packages.getURLForPackage(withKey: entry.0.packageKeys[0])!
      let batchForKey = TestUtils.Downloading.MockResult(location: packageLocation, error: nil)
      mockedURLSession.queueMockResult(.download(batchForKey))
    }

    // Step 3:  Let the updater do its thing.

    downloadManager.downloader.autoExecute = true // We'll let it auto-run for this test.
    downloadManager.downloader.step() // It will auto-run after this.

    wait(for: [updateExpectation], timeout: 15)

    // Step 4:  Assertion time.

    let packages = ResourceFileManager.shared.installedPackages
    packages.forEach { package in
      // Baseline - the kmp.json files should no longer be the old, auto-generated versions.
      XCTAssertFalse(package.metadata.isAutogeneratedWrapper, "\(package.key.id) @ \(package.sourceFolder)")
      XCTAssertNotEqual(package.version, Version.freshInstall)

      // Most of the packages have author information included within their metadata.
      // This is never autogenerated.
      if package.key != TestUtils.Packages.Keys.indigenous_nt &&
         package.key != TestUtils.Packages.Keys.sil_euro_latin {
        XCTAssertNotNil(package.metadata.info?.author, package.key.id)
      }

      if let folderContents = try? FileManager.default.contentsOfDirectory(atPath: package.sourceFolder.path) {
        if package.resourceType() == .keyboard {
          // At it turns out, all of the keyboard packages handled by this case also contain a
          // 'desktop'-oriented keyboard!
          XCTAssertTrue(folderContents.contains(where: { $0.hasSuffix(".kmx") }))
          // As it turns out, all of the keyboard packages handled by this case have welcome.htm files!
          XCTAssertTrue(folderContents.contains(where: { $0.hasSuffix("welcome.htm") }))
        }
      } else {
        XCTFail()
      }
    }
  }

  func testPerformBatchUpdatePostMigrationWithErrors() throws {
    // Features 7 keyboards and 6 lexical models, most of which use autogenerated kmp.jsons.
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Migrations.cloud_to_kmp_13)
    try Migrations.migrateCloudResourcesToKMPFormat()

    // Step 1 - get the package-version query results in place.

    // Query generated from actual api.keyman.com query on 17 Jul 2020.
    /* Original request URL: https://api.keyman.com/package-version?keyboard=sil_sahu&keyboard=sil_euro_latin&keyboard=balochi_latin&keyboard=sil_cameroon_qwerty&keyboard=fv_sencoten&keyboard=indigenous_nt&model=nrc.en.mtnt&model=nrc.str.sencoten&model=sil.bcc-latn.upp_ptwl1&platform=ios
     */
    let mockedQuery = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_post_migration, error: nil)
    mockedURLSession.queueMockResult(.data(mockedQuery))

    let queryExpectation = XCTestExpectation(description: "package-version query should return cleanly")

    XCTAssertFalse(downloadManager.updateCacheIsCurrent)
    downloadManager.queryKeysForUpdatablePackages { keys, error in
      guard error == nil, keys != nil else {
        XCTFail()
        queryExpectation.fulfill()
        return
      }

      queryExpectation.fulfill()
    }

    wait(for: [queryExpectation], timeout: 5)

    // Now, check to ensure we've cached the results.
    XCTAssertTrue(downloadManager.updateCacheIsCurrent)

    // Step 2:  prepare to run the batch update!
    let updateExpectation = XCTestExpectation(description: "performBatchUpdate should return cleanly")

    let updateSet = downloadManager.getKeysForUpdatablePackages()
    downloadManager.performBatchUpdate(forPackageKeys: updateSet, withNotifications: false) { successes, failures in
      XCTAssertEqual(failures.count, 2)
      XCTAssertEqual(successes.count, updateSet.count-2)

      updateExpectation.fulfill()
    }

    // Step 2.5:  ensure all mocking is properly established.

    // At this stage, the downloading queue is paused.  We need to inspect the batch ordering in order
    // to get the mocking right.
    //
    // Swift implements 'secure hashing' behavior, which means the order is technically
    // inconsistent between runs.  (In order to prevent "hash flooding" attacks.)
    // Reference: https://www.hackingwithswift.com/articles/115/swift-4-2-improves-hashable-with-a-new-hasher-struct
    guard case let .compositeBatch(updateBatch) = downloadManager.downloader.topLevelNodes()[0] else {
      XCTFail()
      return
    }

    // Now that we are examining the batch node, we can determine the order in which KeymanEngine wants to
    // download the updates and mock accordingly.
    updateBatch.batchQueue.forEach { entry in
      // Create the mock corresponding to this key and add it to the mocking queue.
      let packageKey = entry.0.packageKeys[0]
      let packageLocation = TestUtils.Packages.getURLForPackage(withKey: packageKey)!
      var batchForKey: TestUtils.Downloading.MockResult

      // One's a lexical model, one's a keyboard.
      if(packageKey == TestUtils.Packages.Keys.nrc_en_mtnt ||
         packageKey == TestUtils.Packages.Keys.fv_sencoten) {
        batchForKey = TestUtils.Downloading.MockResult(location: packageLocation, error: TestUtils.mockedError)
      } else {
        batchForKey = TestUtils.Downloading.MockResult(location: packageLocation, error: nil)
      }
      mockedURLSession.queueMockResult(.download(batchForKey))
    }

    // Step 3:  Let the updater do its thing.

    downloadManager.downloader.autoExecute = true // We'll let it auto-run for this test.
    downloadManager.downloader.step() // It will auto-run after this.

    // This test's goal:  not crashing due to errors / failures during update ops.
    wait(for: [updateExpectation], timeout: 15)
  }
}
