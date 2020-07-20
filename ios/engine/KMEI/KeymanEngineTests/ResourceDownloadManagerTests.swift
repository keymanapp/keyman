//
//  ResourceDownloadManagement.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 6/29/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest
@testable import KeymanEngine

class ResourceDownloadManagerTests: XCTestCase {
  var mockedURLSession: TestUtils.Downloading.URLSessionMock?
  var downloadManager: ResourceDownloadManager?

  override func setUp() {
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

  func testDownloadPackageForKeyboard() throws {
    let expectation = XCTestExpectation(description: "Mocked \"download\" should complete successfully.")
    let khmer_angkor_id = TestUtils.Keyboards.khmer_angkor.fullID
    let packageKey = TestUtils.Keyboards.khmer_angkor.packageKey

    let mockedResult = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: nil)
    mockedURLSession?.queueMockResult(.download(mockedResult))

    downloadManager?.downloadPackage(forFullID: khmer_angkor_id, withKey: packageKey, from: TestUtils.Keyboards.khmerAngkorKMP, withNotifications: false) { package, error in


      let tempDownloadKMP = ResourceFileManager.shared.packageDownloadTempPath(forKey: packageKey)
      XCTAssertFalse(FileManager.default.fileExists(atPath: tempDownloadKMP.path))
      
      // Assertions:  a copy of the KMP file exists in the documents directory, facilitating user sharing.
      let cachedDocumentsKMP = ResourceFileManager.shared.cachedPackagePath(forKey: packageKey)

      XCTAssertTrue(FileManager.default.fileExists(atPath: cachedDocumentsKMP.path))

      XCTAssertNil(error)
      XCTAssertNotNil(package)

      // Basic verification that it really is the khmer_angkor keyboard package.
      if let package = package {
        XCTAssertEqual(package.id, TestUtils.Keyboards.khmer_angkor.id)
        XCTAssertNotNil(package.findResource(withID: TestUtils.Keyboards.khmer_angkor.fullID))
      }

      expectation.fulfill()
    }

    let downloadQueue = downloadManager!.downloader
    downloadQueue.step()

    wait(for: [expectation], timeout: 5)
  }

  func testDownloadPackageForLexicalModel() throws {
    let expectation = XCTestExpectation(description: "Mocked \"download\" should complete successfully.")
    let mtnt_id = TestUtils.LexicalModels.mtnt.fullID
    let packageKey = TestUtils.LexicalModels.mtnt.packageKey

    let mockedResult = TestUtils.Downloading.MockResult(location: TestUtils.LexicalModels.mtntKMP, error: nil)
    mockedURLSession?.queueMockResult(.download(mockedResult))

    downloadManager?.downloadPackage(forFullID: mtnt_id, withKey: packageKey, from: TestUtils.LexicalModels.mtntKMP, withNotifications: false) { package, error in

      let tempDownloadKMP = ResourceFileManager.shared.packageDownloadTempPath(forKey: packageKey)
      XCTAssertFalse(FileManager.default.fileExists(atPath: tempDownloadKMP.path))

      // Assertion:  a copy of the KMP file exists in the documents directory, facilitating user sharing.
      let cachedDocumentsKMP = ResourceFileManager.shared.cachedPackagePath(forKey: packageKey)

      XCTAssertTrue(FileManager.default.fileExists(atPath: cachedDocumentsKMP.path))

      XCTAssertNil(error)
      XCTAssertNotNil(package)

      // Basic verification that it really is the khmer_angkor keyboard package.
      if let package = package {
        XCTAssertEqual(package.id, TestUtils.LexicalModels.mtnt.id)
        XCTAssertNotNil(package.findResource(withID: TestUtils.LexicalModels.mtnt.fullID))
      }

      expectation.fulfill()
    }

    let downloadQueue = downloadManager!.downloader
    downloadQueue.step()

    wait(for: [expectation], timeout: 5)
  }

  func testDownloadPackageFailure() throws {
    let expectation = XCTestExpectation(description: "Mocked \"download\" should complete, though with an error.")
    let khmer_angkor_id = TestUtils.Keyboards.khmer_angkor.fullID
    let packageKey = TestUtils.Keyboards.khmer_angkor.packageKey

    let mockedResult = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: TestUtils.mockedError)
    mockedURLSession?.queueMockResult(.download(mockedResult))

    downloadManager?.downloadPackage(forFullID: khmer_angkor_id, withKey: packageKey, from: TestUtils.Keyboards.khmerAngkorKMP, withNotifications: false) { package, error in

      let tempDownloadKMP = ResourceFileManager.shared.packageDownloadTempPath(forKey: packageKey)
      XCTAssertFalse(FileManager.default.fileExists(atPath: tempDownloadKMP.path))

      // Assertion:  a copy of the KMP file exists in the documents directory, facilitating user sharing.
      let cachedDocumentsKMP = ResourceFileManager.shared.cachedPackagePath(forKey: packageKey)

      // On download failure, we shouldn't be producing a file here.
      XCTAssertFalse(FileManager.default.fileExists(atPath: cachedDocumentsKMP.path))

      XCTAssertNotNil(error)
      XCTAssertNil(package)

      expectation.fulfill()
    }

    let downloadQueue = downloadManager!.downloader
    downloadQueue.step()

    wait(for: [expectation], timeout: 5)
  }

  func testLegacyDownloadKeyboard() throws {
    let expectation = XCTestExpectation(description: "Mocked package \"download\" should complete successfully.")

    // For this test, we actually want the downloader to run automatically.  It's... a bit tricky to
    // run this integration test otherwise.
    downloadManager = ResourceDownloadManager(session: mockedURLSession!, autoExecute: true)

    let mockedQuery = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_km, error: nil)
    let mockedDownload = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: nil)

    mockedURLSession?.queueMockResult(.data(mockedQuery))
    mockedURLSession?.queueMockResult(.download(mockedDownload))

    let khmer_angkor_id = TestUtils.Keyboards.khmer_angkor.fullID

    downloadManager?.downloadKeyboard(withID: khmer_angkor_id.id, languageID: khmer_angkor_id.languageID, isUpdate: false, fetchRepositoryIfNeeded: false) { package, error in
      if let error = error {
        XCTFail("Mocked query and download should both succeed; reported error: \(error.localizedDescription)")
      } else if let package = package {
        // Double-check it!
        XCTAssertEqual(package.id, "khmer_angkor")
        XCTAssertNotNil(package.findResource(withID: khmer_angkor_id))
      }

      expectation.fulfill()
    }

    wait(for: [expectation], timeout: 5)
  }

  func testLegacyDownloadLexicalModel() throws {
    let expectation = XCTestExpectation(description: "Mocked package \"download\" should complete successfully.")

    // For this test, we actually want the downloader to run automatically.  It's... a bit tricky to
    // run this integration test otherwise.
    downloadManager = ResourceDownloadManager(session: mockedURLSession!, autoExecute: true)
    
    let mockedQuery = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_case_mtnt, error: nil)
    let mockedDownload = TestUtils.Downloading.MockResult(location: TestUtils.LexicalModels.mtntKMP, error: nil)

    mockedURLSession?.queueMockResult(.data(mockedQuery))
    mockedURLSession?.queueMockResult(.download(mockedDownload))

    let mtnt_id = TestUtils.LexicalModels.mtnt.fullID

    downloadManager?.downloadLexicalModel(withID: mtnt_id.id, languageID: mtnt_id.languageID, isUpdate: false, fetchRepositoryIfNeeded: false) { package, error in
      if let error = error {
        XCTFail("Mocked query and download should both succeed; reported error: \(error.localizedDescription)")
      } else if let package = package {
        // Double-check it!
        XCTAssertEqual(package.id, "nrc.en.mtnt")
        XCTAssertNotNil(package.findResource(withID: mtnt_id))
      }

      expectation.fulfill()
    }

    wait(for: [expectation], timeout: 5)
  }

  // This annotation prevents local deprecation warnings for what it tests.
  @available(*, deprecated, message: "This method tests the deprecated ResourceDownloadManager.stateForKeyboard.")
  func testStateForKeyboard() {
    let baseInstallation = XCTestExpectation(description: "Mocked \"download\" should complete successfully.")
    let khmer_angkor_id = TestUtils.Keyboards.khmer_angkor.fullID
    let packageKey = TestUtils.Keyboards.khmer_angkor.packageKey

    let mockedResult = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: nil)
    mockedURLSession?.queueMockResult(.download(mockedResult))

    XCTAssertEqual(downloadManager!.stateForKeyboard(withID: khmer_angkor_id.id), .needsDownload)

    downloadManager!.downloadPackage(forFullID: khmer_angkor_id,
                                     withKey: packageKey,
                                     from: TestUtils.Keyboards.khmerAngkorKMP,
                                     withNotifications: false) { package, error in
      if let _ = error {
        XCTFail()
        baseInstallation.fulfill()
        return
      } else if let package = package {
        do {
          try ResourceFileManager.shared.install(resourceWithID: khmer_angkor_id, from: package)
          baseInstallation.fulfill()
        } catch {
          XCTFail()
          baseInstallation.fulfill()
        }
      }
    }

    XCTAssertEqual(downloadManager!.stateForKeyboard(withID: khmer_angkor_id.id), .downloading)

    downloadManager!.downloader.step()

    wait(for: [baseInstallation], timeout: 5)

    XCTAssertEqual(downloadManager!.stateForKeyboard(withID: khmer_angkor_id.id), .upToDate)

    // Now, to test the update-check part of the function...
    // This fixture was hand-altered.  At the time of this test's creation, this version did not exist.
    let mockedQuery = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_km_updated, error: nil)
    mockedURLSession?.queueMockResult(.data(mockedQuery))

    let versionQuery = XCTestExpectation()

    // Now to do a package-version check.
    KeymanPackage.queryCurrentVersions(for: [KeymanPackage.Key(id: khmer_angkor_id.id, type: .keyboard)], withSession: downloadManager!.session) { result, _ in
      XCTAssertEqual(self.downloadManager!.stateForKeyboard(withID: khmer_angkor_id.id), .needsUpdate)
      versionQuery.fulfill()
    }

    wait(for: [versionQuery], timeout: 5)
  }
}
