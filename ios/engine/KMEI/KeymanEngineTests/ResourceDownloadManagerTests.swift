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
    let mtnt_id = TestUtils.Keyboards.khmer_angkor.fullID

    let mockedResult = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: nil)
    mockedURLSession?.queueMockResult(.download(mockedResult))

    downloadManager?.downloadPackage(forFullID: mtnt_id, from: TestUtils.Keyboards.khmerAngkorKMP, withNotifications: false) { package, error in

      // Assertion:  a copy of the KMP file exists in the documents directory, facilitating user sharing.
      let documentsDir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
      let cachedDocumentsKMP = documentsDir.appendingPathComponent(
        TypedKeymanPackage<InstallableKeyboard>.baseFilename(for: TestUtils.Keyboards.khmer_angkor.fullID))

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

    let mockedResult = TestUtils.Downloading.MockResult(location: TestUtils.LexicalModels.mtntKMP, error: nil)
    mockedURLSession?.queueMockResult(.download(mockedResult))

    downloadManager?.downloadPackage(forFullID: mtnt_id, from: TestUtils.LexicalModels.mtntKMP, withNotifications: false) { package, error in

      // Assertion:  a copy of the KMP file exists in the documents directory, facilitating user sharing.
      let documentsDir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
      let cachedDocumentsKMP = documentsDir.appendingPathComponent(
        TypedKeymanPackage<InstallableLexicalModel>.baseFilename(for: TestUtils.LexicalModels.mtnt.fullID))

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
    let mtnt_id = TestUtils.Keyboards.khmer_angkor.fullID

    let mockedResult = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: TestUtils.mockedError)
    mockedURLSession?.queueMockResult(.download(mockedResult))

    downloadManager?.downloadPackage(forFullID: mtnt_id, from: TestUtils.Keyboards.khmerAngkorKMP, withNotifications: false) { package, error in

      // Assertion:  a copy of the KMP file exists in the documents directory, facilitating user sharing.
      let documentsDir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
      let cachedDocumentsKMP = documentsDir.appendingPathComponent(
        TypedKeymanPackage<InstallableKeyboard>.baseFilename(for: TestUtils.Keyboards.khmer_angkor.fullID))

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
}
