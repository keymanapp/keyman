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

      // TODO:  Add assertion:  file exists in the documents directory, where we expect it
      XCTAssertNil(error)
      XCTAssertNotNil(package)

      // TODO:  Add assertions: it really is the khmer_angkor keyboard package.

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

      // TODO:  Add assertion:  file exists in the documents directory, where we expect it
      XCTAssertNil(error)
      XCTAssertNotNil(package)

      // TODO:  Add assertions: it really is the MTNT lexical model package.

      expectation.fulfill()
    }

    let downloadQueue = downloadManager!.downloader
    downloadQueue.step()

    wait(for: [expectation], timeout: 5)
  }
}
