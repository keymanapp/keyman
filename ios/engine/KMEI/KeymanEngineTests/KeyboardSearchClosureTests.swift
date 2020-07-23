//
//  KeyboardSearchClosureTests.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 7/22/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest
@testable import KeymanEngine

class KeyboardSearchClosureTests: XCTestCase {

  var mockedURLSession: TestUtils.Downloading.URLSessionMock?
  var downloadManager: ResourceDownloadManager!

  override func setUp() {
    mockedURLSession = TestUtils.Downloading.URLSessionMock()
    downloadManager = ResourceDownloadManager(session: mockedURLSession!, autoExecute: true)
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

    func testDefaultKeyboardInstallationClosureTaggedSuccess() throws {
      // Step 1:  mocking.
      let packageDownloadTask = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: nil)
      mockedURLSession!.queueMockResult(.download(packageDownloadTask))

      let installExpectation = XCTestExpectation(description: "Keyboard package download & installation should complete")
      let groupExpectation = XCTestExpectation(description: "DispatchGroup should notify")

      // Step 2 - build closure & synchronization check
      let dispatchGroup = DispatchGroup()

      let closure = KeyboardSearchViewController.defaultKeyboardInstallationClosure(withDownloadManager: downloadManager, dispatchGroup: dispatchGroup) { result in
        if case let .success(fullID) = result {
          XCTAssertEqual(fullID as? FullKeyboardID, TestUtils.Keyboards.khmer_angkor.fullID)
        } else {
          XCTFail("keyboard installation did not succeed.")
        }

        installExpectation.fulfill()
      }

      // As the closure has now been built (and thus, DispatchGroup.enter() called),
      // notification only occurs the callback completes.
      dispatchGroup.notify(queue: .main) {
        groupExpectation.fulfill()
      }

      // Step 3 - run closure
      closure(TestUtils.Packages.Keys.khmer_angkor, TestUtils.Keyboards.khmer_angkor.fullID)

      wait(for: [installExpectation, groupExpectation], timeout: 5)
    }

    // Cancel

    // Error

    func testDefaultLexicalModelInstallationClosureTaggedSuccess() throws {
      // Step 1 - mocking
      let packageDownloadTask = TestUtils.Downloading.MockResult(location: TestUtils.LexicalModels.mtntKMP, error: nil)
      mockedURLSession!.queueMockResult(.download(packageDownloadTask))

      let installExpectation = XCTestExpectation(description: "Lexical model package download & installation should complete")
      let groupExpectation = XCTestExpectation(description: "DispatchGroup should notify")

      // Step 2 - build closure
      let dispatchGroup = DispatchGroup()

      let closure = KeyboardSearchViewController.defaultLexicalModelInstallationClosure(withDownloadManager: downloadManager) { result in
        if case let .success(fullID) = result {
          XCTAssertEqual(fullID as? FullLexicalModelID, TestUtils.LexicalModels.mtnt.fullID)
        } else {
          XCTFail("Lexical model installation did not succeed.")
        }

        installExpectation.fulfill()
      }

      // As the closure has now been built (and thus, DispatchGroup.enter() called),
      // notification only occurs the callback completes.
      dispatchGroup.notify(queue: .main) {
        groupExpectation.fulfill()
      }

      // Step 3 - run closure
      closure(TestUtils.Packages.Keys.nrc_en_mtnt, TestUtils.LexicalModels.mtnt.fullID)

      wait(for: [installExpectation, groupExpectation], timeout: 5)
    }

    // Cancel

    // Error

    // Synchronization test
}
