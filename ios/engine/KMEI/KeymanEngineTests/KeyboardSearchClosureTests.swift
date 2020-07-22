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

    func testDefaultResultInstallationClosureTaggedSuccessNoModel() throws {
      /* Step 1:  mocking.
       *
       * Currently testable:
       *     - Downloads keyboard KMP
       *
       * Not currently testable, but still requires mocking:
       *     - runs lexical model query
       *     - (if available) downloads lexical model KMP.
       *
       * Alas, the chained lexical-model download doesn't currently accept a completion handler,
       * as the keyboard download completion handler isn't designed to accommodate chained downloads.
       */
       let packageDownloadTask = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: nil)
       let modelQueryTask = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_km, error: nil)

       mockedURLSession!.queueMockResult(.download(packageDownloadTask))
       mockedURLSession!.queueMockResult(.data(modelQueryTask))

      // Step 2 - build closure
      let closureToTest = KeyboardSearchViewController.defaultResultInstallationClosure(withDownloadManager: downloadManager) //{ package, error in
        //
      //}

      let expectation = XCTestExpectation(description: "Keyboard package download & installation should complete")

      let wrappedClosure: KeyboardSearchViewController.SearchCompletionHandler = { packageKey, resourceKey in
        closureToTest(packageKey, resourceKey)

        expectation.fulfill()
      }

      // Step 3 - run closure
      wrappedClosure(TestUtils.Packages.Keys.khmer_angkor, TestUtils.Keyboards.khmer_angkor.fullID)

      wait(for: [expectation], timeout: 5)
    }

}
