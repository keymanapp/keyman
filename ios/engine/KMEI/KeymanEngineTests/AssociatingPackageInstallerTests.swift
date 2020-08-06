//
//  AssociatingPackageInstallerTests.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 8/6/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest
@testable import KeymanEngine

class AssociatingPackageInstallerTests: XCTestCase {
  var mockedURLSession: TestUtils.Downloading.URLSessionMock!
  var downloadManager: ResourceDownloadManager!

  let mockedSearchCallback: LanguagePickAssociator.AssociationSearcher = { lgCodes, callback in
    var searchResult: [String: (KeymanPackage.Key, URL)?] = [:]

    // We aren't downloading these models, so a placeholder URL is fine.
    let placeholderURL = URL.init(string: "http://place.holder.com")!

    lgCodes.forEach { lgCode in
      switch lgCode {
        case "en":
          searchResult["en"] = (TestUtils.Packages.Keys.nrc_en_mtnt, placeholderURL)
        case "str":
          searchResult["str"] = (TestUtils.Packages.Keys.nrc_str_sencoten, placeholderURL)
        default:
          searchResult[lgCode] = nil
      }
    }

    callback(searchResult)
  }

  override func setUp() {
    // Resets resource directories for a clean slate.
    // We'll be testing against actually-installed packages.
    TestUtils.standardTearDown()
    
    mockedURLSession = TestUtils.Downloading.URLSessionMock()
    downloadManager = ResourceDownloadManager(session: mockedURLSession!, autoExecute: true)
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

  func testKeyboardNoAssociations() throws {
    guard let package = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.silEuroLatinKMP) as? KeyboardKeymanPackage else {
      XCTFail()
      return
    }

    let startExpectation = XCTestExpectation()
    let completeExpectation = XCTestExpectation()

    let installer = AssociatingPackageInstaller(for: package,
                                                defaultLanguageCode: "en",
                                                downloadManager: downloadManager) { status in
      if status == .starting {
        startExpectation.fulfill()
      } else if status == .complete {
        completeExpectation.fulfill()
      }
    }

    installer.pickLanguages(withCodes: Set(["en"]))

    wait(for: [startExpectation, completeExpectation], timeout: 5)

    guard let userKeyboards = Storage.active.userDefaults.userKeyboards else {
      XCTFail()
      return
    }

    XCTAssertTrue(userKeyboards.contains { $0.fullID == TestUtils.Keyboards.sil_euro_latin.fullID })
    XCTAssertNil(Storage.active.userDefaults.userLexicalModels)
    XCTAssertEqual(ResourceFileManager.shared.installState(forPackage: TestUtils.Packages.Keys.sil_euro_latin), .installed)
    XCTAssertEqual(ResourceFileManager.shared.installState(forPackage: TestUtils.Packages.Keys.nrc_en_mtnt), .none)
  }

  func testLexicalModelNoAssociations() throws {
    guard let package = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.LexicalModels.mtntKMP) as? LexicalModelKeymanPackage else {
      XCTFail()
      return
    }

    let startExpectation = XCTestExpectation()
    let completeExpectation = XCTestExpectation()

    let installer = AssociatingPackageInstaller(for: package,
                                                defaultLanguageCode: "en",
                                                downloadManager: downloadManager) { status in
      if status == .starting {
        startExpectation.fulfill()
      } else if status == .complete {
        completeExpectation.fulfill()
      }
    }

    installer.pickLanguages(withCodes: Set(["en"]))

    wait(for: [startExpectation, completeExpectation], timeout: 5)

    guard let userLexicalModels = Storage.active.userDefaults.userLexicalModels else {
      XCTFail()
      return
    }

    XCTAssertTrue(userLexicalModels.contains { $0.fullID == TestUtils.LexicalModels.mtnt.fullID })
    XCTAssertNil(Storage.active.userDefaults.userKeyboards)
    XCTAssertEqual(ResourceFileManager.shared.installState(forPackage: TestUtils.Packages.Keys.nrc_en_mtnt), .installed)
    XCTAssertEqual(ResourceFileManager.shared.installState(forPackage: TestUtils.Packages.Keys.sil_euro_latin), .none)
  }

  func testKeyboardWithAssociatedLexicalModels() throws {
    let mockedQuery = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_en, error: nil)
    let mockedDownload = TestUtils.Downloading.MockResult(location: TestUtils.LexicalModels.mtntKMP, error: nil)

    mockedURLSession.queueMockResult(.data(mockedQuery))
    mockedURLSession.queueMockResult(.download(mockedDownload))

    guard let package = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.silEuroLatinKMP) as? KeyboardKeymanPackage else {
      XCTFail()
      return
    }

    let startExpectation = XCTestExpectation()
    let completeExpectation = XCTestExpectation()

    let installer = AssociatingPackageInstaller(for: package,
                                                defaultLanguageCode: "en",
                                                downloadManager: downloadManager,
                                                withAssociators: [ .lexicalModels ]) { status in
      if status == .starting {
        startExpectation.fulfill()
      } else if status == .complete {
        completeExpectation.fulfill()
      }
    }

    installer.pickLanguages(withCodes: Set(["en"]))

    wait(for: [startExpectation, completeExpectation], timeout: 5)

    guard let userKeyboards = Storage.active.userDefaults.userKeyboards,
          let userLexicalModels = Storage.active.userDefaults.userLexicalModels else {
      XCTFail()
      return
    }

    XCTAssertTrue(userKeyboards.contains { $0.fullID == TestUtils.Keyboards.sil_euro_latin.fullID })
    XCTAssertTrue(userLexicalModels.contains { $0.fullID == TestUtils.LexicalModels.mtnt.fullID} )

    XCTAssertEqual(ResourceFileManager.shared.installState(forPackage: TestUtils.Packages.Keys.sil_euro_latin), .installed)
    XCTAssertEqual(ResourceFileManager.shared.installState(forPackage: TestUtils.Packages.Keys.nrc_en_mtnt), .installed)
  }
}
