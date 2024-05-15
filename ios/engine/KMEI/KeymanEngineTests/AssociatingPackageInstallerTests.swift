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
    // Ensure Manager's standard init() occurs before our tests.
    // Otherwise, if our first Manager reference is DURING a test, we'll see unwanted behavior from it.
    _ = Manager.shared
    
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
      } else if status == .cancelled {
        XCTFail()
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
      } else if status == .cancelled {
        XCTFail()
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
      } else if status == .cancelled {
        XCTFail()
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
  
  
  func testCancellationNoAssociations() throws {
    guard let package = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.silEuroLatinKMP) as? KeyboardKeymanPackage else {
      XCTFail()
      return
    }
    
    let cancelExpectation = XCTestExpectation()
    
    let installer = AssociatingPackageInstaller(for: package,
                                                defaultLanguageCode: "en",
                                                downloadManager: downloadManager) { status in
      if status == .cancelled {
        // When the user 'cancels' language selection, this installer should also
        // signal cancellation.
        cancelExpectation.fulfill()
      } else {
        XCTFail()
      }
    }
    
    // A bit of white-box testing - sets up the closure framework
    installer.initializeSynchronizationGroups()
    // And the install closure, which we'll send a 'cancel' signal to.
    let installClosure = installer.coreInstallationClosure()
    installClosure(nil)
    
    wait(for: [cancelExpectation], timeout: 5)
    
    XCTAssertNil(Storage.active.userDefaults.userKeyboards)
    XCTAssertNil(Storage.active.userDefaults.userLexicalModels)
  }
  
  func testCancellationWithAssociations() throws {
    guard let package = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.silEuroLatinKMP) as? KeyboardKeymanPackage else {
      XCTFail()
      return
    }
    
    // While the query should occur, the installation phase should never be reached.
    // So, we don't mock the download.
    let mockedQuery = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_en, error: nil)
    mockedURLSession.queueMockResult(.data(mockedQuery))
    
    let cancelExpectation = XCTestExpectation()
    
    let installer = AssociatingPackageInstaller(for: package,
                                                defaultLanguageCode: "en",
                                                downloadManager: downloadManager,
                                                withAssociators: [.lexicalModels]) { status in
      if status == .cancelled {
        // When the user 'cancels' language selection, this installer should also
        // signal cancellation.
        cancelExpectation.fulfill()
      } else {
        XCTFail()
      }
    }
    
    // Heavier white-box testing this time.
    installer.initializeSynchronizationGroups()
    installer.constructAssociationPickers()
    installer.associationQueriers?.forEach {
      $0.pickerInitialized()
      $0.selectLanguages(Set(["en"])) // calls the expected query
      // And, since we're modeling cancellation...
      $0.pickerDismissed()
    }
    
    // Finally, the install closure, which we'll send a 'cancel' signal to.
    let installClosure = installer.coreInstallationClosure()
    installClosure(nil)
    
    wait(for: [cancelExpectation], timeout: 5)
    
    XCTAssertNil(Storage.active.userDefaults.userKeyboards)
    XCTAssertNil(Storage.active.userDefaults.userLexicalModels)
  }
  
  func testDeinitConstructiveCancellation() throws {
    guard let package = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.silEuroLatinKMP) as? KeyboardKeymanPackage else {
      XCTFail()
      return
    }
    
    // While the query should occur, the installation phase should never be reached.
    // So, we don't mock the download.
    let mockedQuery = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_en, error: nil)
    let queryExpectation = XCTestExpectation()
    mockedURLSession.queueMockResult(.data(mockedQuery), expectation: queryExpectation)
    
    let cancelExpectation = XCTestExpectation()
    
    do {
      let installer = AssociatingPackageInstaller(for: package,
                                                  defaultLanguageCode: "en",
                                                  downloadManager: downloadManager,
                                                  withAssociators: [.lexicalModels]) { status in
        // When the user 'cancels' language selection, this installer should also
        // signal cancellation.
        if status == .cancelled {
          // First, wait to ensure that the standard query did launch.
          // Otherwise, we might accidentally skip the mocked query!
          self.wait(for: [queryExpectation], timeout: 4)
          cancelExpectation.fulfill()
        } else {
          XCTFail()
        }
      }
      
      // Heavier white-box testing this time.
      installer.initializeSynchronizationGroups()
      installer.constructAssociationPickers()
      installer.associationQueriers?.forEach {
        $0.pickerInitialized()
        $0.selectLanguages(Set(["en"])) // calls the expected query
        // De-init will trigger the 'dismiss' signal.
      }
    }
    
    wait(for: [cancelExpectation], timeout: 5)
    
    XCTAssertNil(Storage.active.userDefaults.userKeyboards)
    XCTAssertNil(Storage.active.userDefaults.userLexicalModels)
  }
  
  func testDeinitUnstarted() throws {
    guard let package = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.silEuroLatinKMP) as? KeyboardKeymanPackage else {
      XCTFail()
      return
    }
    
    let cancelExpectation = XCTestExpectation()
    
    do {
      let _ = AssociatingPackageInstaller(for: package,
                                          defaultLanguageCode: "en",
                                          downloadManager: downloadManager,
                                          withAssociators: [.lexicalModels]) { status in
        // Since language-picking never began, no "cancelled" signal should occur.
        if status == .cancelled {
          cancelExpectation.fulfill()
        } else {
          XCTFail()
        }
      }
    }
    
    wait(for: [cancelExpectation], timeout: 1)
    
    XCTAssertNil(Storage.active.userDefaults.userKeyboards)
    XCTAssertNil(Storage.active.userDefaults.userLexicalModels)
  }
  
  func testPackageLangCodePartialMatch() throws {
    guard let strPackage = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.fvSencotenKMP) as? KeyboardKeymanPackage else {
      XCTFail()
      return
    }
    
    guard let eurolatinPackage = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.silEuroLatinKMP) as? KeyboardKeymanPackage else {
      XCTFail()
      return
    }
    
    let strInstaller = AssociatingPackageInstaller(for: strPackage,
                                                   defaultLanguageCode: "str", // correct code:  str-latn
                                                   downloadManager: downloadManager) { _ in
    }
    
    // Package does not contain "str", but does contain "str-latn"
    XCTAssertEqual(strInstaller.defaultLgCode, "str-latn")
    
    let eurolatinInstaller = AssociatingPackageInstaller(for: eurolatinPackage,
                                                         defaultLanguageCode: "en-fake-bcp", // correct code:  "en"
                                                         downloadManager: downloadManager) { _ in
    }
    
    // Package does not contain "en-fake-bcp", but does contain "en".
    XCTAssertEqual(eurolatinInstaller.defaultLgCode, "en")
    
    // Note:  the current naive approach isn't exactly BCP-47 subtag aware - it relies on one tag
    // containing the entire other tag as a substring.
  }
}
