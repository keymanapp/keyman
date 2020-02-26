//
//  Migrations.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 2/20/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
@testable import KeymanEngine

import XCTest

extension TestUtils {
  enum Migrations {
    // Important locations within Migration-test .bundles
    static let documentsFolder = "Documents"
    static let baseFolder = "Library/keyman"

    // Bundle references
    static let simple_12 = TestUtils.findSubBundle(forResource: "Simple 12.0 Migration", ofType: ".bundle")
    static let adhoc_12 = TestUtils.findSubBundle(forResource: "12.0 Ad-hoc Migration", ofType: ".bundle")
    static let simple_10 = TestUtils.findSubBundle(forResource: "Simple 10.0 Migration", ofType: ".bundle")
    static let noDefault_10 = TestUtils.findSubBundle(forResource: "No-defaults 10.0 Migration", ofType: ".bundle")

    // The default keyboard in version 10.0.
    static let european2_font = Font(family: "LatinWeb", source: ["DejaVuSans.ttf"], size: nil)
    static let european2 = InstallableKeyboard(id: "european2",
                                              name: "EuroLatin2 Keyboard",
                                              languageID: "en",
                                              languageName: "English",
                                              version: "1.6",
                                              isRTL: false,
                                              font: european2_font,
                                              oskFont: nil,
                                              isCustom: false)

    static func applyBundleToFileSystem(_ bundle: Bundle) {
      let fileManager = FileManager.default
      let bundlePath = URL(fileURLWithPath: bundle.resourcePath!)

      let documentsSource = bundlePath.appendingPathComponent(documentsFolder)
      let documentsDestination = fileManager.urls(for: .documentDirectory, in: .userDomainMask)[0]

      let baseSource = bundlePath.appendingPathComponent(baseFolder)
      let baseDestination = fileManager.urls(for: .libraryDirectory,
                                             in: .userDomainMask)[0]
                                             .appendingPathComponent("keyman")

      do {
        // Avoid .replaceItemAt - it'll remove the files from the source Bundle!
        if fileManager.fileExists(atPath: documentsSource.path) {
          if fileManager.fileExists(atPath: documentsDestination.path) {
            try fileManager.removeItem(at: documentsDestination)
          }
          try fileManager.copyItem(at: documentsSource, to: documentsDestination)
        }
        if fileManager.fileExists(atPath: baseDestination.path) {
          try fileManager.removeItem(at: baseDestination)
        }

        try fileManager.copyItem(at: baseSource, to: baseDestination)
      } catch {
        XCTFail("Could not properly set the state for a migration test: \(error)")
      }
    }

    static func getVersionHistory(for version: Version) -> [LanguageResource] {
      let resourceHistory = KeymanEngine.Migrations.resourceHistory

      let match: [[LanguageResource]] = resourceHistory.compactMap() { entry in
        if entry.version == version {
          return entry.resources
        } else {
          return nil
        }
      }

      return match[0]
    }
  }
}
