//
//  EngineStateBundler.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 7/15/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import XCTest
import Zip
@testable import KeymanEngine

extension TestUtils {
  /**
   * Allows auto-generation of bundles corresponding to the current state of the test-host app for use in actual unit tests.
   */
  enum EngineStateBundler {
    static var documentsDirectory: URL {
      return FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
    }

    static var storageDirectory: URL {
      return Storage.active.baseDir
    }

    static var preferencesDirectory: URL {
      return FileManager.default.urls(for: .libraryDirectory, in: .userDomainMask)[0].appendingPathComponent("Preferences")
    }

    static func createBundle(withName name: String) throws -> XCTAttachment {
      let tempDirectory = FileManager.default.temporaryDirectory
      let bundleConstructionURL = tempDirectory.appendingPathComponent("\(name).bundle")
      if FileManager.default.fileExists(atPath: bundleConstructionURL.path) {
        try? FileManager.default.removeItem(at: bundleConstructionURL)
      }

      do {
        try FileManager.default.createDirectory(at: bundleConstructionURL, withIntermediateDirectories: true, attributes: nil)

        try FileManager.default.createDirectory(at: bundleConstructionURL.appendingPathComponent("Library"), withIntermediateDirectories: true, attributes: nil)

        log.info("Documents directory source: \(documentsDirectory)")
        try FileManager.default.copyItem(at: documentsDirectory, to: bundleConstructionURL.appendingPathComponent("Documents"))

        log.info("Library source: \(storageDirectory)")
        try FileManager.default.copyItem(at: storageDirectory, to: bundleConstructionURL.appendingPathComponent("Library").appendingPathComponent("keyman"))

        log.info("Preferences directory source: \(preferencesDirectory)")
        // If trying to ensure that UserDefaults is properly synchronized for this copy operation,
        // go to the folder for the link above in Finder and watch the file to ensure it's properly
        // fleshed out.
        //
        // Set a breakpoint to the following line, only resuming when the plist is fully flushed to
        // the file system.  You may need to rename it in the resulting bundle.
        try FileManager.default.copyItem(at: preferencesDirectory, to:
            bundleConstructionURL.appendingPathComponent("Library").appendingPathComponent("Preferences"))
      }

      let attachmentFile = try Zip.quickZipFiles([bundleConstructionURL], fileName: "bundleArchive")
      log.info("Archive source: \(bundleConstructionURL)")
      let attachment = XCTAttachment(contentsOfFile: attachmentFile)
      attachment.lifetime = .keepAlways

      return attachment
    }
  }
}
