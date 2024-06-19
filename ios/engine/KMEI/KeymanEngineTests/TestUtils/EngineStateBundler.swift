//
//  EngineStateBundler.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 7/15/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import XCTest
import ZIPFoundation
import os.log

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

        let messageOne = "Documents directory source: \(documentsDirectory)"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, messageOne)
        try FileManager.default.copyItem(at: documentsDirectory, to: bundleConstructionURL.appendingPathComponent("Documents"))

        let messageTwo = "Library source: \(storageDirectory)"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, messageTwo)
        try FileManager.default.copyItem(at: storageDirectory, to: bundleConstructionURL.appendingPathComponent("Library").appendingPathComponent("keyman"))

        let messageThree = "Preferences directory source: \(preferencesDirectory)"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, messageThree)
        let pListPath = bundleConstructionURL.appendingPathComponent("Library").appendingPathComponent("Preferences")

        let testEngineFilename = "com.keyman.testing.KeymanEngineTestHost.plist"
        let appGroupFilename = "group.KM4I.plist"
        // If trying to ensure that UserDefaults is properly synchronized for this copy operation,
        // go to the folder for the link above in Finder and watch the file to ensure it's properly
        // fleshed out.
        //
        // Set a breakpoint to the following line, only resuming when the plist is fully flushed to
        // the file system.  You may need to rename it in the resulting bundle.
        try FileManager.default.copyItem(at: preferencesDirectory, to: pListPath)

        // sleep(1000)

        try FileManager.default.moveItem(at: pListPath.appendingPathComponent(testEngineFilename), to: pListPath.appendingPathComponent(appGroupFilename))
      }

      let archiveURL = bundleConstructionURL.appendingPathComponent("bundleArchive.zip")
      do {
        _ = try Archive(url: archiveURL, accessMode: .create)
        let message = "archiveURL: \(archiveURL)"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
        let attachment = XCTAttachment(contentsOfFile: archiveURL)
        attachment.lifetime = .keepAlways
        return attachment
      } catch let error {
        print (error.localizedDescription)
        return XCTAttachment(string: error.localizedDescription)
      }
    }     
  }
}
