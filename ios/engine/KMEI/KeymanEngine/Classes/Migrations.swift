//
//  Migrations.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-12-08.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

enum Migrations {
  static func migrateForKMP(storage: Storage) {
    let languageDir = storage.baseDir.appendingPathComponent("languages")
    let fontDir = storage.baseDir.appendingPathComponent("fonts")

    log.info("Migrating from base directory: \(storage.baseDir)")

    guard var userKeyboards = storage.userDefaults.userKeyboards else {
      log.info("No user keyboards to migrate")
      return
    }

    var urlsForKeyboard: [String: Set<URL>] = [:]
    for i in userKeyboards.indices {
      let keyboard = userKeyboards[i]
      guard let version = latestKeyboardFileVersion(withID: keyboard.id, dirPath: languageDir.path) else {
        log.warning("Could not find JS file for keyboard \(keyboard.id) in \(languageDir.path)")
        continue
      }
      var urls = urlsForKeyboard[keyboard.id] ?? Set()
      urls.insert(languageDir.appendingPathComponent("\(keyboard.id)-\(version.string).js"))

      let fontFiles = (keyboard.font?.source ?? []) + (keyboard.oskFont?.source ?? [])
      for file in fontFiles {
        guard file.hasFontExtension else {
          log.info("Skipping copy of \(file) for keyboard \(keyboard.id) since it is not a font file.")
          continue
        }
        let url = fontDir.appendingPathComponent(file)
        guard FileManager.default.fileExists(atPath: url.path) else {
          log.warning("Font file \(url) for keyboard \(keyboard.id) does not exist")
          continue
        }
        urls.insert(url)
      }
      urlsForKeyboard[keyboard.id] = urls
      userKeyboards[i].version = version.string
    }

    var successfulKeyboards: [String] = []

    // Copy files
    for (keyboardID, urls) in urlsForKeyboard {
      let keyboardDir = storage.keyboardDir(forID: keyboardID)
      do {
        try FileManager.default.createDirectory(at: keyboardDir,
                                                withIntermediateDirectories: true,
                                                attributes: nil)
      } catch {
        log.error("Failed to create keyboard directory at \(keyboardDir)")
        continue
      }

      var successful = true
      for srcURL in urls {
        let dstURL = keyboardDir.appendingPathComponent(srcURL.lastPathComponent)
        do {
          try FileManager.default.copyItem(at: srcURL, to: dstURL)
        } catch {
          log.error("Failed to copy from \(srcURL) to \(dstURL) for keyboard \(keyboardID)")
          successful = false
        }
      }
      if successful {
        successfulKeyboards.append(keyboardID)
        log.info("Succesfully copied keyboard files for keyboard \(keyboardID)")
      }
    }

    // Remove keyboards that were not copied successfully
    let filteredUserKeyboards = userKeyboards.filter { successfulKeyboards.contains($0.id) }
    storage.userDefaults.userKeyboards = filteredUserKeyboards

    // TODO: Remove old directory
  }

  private static func latestKeyboardFileVersion(withID keyboardID: String, dirPath: String) -> Version? {
    guard let dirContents = try? FileManager.default.contentsOfDirectory(atPath: dirPath) else {
      return nil
    }

    var latestVersion: Version?
    for filename in dirContents where filename.hasPrefix("\(keyboardID)-") && filename.hasJavaScriptExtension {
      let dashRange = filename.range(of: "-", options: .backwards)!
      let extensionRange = filename.range(of: ".js", options: .backwards)!
      guard let version = Version(String(filename[dashRange.upperBound..<extensionRange.lowerBound])) else {
        continue
      }

      if let previousMax = latestVersion {
        latestVersion = max(version, previousMax)
      } else {
        latestVersion = version
      }
    }
    return latestVersion
  }
}
