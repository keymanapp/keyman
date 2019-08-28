//
//  Migrations.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-12-08.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

private enum MigrationLevel {
  static let initial = 0
  static let migratedUserDefaultsToStructs = 10
  static let migratedForKMP = 20
}

struct VersionResourceSet {
  var version: Version
  var resources: [LanguageResource]
  // but how to handle deprecation?

  init(version: Version, resources: [LanguageResource]) {
    self.version = version
    self.resources = resources
  }
}

enum Migrations {
  static let resourceHistory: [VersionResourceSet] = {
    //let eurolatin2 =
    let sil_euro_latin = Defaults.keyboard  // We're already storing the exact metadata needed.

    // Unknown transition point:  european
    // Before v11:  european2
    // Before v12:  sil_euro_latin
    // At v12:      sil_euro_latin + nrc.en.mtnt (lex model)
    var timeline: [VersionResourceSet] = []

    let v12_resources = VersionResourceSet(version: Version("12.0")!, resources: [sil_euro_latin])

    timeline.append(v12_resources)

    return timeline
  }()

  static func migrate(storage: Storage) {
    if storage.userDefaults.migrationLevel < MigrationLevel.migratedUserDefaultsToStructs {
      migrateUserDefaultsToStructs(storage: storage)
      storage.userDefaults.migrationLevel = MigrationLevel.migratedUserDefaultsToStructs
    } else {
      log.info("UserDefaults migration to structs already performed. Skipping.")
    }
    if storage.userDefaults.migrationLevel < MigrationLevel.migratedForKMP {
      migrateForKMP(storage: storage)
      storage.userDefaults.migrationLevel = MigrationLevel.migratedForKMP
    } else {
      log.info("KMP directory migration already performed. Skipping.")
    }
    storage.userDefaults.synchronize()
  }

  static func updateResources(storage: Storage) {
    let lastVersion = storage.userDefaults.lastEngineVersion ?? Version.fallback
    if lastVersion < Version("12.0")! {
      // While the key to track version info existed before v12, it was unused until then.
      log.info("Upgrading from pre-tracked version of KeymanEngine to v12.0")
    }

    // Check against resource history.

    let history = resourceHistory

    // FIXME:  Once the work is done, the following line should be active, not commented.
    // storage.userDefaults.lastEngineVersion = Version("12.0")
  }

  static func migrateUserDefaultsToStructs(storage: Storage) {
    guard let userKeyboardObject = storage.userDefaults.object(forKey: Key.userKeyboardsList),
      let currentKeyboardObject = storage.userDefaults.object(forKey: Key.userCurrentKeyboard)
    else {
      log.info("User keyboard list or current keyboard missing. Skipping migration.")
      return
    }
    guard let oldUserKeyboards = userKeyboardObject as? [[String: String]],
      let oldCurrentKeyboard = currentKeyboardObject as? [String: String]
    else {
      log.error("User keyboard list or current keyboard has an unexpected type")
      return
    }

    let userKeyboards = oldUserKeyboards.compactMap { installableKeyboard(from: $0) }
    let currentKeyboardID = fullKeyboardID(from: oldCurrentKeyboard)

    storage.userDefaults.userKeyboards = userKeyboards
    if userKeyboards.contains(where: { $0.fullID == currentKeyboardID }) {
      storage.userDefaults.currentKeyboardID = currentKeyboardID
    } else {
      storage.userDefaults.currentKeyboardID = nil
    }
  }

  private static func installableKeyboard(from kbDict: [String: String]) -> InstallableKeyboard? {
    log.debug("Migrating keyboard dictionary: \(kbDict)")
    guard let id = kbDict["kbId"],
      let name = kbDict["kbName"],
      let languageID = kbDict["langId"],
      let languageName = kbDict["langName"],
      let version = kbDict["version"]
    else {
      log.error("Missing required fields in keyboard dictionary: \(kbDict)")
      return nil
    }
    let rtl = kbDict["rtl"] == "Y"
    let isCustom = kbDict["CustomKeyboard"] == "Y"
    let displayFont = font(from: kbDict["font"])
    let oskFont = font(from: kbDict["oskFont"])
    let kb = InstallableKeyboard(id: id,
                                 name: name,
                                 languageID: languageID,
                                 languageName: languageName,
                                 version: version,
                                 isRTL: rtl,
                                 font: displayFont,
                                 oskFont: oskFont,
                                 isCustom: isCustom)
    log.debug("Migrated keyboard dictionary to keyboard \(kb)")
    return kb
  }

  private static func fullKeyboardID(from kbDict: [String: String]) -> FullKeyboardID? {
    log.debug("Migrating keyboard dictionary to FullKeyboardID: \(kbDict)")
    guard let keyboardID = kbDict["kbId"],
      let languageID = kbDict["langId"]
    else {
      log.error("Missing required fields in keyboard dictionary for FullKeyboardID: \(kbDict)")
      return nil
    }
    let id = FullKeyboardID(keyboardID: keyboardID, languageID: languageID)
    log.debug("Migrated keyboard dictionary to \(id)")
    return id
  }

  private static func font(from jsonString: String?) -> Font? {
    guard let jsonString = jsonString else {
      return nil
    }
    guard let data = jsonString.data(using: .utf8) else {
      log.error("Failed to encode string: \(jsonString)")
      return nil
    }
    guard let fontDict = (try? JSONSerialization.jsonObject(with: data, options: [])) as? [String: Any] else {
      log.error("Error parsing String as JSON: \(jsonString)")
      return nil
    }
    guard let family = fontDict["family"] as? String else {
      log.error("Missing 'family' String: \(fontDict)")
      return nil
    }
    let files: [String]
    if let filesString = fontDict["files"] as? String {
      files = [filesString]
    } else if let filesArray = fontDict["files"] as? [String] {
      files = filesArray
    } else {
      log.error("Missing 'files': \(fontDict)")
      return nil
    }
    return Font(family: family, source: files)
  }

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
