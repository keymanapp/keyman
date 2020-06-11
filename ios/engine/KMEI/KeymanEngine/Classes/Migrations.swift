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
  var resources: [AnyLanguageResource]
  // but how to handle deprecation?

  init(version: Version, resources: [AnyLanguageResource]) {
    self.version = version
    self.resources = resources
  }
}

public enum Migrations {
  static let resourceHistory: [VersionResourceSet] = {
    let font = Font(family: "LatinWeb", source: ["DejaVuSans.ttf"], size: nil)

    let european =  InstallableKeyboard(id: "european",
                                         name: "EuroLatin Keyboard",
                                         languageID: "en",
                                         languageName: "English",
                                         version: "1.7",
                                         isRTL: false,
                                         font: font,
                                         oskFont: nil,
                                         isCustom: false)

    // Default keyboard in version 10.0 (and likely before)
    let european2 = InstallableKeyboard(id: "european2",
                                         name: "EuroLatin2 Keyboard",
                                         languageID: "en",
                                         languageName: "English",
                                         version: "1.6",
                                         isRTL: false,
                                         font: font,
                                         oskFont: nil,
                                         isCustom: false)

    let nrc_en_mtnt_0_1_2 = InstallableLexicalModel(id: "nrc.en.mtnt",
                                                    name: "English dictionary (MTNT)",
                                                    languageID: "en",
                                                    version: "0.1.2",
                                                    isCustom: false)

    let sil_euro_latin = Defaults.keyboard  // We're already storing the exact metadata needed.
    let nrc_en_mtnt = Defaults.lexicalModel

    // Unknown transition point:  european
    // Before v11:  european2
    // Before v12:  sil_euro_latin
    // At v12:      sil_euro_latin + nrc.en.mtnt (lex model)
    var timeline: [VersionResourceSet] = []

    let legacy_resources = VersionResourceSet(version: Version.fallback, resources: [european])
    let v10_resources = VersionResourceSet(version: Version("10.0")!, resources: [european2])
    let v11_resources = VersionResourceSet(version: Version("11.0")!, resources: [sil_euro_latin])
    let v12_resources = VersionResourceSet(version: Version("12.0")!, resources: [sil_euro_latin, nrc_en_mtnt_0_1_2])
    let v13_resources = VersionResourceSet(version: Version("13.0.65")!, resources: [sil_euro_latin, nrc_en_mtnt])

    timeline.append(legacy_resources)
    timeline.append(v10_resources)
    timeline.append(v11_resources)
    timeline.append(v12_resources)
    timeline.append(v13_resources)

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

    // Version-based migrations
    if let version = engineVersion {
      if version < Version.fileBrowserImplemented {
        do {
          try migrateDocumentsFromPreBrowser()
        } catch {
          log.error("Could not migrate Documents directory contents: \(error)")
        }
      } else {
        log.info("Documents directory structure compatible with \(Version.fileBrowserImplemented)")
      }
    }

    storage.userDefaults.synchronize()
  }

  static func detectLegacyKeymanVersion() -> [Version] {
    // While the 'key' used to track version info existed before v12, it was unused until then.
    log.info("Prior engine version unknown; attepting to auto-detect.")

    // Detect possible version matches.
    let userResources = Storage.active.userDefaults.userResources ?? []

    // If there are no pre-existing resources and we need to detect a version, this is a fresh install.
    if userResources.count == 0 {
      return [Version.freshInstall]
    }
    let possibleMatches: [Version] = resourceHistory.compactMap { set in
      if set.version < Version("12.0")! {
        // Are all of the version's default resources present?
        let match = set.resources.allSatisfy { res in
          return userResources.contains(where: { res2 in
            return res.id == res2.id && res.languageID == res2.languageID
          })
        }

        // If so, report that we can match this version.
        if match {
          return set.version
        } else {
          // No match; don't consider this version.
          return nil
        }
      } else {
        // If it were at least version 12.0, the version would be specified and we wouldn't be guessing.
        return nil
      }
    }

    return possibleMatches
  }

  // The only part actually visible outside of KeymanEngine.
  public internal(set) static var engineVersion: Version? {
    get {
      return Storage.active.userDefaults.lastEngineVersion
    }

    set(value) {
      Storage.active.userDefaults.lastEngineVersion = value!
    }
  }

  static func updateResources(storage: Storage) {
    var lastVersion = engineVersion
    if (lastVersion ?? Version.fallback) >= Version.latestFeature {
      // We're either current or have just been downgraded; no need to do modify resources.
      // If it's a downgrade, it's near-certainly a testing environment.
      return
    }

    // Legacy check - what was the old version?  If it's older than 12.0,
    // we don't actually know.
    if (lastVersion ?? Version.fallback) < Version.firstTracked {
      let possibleMatches: [Version] = detectLegacyKeymanVersion()

      // Now we have a list of possible original versions of the Keyman app.
      if possibleMatches.count > 1 {
        // If more than one case matches, the user never cleaned out deprecated keyboards.
        // They're probably fine with it, so the easiest solution is to give them one more
        // and make it the updated default.

        // No de-installs here; just additional install later in the method.
      } else if possibleMatches.count == 1 {
        // Simplest case - remove old default material so that we can insert the updated
        // resources in a later part of the method.
        lastVersion = possibleMatches[0]
      } else { // if possibleMatches.count == 0
        // The user has previously decided that they don't want our defaults and has already
        // significantly customized their resource selection.  No need to 'update' anything.
        return
      }
    }

    if lastVersion != nil && lastVersion != Version.freshInstall {
      // Time to deinstall the old version's resources.
      // First, find the most recent version with a listed history.
      let possibleHistories: [VersionResourceSet] = resourceHistory.compactMap { set in
        if set.version <= lastVersion! {
          return set
        } else {
          return nil
        }
      }

      // Assumes the history definition is in ascending Version order; takes the last in the list
      // as the correct "old version" resource set.  This allows covering gaps,
      // such as for a 'plain' 13.0 prior install.
      let resources = possibleHistories[possibleHistories.count-1].resources

      resources.forEach { res in
        if let kbd = res as? InstallableKeyboard {
          var userKeyboards = Storage.active.userDefaults.userKeyboards

          // Does not remove the deprecated keyboard's files - just the registration.
          userKeyboards?.removeAll(where: { kbd2 in
            return kbd.id == kbd2.id && kbd.languageID == kbd2.languageID
          })
          Storage.active.userDefaults.userKeyboards = userKeyboards
        } else if let lex = res as? InstallableLexicalModel {
          var userModels = Storage.active.userDefaults.userLexicalModels

          // Parallels the issue with deprecated files for keyboards.
          userModels?.removeAll(where: { lex2 in
            return lex.id == lex2.id && lex.languageID == lex2.languageID
          })
          Storage.active.userDefaults.userLexicalModels = userModels
        } // else Not yet implemented
      }
    }

    // Now to install the new version's resources.
    var userKeyboards = Storage.active.userDefaults.userKeyboards ?? []
    var userModels = Storage.active.userDefaults.userLexicalModels ?? []
    let defaultsNeedBackup = (lastVersion ?? Version.fallback) < Version.defaultsNeedBackup
    var installKbd = false
    var installLex = false

    // Don't add the keyboard a second time if it's already installed and backed up.  Can happen
    // if multiple Keyman versions match.
    if !userKeyboards.contains(where: { kbd in
      kbd.id == Defaults.keyboard.id && kbd.languageID == Defaults.keyboard.languageID
    }) {
      userKeyboards = [Defaults.keyboard] + userKeyboards  // Make sure the default goes in the first slot!
      Storage.active.userDefaults.userKeyboards = userKeyboards

      installKbd = true
    }
    if(defaultsNeedBackup || installKbd) {
      do {
        try Storage.active.installDefaultKeyboard(from: Resources.bundle)
      } catch {
        log.error("Failed to copy default keyboard from bundle: \(error)")
      }
    }

    if !userModels.contains(where: { lex in
      lex.id == Defaults.lexicalModel.id && lex.languageID == Defaults.lexicalModel.languageID
    }) {
      userModels = [Defaults.lexicalModel] + userModels
      Storage.active.userDefaults.userLexicalModels = userModels

      installLex = true
    }
    if(defaultsNeedBackup || installLex) {
      do {
        try Storage.active.installDefaultLexicalModel(from: Resources.bundle)
      } catch {
        log.error("Failed to copy default lexical model from bundle: \(error)")
      }
    }

    // Store the version we just upgraded to.
    storage.userDefaults.lastEngineVersion = Version.latestFeature
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
      urls.insert(languageDir.appendingPathComponent("\(keyboard.id)-\(version.plainString).js"))

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
      userKeyboards[i].version = version.plainString
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

  static func migrateDocumentsFromPreBrowser() throws {
    log.info("Cleaning Documents folder due to 12.0 installation artifacts")

    // Actually DO it.
    let documentFolderURL = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
    // The String-based version will break, at least in Simulator.
    let contents = try FileManager.default.contentsOfDirectory(at: documentFolderURL, includingPropertiesForKeys: nil, options: [])

    try contents.forEach { fileURL in
      // 12.0: extracts .kmp files by first putting them in Documents and giving
      // them this suffix.
      if fileURL.lastPathComponent.hasSuffix(".kmp.zip") {
        // Renames the .kmp.zip files back to their original .kmp filename.
        let destFile = fileURL.lastPathComponent.replacingOccurrences(of: ".kmp.zip", with: ".kmp")
        let destURL = fileURL.deletingLastPathComponent().appendingPathComponent(destFile)

        log.debug("\(fileURL) -> \(destURL)")
        try FileManager.default.moveItem(at: fileURL, to: destURL)
      } else if fileURL.lastPathComponent == "temp" {
        // Removes the 'temp' installation directory; that shouldn't be visible to users.
        log.debug("Deleting directory: \(fileURL)")
        try FileManager.default.removeItem(at: fileURL)
      } else if fileURL.lastPathComponent.hasSuffix(".kmp") {
        // Do nothing; this file is fine.
      } else {
        log.info("Unexpected file found in documents folder during upgrade: \(fileURL)")
      }
    }
  }

  static func migrateCloudResourcesToKMPFormat() throws {
    let cachedKMPsDirectory = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]

    do {
      let files = try FileManager.default.contentsOfDirectory(atPath: cachedKMPsDirectory.path)
      let kmpFiles = files.filter { $0.suffix(4) == ".kmp" }

      // Step 1 - analyze all locally-cached KMPs for possible resource sources.
      let cachedPackages: [KeymanPackage] = kmpFiles.compactMap { file in
        let filePath = cachedKMPsDirectory.appendingPathComponent(file)
        do {
          return try ResourceFileManager.shared.prepareKMPInstall(from: filePath)
        } catch {
          log.debug("Error occurred when processing existing packages during cloud -> KMP migration: \(String(describing: error))")
          return nil as KeymanPackage?
        }
      }

      let userDefaults = Storage.active.userDefaults
//      let userKeyboards = userDefaults.userKeyboards
//      let userLexicalModels = userDefaults.userLexicalModels
    } catch {
      // TODO:
    }
  }
}
