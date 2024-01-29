//
//  Storage.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-11-21.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation
import Sentry
import os.log

// MARK: - Static members
extension Storage {
  /// Storage that is not shared with the other process (app or extension).
  static let nonShared: Storage? = {
    let paths = FileManager.default.urls(for: .libraryDirectory, in: .userDomainMask)
    if paths.isEmpty {
      return nil
    }
    let message = "Storage.nonShared:  using path \(paths[0])"
    os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
    SentryManager.breadcrumb(message)
    return Storage(baseURL: paths[0], userDefaults: UserDefaults.standard)
  }()

  /// Storage shared between the app and app extension.
  /// - Important: `Manager.applicationGroupIdentifier` must be set before accessing this. Modifications to
  ///   `Manager.applicationGroupIdentifier` will not be reflected here.
  static let shared: Storage? = {
    guard let groupID = Manager.applicationGroupIdentifier else {
      return nil
    }
    guard let url = FileManager.default.containerURL(forSecurityApplicationGroupIdentifier: groupID),
      let userDefaults = UserDefaults(suiteName: groupID)
    else {
      return nil
    }
    return Storage(baseURL: url, userDefaults: userDefaults)
  }()

  /// Active storage to be used for app data. This is `shared` if possible, otherwise `nonShared`.
  /// - Important: `Manager.applicationGroupIdentifier` must be set before accessing this. Modifications to
  ///   `Manager.applicationGroupIdentifier` will not be reflected here.
  static let active: Storage! = {
    if let shared = shared {
      return shared
    }
    return nonShared
  }()
}

class Storage {
  let baseDir: URL
  let keyboardDir: URL
  let lexicalModelDir: URL
  let userDefaults: UserDefaults

  let kmwURL: URL
  let specialOSKFontURL: URL

  private init?(baseURL: URL, userDefaults: UserDefaults) {
    guard
      let baseDir = Storage.createSubdirectory(baseDir: baseURL, name: "keyman"),
      let keyboardDir = Storage.createSubdirectory(baseDir: baseDir, name: "keyboards"),
      let lexicalModelDir = Storage.createSubdirectory(baseDir: baseDir, name: "lexicalModels")
    else {
      return nil
    }

    self.baseDir = baseDir
    self.keyboardDir = keyboardDir
    self.lexicalModelDir = lexicalModelDir
    self.userDefaults = userDefaults
    kmwURL = baseDir.appendingPathComponent(Resources.kmwFilename)
    specialOSKFontURL = baseDir.appendingPathComponent(Resources.oskFontFilename)
  }

  private static func createSubdirectory(baseDir: URL, name: String) -> URL? {
    let newDir = baseDir.appendingPathComponent(name)
    do {
      try FileManager.default.createDirectory(at: newDir,
                                              withIntermediateDirectories: true,
                                              attributes: nil)
      return newDir
    } catch {
      let message = ("Failed to create subdirectory at \(newDir)")
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
      SentryManager.capture(error, message:message)
      return nil
    }
  }

  // Note:  legacy* methods are utilized for migrations through 14.0, at which point
  //        KeymanEngine transitioned entirely to using LanguageResource instances
  //        for path lookups.

  func legacyKeyboardDir(forID keyboardID: String) -> URL {
    return keyboardDir.appendingPathComponent(keyboardID)
  }

  func legacyLexicalModelDir(forID lexicalModelID: String) -> URL {
    return lexicalModelDir.appendingPathComponent(lexicalModelID)
  }

  func packageDir(forID packageID: String, ofType resourceType: LanguageResourceType) -> URL {
    switch resourceType {
      case .keyboard:
        return keyboardDir.appendingPathComponent(packageID)
      case .lexicalModel:
        return lexicalModelDir.appendingPathComponent(packageID)
    }
  }

  func packageDir(forKey key: KeymanPackage.Key) -> URL {
    return packageDir(forID: key.id, ofType: key.type)
  }

  func packageDir(for package: KeymanPackage) -> URL? {
    return packageDir(forID: package.id, ofType: package.resourceType())
  }

  func resourceDir(for resource: AnyLanguageResource) -> URL? {
    if let keyboard = resource as? InstallableKeyboard {
      return packageDir(forID: keyboard.packageID ?? keyboard.id, ofType: .keyboard)
    } else if let lexicalModel = resource as? InstallableLexicalModel {
      return packageDir(forID: lexicalModel.packageID ?? lexicalModel.id, ofType: .lexicalModel)
    } else {
      return nil
    }
  }

  func keyboardURL(for keyboard: InstallableKeyboard) -> URL {
    return resourceDir(for: keyboard)!.appendingPathComponent("\(keyboard.id).js")
  }

  func lexicalModelURL(for lexicalModel: InstallableLexicalModel) -> URL {
    return resourceDir(for: lexicalModel)!.appendingPathComponent("\(lexicalModel.id).model.js")
  }

  // Facilitates abstractions based on the LanguageResource protocol.
  func resourceURL(for resource: AnyLanguageResource) -> URL? {
    if let keyboard = resource as? InstallableKeyboard {
      return keyboardURL(for: keyboard)
    } else if let lexicalModel = resource as? InstallableLexicalModel {
      return lexicalModelURL(for: lexicalModel)
    } else {
      return nil
    }
  }

  // Facilitates abstractions based on the LanguageResource protocol.
  func legacyResourceURL(for resource: AnyLanguageResource) -> URL? {
    if let keyboard = resource as? InstallableKeyboard {
      return legacyKeyboardURL(forID: keyboard.id, version: keyboard.version)
    } else if let lexicalModel = resource as? InstallableLexicalModel {
      return legacyLexicalModelURL(forID: lexicalModel.id, version: lexicalModel.version)
    } else {
      return nil
    }
  }

  func lexicalModelPackageURL(for lexicalModel: InstallableLexicalModel) -> URL {
    return resourceDir(for: lexicalModel)!.appendingPathComponent("\(lexicalModel.id).model.kmp")
  }

  // TODO:  Once we drop cloud-keyboard downloads, remove this function.
  func cloudKeyboardURL(forID keyboardID: String) -> URL {
    // For now, we treat cloud keyboards as KMP-less packages, generating a kmp.json after download.
    return packageDir(forID: keyboardID, ofType: .keyboard).appendingPathComponent("\(keyboardID).js")
  }

  func legacyKeyboardURL(forID keyboardID: String, version: String) -> URL {
    // We no longer embed version numbers into filenames
    // This allows us to support existing installs and OEM products that
    // still include the version string, at reasonable cost
    // Note, that for installing a keyboard, it will no longer perform this
    // rename with this model, as the target file will obviously not already
    // exist.
    let filename = legacyKeyboardDir(forID: keyboardID).appendingPathComponent("\(keyboardID).js")
    if FileManager.default.fileExists(atPath: filename.path) {
      return filename
    }
    return legacyKeyboardDir(forID: keyboardID).appendingPathComponent("\(keyboardID)-\(version).js")
  }

  func legacyLexicalModelURL(forID lexicalModelID: String, version: String) -> URL {
    return legacyLexicalModelDir(forID: lexicalModelID).appendingPathComponent("\(lexicalModelID)-\(version).model.js")
  }

  func legacyLexicalModelPackageURL(forID lexicalModelID: String, version: String, asZip: Bool = false) -> URL {
    // Our unzipping dependency requires a .zip extension to function, so we append that here.
    return legacyLexicalModelDir(forID: lexicalModelID).appendingPathComponent("\(lexicalModelID)-\(version).model.kmp\(asZip ? ".zip" : "")")
  }

  func fontURL(forResource resource: AnyLanguageResource, filename: String) -> URL? {
    return resourceDir(for: resource)?.appendingPathComponent(filename)
  }

  var keyboardDirs: [URL]? {
    let contents: [URL]
    do {
      contents = try FileManager.default.contentsOfDirectory(at: keyboardDir,
                                                             includingPropertiesForKeys: [.isDirectoryKey])
    } catch {
      let message = ("Failed to list contents at \(keyboardDir) with error \(error)")
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
      SentryManager.capture(error, message:message)
      return nil
    }
    return contents.filter { url in
      do {
        let values = try url.resourceValues(forKeys: [.isDirectoryKey])
        return values.isDirectory ?? false
      } catch {
        let message = "\(String(describing: error))"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
        SentryManager.capture(error)
        return false
      }
    }
  }
}

// MARK: - Copying
extension Storage {
  func copyKMWFiles(from bundle: Bundle) throws {
    try Storage.copy(from: bundle,
                     resourceName: Resources.kmwFilename,
                     dstDir: baseDir,
                     excludeFromBackup: true)
    try Storage.copy(from: bundle,
                     resourceName: "ios-host.js",
                     dstDir: baseDir,
                     excludeFromBackup: true)
    try Storage.copy(from: bundle,
                     resourceName: "keymanweb-webview.js",
                     dstDir: baseDir,
                     excludeFromBackup: true)
    try Storage.copy(from: bundle,
                     resourceName: "sentry.min.js",
                     dstDir: baseDir,
                     excludeFromBackup: true)
    try Storage.copy(from: bundle,
                     resourceName: "keyman-sentry.js",
                     dstDir: baseDir,
                     excludeFromBackup: true)
    try Storage.copy(from: bundle,
                     resourceName: "kmwosk.css",
                     dstDir: baseDir,
                     excludeFromBackup: true)
    try Storage.copy(from: bundle,
                     resourceName: "keymanweb-osk.ttf",
                     dstDir: baseDir,
                     excludeFromBackup: true)
  }

  func installDefaultKeyboard(from bundle: Bundle) throws {
    let defaultKeyboardDir = self.resourceDir(for: Defaults.keyboard)!
    try FileManager.default.createDirectory(at: defaultKeyboardDir, withIntermediateDirectories: true)

    // Since we only want to do this installation the first time (rather than constantly force-reinstalling
    // the resource), we don't want this excluded from backup.
    try Storage.copy(from: bundle,
                     resourceName: "\(Defaults.keyboard.id).kmp",
                     dstDir: defaultKeyboardDir,
                     excludeFromBackup: false)

    do {
      // Perform an auto-install of the keyboard's KMP if not already installed.
      let defaultKMPFile = defaultKeyboardDir.appendingPathComponent("\(Defaults.keyboard.id).kmp")
      let package = try ResourceFileManager.shared.prepareKMPInstall(from: defaultKMPFile) as! KeyboardKeymanPackage
      try ResourceFileManager.shared.install(resourceWithID: Defaults.keyboard.fullID, from: package)
    } catch {
      let message = "Failed to install the default keyboard from the bundled KMP: \(error)"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
      SentryManager.capture(error, message:message)
    }
  }

  func installDefaultLexicalModel(from bundle: Bundle) throws {
    let defaultLexicalModelDir = self.resourceDir(for: Defaults.lexicalModel)!

    try FileManager.default.createDirectory(at: defaultLexicalModelDir, withIntermediateDirectories: true)

    // Since we only want to do this installation the first time (rather than constantly force-reinstalling
    // the resource), we don't want this excluded from backup.
    try Storage.copy(from: bundle,
                     resourceName: "\(Defaults.lexicalModel.id).model.kmp",
                     dstDir: defaultLexicalModelDir,
                     excludeFromBackup: false)

    do {
      // Perform an auto-install of the lexical model's KMP if not already installed.
      let defaultKMPFile = defaultLexicalModelDir.appendingPathComponent("\(Defaults.lexicalModel.id).model.kmp")
      let package = try ResourceFileManager.shared.prepareKMPInstall(from: defaultKMPFile) as! LexicalModelKeymanPackage

      // Install all languages for the model, not just the default-listed one.
      try ResourceFileManager.shared.install(resourcesWithIDs: package.installables[0].map { $0.fullID }, from: package)
    } catch {
      let message = "Failed to install the default lexical model from the bundled KMP: \(error)"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
      SentryManager.capture(error, message:message)
    }
  }

  func copyFiles(to dst: Storage) throws {
    if FileManager.default.fileExists(atPath: dst.baseDir.path) {
      let message = "Deleting \(dst.baseDir) for copy from \(baseDir) to \(dst.baseDir)"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
      SentryManager.breadcrumb(message)
      try FileManager.default.removeItem(at: dst.baseDir)
    }
    try FileManager.default.copyItem(at: baseDir, to: dst.baseDir)
  }

  func copyUserDefaults(to dst: Storage, withKeys keys: [String], shouldOverwrite: Bool) {
    for key in keys {
      if shouldOverwrite || dst.userDefaults.object(forKey: key) == nil {
        dst.userDefaults.set(userDefaults.object(forKey: key), forKey: key)
      }
    }
    dst.userDefaults.synchronize()
  }

  private static func copy(from bundle: Bundle, resourceName: String, dstDir: URL, excludeFromBackup: Bool = false) throws {
    guard let srcURL = bundle.url(forResource: resourceName, withExtension: nil) else {
      throw KeymanError.missingFile(resourceName, true)
    }
    let dstURL = dstDir.appendingPathComponent(srcURL.lastPathComponent)

    return try Storage.copy(at: srcURL, to: dstURL, excludeFromBackup: true)
  }

  private static func copyDirectoryContents(at srcDir: URL, to dstDir: URL) throws {
    let srcContents = try FileManager.default.contentsOfDirectory(at: srcDir, includingPropertiesForKeys: [])
    for srcFile in srcContents {
      try Storage.copy(at: srcFile, to: dstDir.appendingPathComponent(srcFile.lastPathComponent), excludeFromBackup: true)
    }
  }

  private static func addSkipBackupAttribute(to url: URL) throws {
    var url = url
    var resourceValues = URLResourceValues()
    resourceValues.isExcludedFromBackup = true

    // Writes values to the backing store. It is not only mutating the URL in memory.
    try url.setResourceValues(resourceValues)
  }

  /// Copy and exclude from iCloud backup if `src` is newer than `dst`. Does nothing for a directory.
  static func copy(at src: URL,
                   to dst: URL,
                   shouldOverwrite: Bool = true,
                   excludeFromBackup: Bool = false) throws {
    let fm = FileManager.default

    var isDirectory: ObjCBool = false
    let fileExists = fm.fileExists(atPath: src.path, isDirectory: &isDirectory)
    if isDirectory.boolValue {
      return
    }

    if !fileExists {
      throw KeymanError.missingFile(src.path, false)
    }

    if !fm.fileExists(atPath: dst.path) {
      try fm.copyItem(at: src, to: dst)
    } else if shouldOverwrite {
      try fm.removeItem(at: dst)
      try fm.copyItem(at: src, to: dst)
    } else {
      return
    }

    if excludeFromBackup {
      try Storage.addSkipBackupAttribute(to: dst)
    }
  }
}
