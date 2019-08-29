//
//  Storage.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-11-21.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

// MARK: - Static members
extension Storage {
  /// Storage that is not shared with the other process (app or extension).
  static let nonShared: Storage? = {
    let paths = FileManager.default.urls(for: .libraryDirectory, in: .userDomainMask)
    if paths.isEmpty {
      return nil
    }
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
      log.error("Failed to create subdirectory at \(newDir)")
      return nil
    }
  }
    
  func keyboardDir(forID keyboardID: String) -> URL {
    return keyboardDir.appendingPathComponent(keyboardID)
  }
  
  func lexicalModelDir(forID lexicalModelID: String) -> URL {
    return lexicalModelDir.appendingPathComponent(lexicalModelID)
  }
  
  func keyboardURL(for keyboard: InstallableKeyboard) -> URL {
    return keyboardURL(forID: keyboard.id, version: keyboard.version)
  }
  
  func lexicalModelURL(for lexicalModel: InstallableLexicalModel) -> URL {
    return lexicalModelURL(forID: lexicalModel.id, version: lexicalModel.version)
  }
  
  func lexicalModelPackageURL(for lexicalModel: InstallableLexicalModel) -> URL {
    return lexicalModelPackageURL(forID: lexicalModel.id, version: lexicalModel.version)
  }
  
  func keyboardURL(forID keyboardID: String, version: String) -> URL {
    // We no longer embed version numbers into filenames
    // This allows us to support existing installs and OEM products that
    // still include the version string, at reasonable cost
    // Note, that for installing a keyboard, it will no longer perform this
    // rename with this model, as the target file will obviously not already
    // exist.
    let filename = keyboardDir(forID: keyboardID).appendingPathComponent("\(keyboardID).js")
    if FileManager.default.fileExists(atPath: filename.path) {
      return filename
    }
    return keyboardDir(forID: keyboardID).appendingPathComponent("\(keyboardID)-\(version).js")
  }
  
  func lexicalModelURL(forID lexicalModelID: String, version: String) -> URL {
    return lexicalModelDir(forID: lexicalModelID).appendingPathComponent("\(lexicalModelID)-\(version).model.js")
  }
  
  func lexicalModelPackageURL(forID lexicalModelID: String, version: String, asZip: Bool = true) -> URL {
    // Our unzipping dependency requires a .zip extension to function, so we append that here.
    return lexicalModelDir(forID: lexicalModelID).appendingPathComponent("\(lexicalModelID)-\(version).model.kmp\(asZip ? ".zip" : "")")
  }

  func fontURL(forKeyboardID keyboardID: String, filename: String) -> URL {
    return keyboardDir(forID: keyboardID).appendingPathComponent(filename)
  }

  var keyboardDirs: [URL]? {
    let contents: [URL]
    do {
      contents = try FileManager.default.contentsOfDirectory(at: keyboardDir,
                                                             includingPropertiesForKeys: [.isDirectoryKey])
    } catch {
      log.error("Failed to list contents at \(keyboardDir) with error \(error)")
      return nil
    }
    return contents.filter { url in
      do {
        let values = try url.resourceValues(forKeys: [.isDirectoryKey])
        return values.isDirectory ?? false
      } catch {
        log.error(error)
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
                     dstDir: baseDir)
    try Storage.copy(from: bundle,
                     resourceName: "keymanios.js",
                     dstDir: baseDir)
    // For debug compilations - IF we have a sourcemap file, copy that over too.
    if bundle.url(forResource: "keyman.js.map", withExtension: nil) != nil {
      try Storage.copy(from: bundle,
                       resourceName: "keyman.js.map",
                       dstDir: baseDir)
    }
    try Storage.copy(from: bundle,
                     resourceName: "kmwosk.css",
                     dstDir: baseDir)
    try Storage.copy(from: bundle,
                     resourceName: "keymanweb-osk.ttf",
                     dstDir: baseDir)
    let defaultKeyboardDir = self.keyboardDir(forID: Defaults.keyboard.id)
    let defaultLexicalModelDir = self.lexicalModelDir(forID: Defaults.lexicalModel.id)
    try FileManager.default.createDirectory(at: defaultKeyboardDir, withIntermediateDirectories: true)
    try FileManager.default.createDirectory(at: defaultLexicalModelDir, withIntermediateDirectories: true)
    try Storage.copy(from: bundle,
                     resourceName: "\(Defaults.keyboard.id)-\(Defaults.keyboard.version).js",
                     dstDir: defaultKeyboardDir)
    try Storage.copy(from: bundle,
                     resourceName: "DejaVuSans.ttf",
                     dstDir: defaultKeyboardDir)
    try Storage.copy(from: bundle,
                     resourceName: "\(Defaults.lexicalModel.id)-\(Defaults.lexicalModel.version).model.kmp",
                     dstDir: defaultLexicalModelDir)

    // Perform an auto-install of the lexical model's KMP if not already installed.
    let lexicalModelURLasZIP = Storage.active.lexicalModelPackageURL(forID: Defaults.lexicalModel.id,
                                                                     version: Defaults.lexicalModel.version)
    let lexicalModelURL = Storage.active.lexicalModelPackageURL(forID: Defaults.lexicalModel.id,
                                                                version: Defaults.lexicalModel.version,
                                                                asZip: false)

    // Because of how our .zip dependency works, we need to make the .kmp look like a .zip.  A simple rename will do.
    do {
      try Storage.copyAndExcludeFromBackup(at: lexicalModelURL, to: lexicalModelURLasZIP)
      let downloader = ResourceDownloadQueue()

      // Hijacking the download queue's KMP installer.
      // Issue - requests DL confirmation
      // Issue - in a roundabout way, results in attempted access to Manager.shared during Manager.init, causing crash.
      _ = downloader.installLexicalModelPackage(downloadedPackageFile: lexicalModelURLasZIP)
    } catch {
      log.error("Failed to install the default lexical model from the bundled KMP: \(error)")
    }

  }

  func copyFiles(to dst: Storage) throws {
    if FileManager.default.fileExists(atPath: dst.baseDir.path) {
      log.info("Deleting \(dst.baseDir) for copy from \(baseDir) to \(dst.baseDir)")
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

  private static func copy(from bundle: Bundle, resourceName: String, dstDir: URL) throws {
    guard let srcURL = bundle.url(forResource: resourceName, withExtension: nil) else {
      let message = "Could not locate \(resourceName) in the Keyman bundle"
      throw NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: message])
    }
    let dstURL = dstDir.appendingPathComponent(srcURL.lastPathComponent)

    return try Storage.copyAndExcludeFromBackup(at: srcURL, to: dstURL)
  }

  private static func copyDirectoryContents(at srcDir: URL, to dstDir: URL) throws {
    let srcContents = try FileManager.default.contentsOfDirectory(at: srcDir, includingPropertiesForKeys: [])
    for srcFile in srcContents {
      try Storage.copyAndExcludeFromBackup(at: srcFile, to: dstDir.appendingPathComponent(srcFile.lastPathComponent))
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
  static func copyAndExcludeFromBackup(at src: URL,
                                       to dst: URL,
                                       shouldOverwrite: Bool = true) throws {
    let fm = FileManager.default

    var isDirectory: ObjCBool = false
    let fileExists = fm.fileExists(atPath: src.path, isDirectory: &isDirectory)
    if isDirectory.boolValue {
      return
    }

    if !fileExists {
      let message = "Nothing to copy at \(src)"
      throw NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: message])
    }

    if !fm.fileExists(atPath: dst.path) {
      try fm.copyItem(at: src, to: dst)
    } else if shouldOverwrite {
      try fm.removeItem(at: dst)
      try fm.copyItem(at: src, to: dst)
    } else {
      return
    }
    try Storage.addSkipBackupAttribute(to: dst)
  }
}
