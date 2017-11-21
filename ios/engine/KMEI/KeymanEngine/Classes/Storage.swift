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
      Manager.shared.kmLog("applicationGroupIdentifier is unset", checkDebugPrinting: false)
      return nil
    }
    guard let url = FileManager.default.containerURL(forSecurityApplicationGroupIdentifier: groupID),
      let userDefaults = UserDefaults(suiteName: groupID)
    else {
      Manager.shared.kmLog("Error initializing shared Storage for groupID: \(groupID)", checkDebugPrinting: false)
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

public class Storage {
  public let baseDir: URL
  public let languageDir: URL
  public let fontDir: URL
  public let userDefaults: UserDefaults

  private init?(baseURL: URL, userDefaults: UserDefaults) {
    guard
      let baseDir = Storage.createSubdirectory(baseDir: baseURL, name: "keyman"),
      let languageDir = Storage.createSubdirectory(baseDir: baseDir, name: "languages"),
      let fontDir = Storage.createSubdirectory(baseDir: baseDir, name: "fonts") else {
      return nil
    }

    self.baseDir = baseDir
    self.languageDir = languageDir
    self.fontDir = fontDir
    self.userDefaults = userDefaults
  }

  private static func createSubdirectory(baseDir: URL?, name: String) -> URL? {
    guard let baseDir = baseDir else {
      return nil
    }
    let newDir = baseDir.appendingPathComponent(name)
    do {
      try FileManager.default.createDirectory(at: newDir,
                                              withIntermediateDirectories: true,
                                              attributes: nil)
      return newDir
    } catch {
      Manager.shared.kmLog("Error creating subdirectory: \(error)", checkDebugPrinting: false)
      return nil
    }
  }
}
