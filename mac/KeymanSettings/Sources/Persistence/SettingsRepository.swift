/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2025-12-10
 *
 * SettingsRepository is responsible for reading, writing and removing
 * settings stored in the UserDefaults
 *
 */


import Foundation

public struct SettingsRepository {
  
  fileprivate let keyboardsDirectoryName = "Keyman-Keyboards"
  // keyman 18
  fileprivate(set) var keyman18ApplicationSupportSubDirectory: URL?
  

  public init() {
  }
}
