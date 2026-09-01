/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-02-24
 *
 * Object that represents a Keyman keyboard
 * Each keyboard can be marked as enabled/disabled
 *
 */

import Foundation
import AppKit
import OSLog

public class Keyboard: Identifiable, Hashable, Equatable {
  
  public var id = UUID()
  public var enabled: Bool
  public var name: String
  public let oskFont: String?
  public let displayFont: String?
  public var keyboardId: String
  // a key to uniquely identify the keyboard
  // in the UserDefaults this key is used for the selected Keyboard and enabled keyboards properties
  // the key is in the form "/[package directory]/[package name].kmx"
  // for example, "/khmer_angkor/khmer_angkor.kmx"
  public let keyboardKey: String
  
  public init(keyboardSource: KeyboardSource, packageDirectoryName: String) {
    self.enabled = true
    self.name = keyboardSource.name
    self.oskFont = keyboardSource.oskFont
    self.displayFont = keyboardSource.displayFont
    self.keyboardId = keyboardSource.id
    self.keyboardKey = Keyboard.deriveKeyboardSettingsKey(from: packageDirectoryName, keyboardId: self.keyboardId)
  }
  
  /**
   * initializer that does not rely on package source -- provided to create unit test data
   */
  public init(name: String, oskFont: String? = nil, displayFont: String? = nil, keyboardId: String, packageDirectoryName: String, enabled: Bool) {
    self.name = name
    self.oskFont = oskFont
    self.displayFont = displayFont
    self.keyboardId = keyboardId
    self.enabled = enabled
    self.keyboardKey = Keyboard.deriveKeyboardSettingsKey(from: packageDirectoryName, keyboardId: self.keyboardId)
  }
  
  /**
   * provided for Equatable conformance
   */
  public static func == (lhs: Keyboard, rhs: Keyboard) -> Bool {
    return lhs.keyboardId == rhs.keyboardId && lhs.enabled == rhs.enabled
  }
  
  /**
   * provided for Hashable conformance
   */
  public func hash(into hasher: inout Hasher) {
    hasher.combine(keyboardId)
  }
  
  /**
   * generate the url for the keyboard's kmx file
   */
  func deriveKmxFileUrl(from keyboardDirectory: URL) -> URL {
    return keyboardDirectory.appendingPathComponent("\(keyboardId).kmx")
  }

  /**
   * generate the keyboard's key
   * see the above comment for `keyboardKey` for a description of the format of the key
   */
 static func deriveKeyboardSettingsKey(from packageDirectoryName: String, keyboardId: String) -> String {
    // get filename from keyboardId
    let kmxFilename = "\(keyboardId).kmx"
    
    let settingsKey = "/\(packageDirectoryName)/\(kmxFilename)"
    
    return settingsKey
  }
  
  /**
   * validate whether a corresponding kmx file exists for this keyboard
   */
  public func validateKmxFile(in packageDirectory: URL) throws {
    let kmxFilePath = self.deriveKmxFileUrl(from: packageDirectory).path
    if !FileManager.default.fileExists(atPath: kmxFilePath) {
      Logger.data.error("error: could not find kmx file \(kmxFilePath, privacy: .public)")
      throw LoadPackageError.missingKmxFile
    }
  }
}
