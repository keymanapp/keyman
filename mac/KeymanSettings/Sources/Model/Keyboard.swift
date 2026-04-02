/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-02-24
 *
 * Wrapper object that represents a Keyman keyboard as loaded from a .KMP file
 * Mostly immutable, but can be marked as enabled/disabled
 *
 */


import Foundation
import AppKit

public class Keyboard: Identifiable, Hashable, Equatable {
  
  public var id = UUID()
  public var enabled: Bool
  public var name: String
  public var keyboardId: String
  // the directory we are reading the keyboard from
  public var keyboardDirectoryUrl: URL
  // the URL of the .kmx file for the package
  public let kmxFileUrl: URL
  // the UserDefaults key for this package, used for the selected Keyboard and active keyboards
  // the key is in the form "/[package directory]/[package name].kmx"
  // for example, "/khmer_angkor/khmer_angkor.kmx"
  public let keyboardSettingsKey: String

  public init(keyboardSource: KeyboardSource, directoryUrl: URL) {
    self.enabled = true
    self.name = keyboardSource.name
    self.keyboardId = keyboardSource.id
    self.keyboardDirectoryUrl = directoryUrl
    self.kmxFileUrl = Keyboard.deriveKmxFileUrl(from: self.keyboardDirectoryUrl, keyboardId: self.keyboardId)
    self.keyboardSettingsKey = Keyboard.deriveKeyboardSettingsKey(from: self.keyboardDirectoryUrl, keyboardId: self.keyboardId)

//    ConfigLogger.shared.testLogger.debug("keyboard created for: \(keyboardSource.id)")
    print("keyboard created for: \(keyboardSource.id) \r   with kmxFileUrl: \(self.kmxFileUrl) \r   and settingsKey: \(self.keyboardSettingsKey)")
  }

  public static func == (lhs: Keyboard, rhs: Keyboard) -> Bool {
    return lhs.keyboardId == rhs.keyboardId && lhs.enabled == rhs.enabled
  }

  public func hash(into hasher: inout Hasher) {
      hasher.combine(keyboardId)
  }

  static func deriveKmxFileUrl(from keyboardDirectory: URL, keyboardId: String) -> URL {
    return keyboardDirectory.appendingPathComponent("\(keyboardId).kmx")
  }
  
  static func deriveKeyboardSettingsKey(from keyboardDirectory: URL, keyboardId: String) -> String {
    // get parent directory
    let parentDirectoryName = keyboardDirectory.lastPathComponent

    // get filename from keyboardId
    let kmxFilename = "\(keyboardId).kmx"

    let settingsKey = "/\(parentDirectoryName)/\(kmxFilename)"
    print("settingsKey: \(settingsKey)")

    return settingsKey
  }
  
  // TODO: throw/handle error if no kmx file found in directory
  public func validateKmxFile() -> Bool {
    let fileManager = FileManager.default
    if fileManager.fileExists(atPath: self.kmxFileUrl.path) {
      return true
    } else {
      print("   *** Error: could not find kmx file \(self.kmxFileUrl.path)")
      return false
    }
  }
  
  public func findKmxFile(in keyboardDirectoryUrl: URL) -> URL? {
    var kmxFileUrl: URL? = nil
    let fileManager = FileManager.default
    
    do {
      let fileURLs = try fileManager.contentsOfDirectory(at: keyboardDirectoryUrl, includingPropertiesForKeys: nil)
      let filteredFiles = fileURLs.filter { $0.pathExtension == "kmx" }
      
      if (!filteredFiles.isEmpty) {
        kmxFileUrl = filteredFiles.first!
      }
    } catch {
      print("Error reading directory: \(error)")
    }
    
    return kmxFileUrl;
  }


}
