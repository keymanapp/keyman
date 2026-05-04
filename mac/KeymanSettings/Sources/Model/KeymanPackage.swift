/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-02-24
 *
 * Wrapper object that represents a Keyman package as loaded from a .KMP file
 * Mostly immutable, but can be marked as enabled/disabled
 *
 */

import Foundation
import AppKit

// MARK: - Root
public class KeymanPackage: Identifiable, Hashable, Equatable {
  static let defaultImage: NSImage? = {
    var image: NSImage? = nil
    if let imageUrl = Bundle.main.url(forResource: "SideImage", withExtension: "bmp") {
      image = NSImage(contentsOf: imageUrl)
    }
    return image
  }()

  public var id = UUID()

  // the URL of the directory in which the package is contained
  public let sourceDirectoryUrl: URL
  // the URL of the kmp.json file for the package

  public var keyboards: [Keyboard]
  public let packageName: String
  public let packageVersion: String
  
  public let copyright: String?
  public let jsonFileUrl: URL
  public let readmeFileUrl: URL?
  public let graphicFileUrl: URL?
  public let graphicImage: NSImage?
  
  init(packageSource: PackageSource) {
    self.packageName = packageSource.info.name.description
    self.packageVersion = packageSource.info.version.description
    self.copyright = packageSource.info.copyright?.description
    self.sourceDirectoryUrl = packageSource.directoryUrl!
    self.jsonFileUrl = packageSource.jsonFileUrl!
    
    if let readmeFilename = packageSource.readmeFilename {
      let fileUrl = sourceDirectoryUrl.appendingPathComponent(readmeFilename)
      self.readmeFileUrl = fileUrl
    } else {
      self.readmeFileUrl = nil
    }

    self.graphicFileUrl = KeymanPackage.buildGraphicFileUrl(source: packageSource)
    self.graphicImage = KeymanPackage.loadImage(imageUrl: self.graphicFileUrl)

    var keyboardsArray = [Keyboard]()
    
    if let keyboards = packageSource.keyboards {
      for keyboardSource in keyboards {
        let keyboard = Keyboard(keyboardSource: keyboardSource, directoryUrl: self.sourceDirectoryUrl)
        keyboardsArray.append(keyboard)
      }
    }
    self.keyboards = keyboardsArray
    
    print("package created for: \(packageSource.packageName)")
  }

  /**
   * initializer that does not rely on package source -- provided to create unit test data
   */
  public init(sourceDirectoryUrl: URL, keyboards: [Keyboard], packageName: String, packageVersion: String, copyright: String? = nil, jsonFileUrl: URL, readmeFileUrl: URL? = nil, graphicFileUrl: URL? = nil, graphicImage: NSImage? = nil) {
    self.sourceDirectoryUrl = sourceDirectoryUrl
    self.keyboards = keyboards
    self.packageName = packageName
    self.packageVersion = packageVersion
    self.copyright = copyright
    self.jsonFileUrl = jsonFileUrl
    self.readmeFileUrl = readmeFileUrl
    self.graphicFileUrl = graphicFileUrl
    self.graphicImage = graphicImage
  }

  public func isKeyboardEnabled(keyboardKey: String) -> Bool {
    var enabled = false
    if let keyboard = self.keyboards.first(where: { $0.keyboardKey == keyboardKey }) {
      let akeyboard = keyboard
      print("keyboard: \(akeyboard.keyboardKey) is enabled: \(keyboard.enabled)")
      enabled = keyboard.enabled
    }
    
    print("returning: \(enabled)")
    return enabled
  }

  public func enableKeyboard(keyboardKey: String, enabled: Bool) {
    let keyboard = self.keyboards.first(where: { $0.keyboardKey == keyboardKey })
    if (keyboard != nil) {
      keyboard!.enabled = enabled
    }
  }

  public func getKeyboardSettingsKey(for keyboardId: String) -> String? {
    guard let keyboard = self.keyboards.first(where: { $0.keyboardId == keyboardId }) else {
      return nil
    }
    return keyboard.keyboardKey
  }

  public func getEnabledKeyboardsSettingsKeys() -> [String] {
    var settingsKeyArray = [String]()
    
    self.keyboards.forEach { keyboard in
      if (keyboard.enabled) {
        settingsKeyArray.append(keyboard.keyboardKey)
      }
    }
    
    return settingsKeyArray
  }
  
  public func validate() -> Bool {
    var validKeyboards = self.keyboards.isEmpty == false
    
    self.keyboards.forEach { keyboard in
      if (validKeyboards && !keyboard.validateKmxFile()) {
        validKeyboards = false
      }
    }
    
    return validKeyboards
  }
  
  static func buildGraphicFileUrl(source: PackageSource) -> URL? {
    var fileUrl: URL? = nil
    
    if let graphicFilename = source.graphicFilename {
      fileUrl = source.directoryUrl!.appendingPathComponent(graphicFilename)
    }
    
    return fileUrl
  }
  
  static func loadImage(imageUrl: URL?) -> NSImage? {
   var packageImage: NSImage? = nil;

    if let fileUrl = imageUrl, let image = NSImage(contentsOf: fileUrl) {
      packageImage = image
    } else {
      packageImage = KeymanPackage.defaultImage
    }
    
    return packageImage
  }
  
  public func hash(into hasher: inout Hasher) {
      hasher.combine(id) // only combine the unique ID
  }
  
  // Custom Equatable conformance (required by Hashable)
  public static func == (lhs: KeymanPackage, rhs: KeymanPackage) -> Bool {
      return lhs.id == rhs.id // only compare unique IDs
  }
}
