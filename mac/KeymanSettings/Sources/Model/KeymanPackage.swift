/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-02-24
 *
 * Object that represents a Keyman package
 * KeymanPackage is immutable, but the state of the Keyboards in the keyboards array can change
 */

import Foundation
import AppKit

public class KeymanPackage: Identifiable, Hashable, Equatable {
  static let defaultImage: NSImage? = {
    var image: NSImage? = nil
    if let imageUrl = Bundle.main.url(forResource: "SideImage", withExtension: "bmp") {
      image = NSImage(contentsOf: imageUrl)
    }
    return image
  }()
  
  public let id: UUID
  
  // the URL of the directory in which the package is contained
  public let sourceDirectoryUrl: URL
  // the URL of the kmp.json file for the package
  
  public let keyboards: [Keyboard]
  public let packageName: String
  public let packageVersion: String
  
  public let copyright: String?
  public let jsonFileUrl: URL
  public let readmeFileUrl: URL?
  public let graphicFileUrl: URL?
  public let graphicImage: NSImage?
  
  /**
   * create a KeymanPackage object using the PackageSource object created from the kmp.json
   */
  init(packageSource: PackageSource) {
    self.id = UUID()
    self.packageName = packageSource.info.name.description
    self.packageVersion = packageSource.info.version.description
    self.copyright = packageSource.info.copyright?.description
    self.sourceDirectoryUrl = packageSource.directoryUrl!
    self.jsonFileUrl = packageSource.kmpJsonFileUrl!
    
    if let readmeFilename = packageSource.readmeFilename {
      let fileUrl = sourceDirectoryUrl.appendingPathComponent(readmeFilename)
      self.readmeFileUrl = fileUrl
    } else {
      self.readmeFileUrl = nil
    }
    
    self.graphicFileUrl = KeymanPackage.buildGraphicFileUrl(source: packageSource)
    self.graphicImage = KeymanPackage.loadImage(imageUrl: self.graphicFileUrl)
    
    self.keyboards = KeymanPackage.buildKeyboardsArray(packageSource: packageSource)
  }
  
  /**
   * build an array of Keyboard objects using the array of KeyboardSource object created from the kmp.json
   */
  private static func buildKeyboardsArray (packageSource: PackageSource) -> [Keyboard] {
    var keyboardsArray = [Keyboard]()
    
    if let keyboards = packageSource.keyboards, let directoryUrl = packageSource.directoryUrl {
      for keyboardSource in keyboards {
        let keyboard = Keyboard(keyboardSource: keyboardSource, directoryUrl: directoryUrl)
        keyboardsArray.append(keyboard)
      }
    }
    
    return keyboardsArray
  }
  
  /**
   * initializer that does not rely on package source -- provided to create unit test data
   */
  public init(sourceDirectoryUrl: URL, keyboards: [Keyboard], packageName: String, packageVersion: String, copyright: String? = nil, jsonFileUrl: URL, readmeFileUrl: URL? = nil, graphicFileUrl: URL? = nil, graphicImage: NSImage? = nil) {
    self.id = UUID()
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
  
  /**
   * find the keyboard with the specified key in the package and return its enabled state
   */
  public func isKeyboardEnabled(keyboardKey: String) -> Bool {
    var enabled = false
    if let keyboard = self.keyboards.first(where: { $0.keyboardKey == keyboardKey }) {
      let akeyboard = keyboard
      enabled = keyboard.enabled
    }
    
    return enabled
  }
  
  /**
   * find the keyboard with the specified key in the package and set its enabled state
   */
  public func enableKeyboard(keyboardKey: String, enabled: Bool) {
    let keyboard = self.keyboards.first(where: { $0.keyboardKey == keyboardKey })
    if (keyboard != nil) {
      keyboard!.enabled = enabled
    }
  }
  
  /**
   * get all the keys for enabled keyboards in the package
   */
  public func getEnabledKeyboardsKeys() -> [String] {
    var settingsKeyArray = [String]()
    
    self.keyboards.forEach { keyboard in
      if (keyboard.enabled) {
        settingsKeyArray.append(keyboard.keyboardKey)
      }
    }
    
    return settingsKeyArray
  }
  
  /**
   * validate whether the package contain a kmx file for each of its keyboards
   */
  public func validate() -> Bool {
    var validKeyboards = self.keyboards.isEmpty == false
    
    self.keyboards.forEach { keyboard in
      if (validKeyboards && !keyboard.validateKmxFile()) {
        validKeyboards = false
      }
    }
    
    return validKeyboards
  }
  
  /**
   * build the URL for the graphic file specified for the package
   */
  static func buildGraphicFileUrl(source: PackageSource) -> URL? {
    var fileUrl: URL? = nil
    
    if let graphicFilename = source.graphicFilename {
      fileUrl = source.directoryUrl!.appendingPathComponent(graphicFilename)
    }
    
    return fileUrl
  }
  
  /**
   * create the image specified for the package
   * if none specified, load the default image
   */
  static func loadImage(imageUrl: URL?) -> NSImage? {
    var packageImage: NSImage? = nil;
    
    if let fileUrl = imageUrl, let image = NSImage(contentsOf: fileUrl) {
      packageImage = image
    } else {
      packageImage = KeymanPackage.defaultImage
    }
    
    return packageImage
  }
  
  /**
   * provided for Hashable conformance
   */
  public func hash(into hasher: inout Hasher) {
    hasher.combine(id) // only combine the unique ID
  }
  
  /**
   * provided for Equatable conformance
   */
  public static func == (lhs: KeymanPackage, rhs: KeymanPackage) -> Bool {
    return lhs.id == rhs.id // only compare unique IDs
  }
}
