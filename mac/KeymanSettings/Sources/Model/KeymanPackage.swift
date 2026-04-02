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
  let source: PackageSource
  public var keyboards: [Keyboard]
  
  // the URL of the directory in which the package is contained
  public let sourceDirectoryUrl: URL
  // the URL of the kmp.json file for the package
  public let jsonFileUrl: URL
  public let readmeFileUrl: URL?
  public let graphicFileUrl: URL?
  public let graphicImage: NSImage?
  
  // computed properties for convenience
  public var packageName: String {
    return source.info.name.description
  }
  public var packageVersion: String {
    return source.info.version.description
  }
  public var copyright: String? {
    if let copy = source.info.copyright?.description {
      return copy
    } else {
      return nil
    }
  }

  init(packageSource: PackageSource) {
    self.source = packageSource
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
    
    if let keyboards = source.keyboards {
      for keyboardSource in keyboards {
        let keyboard = Keyboard(keyboardSource: keyboardSource, directoryUrl: self.sourceDirectoryUrl)
        keyboardsArray.append(keyboard)
      }
    }
    self.keyboards = keyboardsArray
    
//    ConfigLogger.shared.testLogger.debug("package created for: \(packageSource.packageName)")
    print("package created for: \(packageSource.packageName)")
  }
  
  public func isKeyboardEnabled(keyboardId: String) -> Bool {
    var enabled = false
    if let keyboard = self.keyboards.first(where: { $0.keyboardId == keyboardId }) {
      enabled = keyboard.enabled
    }
    
    return enabled
  }

  public func enableKeyboard(keyboardId: String, enabled: Bool) {
    let keyboard = self.keyboards.first(where: { $0.keyboardId == keyboardId })
    if (keyboard != nil) {
      keyboard!.enabled = enabled
    }
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
