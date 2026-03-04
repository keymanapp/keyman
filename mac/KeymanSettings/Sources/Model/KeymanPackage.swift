/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-02-24
 *
 * Wrapper object that represents a Keyman package as loaded from a .KMP file
 * Can be loaded but disabled
 *
 */

import Foundation
import AppKit

// MARK: - Root
public struct KeymanPackage: Identifiable, Hashable, Equatable {
  static let defaultImage: NSImage? = {
    var image: NSImage? = nil
    if let imageUrl = Bundle.main.url(forResource: "SideImage", withExtension: "bmp") {
      image = NSImage(contentsOf: imageUrl)
    }
    return image
  }()

  public var id = UUID()
  public var enabled: Bool
  let source: PackageSource
  public let keyboards: [Keyboard]
  
  public let sourceDirectoryUrl: URL
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
    self.enabled = true
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
        let keyboard = Keyboard(keyboardSource: keyboardSource)
        keyboardsArray.append(keyboard)
      }
    }
    self.keyboards = keyboardsArray
    
//    ConfigLogger.shared.testLogger.debug("package created for: \(packageSource.packageName)")
    print("package created for: \(packageSource.packageName)")
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
