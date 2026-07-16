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
import Cocoa
import CoreImage
import CoreImage.CIFilterBuiltins

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
  public let jsonFileUrl: URL
  // the URL for downloading the package from keyman.com
  public let sharePackageUrl: URL?

  public let keyboards: [Keyboard]
  public let fonts: [String]
  public let packageName: String
  public let packageVersion: String
  
  public let author: String?
  public let copyright: String?
  // the URL of the readme file within the package
  public let readmeFileUrl: URL?
  // the URL of the help file within the package, named 'welcomeFile' in kmp.json
  public let helpFileUrl: URL?
  // the URL of the graphic file within the package
  public let graphicFileUrl: URL?
  public let graphicImage: NSImage?
  
  /**
   * create a KeymanPackage object using the PackageSource object created from the kmp.json
   */
  init(packageSource: PackageSource) {
    self.id = UUID()
    self.packageName = packageSource.info.name.description
    self.packageVersion = packageSource.info.version.description
    self.author = packageSource.info.author?.description
    self.copyright = packageSource.info.copyright?.description
    self.sourceDirectoryUrl = packageSource.directoryUrl!
    self.jsonFileUrl = packageSource.kmpJsonFileUrl!
    
    if let readmeFilename = packageSource.readmeFilename {
      let fileUrl = sourceDirectoryUrl.appendingPathComponent(readmeFilename)
      self.readmeFileUrl = fileUrl
    } else {
      self.readmeFileUrl = nil
    }

    if let helpFilename = packageSource.helpFilename {
      let fileUrl = sourceDirectoryUrl.appendingPathComponent(helpFilename)
      self.helpFileUrl = fileUrl
    } else {
      self.helpFileUrl = nil
    }

    self.graphicFileUrl = KeymanPackage.buildGraphicFileUrl(source: packageSource)
    self.graphicImage = KeymanPackage.loadImage(imageUrl: self.graphicFileUrl)
    
    self.sharePackageUrl = KeymanPackage.buildSharePackageUrl(packageUrl: self.sourceDirectoryUrl)
 
    let keyboardsArray = KeymanPackage.buildKeyboardsArray(packageSource: packageSource)
    self.keyboards = keyboardsArray
    
    self.fonts = KeymanPackage.buildFontNamesArray(keyboards: keyboardsArray)
  }
  
  /**
   * build an array of Keyboard objects using the array of KeyboardSource object created from the kmp.json
   */
  private static func buildKeyboardsArray(packageSource: PackageSource) -> [Keyboard] {
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
   * build an array of all the fonts used by the specified array of keyboards
   */
  private static func buildFontNamesArray(keyboards: [Keyboard]) -> [String] {
    var fontNames: Set<String> = []
    for keyboard in keyboards {
      if let oskFontName = keyboard.oskFont {
        fontNames.insert(oskFontName)
      }
      if let displayFontName = keyboard.displayFont {
        fontNames.insert(displayFontName)
      }
    }
    return fontNames.sorted()
  }

  /**
   * initializer that does not rely on package source -- provided to create unit test data
   */
  public init(sourceDirectoryUrl: URL, sharePackageUrl: URL? = nil, keyboards: [Keyboard], packageName: String, packageVersion: String, author: String? = nil, copyright: String? = nil, jsonFileUrl: URL, readmeFileUrl: URL? = nil, helpFileUrl: URL? = nil, graphicFileUrl: URL? = nil, graphicImage: NSImage? = nil) {
    self.id = UUID()
    self.sourceDirectoryUrl = sourceDirectoryUrl
    self.sharePackageUrl = sharePackageUrl
    self.keyboards = keyboards
    self.packageName = packageName
    self.packageVersion = packageVersion
    self.author = author
    self.copyright = copyright
    self.jsonFileUrl = jsonFileUrl
    self.readmeFileUrl = readmeFileUrl
    self.helpFileUrl = helpFileUrl
    self.graphicFileUrl = graphicFileUrl
    self.graphicImage = graphicImage
    self.fonts = []
  }
  
  /**
   * find the keyboard with the specified key in the package and return its enabled state
   */
  public func isKeyboardEnabled(keyboardKey: String) -> Bool {
    var enabled = false
    if let keyboard = self.keyboards.first(where: { $0.keyboardKey == keyboardKey }) {
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
  public func validate() throws {
    // if validateKmxFile throws an error, then the loop is stopped and the error is propagated
    try self.keyboards.forEach { keyboard in
      try keyboard.validateKmxFile()
    }
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
   * build the URL where the keyboard can be installed from the Keyman website
   */
  static func buildSharePackageUrl(packageUrl: URL) -> URL? {
    return URL(string: "https://\(KeymanPaths.keymanDomain)/go/keyboard/\(packageUrl.lastPathComponent)/share")
  }

  /**
   * generate a QR code for sharing the Keyman Package URL
   */
  public func generateSharePackageQRCode(size: CGFloat = 300) -> NSImage? {
    guard let data = self.sharePackageUrl?.absoluteString.data(using: .utf8) else { return nil }
    
    // initialize the built-in Apple QR filter
    let filter = CIFilter.qrCodeGenerator()
    filter.message = data
    filter.correctionLevel = "M"
    
    guard let rawQRImage = filter.outputImage else { return nil }
    
    // calculate scaling factor to ensure crisp rendering of QR code
    let rawWidth = rawQRImage.extent.width
    guard rawWidth > 0 else { return nil }
    let scale = size/rawWidth
    
    // apply transform to scale up without blurring (use interpolation = none in SwiftUI image)
    let scaledQRImage = rawQRImage.transformed(by: CGAffineTransform(scaleX: scale, y: scale))
    
    // convert the CIImage to a macOS NSImage
    let rep = NSCIImageRep(ciImage: scaledQRImage)
    let nsImage = NSImage(size: NSSize(width: size, height: size))
    nsImage.addRepresentation(rep)
    
    return nsImage
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
