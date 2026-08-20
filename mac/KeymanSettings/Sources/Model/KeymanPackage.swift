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
    if let imageUrl = Bundle.module.url(forResource: "SideImage", withExtension: "bmp") {
        image = NSImage(contentsOf: imageUrl)
    } else {
        print("Error: Could not find SideImage.bmp in the module bundle.")
    }
    return image
  }()
  
  public let id: UUID
  
  // The directory where this package is contained.
  // It is used to delete/uninstall the package if requested by the user.
  // This value only changes when the package is moved
  // from the temp directory during package installation.
  public var sourceDirectoryUrl: URL
  
  // the URL for downloading the package from keyman.com
  public let sharePackageUrl: URL?

  public let keyboards: [Keyboard]
  public let fonts: [String]
  public let packageName: String
  public let packageVersion: String
  
  public let author: String?
  public let websiteUrl: URL?
  public let copyright: String?
  
  // the name of the readme file used to generate the Url
  let readmeFilename: String?
  // the URL of the readme file within the package
  public var readmeFileUrl: URL? {
    return readmeFilename.map { sourceDirectoryUrl.appendingPathComponent($0).standardizedFileURL }
  }

  // the name of the help file used to generate the Url
  let helpFilename: String?
  // the URL of the help file within the package, named 'welcomeFile' in kmp.json
  public var helpFileUrl: URL? {
    return helpFilename.map { sourceDirectoryUrl.appendingPathComponent($0).standardizedFileURL }
  }

  // the name of the graphicFile used to generate the Url
  let graphicFilename: String?
  // a cache of the image
  private var cachedGraphicImage: NSImage?
  private var graphicFileUrl: URL? {
    return graphicFilename.map { sourceDirectoryUrl.appendingPathComponent($0).standardizedFileURL }
  }
  public var graphicImage: NSImage? {
    get {
      if let cachedImage = self.cachedGraphicImage {
        return cachedImage
      } else {
        let newImage = KeymanPackage.loadImage(imageUrl: self.graphicFileUrl)
        self.cachedGraphicImage = newImage
        return newImage
      }
    }
  }

  // the qrCode image does not change size so a single cached image per package is sufficient
  var qrCodeImageCache: (image: NSImage, size: CGFloat)? = nil
  
  /**
   * create a KeymanPackage object using the location of the package and the PackageSource object created from the kmp.json
   */
  init(packageUrl: URL, packageSource: PackageSource) {
    self.id = UUID()
    self.sourceDirectoryUrl = packageUrl

    self.packageName = packageSource.info.name.description
    self.packageVersion = packageSource.info.version.description
    self.author = packageSource.info.author?.description
    if let websiteUrlString = packageSource.info.website?.url {
      self.websiteUrl = URL(string: websiteUrlString)
    } else {
      self.websiteUrl = nil
    }
    self.copyright = packageSource.info.copyright?.description
    
    self.readmeFilename = packageSource.readmeFilename
    self.helpFilename = packageSource.helpFilename
    self.graphicFilename = packageSource.graphicFilename
    
    self.sharePackageUrl = KeymanPackage.buildSharePackageUrl(packageUrl: self.sourceDirectoryUrl)
 
    let packageDirectory = packageUrl.lastPathComponent
    let keyboardsArray = KeymanPackage.buildKeyboardsArray(packageSource: packageSource, packageDirectoryName: packageDirectory)
    self.keyboards = keyboardsArray
    
    self.fonts = KeymanPackage.buildFontNamesArray(keyboards: keyboardsArray)
  }
  
  /**
   * build an array of Keyboard objects using the array of KeyboardSource object created from the kmp.json and the package URL
   */
  private static func buildKeyboardsArray(packageSource: PackageSource, packageDirectoryName: String) -> [Keyboard] {
    var keyboardsArray = [Keyboard]()
    
    if let keyboards = packageSource.keyboards {
      for keyboardSource in keyboards {
        let keyboard = Keyboard(keyboardSource: keyboardSource, packageDirectoryName: packageDirectoryName)
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
  public init(sourceDirectoryUrl: URL, sharePackageUrl: URL? = nil, keyboards: [Keyboard], packageName: String, packageVersion: String, author: String? = nil, website: URL? = nil, copyright: String? = nil, readmeFileName: String? = nil, helpFilename: String? = nil, graphicName: String? = nil) {
    self.id = UUID()
    self.sourceDirectoryUrl = sourceDirectoryUrl
    self.sharePackageUrl = sharePackageUrl
    self.keyboards = keyboards
    self.packageName = packageName
    self.packageVersion = packageVersion
    self.author = author
    self.websiteUrl = website
    self.copyright = copyright
    self.readmeFilename = readmeFileName
    self.helpFilename = helpFilename
    self.graphicFilename = graphicName
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
      try keyboard.validateKmxFile(in: self.sourceDirectoryUrl)
    }
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
   * get a QR code image of the specified size from the cache or generate a new one
   */
  public func getSharePackageQRCode(for size: CGFloat) -> NSImage? {
    var qrCodeImage: NSImage? = nil
    
    // if the image is already cached for the specified size, then use it
    if let qrImageCache = self.qrCodeImageCache {
      if size == qrImageCache.size {
        qrCodeImage = qrImageCache.image
      }
    }
    
    // if no matching image cached, then create one and cache it
    if qrCodeImage == nil {
      if let newImage = self.generateSharePackageQRCode(for: size) {
        qrCodeImage = newImage
        self.qrCodeImageCache = (newImage, size)
      }
    }
    
    return qrCodeImage
  }
  
  /**
   * generate a QR code image for sharing the Keyman Package URL
   */
  func generateSharePackageQRCode(for size: CGFloat) -> NSImage? {
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
