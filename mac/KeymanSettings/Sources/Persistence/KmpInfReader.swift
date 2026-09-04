/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-09-04
 *
 * Parses the contents of a kmp.inf file and produces an instance
 * of PackageSource.
 */

import Foundation
import OSLog

public class KMPackageReader {
  public let kmpInfUrl: URL
  
  // kmp.inf section headers
  private let kPackage = "[Package]"
  private let kButtons = "[Buttons]"
  private let kStartMenu = "[StartMenu]"
  private let kStartMenuEntries = "[StartMenuEntries]"
  private let kPackageInfo = "[PackageInfo]"
  private let kInfo = "[Info]"
  private let kFiles = "[Files]"
  
  // kmp.inf properties
  private let kAuthor = "Author"
  private let kCopyright = "Copyright"
  private let kFile = "File"
  private let kFont = "Font"
  private let kGraphicFile = "GraphicFile"
  private let kKeyboard = "Keyboard"
  private let kName = "Name"
  private let kReadMeFile = "ReadMeFile"
  private let kVersion = "Version"
  private let kWebSite = "WebSite"
  
  private enum ContentType {
    case package, buttons, startMenu, startMenuEntries, info, files, unknown
  }
  
  public init(url: URL) {
    self.kmpInfUrl = url
  }
  
  /// Creates a KMPackageInfo object from the specified kmp.inf file.
  public func loadPackageInfoFromInfFile() throws -> KMPackageInfo? {
    
    var files: [String] = []
    var keyboardInfoArray: [KMKeyboardInfo] = []
    var fontArray: [String] = []
    
    // Tracking mutable parsing state variables
    var packageName: String?
    var packageVersion: String?
    var readmeFilename: String?
    var graphicFilename: String?
    var copyright: String?
    var authorName: String?
    var authorUrl: String?
    var website: String?
    
    let encoding = String.Encoding.windowsCP1252
    
    let path = self.kmpInfUrl.path(percentEncoded: false)
    let fileContents = try String(contentsOfFile: path, encoding: encoding)
      .replacingOccurrences(of: "\r", with: "")
    
    let lines = fileContents.components(separatedBy: "\n")
    var contentType: ContentType = .unknown
    
    for line in lines {
      if line.isEmpty { continue }
      let lowerLine = line.lowercased()
      
      // Section Router Switching
      if lowerLine.hasPrefix(kPackage.lowercased()) {
        contentType = .package
        continue
      } else if lowerLine.hasPrefix(kButtons.lowercased()) {
        contentType = .buttons
        continue
      } else if lowerLine.hasPrefix(kStartMenu.lowercased()) {
        contentType = .startMenu
        continue
      } else if lowerLine.hasPrefix(kStartMenuEntries.lowercased()) {
        contentType = .startMenuEntries
        continue
      } else if lowerLine.hasPrefix(kInfo.lowercased()) || lowerLine.hasPrefix(kPackageInfo.lowercased()) {
        contentType = .info
        continue
      } else if lowerLine.hasPrefix(kFiles.lowercased()) {
        contentType = .files
        continue
      }
      
      // Value Line Extraction
      switch contentType {
      case .package:
        if lowerLine.hasPrefix(kReadMeFile.lowercased()) {
          let index = line.index(line.startIndex, offsetBy: kReadMeFile.count + 1)
          readmeFilename = String(line[index...])
        } else if lowerLine.hasPrefix(kGraphicFile.lowercased()) {
          let index = line.index(line.startIndex, offsetBy: kGraphicFile.count + 1)
          graphicFilename = String(line[index...])
        }
        
      case .info:
        if lowerLine.hasPrefix(kName.lowercased()) {
          let index = line.index(line.startIndex, offsetBy: kName.count + 1)
          let sub = String(line[index...])
          let components = sub.components(separatedBy: "\",")
          packageName = components.first?.replacingOccurrences(of: "\"", with: "")
        } else if lowerLine.hasPrefix(kVersion.lowercased()) {
          let index = line.index(line.startIndex, offsetBy: kVersion.count + 1)
          let sub = String(line[index...])
          let components = sub.components(separatedBy: "\",")
          packageVersion = components.first?.replacingOccurrences(of: "\"", with: "")
        } else if lowerLine.hasPrefix(kAuthor.lowercased()) {
          let index = line.index(line.startIndex, offsetBy: kAuthor.count + 1)
          let sub = String(line[index...])
          let components = sub.components(separatedBy: "\",")
          if components.count > 0 {
            authorName = components[0].replacingOccurrences(of: "\"", with: "")
          }
          if components.count > 1 {
            authorUrl = components[1].replacingOccurrences(of: "\"", with: "")
          }
        } else if lowerLine.hasPrefix(kCopyright.lowercased()) {
          let index = line.index(line.startIndex, offsetBy: kCopyright.count + 1)
          let sub = String(line[index...])
          let components = sub.components(separatedBy: "\",")
          copyright = components.first?.replacingOccurrences(of: "\"", with: "")
        } else if lowerLine.hasPrefix(kWebSite.lowercased()) {
          let index = line.index(line.startIndex, offsetBy: kWebSite.count + 1)
          let sub = String(line[index...])
          let components = sub.components(separatedBy: "\",")
          website = components.first?.replacingOccurrences(of: "\"", with: "")
        }
        
      case .files:
        guard let equalIndex = line.firstIndex(of: "=") else { continue }
        // Replicates [line substringFromIndex:x+2] safely
        let valueStartIndex = line.index(equalIndex, offsetBy: 2)
        guard valueStartIndex < line.endIndex else { continue }
        
        let targetValueString = String(line[valueStartIndex...])
        let lowerValue = targetValueString.lowercased()
        
        if lowerValue.hasPrefix(kFile.lowercased()) {
          let components = targetValueString.components(separatedBy: "\",")
          if components.count > 1 {
            let fileName = components[1].replacingOccurrences(of: "\"", with: "")
            files.append(fileName)
          }
        } else if lowerValue.hasPrefix(kFont.lowercased()) {
          let components = targetValueString.components(separatedBy: "\",")
          if let firstSegment = components.first {
            let fontCutIndex = firstSegment.index(firstSegment.startIndex, offsetBy: kFont.count + 1)
            if fontCutIndex < firstSegment.endIndex {
              let fontName = String(firstSegment[fontCutIndex...]).replacingOccurrences(of: "\"", with: "")
              fontArray.append(fontName)
            }
          }
        } else if lowerValue.hasPrefix(kKeyboard.lowercased()) {
          let components = targetValueString.components(separatedBy: "\",")
          if let firstSegment = components.first {
            let kbCutIndex = firstSegment.index(firstSegment.startIndex, offsetBy: kKeyboard.count + 1)
            if kbCutIndex < firstSegment.endIndex {
              let keyboardName = String(firstSegment[kbCutIndex...]).replacingOccurrences(of: "\"", with: "")
              let keyboardInfo = KMKeyboardInfo(name: keyboardName)
              keyboardInfoArray.append(keyboardInfo)
            }
          }
        }
        
      default:
        break
      }
    }
    
    return KMPackageInfo(
      packageName: packageName,
      packageVersion: packageVersion,
      readmeFilename: readmeFilename,
      graphicFilename: graphicFilename,
      copyright: copyright,
      authorName: authorName,
      authorUrl: authorUrl,
      website: website,
      keyboards: keyboardInfoArray,
      files: files,
      fonts: fontArray
    )
  }
  
  // MARK: - Package Models
  public struct KMPackageInfo {
    public let packageName: String?
    public let packageVersion: String?
    public let readmeFilename: String?
    public let graphicFilename: String?
    public let fileVersion: String?
    public let keymanDeveloperVersion: String?
    public let copyright: String?
    public let authorName: String?
    public let authorUrl: String?
    public let website: String?
    public let keyboards: [KMKeyboardInfo]
    public let files: [String]
    public let fonts: [String]
    
    init(
      packageName: String? = nil,
      packageVersion: String? = nil,
      readmeFilename: String? = nil,
      graphicFilename: String? = nil,
      fileVersion: String? = nil,
      keymanDeveloperVersion: String? = nil,
      copyright: String? = nil,
      authorName: String? = nil,
      authorUrl: String? = nil,
      website: String? = nil,
      keyboards: [KMKeyboardInfo] = [],
      files: [String] = [],
      fonts: [String] = []
    ) {
      self.packageName = packageName
      self.packageVersion = packageVersion
      self.readmeFilename = readmeFilename
      self.graphicFilename = graphicFilename
      self.fileVersion = fileVersion
      self.keymanDeveloperVersion = keymanDeveloperVersion
      self.copyright = copyright
      self.authorName = authorName
      self.authorUrl = authorUrl
      self.website = website
      self.keyboards = keyboards
      self.files = files
      self.fonts = fonts
    }
  }
  
  // MARK: - Keyboard & Language Models
  public struct KMKeyboardInfo {
    public let name: String?
    public let identifier: String?
    public let version: String?
    public let oskFont: String?
    public let displayFont: String?
    public let languages: [KMLanguageInfo]
    
    init(
      name: String? = nil,
      identifier: String? = nil,
      version: String? = nil,
      oskFont: String? = nil,
      displayFont: String? = nil,
      languages: [KMLanguageInfo] = []
    ) {
      self.name = name
      self.identifier = identifier
      self.version = version
      self.oskFont = oskFont
      self.displayFont = displayFont
      self.languages = languages
    }
  }
  
  public struct KMLanguageInfo {
    public let name: String
    public let identifier: String
  }
}

