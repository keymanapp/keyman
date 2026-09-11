/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-09-04
 *
 * Parses the contents of a kmp.inf file.
 * After collecting all the data, it validates it and creates a KeymanPackage
 */

import Foundation
import OSLog

// the section headings
enum Section: String {
  case package = "package"
  case info = "info"                // found in version kmp.inf version 6.0 and later
  case packageinfo = "packageinfo"   // found in version kmp.inf version 5.0
  case startmenu = "startmenu"      // ignored
  case files = "files"
}

// the property names for the Keyboard section(s)
enum KeyboardProperty: String {
  case name
  case id
  case version
  case rtl
  case oskfont
  case displayfont
}

// the property names for the Package section
enum PackageProperty: String {
  case version    // the version of Keyman Developer used to create the keyboard
  case graphicfile
  case readmefile
}

// the property names for the Info section
enum InfoProperty: String {
  case name
  case version    // the version number of the package
  case copyright
  case author
  case website
}

class KmpInfParser {
  private let kmpInfFile: URL
  private let packageDirectoryUrl: URL
  private let packageDirectoryName: String
  private let fileContent: String
  
  // default filenames -- used in case they are not specified in kmp.inf
  private let defaultHelpFilename = "welcome.htm"
  private let defaultReadmeFilename = "readme.htm"
  
  // prefixes for dynamic sections/keys
  private let keyboardPrefix = "keyboard"
  private let languagePrefix = "language"
  
  // the section that is currently being parsed
  private var currentSection = ""
  
  // collected parsed data
  private var packageMap: [String: String] = [:]
  private var infoMap: [String: String] = [:]
  private var keyboardMapArray: [[String: String]] = []
  private var fileMap: [String:String] = [:]
  private var currentKeyboardMap: [String: String] = [:]
  private var currentLanguageMap: [String: String] = [:]
  
  // private read-only computed properties to provide easy access
  // to the data we have parsed and saved for package creation
  private var version: String? {
    return self.infoMap[InfoProperty.version.rawValue]
  }
  private var readmeFilename: String? {
    if let filename = self.packageMap[PackageProperty.readmefile.rawValue] {
      return filename
    }
    if self.fileMap.keys.contains(where: { $0 == defaultReadmeFilename }) {
      return defaultReadmeFilename
    }
    return nil
  }
  private var graphicFilename: String? {
    return self.packageMap[PackageProperty.graphicfile.rawValue]
  }
  private var helpFilename: String? {
    if self.fileMap.keys.contains(where: { $0 == defaultHelpFilename }) {
      return defaultHelpFilename
    }
    return nil
  }
  private var packageName: String? {
    return self.infoMap[InfoProperty.name.rawValue]
  }
  private var websiteUrl: URL? {
    let websitePath = self.infoMap[InfoProperty.website.rawValue]
    return websitePath.flatMap(URL.init(string:))
  }
  private var copyright: String? {
    return self.infoMap[InfoProperty.copyright.rawValue]
  }
  private var author: String? {
    return self.infoMap[InfoProperty.author.rawValue]
  }
  
  /**
   * Initializes the parser, reads the file contents with windowsCP1252 encoding and changes to Mac line endings.
   */
  init(fileUrl: URL, in directoryUrl: URL) throws {
    self.kmpInfFile = fileUrl
    self.packageDirectoryUrl = directoryUrl
    self.packageDirectoryName = self.packageDirectoryUrl.lastPathComponent
    
    let path = self.kmpInfFile.path(percentEncoded: false)
    let encoding = String.Encoding.windowsCP1252
    
    self.fileContent = try String(contentsOfFile: path, encoding: encoding)
      .replacingOccurrences(of: "\r", with: "")
  }
  
  /**
   * Collects all the data describing the package and use it to create a KeymanPackage.
   */
  public func parse() throws -> KeymanPackage? {
    
    let lines = self.fileContent.components(separatedBy: .newlines)
    for line in lines {
      let trimmed = line.trimmingCharacters(in: .whitespacesAndNewlines)
      if trimmed.isEmpty { continue }
      
      // determine the current section
      if self.containsSectionHeading(rowString: trimmed) {
        self.prepareNewSection(sectionName: self.extractSectionName(from: trimmed))
      } else if self.containsKeyValuePair(rowString: trimmed) {
        // extract key-value pair, where the value is actually a String array
        let pair = self.extractKeyValuePair(from: trimmed)
        self.addProperty(key: pair.key, valueArray: pair.valueArray)
      }
    }
    
    // done reading properties, add the current keyboard map to the array
    if !self.currentKeyboardMap.isEmpty {
      // copy the current keyboard map to the array
      self.keyboardMapArray.append(self.currentKeyboardMap)
    }
    
    // validate collected data before building
    try self.validateParsedData()
    
    //
    return try self.buildPackage()
  }
  
  /**
   * validates the collected data to be sure we have what is required to build a package
   */
  private func validateParsedData() throws {
    if self.fileMap.isEmpty {
      throw LoadPackageError.containsNoFiles
    }
    if self.keyboardMapArray.isEmpty {
      throw LoadPackageError.containsNoKeyboards
    }
    for keyboardMap in self.keyboardMapArray {
      if !keyboardMap.keys.contains(KeyboardProperty.id.rawValue) { throw LoadPackageError.missingKeyboardId }
      if !keyboardMap.keys.contains(KeyboardProperty.name.rawValue) { throw LoadPackageError.missingKeyboardName }
      if !keyboardMap.keys.contains(KeyboardProperty.version.rawValue) { throw LoadPackageError.missingKeyboardVersion }
    }
  }
  
  /**
   * Constructs the KeymanPackage with its array of Keyboard objects using the data from the .inf file.
   */
  private func buildPackage() throws -> KeymanPackage? {
    var keymanPackage: KeymanPackage? = nil
    var keyboardsArray: [Keyboard] = []
    var fontsSet: Set<String> = []
    
    // first build the keyboards array from the array of keyboard maps
    for keyboardMap in self.keyboardMapArray {
      if let name = keyboardMap[KeyboardProperty.name.rawValue],
         let id = keyboardMap[KeyboardProperty.id.rawValue] {
        let oskFont = keyboardMap[KeyboardProperty.oskfont.rawValue]
        let displayFont = keyboardMap[KeyboardProperty.displayfont.rawValue]
        
        // add fonts to fonts list
        if let oskFont, !oskFont.isEmpty {
          fontsSet.insert(oskFont)
        }
        if let displayFont, !displayFont.isEmpty {
          fontsSet.insert(displayFont)
        }
        
        let keyboard = Keyboard(name: name, oskFont: oskFont, displayFont: displayFont, keyboardId: id, packageDirectoryName: self.packageDirectoryName, enabled: true)
        
        keyboardsArray.append(keyboard)
      }
    }
    
    if let packageName = self.packageName,
       let version = self.version {
      
      keymanPackage = try KeymanPackage(packageUrl: self.packageDirectoryUrl, keyboards: keyboardsArray, fonts: Array(fontsSet), packageName: packageName, packageVersion: version, author: self.author, websiteUrl: self.websiteUrl, copyright: self.copyright, readmeFilename: self.readmeFilename, helpFilename: self.helpFilename, graphicFilename: self.graphicFilename)
    }
    
    return keymanPackage
  }
  
  /**
   * prepare to add properties to a new section
   */
  private func prepareNewSection(sectionName: String) {
    self.currentSection = sectionName
    
    if sectionName.hasPrefix(keyboardPrefix) {
      self.startNewKeyboard()
    }
  }
  
  /**
   * If we have been collecting data for one keyboard, save its map to the keyboard array.
   * Initialize a new keyboard map to collect data for this keyboard
   */
  private func startNewKeyboard() {
    if !self.currentKeyboardMap.isEmpty {
      // if we were already collecting data for a keyboard, then copy it to the keyboard array
      self.keyboardMapArray.append(self.currentKeyboardMap)
    }
    // reset the keyboard and language maps
    self.currentKeyboardMap = [:]
    self.currentLanguageMap = [:]
  }
  
  /**
   * add the property for the current section
   */
  private func addProperty(key: String, valueArray: [String]) {
    if !self.currentSection.isEmpty &&
        (!key.isEmpty) &&
        (!valueArray.isEmpty) {
      
      if self.currentSection == Section.files.rawValue {
        self.addFile(filename: valueArray[1], description: valueArray[0]);
      } else if self.currentSection.hasPrefix(keyboardPrefix) {
        self.addKeyboardProperty(key: key, value: valueArray)
      } else if self.currentSection == Section.info.rawValue || self.currentSection == Section.packageinfo.rawValue  {
        self.addInfoProperty(key: key, value: valueArray)
      } else if self.currentSection == Section.package.rawValue  {
        self.addPackageProperty(key: key, value: valueArray)
      }
    }
  }
  
  /**
   * read the name of the section that is surrounded in square brackets
   */
  private func extractSectionName(from sectionString: String) -> String {
    var extractedString = ""
    
    // regex that looks for square brackets and captures everything inside
    let regex = /\[([^\]]+)\]/
    
    if let match = sectionString.firstMatch(of: regex) {
      // match.1 retrieves the first capture group, the text inside the brackets
      extractedString = String(match.1).lowercased()
    }
    
    return extractedString
  }
  
  /**
   * takes a row of data from the file and parses it into a key-value tuple
   * where the key is a String and the value is an array of Strings
   */
  private func extractKeyValuePair(from rowString: String) -> (key: String, valueArray: [String]) {
    var pair = (key: "", valueArray: [String]())
    
    let cleaned = rowString.trimmingCharacters(in: .whitespacesAndNewlines)
    
    // break the row into, at most, two parts: the key and the value
    let parts = cleaned.split(separator: "=", maxSplits: 1)
    let key = parts[0]
    
    if parts.count == 1 {
      // if there is only one string after split, then we have no values
      pair = (key.lowercased(), [])
    } else {
      // create an array of strings from the value part of the string, converting each Substring to a String
      let valueArray = self.splitValuesPreservingQuotes(parts[1])
//      let valueArray = parts[1].split(separator: ",").map(String.init)
      
      // remove the quote marks from each element of the value array
      let cleanValueArray = valueArray.map {
        $0.replacingOccurrences(of: "\"", with: "")
      }
      
      pair = (key.lowercased(), cleanValueArray)
    }
    
    return pair
  }
  
  /**
   * Splits a value string while ignoring any commas that appear inside double-quoted sections.
   * Doing this instead of a basic `String.split()` to allow values to contain a comma.
   */
  private func splitValuesPreservingQuotes(_ fullValue: Substring) -> [String] {
    var valueArray: [String] = []
    var subValue = ""
    var insideQuotes = false
   
    for char in fullValue {
      if char == "\"" {
        // starting or ending subValue
        insideQuotes.toggle()
        subValue.append(char)      // preserve the quote; it's stripped later
      } else if char == "," && !insideQuotes {
        // collect subValue and clear for next
        valueArray.append(subValue)
        subValue = ""
      } else {
        // add content to current subValue
        subValue.append(char)
      }
    }
    valueArray.append(subValue)
    return valueArray
  }
  
  /**
   * return true if the specified row of data from the file contains a section heading
   */
  private func containsSectionHeading(rowString: String) -> Bool {
    return rowString.hasPrefix("[") && rowString.hasSuffix("]")
  }
  
  /**
   * return true if the specified row of data from the file contains a key-value pair
   */
  private func containsKeyValuePair(rowString: String) -> Bool {
    return rowString.contains("=")
  }
  
  /**
   * add a property from a keyboard section of the kmp.inf file and store with the current keyboard
   */
  private func addKeyboardProperty(key: String, value: [String]) {
    if key.hasPrefix(languagePrefix) {
      self.addLanguage(key: key, value: value)
    }
    if let property = KeyboardProperty(rawValue: key.lowercased()) {
      switch property {
      case .id, .name, .version, .rtl, .displayfont, .oskfont:
        self.currentKeyboardMap[key.lowercased()] = value[0]
      }
    } else {
      Logger.data.debug("ignoring unknown keyboard property: \(key, privacy: .public)")
    }
  }
  
  /**
   * add a property from the info section of the kmp.inf file
   */
  private func addInfoProperty(key: String, value: [String]) {
    if let property = InfoProperty(rawValue: key.lowercased()) {
      switch property {
      case .name:
        self.infoMap[key.lowercased()] = value[0]
      case .version:
        self.infoMap[key.lowercased()] = value[0]
      case .copyright:
        self.infoMap[key.lowercased()] = value[0]
      case .author:
        self.infoMap[key.lowercased()] = value[0]
      case .website:
        // get second value if it exists, first is description
        if (self.infoMap.count > 1) {
          self.infoMap[key.lowercased()] = value[1]
        }
      }
    } else {
      Logger.data.debug("ignoring unknown info property: \(key, privacy: .public)")
    }
  }
  
  /**
   * add a property from the package section of the kmp.inf file
   */
  private func addPackageProperty(key: String, value: [String]) {
    if let property = PackageProperty(rawValue: key.lowercased()) {
      switch property {
      case .version:
        self.packageMap[key.lowercased()] = value[0]
      case .graphicfile:
        self.packageMap[key.lowercased()] = value[0]
      case .readmefile:
        self.packageMap[key.lowercased()] = value[0]
      }
    } else {
      Logger.data.debug("ignoring unknown package property: \(key, privacy: .public)")
    }
  }
  
  /**
   * insert the language in the map with the bcp47code as the key and the description as the value
   */
  private func addLanguage(key: String, value: [String]) {
    let bcp47code = value[0]
    let description = value[1]
    self.currentLanguageMap[bcp47code] = description
  }
  
  /**
   * add a file to the array of filenames
   */
  private func addFile(filename: String, description: String) {
    self.fileMap[filename] = description
  }
}
