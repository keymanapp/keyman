//
//  Keyboard.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-24.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

/// Keyboard object for Keyman Cloud API 4.0.
public struct Keyboard: Codable {
  /// Name of the keyboard.
  public var name: String
  
  /// ID of the keyboard. Always matches the filename of the keyboard.
  public var id: String
  
  /// Name of the keyboard `.js` file.
  public var filename: String
  
  /// The keyboard is the recommended default for the language.
  public var isDefault: Bool
  
  /// Keyboard targets a right-to-left script.
  public var isRTL: Bool
  
  /// Date the keyboard was last updated.
  public var lastModified: Date
  
  /// Size of the keyboard file in bytes.
  public var fileSize: Int?
  
  /// Dot-decimal version number of the keyboard.
  public var version: String
  
  /// Language objects linked to the keyboard.
  public var languages: [Language]?
  
  /// Font for input fields (and OSK if `oskFont` is not present).
  public var font: Font?
  
  /// Font for the OSK.
  public var oskFont: Font?
  
  enum CodingKeys: String, CodingKey {
    case name
    case id
    case filename
    case isDefault = "default"
    case isRTL = "rtl"
    case lastModified
    case fileSize
    case version
    case languages
    case font
    case oskFont
  }
  
  public init(name: String,
              id: String,
              filename: String,
              isDefault: Bool?,
              isRTL: Bool?,
              lastModified: Date,
              fileSize: Int?,
              version: String,
              languages: [Language]?,
              font: Font?,
              oskFont: Font?) {
    self.name = name
    self.id = id
    self.filename = filename
    self.isDefault = isDefault ?? false
    self.isRTL = isRTL ?? false
    self.lastModified = lastModified
    self.fileSize = fileSize
    self.version = version
    self.languages = languages
    self.font = font
    self.oskFont = oskFont
  }
  
  public init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    
    let name = try container.decode(String.self, forKey: .name)
    let id = try container.decode(String.self, forKey: .id)
    let filename = try container.decode(String.self, forKey: .filename)
    let isDefault = try container.decodeIfPresent(Bool.self, forKey: .isDefault)
    let isRTL = try container.decodeIfPresent(Bool.self, forKey: .isRTL)
    // TODO: Handle both seconds and ISO 8601
    let lastModified = try container.decode(Date.self, forKey: .lastModified)
    let fileSize = try container.decodeIfPresent(Int.self, forKey: .fileSize)
    let version = try container.decode(String.self, forKey: .version)
    let languages = try container.decodeIfPresent([Language].self, forKey: .languages)
    let font = try container.decodeIfPresent(Font.self, forKey: .font)
    let oskFont = try container.decodeIfPresent(Font.self, forKey: .oskFont)
    
    self.init(name: name,
              id: id,
              filename: filename,
              isDefault: isDefault,
              isRTL: isRTL,
              lastModified: lastModified,
              fileSize: fileSize,
              version: version,
              languages: languages,
              font: font,
              oskFont: oskFont)
  }
}
