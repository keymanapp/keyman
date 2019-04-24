//
//  LexicalModel.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/14/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

/// LexicalModel object for Keyman Cloud API 4.0.
public struct LexicalModel: Codable {
  /// Name of the lexicalModel.
  public var name: String
  
  /// ID of the lexicalModel.. Always matches the filename of the lexicalModel..
  public var id: String
  
  /// Name of the lexicalModel. `.js` file which should be appended to `Options.lexicalModel.BaseURI`.
  /// Name of the lexicalModel KMP file.
  public var packageFilename: String
  
  /// The lexicalModel. is the recommended default for the language.
  public var isDefault: Bool
  
  /// Date the lexicalModel. was last updated.
  public var lastModified: Date
  
  /// Size of the lexicalModel. file in bytes.
  public var fileSize: Int?
  
  /// Dot-decimal version number of the lexicalModel..
  public var version: String
  
  /// Tags of languages supported by this lexicalModel..
  public var languages: [String]?
  
  enum CodingKeys: String, CodingKey {
    case name
    case id
    case packageFilename
    case isDefault = "default"
    case lastModified
    case fileSize
    case version
    case languages
  }
  
  public init(name: String,
              id: String,
              packageFilename: String,
              isDefault: Bool?,
              lastModified: Date,
              fileSize: Int?,
              version: String,
              languages: [String]?) {
    self.name = name
    self.id = id
    self.packageFilename = packageFilename
    self.isDefault = isDefault ?? false
    self.lastModified = lastModified
    self.fileSize = fileSize
    self.version = version
    self.languages = languages
  }
  
  public init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    
    let name = try container.decode(String.self, forKey: .name)
    let id = try container.decode(String.self, forKey: .id)
    let packageFilename = try container.decode(String.self, forKey: .packageFilename)
    let isDefault = try container.decodeIfPresent(Bool.self, forKey: .isDefault)
    // TODO: Handle both seconds and ISO 8601
    let lastModified = try container.decode(Date.self, forKey: .lastModified)
    let fileSize = try container.decodeIfPresent(Int.self, forKey: .fileSize)
    let version = try container.decode(String.self, forKey: .version)
    let languages = try container.decodeIfPresent([String].self, forKey: .languages)
    
    self.init(name: name,
              id: id,
              packageFilename: packageFilename,
              isDefault: isDefault,
              lastModified: lastModified,
              fileSize: fileSize,
              version: version,
              languages: languages)
  }
}

