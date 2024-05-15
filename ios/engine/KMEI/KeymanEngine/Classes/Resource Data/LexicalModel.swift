//
//  LexicalModel.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/14/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

/// LexicalModel object for Keyman Cloud API 4.0
public struct LexicalModel: Codable {
  /// ID of the lexical model. Always matches the filename of the lexical model
  public var id: String
  
  /// Name of the lexical model
  public var name: String
  
  /// license under which the author released the lexical model (optional)?
  public var license: String?
  
  /// Dot-decimal version number of the lexical model. (optional)?
  public var version: String?
  
  /// bcp47 tags of languages supported by this lexical model
  public var languages: [String]
  
  /// name of the author (optional)
  public var authorName: String?
  
  /// Size in bytes of the lexical model .js file. (optional)
  public var fileSize: Int?
  
  /// full URL of the .js file of the lexical model
  public var filename: String
  
  /// relative path of the lexical model, e.g., "release/example/en.custom"
  public var sourcePath: String?
  
  /// email address of the author (optional)
  public var authorEmail: String?
  
  /// description of the lexical model (optional)
  public var description: String?
  
  /// Size in bytes of the lexical model KMP file
  public var packageFileSize: Int?
  
  /// Full URL of the lexical model KMP file. [Note: yes, this capitalization is correct.]
  public var packageFilename: String
  
  /// Array of Full URLs of the other files included by reference in the lexical model
  public var packageIncludes: [String]?
  
  /// Whether the lexical model is the recommended default for the language
  public var isDefault: Bool?
  
  /// Date the lexical model was last updated
  public var lastModifiedDate: Date?
  
  /// Dot-decimal version number of the earliest Keyman version that supports this lexical model. (optional)
  public var minKeymanVersion: String?
  
  enum CodingKeys: String, CodingKey {
    case id
    case name
    case license
    case version
    case languages
    case authorName
    case fileSize = "jsFileSize"
    case filename = "jsFilename"
    case sourcePath
    case authorEmail
    case description
    case packageFileSize
    case packageFilename
    case packageIncludes
    case isDefault = "default"
    case lastModifiedDate
    case minKeymanVersion
  }
  
  public init(id: String,
              name: String,
              license: String?,
              version: String?,
              languages: [String],
              authorName: String?,
              fileSize: Int?,
              filename: String,
              sourcePath: String?,
              authorEmail: String?,
              description: String?,
              packageFileSize: Int?,
              packageFilename: String,
              packageIncludes: [String]?,
              isDefault: Bool?,
              lastModified: Date?,
              minKeymanVersion: String?) {
    self.id = id
    self.name = name
    self.license = license
    self.version = version
    self.languages = languages
    self.authorName = authorName
    self.fileSize = fileSize
    self.filename = filename
    self.sourcePath = sourcePath
    self.authorEmail = authorEmail
    self.description = description
    self.packageFileSize = packageFileSize
    self.packageFilename = packageFilename
    self.packageIncludes = packageIncludes
    self.isDefault = isDefault ?? false
    self.lastModifiedDate = lastModified
    self.minKeymanVersion = minKeymanVersion
  }
  
  public init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    
    let id = try container.decode(String.self, forKey: .id)
    let name = try container.decode(String.self, forKey: .name)
    let license = try container.decodeIfPresent(String.self, forKey: .license)
    let version = try container.decodeIfPresent(String.self, forKey: .version)
    let languages = try container.decode([String].self, forKey: .languages)
    let authorName = try container.decodeIfPresent(String.self, forKey: .authorName)
    let fileSize = try container.decodeIfPresent(Int.self, forKey: .fileSize)
    let filename = try container.decode(String.self, forKey: .filename)
    let sourcePath = try container.decodeIfPresent(String.self, forKey: .sourcePath)
    let authorEmail = try container.decodeIfPresent(String.self, forKey: .authorEmail)
    let description = try container.decodeIfPresent(String.self, forKey: .description)
    let packageFileSize = try container.decodeIfPresent(Int.self, forKey: .packageFileSize)
    let packageFilename = try container.decode(String.self, forKey: .packageFilename)
    let packageIncludes = try container.decodeIfPresent([String].self, forKey: .packageIncludes)
    let isDefault = try container.decodeIfPresent(Bool.self, forKey: .isDefault)
    // TODO: Handle both seconds and ISO 8601
    let lastModifiedStr = try container.decode(String.self, forKey: .lastModifiedDate)
    let lastModified : Date? = Formatter.iso8601.date(from: lastModifiedStr) ?? Formatter.iso8601noFS.date(from: lastModifiedStr)
    let minKeymanVersion = try container.decodeIfPresent(String.self, forKey: .minKeymanVersion)
    
    self.init(id: id,
              name: name,
              license: license,
              version: version,
              languages: languages,
              authorName: authorName,
              fileSize: fileSize,
              filename: filename,
              sourcePath: sourcePath,
              authorEmail: authorEmail,
              description: description,
              packageFileSize: packageFileSize,
              packageFilename: packageFilename,
              packageIncludes: packageIncludes,
              isDefault: isDefault,
              lastModified: lastModified,
              minKeymanVersion: minKeymanVersion)
  }
}

