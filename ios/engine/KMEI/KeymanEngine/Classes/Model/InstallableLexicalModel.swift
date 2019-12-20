//
//  InstallableLexicalModel.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/14/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

struct InstallableConstants {
  static let defaultVersion = "1.0"
}

/// Mainly differs from the API `LexicalModel` by having an associated language.
public struct InstallableLexicalModel: Codable, LanguageResource {
  // Details what properties are coded and decoded re: serialization.
  enum CodingKeys: String, CodingKey {
    case id
    case name
    case lgCode = "languageID"  // Redirects the old plain-property to something we can wrap with accessors.
    case version
    case isCustom
  }

  public private(set) var id: String
  public var name: String
  private var lgCode: String
  public var version: String
  public var isCustom: Bool

  public var languageID: String {
    return lgCode.lowercased()
  }
  
  public var fullID: FullLexicalModelID {
    return FullLexicalModelID(lexicalModelID: id, languageID: languageID)
  }
  
  public init(id: String,
              name: String,
              languageID: String,
              version: String,
              isCustom: Bool) {
    self.id = id
    self.name = name
    self.lgCode = languageID
    self.version = version
    self.isCustom = isCustom
  }
  
  public init(lexicalModel: LexicalModel, languageID: String, isCustom: Bool) {
    self.id = lexicalModel.id
    self.name = lexicalModel.name
    self.lgCode = languageID
    self.version = lexicalModel.version ?? InstallableConstants.defaultVersion
    self.isCustom = isCustom
  }
}

