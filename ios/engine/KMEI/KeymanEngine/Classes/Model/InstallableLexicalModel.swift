//
//  InstallableLexicalModel.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/14/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

/// Mainly differs from the API `LexicalModel` by having an associated language.
public struct InstallableLexicalModel: Codable {
  public var id: String
  public var name: String
  public var languageID: String
  public var version: String
  public var isCustom: Bool
  
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
    self.languageID = languageID
//    self.languageName = languageName
    self.version = version
    self.isCustom = isCustom
  }
  
  public init(lexicalModel: LexicalModel, languageID: String, isCustom: Bool) {
    self.id = lexicalModel.id
    self.name = lexicalModel.name
    self.languageID = languageID
//    self.languageName = language.name
    self.version = lexicalModel.version ?? "0.1.0"
    self.isCustom = isCustom
  }
}

