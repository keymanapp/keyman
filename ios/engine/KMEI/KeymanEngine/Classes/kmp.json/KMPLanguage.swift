//
//  KMPLanguage.swift
//  KeymanEngine
//
//  Created via refactor by Joshua Horton on 6/1/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

struct KMPLanguage: Codable {
  public var name: String
  public var languageId: String
  
  enum CodingKeys: String, CodingKey {
    case name
    case languageId = "id"
  }
  
  public init(from decoder: Decoder) throws {
    let values = try decoder.container(keyedBy: CodingKeys.self)
    
    name = try values.decode(String.self, forKey: .name)
    languageId = try values.decode(String.self, forKey: .languageId).lowercased()
  }
  
  public init?<Resource: KMPInitializableLanguageResource>(from resource: Resource) {
    if let kbd = resource as? InstallableKeyboard {
      self.name = kbd.languageName
      self.languageId = kbd.languageID
    } else if let lm = resource as? InstallableLexicalModel {
      self.name = lm.languageID // We never stored the language name for lexical models.
      self.languageId = lm.languageID
    } else {
      return nil
    }
  }
  
  public init(name: String, languageId: String) {
    self.name = name
    self.languageId = languageId
  }
}
