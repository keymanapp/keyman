//
//  Language.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-24.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

/// Language object for Keyman Cloud API 4.0.
public struct Language: Codable {
  /// Name of language.
  public let name: String
  
  /// BCP-47 language code.
  public let id: String
  
  /// Corresponding Keyboard objects.
  public let keyboards: [Keyboard]?
  
  /// Corresponding lexical models.
  public let lexicalModels: [LexicalModel]?
  
  /// Font for input fields (and OSK if `oskFont` is not present).
  public let font: Font?
  
  /// Font for the OSK.
  public let oskFont: Font?
  
  init(from kmpLanguage: KMPLanguage) {
    self.name = kmpLanguage.name
    self.id = kmpLanguage.languageId
    
    keyboards = nil
    lexicalModels = nil
    font = nil
    oskFont = nil
  }
  
  init(name: String, id: String, keyboards: [Keyboard]?, lexicalModels: [LexicalModel]?, font: Font?, oskFont: Font?) {
    self.name = name
    self.id = id
    self.keyboards = keyboards
    self.lexicalModels = lexicalModels
    self.font = font
    self.oskFont = oskFont
  }
}
