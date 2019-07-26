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
}
