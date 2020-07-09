//
//  FullKeyboardID.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-12-08.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

/// A complete identifier for an `InstallableKeyboard`. Keyboards must have unique `FullKeyboardID`s.
public struct FullKeyboardID: Codable, LanguageResourceFullID, Hashable {
  public typealias Resource = InstallableKeyboard
  
  public var keyboardID: String
  public var languageID: String

  public var id: String {
    return keyboardID
  }

  public var type: LanguageResourceType {
    return .keyboard
  }
}

// MARK: - CustomStringConvertible
extension FullKeyboardID: CustomStringConvertible {
  public var description: String {
    return "<keyboardID: \"\(keyboardID)\", languageID: \"\(languageID)\">"
  }
}
