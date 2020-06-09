//
//  FullKeyboardID.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-12-08.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

/// A complete identifier for an `InstallableKeyboard`. Keyboards must have unique `FullKeyboardID`s.
public struct FullKeyboardID: Codable, LanguageResourceFullID {
  public var keyboardID: String
  public var languageID: String

  public var id: String {
    return keyboardID
  }

  public var type: LanguageResourceType {
    return .keyboard
  }
}

// MARK: - Equatable
extension FullKeyboardID: Equatable {
  public static func ==(lhs: FullKeyboardID, rhs: FullKeyboardID) -> Bool {
    return lhs.keyboardID == rhs.keyboardID && lhs.languageID == rhs.languageID
  }
}

// MARK: - CustomStringConvertible
extension FullKeyboardID: CustomStringConvertible {
  public var description: String {
    return "<keyboardID: \"\(keyboardID)\", languageID: \"\(languageID)\">"
  }
}
