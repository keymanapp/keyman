//
//  InstallableKeyboard.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-24.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

/// Mainly differs from the API `Keyboard` by having an associated language.
public struct InstallableKeyboard: Codable, LanguageResource {
  public private(set) var id: String
  public var name: String
  public private(set) var languageID: String
  public var languageName: String
  public var version: String
  public var isRTL: Bool
  public var font: Font?
  public var oskFont: Font?
  public var isCustom: Bool

  public var fullID: FullKeyboardID {
    return FullKeyboardID(keyboardID: id, languageID: languageID)
  }

  public init(id: String,
              name: String,
              languageID: String,
              languageName: String,
              version: String,
              isRTL: Bool,
              font: Font?,
              oskFont: Font?,
              isCustom: Bool) {
    self.id = id
    self.name = name
    self.languageID = languageID
    self.languageName = languageName
    self.version = version
    self.isRTL = isRTL
    self.font = font
    self.oskFont = oskFont
    self.isCustom = isCustom
  }

  public init(keyboard: Keyboard, language: Language, isCustom: Bool) {
    self.id = keyboard.id
    self.name = keyboard.name
    self.languageID = language.id
    self.languageName = language.name
    self.version = keyboard.version
    self.isRTL = keyboard.isRTL
    self.font = keyboard.font
    self.oskFont = keyboard.oskFont
    self.isCustom = isCustom
  }
}
