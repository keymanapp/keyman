//
//  InstallableKeyboard.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-24.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

/// Mainly differs from the API `Keyboard` by having an associated language.
public struct InstallableKeyboard: Codable {
  public let id: String
  public let name: String
  public let languageID: String
  public let languageName: String
  public let version: String
  public let isRTL: Bool
  public let font: Font?
  public let oskFont: Font?
  public let isCustom: Bool

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

  public init(keyboard: Keyboard, language: Language) {
    self.id = keyboard.id
    self.name = keyboard.name
    self.languageID = language.id
    self.languageName = language.name
    self.version = keyboard.version
    self.isRTL = keyboard.isRTL
    self.font = keyboard.font
    self.oskFont = keyboard.oskFont
    self.isCustom = false
  }
}
