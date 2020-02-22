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
  // Details what properties are coded and decoded re: serialization.
  enum CodingKeys: String, CodingKey {
    case id
    case name
    case lgCode = "languageID" // The original name of the property, which we maintain for serialization.
    case languageName
    case version
    case isRTL
    case font
    case oskFont
    case isCustom
  }

  public private(set) var id: String
  public var name: String
  public private(set) var lgCode: String
  public var languageName: String
  public var version: String
  public var isRTL: Bool
  public var font: Font?
  public var oskFont: Font?
  public var isCustom: Bool

  public static let sharingLink = "https://keyman.com/go/keyboard/%@/share"

  public var sharableURL: String? {
    get {
      // We don't host custom keyboards, so we can't provide a QR link to share
      // them at this time.
      if isCustom {
        return nil
      } else {
        return String(format: InstallableKeyboard.sharingLink, id)
      }
    }
  }

  public var languageID: String {
    return lgCode.lowercased()
  }

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
    self.lgCode = languageID
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
    self.lgCode = language.id
    self.languageName = language.name
    self.version = keyboard.version
    self.isRTL = keyboard.isRTL
    self.font = keyboard.font
    self.oskFont = keyboard.oskFont
    self.isCustom = isCustom
  }
}
