//
//  InstallableKeyboard.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-24.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation
import os.log

/// Mainly differs from the API `Keyboard` by having an associated language.
public struct InstallableKeyboard: Codable, KMPInitializableLanguageResource {
  public typealias FullID = FullKeyboardID
  internal typealias Metadata = KMPKeyboard
  public typealias Package = KeyboardKeymanPackage
  
  // Details what properties are coded and decoded re: serialization.
  enum CodingKeys: String, CodingKey {
    case id
    case packageID
    case name
    case lgCode = "languageID" // The original name of the property, which we maintain for serialization.
    case languageName
    case version
    case isRTL
    case font
    case oskFont
    case isCustom
    case displayName
  }

  public private(set) var id: String
  public private(set) var packageID: String? = nil
  public var name: String
  public private(set) var lgCode: String
  public var languageName: String
  public var version: String
  public var isRTL: Bool
  public var font: Font?
  public var oskFont: Font?
  public var isCustom: Bool
  public var displayName: String?

  public static let sharingLink = "\(KeymanHosts.KEYMAN_COM)/go/keyboard/%@/share"

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

  // Weird scheme due to https://stackoverflow.com/a/58774558.
  public var typedFullID: FullKeyboardID {
    return FullKeyboardID(keyboardID: id, languageID: languageID)
  }

  public var fullID: FullKeyboardID {
    return typedFullID
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

  internal init?(from metadata: KMPKeyboard, packageID: String, lgCode: String) {
    self.id = metadata.id
    self.name = metadata.name
    self.lgCode = lgCode

    let languageMatches = metadata.languages.compactMap { return $0.languageId == lgCode ? $0.name : nil }
    if (languageMatches.isEmpty) {
      let message = "Could not find languageId '\(lgCode)' for package '\(packageID)'"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .default, message)
      SentryManager.capture(message, sentryLevel: .warning)
    }
    guard languageMatches.count >= 1 else {
      return nil
    }

    self.languageName = languageMatches[0]
    self.version = metadata.version
    self.isRTL = metadata.isRTL
    self.font = metadata.displayFont
    self.oskFont = metadata.oskFont
    self.packageID = packageID
    self.isCustom = false
  }

  public var fonts: [Font] {
    var fonts: [Font] = []

    if let displayFont = self.font {
      fonts.append(displayFont)
    }

    if let oskFont = self.oskFont {
      fonts.append(oskFont)
    }

    return fonts
  }

  public var sourceFilename: String {
    return "\(id).js"
  }
}
