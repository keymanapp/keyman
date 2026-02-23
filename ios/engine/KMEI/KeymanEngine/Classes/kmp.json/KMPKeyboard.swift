//
//  KMPKeyboard.swift
//  KeymanEngine
//
//  Created by Jacob Bullock on 2/16/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

import Foundation

class KMPKeyboard: Codable, KMPResource {
  public var name: String
  public var keyboardId: String
  public var packageId: String?
  public var version: String
  public var osk: String?
  public var font: String?
  public var isRTL: Bool = false
  // Technically, the schema says that this doesn't have to be specified.
  // However, it's a requirement for KeymanEngine for iOS.
  public var languages: [KMPLanguage]

  enum CodingKeys: String, CodingKey {
    case name
    case keyboardId = "id"
    case version
    case osk = "oskFont"
    case font = "displayFont"
    case isRTL = "rtl"
    case languages
  }

  internal required init?(from keyboard: InstallableKeyboard) {
    self.name = keyboard.name
    self.keyboardId = keyboard.id
    self.packageId = keyboard.packageID ?? keyboard.id
    self.version = keyboard.version
    self.isRTL = keyboard.isRTL

    self.font = keyboard.font?.source[0]
    self.osk = keyboard.oskFont?.source[0]

    self.languages = [KMPLanguage(name: keyboard.languageName, languageId: keyboard.languageID)]
  }
  
  required public init(from decoder: Decoder) throws {
    let values = try decoder.container(keyedBy: CodingKeys.self)

    name = try values.decode(String.self, forKey: .name)
    keyboardId = try values.decode(String.self, forKey: .keyboardId)
    version = try values.decode(String.self, forKey: .version)
    osk = try values.decodeIfPresent(String.self, forKey: .osk)
    font = try values.decodeIfPresent(String.self, forKey: .font)
    isRTL = try values.decodeIfPresent(Bool.self, forKey: .isRTL) ?? false
    languages = try values.decode([KMPLanguage].self, forKey: .languages)
  }

  func hasMatchingMetadata(for resource: InstallableKeyboard, ignoreLanguage: Bool = false, ignoreVersion: Bool = true) -> Bool {
    if id != resource.id {
      return false
    } else if !ignoreVersion, version != resource.version {
      return false
    }

    if !ignoreLanguage {
      let resourceMetadata = KMPKeyboard(from: resource)!
      return languages.contains(where: { language in
        return language.languageId == resourceMetadata.languages[0].languageId
      })
    }

    return true
  }

  internal var installableKeyboards: [InstallableKeyboard] {
    var installableKeyboards : [InstallableKeyboard] = []

    for language in self.languages {
      if let keyboard = InstallableKeyboard(from: self, packageID: packageId!, lgCode: language.languageId) {
        installableKeyboards.append( keyboard )
      }
    }

    return installableKeyboards
  }

  // Needed to properly support AnyKMPResource.installableResources
  // because of weird, weird Swift rules.
  public var typedInstallableResources: [InstallableKeyboard] {
    return installableKeyboards
  }

  // Provides our class's method of the same signature, but with
  // the local type signature we know is available.
  public var installableResources: [InstallableKeyboard] {
    return installableKeyboards
  }

  public var displayFont: Font? {
    if let f = font {
      return Font(filename: f)
    }
    return nil
  }
  
  public var oskFont: Font? {
    if let f = osk {
      return Font(filename: f)
    }
    return nil
  }
  
  public var isValid: Bool {
    // Ensures that our 'optional' members are properly instantiated.
    // Any file-based checks will be performed by the object's owner,
    // which knows the containing package's root folder.
    return installableKeyboards.count > 0 && self.languages.count > 0
  }

  public var id: String {
    return keyboardId
  }
}
