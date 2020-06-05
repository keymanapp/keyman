//
//  KMPKeyboard.swift
//  KeymanEngine
//
//  Created by Jacob Bullock on 2/16/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

import Foundation

public class KMPKeyboard: Codable {
  public var name: String
  public var keyboardId: String
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

  public var installableKeyboards: [InstallableKeyboard] {
    var installableKeyboards : [InstallableKeyboard] = []

    for language in self.languages {
      let keyboard = InstallableKeyboard(id: keyboardId, name: name,
                                         languageID: language.languageId,
                                         languageName: language.name,
                                         version: version,
                                         isRTL: isRTL,
                                         font: displayFont,
                                         oskFont: oskFont,
                                         isCustom: true) //update this based on adhoc vs api

      installableKeyboards.append( keyboard )
    }

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
}
