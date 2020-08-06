//
//  UniversalLinks.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 7/27/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

public class UniversalLinks {
  public struct ParsedKeyboardInstallLink {
    public let keyboard_id: String
    public let lang_id: String?

    public var packageKey: KeymanPackage.Key {
      return KeymanPackage.Key(id: keyboard_id, type: .keyboard)
    }
  }

  private static let KEYBOARD_INSTALL_LINK_REGEX = try! NSRegularExpression(pattern: "^http(?:s)?:\\/\\/[^\\/]+\\/keyboards\\/install\\/([^?\\/]+)(?:\\?(.+))?$")

  public static func tryParseKeyboardInstallLink(_ link: URL) -> ParsedKeyboardInstallLink? {
    let linkString = link.absoluteString

    // If it matches the format for the Keyboard Universal Link URL Pattern...
    // (see https://docs.google.com/document/d/1rhgMeJlCdXCi6ohPb_CuyZd0PZMoSzMqGpv1A8cMFHY/edit?ts=5f11cb13#heading=h.qw7pas2adckj)
    if let match = KEYBOARD_INSTALL_LINK_REGEX.firstMatch(in: linkString,
                                                          options: [],
                                                          range: NSRange(location: 0, length: linkString.utf16.count)) {
      let keyboard_id_range = Range(match.range(at: 1), in: linkString)!
      let keyboard_id = String(linkString[keyboard_id_range])

      var lang_id: String? = nil
      let urlComponents = URLComponents(string: linkString)!
      if let lang_id_component = urlComponents.queryItems?.first(where: { $0.name == "bcp47" }) {
        lang_id = lang_id_component.value
      }

      return ParsedKeyboardInstallLink(keyboard_id: keyboard_id, lang_id: lang_id)
    } else {
      return nil
    }
  }
}
