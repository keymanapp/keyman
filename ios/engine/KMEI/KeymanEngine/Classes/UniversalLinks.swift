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
  
  // The standard technique for launching URLs externally is dependent upon being
  // within an app, not in an app-extension.  Furthermore, that distinction is made
  // at bundle compile-time... so we simply CAN'T condition on that within the framework.
  // It's thus up to the app to define this method-field when desired.
  public static var externalLinkLauncher: ((URL) -> Void)? = nil
  
  // e.g. https://keyman.com/keyboards/install/foo
  private static let KEYBOARD_INSTALL_LINK_REGEX = try! NSRegularExpression(pattern: "^http(?:s)?://keyman(?:-staging)?\\.com(?:\\.local)?/keyboards/install/([^?/]+)(?:\\?(.+))?$")
  // e.g. http://keyman.com.local/keyboards/foo
  private static let KEYBOARD_MATCH_ROOT_REGEX = try! NSRegularExpression(pattern: "^http(?:s)?://keyman(?:-staging)?\\.com(?:\\.local)?/keyboards([/?].*)?$");
  // e.g. https://keyman-staging.com/go/windows/14.0/download-keyboards?version=14.0.146.0
  private static let KEYBOARD_MATCH_GO_REGEX = try! NSRegularExpression(pattern: "^http(?:s)?://keyman(?:-staging)?\\.com(?:\\.local)?/go/windows/[^/]+/download-keyboards")
  
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
  
  /**
   * Returns `true` for links that should be opened externally, rather than in an app-hosted WebView.
   * `false` is returned for links we consider to be 'internal' within the Keyman ecosystem.
   */
  public static func isExternalLink(_ link: URL) -> Bool {
    let linkString = link.absoluteString
    let linkRange = NSRange(location: 0, length: linkString.utf16.count)
    
    // Case 1:  the link captured for keyboard search
    if let _ = tryParseKeyboardInstallLink(link) {
      return false
    } else if let _ = KEYBOARD_MATCH_ROOT_REGEX.firstMatch(in: linkString,
                                                           options: [],
                                                           range: linkRange) {
      return false
    } else if let _ = KEYBOARD_MATCH_GO_REGEX.firstMatch(in: linkString,
                                                         options: [],
                                                         range: linkRange) {
      return false
    } else if linkString.starts(with: "keyman:") {
      return false
    }
    
    return true
  }
}
