//
//  InstalledKeyboard.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-24.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

/// Mainly differs from the API `Keyboard` by having an associated language.
public struct InstalledKeyboard: Codable {
  public let id: String
  public let name: String
  public let languageID: String
  public let languageName: String
  public let version: String
  public let isRTL: Bool
  public let font: Font
  public let oskFont: Font
  public let isCustom: Bool
}
