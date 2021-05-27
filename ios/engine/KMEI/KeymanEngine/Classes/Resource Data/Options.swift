//
//  Options.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-24.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

/// Options object for Keyman Cloud API 4.0.
public struct Options: Codable {
  /// For filtering API calls.
  public enum Device: String, Codable {
    case iphone
    case ipad
    case any
  }

  /// Base URL for keyboard filenames.
  public let keyboardBaseURL: URL
  /// Base URL for lexical model filenames.
  public let lexicalModelBaseURL: URL?

  /// Base URL for font filenames.
  public let fontBaseURL: URL

  public enum CodingKeys: String, CodingKey {
    case keyboardBaseURL = "keyboardBaseUri"
    case lexicalModelBaseURL = "lexicalModelBaseUri"
    case fontBaseURL = "fontBaseUri"
  }
}
