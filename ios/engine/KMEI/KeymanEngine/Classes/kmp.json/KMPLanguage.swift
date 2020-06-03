//
//  KMPLanguage.swift
//  KeymanEngine
//
//  Created via refactor by Joshua Horton on 6/1/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

struct KMPLanguage: Codable {
  public var name: String
  public var languageId: String

  enum CodingKeys: String, CodingKey {
    case name
    case languageId = "id"
  }
}
