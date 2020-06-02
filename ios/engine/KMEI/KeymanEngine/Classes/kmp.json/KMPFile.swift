//
//  KMPFile.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 6/1/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

public struct KMPFile: Codable {
  var name: String
  var description: String

  enum CodingKeys: String, CodingKey {
    case name
    case description
    // case copyLocation // deliberately omitted
  }
}
