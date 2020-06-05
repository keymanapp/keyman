//
//  KMPSystem.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 6/1/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

public struct KMPSystem: Codable {
  public var keymanDeveloperVersion: String?
  public var fileVersion: String

  enum CodingKeys: String, CodingKey {
    case keymanDeveloperVersion
    case fileVersion
  }
}
