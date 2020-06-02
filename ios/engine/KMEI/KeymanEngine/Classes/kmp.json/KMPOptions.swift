//
//  KMPOptions.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 6/1/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

public struct KMPOptions: Codable {
  public var readmeFile: String?
  public var graphicFile: String?

  // We do this so that we can safely ignore any other entries.
  // iOS cares not for .msis or 'executeProgram'.
  enum CodingKeys: String, CodingKey {
    case readmeFile
    case graphicFile
  }
}
