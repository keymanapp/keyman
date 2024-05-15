//
//  KMPOptions.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 6/1/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

struct KMPOptions: Codable {
  public var readmeFile: String? = nil
  public var graphicFile: String? = nil
  
  // We do this so that we can safely ignore any other entries.
  // iOS cares not for .msis or 'executeProgram'.
  enum CodingKeys: String, CodingKey {
    case readmeFile
    case graphicFile
  }
}
