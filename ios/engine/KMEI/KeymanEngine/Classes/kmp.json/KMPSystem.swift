//
//  KMPSystem.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 6/1/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

struct KMPSystem: Codable {
  public var keymanDeveloperVersion: String?
  public var fileVersion: String

  init() {
    keymanDeveloperVersion = nil

    // Serves as a simple-enough default.  Not sure what the actual
    // rules that compiled packages follow for this are... but we
    // don't actually use this value within KeymanEngine, anyway.
    fileVersion = "1.1.0"
  }
}
