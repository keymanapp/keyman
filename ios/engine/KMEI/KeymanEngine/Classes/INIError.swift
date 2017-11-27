//
//  INIError.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-11-27.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

enum INIError: Error {
  case charactersAfterSectionHeader
  case duplicateSection
  case duplicateKey
  case missingSectionClosingBracket
  case missingEqualSignInProperty
}
