//
//  KMPInfo.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 6/2/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

public class KMPMetadata: Codable {
  var system: KMPSystem
  var options: KMPOptions
  var info: KMPInfo?
  var files: [KMPFile]?
  var keyboards: [KMPKeyboard]?
  var lexicalModels: [KMPLexicalModel]?

  enum CodingKeys: String, CodingKey {
    case system
    case options
    case info
    case files
    case keyboards
    case lexicalModels
    // The following reflect members that may exist, according to our schema,
    // but that are currently unused by Keyman for iOS.
    // case strings
  }

  enum PackageType: String {
    case Keyboard
    case LexicalModel
    case Unsupported
  }

  var isValid: Bool {
    if keyboards != nil && lexicalModels != nil {
      return false
    }

    if keyboards == nil && lexicalModels == nil {
      return false
    }

    return true
  }

  var packageType: PackageType {
    if !isValid {
      return .Unsupported
    } else if keyboards != nil {
      return .Keyboard
    } else if lexicalModels != nil {
      return .LexicalModel
    } else {
      return .Unsupported
    }
  }

  var version: String? {
    return self.info?.version?.description
  }
}
