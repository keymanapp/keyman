//
//  KMPInfo.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 6/2/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

/**
 * This class serializes to and deserializes from JSON following the schema found here for every package's kmp.json:
 * - https://api.keyman.com/schemas/package/1.1.0/package.json
 *
 * It should be kept updated to match the latest schema version whenever updates occur.
 *
 * Documentation link: https://help.keyman.com/developer/current-version/reference/file-types/metadata
 */
class KMPMetadata: Codable {
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

  init(from resource: LanguageResource) {
    // First, the standard defaults.
    system = KMPSystem()
    options = KMPOptions()
    info = KMPInfo()
    keyboards = nil
    lexicalModels = nil

    // Now to process the resource.
    var fileSet = [ KMPFile(resource.sourceFilename) ]
    resource.fonts.forEach {font in
      fileSet.append(KMPFile(font.source[0]))
    }
    files = fileSet

    if var keyboard = resource as? InstallableKeyboard {
      keyboard.version = "0.0.0"
      keyboards = [KMPKeyboard(from: keyboard)!]
    } else if var lexicalModel = resource as? InstallableLexicalModel {
      lexicalModel.version = "0.0.0"
      lexicalModels = [KMPLexicalModel(from: lexicalModel)!]
    } else {
      fatalError("Cannot utilize unexpected subclass of LanguageResource")
    }
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
