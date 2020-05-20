//
//  KeyboardInfo.swift
//  FirstVoices app
//
//  License: MIT
//
//  Copyright © 2019 FirstVoices.
//
//  Created by Marc Durdin on 21/05/2019.
//

import Foundation

//
// The KeyboardInfoParser pulls out specific pieces of information from
// the .keyboard_info file for each keyboard that we need in order to
// display and install the keyboard into Keyman Engine. Most of the data
// in the files is ignored.
//
struct KeyboardInfoLanguage: Decodable {
  let displayName: String
  let languageName: String
  let scriptName: String
}

struct KeyboardInfo: Decodable {
  let id: String
  let version: String
  let name: String
  let languages: [String:KeyboardInfoLanguage?]

  enum CodingKeys: String, CodingKey {
    case id
    case version
    case name
    case languages
  }

  init(from decoder: Decoder) throws {
    let values = try decoder.container(keyedBy: CodingKeys.self)

    id = try values.decode(String.self, forKey: .id)
    version = try values.decode(String.self, forKey: .version)
    name = try values.decode(String.self, forKey: .name)

    do {
      // Ideal case - the metadata exists, giving us a dictionary of KeyboardInfoLanguage entries.
      languages = try values.decode([String:KeyboardInfoLanguage].self, forKey: .languages)
    } catch {
      // Sometimes the metadata fails to build properly, giving us only an array of language IDs.
      // If this backup fails, we deserve to crash.
      let langIdArray = try values.decode([String].self, forKey: .languages)

      var dict: [String:KeyboardInfoLanguage?] = [:]
      langIdArray.forEach({ id in
        // Fortunately, the app doesn't actually need to know the metadata; it'll function
        // fine with a nil entry.
        // If this changes, we'll just need to be sure to perform guard checks on the optional value.
        dict.updateValue(nil, forKey: id)
      })
      languages = dict
    }
  }
}

class KeyboardInfoParser {
  class func decode(file: String) throws -> KeyboardInfo {
    // Load a .keyboard_info into a KeyboardInfoData
    let fileContents: String = try String(contentsOfFile: file, encoding: .utf8).replacingOccurrences(of: "\r", with: "")
    let jsonData = fileContents.data(using: .utf8)!
    let decoder = JSONDecoder()
    return try decoder.decode(KeyboardInfo.self, from: jsonData)
  }
}
