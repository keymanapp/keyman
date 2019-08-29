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
  let languages: [String:KeyboardInfoLanguage]
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
