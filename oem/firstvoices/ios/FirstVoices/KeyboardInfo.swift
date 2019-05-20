//
//  KeyboardInfo.swift
//  FirstVoices
//
//  Created by Marc Durdin on 20/5/19.
//  Copyright © 2019 FirstVoices. All rights reserved.
//

import Foundation

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
