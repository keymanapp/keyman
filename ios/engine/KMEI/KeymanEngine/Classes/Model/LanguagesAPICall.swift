//
//  LanguagesAPICall.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-24.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

struct LanguagesAPICall: Codable {
  let options: Options
  let languages: [Language]

  enum CodingKeys: String, CodingKey {
    case options
    case languages
  }

  enum LanguagesCodingKeys: String, CodingKey {
    case languages
  }

  init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    let languagesContainer = try container.nestedContainer(keyedBy: LanguagesCodingKeys.self, forKey: .languages)

    self.options = try container.decode(Options.self, forKey: .options)
    self.languages = try languagesContainer.decode([Language].self, forKey: .languages)
  }
}
