//
//  LexicalModelAPICall.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/20/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

struct LexicalModelAPICall: Codable {
  let lexicalModels: [LexicalModel]
  
  enum CodingKeys: String, CodingKey {
    case lexicalModels
  }
  
  enum LexicalModelCodingKeys: String, CodingKey {
    case lexicalModel
  }
  
  init(from decoder: Decoder) throws {
    let lexicalModelContainer = try decoder.container(keyedBy: CodingKeys.self)
    self.lexicalModels = try lexicalModelContainer.decode([LexicalModel].self, forKey: .lexicalModels)
  }
}
