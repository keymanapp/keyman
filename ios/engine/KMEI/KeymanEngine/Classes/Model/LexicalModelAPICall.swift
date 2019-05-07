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
    var lexicalModelContainer = try decoder.unkeyedContainer()
    let mutableLexicalModels = NSMutableArray.init()
    while !lexicalModelContainer.isAtEnd {
      let subdecoder = try lexicalModelContainer.superDecoder()
      let lexicalModel = try LexicalModel.init(from: subdecoder)
      mutableLexicalModels.add(lexicalModel)
    }
    lexicalModels = mutableLexicalModels as! [LexicalModel]
  }
}
