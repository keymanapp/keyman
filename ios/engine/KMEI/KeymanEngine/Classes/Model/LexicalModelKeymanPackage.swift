//
//  LexicalModelKeymanPackage.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/4/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

public class LexicalModelKeymanPackage : KeymanPackage {
  private var models : [KMPLexicalModel]!
  
  public override func defaultInfoHtml() -> String {
    var str = "Found lexical models in package:<br/>"
    for model in models {
      str += model.lexicalModelId! + "<br/>"
    }
    return str
  }
  
  // may be called parse()
  // returns a dictionary mapping language ID to lexical model ID
  public override func parse(json: [String:AnyObject]) {
    self.models = []
    
    if let packagedModels = json["lexicalModels"] as? [[String:AnyObject]] {
      for modelJson in packagedModels {
        let model = KMPLexicalModel.init(kmp: self)
        model.parse(json: modelJson)
        
        if(model.isValid) {
          models.append(model)
        }
      }
    }
  }
  
}

