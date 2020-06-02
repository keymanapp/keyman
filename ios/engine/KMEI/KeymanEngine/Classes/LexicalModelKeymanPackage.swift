//
//  LexicalModelKeymanPackage.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/4/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

public class LexicalModelKeymanPackage : KeymanPackage {
  public var models : [KMPLexicalModel]!
  
  public override func defaultInfoHtml() -> String {
    let count = models.count
    var str = "Found "+(count > 1 ? "\(count) dictionaries" : "dictionary")+" in package:<br/>"
    for model in models {
      str += model.lexicalModelId + "<br/>"
    }
    return str
  }
  
  // may be called parse()
  // returns a dictionary mapping language ID to lexical model ID
  override public func parse(json: [String:AnyObject], version: String) {
    self.models = []
    
    if let packagedModels = json["lexicalModels"] as? [[String:AnyObject]] {
      for modelJson in packagedModels {
      // A temporary hybrid state; we now transition to using a Decoder-based strategy.
        do {
          let jsonData = try JSONSerialization.data(withJSONObject: modelJson, options: .prettyPrinted)
          let decoder = JSONDecoder()

          let model = try decoder.decode(KMPLexicalModel.self, from: jsonData)
          if(model.isValid && FileManager.default.fileExists(atPath: self.sourceFolder.appendingPathComponent("\(model.lexicalModelId).model.js").path)) {
            models.append(model)
          }
        } catch {
          // Append no models.  Not the greatest strategy, but it's the one that had always
          // been taken here.
        }
      }
    }
  }
  
}

