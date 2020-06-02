//
//  KeyboardKeymanPackage.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/14/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

public class KeyboardKeymanPackage : KeymanPackage
{
  private var keyboards: [KMPKeyboard]!
  
  public override func parse(json: [String:AnyObject], version: String) {
    self.keyboards = []
    
    if let packagedKeyboards = json["keyboards"] as? [[String:AnyObject]] {
      for keyboardJson in packagedKeyboards {
        do {
          // A temporary hybrid state; we now transition to using a Decoder-based strategy.
          let jsonData = try JSONSerialization.data(withJSONObject: keyboardJson, options: .prettyPrinted)
          let decoder = JSONDecoder()

          let keyboard = try decoder.decode(KMPKeyboard.self, from: jsonData)
          if(keyboard.isValid && FileManager.default.fileExists(atPath: self.sourceFolder.appendingPathComponent("\(keyboard.keyboardId).js").path)) {
            keyboards.append(keyboard)
          } else {
            log.debug("\(keyboard.name) not valid / corresponding file not found")
          }
        } catch {
          // Simply... don't append a keyboard, like with the .isValid check above.
          // Denotes original behavior of this method; isn't exactly optimal.
          log.debug("Error occurred when processing keyboards: \(String(describing: error))")
          continue
        }
      }
    }
  }
  
  public override func defaultInfoHtml() -> String {
    var str = "Found Keyboards in package:<br/>"
    for keyboard in keyboards {
      str += keyboard.keyboardId + "<br/>"
    }
    return str
  }
  
  public override func isKeyboard() -> Bool {
    return true
  }
  
}

