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
  
  public override func parse(json: [String:AnyObject]) {
    self.keyboards = []
    
    if let packagedKeyboards = json["keyboards"] as? [[String:AnyObject]] {
      for keyboardJson in packagedKeyboards {
        let keyboard = KMPKeyboard.init(kmp: self)
        keyboard.parse(json: keyboardJson)
        
        if(keyboard.isValid) {
          keyboards.append(keyboard)
        }
      }
    }
  }
  
  public override func defaultInfoHtml() -> String {
    var str = "Found Keyboards in package:<br/>"
    for keyboard in keyboards {
      str += keyboard.keyboardId! + "<br/>"
    }
    return str
  }
  
  public override func isKeyboard() -> Bool {
    return true
  }
  
}

