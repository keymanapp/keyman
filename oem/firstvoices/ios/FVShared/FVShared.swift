//
//  FVShared.swift
//  FirstVoices
//
//  Created by Serkan Kurt on 19/11/2015.
//  Converted by Marc Durdin on 15/5/19.
//  Copyright © 2015-2019 FirstVoices. All rights reserved.
//

import Foundation

let FVGroupID: String = "group.FVKeyboards"
let kFVKeyboardList: String = "FVKeyboardList"
let kFVKeyboardNameKey: String = "FVKeyboardName"
let kFVKeyboardLanguageCodeKey: String = "FVKeyboardLanguageCode"
let kFVKeyboardFilenameKey: String = "FVKeyboardFilename"
let kFVKeyboardCheckStateKey: String = "FVKeyboardCheckState"

class FVShared {

  class func userDefaults() -> UserDefaults {
    var _userDefaults: UserDefaults? = nil
    if UserDefaults.standard.responds(to: #selector(UserDefaults.init(suiteName:))) {
      _userDefaults = UserDefaults(suiteName: FVGroupID)
    }
    if _userDefaults == nil {
      _userDefaults = UserDefaults.standard
    }
    return _userDefaults!
  }

}

