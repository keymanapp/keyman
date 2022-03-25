//
//  FVShared.swift
//  FirstVoices app
//
//  License: MIT
//
//  Copyright © 2019 FirstVoices.
//
//  Created by Serkan Kurt on 19/11/2015.
//  Converted and rewritten by Marc Durdin on 15/05/2019.
//

import Foundation
import KeymanEngine

struct FVConstants {
  static let groupID: String = "group.FVKeyboards"
  
  // earlier format was an array of strings, if a keyboard was in the array it was active
  static let kFVLoadedKeyboardList: String = "FVLoadedKeyboardList"
  
  // updated format (2022) is Dictionary of KeyboardSettings structs  
  static let kFVKeyboardSettingsMap: String = "FVKeyboardSettingsMap"

  // Legacy keys
  static let legacy_kFVKeyboardList = "FVKeyboardList"
  static let legacy_kKeyboardsFileLastModDateKey = "KeyboardsFileLastModDate"

  static let legacy_kFVKeyboardCheckStateKey = "FVKeyboardCheckState"
  static let legacy_kFVKeyboardFilenameKey = "FVKeyboardFilename"

  // Keyboard files and paths
  static let keyboardsPath = "Keyboards"
  static let keyboardsPackage = "fv_all"
  static let keyboardsPackageExt = "kmp"
  static let keyboardsCSVPath = "Keyboards"
  static let keyboardsCSVName = "keyboards"
  static let keyboardsCSVType = "csv"
  static let keyboardInfoType = "keyboard_info"

  // Instructions web page
  static let instructionsName = "setup"
  static let instructionsType = "html"
  static let instructionsPath = "Instructions"
}

class FVShared {

  class func userDefaults() -> UserDefaults {
    var _userDefaults: UserDefaults? = nil
    if UserDefaults.standard.responds(to: #selector(UserDefaults.init(suiteName:))) {
      _userDefaults = UserDefaults(suiteName: FVConstants.groupID)
    }
    if _userDefaults == nil {
      _userDefaults = UserDefaults.standard
    }
    return _userDefaults!
  }

  // code for debugging state changes
  class func reportState(location: String) {
    print("at \(location), UserDefaults state:")
    let defaults: UserDefaults = FVShared.userDefaults()
    
    if let userKeyboards = defaults.userKeyboards {
      print(" userKeyboards count = \(userKeyboards.count)")
      
      if (userKeyboards.count > 0) {
        let keyboard = userKeyboards[0]
        if let preferredModelId = defaults.preferredLexicalModelID(forLanguage: keyboard.lgCode) {
          print(" preferredModelId = \(preferredModelId)")
        } else {
          print(" could not find preferredModelId")
        }
      }
    }
    else {
      print(" Could not find userKeyboards in FV UserDefaults")
    }
    
    if let userLexicalModels = defaults.userLexicalModels {
      print(" userLexicalModels count = \(userLexicalModels.count)")
    }
    else {
      print(" Could not find userLexicalModels in FV UserDefaults")
    }

    if let currentKeyboard = defaults.currentKeyboardID {
      print(" currentKeyboard ID = \(currentKeyboard.keyboardID)")
      print(" currentKeyboard language ID = \(currentKeyboard.languageID)")
      print(" currentKeyboard description = \(currentKeyboard.description)")
      print(" currentKeyboard type = \(currentKeyboard.type)")
    }
    else {
      print(" There is no currentKeyboardId in FV UserDefaults")
    }

    if let currentLexicalModel = defaults.currentLexicalModelID {
      print(" currentLexicalModel ID = \(currentLexicalModel.lexicalModelID)")
      print(" currentLexicalModel language ID = \(currentLexicalModel.languageID)")
      print(" currentLexicalModel description = \(currentLexicalModel.description)")
      print(" currentLexicalModel type = \(currentLexicalModel.type)")
    }
    else {
      print(" There is no currentLexicalModel in FV UserDefaults")
    }
  }
}

