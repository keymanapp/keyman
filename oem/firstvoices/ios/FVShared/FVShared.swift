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

struct FVConstants {
  static let groupID: String = "group.FVKeyboards"
  static let kFVLoadedKeyboardList: String = "FVLoadedKeyboardList"

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

}

