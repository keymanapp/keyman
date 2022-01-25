/*
 * FVKeyboardPackage.swift
 * FirstVoices app
 *
 * License: MIT
 *
 * Copyright © 2022 FirstVoices.
 *
 * Created by Shawn Schantz on 2022-01-25.
 * 
 * Description...
 */

import Foundation
import KeymanEngine

class FVKeyboardPackage {

  static private var _availableKeyboards: [String:FVKeyboardDescriptor] = [:]
  static public var availableKeyboards: [String:FVKeyboardDescriptor] {
    get {
      return FVKeyboardPackage._availableKeyboards
    }
  }
  
/*
 * Read available keyboards from the kmp file.
 * This is called shortly after the app launches so that we have a BCP 47 tag.
 * We need this to invoke the API to query for a corresponding lexical model.
 *
 */
  class func loadAvailableKeyboards() {

    // Load the primary keyboards package so that we can install keyboards from it.
    let keyboardPackagePath: String = Bundle.main.path(forResource: FVConstants.keyboardsPackage,
                                                       ofType: FVConstants.keyboardsPackageExt,
                                                       inDirectory: FVConstants.keyboardsPath)!
    let pathUrl = URL(fileURLWithPath: keyboardPackagePath)
    let keyboardsPackage: KeyboardKeymanPackage
    do {
      let package = try ResourceFileManager.shared.prepareKMPInstall(from: pathUrl)
      guard package as? KeyboardKeymanPackage != nil else {
        print("Failed to load \(FVConstants.keyboardsPackage).\(FVConstants.keyboardsPackageExt)")
        return
    }

    keyboardsPackage = (package as? KeyboardKeymanPackage)!
    } catch {
      print("Failed to load \(FVConstants.keyboardsPackage).\(FVConstants.keyboardsPackageExt)")
      return
    }

    
    for keyboardArray in keyboardsPackage.installables {
      // assume one keyboard per package per language as that's currently all we have in practice
      let keyboard = keyboardArray[0]
      let keyboardId = keyboard.id
      let keyboardDescriptor = FVKeyboardDescriptor(name: keyboard.name, keyboardId: keyboardId,
                                                    keyboardVersion: keyboard.version, languageTag:
                                                  keyboard.lgCode, languageName: keyboard.languageName)
      
      _availableKeyboards[keyboardId] = keyboardDescriptor
    }
  }
}

class FVKeyboardDescriptor {
  let name: String
  let keyboardId: String
  let keyboardVersion: String
  let languageTag: String
  let languageName: String
  
  init(name: String, keyboardId: String, keyboardVersion: String, languageTag: String, languageName: String) {
    self.name = name
    self.keyboardId = keyboardId
    self.keyboardVersion = keyboardVersion
    self.languageTag = languageTag
    self.languageName = languageName
  }
}
