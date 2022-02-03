/*
 * KeyboardRepository.swift
 * FirstVoices app
 *
 * License: MIT
 *
 * Copyright © 2022 FirstVoices.
 *
 * Created by Shawn Schantz on 2022-01-25.
 * 
 * Class that loads data related to the definition of the Keyboard and installs and removes keyboards from Keyman.
 * Keyboard definitions are read from the First Voices kmp file and saved in an FVKeyboardDefinition
 * object to associate it with its corresponding language and language tag.
 *
 */

import Foundation
import KeymanEngine

class KeyboardRepository {

  static let shared: KeyboardRepository  = {
    let instance = KeyboardRepository()
    return instance
  }()
    
  private init() {
    self.loadAvailableKeyboards()
  }

  private var _availableKeyboards: [String:FVKeyboardDefinition] = [:]
  public var availableKeyboards: [String:FVKeyboardDefinition] {
    get {
      return _availableKeyboards
    }
  }
  
  private func loadKeyboardPackage() -> KeyboardKeymanPackage? {
    let keyboardPackagePath: String = Bundle.main.path(forResource: FVConstants.keyboardsPackage,
                                                       ofType: FVConstants.keyboardsPackageExt,
                                                       inDirectory: FVConstants.keyboardsPath)!
    let pathUrl = URL(fileURLWithPath: keyboardPackagePath)
    var keyboardsPackage: KeyboardKeymanPackage?
    
    do {
      let package = try ResourceFileManager.shared.prepareKMPInstall(from: pathUrl)
      guard package as? KeyboardKeymanPackage != nil else {
        print("Failed to load \(FVConstants.keyboardsPackage).\(FVConstants.keyboardsPackageExt)")
        return keyboardsPackage
      }

      keyboardsPackage = (package as? KeyboardKeymanPackage)!
    } catch {
      print("Failed to load \(FVConstants.keyboardsPackage).\(FVConstants.keyboardsPackageExt)")
    }
    
    return keyboardsPackage
  }
  
/*
 * Read available keyboards from the kmp file.
 * This is called so that we have the BCP 47 tag needed to query for a lexical model.
 *
 */
  func loadAvailableKeyboards() {
    if let keyboardsPackage = loadKeyboardPackage() {
      for keyboardArray in keyboardsPackage.installables {
        // assume one keyboard per package per language as that's currently all we have in practice
        let keyboard = keyboardArray[0]
        let keyboardId = keyboard.id
        let keyboardDefinition = FVKeyboardDefinition(name: keyboard.name,
                                                      keyboardId: keyboardId,
                                                      keyboardVersion: keyboard.version,
                                                      languageTag: keyboard.lgCode,
                                                      languageName: keyboard.languageName)
        // add this keyboard to the map
        _availableKeyboards[keyboardId] = keyboardDefinition
      }
    }
  }
  
  func findKeyboardDefinition(for keyboardId: String) -> FVKeyboardDefinition? {
    return KeyboardRepository.shared.availableKeyboards[keyboardId]
  }
    
  func installKeyboard(keyboard: FVKeyboardDefinition) {
    if let keyboardsPackage = loadKeyboardPackage() {
      let fullKeyboardId = FullKeyboardID(keyboardID: keyboard.keyboardId, languageID: keyboard.languageTag)
      do {
        try ResourceFileManager.shared.install(resourceWithID: fullKeyboardId, from: keyboardsPackage)
        print("Installed keyboard \(keyboard.keyboardId)")
      } catch {
        print("Failed to load preload " + keyboard.keyboardId + ": " + error.localizedDescription)
      }
    }
  }

  func removeKeyboard(keyboard: FVKeyboardDefinition) {
      let fullKeyboardId = FullKeyboardID(keyboardID: keyboard.keyboardId, languageID: keyboard.languageTag)
      let removed = Manager.shared.removeKeyboard(withFullID: fullKeyboardId)
    if (removed) {
        print("Removed keyboard \(keyboard.keyboardId)")
    } else {
      print("Could not remove keyboard \(keyboard.keyboardId)")
    }
  }

  // TODO: implement for debugging or delete
  func getPackageInfo () {
    //ResourceFileManager.getInstalledPackage(<#T##self: ResourceFileManager##ResourceFileManager#>)
  }
}

class FVKeyboardDefinition {
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
