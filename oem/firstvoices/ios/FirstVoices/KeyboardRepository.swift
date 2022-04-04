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
    
  func installKeyboard(keyboard: FVKeyboardDefinition) -> Bool {
    // make a copy of all the active keyboards
    var newKeyboardsMap = Dictionary(uniqueKeysWithValues:
                                      KeyboardSettingsRepository.shared.activeKeyboards.map { key, value in (key, value.languageTag) })
    // add new keyboard
    newKeyboardsMap[keyboard.keyboardId] = keyboard.languageTag
  
    // Rebuilding the keyboard list will cause the pre-installed EuroLatin SIL keyboard to be deleted if it is installed.
    // EuroLatin SIL is not tracked as an active keyboard in the KeyboardSettingsRepository.
    // This is intentional as it should go away once FV keyboards are added.

    rebuildKeyboardsListInKeymanEngine(keyboards: newKeyboardsMap)
    
    return true
    
  }

  func removeKeyboard(keyboard: FVKeyboardDefinition) -> Bool {
    // make a copy of all the active keyboards
    var newKeyboardsMap = Dictionary(uniqueKeysWithValues:
                                      KeyboardSettingsRepository.shared.activeKeyboards.map { key, value in (key, value.languageTag) })
    // remove keyboard to be deleted
    newKeyboardsMap.removeValue(forKey: keyboard.keyboardId)
    
    rebuildKeyboardsListInKeymanEngine(keyboards: newKeyboardsMap)
    
    return true
  }
  
  func rebuildKeyboardsListInKeymanEngine(keyboards: [String:String]) {

    // call Keyman Engine to remove all keyboards
    while Manager.shared.removeKeyboard(at: 0) {
      print("removed existing keyboard")
    }
    
    // re-install specified keyboards 
    if let keyboardsPackage = loadKeyboardPackage() {
      for (keyboardId, languageTag) in keyboards {
        let fullKeyboardId = FullKeyboardID(keyboardID: keyboardId, languageID: languageTag)
        do {
          try ResourceFileManager.shared.install(resourceWithID: fullKeyboardId, from: keyboardsPackage)
          print("Installed keyboard \(keyboardId)")
        } catch {
          let installError = error
          print("Failed to load preload \(keyboardId) installError: \(installError.localizedDescription)")
       }
      }
    }
  }
  
  class func updateActiveKeyboardsList(keyboardList: FVRegionList, loadedKeyboards: [String]) {

    // Remove all installed keyboards first -- we'll re-add them below

    while Manager.shared.removeKeyboard(at: 0) {
    }

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

    // Iterate through the available keyboards
    for region in keyboardList {
      let keyboards = region.keyboards
      for kb in keyboards {
        if loadedKeyboards.contains(kb.id) {
          // Install the keyboard from its package.
          do {
            // Find the matching keyboard entry from the package.
            // We currently rely on the package to track each keyboard's available language tags.
            if let packageKbId = keyboardsPackage.installables.first(where: { entry in
              // Each entry in the returned array corresponds to one supported language for the keyboard.
              entry.contains { $0.id == kb.id }
              // We only install the first available language.  Easy to change, though.
            }).map({ $0[0] })?.fullID { // then
              try ResourceFileManager.shared.install(resourceWithID: packageKbId, from: keyboardsPackage)
            } else {
              print("Keyboard "+kb.id+" not found in primary keyboards package")
            continue
            }
          } catch {
            print("Failed to load preload "+kb.id+": " + error.localizedDescription)
            continue
          }
        }
      }
    }
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
