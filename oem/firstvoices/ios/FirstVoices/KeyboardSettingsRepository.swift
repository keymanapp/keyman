/*
 * KeyboardSettingsRepository.swift
 * FirstVoices app
 *
 * License: MIT
 *
 * Copyright © 2022 FirstVoices.
 *
 * Created by Shawn Schantz on 2022-01-26.
 *
 * Class responsible for reading and writing the selected keyboards and related settings
 * from the application's UserDefaults database.
 *
 */

import Foundation

class KeyboardSettingsRepository {

  static let shared: KeyboardSettingsRepository  = {
    let instance = KeyboardSettingsRepository()
    return instance
  }()
    
  private init() {
  }

  private var _savedKeyboards: [String:KeyboardState]!
  
  private var savedKeyboards: [String:KeyboardState] {
    get {
      if _savedKeyboards == nil {
        _savedKeyboards = self.loadInstalledKeyboardsFromUserDefaults()
      }
      return _savedKeyboards!
    }
    set {
      _savedKeyboards = newValue
    }
  }

  // TODO: improve error handling
  /*
   * Attempts to load an existing keyboard state from storage.
   * If it is not in storage, create a new default keyboard state object.
   * If the keyboardId is not found, return nil.
   */
  func loadKeyboardState(keyboardId: String) -> KeyboardState? {
    var state:KeyboardState? = nil;
    
    if let state = savedKeyboards[keyboardId] {
      return state
    } else {
        state = createDefaultKeyboardState(for: keyboardId, enabled: false)
      }
      return state
  }

  func createDefaultKeyboardState(for keyboardId: String, enabled: Bool) -> KeyboardState? {
    if let keyboardDefinition = KeyboardRepository.shared.findKeyboardDefinition(for: keyboardId) {
      return KeyboardState(definition: keyboardDefinition,
                                isEnabled: enabled,
                                offerCorrections: false,
                                offerPredictions: false,
                                selectedDictionary: "")
    } else {
      return nil
    }
  }

  func isKeyboardEnabled(keyboardId: String) -> Bool {
    if let state = self.savedKeyboards[keyboardId] {
      return state.isEnabled
    } else {
        return false
    }
  }
  
  func saveKeyboardState(keyboardId: String, enabled: Bool) {
    if let keyboardState = self.loadKeyboardState(keyboardId: keyboardId) {
      if enabled {
        keyboardState.isEnabled = enabled
        self.savedKeyboards[keyboardId] = keyboardState
    } else {
      // invalid keyboardId
        self.savedKeyboards.removeValue(forKey: keyboardId)
      }
    }
    self.saveKeyboardIdArrayToUserDefaults()
  }
  
  func loadInstalledKeyboardsFromUserDefaults() -> [String:KeyboardState] {
    var keyboardsMap: [String:KeyboardState] = [:]
    let sharedData: UserDefaults = FVShared.userDefaults()
    
    if let keyboardIds = sharedData.array(forKey: FVConstants.kFVLoadedKeyboardList) as? [String] {
      for keyboardId in keyboardIds {
        let keyboardState = createDefaultKeyboardState(for: keyboardId, enabled: true)
        keyboardsMap[keyboardId] = keyboardState
      }
    }
    return keyboardsMap
  }

  func saveKeyboardIdArrayToUserDefaults() {
    let sharedData: UserDefaults = FVShared.userDefaults()
    let keyboardIds: [String] = Array(savedKeyboards.keys)
    sharedData.set(keyboardIds, forKey: FVConstants.kFVLoadedKeyboardList)
  }
}

class KeyboardState {
  let definition: FVKeyboardDefinition
  var isEnabled: Bool
  var suggestCorrections: Bool
  var suggestPredictions: Bool
  var selectedDictionary: String?
  var name: String {
      get {
        return definition.name
      }
  }
  var keyboardId: String {
      get {
        return definition.keyboardId
      }
  }
  var languageTag: String {
      get {
        return definition.languageTag
      }
  }
  var version: String {
      get {
        return definition.keyboardVersion
      }
  }

  internal init(definition: FVKeyboardDefinition, isEnabled: Bool, offerCorrections: Bool, offerPredictions: Bool, selectedDictionary: String?) {
    self.definition = definition
    self.isEnabled = isEnabled
    self.suggestCorrections = offerCorrections
    self.suggestPredictions = offerPredictions
    self.selectedDictionary = selectedDictionary
  }
}
