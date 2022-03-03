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
      state = createDefaultKeyboardState(for: keyboardId, isActive: false)
      }
      return state
  }

  func createDefaultKeyboardState(for keyboardId: String, isActive: Bool) -> KeyboardState? {
    if let keyboardDefinition = KeyboardRepository.shared.findKeyboardDefinition(for: keyboardId) {
      return KeyboardState(definition: keyboardDefinition,
                           isActive: isActive)
    } else {
      return nil
    }
  }

  func isKeyboardActive(keyboardId: String) -> Bool {
    if let state = self.savedKeyboards[keyboardId] {
      return state.isActive
    } else {
        return false
    }
  }
  
  func saveKeyboardState(keyboardId: String, active: Bool) {
    if let keyboardState = self.loadKeyboardState(keyboardId: keyboardId) {
      if active {
        keyboardState.isActive = active
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
        let keyboardState = createDefaultKeyboardState(for: keyboardId, isActive: true)
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
  var isActive: Bool
  var suggestCorrections: Bool
  var suggestPredictions: Bool
  var lexicalModelName: String?
  var lexicalModelId: String?
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

  internal init(definition: FVKeyboardDefinition, isActive: Bool) {
    self.definition = definition
    self.isActive = isActive
    self.suggestCorrections = false
    self.suggestPredictions = false
    self.lexicalModelName = ""
    self.lexicalModelId = ""
  }
  
  func selectDictionary(lexicalModel: FVLexicalModel) {
    self.lexicalModelName = lexicalModel.name
    self.lexicalModelId = lexicalModel.id
    self.suggestPredictions = true
    self.suggestCorrections = true
  }
  
  func clearDictionary() {
    self.lexicalModelName = ""
    self.lexicalModelId = ""
    self.suggestPredictions = false
    self.suggestCorrections = false
  }
  
  func updatePredictions(on: Bool) {
    self.suggestPredictions = on
    
    // we cannot do corrections when predictions are off
    if (!on) {
      self.suggestCorrections = false
    }
  }
  
  func canSelectDictionary() -> Bool {
    return self.isActive
  }
  
  func isDictionarySelected() -> Bool {
    return !(self.lexicalModelName ?? "").isEmpty
  }
  
  // if a dictionary is selected the suggestPredictions option is available
  func canSuggestPredictions() -> Bool {
    return isDictionarySelected()
  }
  
  // if the suggestPredictions option is active, then suggestCorrections is available
  func canSuggestCorrections() -> Bool {
    return self.canSuggestPredictions() && self.suggestPredictions
  }
}
