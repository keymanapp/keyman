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

  private var _activeKeyboards: [String:KeyboardState]!
  
  public var activeKeyboards: [String:KeyboardState] {
    get {
      if _activeKeyboards == nil {
        _activeKeyboards = self.loadKeyboardStateFromUserDefaults()
      }
      return _activeKeyboards!
    }
    set {
      _activeKeyboards = newValue
    }
  }

  /*
   * Attempts to load an existing keyboard state from storage.
   * If it is not in storage, create a new default keyboard state or return nil if
   * keyboard is not defined.
   */
  func loadKeyboardState(keyboardId: String) -> KeyboardState? {
    var state:KeyboardState? = nil;
    
    if let state = activeKeyboards[keyboardId] {
      return state
    } else {
      state = createDefaultKeyboardState(for: keyboardId, isActive: false)
      }
      return state
  }

  func hasActiveKeyboards() -> Bool {
    return !activeKeyboards.isEmpty
  }
  
  /*
   * Create a new default keyboard state object or nil if specified keyboard is not defined.
   */
  func createDefaultKeyboardState(for keyboardId: String, isActive: Bool) -> KeyboardState? {
    if let keyboardDefinition = KeyboardRepository.shared.findKeyboardDefinition(for: keyboardId) {
      return KeyboardState(definition: keyboardDefinition,
                           isActive: isActive)
    } else {
      return nil
    }
  }
  
  func saveKeyboardState(state: KeyboardState) {
    if (state.isActive) {
      // add to keyboard state map (or update if already exists)
      self.activeKeyboards[state.keyboardId] = state
    } else {
      // is not active, so remove from keyboard state map if it is there
      self.activeKeyboards.removeValue(forKey: state.keyboardId)
    }
    // persist the entire map
    self.saveKeyboardStateMapToSettingsMap()
  }

  /*
   * Load persisted keyboard settings from UserDefaults.
   * If new and old format of the settings exist, delete the old format.
   */
  func loadKeyboardStateFromUserDefaults() -> [String:KeyboardState] {
    var keyboardsMap: [String:KeyboardState] = [:]
    let sharedData: UserDefaults = FVShared.userDefaults()
    
    if let data = sharedData.data(forKey: FVConstants.kFVKeyboardSettingsMap) {
      do {
        let settingsMap = try JSONDecoder().decode([String:KeyboardSettings].self, from: data)
         // if there is a keyboard settings map, then create a KeyboardState map from it
         keyboardsMap = self.createKeyboardMapFromKeyboardSettings(settingsMap: settingsMap)
      } catch  {
         print(error)
      }

      // data is already in the latest format, so it is safe to remove old format, if it exists
      if (sharedData.object(forKey: FVConstants.kFVLoadedKeyboardList) != nil) {
        sharedData.removeObject(forKey: FVConstants.kFVLoadedKeyboardList)
      }
    }
    else
      if let keyboardIds = sharedData.array(forKey: FVConstants.kFVLoadedKeyboardList) as? [String] {
      // else if the legacy String array is found, then build default settings objects for it
      keyboardsMap = self.createKeyboardMapFromKeyboardNameArray(keyboardIds: keyboardIds)
    }
    else {
      // else if no data is found in UserDefaults, then return an empty map
      keyboardsMap = [:]
    }

    return keyboardsMap
  }
  
  /*
   * Convert the hashmap of KeyboardState objects into a hashmap of KeyboardSettings structs
   * which can then be persisted to the UserDefaults.
   */
  func createSettingsMapFromKeyboardStateMap(keyboards: [String:KeyboardState]) -> [String:KeyboardSettings] {
    var settingsMap: [String:KeyboardSettings] = [:]
    
    for (name, keyboard) in keyboards {
      let settings  = KeyboardSettings(keyboardName: keyboard.name,
                                       keyboardId: keyboard.keyboardId,
                                       isActive: keyboard.isActive,
                                       suggestCorrections: keyboard.suggestCorrections, suggestPredictions: keyboard.suggestPredictions, lexicalModelName: keyboard.lexicalModelName!, lexicalModelId: keyboard.lexicalModelId!)
      settingsMap[name] = settings
      }

    return settingsMap
  }
 
  func createKeyboardState(for keyboardId: String, isActive: Bool) -> KeyboardState? {
    if let keyboardDefinition = KeyboardRepository.shared.findKeyboardDefinition(for: keyboardId) {
      
      return KeyboardState(definition: keyboardDefinition,
                           isActive: isActive)
    } else {
      return nil
    }
  }

  /*
   * used to convert map of KeyboardSettings to map of KeyboardState object
   */
  func createKeyboardMapFromKeyboardSettings(settingsMap: [String:KeyboardSettings]) -> [String:KeyboardState] {
    var keyboardsMap: [String:KeyboardState] = [:]
    
    for (name, settings) in settingsMap {
      if let keyboardState = createKeyboardState(for: name, isActive: settings.isActive) {
        keyboardState.suggestCorrections = settings.suggestCorrections
        keyboardState.suggestPredictions = settings.suggestPredictions
        keyboardState.lexicalModelName = settings.lexicalModelName
        keyboardState.lexicalModelId = settings.lexicalModelId
        keyboardsMap[name] = keyboardState
      }
    }

    return keyboardsMap
  }

  /*
   * used to convert legacy format keyboard name array to keyboard map
   */
  func createKeyboardMapFromKeyboardNameArray(keyboardIds: [String]) -> [String:KeyboardState] {
    var keyboardsMap: [String:KeyboardState] = [:]
    
    // the keyboard array contains only active keyboards
    for keyboardId in keyboardIds {
      let keyboardState = createDefaultKeyboardState(for: keyboardId, isActive: true)
      keyboardsMap[keyboardId] = keyboardState
    }
    
    return keyboardsMap
  }

  func saveKeyboardStateMapToSettingsMap() {
    let settings:[String:KeyboardSettings] = createSettingsMapFromKeyboardStateMap(keyboards: self.activeKeyboards)
    let sharedData: UserDefaults = FVShared.userDefaults()
    guard let data = try? JSONEncoder().encode(settings) else { return }
    sharedData.set(data, forKey: FVConstants.kFVKeyboardSettingsMap)
  }
  
  // deprecated but retained in case of need to test with data saved in old format
  func saveKeyboardIdArrayToUserDefaults() {
    let sharedData: UserDefaults = FVShared.userDefaults()
    let keyboardIds: [String] = Array(activeKeyboards.keys)
    sharedData.set(keyboardIds, forKey: FVConstants.kFVLoadedKeyboardList)
  }
}

struct KeyboardSettings: Codable {
  internal init(keyboardName: String, keyboardId: String, isActive: Bool, suggestCorrections: Bool, suggestPredictions: Bool, lexicalModelName: String, lexicalModelId: String) {
    self.keyboardName = keyboardName
    self.keyboardId = keyboardId
    self.isActive = isActive
    self.suggestCorrections = suggestCorrections
    self.suggestPredictions = suggestPredictions
    self.lexicalModelName = lexicalModelName
    self.lexicalModelId = lexicalModelId
  }
  
  let keyboardName: String
  let keyboardId: String
  let isActive: Bool
  let suggestCorrections: Bool
  let suggestPredictions: Bool
  let lexicalModelName: String
  let lexicalModelId: String
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
