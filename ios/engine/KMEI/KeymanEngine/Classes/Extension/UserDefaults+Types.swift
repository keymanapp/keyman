//
//  UserDefaults+Types.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-26.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

public extension UserDefaults {
  func installableKeyboards(forKey key: String) -> [InstallableKeyboard]? {

    guard let array = array(forKey: key) as? [Data] else {
      return nil
    }
    let decoder = PropertyListDecoder()
    do {
      return try array.map { try decoder.decode(InstallableKeyboard.self, from: $0) }
    } catch {
      log.error("Error decoding keyboards: \(error)")
      return nil
    }
  }
  
  func installableLexicalModels(forKey key: String) -> [InstallableLexicalModel]? {
    guard let array = array(forKey: key) as? [Data] else {
      return nil
    }
    let decoder = PropertyListDecoder()
    do {
      return try array.map { try decoder.decode(InstallableLexicalModel.self, from: $0) }
    } catch {
      log.error("Error decoding lexical models: \(error)")
      return nil
    }
  }

  func set(_ keyboards: [InstallableKeyboard]?, forKey key: String) {
    guard let keyboards = keyboards else {
      removeObject(forKey: key)
      return
    }
    let encoder = PropertyListEncoder()
    do {
      let array = try keyboards.map { try encoder.encode($0) }
      set(array, forKey: key)
    } catch {
      log.error("Error encoding keyboards: \(error)")
    }
  }
    
  func set(_ lexicalModels: [InstallableLexicalModel]?, forKey key: String) {
    guard let lexicalModels = lexicalModels else {
      removeObject(forKey: key)
      return
    }
    let encoder = PropertyListEncoder()
    do {
      let array = try lexicalModels.map { try encoder.encode($0) }
      set(array, forKey: key)
    } catch {
      log.error("Error encoding lexicalModels: \(error)")
    }
  }

  func fullKeyboardID(forKey key: String) -> FullKeyboardID? {
    guard let data = data(forKey: key) else {
      return nil
    }
    do {
      return try PropertyListDecoder().decode(FullKeyboardID.self, from: data)
    } catch {
      log.error("Error decoding FullKeyboardID: \(error)")
      return nil
    }
  }

  func set(_ fullKeyboardID: FullKeyboardID?, forKey key: String) {
    guard let id = fullKeyboardID else {
      removeObject(forKey: key)
      return
    }
    do {
      let data = try PropertyListEncoder().encode(id)
      set(data, forKey: key)
    } catch {
      log.error("Error encoding FullKeyboardID: \(error)")
    }
  }
    
  func fullLexicalModelID(forKey key: String) -> FullLexicalModelID? {
    guard let data = data(forKey: key) else {
      return nil
    }
    do {
      return try PropertyListDecoder().decode(FullLexicalModelID.self, from: data)
    } catch {
      log.error("Error decoding FullLexicalModelID: \(error)")
      return nil
    }
  }
    
  func set(_ fullLexicalModelID: FullLexicalModelID?, forKey key: String) {
    guard let id = fullLexicalModelID else {
      removeObject(forKey: key)
      return
    }
    do {
      let data = try PropertyListEncoder().encode(id)
      set(data, forKey: key)
    } catch {
      log.error("Error encoding FullLexicalModelID: \(error)")
    }
  }

  var userKeyboards: [InstallableKeyboard]? {
    get {
      return installableKeyboards(forKey: Key.userKeyboardsList)
    }

    set(keyboards) {
      set(keyboards, forKey: Key.userKeyboardsList)
    }
  }
    
  var userLexicalModels: [InstallableLexicalModel]? {
    get {
      return installableLexicalModels(forKey: Key.userLexicalModelsList)
    }
    
    set(lexicalModels) {
      set(lexicalModels, forKey: Key.userLexicalModelsList)
    }
  }

  var userResources: [LanguageResource]? {
    get {
      let keyboards = userKeyboards ?? []
      let lexicalModels = userLexicalModels ?? []
      
      let resources: [LanguageResource] = keyboards + lexicalModels
      if resources.count == 0 {
        return nil
      } else {
        return resources
      }
    }
  }
  
  // stores a dictionary of lexical model ids keyed to language ids, i.e., [langID: modelID]
  var languageModelSelections: [String: String]? {
    get {
      return dictionary(forKey: Key.userPreferredLexicalModels) as? [String : String]
    }
    
    set(lexicalModelPrefs) {
      set(lexicalModelPrefs, forKey: Key.userPreferredLexicalModels)
    }
  }
  
  // returns lexical model id, given language id
  func preferredLexicalModelID(forLanguage lgCode: String) -> String? {
    if let modelPrefs = languageModelSelections {
      return modelPrefs[lgCode]
    } else {
      return nil
    }
  }
  
  func set(preferredLexicalModelID: String?, forKey key: String) {
    var modelPrefs: [String: String]?
    modelPrefs = languageModelSelections
    if modelPrefs == nil {
      modelPrefs = [String: String]()
    }
    modelPrefs?[key] = preferredLexicalModelID
    languageModelSelections = modelPrefs
  }

  var currentKeyboardID: FullKeyboardID? {
    get {
      return fullKeyboardID(forKey: Key.userCurrentKeyboard)
    }

    set(fullKeyboardID) {
      set(fullKeyboardID, forKey: Key.userCurrentKeyboard)
    }
  }
    
  var currentLexicalModelID: FullLexicalModelID? {
    get {
      return fullLexicalModelID(forKey: Key.userCurrentLexicalModel)
    }
    
    set(fullLexicalModelID) {
      set(fullLexicalModelID, forKey: Key.userCurrentLexicalModel)
    }
  }

  func userKeyboard(withFullID fullID: FullKeyboardID) -> InstallableKeyboard? {
    return userKeyboards?.first { $0.fullID == fullID }
  }
    
  func userLexicalModel(withFullID fullID: FullLexicalModelID) -> InstallableLexicalModel? {
    return userLexicalModels?.first { $0.fullID == fullID }
  }
  
  func userLexicalModelsForLanguage(languageID: String) -> [InstallableLexicalModel]? {
    return userLexicalModels?.filter({
      $0.languageID == languageID
    }) ?? []
  }

  var migrationLevel: Int {
    get {
      return integer(forKey: Key.migrationLevel)
    }

    set(level) {
      set(level, forKey: Key.migrationLevel)
    }
  }

  var lastEngineVersion: Version? {
    get {
      if let valueString = string(forKey: Key.engineVersion) {
        return Version(valueString)
      } else {
        return nil
      }
    }

    set(version) {
      set(version?.string, forKey: Key.engineVersion)
    }
  }
  
  // stores a dictionary of predict-enablement settings keyed to language ids, i.e., [langID: Bool]
  var predictionEnablements: [String: Bool]? {
    get {
      return dictionary(forKey: Key.userPredictSettings) as? [String : Bool]
    }
    
    set(prefs) {
      set(prefs, forKey: Key.userPredictSettings)
    }
  }
  
  // stores a dictionary of correction-enablement settings keyed to language ids, i.e., [langID: Bool]
  var correctionEnablements: [String: Bool]? {
    get {
      return dictionary(forKey: Key.userCorrectSettings) as? [String : Bool]
    }
    
    set(prefs) {
      set(prefs, forKey: Key.userCorrectSettings)
    }
  }
  
  func predictSettingForLanguage(languageID: String) -> Bool {
    if let dict = predictionEnablements {
      return dict[languageID] ?? true
    } else {
      return true
    }
  }
  
  func set(predictSetting: Bool, forLanguageID: String) {
    var prefs: [String: Bool]?
    prefs = predictionEnablements
    if prefs == nil {
      prefs = [String: Bool]()
    }
    prefs?[forLanguageID] = predictSetting
    predictionEnablements = prefs
  }
  
  func correctSettingForLanguage(languageID: String) -> Bool {
    if let dict = correctionEnablements {
      return dict[languageID] ?? true
    } else {
      return true
    }
  }
  
  func set(correctSetting: Bool, forLanguageID: String) {
    var prefs: [String: Bool]?
    prefs = correctionEnablements
    if prefs == nil {
      prefs = [String: Bool]()
    }
    prefs?[forLanguageID] = correctSetting
    correctionEnablements = prefs
  }
}
