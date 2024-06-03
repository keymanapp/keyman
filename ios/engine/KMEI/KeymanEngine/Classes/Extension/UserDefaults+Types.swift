//
//  UserDefaults+Types.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-26.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation
import os.log

public extension UserDefaults {
  func installableKeyboards(forKey key: String) -> [InstallableKeyboard]? {

    guard let array = array(forKey: key) as? [Data] else {
      return nil
    }
    let decoder = PropertyListDecoder()
    do {
      return try array.map { try decoder.decode(InstallableKeyboard.self, from: $0) }
    } catch {
      let message = "Error decoding keyboards: \(error))"
      os_log("%{public}s", log:KeymanEngineLogger.settings, type: .error, message)
      SentryManager.capture(error, message: message)
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
      let message = "Error decoding lexical models: \(error)"
      os_log("%{public}s", log:KeymanEngineLogger.settings, type: .error, message)
      SentryManager.capture(error, message: message)
      return nil
    }
  }

  internal func cachedPackageQueryMetadata(forKey key: String) -> [KeymanPackage.Key: KeymanPackage.DistributionStateMetadata]? {
    guard let data = self.data(forKey: key) else {
      // May not have been initialized.  Also, it IS just cache data.
      return nil
    }

    // Since we're dealing with dictionaries & Codable, it's probably better to use JSON here.
    let decoder = JSONDecoder()
    if let dict = try? decoder.decode([KeymanPackage.Key: KeymanPackage.DistributionStateMetadata].self, from: data) {
      return dict
    } else {
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
      let message = "Error encoding keyboards: \(error)"
      os_log("%{public}s", log:KeymanEngineLogger.settings, type: .error, message)
      SentryManager.capture(error, message: message)
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
      let message = "Error encoding lexicalModels: \(error)"
      os_log("%{public}s", log:KeymanEngineLogger.settings, type: .error, message)
      SentryManager.capture(error, message: message)
    }
  }

  func fullKeyboardID(forKey key: String) -> FullKeyboardID? {
    guard let data = data(forKey: key) else {
      return nil
    }
    do {
      return try PropertyListDecoder().decode(FullKeyboardID.self, from: data)
    } catch {
      let message = "Error decoding FullKeyboardID: \(error)"
      os_log("%{public}s", log:KeymanEngineLogger.settings, type: .error, message)
      SentryManager.capture(error, message: message)
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
      let message = "Error encoding FullKeyboardID: \(error)"
      os_log("%{public}s", log:KeymanEngineLogger.settings, type: .error, message)
      SentryManager.capture(error, message: message)
    }
  }
    
  func fullLexicalModelID(forKey key: String) -> FullLexicalModelID? {
    guard let data = data(forKey: key) else {
      return nil
    }
    do {
      return try PropertyListDecoder().decode(FullLexicalModelID.self, from: data)
    } catch {
      let message = "Error decoding FullLexicalModelID: \(error)"
      os_log("%{public}s", log:KeymanEngineLogger.settings, type: .error, message)
      SentryManager.capture(error, message: message)
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
      let message = "Error encoding FullLexicalModelID: \(error)"
      os_log("%{public}s", log:KeymanEngineLogger.settings, type: .error, message)
      SentryManager.capture(error, message: message)
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

  internal var cachedPackageQueryMetadata: [KeymanPackage.Key: KeymanPackage.DistributionStateMetadata] {
    get {
      return cachedPackageQueryMetadata(forKey: Key.userPackageQueryCacheDict) ?? [:]
    }

    set(metadata) {
      let encoder = JSONEncoder()
      if let data = try? encoder.encode(metadata) {
        set(data, forKey: Key.userPackageQueryCacheDict)
      }
    }
  }

  func userResources<Resource: LanguageResource>(ofType: Resource.Type) -> [Resource]? {
    if ofType == InstallableKeyboard.self {
      return (userKeyboards as? [Resource])
    } else if ofType == InstallableLexicalModel.self {
      return (userLexicalModels as? [Resource])
    } else {
      return nil
    }
  }

  var userResources: [AnyLanguageResource]? {
    get {
      let keyboards = userKeyboards ?? []
      let lexicalModels = userLexicalModels ?? []
      
      let resources: [AnyLanguageResource] = keyboards + lexicalModels
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

  internal func cachedPackageQueryResult(forPackageKey packageKey: KeymanPackage.Key) -> KeymanPackage.DistributionStateMetadata? {
    return cachedPackageQueryMetadata[packageKey]
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
      set(version?.plainString, forKey: Key.engineVersion)
    }
  }
  
  var optSpacebarText: SpacebarText {
    get {
      if let valueString = string(forKey: Key.optSpacebarText),
         let value = SpacebarText(rawValue: valueString) {
        return value
      } else {
        return SpacebarText.LANGUAGE_KEYBOARD
      }
    }
    
    set(value) {
      set(value.rawValue, forKey: Key.optSpacebarText)
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
