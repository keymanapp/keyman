/*
 * FVKeyboardState.swift
 * FirstVoices app
 *
 * License: MIT
 *
 * Copyright © 2022 FirstVoices.
 *
 * Created by Shawn Schantz on 2022-01-26.
 *
 * Class that encapsualtes the state of a keyboard
 *
 */

import Foundation

class FVKeyboardState {
  static private var cache:[String:FVKeyboardState] = [:]

  static private var _savedKeyboards: [String]!
  
  static private var savedKeyboards: [String] {
    get {
      if _savedKeyboards == nil {
        _savedKeyboards = FVRegionStorage.loadKeyboardListFromUserDefaults()
      }
      return _savedKeyboards!
    }
  }

  let definition: FVKeyboardDefinition
  var isEnabled: Bool
  var suggestCorrections: Bool
  var suggestPredictions: Bool
  var selectedDictionary: String?
  var name: String {
      get {
        return definition.name      }
  }
  var keyboardId: String {
      get {
        return definition.keyboardId      }
  }
  var languageTag: String {
      get {
        return definition.languageTag      }
  }
  var version: String {
      get {
        return definition.keyboardVersion      }
  }

  internal init(definition: FVKeyboardDefinition, isEnabled: Bool, offerCorrections: Bool, offerPredictions: Bool, selectedDictionary: String?) {
    self.definition = definition
    self.isEnabled = isEnabled
    self.suggestCorrections = offerCorrections
    self.suggestPredictions = offerPredictions
    self.selectedDictionary = selectedDictionary
  }
  
  static func loadKeyboardState(keyboardId: String) -> FVKeyboardState? {
    
    var state:FVKeyboardState? = nil;
    
    let keyboardEnabled: Bool = savedKeyboards.contains(keyboardId)

    // load from cache
    if let state = cache[keyboardId] {
      return state
    } else {
      // create default keyboard state
      if let keyboardDefinition: FVKeyboardDefinition = FVKeyboardPackage.availableKeyboards[keyboardId] {
        state = FVKeyboardState(definition: keyboardDefinition, isEnabled: keyboardEnabled,
                                offerCorrections: false, offerPredictions: false,
                                selectedDictionary: "")
        
        // add to cache
        cache[keyboardId] = state
      }
      return state
    }
  }
}
