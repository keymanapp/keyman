//
//  UserDefaults.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 2/19/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import XCTest
@testable import KeymanEngine

extension TestUtils {
  class UserDefaults {
    static func getAndResetDictionary(from storage: Storage) -> [String: Any] {
      let userDefaults = storage.userDefaults
      var currentDictionary = userDefaults.dictionaryRepresentation()
      clear(from: storage)
      let baseDict = userDefaults.dictionaryRepresentation()
      
      baseDict.forEach({ pair in
        currentDictionary.removeValue(forKey: pair.key)
      })
      
      return currentDictionary
    }
    
    static func addDictionary(_ dictionary: [String: Any], storage: Storage) {
      //userDefaults.register(defaults: currentDictionary) // adds them in an unclearable way!
      dictionary.forEach({ pair in
        storage.userDefaults.set(pair.value, forKey: pair.key)
      })
    }
    
    static func getDictionary(from storage: Storage) -> [String: Any] {
      let currentDictionary = getAndResetDictionary(from: storage)
      
      // The return value is now properly constructed.  Now to put the defaults back in place before we leave.
      addDictionary(currentDictionary, storage: storage)
      return currentDictionary
    }
    
    static func clear(from storage: Storage) {
      let userDefaults = storage.userDefaults
      let domain = Bundle.main.bundleIdentifier!
      userDefaults.synchronize()
      userDefaults.removePersistentDomain(forName: domain)
    }
    
    static func addKeyboard(_ keyboard: InstallableKeyboard) {
      // Lifted from Manager.addKeyboard
      let userDefaults = Storage.active.userDefaults
      var userKeyboards = userDefaults.userKeyboards ?? []
      
      // Update keyboard if it exists
      if let index = userKeyboards.firstIndex(where: { $0.fullID == keyboard.fullID }) {
        userKeyboards[index] = keyboard
      } else {
        userKeyboards.append(keyboard)
      }
      
      userDefaults.userKeyboards = userKeyboards
    }
    
    static func addLexicalModel(_ lexicalModel: InstallableLexicalModel) {
      // Lifted from Manager.addKeyboard
      let userDefaults = Storage.active.userDefaults
      var userModels = userDefaults.userLexicalModels ?? []
      
      // Update keyboard if it exists
      if let index = userModels.firstIndex(where: { $0.fullID == lexicalModel.fullID }) {
        userModels[index] = lexicalModel
      } else {
        userModels.append(lexicalModel)
      }
      
      userDefaults.userLexicalModels = userModels
    }
    
    // Adapted from https://stackoverflow.com/a/39453212.
    static func setFromPlist(atPath path: String, for storage: Storage) throws {
      var propertyListFormat = PropertyListSerialization.PropertyListFormat.xml
      let xml = FileManager.default.contents(atPath: path)!
      
      let plistDict = try PropertyListSerialization.propertyList(from: xml, options: .mutableContainersAndLeaves, format: &propertyListFormat) as! [String: Any]
      
      plistDict.forEach({key, value in
        storage.userDefaults.set(value, forKey: key)
      })
    }
  }
}
