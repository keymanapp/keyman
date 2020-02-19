//
//  UserDefaults.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 2/19/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
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
  }
}
