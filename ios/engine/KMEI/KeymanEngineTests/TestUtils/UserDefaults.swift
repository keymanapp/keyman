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
    static func getAndResetUserDefaultsDictionary(from storage: Storage) -> [String: Any] {
      let userDefaults = storage.userDefaults
      var currentDictionary = userDefaults.dictionaryRepresentation()
      clearUserDefaults(from: storage)
      let baseDict = userDefaults.dictionaryRepresentation()

      baseDict.forEach({ pair in
        currentDictionary.removeValue(forKey: pair.key)
      })

      return currentDictionary
    }

    static func addDefaultsDictionary(_ dictionary: [String: Any], storage: Storage) {
      //userDefaults.register(defaults: currentDictionary) // adds them in an unclearable way!
      dictionary.forEach({ pair in
        storage.userDefaults.set(pair.value, forKey: pair.key)
      })
    }

    static func getUserDefaultsDictionary(from storage: Storage) -> [String: Any] {
      let currentDictionary = getAndResetUserDefaultsDictionary(from: storage)

      // The return value is now properly constructed.  Now to put the defaults back in place before we leave.
      addDefaultsDictionary(currentDictionary, storage: storage)
      return currentDictionary
    }

    static func clearUserDefaults(from storage: Storage) {
      let userDefaults = storage.userDefaults
      let domain = Bundle.main.bundleIdentifier!
      userDefaults.synchronize()
      userDefaults.removePersistentDomain(forName: domain)
    }
  }
}
