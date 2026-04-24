/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2025-12-10
 *
 * SettingsRepository is responsible for reading, writing and removing
 * settings stored in the UserDefaults
 *
 */

import Foundation

public enum UserDefaultsError: Error {
    case unknownSuite
}

public struct SettingsRepository {
  fileprivate let pathUtil: KeymanPaths
  let defaultsSuiteName: String
  let defaults: UserDefaults
  
  let kActiveKeyboardsKey = "KMActiveKeyboardsKey"
  let kSelectedKeyboardKey = "KMSelectedKeyboardKey"
  let kPersistedOptionsKey = "KMPersistedOptionsKey"
  let kDataModelVersionKey = "KMDataModelVersion"
  let kShowOskOnActivateKey = "KMShowOskOnActivate"
  let kForceSentryErrorKey = "KMForceSentryError"
  
  public init(suiteName: String) throws(Error) {
    self.pathUtil = KeymanPaths()
    self.defaultsSuiteName = suiteName
    
    // the UserDefaults used are for the app group
    guard let userDefaults = UserDefaults(suiteName: suiteName) else {
      throw(UserDefaultsError.unknownSuite)
    }
    
    self.defaults = userDefaults
  }
  
  public func readActiveKeyboards() -> Set<String> {
    let activeKeyboardsArray = self.defaults.stringArray(forKey: kActiveKeyboardsKey) ?? []
    let activeKeyboardsSet = Set(activeKeyboardsArray)
    
    return activeKeyboardsSet
  }
  
  public func writeActiveKeyboards(activeKeyboardsArray: [String]) {
    self.defaults.set(activeKeyboardsArray, forKey: kActiveKeyboardsKey)
  }
  
  public func readSelectedKeyboard() -> String {
    return self.defaults.string(forKey: kSelectedKeyboardKey) ?? ""
  }
  
  public func writeSelectedKeyboard(keyboardName: String) {
    self.defaults.set(keyboardName, forKey: kSelectedKeyboardKey)
  }
  
  public func readForceSentryError() -> Bool {
    return self.defaults.bool(forKey: kShowOskOnActivateKey)
  }

  public func readShowOskOnActivate() -> Bool {
    return self.defaults.bool(forKey: kShowOskOnActivateKey)
  }

  public func readDataModelVersion() -> Int {
    // note that zero is returned if key is not found in UserDefaults,
    return self.defaults.integer(forKey: kDataModelVersionKey)
  }

  public func readPersistedOptions() -> Dictionary<String, Any> {
    if let options: Dictionary<String, Any> = self.defaults.dictionary(forKey: kPersistedOptionsKey) {
      return options
    } else {
      return [:]
    }
  }
  

  public func logSettings() {
    print("UserDefaults:")
    print("\(kSelectedKeyboardKey): \(self.readSelectedKeyboard())")
    print("\(kDataModelVersionKey): \(self.readDataModelVersion())")
    print("\(kForceSentryErrorKey): \(self.readForceSentryError())")
    print("\(kShowOskOnActivateKey): \(self.readShowOskOnActivate())")
    print("\(kActiveKeyboardsKey): \(self.readActiveKeyboards())")
    print("\(kPersistedOptionsKey): \(self.readPersistedOptions())")
  }

  public func clearSettings() {
    self.defaults.dictionaryRepresentation().keys.forEach { key in
      self.defaults.removeObject(forKey: key)
    }
  }
}
