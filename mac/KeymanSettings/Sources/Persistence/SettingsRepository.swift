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
  let defaults: UserDefaults?
  
  let kActiveKeyboardsKey = "KMActiveKeyboardsKey"
  let kSelectedKeyboardKey = "KMSelectedKeyboardKey"
  let kPersistedOptionsKey = "KMPersistedOptionsKey"
  let kDataModelVersionKey = "KMDataModelVersionKey"
  let kShowOskOnActivateKey = "KMShowOskOnActivate"
  let kForceSentryError = "KMForceSentryError"
  
  //  public init() {
  //    self.pathUtil = KeymanPaths()
  //    self.defaultsSuiteName = KeymanPaths.groupId
  //  }
  //
  public init(suiteName: String) throws(Error) {
    self.pathUtil = KeymanPaths()
    self.defaultsSuiteName = suiteName
    
    let userDefaults = UserDefaults(suiteName: suiteName)
    if userDefaults == nil {
      throw(UserDefaultsError.unknownSuite)
    } else {
      self.defaults = userDefaults
    }
  }
  
  public func readActiveKeyboards() -> Set<String> {
    guard let sharedDefaults = self.defaults else {
      print("Group container UserDefaults not found.")
      return Set([])
    }
    
    let activeKeyboardsArray = sharedDefaults.stringArray(forKey: kActiveKeyboardsKey) ?? []
    let activeKeyboardsSet = Set(activeKeyboardsArray)
    
    return activeKeyboardsSet
  }
  
  public func readSelectedKeyboard() -> String {
    if let sharedDefaults = self.defaults {
      return sharedDefaults.value(forKey: kSelectedKeyboardKey) as? String ?? ""
    } else {
      return ""
    }
  }
  
  public func writeSelectedKeyboard(keyboardName: String) {
    if let sharedDefaults = self.defaults {
      sharedDefaults.set(keyboardName, forKey: kSelectedKeyboardKey)
    }
  }
}
