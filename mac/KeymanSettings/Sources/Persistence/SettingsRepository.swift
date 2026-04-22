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
  let kDataModelVersionKey = "KMDataModelVersion"
  let kShowOskOnActivateKey = "KMShowOskOnActivate"
  let kForceSentryErrorKey = "KMForceSentryError"
  
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
  
  public func writeActiveKeyboards(activeKeyboardsArray: [String]) {
    guard let sharedDefaults = self.defaults else {
      return
      // TODO: throw error?
      //throw SettingsError.internalError("Group container UserDefaults not found.")
    }
    
    sharedDefaults.set(activeKeyboardsArray, forKey: kActiveKeyboardsKey)

    //TODO: remove, for debug only
    let activeKeyboardArrayFromDefaults = sharedDefaults.stringArray(forKey: kActiveKeyboardsKey) ?? []
    print("Active keyboards from defaults: \(activeKeyboardArrayFromDefaults)")
  }
  
  public func readSelectedKeyboard() -> String {
    guard let sharedDefaults = self.defaults else {
      print("Group container UserDefaults not found.")
      return ""
    }
    
    return sharedDefaults.string(forKey: kSelectedKeyboardKey) ?? ""
  }
  
  public func writeSelectedKeyboard(keyboardName: String) {
    if let sharedDefaults = self.defaults {
      sharedDefaults.set(keyboardName, forKey: kSelectedKeyboardKey)
    }
  }
  
  public func readForceSentryError() -> Bool {
    guard let sharedDefaults = self.defaults else {
      print("Group container UserDefaults not found.")
      return false
    }
    
    return sharedDefaults.bool(forKey: kShowOskOnActivateKey)
  }

  public func readShowOskOnActivate() -> Bool {
    guard let sharedDefaults = self.defaults else {
      print("Group container UserDefaults not found.")
      return false
    }
    
    return sharedDefaults.bool(forKey: kShowOskOnActivateKey)
  }

  public func readDataModelVersion() -> Int {
    guard let sharedDefaults = self.defaults else {
      print("Group container UserDefaults not found.")
      return 0
    }
    
    // note that zero is returned if key is not found in UserDefaults,
    return sharedDefaults.integer(forKey: kDataModelVersionKey)
  }

  public func readPersistedOptions() -> Dictionary<String, Any> {
    guard let sharedDefaults = self.defaults else {
      print("Group container UserDefaults not found.")
      return [:]
    }
    
    if let options: Dictionary<String, Any> = sharedDefaults.dictionary(forKey: kPersistedOptionsKey) {
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
    guard let sharedDefaults = self.defaults else {
      print("Group container UserDefaults not found.")
      return
    }
    
    sharedDefaults.dictionaryRepresentation().keys.forEach { key in
      sharedDefaults.removeObject(forKey: key)
    }
  }
}
