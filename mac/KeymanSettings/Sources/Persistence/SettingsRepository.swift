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

public struct SettingsRepository {
  fileprivate let pathUtil: KeymanPaths
  let defaultsSuiteName: String
  let defaults: UserDefaults?
  
  let kActiveKeyboardsKey = "KMActiveKeyboardsKey"
  let kSelectedKeyboardKey = "KMSelectedKeyboardKey"
  let kPersistedOptionsKey = "KMPersistedOptionsKey"
  let kShowOskOnActivate = "KMShowOskOnActivate"
  let kForceSentryError = "KMForceSentryError"
  
  //  public init() {
  //    self.pathUtil = KeymanPaths()
  //    self.defaultsSuiteName = KeymanPaths.groupId
  //  }
  //
  public init(suiteName: String) {
    self.pathUtil = KeymanPaths()
    self.defaultsSuiteName = suiteName
    self.defaults = UserDefaults(suiteName: suiteName)
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
    guard let sharedDefaults = self.defaults else {
      print("Group container UserDefaults not found.")
      return ""
    }
 
    let selectedKeyboard = sharedDefaults.value(forKey: kSelectedKeyboardKey) as? String ?? ""

    return selectedKeyboard
  }
}
