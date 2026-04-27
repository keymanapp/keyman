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
  
  let kEnabledKeyboardsKey = "KMEnabledKeyboardsKey"
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
  
  /**
   * get the list of enabled keyboards from the UserDefaults
   * The enabled keyboards are those that are shown in the Keyman menu
   * when it is selected from the System Input Source menu.
   */
  public func readEnabledKeyboards() -> Set<String> {
    let enabledKeyboardsArray = self.defaults.stringArray(forKey: kEnabledKeyboardsKey) ?? []
    let enabledKeyboardsSet = Set(enabledKeyboardsArray)
    
    return enabledKeyboardsSet
  }
  
  /**
   * update the list of enabled keyboards in the UserDefaults
   */
  public func writeEnabledKeyboards(enabledKeyboardsArray: [String]) {
    self.defaults.set(enabledKeyboardsArray, forKey: kEnabledKeyboardsKey)
  }
  
  /**
   * return the selected keyboard from the UserDefaults
   * The selected keyboard is the one that Keyman is applying for each keydown event.
   */
  public func readSelectedKeyboard() -> String {
    return self.defaults.string(forKey: kSelectedKeyboardKey) ?? ""
  }
  
  /**
   * update the selected keyboard keyboards in the UserDefaults
   */
  public func writeSelectedKeyboard(keyboardName: String) {
    self.defaults.set(keyboardName, forKey: kSelectedKeyboardKey)
  }
  
  /**
   * read the boolean setting which allows a Sentry error to be generated for testing
   * This value is set in the **standard** application UserDefaults -- not the shared app group defaults.
   * This is necessary because it must be set to true from the command line.
   */
  public func readForceSentryError() -> Bool {
    return UserDefaults.standard.bool(forKey: kShowOskOnActivateKey)
  }

  /**
   * read the data model version for the settings to know what values and format to expect
   */
  public func readDataModelVersion() -> Int {
    // note that zero is returned if key is not found in UserDefaults,
    return self.defaults.integer(forKey: kDataModelVersionKey)
  }

  /**
   * read the boolean setting which indicates whether the OSK is opened when the input method is activated
   * There may be no need to read this from with the config app.
   */
  public func readShowOskOnActivate() -> Bool {
    return self.defaults.bool(forKey: kShowOskOnActivateKey)
  }

  /**
   * read the list of persisted options that are recorded dynamically from the input method
   * There may be no need to read this from with the config app.
   */
  public func readPersistedOptions() -> Dictionary<String, Any> {
    if let options: Dictionary<String, Any> = self.defaults.dictionary(forKey: kPersistedOptionsKey) {
      return options
    } else {
      return [:]
    }
  }
  
/**
 * for debugging: prints UserDefaults values to the console
 * with app group UserDefaults, there is no way to view from the command line
 * (unlike standard application-level UserDefaults)
 */
  public func logSettings() {
    print("UserDefaults:")
    print("\(kSelectedKeyboardKey): \(self.readSelectedKeyboard())")
    print("\(kDataModelVersionKey): \(self.readDataModelVersion())")
    print("\(kForceSentryErrorKey): \(self.readForceSentryError())")
    print("\(kShowOskOnActivateKey): \(self.readShowOskOnActivate())")
    print("\(kEnabledKeyboardsKey): \(self.readEnabledKeyboards())")
    print("\(kPersistedOptionsKey): \(self.readPersistedOptions())")
  }

  /**
   * for debugging: clear all the entries for the app group UserDefaults
   * unlike standard application-level UserDefaults, there is no way to view from the command line
   */
  public func clearSettings() {
    self.defaults.dictionaryRepresentation().keys.forEach { key in
      self.defaults.removeObject(forKey: key)
    }
  }
}
