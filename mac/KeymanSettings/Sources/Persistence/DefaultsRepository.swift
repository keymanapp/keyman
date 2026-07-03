/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2025-12-10
 *
 * DefaultsRepository is responsible for reading, writing and removing
 * values stored in the UserDefaults
 *
 */

import Foundation

public enum UserDefaultsError: Error {
  case unknownSuite
}

public class DefaultsRepository: DefaultsRepo {
  fileprivate let pathUtil: KeymanPaths
  let defaultsSuiteName: String
  let groupDefaults: UserDefaults

  let kEnabledKeyboardsKey = "KMEnabledKeyboardsKey"
  let kSelectedKeyboardKey = "KMSelectedKeyboardKey"
  let kPersistedOptionsKey = "KMPersistedOptionsKey"
  let kDataModelVersionKey = "KMDataModelVersion"
  let kShowOskOnActivateKey = "KMShowOskOnActivate"
  let kForceSentryErrorKey = "KMForceSentryError"
  
  let kInstallationState = "KMInstallationState"
  
  let kTimeRestartRequested = "KMTimeRestartRequested"

  public init(suiteName: String) throws(Error) {
    try self.pathUtil = KeymanPaths()
    self.defaultsSuiteName = suiteName
    
    // create defaults for the suite
    guard let userDefaults = UserDefaults(suiteName: suiteName) else {
      throw(UserDefaultsError.unknownSuite)
    }
    
    self.groupDefaults = userDefaults
  }
  
  public func installationStateExists() -> Bool {
    return self.groupDefaults.object(forKey: "kInstallationState") != nil
  }
  
  /**
   * read the dictionary of the installation info from the UserDefaults
   */
  public func readInstallationState() -> Dictionary<String, Any>? {
    return self.groupDefaults.dictionary(forKey: kInstallationState)
  }
  
  /**
   * write the dictionary of the installation info to the UserDefaults
   */
  public func writeInstallationState(_ dictionary: Dictionary<String, Any>) {
    self.groupDefaults.set(dictionary, forKey: kInstallationState)
  }
  
  /**
   * delete the dictionary of the installation info to the UserDefaults
   */
  public func deleteInstallationState() {
    self.groupDefaults.removeObject(forKey: kInstallationState)
  }

 /**
   * get the list of enabled keyboards from the UserDefaults
   * The enabled keyboards are those that are shown in the Keyman menu
   * when it is selected from the System Input Source menu.
   */
  public func readEnabledKeyboards() -> Set<String> {
    let enabledKeyboardsArray = self.groupDefaults.stringArray(forKey: kEnabledKeyboardsKey) ?? []
    let enabledKeyboardsSet = Set(enabledKeyboardsArray)
    
    return enabledKeyboardsSet
  }
  
  /**
   * update the list of enabled keyboards in the UserDefaults
   * each String in the array must be formatted `/[packageName]/[keyboardName].kmx`
   */
  public func writeEnabledKeyboards(enabledKeyboardsArray: [String]) {
    self.groupDefaults.set(enabledKeyboardsArray, forKey: kEnabledKeyboardsKey)
  }
  
  /**
   * return the selected keyboard from the UserDefaults
   * The selected keyboard is the one that Keyman is applying for each keydown event.
   */
  public func readSelectedKeyboard() -> String {
    return self.groupDefaults.string(forKey: kSelectedKeyboardKey) ?? ""
  }
  
  /**
   * update the selected keyboard keyboards in the UserDefaults
   * `keyboardName` must be formatted `/[packageName]/[keyboardName].kmx`
   */
  public func writeSelectedKeyboard(keyboardName: String) {
    self.groupDefaults.set(keyboardName, forKey: kSelectedKeyboardKey)
  }
  
  /**
   * read the boolean value that allows a Sentry error to be generated for testing
   * This value is set in the **standard** application UserDefaults -- not the shared app group defaults.
   * This is necessary because it must be set to true from the command line, and the
   * app group defaults are not accessible from the command line.
   */
  public func readForceSentryError() -> Bool {
    return UserDefaults.standard.bool(forKey: kForceSentryErrorKey)
  }
  
  /**
   * read the data model version to know what values and format to expect
   */
  public func readDataModelVersion() -> Int {
    // note that zero is returned if key is not found in UserDefaults,
    return self.groupDefaults.integer(forKey: kDataModelVersionKey)
  }
  
  /**
   * read the boolean value that indicates whether the OSK is opened when the input method is activated
   * There may be no need to read this from with the config app.
   */
  public func readShowOskOnActivate() -> Bool {
    return self.groupDefaults.bool(forKey: kShowOskOnActivateKey)
  }
  
  /**
   * read the list of persisted options that are recorded dynamically from the input method
   * There may be no need to read this from with the config app.
   */
  public func readPersistedOptions() -> Dictionary<String, Any> {
    if let options: Dictionary<String, Any> = self.groupDefaults.dictionary(forKey: kPersistedOptionsKey) {
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
  public func logDefaults() {
    print("UserDefaults:")
    print("\(kSelectedKeyboardKey): \(self.readSelectedKeyboard())")
    print("\(kDataModelVersionKey): \(self.readDataModelVersion())")
    print("\(kForceSentryErrorKey): \(self.readForceSentryError())")
    print("\(kShowOskOnActivateKey): \(self.readShowOskOnActivate())")
    print("\(kEnabledKeyboardsKey): \(self.readEnabledKeyboards())")
    print("\(kPersistedOptionsKey): \(self.readPersistedOptions())")
    print("\(kInstallationState): \(self.readInstallationState()?.description ?? "nil")")
  }
  
  /**
   * for debugging: clear all the entries for the app group UserDefaults
   * unlike standard application-level UserDefaults, there is no way to view from the command line
   */
  public func clearDefaults() {
    self.groupDefaults.dictionaryRepresentation().keys.forEach { key in
      self.groupDefaults.removeObject(forKey: key)
    }
  }
}
