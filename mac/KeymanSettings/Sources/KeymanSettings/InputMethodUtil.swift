/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-05-11
 *
 * Stateless utility for enabling, selecting, killing, requesting Accessibility permission
 * and getting information about the Keyman input method and for selecting another input source
 */

import Foundation
import Carbon.HIToolbox
import AppKit
import OSLog

public enum KeymanVersionCheckError: Error {
  case inputMethodNotFound
  case versionNotFound
}

public enum KeymanInvocationError: Error {
  case inputMethodNotFound
  case inputMethodCouldNotBeInvoked
}

public let kAccessibilityPermissionGrantedMessage = "granted"

public class InputMethodUtil {
  public static let keymanBundleId = "keyman.inputmethod.Keyman"
  public static let groupId = "group.com.keyman"
  
  public static let keymanDomain = "keyman.com"
  public static let keymanHelpDomain = "help.keyman.com"
  public static let keymanApiDomain = "api.keyman.com"

  public let keymanInputMethodApplicationName = "Keyman.app"

  // only initialized after message is received from input method
  public var accessibilityPermissionGranted: Bool? = nil
  fileprivate let pathUtil: KeymanPaths
  fileprivate var observer: NSObjectProtocol?

  let kMigrateCommand = "migrate"
  let kAccessCommand = "access"
  let kCheckCommand = "check"
  
  public init() throws {
    try self.pathUtil = KeymanPaths()
  }
  
  /**
   * true if the Keyman input method exists at `~/Library/Input Methods`
   */
  public func keymanInputMethodExists()  -> Bool {
    guard let inputMethodUrl = pathUtil.buildInputMethodPathUrl(fileName: self.keymanInputMethodApplicationName) else {
      Logger.setup.log("Keyman input method not found, failed to create input method url")
      return false
    }
    
    return FileManager.default.fileExists(atPath: inputMethodUrl.path)
  }
  
  /**
   * Returns version number string of Keyman input method
   */
  public func getKeymanInputMethodVersion() throws -> String {
    return try self.appVersionForInputMethod(applicationName: keymanInputMethodApplicationName)
  }
  
  /**
   * Returns true if the Keyman input method is running
   */
  public func isKeymanInputMethodRunning() -> Bool {
    return self.isApplicationRunning(bundleId: InputMethodUtil.keymanBundleId)
  }
  
  /**
   * returns true if the specified bundleId is enabled
   */
  public func isKeymanInputMethodEnabled() -> Bool {
    return self.isInputMethodEnabled(bundleId: InputMethodUtil.keymanBundleId)
  }
  
  /**
   * attempts to register the Keyman input method and returns true if successful
   * a newly installed input method must be registered before enabling
   */
  public func registerKeymanInputMethod() -> Bool {
    return self.registerInputMethod(bundleId: InputMethodUtil.keymanBundleId)
  }
  
  /**
   * attempts to enable the Keyman input method and returns true if successful
   */
  public func enableKeymanInputMethod() -> Bool {
    return self.enableInputMethod(bundleId: InputMethodUtil.keymanBundleId)
  }
  
  /**
   * attempts to select the Keyman input method and returns true if successful
   */
  public func selectKeymanInputMethod() -> Bool {
    return self.selectInputSource(inputSourceId: InputMethodUtil.keymanBundleId)
  }
  
  /**
   * attempts to disable the Keyman input method and returns true if successful
   */
  public func disableKeymanInputMethod() -> Bool {
    return self.disableInputMethod(bundleId: InputMethodUtil.keymanBundleId)
  }
  
  /**
   * Kill Keyman -- only permitted when running oustide sandbox
   */
  public func killKeymanInputMethod() -> Bool {
    return killApplication(bundleId: InputMethodUtil.keymanBundleId)
  }
  
  /**
   * uninstalls the Keyman input method
   * note: commenting out for now as default security settings prevent us from deleting the app
   */
//  public func uninstallKeyman() {
//    _ = self.killKeymanInputMethod()
//    _ = self.disableKeymanInputMethod()
//    self.deleteKeyman()
//  }
  
  /**
   * Returns version number string for the specifed app located at `~/Library/Input Methods`
   */
  func appVersionForInputMethod(applicationName: String) throws -> String {
    guard let location = pathUtil.buildInputMethodPathUrl(fileName: applicationName) else {
      throw KeymanVersionCheckError.inputMethodNotFound
    }
    
    guard let keymanBundle = Bundle(url: location) else {
      throw KeymanVersionCheckError.inputMethodNotFound
    }
    
    guard let infoDictionary = keymanBundle.infoDictionary else {
      throw KeymanVersionCheckError.versionNotFound
    }
    
    guard let appVersionString = infoDictionary["CFBundleShortVersionString"] as? String else {
      throw KeymanVersionCheckError.versionNotFound
    }

    return appVersionString
  }
  
  /**
   * returns true if the application with the specified bundleId is running
   */
  func isApplicationRunning(bundleId: String) -> Bool {
    return !NSRunningApplication.runningApplications(withBundleIdentifier: bundleId).isEmpty
  }
  
  /**
   * launch the specified input method
   */
  func runInputMethod(inputMethodName: String) -> Bool {
    if let inputMethodUrl = pathUtil.buildInputMethodPathUrl(fileName: inputMethodName) {
      return NSWorkspace.shared.open(inputMethodUrl)
    } else {
      return false
    }
  }
  
  public func invokeKeymanInputMethodMigration() -> Bool {
    Logger.setup.log("invokeKeymanInputMethodMigration()")
    return self.invokeKeymanInputMethodAsSubProcess(argument: kMigrateCommand) == 0
  }
  
  public func invokeKeymanInputMethodRequestAccess() -> Bool {
    var success = false
    do {
      // because we are launching Keyman with a specific command line argument
      // for this request, we must kill it first
      _ = self.killKeymanInputMethod()
      
      try self.launchKeymanInputMethodAsSeparateProcess(argument: kAccessCommand)
      success = true
    } catch {
      Logger.setup.error("error requesting Accessibility from input method: \(error as NSError, privacy: .public)")
      LogUtil.errorBreadcrumb("error requesting Accessibility from input method: \(error as NSError)", category: .setup)
    }
    
    return success
  }
  
  /**
   * Calls Keyman input method to check whether it has accessibility permission granted.
   * The actual result is not returned from Keyman when called as a separate process.
   * After this function is called, listen to the `DistributedNotificationCenter` for the notification named
   * `accessibilityStateResponse`
   * It contains a message with a value of `granted` or `not-granted`
   */
  func invokeKeymanInputMethodCheckAccess() throws {
    Logger.setup.info("invokeKeymanInputMethodCheckAccess()")
    LogUtil.infoBreadcrumb("invokeKeymanInputMethodCheckAccess()", category: .setup)
    // because we are launching Keyman with a specific command line argument
    // for this request, we must kill it first
    _ = self.killKeymanInputMethod()
    
    try self.launchKeymanInputMethodAsSeparateProcess(argument: kCheckCommand)
  }
  
  /**
   * run Keyman as a subprocess with the specifed argument and return the result
   */
  func invokeKeymanInputMethodAsSubProcess(argument: String) -> Int {
    var result = -1
    let process = Process()
    if let executableUrl = self.pathUtil.buildInputMethodExecutableUrl(fileName: self.keymanInputMethodApplicationName) {
      process.executableURL = executableUrl
      Logger.setup.info("invoking Keyman at: \(String(describing: process.executableURL), privacy: .public)")
      LogUtil.infoBreadcrumb("invoking Keyman at: \(String(describing: process.executableURL))", category: .setup)
      process.arguments = [argument]
    }
    
    var currentEnv = ProcessInfo.processInfo.environment

    currentEnv["__CFBundleIdentifier"] = InputMethodUtil.keymanBundleId // set bundle ID to that of the Keyman input method
    process.environment = currentEnv
    
    do {
      try process.run() // start Keyman
      process.waitUntilExit() // wait for it to finish
      result = Int(process.terminationStatus)
    } catch {
      Logger.setup.error("Failed to run process: \(error as NSError, privacy: .public)")
      LogUtil.errorBreadcrumb("Failed to run process: \(error as NSError)", category: .setup)
    }
    
    return result
  }
  
  /**
   * launches the Keyman input method as an independent process with the specifed argument and return the result
   */
  func launchKeymanInputMethodAsSeparateProcess(argument: String = "") throws {
    let openConfig = NSWorkspace.OpenConfiguration()
    if !argument.isEmpty {
      openConfig.arguments = [argument]
    }
    
    guard let inputMethodUrl = pathUtil.buildInputMethodPathUrl(fileName: self.keymanInputMethodApplicationName) else {
      Logger.setup.error("launchKeymanInputMethodAsSeparateProcess, failed to create input method url")
      LogUtil.errorBreadcrumb("launchKeymanInputMethodAsSeparateProcess, failed to create input method url", category: .setup)
      throw KeymanInvocationError.inputMethodNotFound
    }
    
    NSWorkspace.shared.openApplication(at: inputMethodUrl, configuration: openConfig) { (app, error) in
      if let error = error {
        Logger.setup.error("Could not launch Keyman input method at \(inputMethodUrl), due to error: \(error as NSError, privacy: .public)")
        LogUtil.errorBreadcrumb("Could not launch Keyman input method at \(inputMethodUrl), due to error: \(error as NSError)", category: .setup)
      }
    }
  }
  
  /**
   * Calls Keyman input method to check whether it has accessibility permission granted.
   * Receives response as distributed notification named `accessibilityStateResponse`
   */
  public func doAsyncAccessibilityCheck() {
    do {
      try self.invokeKeymanInputMethodCheckAccess()
    } catch {
      Logger.setup.error("invoking Keyman failed: \(error as NSError, privacy: .public)")
      LogUtil.errorBreadcrumb("invoking Keyman failed: \(error as NSError)", category: .setup)
    }
    
    let timeStyle = Date.FormatStyle()
      .hour(.twoDigits(amPM: Date.FormatStyle.Symbol.Hour.AMPMStyle.abbreviated))
      .minute(.twoDigits)
      .second(.twoDigits)
      .secondFraction(.fractional(3))
    Logger.setup.log("doAsyncAccessibilityCheck, listening across process boundaries, time: \(Date().formatted(timeStyle))")
  }
  
  /**
   * Kill the application with the specified bundle Id
   * This is only permitted when running outside sandbox
   */
  func killApplication(bundleId: String) -> Bool {
    let runningApps = NSRunningApplication.runningApplications(withBundleIdentifier: bundleId)
    var didTerminate = false
    
    Logger.setup.debug("Running app count for \(bundleId, privacy: .public) = \(runningApps.count)")
    if let runningApp = runningApps.first {
      let processId = runningApp.processIdentifier
      didTerminate = runningApp.terminate()
      Logger.setup.log("process \(processId) for \(bundleId, privacy: .public) was terminated: \(didTerminate)")
    }
    
    return didTerminate
  }
  
  /**
   * returns the TISInputSource for the specified bundleId
   */
  func getKeymanInputSource() -> TISInputSource? {
    return self.getInputSource(bundleId: InputMethodUtil.keymanBundleId)
  }
  
  /**
   * returns the TISInputSource for the specified bundleId
   */
  func getInputSource(bundleId: String) -> TISInputSource? {
    let properties: [String: Any] = [
      kTISPropertyInputSourceID as String: bundleId
    ]
    let inputSourceList = TISCreateInputSourceList(properties as CFDictionary, true)
    guard let sources = inputSourceList?.takeRetainedValue() as? [TISInputSource],
          let targetSource = sources.first else {
      Logger.setup.error("Could not find the specified input source with bundleID: \(bundleId, privacy: .public)")
      LogUtil.errorBreadcrumb("Could not find the specified input source with bundleID: \(bundleId)", category: .setup)
     return(nil)
    }
    
    return targetSource
  }
  
  /**
   * returns true if the input method with the specified bundleId is enabled
   */
  func isInputMethodEnabled(bundleId: String) -> Bool {
    var enabled = false
    
    if let inputSource = self.getInputSource(bundleId: bundleId) {
      let enabledValue = TISGetInputSourceProperty(inputSource, kTISPropertyInputSourceIsEnabled)
      if let cfType = enabledValue {
        // Bridge the CFTypeRef to an Unmanaged<AnyObject> and then to a Swift String
        if let inputMethodEnabled = Unmanaged<AnyObject>.fromOpaque(cfType).takeUnretainedValue() as? Bool {
          enabled = inputMethodEnabled
          Logger.setup.info("isInputMethodEnabled: \(enabled)")
          LogUtil.infoBreadcrumb("isInputMethodEnabled: \(enabled)", category: .setup)
        } else {
          Logger.setup.error("Could not read retrieved enabled property for bundleId: \(bundleId, privacy: .public)")
          LogUtil.errorBreadcrumb("Could not read retrieved enabled property for bundleId: \(bundleId)", category: .setup)
        }
      } else {
        Logger.setup.error("Failed to get enabled property for bundleId: \(bundleId, privacy: .public)")
      }
    } else {
      Logger.setup.error("Failed to get input source for bundleId: \(bundleId, privacy: .public)")
      LogUtil.errorBreadcrumb("Failed to get input source for bundleId: \(bundleId)", category: .setup)
    }
    return enabled
  }
  
  /**
   * register the newly installed input method with the specified bundleId
   * this will allow a `TISInputSourceRef` to be obtained to access the input source
   */
  func registerInputMethod(bundleId: String) -> Bool {
    var success = false
    
    guard let inputMethodUrl = pathUtil.buildInputMethodPathUrl(fileName: self.keymanInputMethodApplicationName) else {
      Logger.setup.error("registerInputMethod, failed to create input method url for bundleId: \(bundleId, privacy: .public)")
      LogUtil.errorBreadcrumb("registerInputMethod, failed to create input method url for bundleId: \(bundleId)", category: .setup)
      return false
    }
    let cfUrl = inputMethodUrl as CFURL
    
    let result = TISRegisterInputSource(cfUrl)
    success = result == noErr
    
    if (success) {
      Logger.setup.log("registerInputMethod for bundle ID '\(bundleId, privacy: .public)': success")
    } else {
      Logger.setup.error("registerInputMethod for bundle ID '\(bundleId, privacy: .public)' failed, result = \(result)")
      LogUtil.errorBreadcrumb("registerInputMethod for bundle ID '\(bundleId)' failed, result = \(result)", category: .setup)
    }
    
    return success
  }
  
  /**
   * attempts to enable the input method with the specified bundleId, returns true if successful
   */
  func enableInputMethod(bundleId: String) -> Bool {
    var success = false
    if let inputSource = self.getInputSource(bundleId: bundleId) {
      let result = TISEnableInputSource(inputSource)
      success = result == noErr
      if (success) {
        Logger.setup.log("enableInputMethod for bundle ID '\(bundleId, privacy: .public)': success")
      } else {
        Logger.setup.error("enableInputMethod for bundle ID '\(bundleId, privacy: .public)' failed, result = \(result)")
        LogUtil.errorBreadcrumb("enableInputMethod for bundle ID '\(bundleId)' failed, result = \(result)", category: .setup)
      }
    }
    return success
  }
  
  /**
   * attempts to disable the input method with the specified bundleId, returns true if successful
   */
  func disableInputMethod(bundleId: String) -> Bool {
    var success = false
    if let inputSource = self.getInputSource(bundleId: bundleId) {
      let result = TISDisableInputSource(inputSource)
      success = result == noErr
      if (success) {
        Logger.setup.log("disableInputMethod for bundle ID '\(bundleId, privacy: .public)': success")
      } else {
        Logger.setup.error("disableInputMethod for bundle ID '\(bundleId, privacy: .public)' failed, result = \(result)")
        LogUtil.errorBreadcrumb("disableInputMethod for bundle ID '\(bundleId)' failed, result = \(result)", category: .setup)
      }
    }
    return success
  }
  
  /**
   * deletes the Keyman input method
   * note: commenting out for now as default security settings prevent us from deleting the app
   */
//  func deleteKeyman() {
//    let fileManager = FileManager.default
//    if let keymanFile = self.pathUtil.buildInputMethodPathUrl(fileName: keymanInputMethodApplicationName) {
//      do {
//        try fileManager.removeItem(at: keymanFile)
//        print("Successfully deleted Keyman.app")
//      } catch {
//        print("Error deleting Keyman.app: \(error)")
//      }
//    } else {
//      print("Keyman.app not found")
//    }
//  }
  
  /**
   * select the input source with the specified input source id and return true if successful
   */
  public func selectInputSource(inputSourceId: String) -> Bool {
    let properties: [String: Any] = [
      kTISPropertyInputSourceID as String: inputSourceId
    ]
    let inputSourceList = TISCreateInputSourceList(properties as CFDictionary, false)
    guard let sources = inputSourceList?.takeRetainedValue() as? [TISInputSource],
          let targetSource = sources.first else {
      Logger.setup.error("Error: Could not find the input source '\(inputSourceId, privacy: .public)'.")
      LogUtil.errorBreadcrumb("Error: Could not find the input source '\(inputSourceId)", category: .setup)
      return false
    }
    
    let result = TISSelectInputSource(targetSource)
    if result != noErr {
      Logger.setup.error("Error selecting input source '\(inputSourceId, privacy: .public)'.")
      LogUtil.errorBreadcrumb("Error selecting input source '\(inputSourceId)", category: .setup)
     return false
    } else {
      Logger.setup.log("Successfully selected input source '\(inputSourceId, privacy: .public)'.")
      return true
    }
  }
}
