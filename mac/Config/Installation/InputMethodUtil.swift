/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-05-11
 *
 * Stateless utility for enabling, selecting, killing and
 * getting information about the Keyman input method and
 * for selecting another input source
 */

import Foundation
import Carbon.HIToolbox
import AppKit
import KeymanSettings

public class InputMethodUtil {
  public let keymanInputMethodApplicationName = "Keyman.app"
  fileprivate let pathUtil: KeymanPaths
  fileprivate var inputMethodProccesId: pid_t = 0
  
  let kMigrateCommand = "migrate"
  let kAccessCommand = "access"
  let kCheckCommand = "check"

  public init() {
    self.pathUtil = KeymanPaths()
  }
  
  /**
   * true if the Keyman input method of the correct version exists in the correct location
   */
  public func isKeymanInputMethodCurrent()  -> Bool {
    // MAC-CONFIG-TODO: implement with version check
    return true
  }

  /**
   * Returns version number string of Keyman input method
   */
  public func getKeymanInputMethodVersion() -> String? {
    return self.appVersion(applicationName: keymanInputMethodApplicationName)
  }
  
  /**
   * Returns true if the Keyman input method is running
   */
  public func isKeymanInputMethodRunning() -> Bool {
    return self.isApplicationRunning(bundleId: KeymanPaths.keymanBundleId)
  }
  
  /**
   * returns true if the specified bundleId is enabled
   */
  public func isKeymanInputMethodEnabled() -> Bool {
    return self.isInputMethodEnabled(bundleId: KeymanPaths.keymanBundleId)
  }
  
  /**
   * launch the Keyman input method
   */
  public func runKeymanInputMethod() -> Bool {
    let success = self.launchKeymanInputMethodAsSeparateProcess() == 0
    return success
  }
  
  /**
   * attempts to register the Keyman input method and returns true if successful
   * a newly installed input method must be registered before enabling
   */
  public func registerKeymanInputMethod() -> Bool {
    return self.registerInputMethod(bundleId: KeymanPaths.keymanBundleId)
  }
  
  /**
   * attempts to enable the Keyman input method and returns true if successful
   */
  public func enableKeymanInputMethod() -> Bool {
    return self.enableInputMethod(bundleId: KeymanPaths.keymanBundleId)
  }

  /**
   * attempts to select the Keyman input method and returns true if successful
   */
  public func selectKeymanInputMethod() -> Bool {
    return self.selectInputSource(inputSourceId: KeymanPaths.keymanBundleId)
  }

  /**
   * attempts to disable the Keyman input method and returns true if successful
   */
  public func disableKeymanInputMethod() -> Bool {
    return self.disableInputMethod(bundleId: KeymanPaths.keymanBundleId)
  }
  
  /**
   * Kill Keyman -- only permitted when running oustide sandbox
   */
  public func killKeymanInputMethod() -> Bool {
    return killApplication(bundleId: KeymanPaths.keymanBundleId)
  }
  
  // MAC-CONFIG_TODO: deleting the app files with default security settings, need some other approach to uninstall
  /**
   * uninstalls the Keyman input method
   */
  public func uninstallKeyman() {
    _ = self.killKeymanInputMethod()
    _ = self.disableKeymanInputMethod()
    self.deleteKeyman()
  }
  
  /**
   * Returns version number string for application with specified name
   */
  func appVersion(applicationName: String) -> String? {
    var version: String? = nil
    
    if let location = pathUtil.buildInputMethodPathUrl(fileName: applicationName) {
      print("app location: \(location.absoluteString)")
      if let keyputBundle = Bundle(url: location) {
        print("app bundle path: \(keyputBundle.bundlePath)")
        if let infoDictionary = keyputBundle.infoDictionary {
          print("infoDictionary key count: \(infoDictionary.count)")
          if let appVersion = infoDictionary["CFBundleShortVersionString"] as? String {
            print("app '\(applicationName)', appVersion: \(appVersion)")
            version = appVersion
          }
        }
      } else {
        print("cannot create KeyPut bundle")
      }
      /*
       if let keyputBundle = Bundle(url: location), let infoDictionary = keyputBundle.infoDictionary, let appVersion = infoDictionary["CFBundleShortVersionString"] as? String {
       ConfigLogger.shared.testLogger.debug("App Version: \(appVersion)")
       } else {
       ConfigLogger.shared.testLogger.debug("App Version not found")
       }
       */
    } else {
      print("app '\(applicationName)' not found")
    }
    
    return version
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
  
  func invokeKeymanInputMethodMigration() -> Bool {
    print("invokeKeymanInputMethodMigration()")
    return self.invokeKeymanInputMethodAsSubProcess(argument: kMigrateCommand) == 0
  }
  
  func invokeKeymanInputMethodGrantAccess() -> Bool {
    print("invokeKeymanInputMethodGrantAccess()")
    return self.launchKeymanInputMethodAsSeparateProcess(argument: kAccessCommand) == 0
  }
  
  func invokeKeymanInputMethodCheckAccess() -> Bool {
    print("invokeKeymanInputMethodCheckAccess()")
    return self.launchKeymanInputMethodAsSeparateProcess(argument: kCheckCommand) == 0
  }
  
  /**
   * run Keyman as a subprocess with the specifed argument and return the result
   */
  func invokeKeymanInputMethodAsSubProcess(argument: String) -> Int {
    var result = -1
    let process = Process()
    if let executableUrl = self.pathUtil.buildInputMethodExecutableUrl(fileName: self.keymanInputMethodApplicationName) {
      process.executableURL = executableUrl
      print("invoking Keyman at: \(String(describing: process.executableURL))")
      process.arguments = [argument]
    }
    
    var currentEnv = ProcessInfo.processInfo.environment
    print("current env: \(String(describing: currentEnv))")
    
    currentEnv["__CFBundleIdentifier"] = KeymanPaths.keymanBundleId // set bundle ID to that of the Keyman input method
    process.environment = currentEnv
    
    do {
      try process.run() // start Keyman
      process.waitUntilExit() // wait for it to finish
      result = Int(process.terminationStatus)
    } catch {
      print("Failed to run process: \(error)")
    }
    
    print("invokeKeymanInputMethod() result: \(result)")
    return result
  }
  
  /**
   * launches the Keyman input method as an independent process with the specifed argument and return the result
   */
  func launchKeymanInputMethodAsSeparateProcess(argument: String = "") -> Int {
    let openConfig = NSWorkspace.OpenConfiguration()
    if !argument.isEmpty {
      openConfig.arguments = [argument]
    }
    
    print("NSWorkspace.OpenConfiguration: \(openConfig))")
    
    guard let inputMethodUrl = pathUtil.buildInputMethodPathUrl(fileName: self.keymanInputMethodApplicationName) else {
      print("launchKeymanInputMethodAsSeparateProcess, failed to create input method url")
      return 1
    }
    
    NSWorkspace.shared.openApplication(at: inputMethodUrl, configuration: openConfig) { (app, error) in
      if let error = error {
        print("Could not launch Keyman input method: \(error.localizedDescription)")
      } else if let runningApp = app {
        print("application: \(runningApp.localizedName ?? "unknown"), isActive: \(runningApp.isActive), isTerminated: \(runningApp.isTerminated)")
        print("bundleId: \(runningApp.bundleIdentifier ?? "unknown") processID: \(runningApp.processIdentifier)")
      }
    }
    return 0
  }
  
  /**
   * Kill the application with the specified bundle Id
   * This is only permitted when running oustide sandbox
   */
  func killApplication(bundleId: String) -> Bool {
    let runningApps = NSRunningApplication.runningApplications(withBundleIdentifier: bundleId)
    var didTerminate = false
    
    print("Running app count for \(bundleId) = \(runningApps.count)")
    if let runningApp = runningApps.first {
      let processId = runningApp.processIdentifier
      didTerminate = runningApp.terminate()
      print("process \(processId) for \(bundleId) was terminated: \(didTerminate)")
    }
    
    return didTerminate
  }
  
  /**
   * returns the TISInputSource for the specified bundleId
   */
  func getKeymanInputSource() -> TISInputSource? {
    return self.getInputSource(bundleId: KeymanPaths.keymanBundleId)
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
      print("Error: Could not find the specified input source.")
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
          print("is enabled: \(enabled)")
        } else {
          print("could not read retrieved enabled property for bundleId: \(bundleId)")
        }
      } else {
        print("Failed to get enabled property for bundleId: \(bundleId)")
      }
    } else {
      print("Failed to get input source for bundleId: \(bundleId)")
    }
    return enabled
  }
  
  /**
   * returns true if the input method with the specified bundleId is capable of being enabled
   */
  func isInputMethodEnableCapable(bundleId: String) -> Bool {
    var enableCapable = false
    
    if let inputSource = self.getInputSource(bundleId: bundleId) {
      let enableCapableValue = TISGetInputSourceProperty(inputSource, kTISPropertyInputSourceIsEnableCapable)
      if let cfType = enableCapableValue {
        // Bridge the CFTypeRef to an Unmanaged<AnyObject> and then to a Swift String
        if let capable = Unmanaged<AnyObject>.fromOpaque(cfType).takeUnretainedValue() as? Bool {
          enableCapable = capable
          print("is enable capable: \(enableCapable)")
        } else {
          print("could not read retrieved enable capable property for bundleId: \(bundleId)")
        }
      } else {
        print("Failed to get enable capable property for bundleId: \(bundleId)")
      }
    } else {
      print("Failed to get input source for bundleId: \(bundleId)")
    }
    return enableCapable
  }

  /**
   * register the newly installed input method with the specified bundleId
   * this will allow a `TISInputSourceRef` to be obtained to access the input source
   */
  func registerInputMethod(bundleId: String) -> Bool {
    var success = false
    
    guard let inputMethodUrl = pathUtil.buildInputMethodPathUrl(fileName: self.keymanInputMethodApplicationName) else {
      print("registerInputMethod, failed to create input method url")
      return false
    }
    let cfUrl = inputMethodUrl as CFURL
    
    let result = TISRegisterInputSource(cfUrl)
    success = result == noErr
    
    if (success) {
      print("registerInputMethod for bundle ID '\(bundleId)': success")
    } else {
      print("registerInputMethod for bundle ID '\(bundleId)' failed, result = \(result)")
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
        print("enableInputMethod for bundle ID '\(bundleId)': success")
      } else {
        print("enableInputMethod for bundle ID '\(bundleId)' failed, result = \(result)")
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
        print("disableInputMethod for bundle ID '\(bundleId)': success")
      } else {
        print("disableInputMethod for bundle ID '\(bundleId)' failed, result = \(result)")
      }
    }
    return success
  }
  
  func deleteKeyman() {
    let fileManager = FileManager.default
    if let keymanFile = self.pathUtil.buildInputMethodPathUrl(fileName: keymanInputMethodApplicationName) {
      do {
        try fileManager.removeItem(at: keymanFile)
        print("Successfully deleted Keyman")
      } catch {
        print("Error deleting file: \(error)")
      }
    } else {
      print("Keyman not found")
    }
  }
  
  /**
   * get the ID of the current input source
   */
  public func getCurrentInputSourceId() -> String? {
    // Get the current keyboard input source
    guard let inputSource = TISCopyCurrentKeyboardInputSource()?.takeUnretainedValue() else {
      print("Could not get current keyboard input source.")
      return nil
    }
    
    //    // Get the input source ID
    //    if let inputSourceId = TISGetInputSourceProperty(inputSource, kTISPropertyInputSourceID) {
    //      ConfigLogger.shared.testLogger.debug("Input Source ID: \(String(describing: inputSourceId))")
    //    }
    //
    let inputSourceValue = TISGetInputSourceProperty(inputSource, kTISPropertyInputSourceID)
    if let cfType = inputSourceValue {
      // Bridge the CFTypeRef to an Unmanaged<AnyObject> and then to a Swift String
      let inputSourceId = Unmanaged<AnyObject>.fromOpaque(cfType).takeUnretainedValue() as? String
      print("Input Source ID: \(String(describing: inputSourceId))")
      return inputSourceId
    } else {
      print("Failed to get input source ID property")
      return nil
    }
  }
  
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
      print("Error: Could not find the input source '\(inputSourceId)'.")
      return false
    }
    
    let result = TISSelectInputSource(targetSource)
    if result != noErr {
      print("Error selecting input source '\(inputSourceId)': \(result)")
      return false
    } else {
      print("Successfully selected input source '\(inputSourceId)'.")
      return true
    }
  }
}
