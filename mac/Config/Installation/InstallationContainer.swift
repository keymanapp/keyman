/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-02-24
 *
 * Represents the state of the installation of the Keyman input method
 * Communicates with the Keyman input method when the configuration is changed
 *
 */
import SwiftUI
import Combine
import KeymanSettings

public class InstallationContainer : ObservableObject {
  @Published public var state: String
  
  fileprivate let defaultsRepository: DefaultsRepo
  fileprivate let notificationCenter:DistributedNotificationCenter
  fileprivate let inputMethodUtil: InputMethodUtil

  public init() {
    // create the settings repository, gaining access to the app group UserDefaults
    do {
      try self.defaultsRepository = DefaultsRepository(suiteName: KeymanPaths.groupId)
      print("Found group container")
    } catch UserDefaultsError.unknownSuite {
      fatalError("Group container not found.")
    } catch {
      fatalError("Unable to access settings in group container.")
    }

    notificationCenter = DistributedNotificationCenter.default()
    inputMethodUtil = InputMethodUtil()
    state = self.defaultsRepository.readInstallationState()
    
    if self.isInstallationComplete() {
      self.validateInstallationComplete()
    }
  }
 
  public func isInstallationComplete() -> Bool {
    let isComplete = self.state == kInstallComplete
    
    print("isInstallationComplete, state: \(self.state)")
    
    return isComplete
  }
  
  /**
   * Check the state of the installation to make sure it is really complete.
   * Something may have been tampered with since the last time the configuration app was run.
   */
  public func validateInstallationComplete() {
// TODO: check the state of the installation and ensure that is complete
  }
  
  public func executeNextInstallationStep() {
    let originalState = self.state
    
    if self.state == kNewInstall {
      _ = self.migrateData()
    } else if self.state == kMigrationComplete {
      _ = self.registerKeymanInputMethod()
    } else if self.state == kInputMethodRegistered {
      _ = self.enableKeymanInputMethod()
    } else if self.state == kInputMethodEnabled {
      _ = self.requestAccessibilityPermission()
    } else if self.state == kAccessGranted {
      _ = self.promptUserToRestart()
    } else if self.state == kRestartRequired {
      _ = self.validateRestarted()
    }
    
    if (self.state != originalState) {
      self.defaultsRepository.writeInstallationState(self.state)
    }
  }
  
  public func migrateData() -> Bool {
    let success = self.inputMethodUtil.invokeKeymanInputMethodMigration()
    if (success) {
      self.state = kMigrationComplete
    }
    print("migration suceeded: \(success)")
    
    return success
  }
  
  public func checkAccessibilityPermission() {
    if (inputMethodUtil.isKeymanInputMethodRunning()) {
      let killed = self.inputMethodUtil.killKeymanInputMethod()
      print("checkAccessibilityPermission, Keyman input method killed: \(killed)")
    } else {
      print("checkAccessibilityPermission, Keyman input method not running")
    }
    
    let hasAccess = self.inputMethodUtil.invokeKeymanInputMethodCheckAccess()
    print("checkAccessibilityPermission, Keyman input method has permission: \(hasAccess)")
  }

  public func requestAccessibilityPermission() -> Bool {
    let grantedAccess = self.inputMethodUtil.invokeKeymanInputMethodGrantAccess()
    if (grantedAccess) {
      self.state = kAccessGranted
    }
    print("requestAccess suceeded: \(grantedAccess)")
    
    return grantedAccess
  }
  
  /**
   * save the time that the user was requested to restart their machine
   */
  fileprivate func writeRestartRequestTime() {
    self.defaultsRepository.writeRestartRequestTime(Date())
  }
  
  /**
   * get the time that the user was requested to restart their machine
   */
  fileprivate func readRestartRequestTime() -> Date? {
    return self.defaultsRepository.readRestartRequestTime()
  }

  /**
   * check whether the user has restarted by comparing the latest startup time to the time we requested the user to restart
   */
  public func promptUserToRestart() -> Bool {
    self.writeRestartRequestTime()
    self.state = kRestartRequired
    return true
  }
  
  /**
   * check whether the user has restarted by comparing the latest startup time to the time we requested the user to restart
   */
  public func validateRestarted() -> Bool {
    var hasRestarted = false
    
    if let timeRestartRequested = self.defaultsRepository.readRestartRequestTime() {
      if let mostRecentStartupTime = self.getMostRecentRestartTime() {
        hasRestarted = mostRecentStartupTime > timeRestartRequested
        print("mostRecentStartupTime: \(mostRecentStartupTime), timeRestartRequested: \(timeRestartRequested)")
        self.markInstallationComplete()
      }
    }
    print("validateRestarted: \(hasRestarted)")
    return hasRestarted
  }

  func resetInstallation() {
    self.state = kNewInstall
    self.defaultsRepository.writeInstallationState(kNewInstall)
    self.defaultsRepository.clearRestartRequestTime()
  }

  func markInstallationComplete() {
    self.state = kInstallComplete
    self.defaultsRepository.writeInstallationState(kInstallComplete)
    self.defaultsRepository.clearRestartRequestTime()
  }

  /**
   * return the last time the system was booted
   */
  func getMostRecentRestartTime() -> Date? {
      var timeSince1970 = timeval()
      var size = MemoryLayout<timeval>.size
      
      // Query the kernel for the boot time
      let result = sysctlbyname("kern.boottime", &timeSince1970, &size, nil, 0)
      
      if result == 0 {
          // Convert the timeval (seconds since 1970) into a Swift Date
          return Date(timeIntervalSince1970: Double(timeSince1970.tv_sec) + Double(timeSince1970.tv_usec) / 1_000_000.0)
      } else {
          return nil
      }
  }

//  if let bootDate = getLastRestartTime() {
//      print("Last restart time: \(bootDate)")
//  }

  public func debug() {
    let version = inputMethodUtil.getKeymanInputMethodVersion()
    let enabled = inputMethodUtil.isKeymanInputMethodEnabled()
    let running = inputMethodUtil.isKeymanInputMethodRunning()
    print("Keyman status, version: \(version ?? ""), enabled: \(enabled), running: \(running)")
  }

  public func registerKeymanInputMethod() -> Bool {
    let success = self.inputMethodUtil.registerKeymanInputMethod()
    if (success) {
      self.state = kInputMethodRegistered
    }
    print("registerKeymanInputMethod suceeded: \(success)")
    
    return success
  }
  
  public func selectKeymanInputMethod() -> Bool {
    let success = self.inputMethodUtil.selectKeymanInputMethod()
    if (success) {
      self.state = kInputMethodSelected
    }
    print("selectKeymanInputMethod suceeded: \(success)")
    
    return success
  }
  
  public func enableKeymanInputMethod() -> Bool {
    let success = self.inputMethodUtil.enableKeymanInputMethod()
    if (success) {
      self.state = kInputMethodEnabled
    }
    print("enableKeymanInputMethod suceeded: \(success)")
    
    return success
  }
  
  public func runKeymanInputMethod() -> Bool {
    return self.inputMethodUtil.runKeymanInputMethod()
  }
  
  public func killKeymanInputMethod() -> Bool {
    return self.inputMethodUtil.killKeymanInputMethod()
  }
  
  public func disableKeymanInputMethod() -> Bool {
    return self.inputMethodUtil.disableKeymanInputMethod()
  }
  
  public func uninstall() {
    self.inputMethodUtil.uninstallKeyman()
  }

  /*
  public init() {
    self.settings = SettingsContainer()
    
    notificationCenter = DistributedNotificationCenter.default()
        self.keyboardPackages = []
    
        if let keyboardsUrl = self.pathUtil.keymanKeyboardsDirectory {
    
          if FileManager.default.fileExists(atPath: keyboardsUrl.path) {
            ConfigLogger.shared.testLogger.debug("directory exists: \(keyboardsUrl.absoluteString)")
          } else {
            ConfigLogger.shared.testLogger.debug("non-existent directory: \(keyboardsUrl.absoluteString)")
          }
        }
    
     load keyboards from disk
    let packageSourceArray = self.dataRepo.readKeyboardPackageSource()
    
     create a KeymanPackage object for each PackageSource and insert it in the array
        for source in packageSourceArray {
          let package = KeymanPackage(packageSource: source)
          self.keyboardPackages.append(package)
        }
        
        ConfigLogger.shared.testLogger.debug("posting to notification center from KeyFig")
        notificationCenter.postNotificationName(NSNotification.Name("com.keyman.removedkeyboard"), object: nil, userInfo: ["data": "khmer angkor"], deliverImmediately: false)
    
  }
  */
  
}
