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
  @Published public var installationState: InstallationState
  
  fileprivate let defaultsRepository: DefaultsRepo
  fileprivate let notificationCenter:DistributedNotificationCenter
  fileprivate let inputMethodUtil: InputMethodUtil
  
  public init() {
    let defaultsRepo: DefaultsRepository
    // create the settings repository, gaining access to the app group UserDefaults
    do {
      defaultsRepo = try DefaultsRepository(suiteName: KeymanPaths.groupId)
      print("Found group container")
    } catch UserDefaultsError.unknownSuite {
      fatalError("Group container not found.")
    } catch {
      fatalError("Unable to access settings in group container.")
    }
    
    self.defaultsRepository = defaultsRepo
    inputMethodUtil = InputMethodUtil()
    notificationCenter = DistributedNotificationCenter.default()
        
    self.installationState = InstallationCheck(version: "1.0.0", defaultsRepo: defaultsRepo, inputMethodUtil: inputMethodUtil).evaluate()
  }
  
  /**
   * return trues if every installation task is incomplete
   */
  public func isInstallationComplete() -> Bool {
    return self.installationState.isComplete
  }
  
  /**
   * returns the next installation task which is incomplete, if there is one remaining
   * note that this function determines the order in which the tasks are executed as they are stored in an unsorted Set
   */
  public func nextTask() -> InstallationTask? {
    let incompleteTasks = installationState.tasks.filter { !$0.isComplete }
    
    if let incompleteTask = incompleteTasks.first(where: { $0.taskType == .verifyInputMethod }) {
      return incompleteTask
    } else if let incompleteTask = incompleteTasks.first(where: { $0.taskType == .migrateData }) {
      return incompleteTask
    } else if let incompleteTask = incompleteTasks.first(where: { $0.taskType == .enableInputMethod }) {
      return incompleteTask
    } else if let incompleteTask = incompleteTasks.first(where: { $0.taskType == .requestAccess }) {
      return incompleteTask
    } else if let incompleteTask = incompleteTasks.first(where: { $0.taskType == .restartMac }) {
      return incompleteTask
    }
    
    return nil
  }
 
  /**
   * returns the next installation task which is incomplete, if there is one remaining
   * note that this function determines the order in which the tasks are executed as they are stored in an unsorted Set
   */
  public func executeTask(_ task: InstallationTask) {
    var completedTask = false
    switch task.taskType {
    case .verifyInputMethod:
      completedTask = self.verifyInputMethod()
    case .migrateData:
      completedTask = self.migrateData()
    case .enableInputMethod:
      completedTask = self.enableKeymanInputMethod()
    case .requestAccess:
      completedTask = self.requestAccessibility()
    case .restartMac:
      completedTask = self.promptUserToRestart()
    }
    
    if completedTask {
      print("executeTask: \(task.taskType.rawValue) completed")
      self.installationState.markTaskAsCompleted(task: task.taskType)
      self.writeInstallationState()
    }
  }
  
  /**
   * execute the next task required for installation
   */
  public func executeNextInstallationTask() {
    if let nextTask = self.nextTask() {
      self.executeTask(nextTask)
    }
  }
  
  /**
   * run the Keyman input method as a subprocess to migrate data to the shared space and immediately exit
   */
  public func migrateData() -> Bool {
    let success = self.inputMethodUtil.invokeKeymanInputMethodMigration()
    print("migration suceeded: \(success)")
    
    return success
  }
  
  /**
   * save the installation info
   */
  fileprivate func writeInstallationState() {
    self.defaultsRepository.writeInstallationState(self.installationState.toUserDefaultsDictionary())
  }
  
 /**
   * save the time that the user was requested to restart their machine
   */
  fileprivate func writeRestartRequestTime() {
    self.installationState.dateRestartRequested = Date()
    self.writeInstallationState()
  }
  
  /**
   * get the time that the user was requested to restart their machine
   */
  fileprivate func readRestartRequestTime() -> Date? {
    return self.installationState.dateRestartRequested
  }
  
  /**
   * check whether the user has restarted by comparing the latest startup time to the time we requested the user to restart
   */
  public func promptUserToRestart() -> Bool {
    self.writeRestartRequestTime()
    return true
  }
  
  /**
   * check whether the user has restarted by comparing the latest startup time to the time we requested the user to restart
   */
  public func validateRestarted() -> Bool {
    var hasRestarted = false
    
    if let timeRestartRequested = self.installationState.dateRestartRequested {
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
//    self.state = kNewInstall
//    self.defaultsRepository.writeInstallationState(kNewInstall)
//    self.defaultsRepository.clearRestartRequestTime()
  }
  
  func markInstallationComplete() {
//    self.state = kInstallComplete
//    self.defaultsRepository.writeInstallationState(kInstallComplete)
//    self.defaultsRepository.clearRestartRequestTime()
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
  

/**
 * register may need to happen before enabling
 */
  public func registerKeymanInputMethod() -> Bool {
    let success = self.inputMethodUtil.registerKeymanInputMethod()
    print("registerKeymanInputMethod suceeded: \(success)")
    
    return success
  }
  
  /**
   * set Keyman as the current input method (same effect as choosing Keyman in the input source menu)
   */
  public func selectKeymanInputMethod() -> Bool {
    let success = self.inputMethodUtil.selectKeymanInputMethod()
    print("selectKeymanInputMethod suceeded: \(success)")
    
    return success
  }
  
  /**
   * verify that a Keyman input method exists in the correct location and is of the correct version
   * if false, this means that there was an issue with the installer package or, perhaps more likely,
   * the input method was manually replaced or deleted before the configuration app was run
   */
  public func verifyInputMethod()  -> Bool {
    // MAC-CONFIG-TODO: implement with version check
    return inputMethodUtil.isKeymanInputMethodCurrent()
  }
  
  /**
   * true if the system recognizes Keyman as an enabled input method
   */
  public func isKeymanInputMethodEnabled() -> Bool {
    return inputMethodUtil.isKeymanInputMethodEnabled()
  }
  
  /**
   * ask the system to enable the Keyman input method
   * register it first, just to be safe
   */
  public func enableKeymanInputMethod() -> Bool {
    var success = self.inputMethodUtil.registerInputMethod(bundleId: KeymanPaths.keymanBundleId)
    if success {
      success = self.inputMethodUtil.enableKeymanInputMethod()
    }
    
    print("enableKeymanInputMethod suceeded: \(success)")
    return success
  }
  
  /**
   * first kill the Keyman input method if it is running
   * second, call Keyman as a separate process with an argument that checks whether accessibility has been granted by the user
   * listen for message from Keyman to indicate the result
   */
  public func isAccessibilityGranted() -> Bool {
    if (inputMethodUtil.isKeymanInputMethodRunning()) {
      let killed = self.inputMethodUtil.killKeymanInputMethod()
      print("checkAccessibilityPermission, Keyman input method killed: \(killed)")
    } else {
      print("checkAccessibilityPermission, Keyman input method not running")
    }
    
    let hasAccess = self.inputMethodUtil.invokeKeymanInputMethodCheckAccess()
    print("checkAccessibilityPermission, Keyman input method has permission: \(hasAccess)")
    
    return hasAccess
  }
  
  /**
   * first kill the Keyman input method if it is running
   * second, call Keyman as a separate process with an argument that requests the system to prompt the user to grant accessibility
   * listen for message from Keyman to indicate the result
   */
  // MAC-CONFIG-TODO: do we get an accurate message from Keyman Input Method
  public func requestAccessibility() -> Bool {
    if (inputMethodUtil.isKeymanInputMethodRunning()) {
      let killed = self.inputMethodUtil.killKeymanInputMethod()
      print("checkAccessibilityPermission, Keyman input method killed: \(killed)")
    } else {
      print("checkAccessibilityPermission, Keyman input method not running")
    }

    let grantedAccess = self.inputMethodUtil.invokeKeymanInputMethodGrantAccess()
    print("requestAccess suceeded: \(grantedAccess)")
    
    return grantedAccess
  }

  
  /**
   * run Keyman as a separate process with its full Input Method functionality
   */
  public func runKeymanInputMethod() -> Bool {
    return self.inputMethodUtil.runKeymanInputMethod()
  }
  
  /**
   * kill the Keyman Input Method process
   */
  public func killKeymanInputMethod() -> Bool {
    return self.inputMethodUtil.killKeymanInputMethod()
  }
  
  /**
   * disable Keyman as an Input Method
   */
  public func disableKeymanInputMethod() -> Bool {
    return self.inputMethodUtil.disableKeymanInputMethod()
  }
  
  /**
   * uninstall the Keyman Input Method
   * not functional with default security settings!
   */
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
