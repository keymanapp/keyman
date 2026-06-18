/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-02-24
 *
 * Manages the steps for completing the installation of the Keyman input method
 *
 */
import SwiftUI
import Combine
import KeymanSettings

@MainActor // run on the main actor since data is published directly to the UI
public class InstallationContainer : ObservableObject {
  // if the installer was run, then installed and current should be true
  public let isInputMethodInstalled : Bool
  public let isInputMethodCurrent : Bool
  // for convenience, combination of Installed and Current
  public var isCurrentInputMethodInstalled : Bool {
    isInputMethodInstalled && isInputMethodCurrent
  }
  // installationState describes the remaining tasks to complete the installation
  @Published public var installationState: InstallationState?

  fileprivate let installationCheck: InstallationCheck
  fileprivate let defaultsRepository: DefaultsRepo
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
    
    self.installationCheck = InstallationCheck(defaultsRepo: defaultsRepo, inputMethodUtil: inputMethodUtil)
    self.isInputMethodInstalled = self.installationCheck.isInputMethodInstalled
    self.isInputMethodCurrent = self.installationCheck.isInputMethodCurrent
    self.installationState = self.installationCheck.installationState
  }
  
  /**
   * Returns true if the Accessibility permission has been granted by the user for the Keyman input method.
   * This is an optional return value because it is only set in response to a call to `checkAccessibilityPermissionGranted`
   * and is not populated until an asynchronous message is received in response.
   */
  public func isAccessibilityPermissionGranted() -> Bool? {
    return self.inputMethodUtil.accessibilityPermissionGranted
  }

  /**
   * return trues if every installation task has been completed
   */
  public func isInstallationComplete() -> Bool {
    guard let state = self.installationState else { return false }
    
    return state.isComplete
  }
  
  /**
   * Returns the next incompleted installation task, if there is one remaining.
   * Note that this function determines the order in which the tasks are executed as they are stored in an unsorted Set.
   */
  public func nextTask() -> InstallationTask? {
    guard let state = self.installationState else { return nil }

    let incompleteTasks = state.tasks.filter { !$0.isComplete }
    
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
   * Executes the specified installation task.
   */
  func executeTask(_ task: InstallationTask) {
    guard let state = self.installationState else { return }

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
      completedTask = self.notifyUserPromptedToRestart()
    }
    
    if completedTask {
      print("executeTask: \(task.taskType.rawValue) completed")
      state.updateTaskAsCompleted(task: task.taskType)
      self.writeInstallationState()
    }
  }
  
  /**
   * Executes the next installation task which is incomplete, if there is one remaining.
   */
  public func executeNextInstallationTask() {
    if let nextTask = self.nextTask() {
      self.executeTask(nextTask)
    }
  }
  
  /**
   * Run the Keyman input method as a subprocess to migrate data to the shared space and immediately exit
   */
  public func migrateData() -> Bool {
    let success = self.inputMethodUtil.invokeKeymanInputMethodMigration()
    print("migration suceeded: \(success)")
    
    return success
  }
  
  /**
   * Save the installation state
   */
  func writeInstallationState() {
    guard let state = self.installationState else { return }

    self.defaultsRepository.writeInstallationState(state.toUserDefaultsDictionary())
  }
  
 /**
   * Write the time that the user was requested to restart their machine
   */
  func writeRestartRequestTime() {
    guard let state = self.installationState else { return }

    state.dateRestartRequested = Date()
    self.writeInstallationState()
  }
  
  /**
   * Read the time that the user was requested to restart their machine
   */
  func readRestartRequestTime() -> Date? {
    guard let state = self.installationState else { return nil }
    
    return state.dateRestartRequested
  }
  
  /**
   * Notify that the user has been prompted to restart the machine.
   */
  public func notifyUserPromptedToRestart() -> Bool {
    self.writeRestartRequestTime()
    return true
  }
  
  /**
   * Check whether the user has restarted by comparing the latest startup time to the time we requested the user to restart
   */
  public func validateUserHasRestarted() -> Bool {
    var hasRestarted = false
    
    guard let state = self.installationState else { return false }

    if let timeRestartRequested = state.dateRestartRequested {
      if let mostRecentStartupTime = self.getMostRecentRestartTime() {
        hasRestarted = mostRecentStartupTime > timeRestartRequested
        print("mostRecentStartupTime: \(mostRecentStartupTime), timeRestartRequested: \(timeRestartRequested)")
      }
    }
    print("validateRestarted: \(hasRestarted)")
    return hasRestarted
  }
  
  /**
   * for testing purposes, replace the InstallationState with a new object set for a new installation
   */
  func forceResetInstallation() {
      self.installationState = InstallationCheck(defaultsRepo: self.defaultsRepository, inputMethodUtil: self.inputMethodUtil).createInstallationStateForNewInstallation()
  }
  
  /**
   * for testing purposes, validate the installation
   */
  func forceValidateInstallation() {
    self.installationCheck.startValidation()
    self.installationState = installationCheck.installationState
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
  
  /**
   * used to report on some current state
   */
  public func debug() {
    var permissionString = "unknown"
    if let permissionGranted = self.isAccessibilityPermissionGranted() {
      permissionString = permissionGranted ? "granted" : "denied"
    }

    let version = (try? inputMethodUtil.getKeymanInputMethodVersion()) ?? "unknown"
    let enabled = inputMethodUtil.isKeymanInputMethodEnabled()
    let running = inputMethodUtil.isKeymanInputMethodRunning()

    print("Keyman status, version: \(version), enabled: \(enabled), running: \(running), permissionGranted: \(permissionString)")
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
   * Verify that the input method has been correctly installed.
   * This may be superflous as we are verifying this before creating the installation tasks
   */
  public func verifyInputMethod()  -> Bool {
    // MAC-CONFIG-TODO: currently does nothing, already verified when InstallationCheck is created
    return true
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
   * and listen for message from Keyman to indicate the result
   */
  public func checkAccessibilityPermissionGranted() {
    if (inputMethodUtil.isKeymanInputMethodRunning()) {
      let killed = self.inputMethodUtil.killKeymanInputMethod()
      print("checkAccessibilityPermission, Keyman input method killed: \(killed)")
    } else {
      print("checkAccessibilityPermission, Keyman input method not running")
    }
 
    self.inputMethodUtil.doAsyncAccessibilityCheck()
  }
  
  /**
   * First kill the Keyman input method if it is running.
   * Second, call Keyman as a separate process with an argument that requests the system to prompt the user to grant accessibility.
   * The Keyman input method cannot send a message to indicate success, because it does not know itself when the user has
   * finished making the change in Settings.
   * To learn the result, we must poll with `isAccessibilityGranted()`
   */
  public func requestAccessibility() -> Bool {
    var requested = false
    
    if (inputMethodUtil.isKeymanInputMethodRunning()) {
      let killed = self.inputMethodUtil.killKeymanInputMethod()
      print("requestAccessibility, Keyman input method killed: \(killed)")
    } else {
      print("requestAccessibility, Keyman input method not running")
    }

    requested = self.inputMethodUtil.invokeKeymanInputMethodRequestAccess()
    print("requestAccessibility called, requested: \(requested)")
    
    return requested
  }

  
  /**
   * run Keyman as a separate process with its full Input Method functionality
   */
  public func runKeymanInputMethod() -> Bool {
    do {
      try self.inputMethodUtil.runKeymanInputMethod()
      return true
    } catch {
      print("runKeymanInputMethod error: \(error)")
      return false
    }
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
}
