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

// in-app notifications sent
public extension Notification.Name {
  static let startNewInstallation = Notification.Name("start.new.installation")
  static let startInstallationRepair = Notification.Name("start.installation.repair")
  static let installationRepairStarted = Notification.Name("installation.repair.started")
  static let accessibilityGranted = Notification.Name("installation.accessibility.granted")
  static let accessibilityNotGranted = Notification.Name("installation.accessibility.not.granted")
  static let checkAccessibilitySuccess = Notification.Name("accessibility.success")
  static let checkAccessibilityFailure = Notification.Name("accessibility.failure")
}

@MainActor // run on the main actor since data is published directly to the UI
public class InstallationContainer : ObservableObject {
  public var installationPhase: InstallationPhase {
    return self.installationCheck.installationPhase
  }
  
  var installationState: InstallationState? {
    self.installationCheck.installationState
  }

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

    do {
      try inputMethodUtil = InputMethodUtil()
    } catch {
      fatalError("Unable to access group container path for InputMethodUtil: \(error.localizedDescription).")
    }

    self.installationCheck = InstallationCheck(defaultsRepo: defaultsRepo, inputMethodUtil: inputMethodUtil)
    
    // If we can now confirm that the user restarted (the final task), then the installation
    // will be complete and there is no need to evaluate the state.
    // Otherwise, evaluate the installation to prepare for a new installation or check for repairs.
    if !self.validateConfirmRestart() {
      self.registerObservers()
      self.installationCheck.startInstallationEvaluation()
    }
  }
  
  /**
   * register observers to learn of results of InstallationState evaluation
   */
  func registerObservers() {
    print("InstallationContainer registerObservers")
    NotificationCenter.default.addObserver(
      self,
      selector: #selector(self.handleStartNewInstallation(_:)),
      name: NSNotification.Name.startNewInstallation,
      object: nil // Observe notifications from any sender
    )
    NotificationCenter.default.addObserver(
      self,
      selector: #selector(self.handleStartInstallationRepair(_:)),
      name: NSNotification.Name.startInstallationRepair,
      object: nil // Observe notifications from any sender
    )
    NotificationCenter.default.addObserver(
      self,
      selector: #selector(self.handleAccessibilityGranted(_:)),
      name: NSNotification.Name.accessibilityGranted,
      object: nil // Observe notifications from any sender
    )
    NotificationCenter.default.addObserver(
      self,
      selector: #selector(self.handleAccessibilityNotGranted(_:)),
      name: NSNotification.Name.accessibilityNotGranted,
      object: nil // Observe notifications from any sender
    )
  }

  /**
   * called when `NSNotification.Name.startNewInstallation` is received
   */
  @objc func handleStartNewInstallation(_ notification: Notification) {
    print("handleStartNewInstallation received")
    // the evaluation is done
    self.installationCheck.isEvaluatingNewInstallation = false
  }

  /**
   * called when `NSNotification.Name.startInstallationRepair` is received
   */
  @objc func handleStartInstallationRepair(_ notification: Notification) {
    print("handleStartInstallationRepair received")
    
    // notify observers
    NotificationCenter.default.post(name: .installationRepairStarted, object: nil, userInfo: nil)
  }

  /**
   * called when `NSNotification.Name.accessibilityGranted` is received
   */
  @objc func handleAccessibilityGranted(_ notification: Notification) {
    guard self.installationState != nil else { return }
    
    // the confirmAccess task can now be marked as completed
    self.updateTaskAsCompleted(taskType: .confirmAccess)
    
    NotificationCenter.default.post(name: .checkAccessibilitySuccess, object: nil, userInfo: nil)
  }
  
  /**
   * called when `NSNotification.Name.accessibilityNotGranted` is received
   */
  @objc func handleAccessibilityNotGranted(_ notification: Notification) {
    NotificationCenter.default.post(name: .checkAccessibilityFailure, object: nil, userInfo: nil)
  }
  
  /**
   * If the current task is confirmRestart, mark it as complete if the user has restarted.
   */
  func validateConfirmRestart() -> Bool {
    guard let task = self.currentTask() else { return false }
    guard self.installationState != nil else { return false }
    
    if task.taskType == .confirmRestart &&  self.validateUserHasRestarted() {
      // the confirmAccess task can now be marked as completed
      self.updateTaskAsCompleted(taskType: .confirmRestart)
      return true
    } else {
      return false
    }
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
   * Returns the current incompleted installation task, if there is one.
   * Note that this function determines the order in which the tasks are executed as they are stored in an unsorted Set.
   */
  public func currentTask() -> InstallationTask? {
    guard let state = self.installationState else { return nil }
    guard self.installationPhase.hasTasks else {
      print("the installation phase \(self.installationPhase) has no tasks");
      return nil
    }

    let incompleteTasks = state.tasks.filter { !$0.isComplete }
    
    if let incompleteTask = incompleteTasks.first(where: { $0.taskType == .prepareNewInstall }) {
      return incompleteTask
    } else if let incompleteTask = incompleteTasks.first(where: { $0.taskType == .prepareNewRepair }) {
      return incompleteTask
    } else if let incompleteTask = incompleteTasks.first(where: { $0.taskType == .enableInputMethod }) {
      return incompleteTask
    } else if let incompleteTask = incompleteTasks.first(where: { $0.taskType == .requestAccess }) {
      return incompleteTask
    } else if let incompleteTask = incompleteTasks.first(where: { $0.taskType == .confirmAccess }) {
      return incompleteTask
    } else if let incompleteTask = incompleteTasks.first(where: { $0.taskType == .requestRestart }) {
      return incompleteTask
    } else if let incompleteTask = incompleteTasks.first(where: { $0.taskType == .confirmRestart }) {
      return incompleteTask
    }
    
    return nil
  }
 
  /**
   * Executes the specified installation task.
   */
  func executeTask(_ task: InstallationTask) {
    guard self.installationState != nil else { return }
    guard self.installationPhase.hasTasks else {
      print("the installation phase \(self.installationPhase) has no tasks");
      return
    }

    var completedTask = false
    
    switch task.taskType {
    case .prepareNewInstall:
      completedTask = self.migrateData()
    case .prepareNewRepair:
      completedTask = true
    case .enableInputMethod:
      completedTask = self.enableKeymanInputMethod()
    case .requestAccess:
      completedTask = self.requestAccessibility()
    case .confirmAccess:
      self.checkAccessibilityPermissionGranted()
      // this task is completed asynchronously when the response is returned from the input method
      completedTask = false
    case .requestRestart:
      completedTask = self.notifyUserPromptedToRestart()
    case .confirmRestart:
      completedTask = self.validateUserHasRestarted()
    }
    
    if completedTask {
      self.updateTaskAsCompleted(taskType: task.taskType)
    }
  }

  /**
   * Marks the specified task as completed and saves it to the UserDefaults.
   * Note that this actually creates a copy of the InstallationState object and updates
   * the property in InstallationCheck with the new reference.
   */
  public func updateTaskAsCompleted(taskType: InstallationTaskType) {
    print("executeTask: \(taskType.rawValue) completed")
    if let existingState = self.installationState {
      let updatedState = InstallationState.createCopyWithCompletedTask(from: existingState, with: taskType)
      self.installationCheck.installationState = updatedState
      self.writeInstallationState()
    }
  }

  /**
   * Executes the next installation task which is incomplete, if there is one remaining.
   */
  public func executeNextInstallationTask() {
    if let installTask = self.currentTask() {
      self.executeTask(installTask)
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
    * Record that the installation complete view has been shown to the user
    */
  public func setHasDisplayedInstallationComplete() {
    if let existingState = self.installationState {
      let updatedState = InstallationState.createCopy(from: existingState)
      updatedState.hasDisplayedInstallComplete = true
      self.installationCheck.installationState = updatedState
      self.writeInstallationState()
    }
  }
  
  /**
    * Return whether the installation complete view has been shown to the user
    */
   func getHasDisplayedInstallationComplete() -> Bool {
     guard let state = self.installationState else { return false }

     return state.hasDisplayedInstallComplete
   }

  /**
    * Write the time that the user was requested to restart their machine
    */
   func writeRestartRequestTime() {
     if let existingState = self.installationState {
       let updatedState = InstallationState.createCopy(from: existingState)
       updatedState.dateRestartRequested = Date()
       self.installationCheck.installationState = updatedState
       self.writeInstallationState()
     }
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
   * call Keyman as a separate process with an argument that checks whether accessibility has been granted by the user
   */
  public func checkAccessibilityPermissionGranted() {
    self.inputMethodUtil.doAsyncAccessibilityCheck()
  }
  
  /**
   * Call Keyman as a separate process with an argument that requests the system to prompt the user to grant accessibility.
   * To learn the result, we must poll with `checkAccessibilityPermissionGranted()`
   */
  public func requestAccessibility() -> Bool {
    var requested = false
  
    requested = self.inputMethodUtil.invokeKeymanInputMethodRequestAccess()
    print("requestAccessibility called, requested: \(requested)")
    
    return requested
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
