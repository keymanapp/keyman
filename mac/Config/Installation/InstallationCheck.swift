/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-06-01
 *
 * Class for evaluating the current state of Keyman to determine is complete or in need of repair
 * and what remaining tasks are needed to complete the installation.
 *
 * If the Keyman input method exists in the Input Methods folder and is is the correct version,
 * and the installation is marked as completed, then it will be checked to see if it is completely
 * valid or in need of repair. If so, 'createRepairInstallationState()' creates a new
 * InstallationState object that replaces the completed one.
 */

import Foundation
import KeymanSettings

public enum InstallationPhase {
  case inputMethodMissing
  case inputMethodOutdated
  case evaluatingInstallation
  case newInstallation
  case installationInProgress
  case installationComplete
  case installationRepairNeeded
}

@MainActor
public class InstallationCheck {
  public var installationState: InstallationState?
  // with isEvaluatingInstallation==true, we are awaiting
  // message from input method to determine what tasks are needed
  public var isEvaluatingInstallation: Bool
  fileprivate let isInputMethodInstalled: Bool
  fileprivate let isInputMethodCurrent: Bool
  fileprivate let inputMethodVersion: String
  fileprivate let configurationVersion: String
  fileprivate let defaultsRepository: DefaultsRepo
  fileprivate let inputMethodUtil: InputMethodUtil

  // a simple representation of the install state
  // provided so UI knows what to present to the user
  public var installationPhase: InstallationPhase {
    if !self.isInputMethodInstalled {
      return .inputMethodMissing
    } else if !self.isInputMethodCurrent {
      return .inputMethodOutdated
    } else if self.isEvaluatingInstallation {
      return .evaluatingInstallation
    }
    
    if let state = self.installationState {
      if state.isComplete {
        return .installationComplete
      } else {
        if state.isNew {
          return .newInstallation
        } else if state.isRepair {
          return .installationRepairNeeded
        } else {
          return .installationInProgress
        }
      }
    } else {
      // in case that installationState (optional) == nil
      // will never reach this case because if it is nil
      // we return inputMethodMissing or inputMethodOutdated
      return .newInstallation
    }
  }
  
  public init(defaultsRepo: DefaultsRepo, inputMethodUtil: InputMethodUtil) {
    self.defaultsRepository = defaultsRepo
    self.inputMethodUtil = inputMethodUtil
    self.isEvaluatingInstallation = false
    
    if inputMethodUtil.keymanInputMethodExists() {
      self.isInputMethodInstalled = true
      self.inputMethodVersion = (try? inputMethodUtil.getKeymanInputMethodVersion()) ?? "unknown"
    } else {
      self.isInputMethodInstalled = false
      self.inputMethodVersion = "unknown"
    }
    
    self.configurationVersion = ConfigAppUtil.configAppVersion()
    self.isInputMethodCurrent = InstallationCheck.isVersionCurrent(inputMethodVersion: self.inputMethodVersion, configurationVersion: self.configurationVersion)
    
    self.installationState = nil
    
    if let installState = self.loadState() {
      // if the installationState remains from a different version, then delete it
      if installState.keymanVersion != self.inputMethodVersion {
        print("removing stale installation state \(installState.keymanVersion) because the current version is \(self.inputMethodVersion)")
        self.clearInstallationState()
      } else if installState.isNew {
        // for a new installation, do not create the installationState until the evaluation is complete
        self.isEvaluatingInstallation = true
      } else {
        // If we're already in progress or completed or doing a repair, pick up where we left off
        // Note that a completed installtion will need to be checked for repairs
        self.installationState = installState
        self.isEvaluatingInstallation = false
      }
    } else {
      // if the installationState does not exist, then this is a new installation
      // do not create the installationState until the evaluation is complete
      self.isEvaluatingInstallation = true
    }
    
    self.registerObservers()
  }

  /**
   * Should be called immediately after init to evaluate what is needed for installation.
   * When the notification from the input method is received and the evaluation is done,
   * the installation can move out of the `evaluatingInstallation` phase
   */
  public func startInstallationEvaluation() {
    // call the input method to check whether Accessibility permission has been granted
    self.inputMethodUtil.doAsyncAccessibilityCheck()
  }
  
  static func isVersionCurrent(inputMethodVersion: String, configurationVersion: String) -> Bool {
    //    return inputMethodVersion == configurationVersion
    // MAC-CONFIG_TODO: temporarily hard-coded to true for testing with local config app builds
    return true
  }
  
  /**
   * register the observer to listen for the response from the input method which
   * checks the current state of Accessibility permissions
   */
  func registerObservers() {
    print("InstallationCheck registerObservers")
    DistributedNotificationCenter.default().addObserver(
      self,
      selector: #selector(self.handleAccessibilityResponse(_:)),
      name: NSNotification.Name.accessibilityQueryResponse,
      object: nil // Observe notifications from any sender
    )
    // MAC-CONFIG_TODO: add timeout?
  }
  
  /**
   * If the installation is incomplete, send notifications so that the UI
   * can present the user with the necessary steps to complete the installation.
   */
  func sendIncompleteNotificationsIfNecessary() {
    if !self.isInputMethodInstalled {
      NotificationCenter.default.post(name: .inputMethodMissing, object: nil)
    } else if !self.isInputMethodCurrent {
      NotificationCenter.default.post(name: .inputMethodOutdated, object: nil)
    } else {
      if let state = self.installationState {
        if state.isNew {
          if !state.isComplete {
            NotificationCenter.default.post(name: .newInstallation, object: nil)
          }
        } else if !state.isComplete {
          NotificationCenter.default.post(name: .activeInstallation, object: nil)
        }
      }
    }
  }
  
  /**
   * called when `NSNotification.Name.accessibilityQueryResponse` is received
   */
  @objc func handleAccessibilityResponse(_ notification: Notification) {
    print("handleAccessibilityResponse")
    // Extract message from the notification if available
    if let message = notification.object as? String {
      let permissionGranted = self.processAccessibilityResponse(with: message)
      self.completeEvaluation(accessibilityPermissionGranted: permissionGranted)
    } else {
      print("accessibilityQueryResponse received but did not include message")
    }
  }
  
  /**
   * Process the distributed notification message that we received from the Keyman input method.
   */
  func processAccessibilityResponse(with message: String) -> Bool {
    let timeStyle = Date.FormatStyle()
      .hour(.twoDigits(amPM: Date.FormatStyle.Symbol.Hour.AMPMStyle.abbreviated))
      .minute(.twoDigits)
      .second(.twoDigits)
      .secondFraction(.fractional(3))
    print("processAccessibilityResponse received message: \(message), time: \(Date().formatted(timeStyle))")
    
    // if the message indicates that access was granted, then return true
    return !message.isEmpty && message == kAccessibilityPermissionGrantedMessage
  }

  /**
   * Save the new InstallationState and notify observers
   */
  func applyNewInstallationState(state: InstallationState) {
    self.defaultsRepository.writeInstallationState(state.toUserDefaultsDictionary())
    self.installationState = state
    NotificationCenter.default.post(name: .installationStateEvaluated, object: state)
  }

  /**
   * Save the new InstallationState for handling repairs and notify observers
   */
  func prepareToRepair(newState: InstallationState) {
    self.defaultsRepository.writeInstallationState(newState.toUserDefaultsDictionary())
    self.installationState = newState
    NotificationCenter.default.post(name: .installationRepairEvaluated, object: newState)
  }

  /**
   * Load the installation state and the tasks required to complete the installation of Keyman.
   * This is accomplished by one of the following:
   * 1. reading the saved installation which is either
   *  completed or
   *  in progress
   * 2. creating a new installation
   * 
   */
  public func loadState() -> InstallationState? {
    var installationState: InstallationState? = nil
    
    guard self.isInputMethodInstalled && self.isInputMethodCurrent else {
      return nil
    }
    
    if let savedInstallationState = readInstallationState() {
      installationState = savedInstallationState
    }
    
    return installationState
  }

  /**
   * Clear the installation state from the UserDefaults
   */
  func clearInstallationState() {
    self.defaultsRepository.deleteInstallationState()
  }
  /**
   * Determine whether validation is needed.
   */
  func isValidationNeeded() -> Bool {
    // no need to validate if the input method is not current
    guard (self.isInputMethodInstalled &&  self.isInputMethodCurrent) else { return false }

    // or if the current state is not saved (should never encounter this)
    guard let currentInstallationState = self.installationState else { return false }
    
    // only need to validate if this installation has been completed
    return currentInstallationState.isComplete
  }

  /**
   * Determine whether the completed installation has been altered in some way and needs repair.
   * If repair is needed, then a new InstallationState object will be returned.
   */
  func startValidation() {
    // call the input method to check whether Accessibility permission has been granted
    self.inputMethodUtil.doAsyncAccessibilityCheck()
  }
  
  /**
   * Using the accessibility state returned from the input method, build the new task list
   * and determine what is actually required to complete installation
   */
  func completeEvaluation(accessibilityPermissionGranted: Bool) {
    // see what tasks remain based on the evaluation
    let neededTasks = determineInstallationTasksNeeded(for: accessibilityPermissionGranted)
    var newState: InstallationState? = nil
    let noIncompleteTasksDetected = neededTasks.isEmpty
    
    // read the current state, if we have one
    if let currentState = self.loadState() {
      if currentState.isComplete {
        if noIncompleteTasksDetected {
          print("installation is complete and no repair needed")
        } else {
          // MAC-CONFIG_TODO: handle repair scenario
          print("installation needs repair")
        }
      } else {
        if currentState.isNew {
          // looks like a new installation, but can be recreated with neededTasks
          newState = self.createNewInstallationState(with: neededTasks)
        }
      }
    } else {
      // no installation state exists, must be a new installation
      newState = self.createNewInstallationState(with: neededTasks)
    }
    
    if let newState = newState {
      print("completeEvaluation: created new installation state")
      self.installationState = newState
      self.defaultsRepository.writeInstallationState(newState.toUserDefaultsDictionary())
      self.applyNewInstallationState(state: newState)
    } else {
      print("completeEvaluation: no new installation state applied")
    }
  }
  
  /**
   * Creates a InstallationState object for an installation that is already in progress
   * Mark existing tasks as complete if
   */
  func createInProgressInstallationState(with neededTasks: Set<InstallationTask>,
                                         and existingTasks: Set<InstallationTask>) -> InstallationState {
    var fullTaskList = neededTasks
    
    // add restartMac InstallationTask
    fullTaskList.insert(InstallationTask.createNewInstallationTask(type: .restartMac))
    let installationState = InstallationState(version: self.inputMethodVersion, tasks: fullTaskList)
    
    return installationState
  }

  /**
   * Creates a InstallationState object describing a new installation
   */
  func createNewInstallationState(with neededTasks: Set<InstallationTask>) -> InstallationState {
    var fullTaskList = neededTasks
    
    // add prepareNewInstall and restartMac InstallationTask
    fullTaskList.insert(InstallationTask.createNewInstallationTask(type: .prepareNewInstall))
    fullTaskList.insert(InstallationTask.createNewInstallationTask(type: .restartMac))
    let installationState = InstallationState(version: self.inputMethodVersion, tasks: fullTaskList)
    
    return installationState
  }

  // old repair-only version
  /**
   * Determine whether the completed installation has been altered in some way and needs repair.
   * If repair is needed, then call `prepareToRepair` with the new `InstallationState`
   */
//  func completeValidation(accessibilityPermissionGranted: Bool) {
//    // check whether the installation requires repair
//    if let newInstallationState = self.createRepairInstallationState(accessibilityPermissionGranted: accessibilityPermissionGranted) {
//      self.prepareToRepair(newState: newInstallationState)
//    } else {
//      print("completeEvaluation: no repair needed")
//    }
//  }

  /**
   * Read the currently saved installation state as an object
   */
  func readInstallationState() -> InstallationState? {
    guard let installationMap = self.defaultsRepository.readInstallationState() else {
      return nil
    }
    
    return InstallationState(from: installationMap)
  }
  
  
  /**
   * Creates a InstallationState object describing a new installation
   */
  func createInstallationStateForNewInstallation() -> InstallationState {
    let installationState = InstallationState(version: self.inputMethodVersion, tasks: self.createNewInstallationTasks())
    self.defaultsRepository.writeInstallationState(installationState.toUserDefaultsDictionary())
    
    return installationState
  }
  
  /**
   * Creates a the set of tasks required for a new installation
   */
  func createNewInstallationTasks() -> Set<InstallationTask> {
    var taskList = Set<InstallationTask>()
    taskList.insert(InstallationTask(task: .prepareNewInstall, completed: false))
    taskList.insert(InstallationTask(task: .enableInputMethod, completed: false))
    taskList.insert(InstallationTask(task: .requestAccess, completed: false))
    taskList.insert(InstallationTask(task: .restartMac, completed: false))
    return taskList
  }
  
  
  /**
   * The provided parameter `accessibilityPermissionGranted` was already returned asynchronously from the input method.
   * Use it and other info to see what tasks are needed to complete installation.
   */
  func determineInstallationTasksNeeded(for accessibilityPermissionGranted: Bool) -> Set<InstallationTask> {
    var newTasks = Set<InstallationTask>()
    
    // add task to request Accessibility permission if needed
    if !accessibilityPermissionGranted {
      newTasks.insert(InstallationTask.createNewInstallationTask(type: .requestAccess))
    }
    
    // add enable input method and restart mac tasks if needed
    if !self.inputMethodUtil.isKeymanInputMethodEnabled() {
      newTasks.insert(InstallationTask.createNewInstallationTask(type: .enableInputMethod))
      
      // prompt user to restart after enabling the input method
      newTasks.insert(InstallationTask.createNewInstallationTask(type: .restartMac))
    }
//    
//    if !newTasks.isEmpty {
//      newInstallationState = InstallationState(version: self.inputMethodVersion, isRepair: true, tasks: newTasks)
//    }
    
    return newTasks
  }

  /**
   * Check the installation to see of it is valid -- something may have been tampered with after installation was completed.
   * The provided parameter `accessibilityPermissionGranted` was already returned asynchronously from the input method.
   * If the installation needs repair, create the info needed for repairing the installation.
   */
  func createRepairInstallationState(accessibilityPermissionGranted: Bool) -> InstallationState? {
    var repairInstallationState: InstallationState? = nil
    var repairTasks = Set<InstallationTask>()
    
    if !accessibilityPermissionGranted {
      repairTasks.insert(InstallationTask.createNewInstallationTask(type: .requestAccess))
    }
    
    if !self.inputMethodUtil.isKeymanInputMethodEnabled() {
      repairTasks.insert(InstallationTask.createNewInstallationTask(type: .enableInputMethod))
      
      // also need to restart after enabling the input method
      repairTasks.insert(InstallationTask.createNewInstallationTask(type: .restartMac))
    }
    
    
    if !repairTasks.isEmpty {
      repairInstallationState = InstallationState(version: self.inputMethodVersion, isRepair: true, tasks: repairTasks)
    }
    
    return repairInstallationState
  }
}
