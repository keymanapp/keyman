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
import OSLog

public enum InstallationPhase: String {
  case inputMethodMissing
  case inputMethodOutdated
  case evaluatingInstallation
  case newInstallation
  case installationInProgress
  case installationComplete
  case installationRepairNeeded
  
  // indicates whether the installation contains tasks to complete installation
  public var hasTasks: Bool {
    switch self {
    case .newInstallation,
        .installationInProgress,
        .installationRepairNeeded:
      return true
    default:
      return false
    }
  }
}

enum InstallationStateCondition: String {
  case stale
  case new
  case inProgress
}

@MainActor
public class InstallationCheck {
  public var installationState: InstallationState?
  // with isEvaluatingNewInstallation==true, we are awaiting
  // message from input method to determine what tasks are needed
  public var isEvaluatingNewInstallation: Bool
  fileprivate let isInputMethodInstalled: Bool
  fileprivate let isInputMethodCurrent: Bool
  fileprivate let inputMethodVersion: String
  fileprivate let configurationVersion: String
  fileprivate let defaultsRepository: DefaultsRepo
  fileprivate let inputMethodUtil: InputMethodUtil

  // a classification of the install state
  // provided so UI knows what to present to the user
  public var installationPhase: InstallationPhase {
    if !self.isInputMethodInstalled {
      return .inputMethodMissing
    } else if !self.isInputMethodCurrent {
      return .inputMethodOutdated
    } else if self.isEvaluatingNewInstallation {
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
        }
        
        return .installationInProgress
      }
    }
    
    // In case installationState (optional) == nil --
    // though we will never reach this case because if it is nil
    // we return inputMethodMissing or inputMethodOutdated
    return .newInstallation
  }
  
  public init(defaultsRepo: DefaultsRepo, inputMethodUtil: InputMethodUtil) {
    self.defaultsRepository = defaultsRepo
    self.inputMethodUtil = inputMethodUtil
    self.isEvaluatingNewInstallation = false
    self.configurationVersion = ConfigAppUtil.configAppVersion()
    
    var keymanIsCurrent = false
    var keymanVersion: String = "unknown"

    let keymanExists = inputMethodUtil.keymanInputMethodExists()
    if keymanExists {
      keymanVersion = (try? inputMethodUtil.getKeymanInputMethodVersion()) ?? "unknown"
      keymanIsCurrent = InstallationCheck.isVersionCurrent(inputMethodVersion: keymanVersion, configurationVersion: self.configurationVersion)
    }
    
    self.isInputMethodInstalled = keymanExists
    self.isInputMethodCurrent = keymanIsCurrent
    self.inputMethodVersion = keymanVersion

    let installState = InstallationCheck.readInstallationState(from: defaultsRepo)

    if (keymanExists && keymanIsCurrent) {
      // the input method is valid, examine the installation state recorded on disk
      //
      let installationStateCondition = InstallationCheck.evaluateInstallationState(state: installState, for: keymanVersion);
      Logger.app.log("installationStateCondition: \(installationStateCondition.rawValue, privacy: .public)")

      switch installationStateCondition {
      case .inProgress:
        self.installationState = installState     // resume with the existing installation
      case .new:
        self.isEvaluatingNewInstallation = true   // evaluate before creating a new InstallationState
      case .stale:
        self.clearInstallationState()             // delete the existing installation state from the UserDefaults
        self.isEvaluatingNewInstallation = true   // evaluate before creating a new InstallationState
      }
    }
    
    self.registerObservers()
  }

  /**
   * Check the condition of the InstallationState as recorded in the UserDefaults.
   * Determine whether it is `stale` and should be deleted
   * (which then is treated as a new installation) or
   * is `inProgress` and should be loaded and used, or
   * is `new`. All new installations are re-evaluated to determine
   * what tasks must be executed to complete the installation.
   */
  static func evaluateInstallationState(state: InstallationState?, for version: String) -> InstallationStateCondition {
 
    // If the installationState does not exist, then this is a new installation.
    // The installationState will be created when evaluation is complete.
    guard state != nil else { return .new }
    
    var condition = InstallationStateCondition.inProgress
    
    if let installState = state {
      // If the installationState remains from a different install, mark it as stale.
      // It will be deleted and we will evaluate for a new installation.
      if installState.keymanVersion != version {
        condition = .stale
      } else if installState.isNew {
        condition = .new
      } else {
        // If we're already in progress or completed or doing a repair, pick up where we left off
        // Note that a completed installation will be checked for repairs
        condition = .inProgress
      }
    }

    return condition
  }
  
  /**
   * Should be called immediately after init to evaluate what is needed for installation
   * or, if the installation is complete, whether it needs repairs.
   * When the notification from the input method is received and the evaluation is done,
   * the installation can move out of the `evaluatingInstallation` phase
   */
  public func startInstallationEvaluation() {
    // call the input method to check whether Accessibility permission has been granted
    if (self.isInputMethodInstalled && self.isInputMethodCurrent) &&
        (self.isEvaluatingNewInstallation || self.installationState?.isComplete == true) {
      self.inputMethodUtil.doAsyncAccessibilityCheck()
    }
  }
  
  /**
   * Check whether the input method and configuration app are the same version.
   * Because the version of the config app will not be sent when build locally, this can be overridden,
   * for testing purposes, by specifying `kTestConfigVersion` in config app's standard UserDefaults
   */
  static func isVersionCurrent(inputMethodVersion: String, configurationVersion: String) -> Bool {
    Logger.app.log("isVersionCurrent, comparing input method version: \(inputMethodVersion, privacy: .public) and config app version: \(configurationVersion, privacy: .public)")
    return inputMethodVersion == configurationVersion
  }
  
  /**
   * register the observer to listen for the response from the input method which
   * checks the current state of Accessibility permissions
   */
  func registerObservers() {
    Logger.app.debug("InstallationCheck registerObservers")

    DistributedNotificationCenter.default().addObserver(
      self,
      selector: #selector(self.handleAccessibilityResponse(_:)),
      name: NSNotification.Name.accessibilityStateResponse,
      object: nil // Observe notifications from any sender
    )
    // MAC-CONFIG-TODO: add timeout?
  }
    
  /**
   * called when `NSNotification.Name.accessibilityStateResponse` is received
   */
  @objc func handleAccessibilityResponse(_ notification: Notification) {
    var installCompleted = false
    
    // Extract message from the notification if available
    if let message = notification.object as? String {
      let permissionGranted = self.processAccessibilityResponse(with: message)
      
      Logger.app.debug("handleAccessibilityResponse, message: \(message, privacy: .public)")

      if let state = self.installationState {
        installCompleted = state.isComplete
      }
      
      if self.isEvaluatingNewInstallation {
        // if evaluating the current state for a new installation,
        // complete the evaluation using the results of the permission check
       self.completeNewInstallationEvaluation(accessibilityPermissionGranted: permissionGranted)
      } else if installCompleted {
        // if this is a completed install, check whether repairs are needed
        self.checkForRepair(accessibilityPermissionGranted: permissionGranted)
      } else {
        // otherwise, this is for an install step, post results
        if permissionGranted {
          NotificationCenter.default.post(name: .accessibilityGranted, object: nil)
        } else {
          NotificationCenter.default.post(name: .accessibilityNotGranted, object: nil)
        }
      }
    } else {
      Logger.app.debug("handleAccessibilityResponse, received but did not include message")
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
    Logger.app.debug("processAccessibilityResponse received message: \(message, privacy: .public), time: \(Date().formatted(timeStyle), privacy: .public)")

    // if the message indicates that access was granted, then return true
    return !message.isEmpty && message == kAccessibilityPermissionGrantedMessage
  }

  /**
   * Save the new InstallationState and notify observers to start new installation
   */
  func applyNewInstallationState(state: InstallationState) {
    self.defaultsRepository.writeInstallationState(state.toUserDefaultsDictionary())
    self.installationState = state
    NotificationCenter.default.post(name: .startNewInstallation, object: state)
  }

  /**
   * Save the new InstallationState for handling repairs and notify observers
   */
  func applyRepairedInstallationState(state: InstallationState) {
    self.defaultsRepository.writeInstallationState(state.toUserDefaultsDictionary())
    self.installationState = state
    NotificationCenter.default.post(name: .startInstallationRepair, object: state)
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
   * Using the accessibility state returned from the input method, build the new task list
   * and determine what is actually required for the new installation.
   */
  func completeNewInstallationEvaluation(accessibilityPermissionGranted: Bool) {
    // see what tasks remain based on the evaluation
    let neededTasks = determineInstallationTasksNeeded(isRepair: false, with: accessibilityPermissionGranted)
    let newState = self.createNewInstallationState(with: neededTasks)
    self.applyNewInstallationState(state: newState)
  }
  
  /**
   * Creates a InstallationState object describing a new installation
   */
  func createNewInstallationState(with neededTasks: Set<InstallationTask>) -> InstallationState {
    Logger.app.debug("completeNewInstallationEvaluation: created new installation state")

                     var fullTaskList = neededTasks
    
    // add prepareNewInstall InstallationTask
    fullTaskList.insert(InstallationTask.createNewInstallationTask(type: .prepareNewInstall))
    
    let installationState = InstallationState(version: self.inputMethodVersion, tasks: fullTaskList)
    
    return installationState
  }

  /**
   * Determine whether the completed installation has been altered in some way and needs repair.
   * If repair is needed, then call `applyRepairedInstallationState` with the new `InstallationState`
   */
  func checkForRepair(accessibilityPermissionGranted: Bool) {
    // check whether the installation requires repair
    if let state = self.createRepairInstallationState(accessibilityPermissionGranted: accessibilityPermissionGranted) {
      Logger.app.log("checkForRepair completed: repair is required")
      self.applyRepairedInstallationState(state: state)
    } else {
      Logger.app.log("checkForRepair completed: no repair needed")
    }
  }

/**
 * Read the currently saved installation state as an object
 */
func readInstallationState() -> InstallationState? {
  return InstallationCheck.readInstallationState(from: self.defaultsRepository)
}

/**
 * Read the currently saved installation state as an object
 */
static func readInstallationState(from repo: DefaultsRepo) -> InstallationState? {
  guard let installationMap = repo.readInstallationState() else {
    return nil
  }
  
  return InstallationState(from: installationMap)
}

  /**
   * The provided parameter `accessibilityPermissionGranted` was returned asynchronously from the input method.
   * Use it and other info to see what tasks are needed to complete installation.
   */
  func determineInstallationTasksNeeded(isRepair: Bool, with accessibilityPermissionGranted: Bool) -> Set<InstallationTask> {
    var newTasks = Set<InstallationTask>()
    
    // add task to request Accessibility permission if needed
    if !accessibilityPermissionGranted {
      newTasks.insert(InstallationTask.createNewInstallationTask(type: .requestAccess))
      newTasks.insert(InstallationTask.createNewInstallationTask(type: .confirmAccess))
    }
    
    // add enable input method and restart mac tasks if needed
    if !self.inputMethodUtil.isKeymanInputMethodEnabled() {
      newTasks.insert(InstallationTask.createNewInstallationTask(type: .enableInputMethod))
      
    // when repairing, prompt to restart to ensure that the input method has been loaded by the system
      if (isRepair) {
        newTasks.insert(InstallationTask.createNewInstallationTask(type: .requestRestart))
        newTasks.insert(InstallationTask.createNewInstallationTask(type: .confirmRestart))
      }
    }
    
    return newTasks
  }

  /**
   * The provided parameter `accessibilityPermissionGranted` was returned asynchronously from the input method.
   * Check the installation to see of it is valid -- something may have been tampered with after installation was completed.
   * If the installation needs repair, create the info needed for repairing the installation.
   */
  func createRepairInstallationState(accessibilityPermissionGranted: Bool) -> InstallationState? {
    var repairInstallationState: InstallationState? = nil
    
    var repairTasks = self.determineInstallationTasksNeeded(isRepair: true, with: accessibilityPermissionGranted)
    if !repairTasks.isEmpty {
      // add prepareNewRepair
      repairTasks.insert(InstallationTask.createNewInstallationTask(type: .prepareNewRepair))
      repairInstallationState = InstallationState(version: self.inputMethodVersion, isRepair: true, tasks: repairTasks)
    }
    
    return repairInstallationState
  }
}
