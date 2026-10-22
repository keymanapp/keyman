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

// The data model version number is updated by the input method.
// We only read the value here, and if it is current, then there is no need
// to trigger data migration. If data migration is necessary, then it must be
// performed by the input method because the input method alone has access
// to the data before it was shared in an app group beginning with Keyman 19.
let dataModelVersionStoreInGroupContainer = 2

public enum InstallationPhase: String {
  case inputMethodMissing
  case inputMethodOutdated
  case evaluatingInstallation
  case installationInProgress
  case installationComplete
  
  // indicates whether the installation contains tasks to complete installation
  public var hasTasks: Bool {
    switch self {
    case .installationInProgress:
      return true
    default:
      return false
    }
  }
  
  public var isActiveInstallation: Bool {
    switch self {
    case .evaluatingInstallation,
        .installationInProgress:
      return true
    default:
      return false
    }
  }
}

enum InstallationStateCondition: String {
  case undefined
  case inProgress
  case complete
}

@MainActor
public class InstallationCheck {
  public let isLaunchedByInstaller: Bool      // the app was launched by the installer
  public var installationState: InstallationState?
  // with isEvaluatingNewInstallation==true, we are awaiting
  // message from input method to determine what tasks are needed
  public var isEvaluatingNewInstallation: Bool
  private let isInputMethodInstalled: Bool
  private let isInputMethodCurrent: Bool
  private let inputMethodVersion: String
  private let configurationVersion: String
  private let defaultsRepository: DefaultsRepo
  private let inputMethodUtil: InputMethodUtil
  
  // used to enforce timeout on accessibility response from input method
  private var accessibilityTimeout: DispatchWorkItem?
  private var awaitingAccessibilityResponse = false
  private let accessibilityTimeoutSeconds: TimeInterval = 3.0

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
      }
    } else {
      // If we have no installation state, then consider it complete.
      // This will trigger a repair check.
      // We also have no state in the case of a brand new install,
      // but that would return as .isEvaluatingNewInstallation above.
      return .installationComplete
    }
    
    return .installationInProgress
  }
  
  public init(defaultsRepo: DefaultsRepo, inputMethodUtil: InputMethodUtil, isLaunchedByInstaller: Bool) {
    self.defaultsRepository = defaultsRepo
    self.inputMethodUtil = inputMethodUtil
    self.isLaunchedByInstaller = isLaunchedByInstaller
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
      if isLaunchedByInstaller {
        // this is a new installation: clear the InstallationState and evaluate before creating a new one
        clearInstallationState()
        self.isEvaluatingNewInstallation = true
      } else {
        // the input method is valid, examine the installation state recorded on disk
        let installationStateCondition = InstallationCheck.evaluateInstallationState(state: installState, for: keymanVersion);
        Logger.app.log("installationStateCondition: \(installationStateCondition.rawValue, privacy: .public)")

        switch installationStateCondition {
        case .inProgress:
          Logger.app.debug("InstallationCheck: continuing with existing installation")
          self.installationState = installState     // resume with the existing installation
        case .complete:
          self.installationState = installState     // use the existing installation for now
          Logger.app.debug("InstallationCheck: installation is complete, ready to check for repairs")
        case .undefined:
          Logger.app.debug("InstallationCheck: no state on disk or state is invalid")
        }
      }
    }
    
    self.registerObservers()
  }

  /**
   * Check the user defaults to see whether a migration is needed
   */
  public func isMigrationNeeded() -> Bool {
    var migrationNeeded = true
    
    if self.defaultsRepository.readDataModelVersion() >= dataModelVersionStoreInGroupContainer {
      migrationNeeded = false
    }

    Logger.app.debug("isMigrationNeeded: \(migrationNeeded)")
    return migrationNeeded
  }
  
  /**
   * Run the Keyman input method as a subprocess to migrate data to the shared space and immediately exit
   */
  public func migrateData() -> Bool {
    let success = self.inputMethodUtil.invokeKeymanInputMethodMigration()
    Logger.app.debug("migrateData migration suceeded: \(success)")

    if success {
      NotificationCenter.default.post(name: .dataMigrated, object: nil)
    }
    return success
  }
  
  /**
   * Check the condition of the InstallationState as recorded in the UserDefaults.
   * Determine whether the state is `undefined`, `inProgress` or `complete`
   *
   * If no state is persisted, a new installation may be about to begin.
   * In this case, as far as the UserDefaults are concerned, the installation is determined
   * to be `undefined`. The current state is not needed and will be created though a new
   * installation may have just been triggered by a command line argument passed to the
   * config app from the installer.
   */
  static func evaluateInstallationState(state: InstallationState?, for version: String) -> InstallationStateCondition {
    guard let installationState = state else { return .undefined }
    
    var condition = InstallationStateCondition.undefined
    
    // If the installationState remains from a different install, mark it as undefined.
    // This should not occur, unless someone has manually changed the state.
    if installationState.keymanVersion != version {
      condition = .undefined
    } else if installationState.isComplete {
      condition = .complete
      // A completed installation will be checked for repairs
    } else if installationState.isInProgress {
      // If we're already in progress or completed or doing a repair, pick up where we left off
      condition = .inProgress
    }

    return condition
  }
  
  /**
   * For new installations, called from init to evaluate what is needed to complete install.
   * When the notification from the input method is received and the evaluation is done,
   * the installation can move out of the `evaluatingInstallation` phase
   */
  public func startNewInstall() {
    Logger.app.debug("starting new installation...")
    
    guard self.isInputMethodInstalled else {
      Logger.app.debug("startNewInstall invoked when input method is not installed")
      return
    }
    guard self.isInputMethodCurrent else {
      Logger.app.debug("startNewInstall invoked when input method is not current")
      return
    }

    // call the input method to check whether Accessibility permission has been granted
    if (self.isLaunchedByInstaller) {
      self.inputMethodUtil.doAsyncAccessibilityCheckWithMigration()
      self.startAccessibilityResponseTimer()
    }
  }

  /**
   * For completed installations, called from init to evaluate whether repairs are needed.
   */
  public func startRepairCheck() {
    guard self.isReadyForRepairCheckAtStartup() else {
      Logger.app.debug("startRepairCheck invoked when not ready for repair")
      return
    }
    
    self.doAsyncAccessibilityCheck()
  }

  /**
   * Call the input method to check whether Accessibility permission has been granted and
   * start a timer in case there is no response and we need to timeout.
   */
  public func doAsyncAccessibilityCheck() {
    // call the input method to check whether Accessibility permission has been granted
    self.inputMethodUtil.doAsyncAccessibilityCheck(forceInputMethodRestart: false)
    self.startAccessibilityResponseTimer()
  }

  /**
   * Call immediately after call to `inputMethodUtil.doAsyncAccessibilityCheck`
   * Used to enforce a timeout on the response from the input method.
   */
  func startAccessibilityResponseTimer() {
    accessibilityTimeout?.cancel()
    awaitingAccessibilityResponse = true
    
    let work = DispatchWorkItem { [weak self] in
      guard let self, self.awaitingAccessibilityResponse else { return }
      self.awaitingAccessibilityResponse = false
      Logger.app.error("accessibility response timed out after \(self.accessibilityTimeoutSeconds)s, assume not granted")
      self.applyAccessibilityResult(permissionGranted: false)
    }
    accessibilityTimeout = work
    DispatchQueue.main.asyncAfter(deadline: .now() + accessibilityTimeoutSeconds, execute: work)
  }
    
  /**
   * called when `NSNotification.Name.accessibilityStateResponse` is received
   */
  @objc func handleAccessibilityResponse(_ notification: Notification) {
    guard let message = notification.object as? String else {
      Logger.app.debug("handleAccessibilityResponse, received but did not include message")
      return
    }
    let granted = self.processAccessibilityResponse(with: message)
    
    DispatchQueue.main.async {
      guard self.awaitingAccessibilityResponse else {
        Logger.app.debug("late accessibility response ignored: \(message, privacy: .public)")
        return
      }
      self.awaitingAccessibilityResponse = false
      self.accessibilityTimeout?.cancel()
      self.accessibilityTimeout = nil
      self.applyAccessibilityResult(permissionGranted: granted)
    }
  }

  /**
   * Apply the accessibility result from either the input method response or the expired timer.
   * There are three scenarios where we may be using this result:
   *  1. to evaluate the installation steps needed for a new installation
   *  2. to see if a completed installation is in need of repair
   *  3. while guiding the user through installation, to check whether they have successfully added the Accessibility permission
   */
  func applyAccessibilityResult(permissionGranted: Bool) {
    // should never happen, but if there is no installationState then interpret it as installation complete
    let installCompleted = self.installationState?.isComplete ?? true

    let timeStyle = Date.FormatStyle()
      .hour(.twoDigits(amPM: Date.FormatStyle.Symbol.Hour.AMPMStyle.abbreviated))
      .minute(.twoDigits)
      .second(.twoDigits)
      .secondFraction(.fractional(3))
    Logger.app.debug("applyAccessibilityResult: \(permissionGranted), time: \(Date().formatted(timeStyle), privacy: .public)")

    if self.isEvaluatingNewInstallation {
      self.completeNewInstallationEvaluation(accessibilityPermissionGranted: permissionGranted)
      if self.isLaunchedByInstaller {
        NotificationCenter.default.post(name: .dataMigrated, object: nil)
      }
    } else if installCompleted {
      self.checkForRepair(accessibilityPermissionGranted: permissionGranted)
    } else {
      NotificationCenter.default.post(
        name: permissionGranted ? .accessibilityGranted : .accessibilityNotGranted, object: nil)
    }
  }
  
  /**
   * Returns true if we should check at app startup whether the installation needs repair.
   * Simply returns true if the installation is complete.
   */
  func isReadyForRepairCheckAtStartup() -> Bool {
    guard self.isInputMethodInstalled else {
      Logger.app.debug("isReadyForRepairCheckAtStartup false, input method not installed")
      return false
    }
    guard self.isInputMethodCurrent else {
      Logger.app.debug("isReadyForRepairCheckAtStartup false, input method is not current")
      return false
    }
    
    // should never happen, but if there is no installationState then interpret it as installation complete
    let readyForRepairCheck = self.installationState?.isComplete ?? true
    
    Logger.app.debug("isReadyForRepairCheckAtStartup: \(readyForRepairCheck)")

    return readyForRepairCheck
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
  }
    
  /**
   * Process the distributed notification message that we received from the Keyman input method.
   */
  func processAccessibilityResponse(with message: String) -> Bool {
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
    let installationState = InstallationState(version: self.inputMethodVersion, tasks: neededTasks)
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
    Logger.app.debug("readInstallationState returns nil: no installation state exists")
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
      newTasks.insert(InstallationTask.createNewInstallationTask(type: .confirmEnabled))
      
    // if enable is needed, prompt to restart to ensure that the input method has been loaded by the system
      newTasks.insert(InstallationTask.createNewInstallationTask(type: .requestRestart))
      newTasks.insert(InstallationTask.createNewInstallationTask(type: .confirmRestart))
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
