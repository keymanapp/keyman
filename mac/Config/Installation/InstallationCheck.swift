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


public class InstallationCheck {
  public let isInputMethodInstalled: Bool
  public let isInputMethodCurrent: Bool
  public var installationState: InstallationState?
  fileprivate let inputMethodVersion: String
  fileprivate let configurationVersion: String
  fileprivate let defaultsRepository: DefaultsRepo
  fileprivate let inputMethodUtil: InputMethodUtil

  public init(defaultsRepo: DefaultsRepo, inputMethodUtil: InputMethodUtil) {
    self.defaultsRepository = defaultsRepo
    self.inputMethodUtil = inputMethodUtil
    
    if inputMethodUtil.keymanInputMethodExists() {
      self.isInputMethodInstalled = true
      self.inputMethodVersion = (try? inputMethodUtil.getKeymanInputMethodVersion()) ?? "unknown"
    } else {
      self.isInputMethodInstalled = false
      self.inputMethodVersion = "unknown"
    }
    
    self.configurationVersion = Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String ?? "unknown"
    self.isInputMethodCurrent = InstallationCheck.isVersionCurrent(inputMethodVersion: self.inputMethodVersion, configurationVersion: self.configurationVersion)
    
    self.installationState = self.loadState()
    
    self.registerObservers()
    
    if self.isValidationNeeded() {
      self.startValidation()
    }
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
      selector: #selector(self.handlePermissionNotification(_:)),
      name: NSNotification.Name.accessCheckResponse,
      object: nil // Observe notifications from any sender
    )
    // MAC-CONFIG_TODO: add timeout?
  }
  
  /**
   * called when `NSNotification.Name.accessCheck` is received
   */
  @objc func handlePermissionNotification(_ notification: Notification) {
    print("handlePermissionNotification")
    // Extract message from the notification if available
    if let message = notification.object as? String {
      let permissionGranted = self.processInputMethodResponse(with: message)
      self.completeValidation(accessibilityPermissionGranted: permissionGranted)
    } else {
      print("accessCheckResponse received but did not include message")
    }
  }
  
  /**
   * Process the distributed notification message that we received from the Keyman input method.
   */
  func processInputMethodResponse(with message: String) -> Bool {
    let timeStyle = Date.FormatStyle()
      .hour(.twoDigits(amPM: Date.FormatStyle.Symbol.Hour.AMPMStyle.abbreviated))
      .minute(.twoDigits)
      .second(.twoDigits)
      .secondFraction(.fractional(3))
    print("processAccessibilityCheckResponse received message: \(message), time: \(Date().formatted(timeStyle))")
    
    // if the message indicates that access was granted, then return true
    return !message.isEmpty && message == kAccessibilityPermissionGrantedMessage
  }

  /**
   * Save the new InstallationState for handling repairs and notify observers
   */
  func prepareToRepair(newState: InstallationState) {
    self.defaultsRepository.writeInstallationState(newState.toUserDefaultsDictionary())
    self.installationState = newState
    NotificationCenter.default.post(name: .installationRepairNeeded, object: newState)
  }

  /**
   * Load the installation state and the tasks required to complete the installation of Keyman.
   * This is accomplished by one of the following:
   * 1. reading the saved installation which is either
   *  completed or
   *  in progress
   * 2. creating a new installation
   */
  public func loadState() -> InstallationState? {
    var installationState: InstallationState
    
    guard self.isInputMethodInstalled && self.isInputMethodCurrent else {
      return nil
    }
    
    if let savedInstallationState = readInstallationState() {
        installationState = savedInstallationState
    } else {
      // if installation could not be read, then
      installationState = self.createInstallationStateForNewInstallation()
    }
    
    return installationState
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
   * Determine whether the completed installation has been altered in some way and needs repair.
   * If repair is needed, then call `prepareToRepair` with the new `InstallationState`
   */
  func completeValidation(accessibilityPermissionGranted: Bool) {
    // check whether the installation requires repair
    if let newInstallationState = self.createRepairInstallationState(accessibilityPermissionGranted: accessibilityPermissionGranted) {
      self.prepareToRepair(newState: newInstallationState)
    } else {
      print("completeValidation: no repair needed")
    }
  }
  
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
    taskList.insert(InstallationTask(task: .migrateData, completed: false))
    taskList.insert(InstallationTask(task: .enableInputMethod, completed: false))
    taskList.insert(InstallationTask(task: .requestAccess, completed: false))
    taskList.insert(InstallationTask(task: .restartMac, completed: false))
    return taskList
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
