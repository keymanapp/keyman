/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-06-01
 *
 * Class for evaluating the current state of Keyman to determine is complete or in need of repair
 * and what remaining tasks are needed to complete the installation.
 *
 * If the Keyman input method exists in the Input Methods folder and is is the correct version,
 * then the evaluate() returns an InstallationState object describing what is required to complete
 * the installation.
 */

import Foundation
import KeymanSettings


public class InstallationCheck {
  public let isInputMethodInstalled: Bool
  public let isInputMethodCurrent: Bool
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
    self.isInputMethodCurrent = self.inputMethodVersion == self.configurationVersion
  }
  
  /**
   * Determine what tasks are required to complete the installation of Keyman.
   * This could be one of the following:
   * 1. a new installation
   * 2. a completed installion
   * 3. an installation that is in progress
   * 4. an installation that was completed but has been tampered with in some way and needs repair
   */
  public func evaluate() -> InstallationState? {
    var installationState: InstallationState
    
    guard self.isInputMethodInstalled && self.isInputMethodCurrent else {
      return nil
    }
    
    if let savedInstallationState = readInstallationState() {
      // check whether the installation requires repair
      if let repairInstallationState = self.createRepairInstallationState(savedInstallationState: savedInstallationState) {
        installationState = repairInstallationState
        self.defaultsRepository.writeInstallationState(installationState.toUserDefaultsDictionary())
      } else {
        installationState = savedInstallationState
      }
    } else {
      // if installation could not be read, then
      installationState = self.createInstallationStateForNewInstallation()
    }
    
    return installationState
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
   * If the installation needs repair, create the info needed for repairing the installation
   */
  func createRepairInstallationState(savedInstallationState: InstallationState) -> InstallationState? {
    var repairInstallationState: InstallationState? = nil
    var repairTasks = Set<InstallationTask>()
    
    guard savedInstallationState.isComplete else {
      // do not attempt to repair if the installation is still in progress
      return nil
    }
    
    // MAC-CONFIG-TODO: this task may not be needed
    if !self.inputMethodUtil.isKeymanInputMethodCurrent() {
      repairTasks.insert(InstallationTask.createNewInstallationTask(type: .verifyInputMethod))
    }
    
    if !self.inputMethodUtil.isKeymanInputMethodEnabled() {
      repairTasks.insert(InstallationTask.createNewInstallationTask(type: .enableInputMethod))
      
      // also need to restart after enabling the input method
      repairTasks.insert(InstallationTask.createNewInstallationTask(type: .restartMac))
    }
    
    // call the input method to check whether Accessibility permission has been granted
    self.inputMethodUtil.doAsyncAccessibilityCheck()
    
    // the response will be received from the input method before this executes,
    // so unwrapping the optional will be successfully unwrapped
    if let permissionGranted = self.inputMethodUtil.accessibilityPermissionGranted {
      if !permissionGranted {
        repairTasks.insert(InstallationTask.createNewInstallationTask(type: .requestAccess))
      }
    }
    
    if !repairTasks.isEmpty {
      repairInstallationState = InstallationState(version: self.inputMethodVersion, tasks: repairTasks)
    }
    
    return repairInstallationState
  }
}
