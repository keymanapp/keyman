/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-06-01
 *
 * Class for evaluating the current state of Keyman to determine
 * whether installation is complete or in need of repair
 * and what remaining tasks are needed to complete the installation.
 */

import Foundation
import KeymanSettings


public class InstallationCheck {
  fileprivate let inputMethodVersion: String
  fileprivate let configurationVersion: String
  fileprivate let defaultsRepository: DefaultsRepo
  fileprivate let inputMethodUtil: InputMethodUtil
  fileprivate let inputMethodExists: Bool
  
  public init(defaultsRepo: DefaultsRepo, inputMethodUtil: InputMethodUtil) {
    self.defaultsRepository = defaultsRepo
    self.inputMethodUtil = inputMethodUtil
    
    if inputMethodUtil.keymanInputMethodExists() {
      self.inputMethodExists = true
      self.inputMethodVersion = (try? inputMethodUtil.getKeymanInputMethodVersion()) ?? "unknown"
    } else {
      self.inputMethodExists = false
      self.inputMethodVersion = "unknown"
    }
    
    self.configurationVersion = Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String ?? "unknown"
  }
  
  public func evaluate() -> InstallationState {
    var installationState: InstallationState
    
    guard self.inputMethodExists else {
      return createInstallationStateForNonexistentInputMethod()
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
   * Check the version number to see if this is a new installation
   */
  func checkForNewInstallation() {
    // MAC-CONFIG-TODO: impelement version checks
  }

  /**
   * Create InstallationState for non-existent input method (installer needs to be run)
   * Note that this is not persisted, because a non-existent installation doesn't really have state.
   * Instead this is used to indicate to the user that they need to run the installer and try again.
   */
  func createInstallationStateForNonexistentInputMethod() -> InstallationState {
    var taskList = Set<InstallationTask>()
    taskList.insert(InstallationTask(task: .verifyInputMethod, completed: false))
    return InstallationState(exists: false, version: "unknown", tasks: taskList)
  }

  /**
   * Creates a InstallationState object describing a new installation
   */
  func createInstallationStateForNewInstallation() -> InstallationState {
    let installationState = InstallationState(version: self.inputMethodVersion, tasks: self.createNewInstallationTasks())
    self.defaultsRepository.writeInstallationState(installationState.toUserDefaultsDictionary())

    return installationState
  }

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
    
    // MAC-CONFIG-TODO: maybe this should not be a task as we can only notify the user to re-install -- maybe throw an error elsewhere?
    if !self.inputMethodUtil.isKeymanInputMethodCurrent() {
      repairTasks.insert(InstallationTask.createNewInstallationTask(type: .verifyInputMethod))
    }
    
    if !self.inputMethodUtil.isKeymanInputMethodEnabled() {
      repairTasks.insert(InstallationTask.createNewInstallationTask(type: .enableInputMethod))
      
      // also need to restart after enabling the input method
      repairTasks.insert(InstallationTask.createNewInstallationTask(type: .restartMac))
    }
        
    self.inputMethodUtil.doAsyncAccessCheck()
    if !self.inputMethodUtil.requestAccessGranted {
      repairTasks.insert(InstallationTask.createNewInstallationTask(type: .requestAccess))
    }
    
    if !repairTasks.isEmpty {
      repairInstallationState = InstallationState(version: self.inputMethodVersion, tasks: repairTasks)
    }
    
    return repairInstallationState
  }
}
