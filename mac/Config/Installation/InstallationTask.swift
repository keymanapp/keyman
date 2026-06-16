/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-06-01
 *
 * Class for tracking the state of a single installation task.
 */

import Foundation
import KeymanSettings

// String raw value for each task type is used as key for Dictionary
// when storing the task state in the UserDefaults
public enum InstallationTaskType: String {
  case verifyInputMethod = "verify"             // verify the new input method was installed
  case migrateData = "migrateData"              // only called for new install
  case enableInputMethod = "enableInputMethod"  // triggers register and enable
  case requestAccess = "requestAccess"          // causes prompt to user to grant Accessibility
  case restartMac = "restartMac"                // prompts user to restart machine
}

public struct InstallationTask: Hashable, Equatable {
  public let taskType: InstallationTaskType
  public let isComplete: Bool
  
  /**
   * Hashable conformance based on taskType
   */
  public func hash(into hasher: inout Hasher) {
    hasher.combine(taskType)
  }
  
  /**
   * Hashable conformance based on taskType
   */
  public static func == (lhs: InstallationTask, rhs: InstallationTask) -> Bool {
    return lhs.taskType == rhs.taskType
  }
  
  /**
   * Create new InstallationTask: `completed` set to `false`
   */
  static func createNewInstallationTask(type: InstallationTaskType) -> InstallationTask {
    return InstallationTask(task: type, completed: false)
  }
  
  /**
   * Create InstallationTask with `completed` set to `true`
   */
  static func createCompletedInstallationTask(type: InstallationTaskType) -> InstallationTask {
    return InstallationTask(task: type, completed: true)
  }
  
  init(task: InstallationTaskType, completed: Bool) {
    self.taskType = task
    self.isComplete = completed
  }
}
