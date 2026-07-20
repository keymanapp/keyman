/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-06-01
 *
 * Class for tracking overall state of the installation.
 * Includes functions for mapping to a Dictionary to store in the UserDefaults.
 */

import Foundation
import KeymanSettings

public class InstallationState {
  // keys to the fields we are storing in the UserDefaults KMInstallationState dictionary
  let kVersionKey = "version"
  let kDateRestartRequestedKey = "dateRestartRequested"
  let kRepairKey = "isRepair"
  
  public let keymanVersion: String
  public var dateRestartRequested: Date?
  // indicates whether we are repairing a previous installation or doing a full installation
  public let isRepair: Bool
  // this list of tasks that make up this installation
  public var tasks: Set<InstallationTask>
  
  /**
   * The installation is complete if all its tasks are complete.
   * Returns true if the tasks list is empty, but it should never be empty.
   */
  public var isComplete: Bool {
    tasks.allSatisfy(\.isComplete)
  }

  /**
   * The installation is new if this is not a repair and no tasks are complete.
   * Returns true if the tasks list is empty, but it should never be empty.
   */
  public var isNew: Bool {
    return !isRepair && tasks.allSatisfy{ !$0.isComplete }
  }

  init(version: String, dateRestartRequested: Date? = nil, isRepair: Bool = false, tasks: Set<InstallationTask>) {
    self.keymanVersion = version
    self.dateRestartRequested = dateRestartRequested
    self.isRepair = isRepair
    self.tasks = tasks
  }
  
  /**
   * initialize using the dictionary from UserDefaults
   */
  init?(from dictionary: Dictionary<String, Any>) {
    self.keymanVersion = dictionary[kVersionKey] as? String ?? ""
    self.dateRestartRequested = dictionary[kDateRestartRequestedKey] as? Date
    self.isRepair = dictionary[kRepairKey] as? Bool ?? false
    var installationTasks = Set<InstallationTask>()
    
    // for every task flag found in dictionary, insert a task in the tasks array
    if let taskFlag = dictionary[InstallationTaskType.verifyInputMethod.rawValue] as? Bool {
      installationTasks.insert(InstallationTask(task: .verifyInputMethod, completed: taskFlag))
    }
    if let taskFlag = dictionary[InstallationTaskType.migrateData.rawValue] as? Bool {
      installationTasks.insert(InstallationTask(task: .migrateData, completed: taskFlag))
    }
    if let taskFlag = dictionary[InstallationTaskType.enableInputMethod.rawValue] as? Bool {
      installationTasks.insert(InstallationTask(task: .enableInputMethod, completed: taskFlag))
    }
    if let taskFlag = dictionary[InstallationTaskType.requestAccess.rawValue] as? Bool {
      installationTasks.insert(InstallationTask(task: .requestAccess, completed: taskFlag))
    }
    if let taskFlag = dictionary[InstallationTaskType.restartMac.rawValue] as? Bool {
      installationTasks.insert(InstallationTask(task: .restartMac, completed: taskFlag))
    }
    
    self.tasks = installationTasks
  }
  
  /**
   * create a dictionary for storing the current InstallationState state in the UserDefaults
   */
  public func toUserDefaultsDictionary() -> Dictionary<String, Any> {
    var dictionary: Dictionary<String, Any> = [:]
    
    dictionary[kVersionKey] = keymanVersion
    if let dateRestartRequested = self.dateRestartRequested {
      dictionary[kDateRestartRequestedKey] = dateRestartRequested
    }
    dictionary[kRepairKey] = self.isRepair
    
    for task in self.tasks {
      dictionary[task.taskType.rawValue] = task.isComplete
    }
    
    return dictionary
  }
  
  /**
   * update the task of the specified type as completed
   */
  public func updateTaskAsCompleted(task: InstallationTaskType) {
    let completedTask = InstallationTask(task: task, completed: true)
    self.tasks.update(with: completedTask)
  }
}
