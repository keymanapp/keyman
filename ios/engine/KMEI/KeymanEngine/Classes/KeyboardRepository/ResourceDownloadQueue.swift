//
//  ResourceDownloadQueue.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 8/20/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation
import Reachability

protocol DownloadNode { }

class DownloadTask: DownloadNode {
  public enum Resource {
    case keyboard, lexicalModel, other
  }
  
  public final var type: Resource
  public final var resources: [LanguageResource]?
  public final var request: HTTPDownloadRequest
  
  public init(do request: HTTPDownloadRequest, for resources: [LanguageResource]?, type: DownloadTask.Resource) {
    self.request = request
    self.type = type
    self.resources = resources
  }
}

/**
 * Represents one overall resource-related command for requests against the Keyman Cloud API.
 */
class DownloadBatch: DownloadNode {
  public enum Activity {
    case download, update, composite
  }
  
  /*
   * Three main cases so far:
   * - [.download,  .keyboard]:      download new Keyboard (possibly with an associated lexical model)
   * - [.download,  .lexicalModel]:  download new LexicalModel.
   * - [.composite, .other]:   batch update of all resources, containing the following within its compositeQueue
   *   - [.update, .keyboard]:      updates one Keyboard
   *   - [.update, .lexicalModel]:  updates one LexicalModel
   *   - Depending on needs, this may be extended to allow 'nested' .composite instances.
   */
  public final var activity: Activity
  public final var type: DownloadTask.Resource
  
  public final var tasks: [DownloadNode]
  //public final var promise: Int
  
  public init?(do tasks: [DownloadTask], as activity: Activity, ofType type: DownloadTask.Resource) {
    self.activity = activity
    self.type = type
    self.tasks = tasks
    
    // Indicates 'composite' mode, which should use the other initializer.
    if activity == .composite {
      return nil
    }

    // A batch containing tasks should always be targeting a 'primary' language resource.
    // .other exists to handle fonts packaged with keyboards, not our primary resources.
    if type == .other {
      return nil
    }
  }
  
  public init(queue: [DownloadBatch]) {
    self.activity = .composite
    self.type = .other
    self.tasks = queue
  }
}

// This is a private class used internally by ResourceDownloadQueue to track progress through
// the queue and its DownloadBatches.  We need a stack scheme - each instance of this corresponds
// to one frame of the stack.
private class DownloadQueueFrame {
  public var nodes: [DownloadNode] = []
  public var index: Int = 0
  private(set) var isComposite = false
  
  public init() { }
  
  public init? (from batch: DownloadBatch) {
    if batch.activity != .composite {
      return nil
    }
    
    self.isComposite = true
    nodes.append(contentsOf: batch.tasks)
  }
}

// The other half of ResourceDownloadManager, this class is responsible for executing the downloads
// and handling the results.
class ResourceDownloadQueue: HTTPDownloadDelegate {
  private var queueRoot: DownloadQueueFrame
  private var queueStack: [DownloadQueueFrame]
  
  private var downloader: HTTPDownloader?
  private var reachability: Reachability!
  private let keymanHostName = "api.keyman.com"
  
  public init() {
    reachability = Reachability(hostname: keymanHostName)
    
    queueRoot = DownloadQueueFrame()
    queueStack = [queueRoot] // queueRoot will always be the bottom frame of the stack.
  }
  
  public func hasConnection() -> Bool {
    return reachability.connection != Reachability.Connection.none
  }
  
  // Might should add a "withNotification: Bool" option for clarity.
  public func canExecute(_ batch: DownloadBatch) -> Bool {
    guard reachability.connection != Reachability.Connection.none else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "No internet connection"])
      downloadFailed(forBatch: batch, error: error)
      return false
    }
    
    // At this stage, we now have everything needed to generate download requests.
    guard currentBatch == nil else { // Original behavior - only one download operation is permitted at a time.
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "Download queue is busy"])
      downloadFailed(forBatch: batch, error: error)
      return false
    }
    
    return true
  }
  
  public var currentBatch: DownloadBatch? {
    get {
      let frame = queueStack[queueStack.count - 1]
      
      if(frame.nodes.count == 0) {
        return nil // Nothing's downloading.
      } else {
        return (frame.nodes[frame.index] as! DownloadBatch) // Return the currently processing DownloadBatch.
      }
    }
  }

  private func finalizeCurrentBatch() {
    let frame = queueStack[queueStack.count - 1]
    
    if queueStack.count == 1 {
      // Batches in the root may be safely dumped from the queue.
      // This automatically cleans out old top-level commands once done, instead of maintaining a persistent history.
      //
      // We _could_ keep the full history until the last command in the queue is done and empty the root's node list
      // when everything's done instead.  Might be worth a thought.
      frame.nodes.remove(at: 0)
    } else {
      // Batches in subframes should be kept so that we can report progress; increment the index instead.
      frame.index += 1
    }
  }
  
  private func executeNext() {
    let frame = queueStack[queueStack.count - 1]
    
    // If nothing's in the queue, this will be straight-forward.
    if queueStack.count == 1 { // Are we on the root frame?
      // Is the root frame out of commands?
      if frame.nodes.count == 0 {
        return
      } // else
      
      let batch = frame.nodes[0] as! DownloadBatch
      innerExecute(batch)
    } else { // We're in a queue stack frame; check our state!
      if frame.index == frame.nodes.count {
        // We've hit the end of this stack frame's commands; time to pop and continue from the previous frame's perspective.
        _ = queueStack.popLast()
        
        // Of course, this means we've "finished" a batch download.  We can use the same handlers as before.
        finalizeCurrentBatch()
        executeNext()
        return
      } else {
        // We've got more batches left within this stack frame.  Continue.
        let batch = frame.nodes[frame.index] as! DownloadBatch
        innerExecute(batch)
      }
    }
  }

  private func innerExecute(_ batch: DownloadBatch) {
    if batch.activity == .composite {
      // We're a batch composed of multiple sub-batches.  Time to set that up on the queue!
      let frame = DownloadQueueFrame(from: batch)!
      queueStack.append(frame)
      // TODO:  Notify about the new batch being started!
      
      innerExecute(batch.tasks[0] as! DownloadBatch)
      return
    } else {
      // Make a separate one for each batch; this simplifies event handling when multiple batches are in queue,
      // as the old downloader will have a final event left to trigger at this point.  (downloadQueueFinished)
      downloader = HTTPDownloader(self)
      let tasks = batch.tasks as! [DownloadTask]
      downloader!.userInfo = [Key.downloadBatch: batch]
      
      tasks.forEach { task in
        downloader!.addRequest(task.request)
      }
      
      downloader!.run()
      // TODO:  Notify about the new batch being started!
    }
  }
  
  public func queue(_ batch: DownloadBatch) {
    queueRoot.nodes.append(batch)
    
    // If the queue was empty before this...
    if queueRoot.nodes.count == 1 {
      executeNext()
    }
  }
  
  // MARK - helper methods for ResourceDownloadManager
  
  var currentRequest: HTTPDownloadRequest? {
    get {
      // This was literally the state of this property when the refactor producing this class was performed.
      return nil;
    }
  }

  // Needs validation.
  func keyboardIdForCurrentRequest() -> String? {
    if let currentRequest = currentRequest {
      let tmpStr = currentRequest.url.lastPathComponent
      if tmpStr.hasJavaScriptExtension {
        return String(tmpStr.dropLast(3))
      }
    } else {
      // TODO:  search the queue instead.
//      let kbInfo = downloader!.userInfo[Key.keyboardInfo]
//      if let keyboards = kbInfo as? [InstallableKeyboard], let keyboard = keyboards.first {
//        return keyboard.id
//      }
    }
    return nil
  }
  
  func lexicalModelIdForCurrentRequest() -> String? {
    if let currentRequest = currentRequest {
      let tmpStr = currentRequest.url.lastPathComponent
      if tmpStr.hasJavaScriptExtension {
        return String(tmpStr.dropLast(3))
      }
    } else {
      // TODO: search the queue instead.
//      let kbInfo = downloader!.userInfo[Key.lexicalModelInfo]
//      if let lexicalModels = kbInfo as? [InstallableLexicalModel], let lexicalModel = lexicalModels.first {
//        return lexicalModel.id
//      }
    }
    return nil
  }

  // MARK - notification methods
  
  private func downloadFailed(forBatch batch: DownloadBatch, error: Error) {
    let tasks = batch.tasks as! [DownloadTask]
    if batch.activity == .composite {
      // It's an update operation.
      // Not yet called in this case.
    } else if batch.type == .keyboard {
      let keyboards = tasks.compactMap { task in
        return task.resources as? [InstallableKeyboard]
      }.flatMap {$0}
      downloadFailed(forKeyboards: keyboards, error: error)
    } else if batch.type == .lexicalModel {
      let lexModels = tasks.compactMap { task in
        return task.resources as? [InstallableLexicalModel]
      }.flatMap {$0}
      downloadFailed(forLanguageID: lexModels[0].languageID, error: error)
    }
  }
  
  public func downloadFailed(forKeyboards keyboards: [InstallableKeyboard], error: Error) {
    let notification = KeyboardDownloadFailedNotification(keyboards: keyboards, error: error)
    NotificationCenter.default.post(name: Notifications.keyboardDownloadFailed,
                                    object: self,
                                    value: notification)
  }
  
  public func downloadFailed(forLanguageID languageID: String, error: Error) {
    let notification = LexicalModelDownloadFailedNotification(lmOrLanguageID: languageID, error: error)
    NotificationCenter.default.post(name: Notifications.lexicalModelDownloadFailed,
                                    object: self,
                                    value: notification)
  }
  
  public func downloadFailed(forLexicalModelPackage packageURL: String, error: Error) {
    let notification = LexicalModelDownloadFailedNotification(lmOrLanguageID: packageURL, error: error)
    NotificationCenter.default.post(name: Notifications.lexicalModelDownloadFailed,
                                    object: self,
                                    value: notification)
  }
  
  public func downloadSucceeded(forLexicalModel lm: InstallableLexicalModel) {
    let notification = LexicalModelDownloadCompletedNotification([lm])
    NotificationCenter.default.post(name: Notifications.lexicalModelDownloadCompleted,
                                    object: self,
                                    value: notification)
  }

  //MARK: - HTTPDownloadDelegate methods
  
  func downloadQueueFinished(_ queue: HTTPDownloader) {
    // We can use the properties of the current "batch" to generate specialized notifications.
    
    // Completing the queue means having completed a batch.  We should only move forward in this class's
    // queue at this time, once a batch's task queue is complete.
    finalizeCurrentBatch()
    executeNext()
  }

  func downloadRequestStarted(_ request: HTTPDownloadRequest) {
    // If we're downloading a new keyboard.
    // The extra check is there to filter out other potential request types in the future.
    if request.tag == 0 {
      let task = request.userInfo[Key.downloadTask] as! DownloadTask
      if task.type == .keyboard {
        NotificationCenter.default.post(name: Notifications.keyboardDownloadStarted,
                                        object: self,
                                        value: task.resources as! [InstallableKeyboard])
      } else if task.type == .lexicalModel {
        NotificationCenter.default.post(name: Notifications.lexicalModelDownloadStarted,
                                        object: self,
                                        value: task.resources as! [InstallableLexicalModel])
      }
    }
  }

  func downloadRequestFinished(_ request: HTTPDownloadRequest) {
    let batch = request.userInfo[Key.downloadBatch] as! DownloadBatch
    let isUpdate = batch.activity == .update
    
    let task = request.userInfo[Key.downloadTask] as! DownloadTask
    
    if let statusCode = request.responseStatusCode, statusCode == 200 {
      if task.type == .keyboard {
        // The request has succeeded.
        if downloader!.requestsCount == 0 {
          let keyboards = task.resources as? [InstallableKeyboard]

          // Download queue finished.
          FontManager.shared.registerCustomFonts()
          log.info("Downloaded keyboard: \(keyboards![0].id).")

          NotificationCenter.default.post(name: Notifications.keyboardDownloadCompleted,
                                          object: self,
                                          value: keyboards!)
          // TODO: Trigger by notification.  Needs to be done on Manager.swift, not this class.
//          if isUpdate {
//            shouldReloadKeyboard = true
//            inputViewController.reload()
//          }
          let userDefaults = Storage.active.userDefaults
          userDefaults.set([Date()], forKey: Key.synchronizeSWKeyboard)
          userDefaults.synchronize()
        }
      } else if task.type == .lexicalModel {
        if let lm = installLexicalModelPackage(downloadedPackageFile: URL.init(string: task.request.destinationFile!)!) {
          downloadSucceeded(forLexicalModel: lm)
        } else {
          let installError = NSError(domain: "Keyman", code: 0,
                                     userInfo: [NSLocalizedDescriptionKey: "installError"])
          downloadFailed(forLexicalModelPackage: "\(task.request.url)", error: installError )
        }
      }
    } else { // Possible request error (400 Bad Request, 404 Not Found, etc.)
      downloader!.cancelAllOperations()
      // TODO:  Should we clear the entire queue in this case, or just the current batch?
      //        If just the batch, make sure we handle any corresponding notifications correctly.

      let errorMessage = "\(request.responseStatusMessage ?? ""): \(request.url)"
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: errorMessage])
      
      if task.type == .keyboard {
        let keyboards = task.resources as? [InstallableKeyboard]
        log.error("Keyboard download failed: \(error).")

        if !isUpdate {
          // Clean up keyboard file if anything fails
          // TODO: Also clean up remaining fonts
          try? FileManager.default.removeItem(at: Storage.active.keyboardURL(for: keyboards![0]))
        }

        downloadFailed(forKeyboards: keyboards ?? [], error: error)
      } else if task.type == .lexicalModel {
        let lexicalModels = task.resources as? [InstallableLexicalModel]
        log.error("Dictionary download failed: \(error).")

        if !isUpdate {
          // Clean up keyboard file if anything fails
          // TODO: Also clean up remaining fonts
          try? FileManager.default.removeItem(at: Storage.active.lexicalModelURL(for: lexicalModels![0]))
        }

        downloadFailed(forLanguageID: lexicalModels?[0].languageID ?? "", error: error)
      }
    }
  }
  
  //func downloadRequestSuccess(

  func downloadRequestFailed(_ request: HTTPDownloadRequest) {
    switch request.typeCode {
    case .downloadFile:
      let error = request.error!

      let task = request.userInfo[Key.downloadTask] as! DownloadTask
      let batch = request.userInfo[Key.downloadBatch] as! DownloadBatch
      let isUpdate = batch.activity == .update
      
      if task.type == .keyboard {
        log.error("Keyboard download failed: \(error).")
        let keyboards = task.resources as? [InstallableKeyboard]

        if !isUpdate {
          // Clean up keyboard file if anything fails
          // TODO: Also clean up remaining fonts
          try? FileManager.default.removeItem(at: Storage.active.keyboardURL(for: keyboards![0]))
        }
        
        downloadFailed(forKeyboards: keyboards ?? [], error: error as NSError)
      } else if task.type == .lexicalModel {
        log.error("Dictionary download failed: \(error).")
        let lexicalModels = task.resources as? [InstallableLexicalModel]
        
        if !isUpdate {
          try? FileManager.default.removeItem(at: Storage.active.lexicalModelURL(for: lexicalModels![0]))
        }
        
        downloadFailed(forLanguageID: lexicalModels?[0].languageID ?? "", error: error as NSError)
      }
    }
  }
  
  // MARK - Language resource installation methods
  
  // Processes fetched lexical models.
  // return a lexical model so caller can use it in a downloadSucceeded call
  // is called by other class funcs
  func  installLexicalModelPackage(downloadedPackageFile: URL) -> InstallableLexicalModel? {
    var installedLexicalModel: InstallableLexicalModel? = nil
    let documentsDirectory = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
    var destination =  documentsDirectory
    destination.appendPathComponent("temp/\(downloadedPackageFile.lastPathComponent)")
    
    KeymanPackage.extract(fileUrl: downloadedPackageFile, destination: destination, complete: { kmp in
      if let kmp = kmp as! LexicalModelKeymanPackage? {
        do {
          try Manager.shared.parseLMKMP(kmp.sourceFolder)
          log.info("successfully parsed the lexical model in: \(kmp.sourceFolder)")
          installedLexicalModel = kmp.models[0].installableLexicalModels[0]
          //this can fail gracefully and not show errors to users
          try FileManager.default.removeItem(at: downloadedPackageFile)
        } catch {
          log.error("Error installing the lexical model: \(error)")
        }
      } else {
        log.error("Error extracting the lexical model from the package: \(KMPError.invalidPackage)")
      }
    })
    return installedLexicalModel
  }
}
