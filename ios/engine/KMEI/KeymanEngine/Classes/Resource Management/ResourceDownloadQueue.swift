//
//  ResourceDownloadQueue.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 8/20/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation
import Reachability

enum DownloadNode {
  case simpleBatch(AnyDownloadBatch)
  case compositeBatch(CompositeBatch)

  var resources: [AnyLanguageResource] {
    switch(self) {
      case .simpleBatch(let batch):
        return batch.resources
      case .compositeBatch(let node):
        return node.resources
    }
  }

  var errors: [Error?] {
    get {
      switch(self) {
        case .simpleBatch(let batch):
          return batch.errors
        case .compositeBatch(let node):
          return node.errors
      }
    }

    set(value) {
      switch(self) {
        case .simpleBatch(var batch):
          batch.errors = value
        case .compositeBatch(let node):
          node.errors = value
      }
    }
  }
}

protocol AnyDownloadTask {
  var type: LanguageResourceType? { get }
  var request: HTTPDownloadRequest { get }
}

class DownloadTask<Resource: LanguageResource>: AnyDownloadTask {
  public final var type: LanguageResourceType?
  public final var resources: [Resource]?
  public final var request: HTTPDownloadRequest
  
  public init(do request: HTTPDownloadRequest, for resources: [Resource]?, type: LanguageResourceType?) {
    self.request = request
    self.type = type
    self.resources = resources
  }
}

enum DownloadActivityType {
  case download, update
}

protocol AnyDownloadBatch {
  var activity: DownloadActivityType { get }
  var type: LanguageResourceType? { get }
  var tasks: [AnyDownloadTask] { get }
  var resources: [AnyLanguageResource] { get }
  var errors: [Error?] { get set }
}

/**
 * Represents one overall resource-related command for requests against the Keyman Cloud API.
 */
class DownloadBatch<Resource: LanguageResource, Package: TypedKeymanPackage<Resource>>: AnyDownloadBatch {
  public final var activity: DownloadActivityType
  public final var type: LanguageResourceType?
  
  public final var downloadTasks: [DownloadTask<Resource>]
  var errors: [Error?] // Only used by the ResourceDownloadQueue.
  public final var completionBlock: ResourceDownloadManager.CompletionHandler<Resource, Package>? = nil
  
  public init?(do tasks: [DownloadTask<Resource>], as activity: DownloadActivityType, ofType type: LanguageResourceType, completionBlock: ResourceDownloadManager.CompletionHandler<Resource, Package>? = nil) {
    self.activity = activity
    self.type = type
    self.downloadTasks = tasks

    self.errors = Array(repeating: nil, count: tasks.count)
    self.completionBlock = completionBlock
  }

  public var tasks: [AnyDownloadTask] {
    return downloadTasks
  }
  
  public var resources: [AnyLanguageResource] {
    get {
      var resArray: [AnyLanguageResource] = []
      
      downloadTasks.forEach{ task in
        resArray.append(contentsOf: task.resources ?? [])
      }
      
      return resArray
    }
  }
}

class CompositeBatch {
  public final var batches: [DownloadNode]
  var errors: [Error?] // Only used by the ResourceDownloadQueue.
  // TODO:  Figure out typing, etc for a proper composite completion handler.  If we want one.
  //public final var completionBlock: ResourceDownloadManager.CompletionHandler<Resource, Package>? = nil

  public init(queue: [DownloadNode]) {
    self.batches = queue
    self.errors = Array(repeating: nil, count: batches.count)
  }

  public var tasks: [DownloadNode] {
    return batches
  }

  public var resources: [AnyLanguageResource] {
    return batches.flatMap { $0.resources }
  }
}

// This is a private class used internally by ResourceDownloadQueue to track progress through
// the queue and its DownloadBatches.  We need a stack scheme - each instance of this corresponds
// to one frame of the stack.
private class DownloadQueueFrame {
  public var nodes: [DownloadNode] = []
  public var index: Int = 0
  public final var batch: DownloadNode?
  
  public init() {
    batch = nil
  }
  
  public init? (from batch: CompositeBatch) {
    self.batch = .compositeBatch(batch)
    nodes.append(contentsOf: batch.tasks)
  }

  public var isComposite: Bool {
    if case .compositeBatch(_) = batch {
      return true
    } else {
      return false
    }
  }
}

// The other half of ResourceDownloadManager, this class is responsible for executing the downloads
// and handling the results.
class ResourceDownloadQueue: HTTPDownloadDelegate {
  private var queueRoot: DownloadQueueFrame
  private var queueStack: [DownloadQueueFrame]
  
  private var downloader: HTTPDownloader?
  private var reachability: Reachability?
  private let keymanHostName = "api.keyman.com"
  
  public init() {
    do {
      try reachability = Reachability(hostname: keymanHostName)
    } catch {
      log.error("Could not start Reachability object: \(error)")
    }
    
    queueRoot = DownloadQueueFrame()
    queueStack = [queueRoot] // queueRoot will always be the bottom frame of the stack.
  }
  
  public func hasConnection() -> Bool {
    return reachability?.connection != Reachability.Connection.unavailable
  }
  
  // Might should add a "withNotification: Bool" option for clarity.
  public func canExecute(_ batch: DownloadNode) -> Bool {
    guard hasConnection() else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "No internet connection"])
      if let batch = batch as? AnyDownloadBatch {
        downloadFailed(forBatch: batch, error: error)
      }
      return false
    }
    
    // At this stage, we now have everything needed to generate download requests.
    guard currentBatch == nil else { // Original behavior - only one download operation is permitted at a time.
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "Download queue is busy"])
      if let batch = batch as? AnyDownloadBatch {
        downloadFailed(forBatch: batch, error: error)
      }
      return false
    }
    
    return true
  }
  
  public var currentBatch: DownloadNode? {
    get {
      let frame = currentFrame
      
      if(frame.nodes.count == 0) {
        return nil // Nothing's downloading.
      } else {
        return (frame.nodes[frame.index]) // Return the currently processing DownloadNode.
      }
    }
  }
  
  private var currentFrame: DownloadQueueFrame {
    get {
      return queueStack[queueStack.count - 1]
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
      
      let node = frame.nodes[0]
      innerExecute(node)
    } else { // We're in a queue stack frame; check our state!
      if frame.index == frame.nodes.count {
        // We've hit the end of this stack frame's commands; time to pop and continue from the previous frame's perspective.
        _ = queueStack.popLast()
        
        // In the current state of the app, this is only the case for batch updates.
        // if-check below:  "if the current stack-frame came from a composite batch, let node = that 'composite batch'"
        if case .compositeBatch(let node) = frame.batch {
          updateBatchFinished(node)
          // TODO:  call completion handler, should we decide to support them for composite batches.
        }
        
        // Of course, this means we've "finished" a batch download.  We can use the same handlers as before.
        finalizeCurrentBatch()
        executeNext()
        return
      } else {
        // We've got more batches left within this stack frame.  Continue.
        let node = frame.nodes[frame.index]
        innerExecute(node)
      }
    }
  }

  private func innerExecute(_ node: DownloadNode) {
    switch(node) {
      case .simpleBatch(let batch):
        // Make a separate one for each batch; this simplifies event handling when multiple batches are in queue,
        // as the old downloader will have a final event left to trigger at this point.  (downloadQueueFinished)
        downloader = HTTPDownloader(self)
        let tasks = batch.tasks

        downloader!.userInfo = [Key.downloadBatch: batch]

        tasks.forEach { task in
          downloader!.addRequest(task.request)
        }

        downloader!.run()
      case .compositeBatch(let batch):
        // It's the top level of an update operation.  Time to make a correpsonding notification.
        updateBatchStarted(batch)
        // We're a batch composed of multiple sub-batches.  Time to set that up on the queue!
        let frame = DownloadQueueFrame(from: batch)!
        queueStack.append(frame)

        innerExecute(batch.tasks[0])
    }
  }
  
  public func queue(_ node: DownloadNode) {
    queueRoot.nodes.append(node)
    
    // If the queue was empty before this...
    if queueRoot.nodes.count == 1 {
      executeNext()
    }
  }
  
  // MARK - helper methods for ResourceDownloadManager
  
  // TODO:  Eliminate this property coompletely.
  var currentRequest: HTTPDownloadRequest? {
    get {
      // This was literally the state of this property when the refactor producing this class was performed.
      return nil;
    }
  }

  // TODO: Needs validation.
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
  
  private func downloadFailed(forBatch batch: AnyDownloadBatch, error: Error) {
    if let batch = batch as? DownloadBatch<InstallableKeyboard, KeyboardKeymanPackage> {
      let keyboards = batch.downloadTasks.compactMap { task in
        return task.resources
      }.flatMap {$0}
      downloadFailed(forKeyboards: keyboards, error: error)
    } else if let batch = batch as? DownloadBatch<InstallableLexicalModel, LexicalModelKeymanPackage> {
      let lexModels = batch.downloadTasks.compactMap { task in
        return task.resources
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
  
  public func downloadSucceeded(forKeyboards keyboards: [InstallableKeyboard]) {
    let notification = KeyboardDownloadCompletedNotification(keyboards)
    NotificationCenter.default.post(name: Notifications.keyboardDownloadCompleted,
                                    object: self,
                                    value: notification)
  }
  
  public func downloadSucceeded(forLexicalModel lm: InstallableLexicalModel) {
    let notification = LexicalModelDownloadCompletedNotification([lm])
    NotificationCenter.default.post(name: Notifications.lexicalModelDownloadCompleted,
                                    object: self,
                                    value: notification)
  }
  
  
  public func updateBatchStarted(_ batch: CompositeBatch) {
    let notification = BatchUpdateStartedNotification(batch.resources)
    NotificationCenter.default.post(name: Notifications.batchUpdateStarted,
                                    object: self,
                                    value: notification)
  }
  
  public func updateBatchFinished(_ batch: CompositeBatch) {
    var successes: [[AnyLanguageResource]] = []
    var failures: [[AnyLanguageResource]] = []
    var errors: [Error] = []
    
    // Remember, since this is for .composite batches, batch.tasks is of type [DownloadBatch].
    for (index, res) in batch.tasks.enumerated() {
      if batch.errors[index] == nil {
        successes.append(res.resources)
      } else {
        failures.append(res.resources)
        errors.append(batch.errors[index]!)
      }
    }
    
    let notification = BatchUpdateCompletedNotification(successes: successes, failures: failures, errors: errors)
    NotificationCenter.default.post(name: Notifications.batchUpdateCompleted,
                                    object: self,
                                    value: notification)
  }

  //MARK: - HTTPDownloadDelegate methods
  
  func downloadQueueFinished(_ queue: HTTPDownloader) {
    // We can use the properties of the current "batch" to generate specialized notifications.
    let batch = queue.userInfo[Key.downloadBatch] as! AnyDownloadBatch
    let isUpdate = batch.activity == .update

    if let batch = batch as? DownloadBatch<InstallableKeyboard, KeyboardKeymanPackage> {
      // The request has succeeded.
      if downloader!.requestsCount == 0 { // Download queue finished.
        let keyboards = batch.resources as! [InstallableKeyboard]
        log.info("Downloaded keyboard: \(keyboards[0].id).")

        // TEMP:  wrap the newly-downloaded resources with a kmp.json.
        //        Serves as a bridge until we're downloading actual .kmps for keyboards.
        let wrappedKeyboards = Migrations.migrateToKMPFormat(keyboards)

        wrappedKeyboards.forEach { keyboard in
          ResourceFileManager.shared.addResource(keyboard)
        }

        if(!isUpdate) {
          downloadSucceeded(forKeyboards: wrappedKeyboards)
        }

        let userDefaults = Storage.active.userDefaults
        userDefaults.set([Date()], forKey: Key.synchronizeSWKeyboard)
        userDefaults.synchronize()

        if let package: KeyboardKeymanPackage = ResourceFileManager.shared.getInstalledPackage(for: keyboards[0]) {
          batch.completionBlock?(package, nil)
        } else {
          log.error("Could not load metadata for newly-installed keyboard")
        }
      }
    } else if let batch = batch as? DownloadBatch<InstallableLexicalModel, LexicalModelKeymanPackage> {
      let task = batch.downloadTasks[0] // It's always at this index.
      let (lm, package) = installLexicalModelPackage(downloadedPackageFile: URL(fileURLWithPath: task.request.destinationFile!))
      if let lm = lm, let package = package {
        if !isUpdate {
          downloadSucceeded(forLexicalModel: lm)
        } else {
          // Since we don't generate the notification as above, we need to manually update
          // the lexical model's metadata.
          Manager.shared.updateUserLexicalModels(with: lm)
        }

        batch.completionBlock?(package, nil)
      } else if !isUpdate {
        let installError = NSError(domain: "Keyman", code: 0,
                                   userInfo: [NSLocalizedDescriptionKey: "installError"])
        downloadFailed(forLexicalModelPackage: "\(task.request.url)", error: installError)
        currentFrame.batch?.errors[currentFrame.index] = installError
      }
    }
    
    // Completing the queue means having completed a batch.  We should only move forward in this class's
    // queue at this time, once a batch's task queue is complete.
    finalizeCurrentBatch()
    executeNext()
  }
  
  func downloadQueueCancelled(_ queue: HTTPDownloader) {
    //
    
    // In case we're part of a 'composite' operation, we should still keep the queue moving.
    finalizeCurrentBatch()
    executeNext()
  }

  func downloadRequestStarted(_ request: HTTPDownloadRequest) {
    // If we're downloading a new keyboard.
    // The extra check is there to filter out other potential request types in the future.
    let batch = request.userInfo[Key.downloadBatch] as! AnyDownloadBatch

    if request.tag == 0 && batch.activity != .update {
      let task = request.userInfo[Key.downloadTask] as! AnyDownloadTask
      if task.type == .keyboard {
        let task = task as! DownloadTask<InstallableKeyboard>
        NotificationCenter.default.post(name: Notifications.keyboardDownloadStarted,
                                        object: self,
                                        value: task.resources!)
      } else if task.type == .lexicalModel {
        let task = task as! DownloadTask<InstallableLexicalModel>
        NotificationCenter.default.post(name: Notifications.lexicalModelDownloadStarted,
                                        object: self,
                                        value: task.resources!)
      }
    }
  }

  func downloadRequestFinished(_ request: HTTPDownloadRequest) {
    // Did we finish, but with an request error code?
    if request.responseStatusCode != 200 {
      // Possible request error (400 Bad Request, 404 Not Found, etc.)

      let errorMessage = "\(request.responseStatusMessage ?? ""): \(request.url)"
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: errorMessage])
      currentFrame.batch?.errors[currentFrame.index] = error
      
      // Now that we've synthesized an appropriate error instance, use the same handler
      // as for HTTPDownloader's 'failed' condition.
      downloadRequestFailed(request, with: error)
    } // else - handled once the entire queue is completed without errors.
      //        This particularly matters for keyboards.
  }
  
  func downloadRequestFailed(_ request: HTTPDownloadRequest) {
    // We should never be in a state to return 'Unknown error", but better safe than sorry.
    downloadRequestFailed(request, with: request.error ?? NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "Unknown error"]))
  }

  func downloadRequestFailed(_ request: HTTPDownloadRequest, with error: Error) {
    currentFrame.batch?.errors[currentFrame.index] = error

    let task = request.userInfo[Key.downloadTask] as! AnyDownloadTask
    let batch = request.userInfo[Key.downloadBatch] as! AnyDownloadBatch
    let isUpdate = batch.activity == .update
    
    if let task = task as? DownloadTask<InstallableKeyboard> {
      log.error("Keyboard download failed: \(error).")
      let keyboards = task.resources

      if !isUpdate {
        // Clean up keyboard file if anything fails
        // TODO: Also clean up remaining fonts
        try? FileManager.default.removeItem(at: Storage.active.keyboardURL(for: keyboards![0]))
        downloadFailed(forKeyboards: keyboards ?? [], error: error as NSError)
      }
    } else if let task = task as? DownloadTask<InstallableLexicalModel> {
      log.error("Lexical model download failed: \(error).")
      let lexicalModels = task.resources
      
      if !isUpdate {
        // Clean up model file if anything fails
        try? FileManager.default.removeItem(at: Storage.active.lexicalModelURL(for: lexicalModels![0]))
        downloadFailed(forLanguageID: lexicalModels?[0].languageID ?? "", error: error as NSError)
      }
    }
    
    downloader!.cancelAllOperations()
  }
  
  // MARK - Language resource installation methods
  
  // Processes fetched lexical models.
  // return a lexical model so caller can use it in a downloadSucceeded call
  // is called by other class funcs
  public func installLexicalModelPackage(downloadedPackageFile: URL) -> (InstallableLexicalModel?, LexicalModelKeymanPackage?) {
    var installedLexicalModel: InstallableLexicalModel? = nil
    var kmp: LexicalModelKeymanPackage?

    do {
      let package = try ResourceFileManager.shared.prepareKMPInstall(from: downloadedPackageFile)
      kmp = package as? LexicalModelKeymanPackage
      if let kmp = kmp {
        do {
          try ResourceFileManager.shared.finalizePackageInstall(kmp, isCustom: false)
          log.info("successfully parsed the lexical model in: \(kmp.sourceFolder)")
          installedLexicalModel = kmp.models[0].installableLexicalModels[0]
        } catch {
          log.error("Error installing the lexical model: \(String(describing: error))")
        }
      } else {
        log.error("Provided package did not contain lexical models.")
      }
    } catch {
      log.error("Error extracting the lexical model from the package: \(String(describing: error))")
    }
    
    return (installedLexicalModel, kmp)
  }
}
