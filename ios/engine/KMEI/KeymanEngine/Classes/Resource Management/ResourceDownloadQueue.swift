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

  var startBlock: (() -> Void)? { get }
  var errors: [Error?] { get set }

  func completeWithCancellation() -> Void
  func completeWithError(error: Error) -> Void
  func completeWithPackage(fromKMP file: URL) -> Void
}

/**
 * Represents one overall resource-related command for requests against the Keyman Cloud API.
 */
class DownloadBatch<Resource: LanguageResource>: AnyDownloadBatch {
  public final var activity: DownloadActivityType
  public final var type: LanguageResourceType?
  
  public final var downloadTasks: [DownloadTask<Resource>]
  var errors: [Error?] // Only used by the ResourceDownloadQueue.
  public final var startBlock: (() -> Void)? = nil
  public final var completionBlock: ResourceDownloadManager.CompletionHandler<Resource>? = nil
  
  public init?(do tasks: [DownloadTask<Resource>],
               as activity: DownloadActivityType,
               ofType type: LanguageResourceType,
               startBlock: (() -> Void)? = nil,
               completionBlock: ResourceDownloadManager.CompletionHandler<Resource>? = nil) {
    self.activity = activity
    self.type = type
    self.downloadTasks = tasks

    self.errors = Array(repeating: nil, count: tasks.count)
    self.startBlock = startBlock
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

  public func completeWithCancellation() {
    completionBlock?(nil, nil)
  }

  public func completeWithError(error: Error) {
    completionBlock?(nil, error)
  }

  public func completeWithPackage(fromKMP file: URL) {
    do {
      if let package = try ResourceFileManager.shared.prepareKMPInstall(from: file) as? Resource.Package {
        completionBlock?(package, nil)
      } else {
        completionBlock?(nil, KMPError.invalidPackage)
      }
    } catch {
      completionBlock?(nil, error)
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
  enum QueueState: String {
    case clear
    case busy
    case noConnection

    var error: Error? {
      switch(self) {
        case .clear:
          return nil
        case .busy:
          return NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: "No internet connection"])
        case .noConnection:
          return NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: "Download queue is busy"])
      }
    }
  }

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

  public var state: QueueState {
    guard hasConnection() else {
      return .noConnection
    }
    
    // At this stage, we now have everything needed to generate download requests.
    guard currentBatch == nil else { // Original behavior - only one download operation is permitted at a time.
      return .busy
    }
    
    return .clear
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

        // Signal that we've started processing this batch.
        batch.startBlock?()
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

    if let batch = batch as? DownloadBatch<InstallableKeyboard> {
      // batch.downloadTasks[0].request.destinationFile - currently, the downloaded keyboard .js.
      // Once downlading KMPs, will be the downloaded .kmp.

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

        if let package: KeyboardKeymanPackage = ResourceFileManager.shared.getInstalledPackage(for: keyboards[0]) {
          batch.completionBlock?(package, nil)
        } else {
          log.error("Could not load metadata for newly-installed keyboard")
        }
      }
    } else if let batch = batch as? DownloadBatch<InstallableLexicalModel> {
      let task = batch.downloadTasks[0] // It's always at this index.
      let (lm, package) = installLexicalModelPackage(downloadedPackageFile: URL(fileURLWithPath: task.request.destinationFile!))
      if let lm = lm, let package = package {
        if !isUpdate {
          // Since we don't generate the notification as above, we need to manually update
          // the lexical model's metadata.
          Manager.shared.updateUserLexicalModels(with: lm)
        }

        batch.completionBlock?(package, nil)
      }
    }
    
    // Completing the queue means having completed a batch.  We should only move forward in this class's
    // queue at this time, once a batch's task queue is complete.
    finalizeCurrentBatch()
    executeNext()
  }
  
  func downloadQueueCancelled(_ queue: HTTPDownloader) {
    if case .simpleBatch(let batch) = self.currentBatch {
      batch.completeWithCancellation()
    }
    
    // In case we're part of a 'composite' operation, we should still keep the queue moving.
    finalizeCurrentBatch()
    executeNext()
  }

  func downloadRequestStarted(_ request: HTTPDownloadRequest) {
    // If we're downloading a new keyboard.
    // The extra check is there to filter out other potential request types in the future.
    let batch = request.userInfo[Key.downloadBatch] as! AnyDownloadBatch

    // TODO:  remove the != .update check - that should be handled when startBlocks are assigned.
    if batch.activity != .update {
      batch.startBlock?()
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
    let batch = request.userInfo[Key.downloadBatch] as! AnyDownloadBatch

    batch.completeWithError(error: error)
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
        // Now that we have the package, we do the actual lexical model installation
        // Marked here as prep for the next stage.

        do {
          try ResourceFileManager.shared.finalizePackageInstall(kmp, isCustom: false)
          log.info("successfully parsed the lexical model in: \(kmp.sourceFolder)")
          installedLexicalModel = kmp.models[0].installableLexicalModels[0]
        } catch {
          log.error("Error installing the lexical model: \(String(describing: error))")
        }

        // End of block to be spun off.
      } else {
        log.error("Provided package did not contain lexical models.")
      }
    } catch {
      log.error("Error extracting the lexical model from the package: \(String(describing: error))")
    }
    
    return (installedLexicalModel, kmp)
  }
}
