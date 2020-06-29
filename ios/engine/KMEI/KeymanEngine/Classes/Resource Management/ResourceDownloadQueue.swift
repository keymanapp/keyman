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

  var resourceIDs: [AnyLanguageResourceFullID] {
    switch(self) {
      case .simpleBatch(let batch):
        return batch.resourceIDs
      case .compositeBatch(let node):
        return node.resourceIDs
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
  var request: HTTPDownloadRequest { get }
}

class DownloadTask<FullID: LanguageResourceFullID>: AnyDownloadTask {
  public final var resourceIDs: [FullID]?
  public final var request: HTTPDownloadRequest

  // TEMPORARY:  needed to support Cloud JS downloads.
  public final var resources: [FullID.Resource]?
  
  public init(do request: HTTPDownloadRequest, for resourceIDs: [FullID]?, resources: [FullID.Resource]? = nil) {
    self.request = request
    self.resourceIDs = resourceIDs
    self.resources = resources
  }

  public init(forPackageWithID fullID: FullID,
               from url: URL,
               at destURL: URL) {
    resourceIDs = [fullID]

    let request = HTTPDownloadRequest(url: url, userInfo: [:])
    request.destinationFile = destURL.path
    request.tag = 0

    self.request = request
  }
}

enum DownloadActivityType {
  case download, update
}

protocol AnyDownloadBatch {
  var tasks: [AnyDownloadTask] { get }
  var resourceIDs: [AnyLanguageResourceFullID] { get }

  var startBlock: (() -> Void)? { get }
  var errors: [Error?] { get set }

  func completeWithCancellation() -> Void
  func completeWithError(error: Error) -> Void
  func completeWithPackage(fromKMP file: URL) -> Void
}

/**
 * Represents one overall resource-related command for requests against the Keyman Cloud API.
 */
class DownloadBatch<FullID: LanguageResourceFullID>: AnyDownloadBatch where FullID.Resource.Package: TypedKeymanPackage<FullID.Resource> {
  typealias CompletionHandler = ResourceDownloadManager.CompletionHandler

  public final var downloadTasks: [DownloadTask<FullID>]
  var errors: [Error?] // Only used by the ResourceDownloadQueue.
  public final var startBlock: (() -> Void)? = nil
  public final var completionBlock: CompletionHandler<FullID.Resource>? = nil
  
  public init?(do tasks: [DownloadTask<FullID>],
               startBlock: (() -> Void)? = nil,
               completionBlock: CompletionHandler<FullID.Resource>? = nil) {
    self.downloadTasks = tasks

    self.errors = Array(repeating: nil, count: tasks.count)
    self.startBlock = startBlock
    self.completionBlock = completionBlock
  }

  public init(forPackageWithID fullID: FullID,
              from url: URL,
              startBlock: (() -> Void)?,
              completionBlock: CompletionHandler<FullID.Resource>?) {
    // If we can't build a proper DownloadTask, we can't build the batch.
    let tempArtifact = ResourceFileManager.shared.packageDownloadTempPath(forID: fullID)
    let finalFile = ResourceFileManager.shared.cachedPackagePath(forID: fullID)

    let task = DownloadTask(forPackageWithID: fullID, from: url, at: finalFile)


    self.downloadTasks = [task]
    self.errors = Array(repeating: nil, count: 1) // We only build the one task.

    self.startBlock = startBlock

    self.completionBlock = DownloadBatch.resourceDownloadFinalizeClosure(tempURL: tempArtifact, finalURL: finalFile, closure: completionBlock)
  }

  /**
   * Supports downloading to a 'temp' file that is renamed once the download completes.
   */
  internal static func resourceDownloadFinalizeClosure(tempURL: URL,
                                                finalURL: URL,
                                                closure: CompletionHandler<FullID.Resource>?)
                                                 -> CompletionHandler<FullID.Resource> {
    return { package, error in
      if let error = error {
        if FileManager.default.fileExists(atPath: tempURL.path) {
          try? FileManager.default.removeItem(at: tempURL)
        }

        closure?(nil, error)
      } else {
        do {
          try ResourceFileManager.shared.copyWithOverwrite(from: tempURL, to: finalURL)
          try? FileManager.default.removeItem(at: tempURL)
          closure?(package, nil)
        } catch {
          closure?(nil, error)
        }
      }
    }
  }

  public var tasks: [AnyDownloadTask] {
    return downloadTasks
  }
  
  public var resourceIDs: [AnyLanguageResourceFullID] {
    get {
      var resArray: [AnyLanguageResourceFullID] = []
      
      downloadTasks.forEach{ task in
        resArray.append(contentsOf: task.resourceIDs ?? [])
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
      if let package = try ResourceFileManager.shared.prepareKMPInstall(from: file) as? FullID.Resource.Package {
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
  public final var startBlock: (() -> Void)? = nil
  public final var completionBlock: ResourceDownloadManager.InternalBatchCompletionHandler? = nil

  public init(queue: [DownloadNode],
              startBlock: (() -> Void)? = nil,
              completionBlock: ResourceDownloadManager.InternalBatchCompletionHandler? = nil) {
    self.batches = queue
    self.errors = Array(repeating: nil, count: batches.count)

    self.startBlock = startBlock
    self.completionBlock = completionBlock
  }

  public var tasks: [DownloadNode] {
    return batches
  }

  public var resourceIDs: [AnyLanguageResourceFullID] {
    return batches.flatMap { $0.resourceIDs }
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

  private let session: URLSession

  // Designed for use with testing.
  internal var autoExecute: Bool = true
  
  public convenience init() {
    self.init(session: URLSession.shared, autoExecute: true)
  }

  internal init(session: URLSession, autoExecute: Bool) {
    do {
      try reachability = Reachability(hostname: keymanHostName)
    } catch {
      log.error("Could not start Reachability object: \(error)")
    }

    self.session = session
    self.autoExecute = autoExecute

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
        
        // Of course, this means we've "finished" a batch download.  We can use the same handlers as before.
        finalizeCurrentBatch()

        // if-check below:  "if the current stack-frame came from a composite batch, let node = that 'composite batch'"
        if case .compositeBatch(let node) = frame.batch {
          // The base handler requires access to the batch's tracked success/failure data.
          node.completionBlock?(node)
        }

        if autoExecute {
          executeNext()
        }
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
        downloader = HTTPDownloader(self, session: self.session)
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
        batch.startBlock?()
        // We're a batch composed of multiple sub-batches.  Time to set that up on the queue!
        let frame = DownloadQueueFrame(from: batch)!
        queueStack.append(frame)

        innerExecute(batch.tasks[0])
    }
  }
  
  public func queue(_ node: DownloadNode) {
    queueRoot.nodes.append(node)
    
    // If the queue was empty before this...
    if queueRoot.nodes.count == 1 && autoExecute {
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

  func containsResourceInQueue(matchingID: AnyLanguageResourceFullID, requireLanguageMatch: Bool = true) -> Bool {
    return queueRoot.nodes.contains { node in
      node.resourceIDs.contains { fullID in
        return fullID.matches(matchingID, requireLanguageMatch: requireLanguageMatch)
      }
    }
  }

  //MARK: - HTTPDownloadDelegate methods
  
  func downloadQueueFinished(_ queue: HTTPDownloader) {
    // We can use the properties of the current "batch" to generate specialized notifications.
    let batch = queue.userInfo[Key.downloadBatch] as! AnyDownloadBatch

    // Really, "if resource is installed from the cloud, only packaged locally"
    if let batch = batch as? DownloadBatch<InstallableKeyboard.FullID> {
      // batch.downloadTasks[0].request.destinationFile - currently, the downloaded keyboard .js.
      // Once downlading KMPs, will be the downloaded .kmp.

      // The request has succeeded.
      if downloader!.requestsCount == 0 { // Download queue finished.
        // TEMPORARY:  to get the full InstallableKeyboard as specified by the cloud.
        let keyboards = batch.downloadTasks[0].resources!
        log.info("Downloaded keyboard: \(keyboards[0].id).")

        // TEMP:  wrap the newly-downloaded resources with a kmp.json.
        //        Serves as a bridge until we're downloading actual .kmps for keyboards.
        let _ = Migrations.migrateToKMPFormat(keyboards)

        if let package: KeyboardKeymanPackage = ResourceFileManager.shared.getInstalledPackage(for: keyboards[0]) {
          batch.completionBlock?(package, nil)
        } else {
          log.error("Could not load metadata for newly-installed keyboard")
        }
      }
      // else "if resource is installed from an actual KMP"
      // - in other words, the long-term target.
    } else if let batch = batch as? DownloadBatch<InstallableLexicalModel.FullID> {
      let task = batch.downloadTasks[0] // It's always at this index.
      let packagePath = URL(fileURLWithPath: task.request.destinationFile!)
      batch.completeWithPackage(fromKMP: packagePath)
    }
    
    // Completing the queue means having completed a batch.  We should only move forward in this class's
    // queue at this time, once a batch's task queue is complete.
    finalizeCurrentBatch()

    if autoExecute {
      executeNext()
    }
  }
  
  func downloadQueueCancelled(_ queue: HTTPDownloader) {
    if case .simpleBatch(let batch) = self.currentBatch {
      batch.completeWithCancellation()
    }
    
    // In case we're part of a 'composite' operation, we should still keep the queue moving.
    finalizeCurrentBatch()

    if autoExecute {
      executeNext()
    }
  }

  func downloadRequestStarted(_ request: HTTPDownloadRequest) {
    if let batch = request.userInfo[Key.downloadBatch] as? AnyDownloadBatch {
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
}
