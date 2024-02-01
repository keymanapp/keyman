//
//  ResourceDownloadQueue.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 8/20/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation
import Reachability
import os.log

enum DownloadNode {
  case simpleBatch(AnyDownloadBatch)
  case compositeBatch(CompositeBatch)

  var packageKeys: [KeymanPackage.Key] {
    switch(self) {
      case .simpleBatch(let batch):
        return batch.packageKeys
      case .compositeBatch(let node):
        return node.packageKeys
    }
  }

  var errors: [Error?] {
    get {
      switch(self) {
        case .simpleBatch(let batch):
          return batch.errors
        case .compositeBatch(let node):
          return node.batchQueue.map{ $0.1 }
      }
    }
  }
}

protocol AnyDownloadTask {
  var request: HTTPDownloadRequest { get }
  var file: URL { get }
  var downloadFinalizationBlock: ((Bool) throws -> Void)? { get }
}

class DownloadTask: AnyDownloadTask {
  public final var request: HTTPDownloadRequest
  public var file: URL {
    if let file = finalFile {
      return file
    } else {
      return URL(fileURLWithPath: request.destinationFile!)
    }
  }

  public var finalFile: URL?

  public final var downloadFinalizationBlock: ((Bool) throws -> Void)? = nil
  
  public init(do request: HTTPDownloadRequest, forPackage packageKey: KeymanPackage.Key?) {
    self.request = request
  }

  public init(forPackageWithKey packageKey: KeymanPackage.Key,
               from url: URL,
               as destURL: URL, tempURL: URL) {

    let request = HTTPDownloadRequest(url: url, userInfo: [:])
    request.destinationFile = tempURL.path
    request.tag = 0

    self.finalFile = destURL
    self.request = request

    self.downloadFinalizationBlock = DownloadTask.resourceDownloadFinalizationClosure(tempURL: tempURL, finalURL: destURL)
  }


  /**
   * Supports downloading to a 'temp' file that is renamed once the download completes.
   */
  internal static func resourceDownloadFinalizationClosure(tempURL: URL,
                                                       finalURL: URL) -> ((Bool) throws -> Void) {
    return { success in
      // How to check for download failure
      if !success {
        if FileManager.default.fileExists(atPath: tempURL.path) {
          try? FileManager.default.removeItem(at: tempURL)
        }
      } else {
        try ResourceFileManager.shared.copyWithOverwrite(from: tempURL, to: finalURL)
        try? FileManager.default.removeItem(at: tempURL)
      }
    }
  }
}

enum DownloadActivityType {
  case download, update
}

protocol AnyDownloadBatch {
  var tasks: [AnyDownloadTask] { get }
  var packageKey: KeymanPackage.Key { get }

  // Needed for DownloadNode compliance.
  var packageKeys: [KeymanPackage.Key] { get }

  var startBlock: (() -> Void)? { get }
  var errors: [Error?] { get set }

  func completeWithCancellation() throws -> Void
  func completeWithError(error: Error) throws -> Void
  func completeWithPackage(fromKMP file: URL) throws -> Void
}

/**
 * Represents one overall resource-related command for requests against the Keyman Cloud API.
 */
class DownloadBatch<Package: KeymanPackage>: AnyDownloadBatch {
  typealias CompletionHandler = ResourceDownloadManager.CompletionHandler

  public final var downloadTasks: [DownloadTask]
  public let packageKey: KeymanPackage.Key
  var errors: [Error?] // Only used by the ResourceDownloadQueue.
  public final var startBlock: (() -> Void)? = nil
  public final var completionBlock: CompletionHandler<Package>? = nil
  
  public init(forPackageWithKey packageKey: KeymanPackage.Key,
              from url: URL,
              startBlock: (() -> Void)?,
              completionBlock: CompletionHandler<Package>?) {
    // If we can't build a proper DownloadTask, we can't build the batch.
    self.packageKey = packageKey
    let tempArtifact = ResourceFileManager.shared.packageDownloadTempPath(forKey: packageKey)
    let finalFile = ResourceFileManager.shared.cachedPackagePath(forKey: packageKey)

    let task = DownloadTask(forPackageWithKey: packageKey, from: url, as: finalFile, tempURL: tempArtifact)


    self.downloadTasks = [task]
    self.errors = Array(repeating: nil, count: 1) // We only build the one task.

    self.startBlock = startBlock
    self.completionBlock = completionBlock

    task.request.userInfo[Key.downloadTask] = task
    task.request.userInfo[Key.downloadBatch] = self
  }

  public var tasks: [AnyDownloadTask] {
    return downloadTasks
  }
  
  public var packageKeys: [KeymanPackage.Key] {
    get {
      return [ packageKey ]
    }
  }

  public func completeWithCancellation() throws {
    let complete = completionBlock
    completionBlock = nil
    try complete?(nil, nil)
  }

  public func completeWithError(error: Error) throws {
    let complete = completionBlock
    completionBlock = nil
    try complete?(nil, error)
  }

  public func completeWithPackage(fromKMP file: URL) throws {
    let complete = completionBlock
    completionBlock = nil

    do {
      if let package = try ResourceFileManager.shared.prepareKMPInstall(from: file) as? Package {
        try complete?(package, nil)
      } else {
        try complete?(nil, KMPError.invalidPackage)
      }
    } catch {
      try complete?(nil, error)
    }
  }
}

class CompositeBatch {
  public final var batchQueue: [(DownloadNode, Error?)]
  public final var startBlock: (() -> Void)? = nil
  public final var completionBlock: ResourceDownloadManager.InternalBatchCompletionHandler? = nil

  public init(queue: [DownloadNode],
              startBlock: (() -> Void)? = nil,
              completionBlock: ResourceDownloadManager.InternalBatchCompletionHandler? = nil) {
    self.batchQueue = queue.map { ($0, nil) }
    self.startBlock = startBlock
    self.completionBlock = completionBlock
  }

  public var tasks: [DownloadNode] {
    return batchQueue.map { $0.0 }
  }

  public var packageKeys: [KeymanPackage.Key] {
    return batchQueue.flatMap { $0.0.packageKeys }
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

/**
 * The other half of ResourceDownloadManager, this class is responsible for executing the downloads
 * and handling the results.  Internally manages its own single-threaded `DispatchQueue`, which
 * handles all necessary synchronization.
 *
 * At present, submissions to this class will be evaluated sequentially by its `DispatchQueue`, though
 * this may change at a later time.  (The 'sequential' aspect is due to legacy design decisions from
 * before concurrency was a consideration.)
 */
class ResourceDownloadQueue: HTTPDownloadDelegate {
  enum QueueState: String {
    case clear
    case noConnection

    var error: Error? {
      switch(self) {
        case .clear:
          return nil
        case .noConnection:
          return DownloadError.noInternet
      }
    }
  }

  // ---- CRITICAL SECTION FIELDS ----
  // Any writes to these must be performed on `queueThread`.
  // Async reads are permitted.
  private var queueRoot: DownloadQueueFrame
  private var queueStack: [DownloadQueueFrame]
  // ------------- END ---------------

  private var queueThread: DispatchQueue
  private var downloader: HTTPDownloader?
  private var reachability: Reachability?

  private let session: URLSession

  // Designed for use with testing.
  internal var autoExecute: Bool = true
  
  public convenience init() {
    self.init(session: URLSession.shared, autoExecute: true)
  }

  internal init(session: URLSession, autoExecute: Bool) {
    do {
      try reachability = Reachability(hostname: KeymanHosts.API_KEYMAN_COM.host!)
    } catch {
      let message = "Could not start Reachability object: \(error)"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
      SentryManager.capture(error, message: message)
    }

    self.session = session
    self.autoExecute = autoExecute

    queueRoot = DownloadQueueFrame()
    queueStack = [queueRoot] // queueRoot will always be the bottom frame of the stack.

    // Creates a single-threaded DispatchQueue, facilitating concurrency at this class's
    // critical sections.
    queueThread = DispatchQueue(label: "com.keyman.ResourceDownloadQueue")
  }
  
  public func hasConnection() -> Bool {
    return reachability?.connection != Reachability.Connection.unavailable
  }

  public var state: QueueState {
    guard hasConnection() else {
      return .noConnection
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

  // ---------- CRITICAL SECTION:  START -----------
  // IMPORTANT: These functions should only ever be referenced from `queueThread`.
  //            as they maintain synchronization for the queue's critical
  //            tracking fields.
  //
  // As a result, these functions are intentionally and explicitly `private`.
  private func finalizeCurrentBatch(withError error: Error? = nil) {
    let frame = queueStack[queueStack.count - 1]
    
    if queueStack.count == 1 {
      // Batches in the root may be safely dumped from the queue.
      // This automatically cleans out old top-level commands once done, instead of maintaining a persistent history.
      //
      // We _could_ keep the full history until the last command in the queue is done and empty the root's node list
      // when everything's done instead.  Might be worth a thought.
      frame.nodes.remove(at: 0)
    } else {
      if case let .compositeBatch(batch) = frame.batch {
        batch.batchQueue[frame.index].1 = error
      }
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
        // if-check below:  "if the current stack-frame came from a composite batch, let node = that 'composite batch'"
        if case .compositeBatch(let node) = frame.batch {
          // The base handler requires access to the batch's tracked success/failure data.
          let error = node.batchQueue.first(where: { $0.1 != nil })?.1
          finalizeCurrentBatch(withError: error)
          node.completionBlock?(node)
        } else {
          fatalError("Unexpected download queue state; cannot recover")
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

  // ----------- CRITICAL SECTION:  END ------------

  // The next two functions are used as queueThread's "API".
  // Internally, they must perform `sync` / `async` operations when
  // writing to the critical tracking fields.
  public func queue(_ node: DownloadNode) {
    // `sync` is particularly important here for some automated testing checks
    // when `autoExecute == `false`.
    queueThread.sync {
      self.queueRoot.nodes.append(node)
    }

    queueThread.async {
      // If the queue was empty before this...
      if self.queueRoot.nodes.count == 1 && self.autoExecute {
        self.executeNext()
      }
    }
  }

  internal func step() {
    queueThread.async { self.executeNext() }
  }

  // Exposes useful information for runtime mocking.  Used by some automated tests.
  internal func topLevelNodes() -> [DownloadNode] {
    return queueRoot.nodes
  }

  // MARK - helper methods for ResourceDownloadManager
  
  // TODO:  Eliminate this property coompletely.
  var currentRequest: HTTPDownloadRequest? {
    get {
      // This was literally the state of this property when the refactor producing this class was performed.
      return nil;
    }
  }

  func containsPackageKeyInQueue(matchingKey: KeymanPackage.Key) -> Bool {
    return queueRoot.nodes.contains { node in
      node.packageKeys.contains { fullID in
        return fullID == matchingKey
      }
    }
  }

  //MARK: - HTTPDownloadDelegate methods
  
  func downloadQueueFinished(_ queue: HTTPDownloader) {
    // We can use the properties of the current "batch" to generate specialized notifications.
    let batch = queue.userInfo[Key.downloadBatch] as! AnyDownloadBatch

    let packagePath = batch.tasks[0].file
    var err: Error? = nil
    do {
      try batch.completeWithPackage(fromKMP: packagePath)
    } catch {
      err = error
    }

    queueThread.async {
      if let error = err {
        self.finalizeCurrentBatch(withError: error)
      } else {
        // Completing the queue means having completed a batch.  We should only move forward in this class's
        // queue at this time, once a batch's task queue is complete.
        self.finalizeCurrentBatch()
      }

      if self.autoExecute {
        self.executeNext()
      }
    }
  }
  
  func downloadQueueCancelled(_ queue: HTTPDownloader) {
    if case .simpleBatch(let batch) = self.currentBatch {
      try? batch.completeWithCancellation()
    }

    queueThread.async {
      // In case we're part of a 'composite' operation, we should still keep the queue moving.
      self.finalizeCurrentBatch()

      if self.autoExecute {
        self.executeNext()
      }
    }
  }

  func downloadRequestStarted(_ request: HTTPDownloadRequest) {
    if let batch = request.userInfo[Key.downloadBatch] as? AnyDownloadBatch {
      batch.startBlock?()
    }
  }

  func downloadRequestFinished(_ request: HTTPDownloadRequest) {
    let task = request.userInfo[Key.downloadTask] as! AnyDownloadTask
    // Did we finish, but with an request error code?
    if request.responseStatusCode != 200 {
      // Possible request error (400 Bad Request, 404 Not Found, etc.)
      let error = DownloadError.failed(.responseCode(request.responseStatusCode ?? 400,
                                                     request.responseStatusMessage ?? "",
                                                     request.url))

      if case var .simpleBatch(batch) = currentFrame.batch {
        batch.errors[currentFrame.index] = error
      }
      
      // Now that we've synthesized an appropriate error instance, use the same handler
      // as for HTTPDownloader's 'failed' condition.
      downloadRequestFailed(request, with: error)

      // If we used a temp filename during download, resolve it.
      try? task.downloadFinalizationBlock?(false)
    } else {
      do {
        try task.downloadFinalizationBlock?(true)
      } catch {
        downloadRequestFailed(request, with: error)
      }
    }
  }

  func downloadRequestFailed(_ request: HTTPDownloadRequest, with error: Error?) {
    let task = request.userInfo[Key.downloadTask] as! AnyDownloadTask
    var batch = request.userInfo[Key.downloadBatch] as! AnyDownloadBatch

    /* Is a single-entry array for `DownloadBatch` and its type erasure.
     * CompositeBatch (the `currentFrame`, during resource updates)
     * maps these errors into an array.
     *
     * The index of a DownloadNode (in `tasks`) and its Error (in errors)
     * after this mapping will match.
     */
    batch.errors[0] = error

    var err: Error
    if let error = error {
      err = error
    } else {
      err = KeymanError.unknown
    }

    try? task.downloadFinalizationBlock?(false)
    try? batch.completeWithError(error: err)
    downloader!.cancelAllOperations()
  }
}
