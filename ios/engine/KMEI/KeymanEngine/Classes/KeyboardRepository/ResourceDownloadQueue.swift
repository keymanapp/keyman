//
//  ResourceDownloadQueue.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 8/20/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation
import Reachability

private protocol DownloadNode { }

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
  
  public final var tasks: [DownloadTask]?
  public final var batchQueue: [DownloadBatch]?
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
    
    self.batchQueue = nil
  }
  
  public init(queue: [DownloadBatch]) {
    self.activity = .composite
    self.type = .other
    self.batchQueue = queue
    
    self.tasks = nil
  }
}

// This is a private class used internally by ResourceDownloadQueue to track progress through
// the queue and its DownloadBatches.  We need a stack scheme - each instance of this corresponds
// to one frame of the stack.
private class DownloadQueueFrame {
  public var nodes: [DownloadNode] = []
  public var index: Int = 0
  private(set) var isComposite = false
  
  public static func from(batch: DownloadBatch) {
    let frame = DownloadQueueFrame()
    
    if batch.activity == .composite {
      frame.isComposite = true
      frame.nodes.append(contentsOf: batch.batchQueue!)
    } else {
      frame.nodes.append(contentsOf: batch.tasks!)
    }
  }
}

// The other half of ResourceDownloadManager, this class is responsible for executing the downloads
// and handling the results.
class ResourceDownloadQueue: HTTPDownloadDelegate {
  private var queueRoot: DownloadQueueFrame?
  private var batchQueue: [DownloadQueueFrame] = []
  
  private var downloader: HTTPDownloader? = nil
  private var reachability: Reachability!
  private let keymanHostName = "api.keyman.com"
  
  public init() {
    reachability = Reachability(hostname: keymanHostName)
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
    guard downloader == nil else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "Download queue is busy"])
      downloadFailed(forBatch: batch, error: error)
      return false
    }
    
    return true
  }
  
  private func execute(_ batch: DownloadBatch) {
    // TODO:  Handling composite batches.
    downloader = HTTPDownloader(self)
    downloader!.userInfo = [Key.downloadBatch: batch]
    
    batch.tasks?.forEach { task in
      downloader!.addRequest(task.request)
    }
    
    //currentBatch = batch
    queueRoot = DownloadQueueFrame()
    queueRoot!.nodes.append(batch)
    downloader!.run()
  }
  
  public func queue(_ batch: DownloadBatch) {
    if queueRoot == nil {
      execute(batch)
    } else {
      queueRoot?.nodes.append(batch)
    }
  }
  
  // MARK - helper methods for ResourceDownloadManager
  
  var currentRequest: HTTPDownloadRequest? {
    get {
      // This was literally the state of this property when this refactor was performed.
      return nil;
    }
  }

  // Needs validation - I don't think this function is practically utilized (as of 2019-08-16)
  func keyboardIdForCurrentRequest() -> String? {
    if let currentRequest = currentRequest {
      let tmpStr = currentRequest.url.lastPathComponent
      if tmpStr.hasJavaScriptExtension {
        return String(tmpStr.dropLast(3))
      }
    } else if let downloadQueue = downloader {
      let kbInfo = downloadQueue.userInfo[Key.keyboardInfo]
      if let keyboards = kbInfo as? [InstallableKeyboard], let keyboard = keyboards.first {
        return keyboard.id
      }
    }
    return nil
  }
  
  func lexicalModelIdForCurrentRequest() -> String? {
    if let currentRequest = currentRequest {
      let tmpStr = currentRequest.url.lastPathComponent
      if tmpStr.hasJavaScriptExtension {
        return String(tmpStr.dropLast(3))
      }
    } else if let downloadQueue = downloader {
      let kbInfo = downloadQueue.userInfo[Key.lexicalModelInfo]
      if let lexicalModels = kbInfo as? [InstallableLexicalModel], let lexicalModel = lexicalModels.first {
        return lexicalModel.id
      }
    }
    return nil
  }

  // MARK - notification methods
  
  private func downloadFailed(forBatch batch: DownloadBatch, error: Error) {
    if batch.activity == .composite {
      // It's an update operation.
      // TODO:  Needs implementation.
    } else if batch.type == .keyboard {
      let keyboards = batch.tasks?.compactMap { task in
        return task.resources as? [InstallableKeyboard]
      }.flatMap {$0}
      downloadFailed(forKeyboards: keyboards ?? [], error: error)
    } else if batch.type == .lexicalModel {
      let lexModels = batch.tasks?.compactMap { task in
        return task.resources as? [InstallableLexicalModel]
      }.flatMap {$0}
      downloadFailed(forLanguageID: lexModels![0].languageID, error: error)
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
    queueRoot = nil
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
    
    // FIXME
    if let statusCode = request.responseStatusCode, statusCode == 200 {
      if task.type == .keyboard {
        // The request has succeeded.
        if downloader!.requestsCount == 0 {
          let keyboards = task.resources as? [InstallableKeyboard]

          // Download queue finished.
          downloader = nil
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
        
        // Temp - gotta clear the queue to match original behavior for now.
        if downloader!.requestsCount == 0 {
          downloader = nil
        }
      }
    } else { // Possible request error (400 Bad Request, 404 Not Found, etc.)
      downloader!.cancelAllOperations()
      downloader = nil

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
      downloader = nil
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
