//
//  ResourceDownloadManager.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 8/15/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation
import Reachability

private protocol DownloadNode { }

private class DownloadTask: DownloadNode {
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
private class DownloadBatch: DownloadNode {
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

public class ResourceDownloadManager: HTTPDownloadDelegate {
  private var queueRoot: DownloadQueueFrame?
  private var batchQueue: [DownloadQueueFrame] = []
  
  private var downloadQueue: HTTPDownloader? = nil
  var currentRequest: HTTPDownloadRequest?
  private var reachability: Reachability!
  private let keymanHostName = "api.keyman.com"

  private var updateKbdQueue: [InstallableKeyboard]? = nil
  private var updateLexQueue: [InstallableLexicalModel]? = nil
  private var _isDoneButtonEnabled = false
  private var isDidUpdateCheck = false
  
  public static let shared = ResourceDownloadManager()
  
  private init() {
    //downloadQueue = HTTPDownloader(self) // TODO:  Consider using a persistent one.
    reachability = Reachability(hostname: keymanHostName)
  }
  
  deinit {
    // Just to be safe, we'll invalidate any pending download requests when this class is deinitialized.
    if let currentRequest = currentRequest {
      currentRequest.userInfo["completionBlock"] = nil
    }
  }
  
  // MARK: Update checks + management
  
  public func updatesAvailable() -> Bool {
    if Manager.shared.apiKeyboardRepository.languages == nil && Manager.shared.apiLexicalModelRepository.languages == nil {
      return false
    }

    isDidUpdateCheck = true
    let userKeyboards = Storage.active.userDefaults.userKeyboards
    let hasKbdUpdate = userKeyboards?.contains { keyboard in
      let kbID = keyboard.id
      return stateForKeyboard(withID: kbID) == .needsUpdate
    } ?? false
    
    let userLexicalModels = Storage.active.userDefaults.userLexicalModels
    let hasLexUpdate = userLexicalModels?.contains { lexicalModel in
      let lmID = lexicalModel.id
      return stateForLexicalModel(withID: lmID) == .needsUpdate
    } ?? false
    
    // FIXME:  Testing only!  Forces 'update'.
    return true
    //return hasKbdUpdate || hasLexUpdate
  }
  
  // TODO:  Not yet ready.
  public func performUpdates() {
    // The plan is to create new notifications to handle batch updates here, rather than
    // require a UI to manage the update queue.
    updateKeyboards()
    updateLexicalModels()
  }
  
  private func updateKeyboards() {
    updateKbdQueue = []
    var kbIDs = Set<String>()
    
    // Build the keyboard update queue
    Storage.active.userDefaults.userKeyboards?.forEach { kb in
      let kbState = stateForKeyboard(withID: kb.id)
      if kbState == .needsUpdate {
        if(!kbIDs.contains(kb.id)) {
          kbIDs.insert(kb.id)
          updateKbdQueue?.append(kb)
        }
      }
    }

    // Execute the keyboard update queue
    if !updateKbdQueue!.isEmpty {
      let langID = updateKbdQueue![0].languageID
      let kbID = updateKbdQueue![0].id
      downloadKeyboard(withID: kbID, languageID: langID, isUpdate: true)
    }
  }

  private func updateLexicalModels() {
    // Build the lexical model update queue
    updateLexQueue = []
    var lmIDs = Set<String>()
    
    Storage.active.userDefaults.userLexicalModels?.forEach { lm in
      let lmState = stateForLexicalModel(withID: lm.id)
      if lmState == .needsUpdate {
        if !lmIDs.contains(lm.id) {
          lmIDs.insert(lm.id)
          updateLexQueue!.append(lm)
        }
      }
    }

    // Execute the lexical model update queue
    if !updateLexQueue!.isEmpty {
      let langID = updateLexQueue![0].languageID
      let lmID = updateLexQueue![0].id
      downloadLexicalModel(withID: lmID, languageID: langID, isUpdate: true)
    }
  }
  
  // MARK: - Common functionality
  
  private func checkCanExecute(_ batch: DownloadBatch) -> Bool {
    guard reachability.connection != Reachability.Connection.none else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "No internet connection"])
      downloadFailed(forBatch: batch, error: error)
      return false
    }
    
    // At this stage, we now have everything needed to generate download requests.
    guard downloadQueue == nil else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "Download queue is busy"])
      downloadFailed(forBatch: batch, error: error)
      return false
    }
    
    return true
  }
  
  private func executeDownloadBatch(_ batch: DownloadBatch) {
    // TODO:  Handling composite batches.
    downloadQueue = HTTPDownloader(self)
    downloadQueue!.userInfo = [Key.downloadBatch: batch]
    
    batch.tasks?.forEach { task in
      downloadQueue!.addRequest(task.request)
    }
    
    //currentBatch = batch
    queueRoot = DownloadQueueFrame()
    queueRoot!.nodes.append(batch)
    downloadQueue!.run()
  }
  
  private func queueDownloadBatch(_ batch: DownloadBatch) {
    if queueRoot == nil {
      executeDownloadBatch(batch)
    } else {
      queueRoot?.nodes.append(batch)
    }
  }
  
  private func fetchHandler(for resourceType: DownloadTask.Resource, _ completionHandler: @escaping () -> Void)
                            -> (_ error: Error?) -> Void {
    return { error in
      if let error = error {
        // TODO:  Connect to an error handler (or just render appropriate text) based on the resource type.
        self.downloadFailed(forKeyboards: [], error: error)
      } else {
        log.info("Fetched repository. Continuing with download.")
        completionHandler()
      }
    }
  }
  
  // MARK - Downloading keyboards
  
  private func getInstallableKeyboardMetadata(withID keyboardID: String, languageID: String) -> InstallableKeyboard? {
    // Grab info for the relevant API version of the keyboard.
    guard let keyboard = Manager.shared.apiKeyboardRepository.installableKeyboard(withID: keyboardID, languageID: languageID)
    else {
      let message = "Keyboard not found with id: \(keyboardID), languageID: \(languageID)"
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: message])
      downloadFailed(forKeyboards: [], error: error)
      return nil
    }
    
    return keyboard
  }
  
  // We return the batch instance to indicate success.  Also, in case we decide to implement Promises based on batch completion,
  // since this will expose the generated Promise for the caller's use.
  private func downloadKeyboardCore(withMetadata keyboards: [InstallableKeyboard], asActivity activity: DownloadBatch.Activity,
      withFilename filename: String, withOptions options: Options) -> DownloadBatch? {

    if let dlBatch = buildKeyboardDownloadBatch(for: keyboards[0], withFilename: filename, asActivity: activity, withOptions: options) {
      // We want to denote ALL language variants of a keyboard as part of the batch's metadata, even if we only download a single time.
      dlBatch.tasks?.forEach { task in
        task.resources = keyboards
      }
      
      // Perform common 'can download' check.  We need positive reachability and no prior download queue.
      // The parameter facilitates error logging.
      if !checkCanExecute(dlBatch) {
        return nil
      }
      
      queueDownloadBatch(dlBatch)
      return dlBatch
    }
    return nil
  }
  
  private func buildKeyboardDownloadBatch(for keyboard: InstallableKeyboard, withFilename filename: String,
                                          asActivity activity: DownloadBatch.Activity, withOptions options: Options) -> DownloadBatch? {
    let keyboardURL = options.keyboardBaseURL.appendingPathComponent(filename)
    let fontURLs = Array(Set(keyboardFontURLs(forFont: keyboard.font, options: options) +
                             keyboardFontURLs(forFont: keyboard.oskFont, options: options)))

    do {
      try FileManager.default.createDirectory(at: Storage.active.keyboardDir(forID: keyboard.id),
                                              withIntermediateDirectories: true)
    } catch {
      log.error("Could not create dir for download: \(error)")
      return nil
    }

    var request = HTTPDownloadRequest(url: keyboardURL, userInfo: [:])
    request.destinationFile = Storage.active.keyboardURL(forID: keyboard.id, version: keyboard.version).path
    request.tag = 0

    let keyboardTask = DownloadTask(do: request, for: [keyboard], type: .keyboard)
    var batchTasks: [DownloadTask] = [ keyboardTask ]
    
    for (i, url) in fontURLs.enumerated() {
      request = HTTPDownloadRequest(url: url, userInfo: [:])
      request.destinationFile = Storage.active.fontURL(forKeyboardID: keyboard.id, filename: url.lastPathComponent).path
      request.tag = i + 1
      
      let fontTask = DownloadTask(do: request, for: nil, type: .other)
      batchTasks.append(fontTask)
    }
    
    let batch = DownloadBatch(do: batchTasks, as: activity, ofType: .keyboard)
    batch!.tasks?.forEach { task in
      task.request.userInfo[Key.downloadBatch] = batch
      task.request.userInfo[Key.downloadTask] = task
    }
    
    return batch
  }

  /// Asynchronously fetches the .js file for the keyboard with given IDs.
  /// See `Notifications` for notification on success/failiure.
  /// - Parameters:
  ///   - isUpdate: Keep the keyboard files on failure
  ///   - fetchRepositoryIfNeeded: Fetch the list of keyboards from the API if necessary.
  public func downloadKeyboard(withID keyboardID: String,
                               languageID: String,
                               isUpdate: Bool,
                               fetchRepositoryIfNeeded: Bool = true) {
    guard let _ = Manager.shared.apiKeyboardRepository.keyboards,
      let options = Manager.shared.apiKeyboardRepository.options
    else {
      if fetchRepositoryIfNeeded {
        log.info("Fetching repository from API for keyboard download")
        Manager.shared.apiKeyboardRepository.fetch(completionHandler: fetchHandler(for: .keyboard) {
          self.downloadKeyboard(withID: keyboardID, languageID: languageID, isUpdate: isUpdate, fetchRepositoryIfNeeded: false)
        })
        return
      } else {
        let message = "Keyboard repository not yet fetched"
        let error = NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: message])
        downloadFailed(forKeyboards: [], error: error)
        return
      }
    }

    // Grab info for the relevant API version of the keyboard.
    guard let keyboard = getInstallableKeyboardMetadata(withID: keyboardID, languageID: languageID),
      let filename = Manager.shared.apiKeyboardRepository.keyboards?[keyboardID]?.filename
    else {
      return
    }
    
    if let _ = downloadKeyboardCore(withMetadata: [keyboard], asActivity: isUpdate ? .update : .download, withFilename: filename, withOptions: options) {
      self.downloadLexicalModelsForLanguageIfExists(languageID: languageID)
    }
  }

  private func keyboardFontURLs(forFont font: Font?, options: Options) -> [URL] {
    guard let font = font else {
      return []
    }
    return font.source.filter({ $0.hasFontExtension })
      .map({ options.fontBaseURL.appendingPathComponent($0) })
  }
  
  /// Downloads a custom keyboard from the URL (old ad-hoc method)
  /// - Parameters:
  ///   - url: URL to a JSON description of the keyboard
  public func downloadKeyboard(from url: URL) {
    guard reachability.connection != Reachability.Connection.none else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "No connection"])
      downloadFailed(forKeyboards: [], error: error)
      return
    }

    guard let data = try? Data(contentsOf: url) else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "Failed to fetch JSON file"])
      downloadFailed(forKeyboards: [], error: error)
      return
    }
    
    decodeKeyboardData(data, decodingStrategy: .ios8601WithFallback)
  }

  // Step 2 of old ad-hoc process.  May be worth preserving for use with .kmp packages.
  private func decodeKeyboardData(_ data: Data, decodingStrategy : JSONDecoder.DateDecodingStrategy) {
    let decoder = JSONDecoder()
    decoder.dateDecodingStrategy = decodingStrategy
  
    if let keyboard = try? decoder.decode(KeyboardAPICall.self, from: data) {
      downloadKeyboard(keyboard)
    } else {
      decoder.dateDecodingStrategy = .iso8601WithoutTimezone
      if let keyboard = try? decoder.decode(KeyboardAPICall.self, from: data) {
        downloadKeyboard(keyboard)
      } else {
        decoder.dateDecodingStrategy = .ios8601WithMilliseconds
        do {
          let keyboard = try decoder.decode(KeyboardAPICall.self, from: data)
          downloadKeyboard(keyboard)
        } catch {
          downloadFailed(forKeyboards: [], error: error)
        }
      }
    }
  }
  
  // Step 3 of the old ad-hoc process.
  /// Assumes that Keyboard has font and oskFont set and ignores fonts contained in Language.
  private func downloadKeyboard(_ keyboardAPI: KeyboardAPICall) {
    let keyboard = keyboardAPI.keyboard
    let installableKeyboards = keyboard.languages!.map { language in
      InstallableKeyboard(keyboard: keyboard, language: language, isCustom: true)
    }

    let filename = keyboard.filename
    let isUpdate = Storage.active.userDefaults.userKeyboards?.contains { $0.id == keyboard.id } ?? false

    _ = downloadKeyboardCore(withMetadata: installableKeyboards, asActivity: isUpdate ? .update : .download, withFilename: filename, withOptions: keyboardAPI.options)
  }
  
  // TODO:  Needs an update!
  /// - Returns: The current state for a keyboard
  public func stateForKeyboard(withID keyboardID: String) -> KeyboardState {
    // Needs validation - I don't think this if-condition can be met in Keyman's current state
    // (as of 2019-08-16)
    if keyboardIdForCurrentRequest() == keyboardID {
      return .downloading
    }
    let userKeyboards = Storage.active.userDefaults.userKeyboards
    guard let userKeyboard = userKeyboards?.first(where: { $0.id == keyboardID }) else {
      return .needsDownload
    }

    // Check version
    if let repositoryVersionString = Manager.shared.apiKeyboardRepository.keyboards?[keyboardID]?.version {
      let downloadedVersion = Version(userKeyboard.version) ?? Version.fallback
      let repositoryVersion = Version(repositoryVersionString) ?? Version.fallback
      if downloadedVersion < repositoryVersion {
        return .needsUpdate
      }
    }
    return .upToDate
  }

  // Needs validation - I don't think this function is practically utilized (as of 2019-08-16)
  func keyboardIdForCurrentRequest() -> String? {
    if let currentRequest = currentRequest {
      let tmpStr = currentRequest.url.lastPathComponent
      if tmpStr.hasJavaScriptExtension {
        return String(tmpStr.dropLast(3))
      }
    } else if let downloadQueue = downloadQueue {
      let kbInfo = downloadQueue.userInfo[Key.keyboardInfo]
      if let keyboards = kbInfo as? [InstallableKeyboard], let keyboard = keyboards.first {
        return keyboard.id
      }
    }
    return nil
  }

  // MARK - Lexical models

  private func getInstallableLexicalModelMetadata(withID lexicalModelID: String, languageID: String) -> InstallableLexicalModel? {
    // Grab info for the relevant API version of the keyboard.
    guard let keyboard = Manager.shared.apiLexicalModelRepository.installableLexicalModel(withID: lexicalModelID, languageID: languageID)
    else {
      let message = "Lexical model not found with id: \(lexicalModelID), languageID: \(languageID)"
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: message])
      // TODO: better error target.
      downloadFailed(forLanguageID: "", error: error)
      return nil
    }
    
    return keyboard
  }
  
  // We return the batch instance to indicate success.  Also, in case we decide to implement Promises based on batch completion,
  // since this will expose the generated Promise for the caller's use.
  private func downloadLexicalModelCore(withMetadata lexicalModels: [InstallableLexicalModel], asActivity activity: DownloadBatch.Activity,
      fromPath path: URL) -> DownloadBatch? {

    if let dlBatch = buildLexicalModelDownloadBatch(for: lexicalModels[0], fromPath: path, asActivity: activity) {
      // We want to denote ALL language variants of a keyboard as part of the batch's metadata, even if we only download a single time.
      dlBatch.tasks?.forEach { task in
        task.resources = lexicalModels
      }
      
      // Perform common 'can download' check.  We need positive reachability and no prior download queue.
      // The parameter facilitates error logging.
      if !checkCanExecute(dlBatch) {
        return nil
      }
      
      queueDownloadBatch(dlBatch)
      return dlBatch
    }
    return nil
  }
  
  private func buildLexicalModelDownloadBatch(for lexicalModel: InstallableLexicalModel, fromPath path: URL,
      asActivity activity: DownloadBatch.Activity) -> DownloadBatch? {
    do {
      try FileManager.default.createDirectory(at: Storage.active.lexicalModelDir(forID: lexicalModel.id),
                                              withIntermediateDirectories: true)
    } catch {
      log.error("Could not create dir for download: \(error)")
      return nil
    }

    let request = HTTPDownloadRequest(url: path, userInfo: [:])
    request.destinationFile = Storage.active.lexicalModelPackageURL(forID: lexicalModel.id, version: lexicalModel.version).path
    request.tag = 0

    let lexicalModelTask = DownloadTask(do: request, for: [lexicalModel], type: .lexicalModel)
    let batchTasks: [DownloadTask] = [ lexicalModelTask ]
    
    let batch = DownloadBatch(do: batchTasks, as: activity, ofType: .keyboard)
    batch!.tasks?.forEach { task in
      task.request.userInfo[Key.downloadBatch] = batch
      task.request.userInfo[Key.downloadTask] = task
    }
    
    return batch
  }
  
  // Can be called by the cloud keyboard downloader and utilized.
  
  /// Starts the process of fetching the package file of the lexical model for the given language ID
  ///   first it fetches the list of lexical models for the given language
  ///   then it takes the first of the list and download the KMP package file and asks the app to open it (like adhoc download)
  /// - Parameters:
  ///   - languageID: the bcp47 string of the desired language
  public func downloadLexicalModelsForLanguageIfExists(languageID: String) {
    // TODO:  This fetch will conflict with the fetch in the next method; we need some scheme to reset
    //        the other's fetch after this completes.
    //
    //        It _may_ be better to retool how this looks up the lexical model for a language.  Not sure yet.
  
    //get list of lexical models for this languageID  /?q=bcp47:en
    func listCompletionHandler(lexicalModels: [LexicalModel]?, error: Error?) -> Void {
      if let error = error {
        log.info("Failed to fetch lexical model list for "+languageID+". error: "+(error as! String))
        self.downloadFailed(forLanguageID: languageID, error: error) //???forKeyboards
      } else if nil == lexicalModels {
        //TODO: put up an alert instead
        log.info("No lexical models available for language \(languageID) (nil)")
      } else if 0 == lexicalModels?.count {
        log.info("No lexical models available for language \(languageID) (empty)")
      } else {
        log.info("Fetched lexical model list for "+languageID+".")
        // choose which of the lexical models to download
        //  for now, this just downloads the first one
        let chosenIndex = 0
        if let lexicalModel = lexicalModels?[chosenIndex] {
          //downloadLexicalModelPackage(url: URL.init(string: lexicalModel.packageFilename)!)
          // We've already fetched part of the repository to do this.
          downloadLexicalModel(withID: lexicalModel.id, languageID: languageID, isUpdate: false, fetchRepositoryIfNeeded: false)
        } else {
          log.info("no error, but no lexical model in list, either!")
        }
      }
    }
    
    Manager.shared.apiLexicalModelRepository.fetchList(languageID: languageID, completionHandler: listCompletionHandler)
  }

  /// Asynchronously fetches the .js file for the lexical model with given IDs.
  /// See `Notifications` for notification on success/failiure.
  /// - Parameters:
  ///   - isUpdate: Keep the lexical model files on failure
  ///   - fetchRepositoryIfNeeded: Fetch the list of lexical models from the API if necessary.
  public func downloadLexicalModel(withID lexicalModelID: String,
                                   languageID: String,
                                   isUpdate: Bool,
                                   fetchRepositoryIfNeeded: Bool = true) {
    
    // TODO:  We should always force a refetch after new keyboards are installed so we can redo our language queries.
    //        That should probably be done on successful keyboard installs, not here, though.
    if fetchRepositoryIfNeeded {
      // A temp measure to make sure things aren't totally broken.  Definitely not optimal.
      Manager.shared.apiLexicalModelRepository.fetch(completionHandler: nil)
    }
    
    guard let _ = Manager.shared.apiLexicalModelRepository.lexicalModels else {
      if fetchRepositoryIfNeeded {
        log.info("Fetching repository from API for keyboard download")
        Manager.shared.apiLexicalModelRepository.fetch(completionHandler: fetchHandler(for: .keyboard) {
          self.downloadKeyboard(withID: lexicalModelID, languageID: languageID, isUpdate: isUpdate, fetchRepositoryIfNeeded: false)
        })
        return
      } else {
        let message = "Keyboard repository not yet fetched"
        let error = NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: message])
        downloadFailed(forKeyboards: [], error: error)
        return
      }
    }

    // Grab info for the relevant API version of the keyboard.
    guard let lexicalModel = getInstallableLexicalModelMetadata(withID: lexicalModelID, languageID: languageID),
      let filename = Manager.shared.apiLexicalModelRepository.lexicalModels?[lexicalModelID]?.packageFilename
    else {
      return
    }
    
    _ = downloadLexicalModelCore(withMetadata: [lexicalModel], asActivity: isUpdate ? .update : .download, fromPath: URL.init(string: filename)!)
  }
  
  /// Downloads a custom lexical model from the URL
  /// - Parameters:
  ///   - url: URL to a JSON description of the lexical model
  public func downloadLexicalModel(from url: URL) {
    guard reachability.connection != Reachability.Connection.none else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "No connection"])
      downloadFailed(forKeyboards: [], error: error) //??? forLexicalModels
      return
    }
    
    guard let data = try? Data(contentsOf: url) else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "Failed to fetch JSON file"])
      downloadFailed(forKeyboards: [], error: error) //??? forLexicalModels
      return
    }
    
    decodeLexicalModelData(data, decodingStrategy: .ios8601WithFallback)
  }
  
  // This should be usable as part of the .model.kmp processing process; we have a kmp.json, after all.
  private func decodeLexicalModelData(_ data: Data, decodingStrategy : JSONDecoder.DateDecodingStrategy) {
    let decoder = JSONDecoder()
    decoder.dateDecodingStrategy = decodingStrategy
    
    if let lexicalModel = try? decoder.decode(LexicalModelAPICall.self, from: data) {
      downloadLexicalModel(lexicalModel)
    } else {
      decoder.dateDecodingStrategy = .iso8601WithoutTimezone
      if let lexicalModel = try? decoder.decode(LexicalModelAPICall.self, from: data) {
        downloadLexicalModel(lexicalModel)
      } else {
        decoder.dateDecodingStrategy = .ios8601WithMilliseconds
        do {
          let lexicalModel = try decoder.decode(LexicalModelAPICall.self, from: data)
          downloadLexicalModel(lexicalModel)
        } catch {
          downloadFailed(forKeyboards: [], error: error) //??? forLexicalModels
        }
      }
    }
  }
  
  // Accordingly, this could be retooled to help with .model.kmp files too.
  private func downloadLexicalModel(_ lexicalModelAPI: LexicalModelAPICall) {
    let lexicalModel = lexicalModelAPI.lexicalModels[0]
    let installableLexicalModels = lexicalModel.languages.map { language in
      InstallableLexicalModel(lexicalModel: lexicalModel, languageID: language, isCustom: true)
    }
    
    let packageFilename = lexicalModel.packageFilename
    let lexicalModelURL = URL(string: "https://api.keyman.com/model")!.appendingPathComponent(packageFilename)
    
    do {
      try FileManager.default.createDirectory(at: Storage.active.lexicalModelDir(forID: lexicalModel.id),
                                              withIntermediateDirectories: true)
    } catch {
      log.error("Could not create dir for download: \(error)")
      return
    }
    
    let isUpdate = Storage.active.userDefaults.userLexicalModels?.contains { $0.id == lexicalModel.id } ?? false
    
    if let batch = buildLexicalModelDownloadBatch(for: installableLexicalModels[0], fromPath: lexicalModelURL, asActivity: isUpdate ? .update : .download) {
      if !checkCanExecute(batch) {
        return
      }

      queueDownloadBatch(batch)
    }
  }
  
  /// - Returns: The current state for a lexical model
  //TODO: rename KeyboardState to ResourceState? so it can be used with both keybaoards and lexical models without confusion
  public func stateForLexicalModel(withID lexicalModelID: String) -> KeyboardState {
    if lexicalModelIdForCurrentRequest() == lexicalModelID {
      return .downloading
    }
    let userLexicalModels = Storage.active.userDefaults.userLexicalModels
    guard let userLexicalModel = userLexicalModels?.first(where: { $0.id == lexicalModelID }) else {
      return .needsDownload
    }
    
    // Check version
    if let repositoryVersionString = Manager.shared.apiLexicalModelRepository.lexicalModels?[lexicalModelID]?.version {
      let downloadedVersion = Version(userLexicalModel.version) ?? Version.fallback
      let repositoryVersion = Version(repositoryVersionString) ?? Version.fallback
      if downloadedVersion < repositoryVersion {
        return .needsUpdate
      }
    }
    return .upToDate
  }
  
  func lexicalModelIdForCurrentRequest() -> String? {
    if let currentRequest = currentRequest {
      let tmpStr = currentRequest.url.lastPathComponent
      if tmpStr.hasJavaScriptExtension {
        return String(tmpStr.dropLast(3))
      }
    } else if let downloadQueue = downloadQueue {
      let kbInfo = downloadQueue.userInfo[Key.lexicalModelInfo]
      if let lexicalModels = kbInfo as? [InstallableLexicalModel], let lexicalModel = lexicalModels.first {
        return lexicalModel.id
      }
    }
    return nil
  }
  
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
  
  
  
  // MARK - deprecated helper/handler methods - they help avoid errors for now.
  
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
  
  private func downloadFailed(forKeyboards keyboards: [InstallableKeyboard], error: Error) {
    let notification = KeyboardDownloadFailedNotification(keyboards: keyboards, error: error)
    NotificationCenter.default.post(name: Notifications.keyboardDownloadFailed,
                                    object: self,
                                    value: notification)
  }
  
  private func downloadFailed(forLanguageID languageID: String, error: Error) {
    let notification = LexicalModelDownloadFailedNotification(lmOrLanguageID: languageID, error: error)
    NotificationCenter.default.post(name: Notifications.lexicalModelDownloadFailed,
                                    object: self,
                                    value: notification)
  }
  
  private func downloadFailed(forLexicalModelPackage packageURL: String, error: Error) {
    let notification = LexicalModelDownloadFailedNotification(lmOrLanguageID: packageURL, error: error)
    NotificationCenter.default.post(name: Notifications.lexicalModelDownloadFailed,
                                    object: self,
                                    value: notification)
  }
  
  private func downloadSucceeded(forLexicalModel lm: InstallableLexicalModel) {
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
        if downloadQueue!.requestsCount == 0 {
          let keyboards = task.resources as? [InstallableKeyboard]

          // Download queue finished.
          downloadQueue = nil
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
        if downloadQueue!.requestsCount == 0 {
          downloadQueue = nil
        }
      }
    } else { // Possible request error (400 Bad Request, 404 Not Found, etc.)
      downloadQueue!.cancelAllOperations()
      downloadQueue = nil

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
      downloadQueue = nil
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
  // End REWORK THIS SECTION ------
}
