//
//  ResourceDownloadManager.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 8/15/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation
import Reachability

private class DownloadTask {
  public enum Resource {
    case keyboard, lexicalModel, other
  }
  
  public final var resource: Resource
  public final var resourceID: String
  public final var lgID: String
  public final var request: HTTPDownloadRequest
  
  public init(do request: HTTPDownloadRequest, for resourceType: DownloadTask.Resource, resID: String, lgID: String) {
    self.resource = resourceType
    self.resourceID = resID
    self.lgID = lgID
    self.request = request
  }
}

/**
 * Represents one overall resource-related command for requests against the Keyman Cloud API.
 */
private class DownloadBatch {
  public enum Activity {
    case download, update
  }
  
  /*
   * Three main cases so far:
   * - [.download, .keyboard]:      download new Keyboard (possibly with an associated lexical model)
   * - [.download, .lexicalModel]:  download new LexicalModel
   * - [.update,   -don't-care-]:   batch update of all resources.
   */
  public final var activity: Activity
  public final var resource: DownloadTask.Resource
  
  public final var tasks: [DownloadTask]
  //public final var promise: Int
  
  public init(do tasks: [DownloadTask], as activity: Activity, ofType resource: DownloadTask.Resource) {
    self.activity = activity
    self.resource = resource
    self.tasks = tasks
  }
}

public class ResourceDownloadManager: HTTPDownloadDelegate {
  private var activeTask: DownloadTask?
  private var taskQueue: [DownloadTask] = []
  
  private var downloadQueue: HTTPDownloader? = nil
  var currentRequest: HTTPDownloadRequest?
  private var reachability: Reachability!
  private let keymanHostName = "api.keyman.com"

  private var updateKbdQueue: [InstallableKeyboard]? = nil
  private var updateLexQueue: [InstallableLexicalModel]? = nil
  private var _isDoneButtonEnabled = false
  private var isDidUpdateCheck = false
  
//  private var keyboardDownloadStartedObserver: NotificationObserver?
//  private var keyboardDownloadCompletedObserver: NotificationObserver?
//  private var keyboardDownloadFailedObserver: NotificationObserver?
//  private var lexicalModelDownloadStartedObserver: NotificationObserver?
//  private var lexicalModelDownloadCompletedObserver: NotificationObserver?
//  private var lexicalModelDownloadFailedObserver: NotificationObserver?
  
  public static let shared = ResourceDownloadManager()
  
  private init() {
    //downloadQueue = HTTPDownloader(self) // TODO:  Consider using a persistent one.
    reachability = Reachability(hostname: keymanHostName)
    
//    keyboardDownloadStartedObserver = NotificationCenter.default.addObserver(
//      forName: Notifications.keyboardDownloadStarted,
//      observer: self,
//      function: ResourceDownloadManager.keyboardDownloadStarted)
//    keyboardDownloadCompletedObserver = NotificationCenter.default.addObserver(
//      forName: Notifications.keyboardDownloadCompleted,
//      observer: self,
//      function: ResourceDownloadManager.keyboardDownloadCompleted)
//    keyboardDownloadFailedObserver = NotificationCenter.default.addObserver(
//      forName: Notifications.keyboardDownloadFailed,
//      observer: self,
//      function: ResourceDownloadManager.keyboardDownloadFailed)
//
//    lexicalModelDownloadStartedObserver = NotificationCenter.default.addObserver(
//      forName: Notifications.lexicalModelDownloadStarted,
//      observer: self,
//      function: ResourceDownloadManager.lexicalModelDownloadStarted)
//    lexicalModelDownloadCompletedObserver = NotificationCenter.default.addObserver(
//      forName: Notifications.lexicalModelDownloadCompleted,
//      observer: self,
//      function: ResourceDownloadManager.lexicalModelDownloadCompleted)
//    lexicalModelDownloadFailedObserver = NotificationCenter.default.addObserver(
//      forName: Notifications.lexicalModelDownloadFailed,
//      observer: self,
//      function: ResourceDownloadManager.lexicalModelDownloadFailed)
  }
  
  deinit {
    // Just to be safe, we'll invalidate any pending download requests when this class is deinitialized.
    if let currentRequest = currentRequest {
      currentRequest.userInfo["completionBlock"] = nil
    }
  }
  
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
  
  public func performUpdates() {
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
  
  // MARK: - Downloading keyboards and lexical models

  /// Asynchronously fetches the .js file for the keyboard with given IDs.
  /// See `Notifications` for notification on success/failiure.
  /// - Parameters:
  ///   - isUpdate: Keep the keyboard files on failure
  ///   - fetchRepositoryIfNeeded: Fetch the list of keyboards from the API if necessary.
  public func downloadKeyboard(withID keyboardID: String,
                               languageID: String,
                               isUpdate: Bool,
                               fetchRepositoryIfNeeded: Bool = true) {
    guard let keyboards = Manager.shared.apiKeyboardRepository.keyboards,
      let options = Manager.shared.apiKeyboardRepository.options
    else {
      if fetchRepositoryIfNeeded {
        log.info("Fetching repository from API for keyboard download")
        Manager.shared.apiKeyboardRepository.fetch { error in
          if let error = error {
            self.downloadFailed(forKeyboards: [], error: error)
          } else {
            log.info("Fetched repository. Continuing with keyboard download.")
            self.downloadKeyboard(withID: keyboardID,
                                  languageID: languageID,
                                  isUpdate: isUpdate,
                                  fetchRepositoryIfNeeded: false)
          }
        }
        return
      }
      let message = "Keyboard repository not yet fetched"
      let error = NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: message])
      downloadFailed(forKeyboards: [], error: error)
      return
    }

    // Grab info for the relevant API version of the keyboard.
    guard let keyboard = Manager.shared.apiKeyboardRepository.installableKeyboard(withID: keyboardID, languageID: languageID),
      let filename = keyboards[keyboardID]?.filename
    else {
      let message = "Keyboard not found with id: \(keyboardID), languageID: \(languageID)"
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: message])
      downloadFailed(forKeyboards: [], error: error)
      return
    }

    guard reachability.connection != Reachability.Connection.none else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "No internet connection"])
      downloadFailed(forKeyboards: [keyboard], error: error)
      return
    }
    
    // At this stage, we now have everything needed to generate download requests.
    guard downloadQueue == nil else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "Download queue is busy"])
      downloadFailed(forKeyboards: [keyboard], error: error)
      return
    }

    // TODO: Better typing
    let commonUserData: [String: Any] = [
      Key.keyboardInfo: [keyboard],
      Key.update: isUpdate
    ]

    let dlBatch = buildKeyboardDownloadBatch(for: keyboard, withOptions: options, withFilename: filename, asActivity: .download, with: commonUserData)

    downloadQueue = HTTPDownloader(self)
    downloadQueue!.userInfo = commonUserData
    
    dlBatch?.tasks.forEach { task in
      downloadQueue!.addRequest(task.request)
    }
    
    downloadQueue!.run()
    
    self.downloadLexicalModelsForLanguageIfExists(languageID: languageID)
  }

  private func keyboardFontURLs(forFont font: Font?, options: Options) -> [URL] {
    guard let font = font else {
      return []
    }
    return font.source.filter({ $0.hasFontExtension })
      .map({ options.fontBaseURL.appendingPathComponent($0) })
  }
  
  /// Assumes that Keyboard has font and oskFont set and ignores fonts contained in Language.
  private func downloadKeyboard(_ keyboardAPI: KeyboardAPICall) {
    let keyboard = keyboardAPI.keyboard
    let installableKeyboards = keyboard.languages!.map { language in
      InstallableKeyboard(keyboard: keyboard, language: language, isCustom: true)
    }

    let filename = keyboard.filename

    if downloadQueue != nil {
      // Download queue is active.
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "Download queue is busy"])
      downloadFailed(forKeyboards: installableKeyboards, error: error)
      return
    }

    if reachability.connection == Reachability.Connection.none {
      let error = NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: "No internet connection"])
      downloadFailed(forKeyboards: installableKeyboards, error: error)
      return
    }

    do {
      try FileManager.default.createDirectory(at: Storage.active.keyboardDir(forID: keyboard.id),
                                              withIntermediateDirectories: true)
    } catch {
      log.error("Could not create dir for download: \(error)")
      return
    }

    let isUpdate = Storage.active.userDefaults.userKeyboards?.contains { $0.id == keyboard.id } ?? false

    downloadQueue = HTTPDownloader(self)
    let commonUserData: [String: Any] = [
      Key.keyboardInfo: installableKeyboards,
      Key.update: isUpdate
    ]
    downloadQueue!.userInfo = commonUserData

    // We're only installing a single keyboard, even if for multiple languages.  We should only do the actual 'download' task once.
    let dlBatch = buildKeyboardDownloadBatch( for: installableKeyboards[0], withOptions: keyboardAPI.options, withFilename: filename,
                                              asActivity: .download, with: commonUserData)

    downloadQueue = HTTPDownloader(self)
    downloadQueue!.userInfo = commonUserData
    
    dlBatch?.tasks.forEach { task in
      downloadQueue!.addRequest(task.request)
    }
    
    downloadQueue!.run()
  }
  
  private func buildKeyboardDownloadBatch(for keyboard: InstallableKeyboard, withOptions options: Options, withFilename filename: String,
                                          asActivity activity: DownloadBatch.Activity, with userInfo: [String:Any]) -> DownloadBatch? {
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

    var request = HTTPDownloadRequest(url: keyboardURL, userInfo: userInfo)
    request.destinationFile = Storage.active.keyboardURL(forID: keyboard.id, version: keyboard.version).path
    request.tag = 0

    let keyboardTask = DownloadTask(do: request, for: .keyboard, resID: keyboard.id, lgID: keyboard.languageID)
    var batchTasks: [DownloadTask] = [ keyboardTask ]
    
    for (i, url) in fontURLs.enumerated() {
      request = HTTPDownloadRequest(url: url, userInfo: userInfo)
      request.destinationFile = Storage.active.fontURL(forKeyboardID: keyboard.id, filename: url.lastPathComponent).path
      request.tag = i + 1
      
      let fontTask = DownloadTask(do: request, for: .other, resID: keyboard.id, lgID: keyboard.languageID)
      batchTasks.append(fontTask)
    }
    
    let batch = DownloadBatch(do: batchTasks, as: activity, ofType: .keyboard)
    return batch
  }

  /// Downloads a custom keyboard from the URL
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

  /// - Returns: The current state for a keyboard
  public func stateForKeyboard(withID keyboardID: String) -> KeyboardState {
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
  
  // return a lexical model so caller can use it in a downloadSucceeded call
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
  
  func downloadLexicalModelPackage(string lexicalModelPackageURLString: String) -> Void {
    if let lexicalModelKMPURL = URL.init(string: lexicalModelPackageURLString) {
      //determine where to put the data (local  file  URL)
      var destinationUrl = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
      destinationUrl.appendPathComponent("\(lexicalModelKMPURL.lastPathComponent).zip")
      //callback to handle the data downloaded
      func lexicalModelDownloaded(data: Data?,
                                  response: URLResponse?,
                                  dest: URL,
                                  error: Error?) {
        if let error = error {
          log.error("Failed to fetch lexical model KMP file")
          downloadFailed(forLexicalModelPackage: lexicalModelPackageURLString, error: error)
        } else {
          do {
            try data!.write(to: dest)
          } catch {
            log.error("Error writing the lexical model download data: \(error)")
          }
          if let lm = installLexicalModelPackage(downloadedPackageFile: dest) {
            downloadSucceeded(forLexicalModel: lm)
          } else {
            let installError = NSError(domain: "Keyman", code: 0,
                                       userInfo: [NSLocalizedDescriptionKey: "installError"])
            downloadFailed(forLexicalModelPackage: lexicalModelPackageURLString, error: installError )
          }
        }
      }

      log.info("downloading lexical model from Keyman cloud: \(lexicalModelKMPURL).")
      let task = URLSession.shared.dataTask(with: lexicalModelKMPURL) { (data, response, error) in
        DispatchQueue.main.async {
          lexicalModelDownloaded(data: data, response: response, dest: destinationUrl, error: error)
        }
      }
      task.resume()

    } else {
      log.info("\(lexicalModelPackageURLString) is not a URL string?")
      // might want to download the .js file directly, then, instead
    }
  }
  
  /// Starts the process of fetching the package file of the lexical model for the given language ID
  ///   first it fetches the list of lexical models for the given language
  ///   then it takes the first of the list and download the KMP package file and asks the app to open it (like adhoc download)
  /// - Parameters:
  ///   - languageID: the bcp47 string of the desired language
  public func downloadLexicalModelsForLanguageIfExists(languageID: String) {
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
          downloadLexicalModelPackage(string: lexicalModel.packageFilename)
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
    // Everything here was already commented-out.
  }
  
  private func lexicalModelFontURLs(forFont font: Font?, options: Options) -> [URL] {
    guard let font = font else {
      return []
    }
    return font.source.filter({ $0.hasFontExtension })
      .map({ options.fontBaseURL.appendingPathComponent($0) })
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
  
  private func downloadLexicalModel(_ lexicalModelAPI: LexicalModelAPICall) {
    let lexicalModel = lexicalModelAPI.lexicalModels[0]
    let installableLexicalModels = lexicalModel.languages.map { language in
      InstallableLexicalModel(lexicalModel: lexicalModel, languageID: language, isCustom: true)
    }
    
    let packageFilename = lexicalModel.packageFilename
    let lexicalModelURL = URL(string: "https://api.keyman.com/model")!.appendingPathComponent(packageFilename)
    
    if downloadQueue != nil {
      // Download queue is active.
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "Download queue is busy"])
      downloadFailed(forKeyboards: [], error: error) //??? forLexicalModels : installableLexicalModels
      return
    }
    
    if reachability.connection == Reachability.Connection.none {
      let error = NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: "No internet connection"])
      downloadFailed(forKeyboards: [], error: error) //??? forLexicalModels : installableLexicalModels
      return
    }
    
    do {
      try FileManager.default.createDirectory(at: Storage.active.lexicalModelDir(forID: lexicalModel.id),
                                              withIntermediateDirectories: true)
    } catch {
      log.error("Could not create dir for download: \(error)")
      return
    }
    
    let isUpdate = Storage.active.userDefaults.userLexicalModels?.contains { $0.id == lexicalModel.id } ?? false
    
    downloadQueue = HTTPDownloader(self)
    let commonUserData: [String: Any] = [
      Key.lexicalModelInfo: installableLexicalModels,
      Key.update: isUpdate
    ]
    downloadQueue!.userInfo = commonUserData
    
    let request = HTTPDownloadRequest(url: lexicalModelURL, userInfo: commonUserData)
    request.destinationFile = Storage.active.lexicalModelURL(forID: lexicalModel.id, version: lexicalModel.version ?? InstallableConstants.defaultVersion).path
    request.tag = 0
    
    downloadQueue!.addRequest(request)
    downloadQueue!.run()
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
  
  
  // MARK - deprecated helper/handler methods - they help avoid errors for now.
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
  
  func downloadQueueFinished(_ queue: HTTPDownloader) { }

  func downloadRequestStarted(_ request: HTTPDownloadRequest) {
    // If we're downloading a new keyboard.
    // The extra check is there to filter out other potential request types in the future.
    if request.tag == 0 && request.typeCode == .downloadFile {
      NotificationCenter.default.post(name: Notifications.keyboardDownloadStarted,
                                      object: self,
                                      value: request.userInfo[Key.keyboardInfo] as! [InstallableKeyboard])
    }
  }

  func downloadRequestFinished(_ request: HTTPDownloadRequest) {
    switch request.typeCode {
    case .downloadFile:
      let keyboards = request.userInfo[Key.keyboardInfo] as! [InstallableKeyboard]
      let keyboard = keyboards[0]
      let isUpdate = request.userInfo[Key.update] as! Bool

      if let statusCode = request.responseStatusCode, statusCode == 200 {
        // The request has succeeded.
        if downloadQueue!.requestsCount == 0 {
          // Download queue finished.
          downloadQueue = nil
          FontManager.shared.registerCustomFonts()
          log.info("Downloaded keyboard: \(keyboard.id).")

          NotificationCenter.default.post(name: Notifications.keyboardDownloadCompleted,
                                          object: self,
                                          value: keyboards)
          // Trigger by notification.
//          if isUpdate {
//            shouldReloadKeyboard = true
//            inputViewController.reload()
//          }
          let userDefaults = Storage.active.userDefaults
          userDefaults.set([Date()], forKey: Key.synchronizeSWKeyboard)
          userDefaults.synchronize()
        }
      } else { // Possible request error (400 Bad Request, 404 Not Found, etc.)
        downloadQueue!.cancelAllOperations()
        downloadQueue = nil

        let errorMessage = "\(request.responseStatusMessage ?? ""): \(request.url)"
        let error = NSError(domain: "Keyman", code: 0,
                            userInfo: [NSLocalizedDescriptionKey: errorMessage])
        log.error("Keyboard download failed: \(error).")

        if !isUpdate {
          // Clean up keyboard file if anything fails
          // TODO: Also clean up remaining fonts
          try? FileManager.default.removeItem(at: Storage.active.keyboardURL(for: keyboard))
        }

        // TODO:
        //Manager.shared.downloadFailed(forKeyboards: keyboards, error: error)
      }
    }
  }

  func downloadRequestFailed(_ request: HTTPDownloadRequest) {
    switch request.typeCode {
    case .downloadFile:
      downloadQueue = nil
      let error = request.error!
      log.error("Keyboard download failed: \(error).")

      let keyboards = request.userInfo[Key.keyboardInfo] as! [InstallableKeyboard]
      let keyboard = keyboards[0]
      let isUpdate = request.userInfo[Key.update] as! Bool

      if !isUpdate {
        // Clean up keyboard file if anything fails
        // TODO: Also clean up remaining fonts
        try? FileManager.default.removeItem(at: Storage.active.keyboardURL(for: keyboard))
      }
      
      // TODO:
      //Manager.shared.downloadFailed(forKeyboards: keyboards, error: error as NSError)
    }
  }
  
  // TODO:  REWORK THIS SECTION -------
  // Nothing here is actually used yet; the goal is to overhaul these notifications into something more generally useful.
  private func keyboardDownloadStarted() {
    log.info("keyboardDownloadStarted: ResourceDownloadManager")
//    view.isUserInteractionEnabled = false
//    navigationItem.setHidesBackButton(true, animated: true)
//    showDownloading("keyboard")
  }
  
  private func lexicalModelDownloadStarted(_ lexicalModels: [InstallableLexicalModel]) {
    log.info("lexicalModelDownloadStarted: ResourceDownloadManager")
//    showDownloading("dictionary")
  }
  
  private func keyboardDownloadCompleted(_ keyboards: [InstallableKeyboard]) {
    log.info("keyboardDownloadCompleted: ResourceDownloadManager")
    Manager.shared.shouldReloadKeyboard = true
    
    // Update keyboard version
    for keyboard in keyboards {
      Manager.shared.updateUserKeyboards(with: keyboard)
    }
    
//    if let toolbar = navigationController?.toolbar as? ResourceDownloadStatusToolbar {
//      toolbar.displayStatus("Keyboard successfully downloaded!", withIndicator: false, duration: 3.0)
//    }
//    restoreNavigation()
    
    // Add keyboard.
    for keyboard in keyboards {
      Manager.shared.addKeyboard(keyboard)
      _ = Manager.shared.setKeyboard(keyboard)
    }
    
//    navigationController?.popToRootViewController(animated: true)
  }
  
  private func lexicalModelDownloadCompleted(_ lexicalModels: [InstallableLexicalModel]) {
    log.info("lexicalModelDownloadCompleted: ResourceDownloadManager")
    // Add models.
    for lexicalModel in lexicalModels {
      // TODO:  Do we need a language id check?
      Manager.shared.addLexicalModel(lexicalModel)
      _ = Manager.shared.registerLexicalModel(lexicalModel)
    }
    // Add lexicalModel.
    
//    if let toolbar = navigationController?.toolbar as? ResourceDownloadStatusToolbar {
//      toolbar.displayStatus("Dictionary successfully downloaded!", withIndicator: false, duration: 3.0)
//    }
//
//    restoreNavigation()
//    navigationController?.popToRootViewController(animated: true)
  }
  
  private func keyboardDownloadFailed() {
    log.info("keyboardDownloadFailed: ResourceDownloadManager")
//    restoreNavigation()
  }
  
  private func lexicalModelDownloadFailed() {
    log.info("lexicalModelDownloadFailed: ResourceDownloadManager")
//    restoreNavigation()
    
//    let title = "Dictionary Download Error"
//    navigationController?.setToolbarHidden(true, animated: true)
//
//    let alertController = UIAlertController(title: title, message: "",
//                                            preferredStyle: UIAlertControllerStyle.alert)
//    alertController.addAction(UIAlertAction(title: "OK",
//                                            style: UIAlertActionStyle.cancel,
//                                            handler: nil))
//
//    self.present(alertController, animated: true, completion: nil)
  }
  // End REWORK THIS SECTION ------
}
