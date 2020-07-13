//
//  ResourceDownloadManager.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 8/15/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

// One half of the resource management puzzle - this part generates download and update requests at the demand
// of app UI and submits them to the actual 'download manager', the ResourceDownloadQueue.
//
// This will be the public face of resource download management in KMEI, while the other half is private and
// only accessible within the library.
public class ResourceDownloadManager {
  // internal b/c testing access.
  internal var session: URLSession
  internal var downloader: ResourceDownloadQueue
  private var isDidUpdateCheck = false

  public typealias CompletionHandler<Resource: LanguageResource> = (Resource.Package?, Error?) -> Void where Resource.Package: TypedKeymanPackage<Resource>
  public typealias BatchCompletionHandler = () -> Void
  internal typealias InternalBatchCompletionHandler = (CompositeBatch) -> Void
  
  public static let shared = ResourceDownloadManager()

  internal init() {
    session = URLSession.shared
    downloader = ResourceDownloadQueue()
  }

  // Intended only for use in testing!
  internal init(session: URLSession, autoExecute: Bool) {
    self.session = session
    downloader = ResourceDownloadQueue(session: session, autoExecute: autoExecute)
  }
  
  // MARK: - Common functionality
  
  private func fetchHandler(for resourceType: LanguageResourceType?, _ completionHandler: @escaping () -> Void)
                            -> (_ error: Error?) -> Void {
    return { error in
      if let error = error {
        // TODO:  Connect to an error handler (or just render appropriate text) based on the resource type.
        self.resourceDownloadFailed(for: [] as [InstallableKeyboard], with: error)
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
      self.resourceDownloadFailed(for: [] as [InstallableKeyboard], with: error)
      return nil
    }
    
    return keyboard
  }

  // Used to maintain legacy API:  downloadKeyboard and downloadLexicalModel (based on ID, language ID)
  private func downloadResource<FullID: LanguageResourceFullID>(withFullID fullID: FullID,
                                                                sendNotifications: Bool,
                                                                completionBlock: CompletionHandler<FullID.Resource>?)
  where FullID.Resource.Package: TypedKeymanPackage<FullID.Resource> {
    // Note:  in this case, someone knows the "full ID" of the resource already, but NOT its location.
    //        We can use the package-version query to attempt a lookup for a .kmp location
    //        for download.
    let packageKey = KeymanPackage.Key(id: fullID.id, type: fullID.type)
    Queries.PackageVersion.fetch(for: [packageKey], withSession: session) { result, error in
      guard let result = result, error == nil else {
        log.info("Error occurred requesting location for \(fullID.description)")
        self.resourceDownloadFailed(forFullID: fullID, with: error ?? .noData)
        completionBlock?(nil, error ?? .noData)
        return
      }

      guard case let .success(data) = result.entryFor(packageKey) else {
        if case let .failure(errorEntry) = result.entryFor(packageKey) {
          if let errorEntry = errorEntry {
            log.info("Query reported error: \(String(describing: errorEntry.error))")
          }
        }
        self.resourceDownloadFailed(forFullID: fullID, with: Queries.ResultError.unqueried)
        completionBlock?(nil, Queries.ResultError.unqueried)
        return
      }

      // Perform common 'can download' check.  We need positive reachability and no prior download queue.
      guard self.downloader.state == .clear else {
        let err = self.downloader.state.error ??
          NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: "Already busy downloading something"])
        self.resourceDownloadFailed(forFullID: fullID, with: err)
        completionBlock?(nil, err)
        return
      }

      let completionClosure: CompletionHandler<FullID.Resource> = completionBlock ?? { package, error in
        // If the caller doesn't specify a completion block, this will carry out a default installation.
        if let package = package {
          try? ResourceFileManager.shared.install(resourceWithID: fullID, from: package)
        }
      }

      self.downloadPackage(forFullID: fullID,
                           from: URL.init(string: data.packageURL)!,
                           withNotifications: sendNotifications,
                           completionBlock: completionClosure)
    }
  }

  /// Asynchronously fetches the .js file for the keyboard with given IDs.
  /// See `Notifications` for notification on success/failiure.
  /// - Parameters:
  ///   - isUpdate: Keep the keyboard files on failure
  ///   - fetchRepositoryIfNeeded: Fetch the list of keyboards from the API if necessary.
  public func downloadKeyboard(withID keyboardID: String,
                               languageID: String,
                               isUpdate: Bool,
                               fetchRepositoryIfNeeded: Bool = true,
                               completionBlock: CompletionHandler<InstallableKeyboard>? = nil) {
    let kbdFullID = FullKeyboardID(keyboardID: keyboardID, languageID: languageID)
    downloadResource(withFullID: kbdFullID, sendNotifications: !isUpdate, completionBlock: completionBlock)
  }

  private func keyboardFontURLs(forFont font: Font?, options: Options) -> [URL] {
    guard let font = font else {
      return []
    }
    return font.source.filter({ $0.hasFontExtension })
      .map({ options.fontBaseURL.appendingPathComponent($0) })
  }

  @available(*, deprecated) // Used to maintain deprecated methods stateForKeyboard, stateForLexicalModel.
  private func keyboardState(for key: KeymanPackage.Key) -> KeyboardState {
    switch(ResourceFileManager.shared.installState(forPackage: key)) {
      case .none:
        return .needsDownload
      case .downloading:
        return .downloading
      case .pending:
        return .downloading
      case .installed:
        let package = ResourceFileManager.shared.getInstalledPackage(withKey: key)!
        switch(package.versionState) {
          case .unknown:
            return .upToDate
          case .upToDate:
            return .upToDate
          case .needsUpdate:
            return .needsUpdate
        }
    }
  }

  // Deprecating due to tricky assumption - a keyboard _can_ be installed from two separate packages.
  /// - Returns: The current state for a keyboard
  @available(*, deprecated, message: "Use `ResourceFileManager.shared.installState(forPackage:)` instead.  Keyboard states are now tied to the state of their package.")
  public func stateForKeyboard(withID keyboardID: String) -> KeyboardState {
    // For this call, we don't actually need the language ID to be correct.
    let fullKeyboardID = FullKeyboardID(keyboardID: keyboardID, languageID: "")
    if downloader.containsResourceInQueue(matchingID: fullKeyboardID, requireLanguageMatch: false) {
      return .downloading
    }

    let userKeyboards = Storage.active.userDefaults.userKeyboards
    guard let userKeyboard = userKeyboards?.first(where: { $0.id == keyboardID }) else {
      return .needsDownload
    }

    // Check version
    let packageKey = KeymanPackage.Key(forResource: userKeyboard)
    return keyboardState(for: packageKey)
  }

  // MARK - Lexical models
  
  // Can be called by the keyboard downloader and utilized.
  
  /// Starts the process of fetching the package file of the lexical model for the given language ID
  ///   first it fetches the list of lexical models for the given language
  ///   then it takes the first of the list and download the KMP package file and asks the app to open it (like adhoc download)
  /// - Parameters:
  ///   - languageID: the bcp47 string of the desired language
  public func downloadLexicalModelsForLanguageIfExists(languageID: String) {
    // Note:  we aren't caching the result of this query in any way.
    // TODO:  There's no check to ensure we don't already have a model installed for the language.
    //        The original lacked this check, as well, and it's a bit of an edge case right now.
    Queries.LexicalModel.fetch(forLanguageCode: languageID) { result, error in
      if let error = error {
        log.info("Failed to fetch lexical model list for "+languageID+". error: "+error.localizedDescription)
        self.resourceDownloadFailed(for: [] as [InstallableLexicalModel], with: error)
        return
      }

      guard let result = result else {
        //TODO: put up an alert instead
        log.info("No lexical models available for language \(languageID) (nil)")
        return
      }

      if result.count == 0 {
        log.info("No lexical models available for language \(languageID) (empty)")
        // We automatically use the first model in the list.
      } else if let lmFullID = result[0].modelFor(languageID: languageID)?.fullID {
        log.info("Fetched lexical model list for "+languageID+".")
        let completionClosure = self.standardLexicalModelInstallCompletionBlock(forFullID: lmFullID)
        self.downloadPackage(forFullID: lmFullID, from: URL.init(string: result[0].packageFilename)!, completionBlock: completionClosure)
      }
    }
  }

  /// Asynchronously fetches the .js file for the lexical model with given IDs.
  /// See `Notifications` for notification on success/failiure.
  /// - Parameters:
  ///   - isUpdate: Keep the lexical model files on failure
  ///   - fetchRepositoryIfNeeded: Fetch the list of lexical models from the API if necessary.
  public func downloadLexicalModel(withID lexicalModelID: String,
                                   languageID: String,
                                   isUpdate: Bool,
                                   fetchRepositoryIfNeeded: Bool = true,
                                   completionBlock: CompletionHandler<InstallableLexicalModel>? = nil) {
    // Note:  in this case, someone knows the "full ID" of the model already, but NOT its location.
    //        For lexical models, we can use either the LexicalModel query or the PackageVersion query.
    //        For consistency with keyboard download behavior, we use the PackageVersion query here.

    let lmFullID = FullLexicalModelID(lexicalModelID: lexicalModelID, languageID: languageID)
    downloadResource(withFullID: lmFullID, sendNotifications: !isUpdate, completionBlock: completionBlock)
  }


  
  /// - Returns: The current state for a lexical model
  @available(*, deprecated, message: "Use `ResourceFileManager.shared.installState(forPackage:)` instead.  Lexical model states are tied to the state of their package.")
  public func stateForLexicalModel(withID lexicalModelID: String) -> KeyboardState {
    // For this call, we don't actually need the language ID to be correct.
    let fullLexicalModelID = FullLexicalModelID(lexicalModelID: lexicalModelID, languageID: "")
    if downloader.containsResourceInQueue(matchingID: fullLexicalModelID, requireLanguageMatch: false) {
      return .downloading
    }

    let userLexicalModels = Storage.active.userDefaults.userLexicalModels
    guard let userLexicalModel = userLexicalModels?.first(where: { $0.id == lexicalModelID }) else {
      return .needsDownload
    }

    // Check version
    let packageKey = KeymanPackage.Key(forResource: userLexicalModel)
    return keyboardState(for: packageKey)
  }

  /**
   * Downloads a package for the specified `LanguageResourceFullID` (`FullKeyboardID` / `FullLexicalModelID`)
   * and parses it automatically, given a pre-known URL.  The fully parsed package (`KeyboardKeymanPackage` /
   * `LexicalModelKeymanPackage`) or `Error` that results is available upon completion.
   *
   * Actual installation of the resource is left to calling code; consider use of `ResourceFileManager`'s `install` methods
   * with the returned package.
   *
   * `withNotifications` specifies whether or not any of KeymanEngine's `NotificationCenter` notifications should be generated.
   */
  public func downloadPackage<FullID: LanguageResourceFullID>(forFullID fullID: FullID,
                                                              from url: URL,
                                                              withNotifications: Bool = false,
                                                              completionBlock: @escaping CompletionHandler<FullID.Resource>)
  where FullID.Resource.Package: TypedKeymanPackage<FullID.Resource> {
    let batch = buildPackageBatch(forFullID: fullID, from: url, withNotifications: withNotifications, completionBlock: completionBlock)
    downloader.queue(.simpleBatch(batch))
  }

  // Facilitates re-use of the downloadPackage core for updates.
  // Also allows specifying LanguageResource instances for use in notifications.
  internal func buildPackageBatch<FullID: LanguageResourceFullID>(forFullID fullID: FullID,
                                                                  from url: URL,
                                                                  withNotifications: Bool = false,
                                                                  withResource resource: FullID.Resource? = nil,
                                                                  completionBlock: @escaping CompletionHandler<FullID.Resource>) -> DownloadBatch<FullID>
  where FullID.Resource.Package: TypedKeymanPackage<FullID.Resource> {
    var startClosure: (() -> Void)? = nil
    var completionClosure: CompletionHandler<FullID.Resource>? = completionBlock

    if withNotifications {
      let resources = resource != nil ? [resource!] : [] as [FullID.Resource]
      // We don't have the full metadata available, but we can at least signal which resource type this way.
      startClosure = resourceDownloadStartClosure(forFullID: fullID)
      completionClosure = resourceDownloadCompletionClosure(for: resources, handler: completionBlock)
    }

    // build batch for package
    return DownloadBatch(forPackageWithID: fullID, from: url, startBlock: startClosure, completionBlock: completionClosure)
  }
  
  // MARK: Update checks + management
  /**
   * Given that an update-check query has already been run, returns whether or not any updates are available.
   */
  public var updatesAvailable: Bool {
    get {
      return getAvailableUpdates() != nil
    }
  }

  public func getAvailableUpdates() -> [AnyLanguageResource]? {
    // Relies upon KMManager's preload; this was the case before the rework.
    if Manager.shared.apiKeyboardRepository.languages == nil && Manager.shared.apiLexicalModelRepository.languages == nil {
      return nil
    }

    isDidUpdateCheck = true
    
    var updatables: [AnyLanguageResource] = []

    // Gets the list of current, local keyboards in need of an update.
    // Version matches the current version, not the updated version.
    let kbds = getUpdatableKeyboards()
    updatables.append(contentsOf: kbds)

    // Likewise for lexical models.
    let lexModels = getUpdatableLexicalModels()
    updatables.append(contentsOf: lexModels)
    
    if updatables.count > 0 {
      return updatables
    } else {
      return nil
    }
  }
  
  public func performUpdates(forResources resources: [AnyLanguageResource]) {
    // The plan is to create new notifications to handle batch updates here, rather than
    // require a UI to manage the update queue.
    var batches: [AnyDownloadBatch] = []

    // TODO:  Keyboard update is broken, as apiKeyboardRepository will specify the wrong file.
    // TODO:  Merge the keyboard and lexical model pathways; it's WET code.
    resources.forEach { res in
      if let kbd = res as? InstallableKeyboard {
//        if let filename = Manager.shared.apiKeyboardRepository.keyboards?[kbd.id]?.filename,
//           let path = URL.init(string: filename) {
//          let batch = self.buildPackageBatch(forFullID: kbd.fullID, from: path, withResource: kbd) { package, error in
//            if let package = package {
//              try? ResourceFileManager.shared.install(resourceWithID: kbd.fullID, from: package)
//            }
//            // else error:  already handled by wrapping closure set within buildPackageBatch.
//          }
//          batches.append(batch)
//        }
      } else if let lex = res as? InstallableLexicalModel {
        if let filename = Manager.shared.apiLexicalModelRepository.lexicalModels?[lex.id]?.packageFilename,
           let path = URL.init(string: filename) {
          let batch = self.buildPackageBatch(forFullID: lex.fullID, from: path, withResource: lex) { package, error in
            if let package = package {
              try? ResourceFileManager.shared.install(resourceWithID: lex.fullID, from: package)
            }
            // else error:  already handled by wrapping closure set within buildPackageBatch.
          }
          batches.append(batch)
        }
      }
    }
    
    let batchUpdate = CompositeBatch(queue: batches.map { return DownloadNode.simpleBatch($0) },
                                     startBlock: resourceBatchUpdateStartClosure(for: resources),
                                     completionBlock: resourceBatchUpdateCompletionClosure(for: resources))
    downloader.queue(.compositeBatch(batchUpdate))
  }
  
  private func getUpdatableKeyboards() -> [InstallableKeyboard] {
    var updateQueue: [InstallableKeyboard] = []
    var kbIDs = Set<String>()
    
    // Build the keyboard update queue
    Storage.active.userDefaults.userKeyboards?.forEach { kb in
      let kbState = stateForKeyboard(withID: kb.id)
      if kbState == .needsUpdate {
        if(!kbIDs.contains(kb.id)) {
          kbIDs.insert(kb.id)
          updateQueue.append(kb)
        }
      }
    }
    
    return updateQueue
  }

  private func getUpdatableLexicalModels() -> [InstallableLexicalModel] {
    // Build the lexical model update queue
    var updateQueue: [InstallableLexicalModel] = []
    var lmIDs = Set<String>()
    
    Storage.active.userDefaults.userLexicalModels?.forEach { lm in
      let lmState = stateForLexicalModel(withID: lm.id)
      if lmState == .needsUpdate {
        if !lmIDs.contains(lm.id) {
          lmIDs.insert(lm.id)
          updateQueue.append(lm)
        }
      }
    }

    return updateQueue
  }

  @available(*, deprecated)
  public func installLexicalModelPackage(at packageURL: URL) -> InstallableLexicalModel? {
    do {
      if let package = try ResourceFileManager.shared.prepareKMPInstall(from: packageURL) as? LexicalModelKeymanPackage {
        try ResourceFileManager.shared.finalizePackageInstall(package, isCustom: false)
        // The reason we're deprecating it; only returns the first model, even if more language pairings are installed.
        return package.installables[0][0]
      } else {
        log.error("Specified package (at \(packageURL)) does not contain lexical models: \(KMPError.invalidPackage)")
        return nil
      }
    } catch {
      log.error("Error occurred while attempting to install package from \(packageURL): \(String(describing: error))")
      return nil
    }
  }

  // MARK - Completion handlers.

  internal func resourceDownloadStartClosure<Resource: LanguageResource>(for resources: [Resource]) -> (() -> Void) {
    return { self.resourceDownloadStarted(for: resources) }
  }

  internal func resourceDownloadStartClosure<FullID: LanguageResourceFullID>(forFullID fullID: FullID) -> (() -> Void) {
    return { self.resourceDownloadStarted(forFullID: fullID) }
  }

  // Only for use with individual downloads.  Updates should have different completion handling.
  internal func resourceDownloadCompletionClosure<Resource: LanguageResource>(for resources: [Resource], handler: CompletionHandler<Resource>?) -> CompletionHandler<Resource> {
    return { package, error in
      if let error = error {
        resources.forEach { resource in
          do {
            let resourcePath = Storage.active.resourceURL(for: resource)!
            if FileManager.default.fileExists(atPath: resourcePath.path) {
              try? FileManager.default.removeItem(at: resourcePath)
            }
          }
        }

        self.resourceDownloadFailed(for: resources, with: error)
      } else if let _ = package {
        // successful download
        // Problem:  this uses the lookup-version of the resources, which may not be perfect matches
        // for what lies within the newly-downloaded package.
        self.resourceDownloadCompleted(for: resources)
      }

      handler?(package, error)

      // After the custom handler operates, ensure that any changes it made are synchronized for use
      // with the app extension, too.
      let userDefaults = Storage.active.userDefaults
      userDefaults.set([Date()], forKey: Key.synchronizeSWKeyboard)
      userDefaults.synchronize()
    }
  }

  internal func resourceUpdateCompletionClosure<Resource: LanguageResource>(for resources: [Resource]) -> CompletionHandler<Resource> {
    // Updates should not generate notifications per resource.
    return { package, error in
      if let package = package {
        // Do not send notifications for individual resource updates.
        let resourceIDs: [Resource.FullID] = resources.map { $0.typedFullID }
        do {
          if let keyboards = resources as? [InstallableKeyboard], let package = package as? KeyboardKeymanPackage {
            // TEMP:  currently required because downloaded keyboards aren't actually in packages.
            keyboards.forEach { keyboard in
              if let updatedKbd = package.findResource(withID: keyboard.typedFullID) {
                Manager.shared.updateUserKeyboards(with: updatedKbd)

                if Manager.shared.currentKeyboard?.fullID == keyboard.fullID {
                  // Issue:  does not actually trigger a reload if the user isn't within the Settings view hierarchy
                  // Fixing this requires a refactor of `shouldReloadKeyboard`.
                  Manager.shared.shouldReloadKeyboard = true
                }
              }
            }
          } else if let _ = resources as? [InstallableLexicalModel] {
            try ResourceFileManager.shared.install(resourcesWithIDs: resourceIDs, from: package)
          }
        } catch {
          log.error("Error updating resources from package \(package.id)")
        }

        // After the custom handler operates, ensure that any changes it made are synchronized for use
        // with the app extension, too.
        let userDefaults = Storage.active.userDefaults
        userDefaults.set([Date()], forKey: Key.synchronizeSWKeyboard)
        userDefaults.synchronize()
      }
    }
  }

  internal func resourceBatchUpdateStartClosure(for resources: [AnyLanguageResource]) -> (() -> Void) {
    return {
      let notification = BatchUpdateStartedNotification(resources)
      NotificationCenter.default.post(name: Notifications.batchUpdateStarted,
                                      object: self,
                                      value: notification)
    }
  }

  internal func resourceBatchUpdateCompletionClosure(for resources: [AnyLanguageResource],
                                                     completionBlock: BatchCompletionHandler? = nil)
                                                     -> InternalBatchCompletionHandler {
    return { batch in
      var successes: [[AnyLanguageResource]] = []
      var failures: [[AnyLanguageResource]] = []
      var errors: [Error] = []

      // Remember, since this is for .composite batches, batch.tasks is of type [DownloadBatch].
      for (index, res) in batch.tasks.enumerated() {
        if batch.errors[index] == nil {
          let successSet: [AnyLanguageResource] = res.resourceIDs.compactMap { fullID in
            return resources.first(where: { resource in
              resource.fullID.matches(fullID)
            })
          }
          successes.append(successSet)
        } else {
          let failureSet: [AnyLanguageResource] = res.resourceIDs.compactMap { fullID in
            return resources.first(where: { resource in
              resource.fullID.matches(fullID)
            })
          }
          failures.append(failureSet)
          errors.append(batch.errors[index]!)
        }
      }

      let notification = BatchUpdateCompletedNotification(successes: successes, failures: failures, errors: errors)
      NotificationCenter.default.post(name: Notifications.batchUpdateCompleted,
                                      object: self,
                                      value: notification)
      completionBlock?()
    }
  }

  public func standardKeyboardInstallCompletionBlock(forFullID fullID: FullKeyboardID, withModel: Bool = true) -> CompletionHandler<InstallableKeyboard> {
    return { package, error in
      if let package = package {
        do {
          try ResourceFileManager.shared.install(resourceWithID: fullID, from: package)
          log.info("succesfully parsed the keyboard in: \(package.sourceFolder)")

          // Maintains legacy behavior; automatically sets the newly-downloaded keyboard as active.
          if let keyboard = package.findResource(withID: fullID) {
            _ = Manager.shared.setKeyboard(keyboard)
          }

          if withModel {
            self.downloadLexicalModelsForLanguageIfExists(languageID: fullID.languageID)
          }
        } catch {
          log.error("Keyboard installation error: \(String(describing: error))")
        }
      } else if let error = error {
        log.error("Installation failed: \(String(describing: error))")
      } else {
        log.error("Unknown error when attempting to install \(fullID.description))")
      }
    }
  }

  public func standardLexicalModelInstallCompletionBlock(forFullID fullID: FullLexicalModelID) -> CompletionHandler<InstallableLexicalModel> {
    return { package, error in
      if let package = package {
        do {
          // A raw port of the queue's old installation method for lexical models.
          try ResourceFileManager.shared.finalizePackageInstall(package, isCustom: false)
          log.info("successfully parsed the lexical model in: \(package.sourceFolder)")

          if let installedLexicalModel = package.findResource(withID: fullID) {
            _ = Manager.shared.registerLexicalModel(installedLexicalModel)
          }
        } catch {
          log.error("Error installing the lexical model: \(String(describing: error))")
        }
      } else if let error = error {
        log.error("Error downloading the lexical model \(String(describing: error))")
      } else {
        log.error("Unknown error when attempting to install \(fullID.description)")
      }
    }
  }

  // MARK - Notifications
  internal func resourceDownloadStarted<Resource: LanguageResource>(for resources: [Resource]) {
    if let keyboards = resources as? [InstallableKeyboard] {
      NotificationCenter.default.post(name: Notifications.keyboardDownloadStarted,
                                          object: self,
                                          value: keyboards)
    } else if let lexicalModels = resources as? [InstallableLexicalModel] {
      NotificationCenter.default.post(name: Notifications.lexicalModelDownloadStarted,
                                              object: self,
                                              value: lexicalModels)
    }
  }

  internal func resourceDownloadStarted<FullID: LanguageResourceFullID>(forFullID id: FullID) {
    // Note:  when all is said and done, we may want to rework notifications to report the FullID, not
    //        the full LanguageResource.
    if let _ = id as? FullKeyboardID {
      NotificationCenter.default.post(name: Notifications.keyboardDownloadStarted,
                                          object: self,
                                          value: [])
    } else if let _ = id as? FullLexicalModelID {
      NotificationCenter.default.post(name: Notifications.lexicalModelDownloadStarted,
                                              object: self,
                                              value: [])
    }
  }

  internal func resourceDownloadCompleted<Resource: LanguageResource>(for resources: [Resource]) {
    if let keyboards = resources as? [InstallableKeyboard] {
      let notification = KeyboardDownloadCompletedNotification(keyboards)
      NotificationCenter.default.post(name: Notifications.keyboardDownloadCompleted,
                                      object: self,
                                      value: notification)
    } else if let lexicalModels = resources as? [InstallableLexicalModel] {
      let notification = LexicalModelDownloadCompletedNotification(lexicalModels)
      NotificationCenter.default.post(name: Notifications.lexicalModelDownloadCompleted,
                                      object: self,
                                      value: notification)
    }
  }

  internal func resourceDownloadFailed<Resource: LanguageResource>(for resources: [Resource], with error: Error) {
    if let keyboards = resources as? [InstallableKeyboard] {
      let notification = KeyboardDownloadFailedNotification(keyboards: keyboards, error: error)
      NotificationCenter.default.post(name: Notifications.keyboardDownloadFailed,
                                      object: self,
                                      value: notification)
    } else if let lexicalModels = resources as? [InstallableLexicalModel] {
      // Sadly, this notification reports with a different format.
      let languageID = lexicalModels.count > 0 ? lexicalModels[0].languageID : ""
      let notification = LexicalModelDownloadFailedNotification(lmOrLanguageID: languageID, error: error)
      NotificationCenter.default.post(name: Notifications.lexicalModelDownloadFailed,
                                      object: self,
                                      value: notification)
    }
  }

  internal func resourceDownloadFailed<FullID: LanguageResourceFullID>(forFullID fullID: FullID, with error: Error) {
    if let _ = fullID as? FullKeyboardID {
      let notification = KeyboardDownloadFailedNotification(keyboards: [], error: error)
      NotificationCenter.default.post(name: Notifications.keyboardDownloadFailed,
                                      object: self,
                                      value: notification)
    } else if let _ = fullID as? FullLexicalModelID {
      let notification = LexicalModelDownloadFailedNotification(lmOrLanguageID: fullID.languageID, error: error)
      NotificationCenter.default.post(name: Notifications.lexicalModelDownloadFailed,
                                      object: self,
                                      value: notification)
    }
  }
}
