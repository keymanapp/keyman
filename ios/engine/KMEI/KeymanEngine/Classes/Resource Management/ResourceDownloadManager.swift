//
//  ResourceDownloadManager.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 8/15/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation
import os.log

public enum DownloadError : Error {
  public enum Cause: Error {
    case error(Error)
    // code, message, triggering link
    case responseCode(Int, String, URL)
  }
  
  // The value:  the translation-lookup key.
  case failed(Cause)
  case busy
  case noInternet
  
  var localizedDescription: String {
    switch self {
    case .failed(_):
      return engineBundle.localizedString(forKey: "alert-download-error-detail", value: nil, table: nil)
    case .busy:
      return engineBundle.localizedString(forKey: "alert-download-error-busy", value: nil, table: nil)
    case .noInternet:
      return engineBundle.localizedString(forKey: "alert-no-connection-detail", value: nil, table: nil)
    }
  }
}

public enum UpdateError : Error {
  public enum Cause: Error {
    case error(Error)
    // code, message, triggering link
    case responseCode(Int, String, URL)
  }
  
  // The value:  the translation-lookup key.
  case unmanaged
  case sourceUnavailable
  
  var localizedDescription: String {
    switch self {
    case .unmanaged:
      return engineBundle.localizedString(forKey: "error-update-not-managed", value: nil, table: nil)
    case .sourceUnavailable:
      return engineBundle.localizedString(forKey: "error-update-no-link", value: nil, table: nil)
    }
  }
}

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
  internal var unitTestCurrentDate: Date?
  
  public static let DISTRIBUTION_CACHE_VALIDITY_THRESHOLD = TimeInterval(60*24*7) // in seconds.  60 minutes, 24 hrs, 7 days.
  
  public typealias CompletionHandler<Package: KeymanPackage> = (Package?, Error?) throws -> Void
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
  
  // MARK - Downloading resources
  
  // TODO:  Consider renaming this if the format will only ever be used for keyboard packages.
  /**
   * Generates a download link for the specified package.  The resulting link is planned to be evergreen, with redirects as
   * necessary to support it long, long into the future.
   */
  public func defaultDownloadURL(forPackage packageKey: KeymanPackage.Key,
                                 andResource resourceKey: AnyLanguageResourceFullID? = nil,
                                 withVersion version: Version? = nil,
                                 asUpdate: Bool? = nil) -> URL {
    var baseURL = KeymanHosts.KEYMAN_COM
    baseURL.appendPathComponent("go")
    baseURL.appendPathComponent("package")
    baseURL.appendPathComponent("download")
    baseURL.appendPathComponent(packageKey.id)
    
    var typeForEndpoint: String
    switch(packageKey.type) {
    case .keyboard:
      typeForEndpoint = "keyboard"
    case .lexicalModel:
      typeForEndpoint = "model"
    }
    
    var urlComponents = URLComponents(string: baseURL.absoluteString)!
    var queryItems: [URLQueryItem] = [
      URLQueryItem(name: "platform", value: "ios"),
      URLQueryItem(name: "tier", value: (Version.currentTagged.tier ?? .stable).rawValue),
      URLQueryItem(name: "type", value: typeForEndpoint)
    ]
    
    if let version = version {
      queryItems.append(URLQueryItem(name: "version", value: version.description))
    }
    
    if let resourceKey = resourceKey {
      queryItems.append(URLQueryItem(name: "bcp47", value: resourceKey.languageID))
    }
    
    if let asUpdate = asUpdate {
      queryItems.append(URLQueryItem(name: "update", value: asUpdate ? "1" : "0"))
    }
    
    urlComponents.queryItems = queryItems
    return urlComponents.url!
  }
  
  // Used to maintain legacy API:  downloadKeyboard and downloadLexicalModel (based on ID, language ID)
  private func downloadResource<FullID: LanguageResourceFullID>(withFullID fullID: FullID,
                                                                sendNotifications: Bool,
                                                                asUpdate: Bool? = nil,
                                                                completionBlock: CompletionHandler<FullID.Resource.Package>?)
  where FullID.Resource.Package: TypedKeymanPackage<FullID.Resource> {
    let packageKey = KeymanPackage.Key(id: fullID.id, type: fullID.type)
    let packageURL = defaultDownloadURL(forPackage: KeymanPackage.Key(id: fullID.id, type: fullID.type),
                                        andResource: fullID,
                                        asUpdate: asUpdate)
    
    // Perform common 'can download' check.  We need positive reachability and no prior download queue.
    guard self.downloader.state == .clear else {
      let err = self.downloader.state.error ?? DownloadError.busy
      self.resourceDownloadFailed(withKey: packageKey, with: err)
      try? completionBlock?(nil, err)
      return
    }
    
    let completionClosure: CompletionHandler<FullID.Resource.Package> = completionBlock ?? { package, error in
      // If the caller doesn't specify a completion block, this will carry out a default installation.
      if let package = package {
        try? ResourceFileManager.shared.install(resourceWithID: fullID, from: package)
      }
    }
    
    self.downloadPackage(withKey: packageKey,
                         from: packageURL,
                         withNotifications: sendNotifications,
                         completionBlock: completionClosure)
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
                               completionBlock: CompletionHandler<KeyboardKeymanPackage>? = nil) {
    let kbdFullID = FullKeyboardID(keyboardID: keyboardID, languageID: languageID)
    downloadResource(withFullID: kbdFullID, sendNotifications: !isUpdate, completionBlock: completionBlock)
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
    let userKeyboards = Storage.active.userDefaults.userKeyboards
    guard let userKeyboard = userKeyboards?.first(where: { $0.id == keyboardID }) else {
      // If we don't have a local installed copy, check if we're not downloading a package
      // with matching ID.
      if downloader.containsPackageKeyInQueue(matchingKey: KeymanPackage.Key(id: keyboardID, type: .keyboard)) {
        return .downloading
      } else {
        return .needsDownload
      }
    }
    
    // Check version
    let packageKey = KeymanPackage.Key(forResource: userKeyboard)
    if downloader.containsPackageKeyInQueue(matchingKey: packageKey) {
      return .downloading
    } else {
      return keyboardState(for: packageKey)
    }
  }
  
  // MARK - Lexical models
  
  // Can be called by the keyboard downloader and utilized.
  
  /// Starts the process of fetching the package file of the lexical model for the given language ID
  ///   first it fetches the list of lexical models for the given language
  ///   then it takes the first of the list and download the KMP package file and asks the app to open it (like adhoc download)
  /// - Parameters:
  ///   - languageID: the bcp47 string of the desired language
  ///   - completionClosure: a optional callback that receives either an error or the default lexical model package for the specified language.. If no closure is specified, all contents of the lexical model package will be installed automatically.
  public func downloadLexicalModelsForLanguageIfExists(languageID: String, completionClosure: CompletionHandler<LexicalModelKeymanPackage>? = nil) {
    // Note:  we aren't caching the result of this query in any way.
    // TODO:  There's no check to ensure we don't already have a model installed for the language.
    //        The original lacked this check, as well, and it's a bit of an edge case right now.
    Queries.LexicalModel.fetchModels(forLanguageCode: languageID, withSession: session) { results, error in
      if let error = error {
        // We never quite started downloading the lexical model, so there's no download to have failed.
        let message = "Failed to fetch lexical model list for \(languageID), error: \(error.localizedDescription)"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
        try? completionClosure?(nil, error)
        return
      }
      
      guard let results = results else {  // Should not be possible.
        //TODO: put up an alert instead
        let message = "No lexical models available for language \(languageID) (nil)"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
        try? completionClosure?(nil, nil)
        return
      }
      
      if results.count == 0 {
        let message = "No lexical models available for language \(languageID) (empty)"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
        try? completionClosure?(nil, nil)
        // We automatically use the first model in the list.
      } else {
        let lexicalModel = results[0].0
        let lmFullID = results[0].0.fullID
        let message = "Fetched lexical model list for \(languageID)."
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
        
        let closure = completionClosure ?? self.standardLexicalModelInstallCompletionBlock(forFullID: lmFullID)
        let downloadURL = self.defaultDownloadURL(forPackage: lexicalModel.packageKey,
                                                  andResource: lexicalModel.fullID,
                                                  asUpdate: false)
        
        self.downloadPackage(withKey: lexicalModel.packageKey, from: downloadURL, completionBlock: closure)
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
                                   completionBlock: CompletionHandler<LexicalModelKeymanPackage>? = nil) {
    // Note:  in this case, someone knows the "full ID" of the model already, but NOT its location.
    //        For lexical models, we can use either the LexicalModel query or the PackageVersion query.
    //        For consistency with keyboard download behavior, we use the PackageVersion query here.
    
    let lmFullID = FullLexicalModelID(lexicalModelID: lexicalModelID, languageID: languageID)
    downloadResource(withFullID: lmFullID, sendNotifications: !isUpdate, completionBlock: completionBlock)
  }
  
  
  
  /// - Returns: The current state for a lexical model
  @available(*, deprecated, message: "Use `ResourceFileManager.shared.installState(forPackage:)` instead.  Lexical model states are tied to the state of their package.")
  public func stateForLexicalModel(withID lexicalModelID: String) -> KeyboardState {
    let userLexicalModels = Storage.active.userDefaults.userLexicalModels
    guard let userLexicalModel = userLexicalModels?.first(where: { $0.id == lexicalModelID }) else {
      // If we don't have a local installed copy, check if we're not downloading a package
      // with matching ID.
      if downloader.containsPackageKeyInQueue(matchingKey: KeymanPackage.Key(id: lexicalModelID, type: .lexicalModel)) {
        return .downloading
      } else {
        return .needsDownload
      }
    }
    
    // Check version
    let packageKey = KeymanPackage.Key(forResource: userLexicalModel)
    if downloader.containsPackageKeyInQueue(matchingKey: packageKey) {
      return .downloading
    } else {
      return keyboardState(for: packageKey)
    }
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
  public func downloadPackage<Package: KeymanPackage>(withKey packageKey: KeymanPackage.Key,
                                                      from url: URL,
                                                      withNotifications: Bool = false,
                                                      completionBlock: @escaping CompletionHandler<Package>) {
    let batch = buildPackageBatch(withKey: packageKey,
                                  from: url,
                                  withNotifications: withNotifications,
                                  completionBlock: completionBlock)
    downloader.queue(.simpleBatch(batch))
  }
  
  // Facilitates re-use of the downloadPackage core for updates.
  // Also allows specifying LanguageResource instances for use in notifications.
  internal func buildPackageBatch<Package: KeymanPackage>(withKey packageKey: KeymanPackage.Key,
                                                          from url: URL,
                                                          withNotifications: Bool = false,
                                                          withResource resource: AnyLanguageResource? = nil,
                                                          completionBlock: @escaping CompletionHandler<Package>) -> DownloadBatch<Package> {
    var startClosure: (() -> Void)? = nil
    var completionClosure: CompletionHandler<Package>? = completionBlock
    
    if withNotifications {
      startClosure = resourceDownloadStartClosure(withKey: packageKey)
      completionClosure = resourceDownloadCompletionClosure(withKey: packageKey, handler: completionBlock)
    }
    
    // build batch for package
    return DownloadBatch(forPackageWithKey: packageKey, from: url, startBlock: startClosure, completionBlock: completionClosure)
  }
  
  // MARK: Update checks + management
  /**
   * Given that an update-check query has already been run, returns whether or not any updates are available.
   */
  public var updateCacheIsCurrent: Bool {
    let packages = ResourceFileManager.shared.installedPackages
    
    let packageWithOldestCache = packages.min { lhs, rhs in
      if let lhsTimestamp = lhs.versionQueryCache.timestampForLastQuery,
         let rhsTimestamp = rhs.versionQueryCache.timestampForLastQuery {
        return lhsTimestamp <= rhsTimestamp
      } else {
        // Ensure that the 'nil' timestamp is returned overall if it exists.
        return rhs.versionQueryCache.timestampForLastQuery != nil
      }
    }
    
    // If there are no packages, the cache is considered 'current' - there's nothing to update.
    guard let package = packageWithOldestCache else {
      return true
    }
    
    // If there is no timestamp for the 'oldest' entry, there's nothing cached for at least one package.
    // We should run the query to initialize its distribution-state metadata.
    guard let timestamp = package.versionQueryCache.timestampForLastQuery else {
      return false
    }
    
    // Every package has a cached entry... but are they recent enough?
    let date = unitTestCurrentDate ?? Date()
    let now = date.timeIntervalSince1970
    let timeDelta = now - timestamp
    
    return timeDelta <= ResourceDownloadManager.DISTRIBUTION_CACHE_VALIDITY_THRESHOLD
  }
  
  /**
   * Indicates whether any packages are known to be updatable based on the most recently
   * cached package-version query for each.  To bypass the cache and query for updated results,
   * use `queryKeysForUpdatablePackages` instead.
   */
  public var updatesAvailable: Bool {
    get {
      return getKeysForUpdatablePackages().count > 0
    }
  }
  
  /**
   * Returns a Set of package keys for packages known to be updatable,
   * based upon the results of their most recently-run latest-version queries.
   *
   * If neither `KeymanPackage.queryCurrentVersions` nor `KeymanPackage.queryDistributionStates`
   * has been run for a package, it is assumed to be current, as this is a synchronous method.
   */
  public func getKeysForUpdatablePackages() -> Set<KeymanPackage.Key> {
    let packages = ResourceFileManager.shared.installedPackages
    
    let keys: [KeymanPackage.Key] = packages.compactMap { package in
      guard package.versionQueryCache.distributionMethod == .cloud,
            let versionString = package.versionQueryCache.latestVersion,
            let latestVersion = Version(versionString) else {
        // We only update packages with full cloud support.
        return nil
      }
      
      if package.version < latestVersion {
        return package.key
      } else {
        return nil
      }
    }
    
    return Set(keys)
  }
  
  /**
   * Queries the cloud for the latest version of every installed package, returning a Set
   * of keys for packages with available updates.  The results will also be cached for use
   * with related `get`-prefixed methods, like `getKeysForUpdatablePackages`.
   */
  public func queryKeysForUpdatablePackages(completionBlock: @escaping (Set<KeymanPackage.Key>?, Error?) -> Void) {
    let allPackages = ResourceFileManager.shared.installedPackages
    
    // This call will automatically cache the results for future use / access by the 'get' equivalent.
    KeymanPackage.queryCurrentVersions(for: allPackages.map { $0.key }, withSession: session) { results, error in
      guard error == nil, let results = results else {
        completionBlock(nil, error)
        return
      }
      
      let updatables: [KeymanPackage.Key] = allPackages.compactMap { package in
        guard let result = results[package.key] else {
          return nil
        }
        
        return package.version < result ? package.key : nil
      }
      
      completionBlock(Set(updatables), nil)
    }
  }
  
  public func performBatchUpdate(forPackageKeys keysToUpdate: Set<KeymanPackage.Key>? = nil,
                                 withNotifications: Bool = true,
                                 completionBlock: (([KeymanPackage.Key], [(KeymanPackage.Key, Error)]) -> Void)? = nil) {
    let engineUpdatables = getKeysForUpdatablePackages()
    var updatables = Set<KeymanPackage.Key>()
    
    var invalids: [(KeymanPackage.Key, Error)] = []
    
    if keysToUpdate == nil {
      updatables = engineUpdatables
    } else {
      // Verify that KeymanEngine actually can update the entry.
      updatables = engineUpdatables.intersection(keysToUpdate!)
      
      // Generate errors for any entries that KeymanEngine cannot update.
      keysToUpdate!.forEach { key in
        if !updatables.contains(key) {
          invalids.append( (key, UpdateError.unmanaged) )
        }
      }
    }
    
    if updatables.count == 0 {
      completionBlock?([], invalids)
      return
    }
    
    var updateMapping: Dictionary<KeymanPackage.Key, AnyDownloadBatch> = [:]
    updatables.forEach { key in
      guard let downloadURL = Storage.active.userDefaults.cachedPackageQueryResult(forPackageKey: key)!.downloadURL else {
        // Note error - download URL missing.  Shouldn't be possible, but still.
        invalids.append( (key, UpdateError.sourceUnavailable) )
        return
      }
      
      updateMapping[key] = self.buildPackageBatch(withKey: key,
                                                  from: downloadURL) { package, error in
        guard let package = package, error == nil else {
          let errString = error != nil ? String(describing: error!) : ""
          let errorMessage = "Could not successfully download package \(key) for update: \(errString)"
          os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, errorMessage)
          return
        }
        
        if let kbdPackage = package as? KeyboardKeymanPackage {
          let updatables = ResourceFileManager.shared.findPotentialUpdates(in: kbdPackage)
          try ResourceFileManager.shared.install(resourcesWithIDs: updatables.map { $0.fullID }, from: kbdPackage)
        } else if let lmPackage = package as? LexicalModelKeymanPackage {
          let updatables = ResourceFileManager.shared.findPotentialUpdates(in: lmPackage)
          try ResourceFileManager.shared.install(resourcesWithIDs: updatables.map { $0.fullID }, from: lmPackage)
        }
      }
    }
    
    // Build the composite batch.
    let batchNodes: [DownloadNode] = updateMapping.values.map { .simpleBatch($0) }
    let updateCompletionClosure = resourceBatchUpdateCompletionClosure(withNotifications: withNotifications, completionBlock: completionBlock)
    let updateBatch = CompositeBatch(queue: batchNodes, startBlock: nil, completionBlock: updateCompletionClosure)
    
    downloader.queue(.compositeBatch(updateBatch))
  }
  
  static func packageKeys(forResources resources: [AnyLanguageResource]) -> Set<KeymanPackage.Key> {
    let keys = resources.map { $0.packageKey }
    return Set(keys)
  }
  
  @available(*, deprecated)
  public func installLexicalModelPackage(at packageURL: URL) -> InstallableLexicalModel? {
    do {
      if let package = try ResourceFileManager.shared.prepareKMPInstall(from: packageURL) as? LexicalModelKeymanPackage {
        try ResourceFileManager.shared.finalizePackageInstall(package, isCustom: false)
        // The reason we're deprecating it; only returns the first model, even if more language pairings are installed.
        return package.installables[0][0]
      } else {
        let message = "Specified package (at \(packageURL)) does not contain lexical models: \(KMPError.invalidPackage)"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
        SentryManager.capture(message)
        return nil
      }
    } catch {
      let message = "Error occurred while attempting to install package from \(packageURL): \(String(describing: error))"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
      SentryManager.capture(error, message: message)
      return nil
    }
  }
  
  // MARK - Completion handlers.
  
  internal func resourceDownloadStartClosure(withKey packageKey: KeymanPackage.Key) -> (() -> Void) {
    return { self.resourceDownloadStarted(withKey: packageKey) }
  }
  
  // Only for use with individual downloads.  Updates should have different completion handling.
  internal func resourceDownloadCompletionClosure<Package: KeymanPackage>(withKey packageKey: KeymanPackage.Key, handler: CompletionHandler<Package>?) -> CompletionHandler<Package> {
    return { package, error in
      guard let package = package, error == nil else {
        self.resourceDownloadFailed(withKey: packageKey,
                                    with: DownloadError.failed(.error( error ?? KeymanError.unknown )))
        try? handler?(nil, error)
        return
      }
      
      do {
        try handler?(package, error)
        self.resourceDownloadCompleted(with: package)
      } catch {
        let message = "Unhandled error occurred after resource successfully downloaded: \(String(describing: error))"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
        SentryManager.capture(error, message: message)
        self.resourceDownloadFailed(withKey: packageKey, with: error)
      }
      
      // After the custom handler operates, ensure that any changes it made are synchronized for use
      // with the app extension, too.
      let userDefaults = Storage.active.userDefaults
      userDefaults.set([Date()], forKey: Key.synchronizeSWKeyboard)
      userDefaults.synchronize()
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
  
  internal func resourceBatchUpdateCompletionClosure(withNotifications: Bool,
                                                     completionBlock: (([KeymanPackage.Key], [(KeymanPackage.Key, Error)]) -> Void)? = nil)
  -> InternalBatchCompletionHandler {
    return { batch in
      var successes: [KeymanPackage.Key] = []
      var failures: [(KeymanPackage.Key, Error)] = []
      
      batch.batchQueue.forEach{ tuple in
        if case let .simpleBatch(node) = tuple.0 {
          if let err = node.errors.first(where: { $0 != nil }), let error = err {
            failures.append( (node.packageKey, error) )
          } else {
            successes.append(node.packageKey)
          }
        }
      }
      
      if withNotifications {
        // Send update notification to the UI.
        let notification = BatchUpdateCompletedNotification(successes: successes, failures: failures)
        NotificationCenter.default.post(name: Notifications.batchUpdateCompleted,
                                        object: self,
                                        value: notification)
      }
      
      completionBlock?(successes, failures)
    }
  }
  
  public func standardKeyboardInstallCompletionBlock(forFullID fullID: FullKeyboardID, withModel: Bool = true) -> CompletionHandler<KeyboardKeymanPackage> {
    return { package, error in
      if let package = package {
        do {
          try ResourceFileManager.shared.install(resourceWithID: fullID, from: package)
          let message = "successfully parsed the keyboard in: \(package.sourceFolder)"
          os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
          SentryManager.breadcrumb(message)
          
          // Maintains legacy behavior; automatically sets the newly-downloaded keyboard as active.
          if let keyboard = package.findResource(withID: fullID) {
            _ = Manager.shared.setKeyboard(keyboard)
          }
          
          if withModel {
            self.downloadLexicalModelsForLanguageIfExists(languageID: fullID.languageID)
          }
        } catch {
          let message = "Keyboard installation error: \(String(describing: error))"
          os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
          SentryManager.capture(error, message: message)
        }
      } else if let error = error {
        // Often a download error.
        let errorMessage = "Installation failed: \(String(describing: error))"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, errorMessage)
      } else {
        let message = "Unknown error when attempting to install \(fullID.description))"
        os_log("%{public}s", log:KeymanEngineLogger.migration, type: .error, message)
        SentryManager.capture(message)
      }
    }
  }
  
  public func standardLexicalModelInstallCompletionBlock(forFullID fullID: FullLexicalModelID) -> CompletionHandler<LexicalModelKeymanPackage> {
    return { package, error in
      if let package = package {
        do {
          // A raw port of the queue's old installation method for lexical models.
          try ResourceFileManager.shared.finalizePackageInstall(package, isCustom: false)
          let message = "successfully parsed the lexical model in: \(package.sourceFolder)"
          os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
          SentryManager.breadcrumb(message)
          
          if let installedLexicalModel = package.findResource(withID: fullID) {
            _ = Manager.shared.registerLexicalModel(installedLexicalModel)
          }
        } catch {
          let message = "Error installing the lexical model: \(String(describing: error))"
          os_log("%{public}s", log:KeymanEngineLogger.migration, type: .error, message)
          SentryManager.capture(error, message: message)
        }
      } else if let error = error {
        let errorMessage = "Error downloading the lexical model \(String(describing: error))"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, errorMessage)
      } else {
        let message = "Unknown error when attempting to install \(fullID.description)"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
        SentryManager.capture(message)
      }
    }
  }
  
  // MARK - Notifications
  internal func resourceDownloadStarted(withKey packageKey: KeymanPackage.Key) {
    DispatchQueue.main.async {
      NotificationCenter.default.post(name: Notifications.packageDownloadStarted,
                                      object: self,
                                      value: packageKey)
    }
  }
  
  internal func resourceDownloadCompleted(with package: KeymanPackage) {
    DispatchQueue.main.async {
      NotificationCenter.default.post(name: Notifications.packageDownloadCompleted,
                                      object: self,
                                      value: package)
    }
  }
  
  internal func resourceDownloadFailed(withKey packageKey: KeymanPackage.Key, with error: Error) {
    DispatchQueue.main.async {
      NotificationCenter.default.post(name: Notifications.packageDownloadFailed,
                                      object: self,
                                      value: PackageDownloadFailedNotification(packageKey: packageKey, error: error))
    }
  }
  
  /**
   * Designed for downloading KMP files when no metadata is available in advance.
   */
  public func downloadRawKMP(from url: URL, handler: @escaping (URL?, Error?) -> Void) {
    
    // First, we need something to handle the download.
    class NoMetadataDelegate: HTTPDownloadDelegate {
      private let closure: (URL?, Error?) -> Void
      
      init(withHandler handler: @escaping (URL?, Error?) -> Void) {
        self.closure = handler;
      }
      
      func downloadRequestStarted(_ request: HTTPDownloadRequest) {
        // Not relevant
      }
      
      func downloadRequestFinished(_ request: HTTPDownloadRequest) {
        if request.responseStatusCode != 200 {
          // Possible request error (400 Bad Request, 404 Not Found, etc.)
          let error = DownloadError.failed(.responseCode(request.responseStatusCode ?? 400,
                                                         request.responseStatusMessage ?? "",
                                                         request.url))
          
          // Now that we've synthesized an appropriate error instance, use the same handler
          // as for HTTPDownloader's 'failed' condition.
          downloadRequestFailed(request, with: error)
        } else {
          self.closure(URL(fileURLWithPath: request.destinationFile!), nil)
        }
      }
      
      func downloadRequestFailed(_ request: HTTPDownloadRequest, with error: Error?) {
        self.closure(nil, error)
      }
      
      func downloadQueueFinished(_ queue: HTTPDownloader) {
        // Not relevant
      }
      
      func downloadQueueCancelled(_ queue: HTTPDownloader) {
        // Not relevant, but to be safe...
        self.closure(nil, nil)
      }
    }
    
    let delegate = NoMetadataDelegate(withHandler: handler)
    let downloader = HTTPDownloader(delegate, session: self.session)
    let request = HTTPDownloadRequest(url: url, downloadType: .downloadFile)
    
    // Since we don't know the package key in advance, we'll download it to
    // the app's cache directory, then figure everything out once we open it.
    let cachesDir = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
    let tempFilename = url.lastPathComponent
    let cachedDest = cachesDir.appendingPathComponent(tempFilename)
    
    request.destinationFile = cachedDest.path
    
    downloader.addRequest(request)
    downloader.run()
  }
}
