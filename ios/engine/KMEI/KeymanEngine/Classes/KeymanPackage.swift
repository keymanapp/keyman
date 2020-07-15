//
//  KeymanPackage.swift
//  KeymanEngine
//
//  Created by Jacob Bullock on 2/16/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

import Foundation
import Zip

public enum KMPError : String, Error {
  case noMetadata = "Could not find kmp.json for package."
  case invalidPackage = "Invalid Keyman Package."
  case fileSystem = "Unable to create directory structure in file system."
  case copyFiles = "Unable to copy keyboard files to file system."
  case wrongPackageType = "Package provided does not match the type expected by this method"
  case resourceNotInPackage = "Resource cannot be found in specified package"
}

/**
 * Common base class for the different KeymanPackage types.  If Swift had abstract classes, this would be one.
 *
 * Provides type-agnostic common operations on behalf of TypedKeymanPackage while otherwise providing "type erasure"
 * services for it.
 */
public class KeymanPackage {
  /**
   * Similar in role to LanguageResourceFullID, but for packages, rather than resource-languge pairings.
   *
   * Provides a unique, hashable key for use with package-oriented operations.
   */
  public struct Key: Hashable, Codable {
    var id: String
    var type: LanguageResourceType

    internal init(for package: KeymanPackage) {
      self.id = package.id
      self.type = package.resourceType()
    }

    internal init(forResource resource: AnyLanguageResource) {
      self.id = resource.packageID ?? resource.id
      self.type = resource.fullID.type
    }

    internal init(id: String, type: LanguageResourceType) {
      self.id = id
      self.type = type
    }

    public static func == (lhs: KeymanPackage.Key, rhs: KeymanPackage.Key) -> Bool {
      return lhs.id == rhs.id && lhs.type == rhs.type
    }
  }

  /**
   * Indicates the distribution level and state of the package.
   */
  public enum DistributionMethod: String, Codable {
    /**
     * Indicates that the distribution method for the package is currently unknown.  Generally occurs when a
     * package is installed via file-sharing before KeymanEngine is able to perform a package-version check.
     */
    case unknown

    /**
     * Indicates that the package is publicly distributed via Keyman's cloud API is is no longer maintained.
     * Other, better-maintained packages targetting the same language exist and are more favored.
     */
    case cloudDeprecated = "cloud-deprecated"

    /**
     * Indicates that this package is publicly distributed via Keyman's cloud API and is considered well-maintained.
     */
    case cloud

    /**
     * Indicates that this package is distributed outside the Keyman cloud API network.  As a result, KeymanEngine
     * cannot automatically validate or update this resource.
     */
    case custom
  }

  public enum VersionState: String, Codable {
    /**
     * KeymanEngine can't track up-to-date versions for custom packages, and we simply don't know before the first update-check for
     * any models installed from local files.
     */
    case unknown

    /**
     * The installed version of this package matches the latest distributed version.
     */
    case upToDate

    /**
     * There is a publicly-distributed version of the package that is newer than the one currently installed within KeymanEngine.
     */
    case needsUpdate
  }

  public enum InstallationState: String, Codable {
    /**
     * Indicates that the package is neither installed nor being downloaded.
     *
     * While a `KeymanPackage` instance will never return it, external functions such as
     * `ResourceFileManager.installState(for:)` that merely perform key-based lookups may..
     */
     case none

    /**
     * Indicates that the package not yet installed or downloaded, but is currently _being_ downloaded via KeymanEngine.
     *
     * While a `KeymanPackage` instance will never return it, external functions such as
     * `ResourceFileManager.installState(for:)` that merely perform key-based lookups may..
     */
     case downloading

     /**
      * Indicates that this `KeymanPackage` instance has been temporarily extracted for installation from a KMP file, regardless
      * of whether or not its contents have already been installed within KeymanEngine.
      */
     case pending

     /**
      * Indicates that this `KeymanPackage` instance corresponds has been fully installed and was loaded from its installed files, rather
      * than its source KMP.
      */
     case installed
  }

  /**
   * Cloud/query related metadata not tracked (or even trackable) within kmp.json regarding the support state of a keyboard.
   */
  public struct SupportStateMetadata: Codable {
    var latestVersion: String?
    var timestampForLastQuery: TimeInterval?

    var distributionMethod: DistributionMethod

    init(from queryResult: Queries.PackageVersion.ResultComponent) {
      if let entry = queryResult as? Queries.PackageVersion.ResultEntry {
        self.latestVersion = entry.version
        self.distributionMethod = .cloud
      } else /* if queryResult is Queries.PackageVersion.ResultError */ {
        self.latestVersion = nil
        // The package-version query knows nothing about it - must be custom.
        self.distributionMethod = .custom
      }

      self.timestampForLastQuery = NSDate().timeIntervalSince1970
    }

    init(fromQuery: Bool = true) {
      latestVersion = nil
      timestampForLastQuery = nil

      // Assumption: we cannot install a cloud-deprecated resource from
      // in-app cloud-backed searches.
      distributionMethod = fromQuery ? .cloud : .unknown
    }
  }

  static private let kmpFile = "kmp.json"
  public let sourceFolder: URL
  public let id: String
  internal let metadata: KMPMetadata

  // Used to denote Packages pending installation; referenced by ResourceFileManager.
  internal let isTemp: Bool

  internal init(metadata: KMPMetadata, folder: URL) {
    sourceFolder = folder
    self.metadata = metadata

    let folderName = folder.lastPathComponent
    let cacheDirectory = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
    var nameComponents = folderName.components(separatedBy: ".")

    // Have we parsed a temporary extraction site?
    // This case arises during package installation.
    isTemp = folderName.lowercased().hasSuffix(".kmp.zip") && folder.path.contains(cacheDirectory.path)
    if isTemp {
      nameComponents.removeLast() // .zip
      nameComponents.removeLast() // .kmp
    }

    // Lexical model packages use .model.kmp, so we remove the final 'model' bit.
    if nameComponents.last?.lowercased() == "model" {
      nameComponents.removeLast()
    }

    self.id = nameComponents.joined(separator: ".")
  }

  deinit {
    // Temporary packages should clean up after themselves when they go out of scope.
    if isTemp {
      do {
        if FileManager.default.fileExists(atPath: self.sourceFolder.path) {
          try FileManager.default.removeItem(at: self.sourceFolder)
        }
      } catch {
        log.debug("Could not remove temporary extraction site on package deinit")
      }
    }
  }

  public var key: Key {
    return Key(for: self)
  }

  public func isKeyboard() -> Bool {
    return metadata.packageType == .Keyboard
  }

  public var distributionMethod: DistributionMethod {
    let cachedQueryResult = Storage.active.userDefaults.cachedPackageQueryResult(forPackageKey: self.key)
    return cachedQueryResult?.distributionMethod ?? .unknown
  }

  public var installState: InstallationState {
    // Other possible states are invalid for instantiated packages.
    return isTemp ? .pending : .installed
  }

  public var versionState: VersionState {
    if distributionMethod == .unknown || distributionMethod == .custom {
      return .unknown
    } else {
      let cachedVersion = Version(Storage.active.userDefaults.cachedPackageQueryResult(forPackageKey: self.key)!.latestVersion!)!

      if cachedVersion > self.version {
        return .needsUpdate
      } else {
        return .upToDate
      }
    }
  }

  /**
   * Returns the type of LanguageResource contained within the parsed Package.
   */
  public func resourceType() -> LanguageResourceType {
    switch metadata.packageType {
      case .Keyboard:
        return .keyboard
      case .LexicalModel:
        return .lexicalModel
      default:
        // See KeymanPackage.parse below; an error is thrown there for case .Unsupported.
        fatalError("KeymanPackage.parse failed to block construction of unsupported KeymanPackage instance")
    }
  }

  // to be overridden by subclasses
  public func defaultInfoHtml() -> String {
    return "base class!"
  }
  
  public func infoHtml() -> String {
    let welcomePath = self.sourceFolder.appendingPathComponent("welcome.htm")
    
    if FileManager.default.fileExists(atPath: welcomePath.path) {
      if let html = try? String(contentsOfFile: welcomePath.path, encoding: String.Encoding.utf8) {
        return html
      }
    }
  
    return defaultInfoHtml()
  }

  public var version: Version {
    if let versionString = metadata.version, let version = Version(versionString) {
      return version
    } else {
      return Version.packageBasedFileReorg
    }
  }

  var resources: [AnyKMPResource] {
    fatalError("abstract base method went uninplemented by derived class")
  }

  /**
   * Returns an array of arrays corresponding to the available permutations of resource + language for each resource
   * specified by the package.
   *
   * Example:  nrc.en.mtnt supports three languages.  A package containing only said lexical model would return an array
   * with 1 entry:  an array with three InstallableLexicalModel entries, one for each supported language of the model, identical
   * aside from language-specific metadata.
   */
  public var installableResourceSets: [[AnyLanguageResource]] {
    fatalError("abstract base method went unimplemented by derived class")
  }

  /**
   * Parses a decompressed KMP's metadata file, producing the typed KeymanPackage instance corresponding to it.
   *
   * Typecast the return value to either KeyboardKeymanPackage or LexicalModelKeymanPackage for richly-typed
   * information about the package's contents.
   */
  static public func parse(_ folder: URL) -> KeymanPackage? {
    do {
      var path = folder
      path.appendPathComponent(kmpFile)
      if FileManager.default.fileExists(atPath: path.path) {
        let data = try Data(contentsOf: path, options: .mappedIfSafe)
        let decoder = JSONDecoder()

        var metadata: KMPMetadata

        do {
          metadata = try decoder.decode(KMPMetadata.self, from: data)
        } catch {
          throw KMPError.noMetadata
        }

        switch metadata.packageType {
          case .Keyboard:
            return KeyboardKeymanPackage(metadata: metadata, folder: folder)
          case .LexicalModel:
            return LexicalModelKeymanPackage(metadata: metadata, folder: folder)
          default:
            throw KMPError.invalidPackage
        }
      }
    } catch {
      log.error("error parsing keyman package: \(error)")
    }
    
    return nil
  }

  @available(*, deprecated, message: "Use of the completion block is unnecessary; this method now returns synchronously.")
  static public func extract(fileUrl: URL, destination: URL, complete: @escaping (KeymanPackage?) -> Void) throws {
    try unzipFile(fileUrl: fileUrl, destination: destination) {
      complete(KeymanPackage.parse(destination))
    }
  }

  static public func extract(fileUrl: URL, destination: URL) throws -> KeymanPackage? {
    try unzipFile(fileUrl: fileUrl, destination: destination)
    return KeymanPackage.parse(destination)
  }

  static public func unzipFile(fileUrl: URL, destination: URL, complete: @escaping () -> Void = {}) throws {
    try Zip.unzipFile(fileUrl, destination: destination, overwrite: true, password: nil)
    complete()
  }

  internal static func forMigration<Resource: LanguageResource, Package: TypedKeymanPackage<Resource>>(of resource: Resource, at location: URL) -> Package? {
    let metadata = KMPMetadata(from: resource, packageId: resource.id)

    if let _ = resource as? InstallableKeyboard {
      return (KeyboardKeymanPackage(metadata: metadata, folder: location) as! Package)
    } else if let _ = resource as? InstallableLexicalModel {
      return (LexicalModelKeymanPackage(metadata: metadata, folder: location) as! Package)
    } else {
      return nil
    }
  }

  /**
   * Runs the package-version query for the specified packages to determine their current support-state.
   */
  public static func querySupportStates(for keys: [Key], withSession session: URLSession = URLSession.shared, completionBlock: (([Key : SupportStateMetadata]?, Error?)-> Void)? = nil) {
    Queries.PackageVersion.fetch(for: keys, withSession: session) { results, error in
      guard error == nil, let results = results else {
        completionBlock?(nil, error)
        return
      }

      var keyboardStates: [Key : SupportStateMetadata] = [:]
      keyboardStates.reserveCapacity(results.keyboards?.count ?? 0)

      // Sadly, Dictionaries do not support mapping to other dictionaries when considering the keys.
      results.keyboards?.forEach { key, value in
        keyboardStates[KeymanPackage.Key(id: key, type: .keyboard)] = KeymanPackage.SupportStateMetadata(from: value)
      }

      var lexicalModelStates: [Key : SupportStateMetadata] = [:]
      lexicalModelStates.reserveCapacity(results.models?.count ?? 0)

      results.models?.forEach { key, value in
        lexicalModelStates[KeymanPackage.Key(id: key, type: .lexicalModel)] = KeymanPackage.SupportStateMetadata(from: value)
      }

      keyboardStates.forEach { result in
        Storage.active.userDefaults.cachedPackageQueryMetadata[result.key] = result.value
      }

      lexicalModelStates.forEach { result in
        Storage.active.userDefaults.cachedPackageQueryMetadata[result.key] = result.value
      }

      // There will be no 'merge' conflicts, so we ignore them by simply selecting the original before
      // passing them off to the completion block.
      if let completionBlock = completionBlock {
        let stateSet = keyboardStates.merging(lexicalModelStates, uniquingKeysWith: { lhs, _ in return lhs })
        completionBlock(stateSet, nil)
      }
    }
  }

  /**
   * Runs the package-version query for the specified packages to determine if any updates are available.
   */
  public static func queryCurrentVersions(for keys: [Key], withSession session: URLSession = URLSession.shared, completionBlock: (([Key : Version]?, Error?) -> Void)? = nil) {
    querySupportStates(for: keys, withSession: session) { stateSet, error in
      guard error == nil, let stateSet = stateSet else {
        completionBlock?(nil, error)
        return
      }

      let versionSet: [Key: Version] = stateSet.compactMapValues { value in
        if let versionString = value.latestVersion, let version = Version(versionString) {
          return version
        } else {
          return nil
        }
      }

      completionBlock?(versionSet, nil)
    }
  }
}

/**
 * The actual base class for Keyman packages, which are only permitted to contain a single LanguageResource type at a time.
 */
public class TypedKeymanPackage<TypedLanguageResource: LanguageResource>: KeymanPackage {
  public private(set) var installables: [[TypedLanguageResource]] = []

  internal func setInstallableResourceSets<Resource: KMPResource>(for kmpResources: [Resource]) where
      TypedLanguageResource: KMPInitializableLanguageResource {
    self.installables = kmpResources.map { resource in
      return resource.typedInstallableResources as! [TypedLanguageResource]
    } as [[TypedLanguageResource]]
  }

  internal static func baseFilename(for fullID: TypedLanguageResource.FullID) -> String {
    var ext: String

    switch(TypedLanguageResource.self) {
      case is InstallableKeyboard.Type:
        ext = "kmp"
      case is InstallableLexicalModel.Type:
        ext = "model.kmp"
      default:
        fatalError("Unsupported language resource type")
    }

    return "\(fullID.id).\(ext)"
  }

  // Cannot directly override base class's installableResourceSets with more specific type,
  // despite covariance.  So, we have to provide two separate properties.  Joy.
  // See https://forums.swift.org/t/confusing-limitations-on-covariant-overriding/16252
  public override var installableResourceSets: [[AnyLanguageResource]] {
    return installables
  }

  public func findResource(withID fullID: TypedLanguageResource.FullID) -> TypedLanguageResource? {
    let matchesFound: [TypedLanguageResource] = self.installables.compactMap { set in
      let setMatches: [TypedLanguageResource] = set.compactMap { other in
        if fullID.id == other.id && fullID.languageID == other.languageID {
          return other
        } else {
          return nil
        }
      }

      return setMatches.count > 0 ? setMatches[0] : nil
    }

    return matchesFound.count > 0 ? matchesFound[0] : nil
  }

  // Designed for use in cloud JS -> KMP migrations as needed for 13.0 -> 14.0 upgrades.
  internal func findMetadataMatchFor<Metadata: KMPResource>(resource: TypedLanguageResource, ignoreLanguage: Bool, ignoreVersion: Bool) -> Metadata?
    where TypedLanguageResource: KMPInitializableLanguageResource {

    var metadataList: [Metadata]
    if self is KeyboardKeymanPackage {
      metadataList = self.metadata.keyboards! as! [Metadata]
    } else if self is LexicalModelKeymanPackage {
      metadataList = self.metadata.lexicalModels! as! [Metadata]
    } else {
      return nil
    }

    return metadataList.first(where: { kbdMetadata in
      // For some reason, Swift just won't recognize that it's the same type in the line below.
      kbdMetadata.hasMatchingMetadata(for: resource as! Metadata.LanguageResourceType, ignoreLanguage: ignoreLanguage, ignoreVersion: ignoreVersion)
    })
  }
}
