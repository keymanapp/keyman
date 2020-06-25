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
 * Common base class for the different KeymanPackage types.
 */
public class KeymanPackage {
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

  public func isKeyboard() -> Bool {
    return metadata.packageType == .Keyboard
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

  public var version: String? {
    return metadata.version
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
}

public class TypedKeymanPackage<TypedLanguageResource: LanguageResource>: KeymanPackage {
  public private(set) var installables: [[TypedLanguageResource]] = []

  internal func setInstallableResourceSets<Resource: KMPResource>(for kmpResources: [Resource]) where
      TypedLanguageResource: KMPInitializableLanguageResource {
    self.installables = kmpResources.map { resource in
      return resource.typedInstallableResources as! [TypedLanguageResource]
    } as [[TypedLanguageResource]]
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
