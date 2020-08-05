//
//  AssociatingPackageInstaller.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 8/5/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

public class AssociatingPackageInstaller<Resource: LanguageResource, Package: TypedKeymanPackage<Resource>> where Resource.Package == Package {

  // The second parameter should be used to report on installation success on a per-package basis.
  public typealias AssociationInstaller = (LanguagePickAssociator.AssociationMap,
                                           @escaping (KeymanPackage.Key, PackageInstallResult) -> Void)
                                           -> Void

  public typealias AssociatorBuilder = (AssociatingPackageInstaller,
                                        LanguagePickAssociator.AssociationReceiver)
                                        -> LanguagePickAssociator
  /**
   * Defines behaviors for finding and installing resources associated with the set of languages selected for installation.
   */
  public enum Associator {
    case lexicalModels
    case custom(AssociatorBuilder, AssociationInstaller)

    internal func instance(for installer: AssociatingPackageInstaller,
                           withReceiver receiver: @escaping LanguagePickAssociator.AssociationReceiver)
                           -> LanguagePickAssociator {
      switch self {
        case .lexicalModels:
          return LanguagePickAssociator(searchWith: LanguagePickAssociator.lexicalModelSearcher,
                                        progressClosure: receiver)
        case .custom(let constructor, _):
          return constructor(installer, receiver)
      }
    }

    internal func installer() -> AssociationInstaller {
      switch self {
        case .lexicalModels:
          return { associatedLexicalModelMap, completionClosure in
            // TODO:  Stuff!
            associatedLexicalModelMap.forEach { packageKey, association in
              let url = association.url
              let lgCodes = association.languageCodes

              ResourceDownloadManager.shared.downloadPackage(withKey: packageKey, from: url) {
                (package: LexicalModelKeymanPackage?, error: Error?) in
                guard error == nil, let package = package else {
                  // TODO:  Error handling, progress tracking
                  return
                }

                let associatedResources = lgCodes.flatMap { package.installableResources(forLanguage: $0) } as! [InstallableLexicalModel]
                let associatedResourceIDs = associatedResources.map { $0.typedFullID }

                do {
                  try ResourceFileManager.shared.install(resourcesWithIDs: associatedResourceIDs, from: package)
                  completionClosure(packageKey, .complete)
                } catch {
                  completionClosure(packageKey, .error(error))
                }
              }
            }
          }
        case .custom(_, let closure):
          return closure
      }
    }
  }

  public enum PackageInstallResult {
    case error(Error)
    case complete
  }

  typealias AssociationInstallMap = [KeymanPackage.Key: PackageInstallResult]

  // TODO:  Move more relevant class properties into the inner class below.
  private class ClosureShared {
    // Synchronization
    let queryGroup = DispatchGroup()
    let installGroup = DispatchGroup()
    let progressQueue = DispatchQueue(label: "com.keyman.associatingPackageInstaller")

    // Specifications needed across multiple threads.
    let associationSpecs: [Associator]
    var associationInstallers: [Int: AssociationInstaller] = [:]

    // Holds data needed for progress tracking.
    var pickingCompleted: Bool = false
    var associationQueryProgress: [Int: LanguagePickAssociator.Progress] = [:]
    var installProgressMap: [KeymanPackage.Key: PackageInstallResult?] = [:]

    var isCancelled = false

    init(with associationSpecs: [Associator]) {
      self.associationSpecs = associationSpecs
    }
  }

  private let closureShared: ClosureShared

  let package: Package
  let defaultLgCode: String?
  var associationQueriers: [LanguagePickAssociator]?

  // TODO:  Add progress-report callback closure parameter.
  public init(for package: Package,
              defaultLanguageCode: String? = nil,
              withAssociators associators: [Associator] = []) {
    self.package = package
    self.defaultLgCode = defaultLanguageCode

    self.closureShared = ClosureShared(with: associators)
  }

  private func constructAssociationPickers() {
    // We instantiate this now, indicating that language selection has begun.
    self.associationQueriers = []

    let closureShared = self.closureShared
    // Using a classical for-loop allows us to 'map' receivers (within their closures) to their associators.
    for i in 0 ... closureShared.associationSpecs.count-1 {
      let associationSpec = closureShared.associationSpecs[i]

      closureShared.queryGroup.enter()
      // TODO:  Define in separate method.
      let receiver: LanguagePickAssociator.AssociationReceiver = { status in
        switch status {
          case .cancelled:
            // TODO:  anything else to do if cancelled?
            closureShared.queryGroup.leave()
            // TODO:
          //case .inProgress(let queryCount, let associationsFound):
          case .complete(_, let associationMap):
            let installer = closureShared.associationSpecs[i].installer()
            closureShared.associationInstallers[i] = installer

            // Since completions will be signaled once per package,
            // we enter the install group once per package.
            associationMap.forEach { _, _ in closureShared.installGroup.enter() }
            closureShared.queryGroup.leave()

            installer(associationMap) { packageKey, result in
              closureShared.progressQueue.async {
                closureShared.installProgressMap[packageKey] = result

                // TODO:  compute & report progress.

                // Last thing in this closure.
                closureShared.installGroup.leave()
              }
            }
          default:
            break
        }

        closureShared.progressQueue.async {
          closureShared.associationQueryProgress[i] = status

          if case let .complete(_, associationMap) = status {
            associationMap.forEach { (key, _) in
              // Allows tracking install-completion progress.
              closureShared.installProgressMap[key] = nil  // indicates 'downloading'.
            }
          }

          // TODO:  trigger a 'progress report' if in the right state. (isCancelled == false is part of that.)
        }
      }
      self.associationQueriers!.append(associationSpec.instance(for: self, withReceiver: receiver))
    }
  }

  private func initializeSynchronizationGroups() {
    let closureShared = self.closureShared
    closureShared.queryGroup.enter()
    closureShared.installGroup.enter()

    // All queries must have returned in order for us to be sure all
    // needed installations have occurrred.
    closureShared.queryGroup.notify(queue: closureShared.progressQueue) {
      closureShared.installGroup.leave() // ?
    }

    closureShared.installGroup.notify(queue: DispatchQueue.main) {
      // TODO:  The final 'progress report' / completion signaller.
    }
  }

  public func promptForLanguages(inNavigationVC navVC: UINavigationController) {
    guard self.associationQueriers == nil, !closureShared.pickingCompleted else {
      fatalError("Invalid state - language picking has already been triggered.")
    }

    initializeSynchronizationGroups()
    constructAssociationPickers()

    let closureShared = self.closureShared
    // For use in closures called by other DispatchGroups.
    let packageKey = self.package.key

    // TODO:  Move contents to their own method so that this may be shared with a future
    //        programmatic installation method.
    //
    // Is triggered from UI, so is in the standard, main UI thread / DispatchGroup.
    let baseResourceReceiver: PackageInstallViewController<Resource>.CompletionHandler = { fullIDs in
      guard let fullIDs = fullIDs else {
        closureShared.isCancelled = true
        closureShared.queryGroup.leave()

        return
      }

      closureShared.progressQueue.sync {
        // Indicates a transition to 'installing' state.
        closureShared.pickingCompleted = true
        closureShared.installProgressMap[packageKey] = nil
      }
      closureShared.installGroup.enter()  // Enter the 'install' group before exiting the 'query' group, to be safe.
      closureShared.queryGroup.leave()

      var result: PackageInstallResult
      do {
        try ResourceFileManager.shared.install(resourcesWithIDs: fullIDs, from: self.package)
        result = .complete
      } catch {
        result = .error(error)
      }

      closureShared.progressQueue.async {
        closureShared.installProgressMap[packageKey] = result

        // TODO:  report on installation progress?
      }

      closureShared.installGroup.leave()
    }

    let pickerPrompt = PackageInstallViewController<Resource>(for: self.package,
                                                              defaultLanguageCode: defaultLgCode,
                                                              languageAssociators: associationQueriers!,
                                                              completionHandler: baseResourceReceiver)

    navVC.pushViewController(pickerPrompt, animated: true)
  }

  // TODO: a programmatic version with the approximate signature seen below.
  // public func selectLanguages(withCodes languageIDs: Set<String>)
}
