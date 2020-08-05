//
//  AssociatingPackageInstaller.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 8/5/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

public class AssociatingPackageInstaller<Resource: LanguageResource, Package: TypedKeymanPackage<Resource>> where Resource.Package == Package {
  // TODO:  extra parameter for error / success tracking
  //        Receives a map, so we should track per-package progress.
  public typealias AssociationInstaller = (LanguagePickAssociator.AssociationMap) -> Void

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
          return { associatedLexicalModelMap in
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

                // TODO:  Error handling, related progress tracking.
                try? ResourceFileManager.shared.install(resourcesWithIDs: associatedResourceIDs, from: package)
              }
            }
          }
        case .custom(_, let closure):
          return closure
      }
    }
  }

  let package: Package
  let defaultLgCode: String?
  var associationSpecs: [Associator]
  var associationQueriers: [LanguagePickAssociator]?
  var associationInstallers: [Int: AssociationInstaller] = [:]

  public init(for package: Package,
              defaultLanguageCode: String? = nil,
              withAssociators associators: [Associator] = []) {
    self.package = package
    self.defaultLgCode = defaultLanguageCode
    self.associationSpecs = associators
  }

  private func constructAssociationPickers() {
    // We instantiate this now, indicating that language selection has begun.
    self.associationQueriers = []

    // Using a classical for-loop allows us to 'map' receivers (within their closures) to their associators.
    for i in 0 ... associationSpecs.count-1 {
      let associationSpec = associationSpecs[i]

      let receiver: LanguagePickAssociator.AssociationReceiver = { status in
        // TODO: implement handling for the various progress states.
        switch status {
          // TODO:
          //case .inProgress(let queryCount, let associationsFound):
          // TODO:  use of first parameter for progress tracking.
          case .complete(_, let associationMap):
            let installer = self.associationSpecs[i].installer()
            self.associationInstallers[i] = installer

            // TODO:  tracking, error handling.
            installer(associationMap)
          default:
            break
        }
      }
      self.associationQueriers!.append(associationSpec.instance(for: self, withReceiver: receiver))
    }
  }

  public func promptForLanguages(inNavigationVC navVC: UINavigationController) {
    guard self.associationQueriers == nil else {
      fatalError("Invalid state - language picking has already been triggered.")
    }

    constructAssociationPickers()

    let baseResourceReceiver: PackageInstallViewController<Resource>.CompletionHandler = { fullIDs in
      guard let fullIDs = fullIDs else {
        // TODO:  cancellation handling
        return
      }

      // TODO:  error handling, progress tracking.
      try? ResourceFileManager.shared.install(resourcesWithIDs: fullIDs, from: self.package)
    }

    let pickerPrompt = PackageInstallViewController<Resource>(for: package,
                                                              defaultLanguageCode: defaultLgCode,
                                                              languageAssociators: associationQueriers!,
                                                              completionHandler: baseResourceReceiver)

    navVC.pushViewController(pickerPrompt, animated: true)
  }

  // TODO: a programmatic version with the approximate signature seen below.
  // public func selectLanguages(withCodes languageIDs: Set<String>)
}
