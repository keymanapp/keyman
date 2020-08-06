//
//  AssociatingPackageInstaller.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 8/5/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

/**
 * Given a `KeymanPackage` that KeymanEngine knows how to install, this class facilitates language selection as part of the
 * package installation process.
 *
 * "Associations" may also be specified to look up 'associated' resources for the selected language codes and have them install alongside
 * the base package.  For example, installing the `sil_euro_latin` keyboard for `en` (English) with the `.lexicalModels` association
 * will query for `en` models and install them as well as the `sil_euro_latin` package.
 *
 * At the time of writing, this will return the `nrc.en.mtnt` model and will install it for use with `en`
 * before indicating that overall package installation is complete.
 */
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
    /**
     * Facilitates installing lexical models for the languages being installed.
     */
    case lexicalModels

    /**
     * Designed to allow framework users to specify their own associations if desired.
     */
    case custom(AssociatorBuilder, AssociationInstaller)

    internal func instance(for installer: AssociatingPackageInstaller,
                           withSession session: URLSession,
                           withReceiver receiver: @escaping LanguagePickAssociator.AssociationReceiver)
                           -> LanguagePickAssociator {
      switch self {
        case .lexicalModels:
          return LanguagePickAssociator(searchWith: LanguagePickAssociator.constructLexicalModelSearcher(session: session),
                                        progressClosure: receiver)
        case .custom(let constructor, _):
          return constructor(installer, receiver)
      }
    }

    internal func installer(downloadManager: ResourceDownloadManager) -> AssociationInstaller {
      switch self {
        case .lexicalModels:
          return { associatedLexicalModelMap, completionClosure in
            // TODO:  Stuff!
            associatedLexicalModelMap.forEach { packageKey, association in
              let url = association.url
              let lgCodes = association.languageCodes

              downloadManager.downloadPackage(withKey: packageKey, from: url) {
                (package: LexicalModelKeymanPackage?, error: Error?) in
                guard error == nil, let package = package else {
                  completionClosure(packageKey, .error(error ?? NSError()))
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

  public enum Progress {
    case cancelled
    case starting // signals that language picks are finalized, so progress reports will now begin.
    case inProgress
    case complete
  }

  public typealias ProgressReceiver = (Progress) -> Void

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

    let progressCallback: ProgressReceiver
    let downloadManager: ResourceDownloadManager

    var isCancelled = false

    init(with associationSpecs: [Associator], downloadManager: ResourceDownloadManager, receiver: @escaping ProgressReceiver) {
      self.associationSpecs = associationSpecs
      self.downloadManager = downloadManager
      self.progressCallback = receiver
    }

    // Since progress info is stored here, it makes the most sense to track progress-related
    // methods here too.

    internal var reportsProgress: Bool {
      return !isCancelled && pickingCompleted
    }

    /**
     * Computes the initial level of progress made toward the overall installation at the time that language selections were
     * finalized.
     */
    internal func initializeProgress() {
      self.progressCallback(.starting)
    }

    /**
     * Generates data useful for reporting about the installation's overall progress.
     */
    internal func reportProgress(complete: Bool = false) {
      if reportsProgress {
        if complete {
          progressCallback(.complete)
        } else {
          progressCallback(.inProgress)
        }
      }
    }

    internal func reportCancelled() {
      progressCallback(.cancelled)
    }
  }

  private let closureShared: ClosureShared

  let package: Package
  let defaultLgCode: String?
  var associationQueriers: [LanguagePickAssociator]?

  public convenience init(for package: Package,
                          defaultLanguageCode: String? = nil,
                          withAssociators associators: [Associator] = [],
                          progressReceiver: @escaping ProgressReceiver) {
    self.init(for: package,
              defaultLanguageCode: defaultLanguageCode,
              downloadManager: ResourceDownloadManager.shared,
              withAssociators: associators,
              progressReceiver: progressReceiver)
  }

  internal init(for package: Package,
                defaultLanguageCode: String? = nil,
                downloadManager: ResourceDownloadManager = ResourceDownloadManager.shared,
                withAssociators associators: [Associator] = [],
                progressReceiver: @escaping ProgressReceiver) {
    self.package = package
    self.defaultLgCode = defaultLanguageCode

    self.closureShared = ClosureShared(with: associators, downloadManager: downloadManager, receiver: progressReceiver)
  }

  // TODO:  deinit handling in case everything gets cancelled.
  // deinit { }

  private func constructAssociationPickers() {
    // We instantiate this now, indicating that language selection has begun.
    associationQueriers = []

    // So, the for-loop below throws an error for this case.  Yay, Swift weirdnesses.
    if closureShared.associationSpecs.count == 0 {
      return
    }

    // Using a classical for-loop allows us to 'map' receivers (within their closures) to their associators.
    for i in 0 ... closureShared.associationSpecs.count-1 {
      let associationSpec = closureShared.associationSpecs[i]

      closureShared.queryGroup.enter()
      let receiver: LanguagePickAssociator.AssociationReceiver = constructAssociationReceiver(specIndex: i)

      associationQueriers!.append(associationSpec.instance(for: self,
                                                           withSession: closureShared.downloadManager.session,
                                                           withReceiver: receiver))
    }
  }

  private func constructAssociationReceiver(specIndex: Int) -> LanguagePickAssociator.AssociationReceiver {
    let closureShared = self.closureShared

    return { status in
      switch status {
        case .cancelled:
          closureShared.queryGroup.leave()
        // TODO:
        // case .inProgress(let queryCount, let associationsFound):
        //   break
        case .complete(_, let associationMap):
          let installer = closureShared.associationSpecs[specIndex].installer(downloadManager: closureShared.downloadManager)
          closureShared.associationInstallers[specIndex] = installer

          // Since completions will be signaled once per package,
          // we enter the install group once per package.
          associationMap.forEach { _, _ in closureShared.installGroup.enter() }
          closureShared.queryGroup.leave()

          installer(associationMap) { packageKey, result in
            closureShared.progressQueue.async {
              closureShared.installProgressMap[packageKey] = result
              closureShared.reportProgress()

              // Last thing in this closure.
              closureShared.installGroup.leave()
            }
          }
        // Since we aren't yet directly handling case .inProgress.
        default:
          break
      }

      closureShared.progressQueue.async {
        closureShared.associationQueryProgress[specIndex] = status

        if case let .complete(_, associationMap) = status {
          associationMap.forEach { (key, _) in
            // Allows tracking install-completion progress.
            closureShared.installProgressMap[key] = nil  // indicates 'downloading'.
          }
        }

        closureShared.reportProgress()
      }
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
      // The final 'progress report' / completion signaller.
      closureShared.reportProgress(complete: true)
    }
  }

  private func coreInstallationClosure() -> ([Resource.FullID]?) -> Void {
    // For use in closures called by other DispatchGroups.
    let packageKey = self.package.key
    let closureShared = self.closureShared

    // This closure should be triggered from UI, thus from the standard, main UI thread / DispatchGroup.
    return { fullIDs in
      guard let fullIDs = fullIDs else {
        closureShared.isCancelled = true
        closureShared.queryGroup.leave()
        closureShared.reportCancelled()

        return
      }

      closureShared.progressQueue.sync {
        // Indicates a transition to 'installing' state.
        closureShared.pickingCompleted = true
        closureShared.installProgressMap[packageKey] = nil
        closureShared.initializeProgress()
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
        closureShared.reportProgress()
      }

      closureShared.installGroup.leave()
    }
  }

  /**
   * Summons UI that allows the user to interactively select which languages are installed for this package,
   * searching for specified associations in the background and adding them to the installation set.
   *
   * May only be called once during the lifetime of its instance and is mutually exclusive with `pickLanguages`,
   * the programmatic alternative.
   */
  public func promptForLanguages(inNavigationVC navVC: UINavigationController) {
    guard self.associationQueriers == nil, !closureShared.pickingCompleted else {
      fatalError("Invalid state - language picking has already been triggered.")
    }

    initializeSynchronizationGroups()
    constructAssociationPickers()

    let pickerPrompt = PackageInstallViewController<Resource>(for: self.package,
                                                              defaultLanguageCode: defaultLgCode,
                                                              languageAssociators: associationQueriers!,
                                                              completionHandler: coreInstallationClosure())

    navVC.pushViewController(pickerPrompt, animated: true)
  }

  /**
   * Allows programmatically selecting which languages are installed for this package.
   * The specified associations will be used to find related packages and add them to the installation set.
   *
   * May only be called once during the lifetime of its instance and is mutually exclusive with `promptForLanguages`,
   * the user-interactive alternative.
   */
  public func pickLanguages(withCodes languageIDs: Set<String>) {
    guard self.associationQueriers == nil, !closureShared.pickingCompleted else {
      fatalError("Invalid state - language picking has already been triggered.")
    }

    initializeSynchronizationGroups()
    constructAssociationPickers()

    let baseResources: [Resource] = languageIDs.flatMap { package.installables(forLanguage: $0) }
    let baseFullIDs: [Resource.FullID] = baseResources.map { $0.typedFullID }

    self.associationQueriers?.forEach { associator in
      associator.pickerInitialized()
      associator.selectLanguages(languageIDs)
    }

    let installClosure = coreInstallationClosure()
    installClosure(baseFullIDs)

    self.associationQueriers?.forEach { $0.pickerFinalized() }
  }
}
