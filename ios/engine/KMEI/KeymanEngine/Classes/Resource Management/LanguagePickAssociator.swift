//
//  LanguagePickAssociator.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 8/3/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

/**
 * This class is designed for use with the `PackageInstallViewController` or
 * other language-picking UI classes.  As languages are selected in a picker, instances of
 * this class will use the provided `AssociationSearcher` to look up associated
 * resources that are compatible.  These searches will be run concurrently in order to
 * reduce waiting time for end users.
 *
 * When the picker signals that language selection is complete, any detected associations
 * will be returned through the instance's provided `AssociationReceiver`.
 * It is then up to the `AssociationReceiver` to download and install these associations.
 *
 * A default `AssociationSearcher` for lexical model lookups is provided as
 * `static let lexicalModelSearcher`.
 */
public class LanguagePickAssociator {
  /**
   * A callback that returns available packages corresponding to provided language codes.
   *
   * This callback may preemptively filter out unwanted associations if desired.  One good example:
   * not searching for associated resources when one of the appropriate type is already installed
   * for the specified language code.
   */
  public typealias AssociationSearcher = (Set<String>, @escaping ([String: (KeymanPackage.Key, URL)?]) -> Void) -> Void

  /**
   * Returns a set of package keys and source URLs found by the provided search closure associated with
   * the selected languages.
   */
  public typealias AssociationReceiver = ([KeymanPackage.Key: Association]) -> Void

  public struct Association {
    public let url: URL
    public let languageCodes: Set<String>
  }

  /**
   * The default AssociationSearcher for lexical model lookups.
   */
  public static let lexicalModelSearcher: AssociationSearcher = constructLexicalModelSearcher(session: URLSession.shared)

  internal static func constructLexicalModelSearcher(session: URLSession) -> AssociationSearcher {
    return { lgCodes, completion in
      // Sadly, we don't yet have an actual multi-language-code server-side model query yet.
      // So, we have to sync up the results of this query first.
      //
      // Should we implement such a server-side query, this becomes markedly simpler.
      let queueDispatch = DispatchGroup()
      var modelMap: [String: (KeymanPackage.Key, URL)?] = [:]

      lgCodes.forEach { lgCode in
        // First, filter out any language codes that already have an installed lexical model.
        let alreadyInstalled = Storage.active.userDefaults.userLexicalModels?.contains(where: { $0.languageID == lgCode}) ?? false

        if(!alreadyInstalled) {
          queueDispatch.enter()
          Queries.LexicalModel.fetchModels(forLanguageCode: lgCode,
                                           withSession: session) { models, error in
            guard error == nil, let models = models, models.count > 0 else {
              modelMap[lgCode] = nil
              queueDispatch.leave()
              return
            }

            modelMap[lgCode] = (models[0].0.packageKey, models[0].1)
            queueDispatch.leave()
          }
        }
      }

      queueDispatch.notify(queue: DispatchQueue.global()) {
        completion(modelMap)
      }
    }
  }

  /**
   * Provides an internal data structure for storing closure-referenced values
   * in order to facilitate proper Swift memory management.
   *
   * If closures using `self` haven't yet been called when the class
   * otherwise goes out of scope, those closures maintain `self` and
   * prevent `deinit`.
   *
   * By moving closure-referenced properties to a class field and creating local
   * variables to reference it as needed, , `self` is no longer kept alive by
   * closures, allowing `deinit` to occur naturally.
   *
   * The 'master' `deinit` may then ensure everything else `deinit`s properly..
   */
  private class ClosureSharedSpace {
    /**
     * Used to track whether or not a language picker is currently active.
     * Installs will not be started until after this DispatchGroup is completed.
     *
     * Also used to ensure any queued queries actually start before listening
     * for all currently-active queries to complete.
     */
    let pickerDispatch: DispatchGroup = DispatchGroup()

    /**
     * Used to track whether or not any queries are pending completion.
     *
     * Note that due to internal constraints,`wait()` **must not** be
     * called on this `DispatchGroup` until after `pickerDispatch`'s
     * tasks have completed.
     */
    let queryDispatch: DispatchGroup = DispatchGroup()

    /**
     * The data structure used to collate query results.
     */
    var packageMap: [KeymanPackage.Key: Association] = [:]

    /**
     * The currently-picked set of languages
     */
    var languageSet: Set<String> = Set()

    /**
     * Used as a write-lock for collating query results.
     */
    let querySyncQueue: DispatchQueue = DispatchQueue(label: "com.keyman.associationQueue.sync")

    /**
     * Performs package queries based on picked languages.
     */
    let searcher: AssociationSearcher

    /**
     * The closure provided to the `LanguagePickAssociator` containing this class
     * for processing any detected language associations.
     */
    let completionClosure: AssociationReceiver

    public init(searcher: @escaping AssociationSearcher, closure: @escaping AssociationReceiver) {
      self.searcher = searcher
      self.completionClosure = closure
    }
  }

  /**
   * Used to facilitate common references across various closures and threads by the class.
   */
  private let closureShared: ClosureSharedSpace

  /**
   * Used to facilitate concurrent association searches and manage internal concurrency.
   */
  private let associationQueue: DispatchQueue

  /**
   * The set of languages that have been queried, whether or not they are _currently_ picked.
   */
  private var languageSetSearched: Set<String>

  /**
   * Used to prevent resource leaks from waiting threads/query processing.
   */
  private var pickerActive: Bool = false

  private static var queueToken: Int = 0

  public init(searchWith searcher: @escaping AssociationSearcher, completion: @escaping AssociationReceiver) {
    closureShared = ClosureSharedSpace(searcher: searcher, closure: completion)

    let queueID = LanguagePickAssociator.queueToken
    LanguagePickAssociator.queueToken += 1
    associationQueue = DispatchQueue(label: "com.keyman.associationQueue.\(queueID)",
                                     qos: .default,
                                     attributes: .concurrent,
                                     autoreleaseFrequency: .inherit,
                                     target: nil)

    languageSetSearched = Set()
  }

  deinit {
    if(pickerActive) {
      // When the class deinits in this state, we have a 'constructive dismissal'.
      // Cancelling all selections and then allowing all closures to complete naturally
      // will facilitate deinit for everything else.
      self.pickerDismissed()
    }
  }

  public func pickerInitialized() {
    let closureShared = self.closureShared
    closureShared.pickerDispatch.enter()
    pickerActive = true

    closureShared.queryDispatch.enter()
    closureShared.pickerDispatch.notify(queue: DispatchQueue.main) {
      closureShared.queryDispatch.leave()
    }

    // Establishes the actual 'return' callback for the class asynchronously.
    // We `notify` on the single-threaded queue to ensure all writes (performed
    // exclusively on the same queue) have already completed.
    closureShared.queryDispatch.notify(queue: closureShared.querySyncQueue) {
      // With all association info collated, it's time to prepare the actual 'return' values,
      // filtered to only give data regarding the picked languages.
      var associationMap: [KeymanPackage.Key: Association] = [:]

      closureShared.packageMap.forEach { (key, association) in
        let selectedLanguages = association.languageCodes.filter { closureShared.languageSet.contains($0) }

        if selectedLanguages.count > 0 {
          associationMap[key] = Association(url: association.url, languageCodes: selectedLanguages)
        }
      }

      // Now that all association stuff is computed, use the finalization callback
      // Use the 'main' (UI) thread to prevent unexpected UI issues.
      DispatchQueue.main.async {
        closureShared.completionClosure(associationMap)
      }
    }
  }

  public func selectLanguages(_ languages: Set<String>) {
    let unsearched = languages.filter { !languageSetSearched.contains($0) }

    languages.forEach {
      closureShared.languageSet.insert($0)
      languageSetSearched.insert($0)
    }

    // Start fetch queries for any previously-unsearched language codes!
    if unsearched.count > 0 {
      queueSelection(forLanguages: unsearched)
    }
  }

  internal func queueSelection(forLanguages languages: Set<String>) {
    let closureShared = self.closureShared

    // We synchronize on this to ensure that the queue starts before pickerDispatch can leave.
    // Not that human inputs _should_ be able to trigger a race condition here,
    // but it's still best to be sure.
    closureShared.pickerDispatch.enter()

    // Queries are allowed to function across multiple threads without issue.
    // They should be embarrasingly parallel until the fetch is complete.
    associationQueue.async {
      closureShared.queryDispatch.enter()
      closureShared.pickerDispatch.leave()

      // Use the closure provided at initialization to search for associated resource packages.
      closureShared.searcher(languages) { map in
        // We allow the searcher to return 'sparse' maps.  If no association is detected,
        // we don't require an entry in the returned map.
        //
        // This allows us to save on context switching with the following if-check.
        // We only need to synchronize when there's something actually worth writing.
        if map.count > 0 {
          // The only non-parallel part - writing to the result cache.
          // We synchronize by evaluating all writes on our single-threaded DispatchQueue.
          closureShared.querySyncQueue.async {
            map.forEach { (lgCode, tuple) in
              guard let (packageKey, url) = tuple else {
                return
              }

              var languageSet = (closureShared.packageMap[packageKey]?.languageCodes ?? Set())
              languageSet.insert(lgCode)

              closureShared.packageMap[packageKey] = Association(url: url, languageCodes: languageSet)
            }
          }
        }

        // We leave this dispatch group AFTER collating all relevant install requests.
        closureShared.queryDispatch.leave()
      }
    }
  }

  public func deselectLanguages(_ languages: Set<String>) {
    languages.forEach { closureShared.languageSet.remove($0) }
  }

  public func pickerDismissed() {
    closureShared.languageSet = Set()
    pickerActive = false

    // Signals that no further queries will start.
    // Once all current queries complete, completionClosure will be called.
    closureShared.pickerDispatch.leave()
  }

  public func pickerFinalized() {
    pickerActive = false

    // Signals that no further queries will start.
    // Once all current queries complete, completionClosure will be called.
    closureShared.pickerDispatch.leave()
  }
}
