//
//  LanguagePickAssociator.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 8/3/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

// Important note:  be sure to always call pickerDismissed or pickerFinalized when using this class.
public class LanguagePickAssociator<Resource: LanguageResource> {
  public typealias AssociationSearcher = (Set<String>, @escaping ([String: (KeymanPackage.Key, URL)?]) -> Void) -> Void
  public typealias AssociationReceiver = ([KeymanPackage.Key: Association]) -> Void

  public struct Association {
    public let url: URL
    public let languageCodes: Set<String>
  }

  /**
   * Used to track whether or not a language picker is currently active.
   * Installs will not be started until after this DispatchGroup is completed.
   *
   * Also used to ensure any queued queries actually start before listening
   * for all currently-active queries to complete.
   */
  private let pickerDispatch: DispatchGroup

  /**
   * Used to track whether or not any queries are pending completion.
   *
   * Note that due to internal constraints,`wait()` **must not** be
   * called on this `DispatchGroup` until after `pickerDispatch`'s
   * tasks have completed.
   */
  private let queryDispatch: DispatchGroup

  /**
   * Used as a write-lock for collating query results.
   */
  private let querySyncQueue: DispatchQueue

  /**
   * Used to facilitate concurrent association searches and manage internal concurrency.
   */
  private let associationQueue: DispatchQueue

  /**
   * The currently-picked set of languages
   */
  private var languageSet: Set<String>

  /**
   * The set of languages that have been queried, whether or not they are _currently_ picked.
   */
  private var languageSetSearched: Set<String>

  /**
   * Performs package queries based on picked languages.
   */
  private let searcher: AssociationSearcher

  /**
   * The data structure used to collate query results.
   */
  private var packageMap: [KeymanPackage.Key: Association] = [:]

  /**
   * Used to prevent resource leaks from waiting threads/query processing.
   */
  private var pickerActive: Bool = false
  private let completionClosure: AssociationReceiver

  public init(searchWith searcher: @escaping AssociationSearcher, completion: @escaping AssociationReceiver) {
    pickerDispatch = DispatchGroup()
    queryDispatch = DispatchGroup()

    querySyncQueue = DispatchQueue(label: "com.keyman.associationQueue.sync")
    associationQueue = DispatchQueue(label: "com.keyman.associationQueue.\(String(describing: Resource.self))",
                                     qos: .default,
                                     attributes: .concurrent,
                                     autoreleaseFrequency: .inherit,
                                     target: nil)

    languageSet = Set()
    languageSetSearched = Set()

    self.searcher = searcher

    self.completionClosure = completion
  }

//  deinit {
//    if(pickerActive) {
//      pickerActive = false
//      self.pickerDismissed()
//      self.completionClosure([:])
//    }
//  }  

  public func pickerInitialized() {
    pickerDispatch.enter()
    pickerActive = true

    queryDispatch.enter()
    pickerDispatch.notify(queue: DispatchQueue.main) {
      self.queryDispatch.leave()
    }

    // Establishes the actual 'return' callback for the class asynchronously.
    // We `notify` on the single-threaded queue to ensure all writes (performed
    // exclusively on the same queue) have already completed.
    queryDispatch.notify(queue: querySyncQueue) {
      // With all association info collated, it's time to prepare the actual 'return' values,
      // filtered to only give data regarding the picked languages.
      var associationMap: [KeymanPackage.Key: Association] = [:]

      self.packageMap.forEach { (key, association) in
        let selectedLanguages = association.languageCodes.filter { self.languageSet.contains($0) }

        if selectedLanguages.count > 0 {
          associationMap[key] = Association(url: association.url, languageCodes: selectedLanguages)
        }
      }

      // Now that all association stuff is computed, use the finalization callback
      // Use the 'main' (UI) thread to prevent unexpected UI issues.
      DispatchQueue.main.async {
        self.completionClosure(associationMap)
      }
    }
  }

  public func selectLanguages(_ languages: Set<String>) {
    let unsearched = languages.filter { !languageSetSearched.contains($0) }

    languages.forEach {
      languageSet.insert($0)
      languageSetSearched.insert($0)
    }

    // Start fetch queries for any previously-unsearched language codes!
    if unsearched.count > 0 {
      queueSelection(forLanguages: unsearched)
    }
  }

  internal func queueSelection(forLanguages languages: Set<String>) {
    // We synchronize on this to ensure that the queue starts before pickerDispatch can leave.
    // Not that human inputs _should_ be able to trigger a race condition here,
    // but it's still best to be sure.
    self.pickerDispatch.enter()

    // Queries are allowed to function across multiple threads without issue.
    // They should be embarrasingly parallel until the fetch is complete.
    associationQueue.async {
      self.queryDispatch.enter()
      self.pickerDispatch.leave()

      // Use the closure provided at initialization to search for associated resource packages.
      self.searcher(languages) { map in
        map.forEach { (lgCode, tuple) in
          guard let (packageKey, url) = tuple else {
            return
          }

          // The only non-parallel part - writing to the result cache.
          // We synchronize by evaluating all writes on our single-threaded DispatchQueue.
          self.querySyncQueue.async {
            var languageSet = (self.packageMap[packageKey]?.languageCodes ?? Set())
            languageSet.insert(lgCode)

            self.packageMap[packageKey] = Association(url: url, languageCodes: languageSet)
          }
        }

        // We leave this dispatch group AFTER collating all relevant install requests.
        self.queryDispatch.leave()
      }
    }
  }

  public func deselectLanguages(_ languages: Set<String>) {
    languages.forEach { languageSet.remove($0) }
  }

  public func pickerDismissed() {
    languageSet = Set()
    pickerActive = false
    pickerDispatch.leave()
  }

  public func pickerFinalized() {
    pickerActive = false
    pickerDispatch.leave()
  }
}
