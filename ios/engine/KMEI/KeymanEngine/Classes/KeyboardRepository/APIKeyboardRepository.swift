//
//  APIKeyboardRepository.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-12-01.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation
import UIKit

@available(*, deprecated, message: "APIKeyboardRepository use is no longer recommended; interactive user selection of keyboards is now facilitated by KeyboardSearchViewController.")
public enum APIKeyboardFetchError: Error {
  case duplicateLanguageCodes
  case networkError(Error)
  case noData
  case parsingError(Error)
}

// The primary reason we aren't obsoleting this and its associated protocols & classes outright
// is that there are no direct replacements for them within KeymanEngine at present.
// Should we add a keyboard-search query like the one we have for lexical models,
// we may then obsolete them.
@available(*, deprecated, message: "APIKeyboardRepository use is no longer recommended; interactive user selection of keyboards is now facilitated by KeyboardSearchViewController.")
public class APIKeyboardRepository: KeyboardRepository {
  private let languagesAPIURL = URLComponents(string: "\(KeymanHosts.API_KEYMAN_COM)/cloud/4.0/languages")!

  public weak var delegate: KeyboardRepositoryDelegate?
  public private(set) var languages: [String: Language]?
  public private(set) var keyboards: [String: Keyboard]?
  private(set) var options: Options?

  private func resultsToDictionary(for result: LanguagesAPICall, handler: (_ error: Error) -> Void) -> Dictionary<String, Language>? {
    // Detect duplicate ids - this scenario indicates an infrastructure problem.
    var dictionary = Dictionary<String, Language>()
    var duplicates: [String] = []

    result.languages.forEach { language in
      // Does an entry for this ID already exist?  Trouble if so.
      if let _ = dictionary[language.id] {
        if duplicates.firstIndex(of: language.id) != -1 {
          duplicates.append(language.id)
        }
      }

      dictionary.updateValue(language, forKey: language.id)
    }

    guard duplicates.count == 0 else {
      // This indicates an issue in our keyboard API infrastructure.
      // Don't let it crash the app, but definitely make note of it
      // as something to be fixed.
      log.error("Corrupt data detected in API returns - duplicate language codes: \(duplicates)")
      handler(APIKeyboardFetchError.duplicateLanguageCodes)
      return nil
    }

    return dictionary
  }

  public func fetch(completionHandler: CompletionHandler?) {
    let deviceType = UIDevice.current.userInterfaceIdiom == .phone ? "iphone" : "ipad"
    let keymanVersion = Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String
    var urlComponents = languagesAPIURL
    urlComponents.queryItems = [
      URLQueryItem(name: "dateformat", value: "seconds"),
      URLQueryItem(name: "device", value: deviceType),
      URLQueryItem(name: "version", value: keymanVersion),
      URLQueryItem(name: "languageidtype", value: "bcp47")
    ]
    log.info("Connecting to Keyman cloud: \(urlComponents.url!).")
    let task = URLSession.shared.dataTask(with: urlComponents.url!) { (data, response, error) in
      self.apiCompletionHandler(data: data, response: response, error: error,
                                fetchCompletionHandler: completionHandler)
    }
    task.resume()
  }

  private func apiCompletionHandler(data: Data?,
                                    response: URLResponse?,
                                    error: Error?,
                                    fetchCompletionHandler: CompletionHandler?) {
    let errorHandler = { (error: Error) -> Void in
      DispatchQueue.main.async {
        self.delegate?.keyboardRepository(self, didFailFetch: error)
        fetchCompletionHandler?(error)
      }
    }

    if let error = error {
      log.error("Network error fetching languages: \(error)")
      errorHandler(APIKeyboardFetchError.networkError(error))
      return
    }
    guard let data = data else {
      log.error("Language API did not return data")
      errorHandler(APIKeyboardFetchError.noData)
      return
    }

    let decoder = JSONDecoder()
    decoder.dateDecodingStrategy = .secondsSince1970
    let result: LanguagesAPICall
    do {
      result = try decoder.decode(LanguagesAPICall.self, from: data)
    } catch {
      log.error("Failed parsing API languages: \(error)")
      errorHandler(APIKeyboardFetchError.parsingError(error))
      return
    }

    options = result.options

    // Detect duplicate ids - this scenario indicates an infrastructure problem.
    guard let dictionary = resultsToDictionary(for: result, handler: errorHandler) else {
      // The helper performs any relevant error handling on our behalf.
      return
    }

    // Now that we've successfully processed the API returns, assign
    // the results to the proper field.
    languages = dictionary

    let keyboardsWithID = result.languages.flatMap { language in
      language.keyboards?.map { kb in (kb.id, kb) } ?? []
    }
    keyboards = Dictionary(keyboardsWithID) { old, new in
      var kb = old
      if old.languages == nil {
        kb.languages = new.languages
        return kb
      }
      if let newLanguages = new.languages {
        let oldLanguageIDs = Set(old.languages!.map { $0.id })
        kb.languages!.append(contentsOf: newLanguages.filter { !oldLanguageIDs.contains($0.id) })
      }
      return kb
    }

    log.info("Request completed -- \(result.languages.count) languages.")
    DispatchQueue.main.async {
      self.delegate?.keyboardRepositoryDidFetch(self)
      fetchCompletionHandler?(nil)
    }
  }
}
