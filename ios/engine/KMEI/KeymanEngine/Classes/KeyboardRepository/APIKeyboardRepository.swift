//
//  APIKeyboardRepository.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-12-01.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

public enum APIKeyboardFetchError: Error {
  case networkError(Error)
  case noData
  case parsingError(Error)
}

public class APIKeyboardRepository: KeyboardRepository {
  private let languagesAPIURL = URLComponents(string: "https://api.keyman.com/cloud/4.0/languages")!

  public weak var delegate: KeyboardRepositoryDelegate?
  public private(set) var languages: [String: Language]?
  public private(set) var keyboards: [String: Keyboard]?
  private(set) var options: Options?

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
    languages = Dictionary(uniqueKeysWithValues: result.languages.map { ($0.id, $0) })

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
