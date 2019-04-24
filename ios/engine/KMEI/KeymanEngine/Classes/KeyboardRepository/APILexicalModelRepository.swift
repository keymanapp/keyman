//
//  APILexicalModelRepository.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/19/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation
import UIKit // for UIDevice

public enum APILexicalModelFetchError: Error {
  case networkError(Error)
  case noData
  case parsingError(Error)
}

public class APILexicalModelRepository: LexicalModelRepository {
  private let modelsAPIURL = URLComponents(string: "https://api.keyman.com/model")! //?q=bcp47:en
  
  public weak var delegate: LexicalModelRepositoryDelegate?
  public private(set) var languages: [String: Language]?
  public private(set) var lexicalModels: [String: LexicalModel]?
  
  public func fetch(completionHandler: CompletionHandler?) {
    let deviceType = UIDevice.current.userInterfaceIdiom == .phone ? "iphone" : "ipad"
    let keymanVersion = Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String
    var urlComponents = modelsAPIURL
    urlComponents.queryItems = [
      URLQueryItem(name: "q", value: "bcp47:en") //+languages?[0]
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
        self.delegate?.lexicalModelRepository(self, didFailFetch: error)
        fetchCompletionHandler?(error)
      }
    }
    
    if let error = error {
      log.error("Network error fetching languages: \(error)")
      errorHandler(APILexicalModelFetchError.networkError(error))
      return
    }
    guard let data = data else {
      log.error("Language API did not return data")
      errorHandler(APILexicalModelFetchError.noData)
      return
    }
    
    let decoder = JSONDecoder()
    decoder.dateDecodingStrategy = .secondsSince1970
    let result: LanguagesAPICall
    do {
      result = try decoder.decode(LanguagesAPICall.self, from: data)
    } catch {
      log.error("Failed parsing API languages: \(error)")
      errorHandler(APILexicalModelFetchError.parsingError(error))
      return
    }
    
    languages = Dictionary(uniqueKeysWithValues: result.languages.map { ($0.id, $0) })
    
    let lexicalModelsWithID = result.languages.flatMap { language in
      language.lexicalModels?.map { lm in (lm.id, lm) } ?? []
    }
    lexicalModels = Dictionary(lexicalModelsWithID) { old, new in
      var lm = old
      if old.languages == nil {
        lm.languages = new.languages
        return lm
      }
      if let newLanguages = new.languages {
        let oldLanguageIDs = Set(old.languages!.map { $0 })
        lm.languages!.append(contentsOf: newLanguages.filter { !oldLanguageIDs.contains($0) })
      }
      return lm
    }
    
    log.info("Request completed -- \(result.languages.count) languages.")
    DispatchQueue.main.async {
      self.delegate?.lexicalModelRepositoryDidFetch(self)
      fetchCompletionHandler?(nil)
    }
  }
}
