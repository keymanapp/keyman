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

@available(*, deprecated)
public class APILexicalModelRepository: LexicalModelRepository {
  private let modelsAPIURL = URLComponents(string: "\(KeymanHosts.API_KEYMAN_COM)/model")!
  
  public weak var delegate: LexicalModelRepositoryDelegate?
  public private(set) var languages: [String: Language]?
  public private(set) var lexicalModels: [String: LexicalModel]?
  
  public func fetch(completionHandler: CompletionHandler?) {
    var urlComponents = modelsAPIURL
    urlComponents.queryItems = [
      URLQueryItem(name: "q", value: "")
    ]
    log.info("Connecting to Keyman cloud: \(urlComponents.url!).")
    let task = URLSession.shared.dataTask(with: urlComponents.url!) { (data, response, error) in
      self.apiCompletionHandler(data: data, response: response, error: error,
                                fetchCompletionHandler: completionHandler)
    }
    task.resume()
  }

  
  public func fetchList(languageID: String, completionHandler: @escaping ListCompletionHandler) {
    var urlComponents = modelsAPIURL
    let bcp47Value = "bcp47:" + languageID
    urlComponents.queryItems = [
      URLQueryItem(name: "q", value: bcp47Value)
    ]
    log.info("Connecting to Keyman cloud: \(urlComponents.url!).")
    let task = URLSession.shared.dataTask(with: urlComponents.url!) { (data, response, error) in
      self.apiListCompletionHandler(data: data, response: response, error: error,
                                fetchCompletionHandler: completionHandler)
    }
    task.resume()
  }
  
  private func commonErrorCheck(data: Data?, response: URLResponse?, error: Error?, errorForwarder: @escaping (_ error: Error) -> Void) -> LexicalModelAPICall? {
    let errorHandler = { (error: Error) -> Void in
      DispatchQueue.main.async {
        self.delegate?.lexicalModelRepository(self, didFailFetch: error)
        errorForwarder(error)
      }
    }
  
    if let error = error {
      log.error("Network error fetching languages: \(error)")
      errorHandler(APILexicalModelFetchError.networkError(error))
      return nil
    }
    guard let data = data else {
      log.error("Language API did not return data")
      errorHandler(APILexicalModelFetchError.noData)
      return nil
    }
    
    let decoder = JSONDecoder()
    decoder.dateDecodingStrategy = .secondsSince1970
    let result: LexicalModelAPICall
    do {
      result = try decoder.decode(LexicalModelAPICall.self, from: data)
    } catch {
      log.error("Failed parsing API lexical models: \(error)")
      errorHandler(APILexicalModelFetchError.parsingError(error))
      return nil
    }
    
    // No errors!
    return result
  }
  
  private func apiCompletionHandler(data: Data?,
                                    response: URLResponse?,
                                    error: Error?,
                                    fetchCompletionHandler: CompletionHandler?) {
    let errorHandler = { (error: Error) -> Void in
        fetchCompletionHandler?(error)
    }
    
    guard let result = commonErrorCheck(data: data, response: response, error: error, errorForwarder: errorHandler) else {
      return
    }
    
    let lexicalModels = result.lexicalModels // Simpler debugging this way.
    self.lexicalModels = Dictionary(uniqueKeysWithValues: lexicalModels.map { ($0.id, $0) })

    
    log.info("Request list completed -- \(result.lexicalModels.count) lexical models.")
    DispatchQueue.main.async {
      self.delegate?.lexicalModelRepositoryDidFetchList(self)
      fetchCompletionHandler?(nil)
    }
  }
  
  private func apiListCompletionHandler(data: Data?,
                                    response: URLResponse?,
                                    error: Error?,
                                    fetchCompletionHandler: @escaping ListCompletionHandler) {
    let errorHandler = { (error: Error) -> Void in
        fetchCompletionHandler(nil, error)
    }
    
    guard let result = commonErrorCheck(data: data, response: response, error: error, errorForwarder: errorHandler) else {
      return
    }
    
    let lexicalModels = result.lexicalModels
    self.lexicalModels = Dictionary(uniqueKeysWithValues: result.lexicalModels.map { ($0.id, $0) })

    
    log.info("Request list completed -- \(result.lexicalModels.count) lexical models.")
    DispatchQueue.main.async {
      self.delegate?.lexicalModelRepositoryDidFetchList(self)
      fetchCompletionHandler(lexicalModels, nil)
    }
  }
}
