//
//  Queries+PackageVersion.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 6/25/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import os.log

extension Queries {
  // Kept internal; the query is 'publicly exposed' through KeymanPackage and ResourceFileManager.
  class PackageVersion {
    class ResultComponent {}

    class ResultEntry: ResultComponent, Decodable {
      let version: String
      let packageURL: String

      enum CodingKeys: String, CodingKey {
        case version
        case packageURL = "kmp"
      }
    }

    class ResultError: ResultComponent, Decodable {
      let error: String

      enum CodingKeys: String, CodingKey {
        case error
      }
    }

    enum Entry {
      case success(ResultEntry)
      case failure(ResultError?)
    }

    struct Result: Decodable {
      let keyboards: [String : ResultComponent]?
      let models: [String : ResultComponent]?

      enum CodingKeys: String, CodingKey {
        case keyboards
        case models
      }

      init(from decoder: Decoder) throws {
        let rootValues = try decoder.container(keyedBy: CodingKeys.self)

        func dictionaryReducer(container: KeyedDecodingContainer<Queries.IDCodingKey>, category: String) -> ([String: ResultComponent], Queries.IDCodingKey) throws -> [String: ResultComponent] {
          return { (dict, id) -> [String: ResultComponent] in
            var dict = dict
            if let entry = try? container.decode(ResultEntry.self, forKey: id) {
              dict[id.stringValue] = entry
            } else if let error = try? container.decode(ResultError.self, forKey: id) {
              dict[id.stringValue] = error
            } else {
              throw FetchError.decodingError(category, id.stringValue)
            }
            return dict
          }
        }

        if rootValues.contains(.keyboards) {
          let keyboardValueSet = try rootValues.nestedContainer(keyedBy: IDCodingKey.self, forKey: .keyboards)
          keyboards = try keyboardValueSet.allKeys.reduce([String: ResultComponent](), dictionaryReducer(container: keyboardValueSet, category: "keyboard"))
        } else {
          keyboards = nil
        }

        if rootValues.contains(.models) {
          let modelValueSet = try rootValues.nestedContainer(keyedBy: IDCodingKey.self, forKey: .models)
          models = try modelValueSet.allKeys.reduce([String: ResultComponent](), dictionaryReducer(container: modelValueSet, category: "lexical model"))
        } else {
          models = nil
        }
      }

      func entryFor(_ key: KeymanPackage.Key) -> Entry {
        var result: ResultComponent?

        switch(key.type) {
          case .keyboard:
            result = keyboards?[key.id]
          case .lexicalModel:
            result = models?[key.id]
        }

        if let entry = result as? ResultEntry {
          return .success(entry)
        } else {
          return .failure(result as? ResultError)
        }
      }
    }

    private static let PACKAGE_VERSION_ENDPOINT = URLComponents(string: "\(KeymanHosts.API_KEYMAN_COM)/package-version")!

    public static func fetch(for packageKeys: [KeymanPackage.Key],
                             withSession session: URLSession = .shared,
                             fetchCompletion: @escaping JSONQueryCompletionBlock<Result>) {
      // Step 1:  build the query
      var urlComponents = PACKAGE_VERSION_ENDPOINT

      let queryItems: [URLQueryItem] = packageKeys.map { key in
        var resourceField: String
        switch(key.type) {
          case .keyboard:
            resourceField = "keyboard"
          case .lexicalModel:
            resourceField = "model"
        }

        return URLQueryItem(name: resourceField, value: key.id)
      }

      urlComponents.queryItems = queryItems + [URLQueryItem(name: "platform", value: "ios")]
      let message = "Querying package versions through API endpoint: \(urlComponents.url!)"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
      SentryManager.breadcrumb(message)

      // Step 2:  configure the completion closure.
      let completionClosure = Queries.jsonDataTaskCompletionAdapter(resultType: Result.self, completionBlock: fetchCompletion)

      // Step 3:  run the actual query, letting the prepared completion closure take care of the rest.
      let task = session.dataTask(with: urlComponents.url!, completionHandler: completionClosure)
      task.resume()
    }
  }
}
