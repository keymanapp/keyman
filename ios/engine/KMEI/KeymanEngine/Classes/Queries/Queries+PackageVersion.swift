//
//  Queries+PackageVersion.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 6/25/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

extension Queries {
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
              throw NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: "Unexpected value contained during \(category) decoding, id \(id.stringValue)"])
            }
            return dict
          }
        }

        let keyboardValueSet = try rootValues.nestedContainer(keyedBy: IDCodingKey.self, forKey: .keyboards)
        keyboards = try keyboardValueSet.allKeys.reduce([String: ResultComponent](), dictionaryReducer(container: keyboardValueSet, category: "keyboard"))

        let modelValueSet = try rootValues.nestedContainer(keyedBy: IDCodingKey.self, forKey: .models)
        models = try modelValueSet.allKeys.reduce([String: ResultComponent](), dictionaryReducer(container: modelValueSet, category: "lexical model"))
      }

      func entryFor<FullID: LanguageResourceFullID>(_ fullID: FullID) -> Entry {
        var result: ResultComponent?
        if let fullID = fullID as? FullKeyboardID, let keyboards = self.keyboards {
          result = keyboards[fullID.keyboardID]
        } else if let fullID = fullID as? FullLexicalModelID, let models = self.models {
          result = models[fullID.lexicalModelID]
        }

        if let entry = result as? ResultEntry {
          return .success(entry)
        } else {
          return .failure(result as? ResultError)
        }
      }
    }

    private static let PACKAGE_VERSION_ENDPOINT = URLComponents(string: "https://api.keyman.com/package-version")!

    public static func fetch(for fullIDs: [AnyLanguageResourceFullID],
                             withSession session: URLSession = .shared,
                             fetchCompletion: @escaping JSONQueryCompletionBlock<Result>) {
      // Step 1:  build the query
      var urlComponents = PACKAGE_VERSION_ENDPOINT

      let queryItems: [URLQueryItem] = fullIDs.map { fullID in
        var resourceField: String
        switch(fullID.type) {
          case .keyboard:
            resourceField = "keyboard"
          case .lexicalModel:
            resourceField = "model"
        }

        return URLQueryItem(name: resourceField, value: fullID.id)
      }

      urlComponents.queryItems = queryItems + [URLQueryItem(name: "platform", value: "ios")]
      log.info("Querying package versions through API endpoint: \(urlComponents.url!)")

      // Step 2:  configure the completion closure.
      let completionClosure = Queries.jsonDataTaskCompletionAdapter(resultType: Result.self, completionBlock: fetchCompletion)

      // Step 3:  run the actual query, letting the prepared completion closure take care of the rest.
      let task = session.dataTask(with: urlComponents.url!, completionHandler: completionClosure)
      task.resume()
    }
  }
}
