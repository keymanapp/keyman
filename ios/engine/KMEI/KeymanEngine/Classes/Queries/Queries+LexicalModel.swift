//
//  Queries+LexicalModel.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 7/8/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import os.log

extension Queries {
  public class LexicalModel {
    public struct Result: Decodable {
      let id: String
      let name: String
      let version: String
      let languages: [String]
      let packageFilename: String
      let minKeymanVersion: String

      enum CodingKeys: String, CodingKey {
        case id
        case name
        case version
        case languages
        case packageFilename
        case minKeymanVersion
      }

      var models: [InstallableLexicalModel] {
        return languages.map { modelFor(languageID: $0)! }
      }

      func modelFor(languageID: String) -> InstallableLexicalModel? {
        if languages.contains(languageID) {
          return InstallableLexicalModel(id: id, name: name, languageID: languageID, version: version, isCustom: false)
        } else {
          return nil
        }
      }
    }

    private static let MODEL_ENDPOINT = URLComponents(string: "\(KeymanHosts.API_KEYMAN_COM)/model")!

    public static func fetch(forLanguageCode bcp47: String,
                             fetchCompletion: @escaping JSONQueryCompletionBlock<[Result]>) {
      fetch(forLanguageCode: bcp47, withSession: URLSession.shared, fetchCompletion: fetchCompletion)
    }

    internal static func fetch(forLanguageCode bcp47: String,
                               withSession session: URLSession,
                               fetchCompletion: @escaping JSONQueryCompletionBlock<[Result]>) {
      // Step 1:  build the query
      var urlComponents = MODEL_ENDPOINT

      urlComponents.queryItems = [URLQueryItem(name: "q", value: "bcp47:\(bcp47)")]
      let message = "Querying package versions through API endpoint: \(urlComponents.url!)"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
      SentryManager.breadcrumb(message)

      // Step 2:  configure the completion closure.
      let completionClosure = Queries.jsonDataTaskCompletionAdapter(resultType: [Result].self, completionBlock: fetchCompletion)

      // Step 3:  run the actual query, letting the prepared completion closure take care of the rest.
      let task = session.dataTask(with: urlComponents.url!, completionHandler: completionClosure)
      task.resume()
    }

    /**
     * Returns an array of package keys containing models that support the specified language.
     */
    public static func fetchModels(forLanguageCode bcp47: String,
                                        fetchCompletion: @escaping ([(InstallableLexicalModel, URL)]?, Error?) -> Void) {
      fetchModels(forLanguageCode: bcp47, withSession: URLSession.shared, fetchCompletion: fetchCompletion)
    }

    internal static func fetchModels(forLanguageCode bcp47: String,
                                     withSession session: URLSession,
                                     fetchCompletion: @escaping ([(InstallableLexicalModel, URL)]?, Error?) -> Void) {
      Queries.LexicalModel.fetch(forLanguageCode: bcp47, withSession: session) { result, error in
        if let error = error {
          fetchCompletion(nil, error)
          return
        }

        guard let result = result, result.count > 0 else {
          fetchCompletion([], nil)
          return
        }

        // There are valid packages for the language code - send off the report!
        fetchCompletion(result.map { ($0.modelFor(languageID: bcp47)!, URL.init(string: $0.packageFilename)!) }, nil)
      }
    }
  }
}
