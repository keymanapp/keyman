//
//  Queries+LexicalModel.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 7/8/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

extension Queries {
  class LexicalModel {
    struct Result: Decodable {
      let id: String
      let name: String
      let version: String
      let languages: [String]
      let packageFilename: String
      let description: String
      let minKeymanVersion: String

      enum CodingKeys: String, CodingKey {
        case id
        case name
        case version
        case languages
        case packageFilename
        case minKeymanVersion
        case description
      }

      var models: [(InstallableLexicalModel)] {
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

    private static let MODEL_ENDPOINT = URLComponents(string: "https://api.keyman.com/model")!

    public static func fetch(forLanguageCode bcp47: String,
                               withSession session: URLSession = .shared,
                               fetchCompletion: @escaping JSONQueryCompletionBlock<[Result]>) {
      // Step 1:  build the query
      var urlComponents = MODEL_ENDPOINT

      urlComponents.queryItems = [URLQueryItem(name: "q", value: "bcp47:\(bcp47)")]
      log.info("Querying package versions through API endpoint: \(urlComponents.url!)")

      // Step 2:  configure the completion closure.
      let completionClosure = Queries.jsonDataTaskCompletionAdapter(resultType: [Result].self, completionBlock: fetchCompletion)

      // Step 3:  run the actual query, letting the prepared completion closure take care of the rest.
      let task = session.dataTask(with: urlComponents.url!, completionHandler: completionClosure)
      task.resume()
    }
  }
}
