//
//  Queries.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 6/25/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

// The core `Queries` definition provides common utility methods, handlers, etc across our API queries.
class Queries {
  public enum FetchError: LocalizedError {
    case networkError(Error)
    case noData
    case parsingError(Error)

    var localizedDescription: String {
      switch self {
        case .noData:
          return "no data returned"
        case .networkError(let baseError):
          return "network error occurred - \(String(describing: baseError))"
        case .parsingError(let baseError):
          return "failure occurred when parsing results - \(String(describing: baseError))"
      }
    }
  }

  public enum ResultError: LocalizedError {
    case unqueried

    var localizedDescription: String {
      switch self {
        case .unqueried:
          return "Query was not run against specified parameter"
      }
    }
  }

  public struct IDCodingKey: CodingKey {
    /* Required by CodingKey, though we don't particularly need these */
    public var intValue: Int?

    public init?(intValue: Int) {
      self.init(stringValue: "\(intValue)")
      self.intValue = intValue
    }
    /* End section */

    var stringValue: String

    init?(stringValue: String) {
      self.stringValue = stringValue
    }
  }

  typealias JSONQueryCompletionBlock<T: Decodable> = (_: T?, _: Queries.FetchError?) -> Void
  typealias DataTaskCompletionBlock = (_: Data?, _: URLResponse?, _: Error?) -> Void

  /**
   * Returns a closure that wraps closures expecting a decodable type,
   * performing standard error checking and decoding the query's results
   * on behalf of the wrapped closure.
   *
   * Returns either the decoded result when successful or the error that
   * occurred when attempting to run the query.
   */
  internal static func jsonDataTaskCompletionAdapter<T: Decodable>(resultType: T.Type, completionBlock: @escaping JSONQueryCompletionBlock<T>) -> DataTaskCompletionBlock {
    return { data, response, baseError in
      guard baseError == nil else {
        DispatchQueue.main.async { completionBlock(nil, .networkError(baseError!)) }
        return
      }

      guard let data = data else {
        DispatchQueue.main.async { completionBlock(nil, .noData) }
        return
      }

      let decoder = JSONDecoder()
      decoder.dateDecodingStrategy = .secondsSince1970

      do {
        let result = try decoder.decode(resultType, from: data)
        DispatchQueue.main.async { completionBlock(result, nil) }
      } catch {
        DispatchQueue.main.async { completionBlock(nil, .parsingError(error)) }
      }
    }
  }
}
