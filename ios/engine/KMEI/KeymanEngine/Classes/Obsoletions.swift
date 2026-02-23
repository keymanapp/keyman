//
//  Obsoletions.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 7/24/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

// Obsoleted during development of Keyman 14.0 alpha.

@available(swift, deprecated: 0.1, obsoleted: 0.1, message: "APIKeyboardRepository and associated types have been obsoleted in favor of keyboard search and package-oriented queries.")
public enum APIKeyboardFetchError: Error { }

@available(swift, deprecated: 0.1, obsoleted: 0.1, message: "APIKeyboardRepository and associated types have been obsoleted in favor of keyboard search and package-oriented queries.  If necessary, queries may be built against our api.keyman.com/search endpoint as documented at https://help.keyman.com/developer/cloud/search/1.0/")
public class APIKeyboardRepository { }

@available(swift, deprecated: 0.1, obsoleted: 0.1, message: "APIKeyboardRepository and associated types have been obsoleted in favor of keyboard search and package-oriented queries.")
public protocol KeyboardRepository: class { }

@available(swift, deprecated: 0.1, obsoleted: 0.1, message: "APIKeyboardRepository and associated types have been obsoleted in favor of keyboard search and package-oriented queries.")
public protocol KeyboardRepositoryDelegate: class { }

@available(swift, deprecated: 0.1, obsoleted: 0.1, message: "APILexicalModelRepository and associated types have been obsoleted by `Queries.LexicalModel` and its `fetch` variants.")
public protocol LexicalModelRepository { }

@available(swift, deprecated: 0.1, obsoleted: 0.1, message: "APILexicalModelRepository and associated types have been obsoleted by `Queries.LexicalModel` and its `fetch` variants.")
public enum APILexicalModelFetchError: Error { }

@available(swift, deprecated: 0.1, obsoleted: 0.1, message: "APILexicalModelRepository and associated types have been obsoleted by `Queries.LexicalModel` and its `fetch` variants.")
public class APILexicalModelRepository { }

@available(swift, deprecated: 0.1, obsoleted: 0.1, message: "APILexicalModelRepository and associated types have been obsoleted by `Queries.LexicalModel` and its `fetch` variants.")
public protocol LexicalModelRepositoryDelegate: class { }

@available(swift, deprecated: 0.1, obsoleted: 0.1, message: "This type is no longer utilized by KeymanEngine, as the related API methods have all been obsoleted.")
public struct Options: Codable { }

@available(swift, deprecated: 0.1, obsoleted: 0.1, message: "The old list-based keyboard search is no longer supported.  Please use `KeyboardSearchViewController` instead.")
class LanguageDetailViewController { }

@available(swift, deprecated: 0.1, obsoleted: 0.1, message: "The old list-based keyboard search is no longer supported.  Please use `KeyboardSearchViewController` instead.")
class LanguageViewController { }
