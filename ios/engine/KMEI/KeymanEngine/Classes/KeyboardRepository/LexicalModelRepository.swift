//
//  LexicalModelRepository.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/19/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

public protocol LexicalModelRepository: class {
  typealias CompletionHandler = (Error?) -> Void
  typealias ListCompletionHandler = ([LexicalModel]?, Error?) -> Void // one of the arguments will be nil, depending on success

  var delegate: LexicalModelRepositoryDelegate? { get set }
  var languages: [String: Language]? { get }
  var lexicalModels: [String: LexicalModel]? { get }
  
  func installableLexicalModel(withID lexicalModelID: String, languageID: String) -> InstallableLexicalModel?
  func fetch(completionHandler: CompletionHandler?)
}

public extension LexicalModelRepository {
  func installableLexicalModel(withID lexicalModelID: String, languageID: String) -> InstallableLexicalModel? {
    guard let lexicalModel = lexicalModels?[lexicalModelID] else {
      return nil
    }
  
    // If the lexicalModel (still) supports the requested language, use that one.
    guard let language = lexicalModel.languages.first(where: {$0 == languageID}) ??
        // Otherwise, just use the first language listed for the lexicalModel.
      lexicalModel.languages.first
    else {
        return nil
    }
  
    return InstallableLexicalModel(lexicalModel: lexicalModel, languageID: language, isCustom: false)
  }
  
  func fetch(completionHandler: CompletionHandler? = nil) {
    fetch(completionHandler: completionHandler)
  }
}
