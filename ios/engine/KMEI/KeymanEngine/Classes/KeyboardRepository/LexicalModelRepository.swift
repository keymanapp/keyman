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
  
  var delegate: LexicalModelRepositoryDelegate? { get set }
  var languages: [String: Language]? { get }
  var lexicalModels: [String: LexicalModel]? { get }
  
  func installableLexicalModel(withID lexicalModelID: String, languageID: String) -> InstallableLexicalModel?
  func fetch(completionHandler: CompletionHandler?)
}

public extension LexicalModelRepository {
  public func installableLexicalModel(withID lexicalModelID: String, languageID: String) -> InstallableLexicalModel? {
    guard let lexicalModel = lexicalModels?.first(where: { $0.key == lexicalModelID })?.value else {
      return nil
    }
  
    // If the lexicalModel (still) supports the requested language, use that one.
    guard let language = lexicalModel.languages?.first(where: {$0.id == languageID}) ??
        // Otherwise, just use the first language listed for the lexicalModel.
        lexicalModel.languages?.first ??
        // In cases where the lexicalModel fails to specify any language, use the requested one if it's in the collection.
        languages?[languageID]
    else {
        return nil
    }
  
    return InstallableLexicalModel(lexicalModel: lexicalModel, language: language, isCustom: false)
  }
  
  public func fetch(completionHandler: CompletionHandler? = nil) {
    fetch(completionHandler: completionHandler)
  }
}
