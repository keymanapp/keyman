//
//  LexicalModelRepositoryDelegate.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/19/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

public protocol LexicalModelRepositoryDelegate: class {
  func lexicalModelRepositoryDidFetch(_ repository: LexicalModelRepository)
  func lexicalModelRepository(_ repository: LexicalModelRepository, didFailFetch error: Error)
}
