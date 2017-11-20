//
//  Collection+SafeAccess.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-13.
//  Copyright © 2017 SIL International. All rights reserved.
//

extension Collection {
  subscript (safe index: Index) -> Element? {
    return indices.contains(index) ? self[index] : nil
  }
}
