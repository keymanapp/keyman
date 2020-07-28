//
//  LexicalModelKeymanPackage.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/4/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

public class LexicalModelKeymanPackage : TypedKeymanPackage<InstallableLexicalModel> {
  internal var models : [KMPLexicalModel]!

  override internal init(metadata: KMPMetadata, folder: URL) {
    super.init(metadata: metadata, folder: folder)
    self.models = []
    
    if let packagedModels = metadata.lexicalModels {
      for model in packagedModels {
        model.packageId = self.id
        
        // If completely missing, we set the version to 1.0.  Legacy decision from 2005.
        model.setNilVersion(to: metadata.info?.version?.description ?? "1.0")
        if(model.isValid && FileManager.default.fileExists(atPath: self.sourceFolder.appendingPathComponent("\(model.lexicalModelId).model.js").path)) {
          models.append(model)
        } else {
          log.debug("\(model.name) not valid / corresponding file not found")
        }
      }
    }

    self.setInstallableResourceSets(for: models)
  }

  public override func defaultInfoHtml() -> String {
    let count = models.count
    var str = "Found "+(count > 1 ? "\(count) dictionaries" : "dictionary")+" in package:<br/>"
    for model in models {
      str += model.lexicalModelId + "<br/>"
    }
    return str
  }

  override var resources: [AnyKMPResource] {
    return models
  }
}

