//
//  LexicalModelKeymanPackage.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/4/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation
import os.log

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
          let message = "\(model.name) not valid / corresponding file not found"
          os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
          SentryManager.breadcrumb(message)
        }
      }
    }
    
    self.setInstallableResourceSets(for: models)
  }
  
  public override func defaultInfoHtml() -> String {
    let formatString = NSLocalizedString("package-default-found-lexical-models", bundle: engineBundle, comment: "See Localized.stringsdict")
    var str = String.localizedStringWithFormat(formatString, models.count) + "<br/>"
    for model in models {
      str += model.lexicalModelId + "<br/>"
    }
    return str
  }
  
  override var resources: [AnyKMPResource] {
    return models
  }
}

