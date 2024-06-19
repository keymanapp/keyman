//
//  KeyboardKeymanPackage.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/14/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation
import os.log

public class KeyboardKeymanPackage : TypedKeymanPackage<InstallableKeyboard> {
  internal var keyboards: [KMPKeyboard]!

  override internal init(metadata: KMPMetadata, folder: URL) {
    super.init(metadata: metadata, folder: folder)
    self.keyboards = []

    if let packagedKeyboards = metadata.keyboards {
      for keyboard in packagedKeyboards {
        keyboard.packageId = self.id

        if(keyboard.isValid && FileManager.default.fileExists(atPath: self.sourceFolder.appendingPathComponent("\(keyboard.keyboardId).js").path)) {
          keyboards.append(keyboard)
        } else {
          let message = "\(keyboard.name) not valid / corresponding file not found"
          os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
          SentryManager.breadcrumb(message)
        }
      }
    }

    self.setInstallableResourceSets(for: keyboards)
  }
  
  public override func defaultInfoHtml() -> String {
    let formatString = NSLocalizedString("package-default-found-keyboards", bundle: engineBundle, comment: "See Localized.stringsdict")
    var str = String.localizedStringWithFormat(formatString, keyboards.count) + "<br/>"
    for keyboard in keyboards {
      str += keyboard.keyboardId + "<br/>"
    }
    return str
  }

  override var resources: [AnyKMPResource] {
    return keyboards
  }
}

