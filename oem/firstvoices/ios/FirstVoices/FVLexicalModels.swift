/*
 * FVLexicalModels.swift
 * FirstVoices app
 *
 * License: MIT
 *
 * Copyright © 2022 FirstVoices.
 *
 * Created by Shawn Schantz on 2022-01-27.
 *
 * Class to call the Keyman API to query about lexical models for a given language.
 *
 */

import UIKit
import KeymanEngine

class FVLexicalModels {
  static let keymanLexicalModelApiUrl = "https://api.keyman.com/model?q=bcp47:"

  class func getAvailableLexicalModels(languageTag: String) -> [FVLexicalModel] {
    var modelArray: [FVLexicalModel] = []
    let keymanApiUrl: URL = URL.init(string: "\(keymanLexicalModelApiUrl)\(languageTag)")!
    // UIApplication.shared.openURL(keymanApiUrl)
    
    do {
      let lexicalModelData = try Data(contentsOf: keymanApiUrl, options: NSData.ReadingOptions())

      do {
          // make sure this JSON is in the format we expect
          if let jsonArray = try JSONSerialization.jsonObject(with: lexicalModelData, options: []) as? [[String : Any]] {
              // try to read array
            if !jsonArray.isEmpty {
              let jsonModelMap = jsonArray.first
              let name = jsonModelMap!["description"] as! String
              let modelId = jsonModelMap!["id"] as! String
              let packageUrl = jsonModelMap!["packageFilename"] as! String
              let version = jsonModelMap!["version"] as! String
              let model = FVLexicalModel(name: name, id: modelId, packageUrl: packageUrl, languageTag: languageTag, version: version)
              modelArray.append(model)
            }
          }
      } catch let error as NSError {
          print("Failed to load: \(error.localizedDescription)")
      }
      print(lexicalModelData)
    } catch {
      // TODO: handle errors
        print(error)
    }
    
    return modelArray
  }
  
  class func downloadModel(keyboard: FVKeyboardState, modelId: String) {
    Manager.shared.downloadLexicalModel(withID: modelId, languageID: keyboard.languageTag, isUpdate: true, fetchRepositoryIfNeeded: true)
  }
}

class FVLexicalModel {
  let name: String
  let id: String
  let packageUrl: URL
  let languageTag: String
  let version: String
  
  internal init(name: String, id: String, packageUrl: String, languageTag: String, version: String) {
    self.name = name
    self.id = id
    self.packageUrl = URL(string: packageUrl)!
    self.languageTag = languageTag
    self.version = version
  }
  
}
