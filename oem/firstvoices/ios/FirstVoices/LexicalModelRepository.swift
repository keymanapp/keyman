/*
 * LexicalModelRepository.swift
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

class LexicalModelRepository {

  private static let keymanLexicalModelApiUrl = "https://api.keyman.com/model?q=bcp47:"
  
  // these strings match those in KeymanEngine so that we can share data across UserDefaults (hack!)
  private static let userPredictionSettings = "UserPredictionEnablementSettings"
  private static let userCorrectionSettings = "UserCorrectionEnablementSettings"


  static let shared: LexicalModelRepository  = {
    let instance = LexicalModelRepository()
    return instance
  }()
    
  private init() {}

  func getAvailableLexicalModels(languageTag: String) -> [FVLexicalModel] {
    var modelArray: [FVLexicalModel] = []
    let keymanApiUrl: URL = URL.init(string: "\(LexicalModelRepository.keymanLexicalModelApiUrl)\(languageTag)")!
    
    do {
      let lexicalModelData = try Data(contentsOf: keymanApiUrl, options: NSData.ReadingOptions())

      do {
          // make sure this JSON is in the format we expect
          if let jsonArray = try JSONSerialization.jsonObject(with: lexicalModelData, options: []) as? [[String : Any]] {
              // try to read array
            if !jsonArray.isEmpty {
              let jsonModelMap = jsonArray.first
              let name = jsonModelMap!["name"] as! String
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
        print(error)
    }
    
    // TODO: delete commented-out test code when a keyboard with multiple dictionaries is available
    /*
    // test code for multi dictionary support since no real data exists for this
    // add two fake dictionaries for single language
    if (languageTag == "ikt-latn") {
      var name = "northern dialect"
      var modelId = "nrc.str.sencoten"
      var packageUrl = "https://keyman.com/go/package/download/model/nrc.str.sencoten?version=1.0.5&update=1"
      var version = "1.01"
      var model = FVLexicalModel(name: name, id: modelId, packageUrl: packageUrl, languageTag: languageTag, version: version)
      modelArray.append(model)
      name = "southern dialect"
      modelId = "nrc.str.sencoten"
      packageUrl = "https://keyman.com/go/package/download/model/nrc.str.sencoten?version=1.0.5&update=1"
      version = "1.03"
      model = FVLexicalModel(name: name, id: modelId, packageUrl: packageUrl, languageTag: languageTag, version: version)
      modelArray.append(model)
    }
     */
    return modelArray
  }
  
  /*
   * install = 1) download lexical model and 2) turn on prediction and corrections by default
   */
  func installLexicalModel(keyboardState: KeyboardState, modelId: String) -> Bool {
    self.downloadModel(keyboardState: keyboardState, modelId: modelId)
    
    // call Keyman once after enabling both prediction and correction flags in UserDefaults
    self.writePredictionSettings(languageId: keyboardState.languageTag, modelId: modelId, on: true)
    self.writeCorrectionSettings(languageId: keyboardState.languageTag, modelId: modelId, on: true)
    return self.applyLexicalModelSettings(languageId: keyboardState.languageTag, modelId: modelId)
  }
  
  func disableLexicalModel(keyboardState: KeyboardState, modelId: String) -> Bool {
    self.downloadModel(keyboardState: keyboardState, modelId: modelId)
    
    // call Keyman once after disabling both prediction and correction flags in UserDefaults
    self.writePredictionSettings(languageId: keyboardState.languageTag, modelId: modelId, on: false)
    self.writeCorrectionSettings(languageId: keyboardState.languageTag, modelId: modelId, on: false)
    return self.applyLexicalModelSettings(languageId: keyboardState.languageTag, modelId: modelId)
  }
  
  func downloadModel(keyboardState: KeyboardState, modelId: String) {
    Manager.shared.downloadLexicalModel(withID: modelId, languageID: keyboardState.languageTag,
                        isUpdate: true, fetchRepositoryIfNeeded: true)
  }
  
  // TODO: delete functions for passing data via UserDefaults to KeymanEngine, replace with direct API call
  /*
   * write prediction settings to UserDefaults but do not apply
   */
  func writePredictionSettings(languageId: String, modelId: String, on: Bool) {
    self.writeLexicalModelSettings(userDefaultsKey: LexicalModelRepository.userPredictionSettings,
                              languageId: languageId, modelId: modelId, on: on)
  }

  // TODO: delete functions for passing data via UserDefaults to KeymanEngine, replace with direct API call
  /*
   * write correction settings to UserDefaults but do not apply
   */
  func writeCorrectionSettings(languageId: String, modelId: String, on: Bool) {
    self.writeLexicalModelSettings(userDefaultsKey: LexicalModelRepository.userCorrectionSettings,
                              languageId: languageId, modelId: modelId, on: on)
  }

  // TODO: delete functions for passing data via UserDefaults to KeymanEngine, replace with direct API call
  /*
   * write prediction settings to UserDefaults and apply them
   */
  func applyPredictionSettings(languageId: String, modelId: String, on: Bool) -> Bool {
    self.writeLexicalModelSettings(userDefaultsKey: LexicalModelRepository.userPredictionSettings,
                              languageId: languageId, modelId: modelId, on: on)
    
    // automatically turn off corrections when predictions are turned off
    if (!on) {
      self.writeLexicalModelSettings(userDefaultsKey: LexicalModelRepository.userCorrectionSettings,
                                languageId: languageId, modelId: modelId, on: false)

    }
    return self.applyLexicalModelSettings(languageId: languageId, modelId: modelId)
 }

  // TODO: delete functions for passing data via UserDefaults to KeymanEngine, replace with direct API call
  /*
   * write correction settings to UserDefaults and apply them
   */
  func applyCorrectionSettings(languageId: String, modelId: String, on: Bool) -> Bool {
    self.writeLexicalModelSettings(userDefaultsKey: LexicalModelRepository.userCorrectionSettings,
                              languageId: languageId, modelId: modelId, on: on)
    return self.applyLexicalModelSettings(languageId: languageId, modelId: modelId)
}

  // TODO: delete functions for passing data via UserDefaults to KeymanEngine, replace with direct API call
  /*
   * method to update prediction or correction UserDefaults as specified
   */
  func writeLexicalModelSettings(userDefaultsKey: String, languageId: String, modelId: String, on: Bool) {
    var newSettings: [String:Bool] = [:]
    
    // get current settings for all dictionaries, if they exist in UserDefaults
    let sharedData: UserDefaults = FVShared.userDefaults()
    
    if let settings = sharedData.dictionary(forKey: userDefaultsKey) as? [String : Bool] {
      if settings[languageId] != nil {
        newSettings = settings
      }
    }
    newSettings[languageId] = on
    
    sharedData.set(newSettings, forKey: userDefaultsKey)
  }

  // TODO: delete call to registerLexicalModel, replace with direct API call
 /*
   * call to KeymanEngine registerLexicalModel to force new settings to be applied
   */
  func applyLexicalModelSettings(languageId: String, modelId: String) -> Bool {
    let registered = Manager.shared.registerLexicalModel(lexicalModelId: modelId, languageId: languageId)
    if (!registered) {
      print("Could not register lexical model for prediction settings; lang=\(languageId),  lexical model ID=\(modelId)")
    }
    return registered
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
