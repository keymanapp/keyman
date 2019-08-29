//
//  KMPLexicalModel.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/12/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

//KMPLanguage in KMPKeyboard.swift

public class KMPLexicalModel
{
  public var kmp: LexicalModelKeymanPackage
  
  public var name: String?
  public var lexicalModelId: String?
  public var version: String! = ""
  public var languages: [KMPLanguage]! = []
  public var installableLexicalModels: [InstallableLexicalModel]! = []
  
  init(kmp: LexicalModelKeymanPackage) {
    self.kmp = kmp
  }
  
  public var isValid: Bool {
    return self.name != nil
      && self.lexicalModelId != nil
      && installableLexicalModels.count > 0
      && self.languages.count > 0
      && FileManager.default.fileExists(atPath: kmp.sourceFolder.appendingPathComponent("\(lexicalModelId!).model.js").path)
  }
  
  public func parse(json: [String:AnyObject]) {
    if let name = json["name"] as? String {
      self.name = name
    }
    if let lexicalModelId = json["id"] as? String {
      self.lexicalModelId = lexicalModelId
    }
    if let version = json["version"] as? String {
      self.version = version
    }
    
    if let languagesJson = json["languages"] as? [[String:String]] {
      for languageJson in languagesJson {
        if let languageName = languageJson["name"],
          let languageId = languageJson["id"] {
          self.languages.append(KMPLanguage(name: languageName, languageId: languageId))
        }
      }
    }
    
    if(self.lexicalModelId != nil && self.name != nil)
    {
      var installableLexicalModels : [InstallableLexicalModel] = []
      
      for language in self.languages {
        let lexicalModel = InstallableLexicalModel(id: lexicalModelId!, name: name!,
                                                   languageID: language.languageId,
//                                                   languageName: language.name,
                                                   version: version,
                                                   isCustom: true) //update this based on adhoc vs api
        
        installableLexicalModels.append( lexicalModel )
      }
      
      self.installableLexicalModels = installableLexicalModels
    }
  }
  
  public func install() throws -> Void {
    if !isValid {
      log.error("can't install invalid lexical model")
      throw KMPError.invalidPackage
    }
    
    do {
      try FileManager.default.createDirectory(at: Storage.active.lexicalModelDir(forID: lexicalModelId!), withIntermediateDirectories: true)
    } catch {
      log.error("Could not create dir for download: \(error)")
      throw KMPError.fileSystem
    }
    
    for lexicalModel in installableLexicalModels {
      var installableFiles: [[Any]] = []
      
      let storedPath = Storage.active.lexicalModelURL(for: lexicalModel)
      installableFiles.append( ["\(lexicalModelId!).js", storedPath] )
      
      for item in installableFiles {
        var filePath = self.kmp.sourceFolder
        filePath.appendPathComponent(item[0] as! String)
        do {
          if(FileManager.default.fileExists(atPath: (item[1] as! URL).path)) {
            try FileManager.default.removeItem(at: item[1] as! URL)
          }
          try FileManager.default.copyItem(at: filePath,
                                           to: item[1] as! URL)
        } catch {
          log.error("Error saving the download: \(error)")
          throw KMPError.copyFiles
        }
      }
      Manager.addLexicalModel(lexicalModel)
    }
  }
}

