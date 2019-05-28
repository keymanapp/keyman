//
//  KMPKeyboard.swift
//  KeymanEngine
//
//  Created by Jacob Bullock on 2/16/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

import Foundation

public struct KMPLanguage {
  public var name: String!
  public var languageId: String!
}

public class KMPKeyboard
{
  public var kmp: KeymanPackage
  
  public var name: String?
  public var keyboardId: String?
  public var version: String! = ""
  public var osk: String?
  public var font: String?
  public var languages: [KMPLanguage]! = []
  public var installableKeyboards: [InstallableKeyboard]! = []
  
  init(kmp: KeymanPackage) {
    self.kmp = kmp
  }

  public var displayFont: Font? {
    if let f = font {
      return Font(filename: f)
    }
    return nil
  }
  
  public var oskFont: Font? {
    if let f = osk {
      return Font(filename: f)
    }
    return nil
  }
  
  public var isValid: Bool {
    return self.name != nil
      && self.keyboardId != nil
      && installableKeyboards.count > 0
      && self.languages.count > 0
      && FileManager.default.fileExists(atPath: kmp.sourceFolder.appendingPathComponent("\(keyboardId!).js").path)
  }
  
  public func parse(json: [String:AnyObject]) {
    if let name = json["name"] as? String {
      self.name = name
    }
    if let keyboardId = json["id"] as? String {
      self.keyboardId = keyboardId
    }
    if let version = json["version"] as? String {
      self.version = version
    }
    
    if let languagesJson = json["languages"] as? [[String:String]] {
      for languageJson in languagesJson {
        if let languageName = languageJson["name"],
           let languageId = languageJson["id"] {
              self.languages.append(KMPLanguage(name: languageName, languageId: languageId))
              break
        }
      }
    }
    
    self.osk = json["oskFont"] as? String
    self.font = json["displayFont"] as? String
    //true if the keyboard targets a right-to-left script. false if absent.
    let isrtl: Bool =  json["rtl"] as? Bool ?? false

    if(self.keyboardId != nil && self.name != nil)
    {
      var installableKeyboards : [InstallableKeyboard] = []
      
      for language in self.languages {
        let keyboard = InstallableKeyboard(id: keyboardId!, name: name!,
                                           languageID: language.languageId,
                                           languageName: language.name,
                                           version: version,
                                           isRTL: isrtl,
                                           font: displayFont,
                                           oskFont: oskFont,
                                           isCustom: true) //update this based on adhoc vs api
        
        installableKeyboards.append( keyboard )
      }
      
      self.installableKeyboards = installableKeyboards
    }
  }
  
  public func install() throws -> Void {
    if !isValid {
      log.error("can't install invalid keyboard")
      throw KMPError.invalidPackage
    }
    
    do {
      try FileManager.default.createDirectory(at: Storage.active.keyboardDir(forID: keyboardId!),
                                              withIntermediateDirectories: true)
    } catch {
      log.error("Could not create dir for download: \(error)")
      throw KMPError.fileSystem
    }
    
    for keyboard in installableKeyboards {
      var installableFiles: [[Any]] = []
      
      let storedPath = Storage.active.keyboardURL(for: keyboard)
      installableFiles.append( ["\(keyboardId!).js", storedPath] )
      
      if let osk = self.osk {
        let oskPath = Storage.active.fontURL(forKeyboardID: keyboardId!, filename: osk)
        installableFiles.append( [osk, oskPath] )
      }
      
      if let font = self.font {
        let displayPath = Storage.active.fontURL(forKeyboardID: keyboardId!, filename: font)
        installableFiles.append( [font, displayPath] )
      }
 
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
      Manager.shared.addKeyboard(keyboard)
    }
  }
}
