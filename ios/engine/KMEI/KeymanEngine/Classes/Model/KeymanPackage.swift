//
//  KeymanPackage.swift
//  KeymanEngine
//
//  Created by Jacob Bullock on 2/16/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

import Foundation

public enum KMPError : String, Error {
  case invalidPackage = "Invalid Keyman Package."
  case fileSystem = "Unable to create directory structure in file system."
  case copyFiles = "Unable to copy keyboard files to file system."
}

public class KeymanPackage
{
  static private let kmpFile = "kmp.json"
  public var sourceFolder: URL

  init(folder: URL) {
    sourceFolder = folder
  }
    
  // to be overridden by subclasses
  public func isKeyboard() -> Bool {
    return false
  }

  // to be overridden by subclasses
  public func parse(json: [String:AnyObject]) {
    return
  }
  
  // to be overridden by subclasses
  public func defaultInfoHtml() -> String {
    return "base class!"
  }
  
  public func infoHtml() -> String {
    let welcomePath = self.sourceFolder.appendingPathComponent("welcome.htm")
    
    if FileManager.default.fileExists(atPath: welcomePath.path) {
      if let html = try? String(contentsOfFile: welcomePath.path, encoding: String.Encoding.utf8) {
        return html
      }
    }
  
    return defaultInfoHtml()
  }
  
  static public func parse(_ folder: URL) -> KeymanPackage? {
    do {
      var path = folder
      path.appendPathComponent(kmpFile)
      if FileManager.default.fileExists(atPath: path.path) {
        let data = try Data(contentsOf: path, options: .mappedIfSafe)
        let jsonResult = try JSONSerialization.jsonObject(with: data, options: .mutableLeaves)
        if let jsonResult = jsonResult as? [String:AnyObject] {
          if let packagedKeyboards = jsonResult["keyboards"] as? [[String:AnyObject]] {
            if let packagedModels = jsonResult["lexicalModels"] as? [[String:AnyObject]] {
                //TODO: rrb show error to user, for now, just log
              log.error("error parsing keyman package: packages  MUST NOT have both keyboards and lexical models")
              return nil
            }
            let kmp = KeyboardKeymanPackage.init(folder: folder)
            kmp.parse(json: jsonResult)
            return kmp
          }
          else if let packagedModels = jsonResult["lexicalModels"] as? [[String:AnyObject]] {
            let kmm = LexicalModelKeymanPackage.init(folder: folder)
            kmm.parse(json: jsonResult)
            return kmm
          }
        }
      }
    } catch {
      log.error("error parsing keyman package: \(error)")
    }
    
    return nil
  }
  
  static public func extract(fileUrl: URL, destination: URL, complete: @escaping (KeymanPackage?) -> Void)
  {
    Manager.shared.unzipFile(fileUrl: fileUrl, destination: destination) {
      complete(KeymanPackage.parse(destination))
    }
  }
}
