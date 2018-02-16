//
//  KeymanPackage.swift
//  KeymanEngine
//
//  Created by Jacob Bullock on 2/16/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

import Foundation

public class KeymanPackage
{
  static private let kmpFile = "kmp.json"
  public var sourceFolder: URL
  private var keyboards: [KMPKeyboard]!

  init(folder: URL) {
    sourceFolder = folder
  }
  
  public func parse(json: [String:AnyObject]) {
    self.keyboards = []
    
    if let packagedKeyboards = json["keyboards"] as? [[String:AnyObject]] {
      for keyboardJson in packagedKeyboards {
        let keyboard = KMPKeyboard.init(kmp: self)
        keyboard.parse(json: keyboardJson)
        
        if(keyboard.isValid) {
          keyboards.append(keyboard)
        }
      }
    }
  }
  
  public func install() -> Int {
    var count = 0
    for keyboard in self.keyboards {
      count += keyboard.installableKeyboards.count
      keyboard.install()
    }
    
    return count
  }
  
  public func defaultInfoHtml() -> String {
    var str = "Found Packages:<br/>"
    for keyboard in keyboards {
      str += keyboard.keyboardId! + "<br/>"
    }
    return str
  }
  
  public func infoHtml() -> String {
    let welcomePath = self.sourceFolder.appendingPathComponent("Welcome.htm")
    
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
          let kmp = KeymanPackage.init(folder: folder)
          kmp.parse(json: jsonResult)
          return kmp
        }
      }
    } catch {
      print("error parsing kmp")
    }
    
    return nil
  }
  
  static public func extract(fileUrl: URL, destination: URL, complete: @escaping (KeymanPackage?) -> Void)
  {
    print("fileUrl:\(fileUrl)")
    print("destination:\(destination)")
    Manager.shared.unzipFile(fileUrl: fileUrl, destination: destination) {
      complete(KeymanPackage.parse(destination))
    }
  }
}
