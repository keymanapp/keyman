//
//  KeymanPackage.swift
//  KeymanEngine
//
//  Created by Jacob Bullock on 2/16/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

import Foundation
import Zip

public enum KMPError : String, Error {
  case noMetadata = "Could not find kmp.json for package."
  case invalidPackage = "Invalid Keyman Package."
  case fileSystem = "Unable to create directory structure in file system."
  case copyFiles = "Unable to copy keyboard files to file system."
}

public class KeymanPackage {
  static private let kmpFile = "kmp.json"
  public let sourceFolder: URL
  public let metadata: KMPMetadata

  init(metadata: KMPMetadata, folder: URL) {
    sourceFolder = folder
    self.metadata = metadata
  }
    
  // to be overridden by subclasses
  public func isKeyboard() -> Bool {
    return metadata.packageType == .Keyboard
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

  public var version: String? {
    return metadata.version
  }
  
  static public func parse(_ folder: URL) -> KeymanPackage? {
    do {
      var path = folder
      path.appendPathComponent(kmpFile)
      if FileManager.default.fileExists(atPath: path.path) {
        let data = try Data(contentsOf: path, options: .mappedIfSafe)
        let decoder = JSONDecoder()

        var metadata: KMPMetadata

        do {
          metadata = try decoder.decode(KMPMetadata.self, from: data)
        } catch {
          throw KMPError.noMetadata
        }

        switch metadata.packageType {
          case .Keyboard:
            return KeyboardKeymanPackage(metadata: metadata, folder: folder)
          case .LexicalModel:
            return LexicalModelKeymanPackage(metadata: metadata, folder: folder)
          default:
            throw KMPError.invalidPackage
        }
      }
    } catch {
      log.error("error parsing keyman package: \(error)")
    }
    
    return nil
  }
  
  static public func extract(fileUrl: URL, destination: URL, complete: @escaping (KeymanPackage?) -> Void) throws {
    try unzipFile(fileUrl: fileUrl, destination: destination) {
      complete(KeymanPackage.parse(destination))
    }
  }

  static public func unzipFile(fileUrl: URL, destination: URL, complete: @escaping () -> Void) throws {
    try Zip.unzipFile(fileUrl, destination: destination, overwrite: true,
                      password: nil,
                      progress: { (progress) -> () in
                        //TODO: add timeout
                        if(progress == 1.0) {
                          complete()
                        }
                      })
  }
}
