//
//  FontManager.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-11-30.
//  Copyright © 2017 SIL International. All rights reserved.
//

import CoreText
import Foundation
import UIKit // for UIFont
import os.log

public class FontManager {
  public static let shared = FontManager()

  public private(set) var fonts: [URL: RegisteredFont] = [:]

  public func fontName(at url: URL) -> String? {
    if let font = fonts[url] {
      return font.name
    }
    guard let name = readFontName(at: url) else {
      return nil
    }
    fonts[url] = RegisteredFont(name: name, isRegistered: false)
    return name
  }

  /// Registers all new fonts found in the font path. Call this after you have preloaded all your font files
  /// with `preloadFontFile(atPath:shouldOverwrite:)`
  public func registerCustomFonts() {
    guard let keyboardDirs = Storage.active.keyboardDirs else {
      return
    }
    for dir in keyboardDirs {
      registerFonts(in: dir)
    }
  }

  /// Unregisters all registered fonts in the font path.
  public func unregisterCustomFonts() {
    guard let keyboardDirs = Storage.active.keyboardDirs else {
      return
    }
    for dir in keyboardDirs {
      unregisterFonts(in: dir)
    }
  }

  private func readFontName(at url: URL) -> String? {
    guard let provider = CGDataProvider(url: url as CFURL) else {
      let message = "Failed to open \(url)"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
      return nil
    }
    guard let font = CGFont(provider),
      let name = font.postScriptName
    else {
      let message = "Failed to read font at \(url)"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
      return nil
    }
    return name as String
  }

  /// - Parameters:
  ///   - url: URL of the font to register
  /// - Returns: Font is registered.
  public func registerFont(at url: URL) -> Bool {
    let fontName: String
    if let font = fonts[url] {
      if font.isRegistered {
        return true
      }
      fontName = font.name
    } else {
      guard let name = readFontName(at: url) else {
        return false
      }
      fontName = name
    }

    let didRegister: Bool
    if !fontExists(fontName) {
      var errorRef: Unmanaged<CFError>?
      didRegister = CTFontManagerRegisterFontsForURL(url as CFURL, .none, &errorRef)
      let error = errorRef?.takeRetainedValue() // Releases errorRef
      if !didRegister {
        let message = "Failed to register font \(fontName) at \(url) reason: \(String(describing: error))"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
      } else {
        let message = "Registered font \(fontName) at \(url)"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
      }
    } else {
      didRegister = false
      let message = "Did not register font at \(url) because font name \(fontName) is already registered"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
    }
    let font = RegisteredFont(name: fontName, isRegistered: didRegister)
    fonts[url] = font
    return didRegister
  }

  /// - Parameters:
  ///   - url: URL of the font to unregister
  ///   - fromSystemOnly: Do not remove the font from the list that `FontManager` maintains.
  /// - Returns: Font is no longer registered.
  public func unregisterFont(at url: URL, fromSystemOnly: Bool = true) -> Bool {
    guard var font = fonts[url] else {
      return true
    }

    if font.isRegistered {
      var errorRef: Unmanaged<CFError>?
      let didUnregister = CTFontManagerUnregisterFontsForURL(url as CFURL, .none, &errorRef)
      let error = errorRef?.takeRetainedValue() // Releases errorRef
      if didUnregister {
        let message = "Unregistered font \(font.name) at \(url)"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
        font.isRegistered = false
        fonts[url] = font
      } else {
        let message = "Failed to unregister font \(font.name) at \(url) reason: \(String(describing: error))"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
      }
    }

    if !font.isRegistered && !fromSystemOnly {
      fonts[url] = nil
    }

    return font.isRegistered
  }

  public func registerFonts(in directory: URL) {
    guard let urls = try? FileManager.default.contentsOfDirectory(at: directory, includingPropertiesForKeys: nil) else {
      let message = "Could not list contents of directory \(directory)"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
      return
    }
    for url in urls where url.lastPathComponent.hasFontExtension {
      _ = registerFont(at: url)
    }
  }

  public func unregisterFonts(in directory: URL, fromSystemOnly: Bool = true) {
    guard let urls = try? FileManager.default.contentsOfDirectory(at: directory, includingPropertiesForKeys: nil) else {
      let message = "Could not list contents of directory \(directory)"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
      return
    }
    for url in urls where url.lastPathComponent.hasFontExtension {
      _ = unregisterFont(at: url, fromSystemOnly: fromSystemOnly)
    }
  }

  private func fontExists(_ fontName: String) -> Bool {
    return UIFont.familyNames.contains { familyName in
      UIFont.fontNames(forFamilyName: familyName).contains(fontName)
    }
  }
}
