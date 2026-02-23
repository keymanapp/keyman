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
  
  private func cachedFont(at url: URL) -> RegisteredFont? {
    if let font = fonts[url] {
      return font
    }
    guard let name = readFontName(at: url) else {
      return nil
    }
    fonts[url] = RegisteredFont(name: name, isRegistered: false)
    return fonts[url]
  }

  /// Registers all new fonts found in the font path. Call this after you have preloaded all your font files
  /// with `preloadFontFile(atPath:shouldOverwrite:)`
  public func registerCustomFonts() {
    guard let keyboardDirs = Storage.active.keyboardDirs else {
      return
    }
    
    var fontSet: Set<URL> = []
    for dir in keyboardDirs {
      fontSet = fontSet.union(listFonts(in: dir))
    }
    
    registerListedFonts(fontSet)
  }
  
  /**
   * Iterates across all listed fonts, registering those not yet registered.
   * Checks for, and filters out, any fonts already registered on the system.
   * Also initializes the registration cache per URL if needed.
   */
  private func registerListedFonts(_ initialFontSet: Set<URL>) {
    // If we are unable to read the font file's properties sufficiently,
    // skip it.  We also don't need to register anything already registered or
    // that cannot be registered due to loading/parsing errors.
    //
    // Calls to `cachedFont` after the `.filter` below may be assumed to have
    // non-nil return values.
    var fontSet = initialFontSet.filter { !(cachedFont(at: $0)?.isRegistered ?? true) }
    
    // The prior line filters out any entries where cachedFont(at: $0) would be nil.
    // Batch-lookups all fonts lacking cache-confirmation of prior registration.
    var fontNamesToRegister = missingFonts(from: Set(fontSet.map { cachedFont(at: $0)!.name }))
    
    for fontUrl in fontSet {
      let fontName = cachedFont(at: fontUrl)!.name
      
      guard fontNamesToRegister.contains(fontName) else {
        let message = "Did not register font at \(fontUrl) because font name \(fontName) is already registered"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
        continue
      }
      
      let didRegister = _registerFont(at: fontUrl)
      fonts[fontUrl] = RegisteredFont(name: fontName, isRegistered: didRegister)
      
      // We no longer need to register a font with this name, so drop it from
      // the set to register.
      if didRegister {
        fontNamesToRegister.remove(fontName)
      }
    }
  }

  /// Unregisters all registered fonts in the font path.
  public func unregisterCustomFonts() {
    guard let keyboardDirs = Storage.active.keyboardDirs else {
      return
    }
    // This doesn't use the expensive looped lookup operation seen in missingFonts,
    // so there's no need to batch similar operations here.
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
  
  private func _registerFont(at url: URL) -> Bool {
    var errorRef: Unmanaged<CFError>?
    let fontName = fontName(at: url)!
    let didRegister = CTFontManagerRegisterFontsForURL(url as CFURL, .none, &errorRef)
    let error = errorRef?.takeRetainedValue() // Releases errorRef
    if !didRegister {
      let message = "Failed to register font \(fontName) at \(url) reason: \(String(describing: error))"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
    } else {
      let message = "Registered font \(fontName) at \(url)"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
    }
    
    return didRegister
  }

  /// - Parameters:
  ///   - url: URL of the font to register
  /// - Returns: Font is registered.
  public func registerFont(at url: URL) -> Bool {
    registerListedFonts([url])
    return fonts[url]?.isRegistered ?? false
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
  
  private func listFonts(in directory: URL) -> [URL] {
    guard let urls = try? FileManager.default.contentsOfDirectory(at: directory, includingPropertiesForKeys: nil) else {
      let message = "Could not list contents of directory \(directory)"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
      return []
    }
    return urls.filter { $0.lastPathComponent.hasFontExtension }
  }

  public func registerFonts(in directory: URL) {
    let fontsToRegister = listFonts(in: directory)
    registerListedFonts(Set(fontsToRegister))
  }

  public func unregisterFonts(in directory: URL, fromSystemOnly: Bool = true) {
    let fontsToUnregister = listFonts(in: directory)
    for url in fontsToUnregister {
      _ = unregisterFont(at: url, fromSystemOnly: fromSystemOnly)
    }
  }

  /**
   * Queries the system for existing registrations for the specified fonts with a single batch run.
   * Only fonts that could not be found will be returned within the result set.
   */
  private func missingFonts(from fontNames: Set<String>) -> Set<String> {
    var fontsToFind = fontNames
    
    UIFont.familyNames.forEach { familyName in
      if fontsToFind.count == 0 {
        return
      }
      
      let familyFonts = UIFont.fontNames(forFamilyName: familyName)
      
      for font in familyFonts {
        if fontsToFind.contains(font) {
          fontsToFind.remove(font)
        }
        
        if fontsToFind.count == 0 {
          break
        }
      }
    }
    
    return fontsToFind
  }
}
