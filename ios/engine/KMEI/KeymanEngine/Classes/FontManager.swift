//
//  FontManager.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-11-30.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

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
    guard let customFonts = customFonts() else {
      return
    }

    for fontURL in customFonts {
      let isRegistered = fonts[fontURL]?.isRegistered ?? false
      if !isRegistered {
        if let font = registerFont(at: fontURL) {
          fonts[fontURL] = font
        }
      }
    }
  }

  /// Unregisters all registered fonts in the font path.
  public func unregisterCustomFonts() {
    guard let customFonts = customFonts() else {
      return
    }

    for fontURL in customFonts {
      if var font = fonts[fontURL], font.isRegistered {
        if unregisterFont(at: fontURL) {
          font.isRegistered = false
          fonts[fontURL] = font
        }
      }
    }
  }

  private func customFonts() -> [URL]? {
    let urls: [URL]
    do {
      urls = try FileManager.default.contentsOfDirectory(at: Manager.shared.activeFontDirectory(),
                                                         includingPropertiesForKeys: nil)
    } catch {
      Manager.shared.kmLog("Failed to list font dir contents: \(error)", checkDebugPrinting: false)
      return nil
    }
    return urls.filter { $0.lastPathComponent.hasFontExtension }
  }

  private func readFontName(at url: URL) -> String? {
    guard let provider = CGDataProvider(url: url as CFURL) else {
      Manager.shared.kmLog("Failed to open \(url)", checkDebugPrinting: false)
      return nil
    }
    guard let font = CGFont(provider),
      let name = font.postScriptName
    else {
      Manager.shared.kmLog("Failed to read font at \(url)", checkDebugPrinting: false)
      return nil
    }
    return name as String
  }

  private func registerFont(at url: URL) -> RegisteredFont? {
    guard let fontName = readFontName(at: url) else {
      return nil
    }
    let didRegister: Bool
    if !fontExists(fontName) {
      var errorRef: Unmanaged<CFError>?
      didRegister = CTFontManagerRegisterFontsForURL(url as CFURL, .none, &errorRef)
      let error = errorRef?.takeRetainedValue() // Releases errorRef
      if !didRegister {
        Manager.shared.kmLog("Failed to register font at \(url) reason: \(String(describing: error))",
                             checkDebugPrinting: false)
      } else {
        Manager.shared.kmLog("Registered font: \(url)", checkDebugPrinting: true)
      }
    } else {
      didRegister = false
      Manager.shared.kmLog("Did not register font at \(url) because font name \(fontName) is already registered",
                           checkDebugPrinting: true)
    }
    return RegisteredFont(name: fontName, isRegistered: didRegister)
  }

  private func unregisterFont(at url: URL) -> Bool {
    var errorRef: Unmanaged<CFError>?
    let didUnregister = CTFontManagerUnregisterFontsForURL(url as CFURL, .none, &errorRef)
    let error = errorRef?.takeRetainedValue() // Releases errorRef
    if !didUnregister {
      Manager.shared.kmLog("Failed to unregister font at \(url) reason: \(String(describing: error))",
                           checkDebugPrinting: false)
    } else {
      Manager.shared.kmLog("Unregistered font at \(url)", checkDebugPrinting: true)
    }
    return didUnregister
  }

  private func fontExists(_ fontName: String) -> Bool {
    return UIFont.familyNames.contains { familyName in
      UIFont.fontNames(forFamilyName: familyName).contains(fontName)
    }
  }
}
