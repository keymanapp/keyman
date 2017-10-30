//
//  Constants.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-20.
//  Copyright © 2017 SIL International. All rights reserved.
//

public struct Key {
  // Common dictionary keys when handling language/keyboard info
  public static let id = "id"
  public static let name = "name"
  // If there is no value for this key, revert to IdKey
  public static let keyboardId = "kbId"
  // If there is no value for this key, revert to IdKey
  public static let languageId = "langId"
  // If there is no value for this key, revert to NameKey
  public static let keyboardName = "kbName"
  // If there is no value for this key, revert to NameKey
  public static let languageName = "langName"
  // If there is no value for this key, revert to NameKey
  public static let keyboardVersion = "version"
  public static let keyboard = "keyboard"
  public static let languageKeyboards = "keyboards"
  public static let keyboardFileSize = "fileSize"
  // If there is no value for this key, revert to KeyboardFileSizeKey
  public static let keyboardGZipFileSize = "fileSizeGzip"
  public static let font = "font"
  public static let oskFont = "oskFont"
  public static let fontFamily = "family"
  public static let fontSource = "source"
  public static let fontFiles = "files"
  // Font filename is deprecated
  public static let fontFilename = "filename"
  public static let fontName = "fontname"
  public static let fontRegistered = "fontregistered"
  public static let keyboardFilename = "filename"
  public static let keyboardModified = "lastModified"
  public static let keyboardRTL = "rtl"
  public static let keyboardInfo = "keyboardInfo"
  public static let customKeyboard = "CustomKeyboard"

  /// Array of user keyboards info list in UserDefaults
  public static let userKeyboardsList = "UserKeyboardsList"

  /// Currently/last selected keyboard info in UserDefaults
  public static let userCurrentKeyboard = "UserCurrentKeyboard"

  // Internal user defaults keys
  static let engineVersion = "KeymanEngineVersion"
  static let keyboardPickerDisplayed = "KeyboardPickerDisplayed"
  static let synchronizeSWKeyboard = "KeymanSynchronizeSWKeyboard"

  // JSON keys for language REST calls
  static let options = "options"
  static let language = "language"

  // TODO: Check if it matches with the key in Keyman Cloud API
  static let keyboardCopyright = "copyright"
  static let languages = "languages"
  static let keyboardBaseURI = "keyboardBaseUri"
  static let fontBaseURI = "fontBaseUri"
  static let keyboardURI = "uri"

  // Other keys
  static let update = "update"
}

public struct Constants {
  private static let defaultFont = Font(family: "LatinWeb", source: ["DejaVuSans.ttf"], size: nil)
  public static let defaultKeyboard = InstallableKeyboard(id: "european2",
                                                          name: "EuroLatin2 Keyboard",
                                                          languageID: "eng",
                                                          languageName: "English",
                                                          version: "1.6",
                                                          isRTL: false,
                                                          font: defaultFont,
                                                          oskFont: nil,
                                                          isCustom: false)
}
