//
//  Constants.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-20.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

public enum Key {
  public static let keyboardInfo = "keyboardInfo"
  public static let lexicalModelInfo = "lexicalModelInfo"

  /// Array of user keyboards info list in UserDefaults
  static let userKeyboardsList = "UserKeyboardsList"

  /// Array of user lexical models info list in UserDefaults
  static let userLexicalModelsList = "UserLexicalModelsList"

  /// Dictionary of cached query results for a user's installed packages
  static let userPackageQueryCacheDict = "UserQueryDistributionStateCache"

  /// Currently/last selected keyboard info in UserDefaults
  static let userCurrentKeyboard = "UserCurrentKeyboard"

  /// Currently/last selected lexical model info in UserDefaults
  static let userCurrentLexicalModel = "UserCurrentLexicalModel"
  
  /// Dictionary of lexical model ids keyed by language id in UserDefaults
  static let userPreferredLexicalModels = "UserPreferredLexicalModels"
  
  /// Dictionary of prediction/correction toggle settings keyed by language id in UserDefaults
  static let userPredictSettings = "UserPredictionEnablementSettings"
  static let userCorrectSettings = "UserCorrectionEnablementSettings"

  // Internal user defaults keys
  static let engineVersion = "KeymanEngineVersion"
  static let keyboardPickerDisplayed = "KeyboardPickerDisplayed"
  static let synchronizeSWKeyboard = "KeymanSynchronizeSWKeyboard"
  static let synchronizeSWLexicalModel = "KeymanSynchronizeSWLexicalModel"

  static let migrationLevel = "KeymanEngineMigrationLevel"
  static let portraitKeyboardHeight = "PortraitKeyboardHeight"
  static let landscapeKeyboardHeight = "LandscapeKeyboardHeight"

  // JSON keys for language REST calls
  static let options = "options"
  static let language = "language"

  // TODO: Check if it matches with the key in Keyman Cloud API
  static let keyboardCopyright = "copyright"
  static let lexicalModelCopyright = "lexicalmodelcopyright"
  static let languages = "languages"

  // Other keys
  static let update = "update"
  
  // ResourceDownloadQueue keys
  static let downloadTask = "downloadTask"
  static let downloadBatch = "downloadBatch"
  static let downloadQueueFrame = "queueFrame"

  // Settings-related keys
  static let optShouldReportErrors = "ShouldReportErrors"
  // Deprecated - no longer used
  static let optShouldShowBanner = "ShouldShowBanner"
  static let optSpacebarText = "SpacebarText"
  // This one SHOULD be app-only, but is needed by the currently
  // in-engine Settings menus.  Alas.
  static let optShouldShowGetStarted = "ShouldShowGetStarted"
}

public enum Defaults {
  private static let font = Font(family: "LatinWeb", source: ["DejaVuSans.ttf"], size: nil)

  public static let keyboardID = FullKeyboardID(keyboardID: "sil_euro_latin", languageID: "en")
  public static let lexicalModelID = FullLexicalModelID(lexicalModelID: "nrc.en.mtnt", languageID: "en")

  public static var keyboardPackage: KeyboardKeymanPackage = {
    let bundledKMP = Resources.bundle.url(forResource: keyboardID.keyboardID, withExtension: ".kmp")!
    return try! ResourceFileManager.shared.prepareKMPInstall(from: bundledKMP) as! KeyboardKeymanPackage
  }()

  public static let lexicalModelPackage: LexicalModelKeymanPackage = {
    let bundledKMP = Resources.bundle.url(forResource: lexicalModelID.lexicalModelID, withExtension: ".model.kmp")!
    return try! ResourceFileManager.shared.prepareKMPInstall(from: bundledKMP) as! LexicalModelKeymanPackage
  }()

  // Must be retrieved from their packages!
  public static let keyboard: InstallableKeyboard = {
    return keyboardPackage.findResource(withID: keyboardID)!
  }()

  public static let lexicalModel: InstallableLexicalModel = {
    return lexicalModelPackage.findResource(withID: lexicalModelID)!
  }()
  
  // default for ancient/unrecognized devices
  static let unknownDeviceKeyboardHeight: CGFloat = 216.0
}

public enum Resources {
  /// Keyman Web resources
  public static let bundle: Bundle = {
    // If we're executing this code, KMEI's framework should already be loaded.  Just use that.
    let frameworkBundle = Bundle(for: Manager.self) //Bundle(identifier: "org.sil.Keyman.ios.Engine")!
    return Bundle(path: frameworkBundle.path(forResource: "Keyman", ofType: "bundle")!)!
  }()

  public static let oskFontFilename = "keymanweb-osk.ttf"
  static let kmwFilename = "keyboard.html"
}

public enum Util {
  /// Is the process of a custom keyboard extension. Avoid using this
  /// in most situations as Manager.shared.isSystemKeyboard is more
  /// reliable in situations where in-app and system keyboard can
  /// be used in the same app, for example using the Web Browser in
  /// the Keyman app. However, in initialization scenarios this test
  /// makes sense.
  public static let isSystemKeyboard: Bool = {
    let infoDict = Bundle.main.infoDictionary
    let extensionInfo = infoDict?["NSExtension"] as? [AnyHashable: Any]
    let extensionID = extensionInfo?["NSExtensionPointIdentifier"] as? String
    return extensionID == "com.apple.keyboard-service"
  }()

  /// The version of the Keyman SDK
  public static let sdkVersion: String = {
    return Version.current.plainString
  }()
}

public enum FileExtensions {
  public static let javaScript = "js"
  public static let trueTypeFont = "ttf"
  public static let openTypeFont = "otf"
  public static let configurationProfile = "mobileconfig"
}

public enum KeymanError: Error, LocalizedError {
  case unknown
  // file that is missing, is the file critical?
  case missingFile(String, Bool)

  var localizedDescription: String {
    switch self {
      case .unknown:
        return engineBundle.localizedString(forKey: "error-unknown", value: nil, table: nil)
      case .missingFile(_, let isCritical):
        if isCritical {
          return engineBundle.localizedString(forKey: "error-missing-file-critical", value: nil, table: nil)
        } else {
          return engineBundle.localizedString(forKey: "error-missing-file", value: nil, table: nil)
        }
    }
  }
}
