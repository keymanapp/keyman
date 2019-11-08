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
}

public enum Defaults {
  private static let font = Font(family: "LatinWeb", source: ["DejaVuSans.ttf"], size: nil)
  public static let keyboard = InstallableKeyboard(id: "sil_euro_latin",
                                                   name: "EuroLatin (SIL)",
                                                   languageID: "en",
                                                   languageName: "English",
                                                   version: "1.8.1",
                                                   isRTL: false,
                                                   font: font,
                                                   oskFont: nil,
                                                   isCustom: false)
  public static let lexicalModel = InstallableLexicalModel(id: "nrc.en.mtnt",
                                                   name: "English dictionary (MTNT)",
                                                   languageID: "en",
                                                   version: "0.1.2",
                                                   isCustom: false)
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
    let url = Resources.bundle.url(forResource: "KeymanEngine-Info", withExtension: "plist")!
    let info = NSDictionary(contentsOf: url)!
    return info["CFBundleVersion"] as! String
  }()
}

public enum FileExtensions {
  public static let javaScript = "js"
  public static let trueTypeFont = "ttf"
  public static let openTypeFont = "otf"
  public static let configurationProfile = "mobileconfig"
}

// Used to facilitate constant colors with legacy (pre iOS-11.0) devices,
// as they can't use "Color Assets".
public enum Colors {

  public static var systemBackground: UIColor {
    get {
      if #available(iOS 13.0, *) {
        return UIColor.systemBackground
      } else {
        return UIColor.white
      }
    }
  }

  // The primary color used for selected UI elements in the settings menu.
  public static var selectionPrimary: UIColor {
    get {
      if #available(iOSApplicationExtension 11.0, *) {
        return UIColor(named: "SelectionPrimary")!
      } else {
        return UIColor(red: 204.0 / 255.0,
                       green: 136.0 / 255.0,
                       blue: 34.0 / 255.0,
                       alpha: 1.0)
      }
    }
  }

  // The primary color used for selected UI elements in Get Started menu.
  public static var selectionSecondary: UIColor {
    get {
      if #available(iOSApplicationExtension 11.0, *) {
        return UIColor(named: "SelectionSecondary")!
      } else {
        return UIColor(red: 95.0 / 255.0,
                       green: 196.0 / 255.0,
                       blue: 217.0 / 255.0,
                       alpha: 1.0)
      }
    }
  }

  public static var statusToolbar: UIColor {
    get {
      if #available(iOSApplicationExtension 11.0, *) {
        return UIColor(named: "StatusToolbar")!
      } else {
        return UIColor(red: 0.5,
                       green: 0.75,
                       blue: 0.25,
                       alpha: 0.9)
      }
    }
  }

  public static var statusResourceUpdateButton: UIColor {
    get {
      if #available(iOSApplicationExtension 11.0, *) {
        return UIColor(named: "StatusResourceUpdateButton")!
      } else {
        return UIColor(red: 0.75,
                       green: 1.0,
                       blue: 0.5,
                       alpha: 1.0)
      }
    }
  }

  public static var popupBorder: UIColor {
    get {
//      if #available(iOSApplicationExtension 11.0, *) {
//        return UIColor(named: "SelectionPrimary")!
//      } else {
        return UIColor(red: 134.0 / 255.0,
                       green: 137.0 / 255.0,
                       blue: 139.0 / 255.0,
                       alpha: 1.0)
//      }
    }
  }

  public static var popupKey: UIColor {
    get {
//      if #available(iOSApplicationExtension 11.0, *) {
//        return UIColor(named: "SelectionPrimary")!
//      } else {
        return UIColor(red: 244.0 / 255.0,
                       green: 244.0 / 255.0,
                       blue: 244.0 / 255.0,
                       alpha: 1.0)
//      }
    }
  }

  public static var popupKeyHighlighted: UIColor {
    get {
//      if #available(iOSApplicationExtension 11.0, *) {
//        return UIColor(named: "SelectionPrimary")!
//      } else {
        return UIColor(red: 136.0 / 255.0,
                       green: 136.0 / 255.0,
                       blue: 1.0,
                       alpha: 1.0)
//      }
    }
  }

  public static var popupKeyTint: UIColor {
    get {
//      if #available(iOSApplicationExtension 11.0, *) {
//        return UIColor(named: "SelectionPrimary")!
//      } else {
        return UIColor(red: 181.0 / 255.0,
                       green: 181.0 / 255.0,
                       blue: 181.0 / 255.0,
                       alpha: 1.0)
//      }
    }
  }

  public static var helpBubbleGradient1: UIColor {
    get {
      if #available(iOSApplicationExtension 11.0, *) {
        return UIColor(named: "HelpBubbleGradient1")!
      } else {
        return UIColor(red: 253.0 / 255.0,
                       green: 244.0 / 255.0,
                       blue: 196.0 / 255.0,
                       alpha: 1.0)
      }
    }
  }

  public static var helpBubbleGradient2: UIColor {
    get {
      if #available(iOSApplicationExtension 11.0, *) {
        return UIColor(named: "HelpBubbleGradient2")!
      } else {
        return UIColor(red: 233.0 / 255.0,
                       green: 224.0 / 255.0,
                       blue: 176.0 / 255.0,
                       alpha: 1.0)
      }
    }
  }

  public static var keyboardBackground: UIColor {
    get {
      if #available(iOSApplicationExtension 11.0, *) {
        return UIColor(named: "KeyboardBackground")!
      } else {
        return UIColor(red: 210.0 / 255.0,
                       green: 214.0 / 255.0,
                       blue: 220.0 / 255.0,
                       alpha: 1.0)
      }
    }
  }

  public static var spinnerBackground: UIColor {
    get {
      if #available(iOSApplicationExtension 11.0, *) {
        return UIColor(named: "SpinnerBackground")!
      } else {
        return UIColor(white: 0.5,
                       alpha: 0.8)
      }
    }
  }

  public static var labelNormal: UIColor {
    get {
      if #available(iOSApplicationExtension 11.0, *) {
        return UIColor(named: "LabelNormal")!
      } else {
        return UIColor.lightGray
      }
    }
  }

  public static var labelHighlighted: UIColor {
    get {
      if #available(iOSApplicationExtension 11.0, *) {
        return UIColor(named: "LabelHighlighted")!
      } else {
        return UIColor.darkGray
      }
    }
  }

  public static var listSeparator: UIColor {
    get {
      if #available(iOSApplicationExtension 11.0, *) {
        return UIColor(named: "ListSeparator")!
      } else {
        return UIColor.lightGray
      }
    }
  }
}
