//
//  ViewController.swift
//  KMSample2
//
//  Created by Gabriel Wong on 2017-10-06.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine
import UIKit

class ViewController: UIViewController, KMTextViewDelegate {
  @IBOutlet var textView: KMTextView!

  override func viewDidLoad() {
    super.viewDidLoad()

    // Replace with your application group id
    KMManager.setApplicationGroupIdentifier("group.KMSample")

    // Disable before release
    KMManager.sharedInstance().debugPrintingOn = true

    KMManager.sharedInstance().keymanHelpOn = false
    KMManager.sharedInstance().preloadLanguageFile(atPath: Bundle.main.path(forResource: "tamil99m-1.1", ofType: "js"),
                                                   shouldOverwrite: true)
    KMManager.sharedInstance().preloadFontFile(atPath: Bundle.main.path(forResource: "aava1", ofType: "ttf"),
                                               shouldOverwrite: true)
    KMManager.sharedInstance().registerCustomFonts()

    KMManager.sharedInstance().addKeyboard(withID: kKeymanDefaultKeyboardID, languageID: kKeymanDefaultLanguageID,
                                           keyboardName: kKeymanDefaultKeyboardName,
                                           languageName: kKeymanDefaultLanguageName,
                                           isRTL: false, isCustom: false,
                                           font: kKeymanDefaultKeyboardFont, oskFont: nil)
    KMManager.sharedInstance().addKeyboard(withID: "tamil99m", languageID: "tam",
                                           keyboardName: "Tamil 99M", languageName: "Tamil",
                                           isRTL: false, isCustom: true, font: "aava1.ttf", oskFont: nil)

    textView.setKeymanDelegate(self)
    textView.viewController = self
  }
}
