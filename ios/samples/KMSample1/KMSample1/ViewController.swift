//
//  ViewController.swift
//  KMSample1
//
//  Created by Gabriel Wong on 2017-10-06.
//  Copyright © 2017 SIL International. All rights reserved.
//
import KeymanEngine
import UIKit

class ViewController: UIViewController, KeymanTextViewDelegate {
  @IBOutlet var textView: KeymanTextView!

  override func viewDidLoad() {
    super.viewDidLoad()

    Manager.shared.isDebugPrintingOn = true
    Manager.shared.isKeymanHelpOn = false
    Manager.shared.preloadLanguageFile(atPath: Bundle.main.path(forResource: "tamil99m-1.1", ofType: "js")!,
                                       shouldOverwrite: true)
    Manager.shared.preloadFontFile(atPath: Bundle.main.path(forResource: "aava1", ofType: "ttf")!,
                                   shouldOverwrite: true)
    Manager.shared.registerCustomFonts()

    Manager.shared.addKeyboard(withID: kKeymanDefaultKeyboardID,
                               languageID: kKeymanDefaultLanguageID,
                               keyboardName: kKeymanDefaultKeyboardName,
                               languageName: kKeymanDefaultLanguageName,
                               isRTL: false, isCustom: false,
                               font: kKeymanDefaultKeyboardFont, oskFont: nil)
    Manager.shared.addKeyboard(withID: "tamil99m", languageID: "tam",
                               keyboardName: "Tamil 99M", languageName: "Tamil",
                               isRTL: false, isCustom: true, font: "aava1.ttf", oskFont: nil)

    textView.setKeymanDelegate(self)
    textView.viewController = self
  }
}
