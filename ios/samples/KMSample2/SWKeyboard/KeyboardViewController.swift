//
//  KeyboardViewController.swift
//  SWKeyboard
//
//  Created by Gabriel Wong on 2017-10-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine
import UIKit

class KeyboardViewController: KeymanInputViewController {
  override func viewDidLoad() {
    // Replace with your application group id
    KMManager.setApplicationGroupIdentifier("group.KMSample")

    // Disable before release
    KMManager.sharedInstance().debugPrintingOn = true

    super.viewDidLoad()

    topBarImageView?.backgroundColor = UIColor(red: 1.0, green: 96.0 / 255.0, blue: 0.0, alpha: 1.0)
    let label = UILabel(frame: CGRect.zero)
    label.text = " \(Bundle.main.object(forInfoDictionaryKey: "CFBundleDisplayName")!)"
    label.sizeToFit()
    label.textColor = UIColor.white
    topBarImageView?.addSubview(label)
  }
}
