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
  override init(nibName nibNameOrNil: String?, bundle nibBundleOrNil: Bundle?) {
    // Replace with your application group id. Must be set before call to super.init().
    Manager.applicationGroupIdentifier = "group.KMSample"

    // Disable before release
    Manager.shared.isDebugPrintingOn = true

    super.init(nibName: nibNameOrNil, bundle: nibBundleOrNil)
  }

  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  override func viewDidLoad() {
    super.viewDidLoad()

    topBarImageView?.backgroundColor = UIColor(red: 1.0, green: 96.0 / 255.0, blue: 0.0, alpha: 1.0)
    let label = UILabel(frame: CGRect.zero)
    label.text = " \(Bundle.main.object(forInfoDictionaryKey: "CFBundleDisplayName")!)"
    label.sizeToFit()
    label.textColor = UIColor.white
    topBarImageView?.addSubview(label)
  }
}
