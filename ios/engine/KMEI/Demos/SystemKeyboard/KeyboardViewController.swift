//
//  KeyboardViewController.swift
//  SystemKeyboard
//
//  Created by Gabriel Wong on 2017-10-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine

class KeyboardViewController: InputViewController {
  override init(nibName nibNameOrNil: String?, bundle nibBundleOrNil: Bundle?) {
    _ = log // forces init of the log, which is useful in sys-kbd contexts.
    Manager.applicationGroupIdentifier = "group.KMEI"
    super.init(nibName: nibNameOrNil, bundle: nibBundleOrNil)
  }
  
  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
}
