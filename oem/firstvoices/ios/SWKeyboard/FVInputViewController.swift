//
//  FVInputViewController.swift
//  SWKeyboard
//
//  Created by Marc Durdin on 14/5/19.
//  Copyright © 2019 FirstVoices. All rights reserved.
//

import Foundation
import KeymanEngine
import UIKit

class FVInputViewController: InputViewController {
  override init(nibName nibNameOrNil: String?, bundle nibBundleOrNil: Bundle?) {
    //TODO
    KeymanEngine.log.outputLevel = .debug
    KeymanEngine.log.logAppDetails()

    Manager.applicationGroupIdentifier = "group.FVKeyboard"

    //[self setGlobeKeyTapBehaviour:GKTapSwitchToNextKeyboard];
    //[self setMenuBehaviour:MenuShowAlways];
    //[self setMenuCloseButtonTitle:@"Switch to other keyboard"];

    super.init(nibName: nibNameOrNil, bundle: nibBundleOrNil)
  }

  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
}

