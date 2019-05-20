//
//  FVInputViewController.swift
//  SWKeyboard
//
//  Created by Marc Durdin on 14/5/19.
//  Copyright © 2019 FirstVoices. All rights reserved.
//

//import Foundation
import KeymanEngine
import UIKit

@objc(FVInputViewController)
class FVInputViewController: InputViewController {
  override init(nibName nibNameOrNil: String?, bundle nibBundleOrNil: Bundle?) {
    #if DEBUG
      KeymanEngine.log.outputLevel = .debug
      KeymanEngine.log.logAppDetails()
    #else
      KeymanEngine.log.outputLevel = .warning
    #endif

    Manager.applicationGroupIdentifier = "group.FVKeyboard"
    //[self setGlobeKeyTapBehaviour:GKTapSwitchToNextKeyboard];
    //[self setMenuBehaviour:MenuShowAlways];
    //[self setMenuCloseButtonTitle:@"Switch to other keyboard"];

    super.init(nibName: nibNameOrNil, bundle: nibBundleOrNil)

    self.globeKeyTapBehaviour = .switchToNextKeyboard
    self.menuBehaviour = .showAlways
    self.menuCloseButtonTitle = "Switch to other keyboard"
  }

  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
}

