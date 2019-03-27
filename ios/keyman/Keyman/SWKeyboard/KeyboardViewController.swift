//
//  KeyboardViewController.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-04.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine
import UIKit

class KeyboardViewController: InputViewController {
  override init(nibName nibNameOrNil: String?, bundle nibBundleOrNil: Bundle?) {
    #if DEBUG
      KeymanEngine.log.outputLevel = .debug
      KeymanEngine.log.logAppDetails()
    #else
      KeymanEngine.log.outputLevel = .warning
    #endif
    Manager.applicationGroupIdentifier = "group.KM4I"

    super.init(nibName: nibNameOrNil, bundle: nibBundleOrNil)
  }

  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  override func updateViewConstraints() {
    super.updateViewConstraints()

    if view.frame.size.width == 0 || self.view.frame.size.height == 0 {
      return
    }

    setupTopBarImage(isPortrait: InputViewController.isPortrait)
  }

  override func viewDidLoad() {
    super.viewDidLoad()
    setupTopBarImage(isPortrait: InputViewController.isPortrait)
  }

  override func viewWillTransition(to size: CGSize, with coordinator: UIViewControllerTransitionCoordinator) {
    super.viewWillTransition(to: size, with: coordinator)
    setupTopBarImage(isPortrait: UIDevice.current.orientation.isPortrait)
  }

  func setupTopBarImage(isPortrait: Bool) {
//    if isPortrait {
//      topBarImageView?.image = #imageLiteral(resourceName: "banner-Portrait.png")
//      return
//    }
//
//    // iPad
//    if UIDevice.current.userInterfaceIdiom != UIUserInterfaceIdiom.phone {
//      topBarImageView?.image = #imageLiteral(resourceName: "banner-Landscape.png")
//      return
//    }
//
//    // iPhone
//    let screenRect = UIScreen.main.bounds
//    if CGFloat.maximum(screenRect.height, screenRect.width) >= 568.0 {
//      topBarImageView?.image = #imageLiteral(resourceName: "banner-Landscape-568h.png")
//    } else {
//      topBarImageView?.image = #imageLiteral(resourceName: "banner-Landscape.png")
//    }
  }
}
