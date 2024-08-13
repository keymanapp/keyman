//
//  KeyboardViewController.swift
//  SWKeyboard
//
//  Created by Gabriel Wong on 2017-10-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine
import UIKit

class KeyboardViewController: InputViewController {
  override init(nibName nibNameOrNil: String?, bundle nibBundleOrNil: Bundle?) {
    // Replace with your application group id
    Manager.applicationGroupIdentifier = "group.KMSample"

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

  func getTopBarImage(isPortrait: Bool) -> String? {
    if isPortrait {
      return Bundle.main.path(forResource: "banner-portrait-text", ofType: "png")
    } else {
      return Bundle.main.path(forResource: "banner-landscape-text", ofType: "png")
    }
  }

  func setupTopBarImage(isPortrait: Bool) {
    let imgPath = getTopBarImage(isPortrait: isPortrait)
    guard let path = imgPath else {
      print("No image specified for the image banner!")
      return
    }

    self.setBannerImage(to: path)
  }
}
