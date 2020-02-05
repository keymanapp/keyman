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
  var topBarImageSource: ImageBannerViewController!

  override init(nibName nibNameOrNil: String?, bundle nibBundleOrNil: Bundle?) {
    #if DEBUG
      KeymanEngine.log.outputLevel = .debug
      KeymanEngine.log.logAppDetails()
    #else
      KeymanEngine.log.outputLevel = .warning
    #endif
    Manager.applicationGroupIdentifier = "group.KM4I"

    topBarImageSource = ImageBannerViewController()

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

    setupTopBarImage(size: view.frame.size)
  }

  override func viewDidLoad() {
    super.viewDidLoad()
    setupTopBarImage(size: view.frame.size)
  }

  override func viewWillTransition(to size: CGSize, with coordinator: UIViewControllerTransitionCoordinator) {
    super.viewWillTransition(to: size, with: coordinator)
    setupTopBarImage(size: size)
  }

  func getTopBarImage(size: CGSize) -> String? {
    return topBarImageSource.renderAsBase64(size: CGSize(width: size.width, height: self.activeTopBarHeight))
  }

  func setupTopBarImage(size: CGSize) {
    let imgPath = getTopBarImage(size: size)
    guard let path = imgPath else {
      log.error("No image specified for the image banner!")
      return
    }

    self.setBannerImage(to: path)
  }

  override func traitCollectionDidChange(_ previousTraitCollection: UITraitCollection?) {
    super.traitCollectionDidChange(previousTraitCollection)
    guard let previousTraitCollection = previousTraitCollection else {return}
    if #available(iOS 13.0, *) {
      if previousTraitCollection.hasDifferentColorAppearance(comparedTo: traitCollection) {
        // Ensure that the keyboard banner image transitions!
        // The backing view isn't in the view hierarchy, so we need to force-trigger the listener.
        topBarImageSource.traitCollectionDidChange(previousTraitCollection)
      }
    }
  }
}
