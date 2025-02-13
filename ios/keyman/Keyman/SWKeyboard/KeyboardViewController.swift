//
//  KeyboardViewController.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-04.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine
import UIKit
import Sentry
import os.log

class KeyboardViewController: InputViewController {
  var topBarImageSource: ImageBannerViewController!

  // The entrypoint for the app-extension.
  override init(nibName nibNameOrNil: String?, bundle nibBundleOrNil: Bundle?) {
    // Only true if it's the first init of the class under app-extension mode.
    if !SentryManager.hasStarted {
      // Sentry can send errors from app extensions when "Allow Full Access"
      // is enabled.  They seem to get blocked otherwise, except in the Simulator.
      SentryManager.start(sendingEnabled: true)
    }

    Manager.applicationGroupIdentifier = "group.KM4I"

    let bundle = Bundle(for: KeyboardViewController.self)
    topBarImageSource = ImageBannerViewController(nibName: "ImageBanner", bundle: bundle)

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
    return topBarImageSource.renderAsBase64(size: CGSize(width: size.width, height: InputViewController.topBarHeight))
  }

  func setupTopBarImage(size: CGSize) {
    // We override the height in getTopBarImage, but not width.
    // If width is 0, we return null from that function; this'll
    // trigger the guard below.
    if size.width == 0 {
      return
    }

    let imgPath = getTopBarImage(size: size)
    guard let path = imgPath else {
      let message = "No image specified for the image banner!"
      os_log("%{public}s", log: KeymanLogger.ui, type: .error, message)
      SentryManager.capture(message)
      return
    }

    self.setBannerImage(to: path)
  }

  override func traitCollectionDidChange(_ previousTraitCollection: UITraitCollection?) {
    super.traitCollectionDidChange(previousTraitCollection)
    guard let previousTraitCollection = previousTraitCollection else {return}
    if previousTraitCollection.hasDifferentColorAppearance(comparedTo: traitCollection) {
      /* Ensure that the keyboard banner image transitions!  Noting that the backing view isn't in the view hierarchy,
       * and that manipulating said hierarchy is key for triggering the appearance change
       * (see https://developer.apple.com/documentation/uikit/uiappearance)...
       *
       * We have to reload the banner's backing view to trigger the change.  Thanks, Apple.
       * The only other alternative - creating a new instance of the view & its controller.
       *
       * With loadView, at least we can reuse the old instance, which would serve far better
       * for engine API calls if we decide to let the base InputViewController perform UIView
       * render functionality (as a KMEI offering) instead of having it only in our app.
       * That said, note https://developer.apple.com/documentation/uikit/uiviewcontroller/1621454-loadview:
       *
       * > You should never call this method directly.
       *
       * Well... I wouldn't, if it weren't the only way to trigger this without requiring a new instance.
       * We should be fine since we never place the banner's backing view into the actual hierarchy.
       */
      topBarImageSource.loadView()
      setupTopBarImage(size: view.frame.size)
    }
  }
}
