//
//  ImageBannerViewController.swift
//  Keyman
//
//  Created by Joshua Horton on 2/4/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import UIKit
import KeymanEngine // for log commands

class ImageBannerView: UIView {
  var intendedFrame: CGRect = .zero

  override init(frame: CGRect) {
    super.init(frame: frame)

    initialize()
  }

  required init?(coder: NSCoder) {
    super.init(coder: coder)

    initialize()
  }

  func initialize() {
    self.translatesAutoresizingMaskIntoConstraints = false
    self.clipsToBounds = true
  }

  override var intrinsicContentSize: CGSize {
    return intendedFrame.size
  }

  override var frame: CGRect {
    get {
      return super.frame
    }
    set(value) {
      super.frame = value
      intendedFrame = value
      self.invalidateIntrinsicContentSize()
    }
  }
}

class ImageBannerViewController: UIViewController {

  init() {
    super.init(nibName: "ImageBanner", bundle: Bundle(for: ImageBannerViewController.self))
  }

  required init?(coder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  func renderToImage(size: CGSize) -> UIImage? {
    if size.height == 0 {
      return nil
    }

    let frame = CGRect(origin: self.view.frame.origin, size: size)

    log.debug("Rendering banner image of size \(size)")
    self.view.frame = frame
    self.view.setNeedsLayout()
    self.view.updateConstraints()
    self.view.layoutIfNeeded()

    // Many thanks to https://stackoverflow.com/a/4334902
    var image: UIImage?
    UIGraphicsBeginImageContextWithOptions(size, true, 0.0)
    if let ctx = UIGraphicsGetCurrentContext() {
      view.layer.render(in: ctx)
      image = UIGraphicsGetImageFromCurrentImageContext()
    }
    UIGraphicsEndImageContext()

    return image
  }

  public func renderAsBase64(size: CGSize) -> String? {
    if let image = renderToImage(size: size), let data = image.pngData() {
      // Thanks to https://stackoverflow.com/a/38748135
      let base64 = data.base64EncodedString(options: [])
      let url = "data:application/png;base64," + base64
      return url
    } else {
      return nil
    }
  }
}
