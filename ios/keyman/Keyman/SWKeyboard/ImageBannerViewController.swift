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

  // Since we never actually put this UIView into the view hierarchy, we need controlled constraints
  // to control the actual frame size.
  var widthConstraint: NSLayoutConstraint?
  var heightConstraint: NSLayoutConstraint?

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

    widthConstraint = self.widthAnchor.constraint(equalToConstant: 0)
    widthConstraint!.isActive = true
    heightConstraint = self.heightAnchor.constraint(equalToConstant: 0)
    heightConstraint!.isActive = true
  }

  override var intrinsicContentSize: CGSize {
    return intendedFrame.size
  }

  override var frame: CGRect {
    get {
      return super.frame
    }
    set(value) {
      intendedFrame = value
      widthConstraint?.constant = value.width
      heightConstraint?.constant = value.height
      super.frame = value
      self.invalidateIntrinsicContentSize()
    }
  }
}

class ImageBannerViewController: UIViewController {
  @IBOutlet weak var imgLogo: UIImageView!
  private let imgName = "Logo"

  init() {
    super.init(nibName: "ImageBanner", bundle: Bundle(for: ImageBannerViewController.self))
  }

  required init?(coder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  func renderToImage(size: CGSize) -> UIImage? {
    if size.width == 0 || size.height == 0 {
      return nil
    }

    let frame = CGRect(origin: self.view.frame.origin, size: size)

    log.debug("Rendering banner image of size \(size)")
    self.view.frame = frame
    self.view.setNeedsLayout()
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

//  public func updateImageWithTraitCollection(_ traitCollection: UITraitCollection?) {
////    imgLogo.image = UIImage(named: imgName,
////                            in: Bundle(for: ImageBannerViewController.self),
////                            compatibleWith: traitCollection)
//    if #available(iOS 12.0, *) {
//      let ois = self.traitCollection.userInterfaceStyle
//
//
//    if #available(iOS 13.0, *), let userInterfaceStyle = traitCollection?.userInterfaceStyle {
//      self.overrideUserInterfaceStyle = userInterfaceStyle
//      self.view.setNeedsDisplay()
//    }
//
//
//      let nis = self.traitCollection.userInterfaceStyle
//
//      log.debug("Should be set to \(nis.description) mode, was \(ois.description) mode, was given \(traitCollection?.userInterfaceStyle.description)")
//      // This is properly set... so why doesn't it visually change in the renders?
//      log.debug("Image style: \(imgLogo.traitCollection.userInterfaceStyle.description)")
//
//      // An attempt to force the style change to follow through; it's currently refusing this.
//    }
//  }
}

//@available(iOS 12.0, *)
//extension UIUserInterfaceStyle {
//  public var description: String {
//    switch self {
//      case .unspecified:
//        return "unspecified"
//      case .light:
//        return "light"
//      case .dark:
//        return "dark"
//      default:
//        return "unknown \(self.rawValue)"
//    }
//  }
//}
