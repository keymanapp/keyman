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
import os

/**
 * Takes in a XIB spec for an image banner and makes it renderable, applying size constraints when rendered
 * to ensure proper scale.  Note that the loaded UIView will never be displayed in the view hierarchy.
 */
class ImageBannerViewController: UIViewController {
  // Since we never actually put this UIView into the view hierarchy, we need controlled constraints
  // to control the actual frame size.
  var widthConstraint: NSLayoutConstraint?
  var heightConstraint: NSLayoutConstraint?
  
  init(nibName: String, bundle: Bundle) {
    super.init(nibName: nibName, bundle: bundle)
  }
  
  required init?(coder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
  
  override func loadView() {
    super.loadView()
    
    view.translatesAutoresizingMaskIntoConstraints = false
    view.clipsToBounds = true
    
    // Usually these would be in viewDidLoad, but they need to be redone with our manual-reload,
    // and that method doesn't retrigger.
    widthConstraint = view.widthAnchor.constraint(equalToConstant: 0)
    widthConstraint!.isActive = true
    heightConstraint = view.heightAnchor.constraint(equalToConstant: 0)
    heightConstraint!.isActive = true
  }
  
  func renderToImage(size: CGSize) -> UIImage? {
    if size.width == 0 || size.height == 0 {
      return nil
    }
    
    let frame = CGRect(origin: self.view.frame.origin, size: size)
    
    os_log("Rendering banner image of size %{public}s", log: KeymanLogger.ui, type: .debug, NSCoder.string(for: size))
    
    self.view.frame = frame
    widthConstraint?.constant = size.width
    heightConstraint?.constant = size.height
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
}
