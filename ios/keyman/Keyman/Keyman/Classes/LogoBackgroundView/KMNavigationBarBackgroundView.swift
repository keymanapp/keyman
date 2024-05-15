//
//  KMNavigationBarBackgroundView.swift
//  Keyman
//
//  Created by Jacob Bullock on 4/26/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

import Foundation
import UIKit

class KMNavigationBarBackgroundView: UIView {
  
  let defaultImageLeading: Int = 10
  let iphonexImageLeading: Int = 25
  let underscoreHeight: Int = 3
  let logoImageWidth: Int = 100
  let logoImageHeightDiff: Int  = -8
  
  var imageLeadingConstraint: NSLayoutConstraint?
  var logoImageView: UIImageView?
  
  required public init?(coder: NSCoder) {
    super.init(coder: coder)
    self.commonInit()
  }
  
  public override init(frame: CGRect) {
    super.init(frame: frame)
    self.commonInit()
  }
  
  func commonInit() {
    self.backgroundColor = .clear
    self.isUserInteractionEnabled = false
    setupLogo()
    setupUnderscore()
  }
  
  func hideLogo() {
    if let logoImageView = logoImageView {
      logoImageView.alpha = 0
    }
  }
  
  func setupUnderscore() {
    let v = KMUnderscoreView(frame: CGRect())
    self.addSubview(v)
    v.translatesAutoresizingMaskIntoConstraints = false
    
    v.constrainEqually(to: self, attribute: .bottom).isActive  = true
    v.constrainEqually(to: self, attribute: .leading).isActive  = true
    v.constrainEqually(to: self, attribute: .trailing).isActive  = true
    v.constrainEqually(to: CGFloat(underscoreHeight), attribute: .height).isActive  = true
  }
  
  func setupLogo() {
    let image = UIImage(named: "Logo")
    let imageView = UIImageView(image: image)
    imageView.contentMode = .scaleAspectFit
    imageView.translatesAutoresizingMaskIntoConstraints = false
    self.addSubview(imageView)
    
    imageView.constrainEqually(to: self, attribute: .centerY).isActive  = true
    imageView.constrainEqually(to: self, attribute: .centerY).isActive  = true
    imageView.constrainEqually(to: self,
                               attribute: .height,
                               constant: CGFloat(logoImageHeightDiff)).isActive  = true
    imageView.constrainEqually(to: CGFloat(logoImageWidth), attribute: .width).isActive  = true
    
    imageLeadingConstraint = imageView.constrainEqually(to: self,
                                                        attribute: .leading,
                                                        constant: CGFloat(defaultImageLeading))
    imageLeadingConstraint?.isActive = true
    
    logoImageView = imageView
  }
  
  func addToNavbar(_ navbar: UINavigationBar!) {
    navbar.addSubview(self)
    self.constrainCorners(to: navbar, options: ["top.constant": 0])
  }
  
  func setOrientation(_ orientation: UIInterfaceOrientation) {
    var constant: CGFloat = CGFloat(defaultImageLeading)
    if let constraint = imageLeadingConstraint {
      if orientation.isLandscape && UIDevice.isIphoneX {
        constant = CGFloat(iphonexImageLeading)
      }
      constraint.constant = constant
      self.setNeedsLayout()
    }
  }
}
